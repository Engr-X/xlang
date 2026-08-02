from pathlib import Path
import argparse
import json
import sys


if __package__ is None or __package__ == "":
    sys.path.append(str(Path(__file__).resolve().parents[1]))


from codegen.util import (
    CodeBlock,
    JsonObject,
    ensure_type,
    gen_constants as genConstants,
    gen_file_class as genFileClass,
    gen_imports as genImports,
    gen_package as genPackage,
    get_class_name as getClassName,
    get_package_name as getPackageName,
    read_json,
    write_text,
)


PARSER_RULES_TYPE: str = "compiler_parser_rules"

DEFAULT_IMPORTS: set[str] = {
    "xlang.lexer.PatternList",
    "xlang.lexer.TokenList",
}


def getImports(config: JsonObject) -> set[str]:
    return DEFAULT_IMPORTS | set(item for item in config.get("imports", []) if item)


def getCodeBlocks(config: JsonObject, name: str) -> list[CodeBlock]:
    blocks = config.get(name, config.get(f"{name}: ", []))

    if not isinstance(blocks, list):
        raise TypeError(f"{name} must be a list")

    return blocks


def genBlocks(blocks: list[CodeBlock]) -> str:
    if not blocks:
        return ""

    lines: list[str] = []

    for block in blocks:
        if isinstance(block, str):
            lines.append(block)
        elif isinstance(block, list):
            lines.extend(block)
        else:
            raise TypeError(f"code block must be string or string list: {block!r}")

        lines.append("")

    return "\n".join(lines).rstrip() + "\n\n\n"


def genRegexExpr(regex: object) -> str | None:
    if regex is None:
        return None

    if not isinstance(regex, str):
        raise TypeError(f"parser pattern regex must be string or null: {regex!r}")

    return json.dumps(regex)


def genKindExpr(kind: object) -> str | None:
    if kind is None:
        return None

    if not isinstance(kind, str) or not kind:
        raise TypeError(f"parser pattern kind must be a non-empty string: {kind!r}")

    return kind


def genPatternPush(pattern: object) -> str:
    if isinstance(pattern, str):
        return f".push({json.dumps(pattern)})"

    if not isinstance(pattern, dict):
        raise TypeError(f"parser pattern must be string or object: {pattern!r}")

    if "pattern" in pattern:
        raise ValueError(f"use regex instead of pattern in parser rule: {pattern!r}")

    if "regx" in pattern:
        raise ValueError(f"use regex instead of regx in parser rule: {pattern!r}")

    kind = genKindExpr(pattern.get("kind", None))
    regex = genRegexExpr(pattern.get("regex", None))

    if kind is None and regex is None:
        raise ValueError(f"parser pattern must contain kind, regex or both: {pattern!r}")

    if kind is None:
        return f".push({regex})"

    if regex is None:
        return f".push({kind})"

    return f".push({kind}, {regex})"


def getRules(config: JsonObject) -> list[JsonObject]:
    rules = config.get("rules", [])

    if not isinstance(rules, list):
        raise TypeError("rules must be a list")

    return rules


def getSubRules(rule: JsonObject) -> list[JsonObject]:
    sub_rules = rule.get("sub_rules", rule.get("subRules", []))

    if not isinstance(sub_rules, list):
        raise TypeError(f"sub_rules must be a list: {rule!r}")

    return sub_rules


def genParserRule(rule: JsonObject, tabs: int) -> str:
    indent = " " * (tabs * 4)
    body_indent = " " * ((tabs + 1) * 4)
    nested_indent = " " * ((tabs + 2) * 4)

    name = rule.get("name")
    input_type = rule.get("input", "pointer<TokenList>")
    return_type = rule.get("return")

    if not isinstance(name, str) or not name:
        raise ValueError(f"parser rule must have a non-empty name: {rule!r}")

    if not isinstance(input_type, str) or not input_type:
        raise ValueError(f"parser rule input must be a non-empty string: {rule!r}")

    if not isinstance(return_type, str) or not return_type:
        class_name = rule.get("class")

        if not isinstance(class_name, str) or not class_name:
            raise ValueError(f"parser rule must have return or class: {rule!r}")

        return_type = f"pointer<{class_name}>"

    lines: list[str] = [
        f"{indent}fun {name}(tokens: {input_type}) -> {return_type}",
        f"{indent}{{",
        f"{body_indent}if tokens == null:",
        f"{nested_indent}return null",
        "",
    ]

    for index, sub_rule in enumerate(getSubRules(rule)):
        if not isinstance(sub_rule, dict):
            raise TypeError(f"sub_rules item must be object: {sub_rule!r}")

        action = sub_rule.get("action", sub_rule.get("acion", None))
        patterns = sub_rule.get("patterns", [])

        if not isinstance(action, str) or not action:
            raise ValueError(f"parser sub rule must have a non-empty action: {sub_rule!r}")

        if not isinstance(patterns, list) or not patterns:
            raise ValueError(f"parser sub rule patterns must be a non-empty list: {sub_rule!r}")

        pattern_expr = "new PatternList()"

        for pattern in patterns:
            pattern_expr += genPatternPush(pattern)

        lines.extend([
            f"{body_indent}val pattern{index}: pointer<PatternList> = {pattern_expr}",
            "",
            f"{body_indent}if pattern{index}.canMatch(tokens, 0):",
            f"{body_indent}{{",
            f"{nested_indent}val matched{index}: pointer<TokenList> = tokens.subToken(0, pattern{index}.length())",
            "",
            f"{nested_indent}if matched{index} != null:",
            f"{nested_indent}    return {action}(matched{index})",
            f"{body_indent}}}",
            "",
        ])

    lines.extend([
        f"{body_indent}return null",
        f"{indent}}}",
    ])

    return "\n".join(lines) + "\n\n\n"


def genParserRules(rules: list[JsonObject], tabs: int) -> str:
    return "".join(genParserRule(rule, tabs) for rule in rules)


def codegen(config: JsonObject, dest: Path) -> str:
    ensure_type(config, PARSER_RULES_TYPE, "parser config")

    package_name = getPackageName(config)
    class_name = getClassName(config, dest)
    imports = getImports(config)

    sections: list[str] = [
        genFileClass(class_name, 0),
        genPackage(package_name, 0),
        genImports(imports, 0),
        genConstants(getCodeBlocks(config, "constants"), 0),
        genBlocks(getCodeBlocks(config, "others")),
        genParserRules(getRules(config), 0),
    ]

    return "".join(section for section in sections if section != "")


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Generate xlang parser source from a parser rules JSON file."
    )
    parser.add_argument(
        "-c",
        "--config",
        required=True,
        type=Path,
        help="Path to parser rules JSON file.",
    )
    parser.add_argument(
        "-d",
        "--dest",
        required=True,
        type=Path,
        help="Path to generated xlang source file.",
    )

    args = parser.parse_args()
    config = read_json(args.config)
    content = codegen(config, args.dest)
    write_text(args.dest, content)


if __name__ == "__main__":
    main()
