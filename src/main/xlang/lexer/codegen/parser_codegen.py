import argparse
import json
import sys
from pathlib import Path


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
    "xlang.parser.ParsedObject",
    "xlang.util.ArrayList",
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
        return f".pushRegex({json.dumps(pattern)})"

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
        return f".pushRegex({regex})"

    if regex is None:
        return f".pushRegex({kind})"

    return f".pushRegex({kind}, {regex})"


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


def upperSnake(value: str) -> str:
    result: list[str] = []
    last_is_lower_or_digit = False
    last_is_underscore = False

    for ch in value:
        if ch == "_" or ch == "-" or ch == " ":
            if result and not last_is_underscore:
                result.append("_")

            last_is_lower_or_digit = False
            last_is_underscore = True
            continue

        if ch.isupper() and last_is_lower_or_digit and not last_is_underscore:
            result.append("_")

        result.append(ch.upper())
        last_is_lower_or_digit = ch.islower() or ch.isdigit()
        last_is_underscore = False

    while result and result[-1] == "_":
        result.pop()

    return "".join(result)


def getRuleName(rule: JsonObject) -> str:
    name = rule.get("name")

    if not isinstance(name, str) or not name:
        raise ValueError(f"parser rule must have a non-empty name: {rule!r}")

    return name


def getRuleInputType(rule: JsonObject) -> str:
    input_type = rule.get("input", "pointer<TokenList>")

    if not isinstance(input_type, str) or not input_type:
        raise ValueError(f"parser rule input must be a non-empty string: {rule!r}")

    return input_type


def getRuleReturnType(rule: JsonObject) -> str:
    return_type = rule.get("return")

    if isinstance(return_type, str) and return_type:
        return return_type

    class_name = rule.get("class")

    if not isinstance(class_name, str) or not class_name:
        raise ValueError(f"parser rule must have return or class: {rule!r}")

    return f"pointer<{class_name}>"


def getParserName(rule: JsonObject) -> str:
    value = rule.get("parser", rule.get("parser_name", rule.get("parserName", None)))

    if value is not None:
        if not isinstance(value, str) or not value:
            raise ValueError(f"parser name must be a non-empty string: {rule!r}")

        return value

    class_name = rule.get("class", None)

    if isinstance(class_name, str) and class_name:
        return f"{upperSnake(class_name)}_PARSER"

    name = getRuleName(rule)

    if name.startswith("parse") and len(name) > 5:
        name = name[5:]

    return f"{upperSnake(name)}_PARSER"


def getSubRuleAction(sub_rule: JsonObject) -> str | None:
    action = sub_rule.get("action", sub_rule.get("acion", None))

    if action is None:
        return None

    if not isinstance(action, str) or not action:
        raise ValueError(f"parser sub rule action must be a non-empty string: {sub_rule!r}")

    return action


def getResultConstructor(rule: JsonObject) -> str:
    value = rule.get(
        "result_constructor",
        rule.get("resultConstructor", rule.get("constructor", None)),
    )

    if value is not None:
        if not isinstance(value, str) or not value:
            raise ValueError(f"parser result constructor must be a non-empty string: {rule!r}")

        return value

    actions: list[str] = []

    for sub_rule in getSubRules(rule):
        if not isinstance(sub_rule, dict):
            raise TypeError(f"sub_rules item must be object: {sub_rule!r}")

        action = getSubRuleAction(sub_rule)

        if action is not None and action not in actions:
            actions.append(action)

    if len(actions) == 1:
        return actions[0]

    if not actions:
        raise ValueError(f"parser rule must have result_constructor or sub rule action: {rule!r}")

    raise ValueError(
        "ParsedObject parser rules require one shared result constructor; "
        f"got multiple actions for {getRuleName(rule)!r}: {actions!r}"
    )


def validateSubRulePatterns(rule: JsonObject, sub_rule: JsonObject) -> list[object]:
    patterns = sub_rule.get("patterns", [])

    if not isinstance(patterns, list) or not patterns:
        raise ValueError(f"parser sub rule patterns must be a non-empty list: {rule!r}")

    return patterns


def genPatternListExpr(patterns: list[object]) -> str:
    pattern_expr = "new PatternList()"

    for pattern in patterns:
        pattern_expr += genPatternPush(pattern)

    return pattern_expr


def genParserDecls(rules: list[JsonObject], tabs: int) -> str:
    indent = " " * (tabs * 4)
    lines: list[str] = [
        f"{indent}var parserIsInit: bool = false",
    ]

    for index, rule in enumerate(rules):
        parser_name = getParserName(rule)
        constructor = f"parserResultConstructor{index}"
        lines.append(
            f"{indent}val {parser_name}: pointer<ParsedObject> = new ParsedObject({constructor})"
        )

    return "\n".join(lines) + "\n\n\n"


def genParserResultConstructors(rules: list[JsonObject], tabs: int) -> str:
    indent = " " * (tabs * 4)
    body_indent = " " * ((tabs + 1) * 4)
    lines: list[str] = []

    for index, rule in enumerate(rules):
        constructor = getResultConstructor(rule)
        lines.extend([
            f"{indent}private fun parserResultConstructor{index}(results: pointer<ArrayList>) -> pointer<*> =",
            f"{body_indent}{constructor}(results) as pointer<*>",
            "",
        ])

    return "\n".join(lines).rstrip() + "\n\n\n" if lines else ""


def genParserInitFun(rules: list[JsonObject], tabs: int) -> str:
    indent = " " * (tabs * 4)
    body_indent = " " * ((tabs + 1) * 4)
    lines: list[str] = [
        f"{indent}private fun parserInit()",
        f"{indent}{{",
    ]

    for index, rule in enumerate(rules):
        parser_name = getParserName(rule)
        local_parser_name = f"parser{index}"
        lines.append(f"{body_indent}val {local_parser_name}: pointer<ParsedObject> = {parser_name}")

        for sub_rule in getSubRules(rule):
            if not isinstance(sub_rule, dict):
                raise TypeError(f"sub_rules item must be object: {sub_rule!r}")

            pattern_expr = genPatternListExpr(validateSubRulePatterns(rule, sub_rule))
            lines.append(f"{body_indent}{local_parser_name}.addRule({pattern_expr})")

    lines.append(f"{body_indent}parserIsInit = true")
    lines.append(f"{indent}}}")

    return "\n".join(lines) + "\n\n\n"


def genParserRule(rule: JsonObject, tabs: int) -> str:
    indent = " " * (tabs * 4)
    body_indent = " " * ((tabs + 1) * 4)
    nested_indent = " " * ((tabs + 2) * 4)
    name = getRuleName(rule)
    input_type = getRuleInputType(rule)
    return_type = getRuleReturnType(rule)
    parser_name = getParserName(rule)

    lines: list[str] = [
        f"{indent}fun {name}(tokens: {input_type}) -> {return_type}",
        f"{indent}{{",
        f"{body_indent}if !parserIsInit:",
        f"{nested_indent}parserInit()",
        "",
        f"{body_indent}if tokens == null:",
        f"{nested_indent}return null",
        "",
        f"{body_indent}val parser: pointer<ParsedObject> = {parser_name}",
        "",
        f"{body_indent}if parser.parse(tokens, 0) <= 0:",
        f"{nested_indent}return null",
        "",
        f"{body_indent}return parser.getResult() as {return_type}",
    ]

    lines.extend([
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
        genParserResultConstructors(getRules(config), 0),
        genParserDecls(getRules(config), 0),
        genParserInitFun(getRules(config), 0),
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
