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


def getRuleClass(rule: JsonObject) -> str:
    class_name = rule.get("class")

    if not isinstance(class_name, str) or not class_name:
        raise ValueError(f"parser rule must have a non-empty class: {rule!r}")

    return class_name


def getRuleLabel(rule: JsonObject) -> str:
    parser_name = rule.get("parser", rule.get("parser_name", rule.get("parserName", None)))

    if isinstance(parser_name, str) and parser_name:
        return parser_name

    class_name = rule.get("class")

    if isinstance(class_name, str) and class_name:
        return class_name

    return repr(rule)


def getParserName(rule: JsonObject) -> str:
    value = rule.get("parser", rule.get("parser_name", rule.get("parserName", None)))

    if value is not None:
        if not isinstance(value, str) or not value:
            raise ValueError(f"parser name must be a non-empty string: {rule!r}")

        return value

    return f"{upperSnake(getRuleClass(rule))}_PARSER"


def getRuleValueName(rule: JsonObject, index: int) -> str:
    parser_name = getParserName(rule)

    if parser_name.endswith("_PARSER"):
        return f"{parser_name[:-7]}_RULE{index}"

    return f"{parser_name}_RULE{index}"


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
        f"got multiple actions for {getRuleLabel(rule)!r}: {actions!r}"
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
    lines: list[str] = []

    for index, rule in enumerate(rules):
        sub_rules = getSubRules(rule)

        if not sub_rules:
            raise ValueError(f"parser rule must have at least one sub_rule: {rule!r}")

        for sub_rule_index, sub_rule in enumerate(sub_rules):
            if not isinstance(sub_rule, dict):
                raise TypeError(f"sub_rules item must be object: {sub_rule!r}")

            rule_name = getRuleValueName(rule, sub_rule_index)
            pattern_expr = genPatternListExpr(validateSubRulePatterns(rule, sub_rule))
            lines.append(f"{indent}val {rule_name}: pointer<PatternList> = {pattern_expr}")

        parser_name = getParserName(rule)
        constructor = getResultConstructor(rule)
        parser_expr = f"new ParsedObject({constructor})"

        for sub_rule_index, _ in enumerate(sub_rules):
            parser_expr += f".addRule({getRuleValueName(rule, sub_rule_index)})"

        lines.append(
            f"{indent}val {parser_name}: pointer<ParsedObject> = {parser_expr}"
        )

    return "\n".join(lines) + "\n\n\n"


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
        genParserDecls(getRules(config), 0),
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
