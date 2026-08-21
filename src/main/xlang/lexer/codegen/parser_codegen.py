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
RECURSIVELY_DOWN_PARSER: str = "recursively-down"
PRATT_PARSER: str = "pratt"

DEFAULT_IMPORTS: set[str] = {
    "xlang.lexer.TokenList",
    "xlang.parser.ParseContainer",
    "xlang.parser.PrattParser",
    "xlang.parser.util.ParserRef",
    "xlang.parser.util.ParserRefs",
    "xlang.parser.util.PatternList",
    "xlang.parser.util.Rule",
    "xlang.util.ArrayList",
}


def getImports(config: JsonObject) -> set[str]:
    imports = DEFAULT_IMPORTS | set(item for item in config.get("imports", []) if item)

    if hasOperations(config):
        imports.add("xlang.Operation")

    return imports


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

    if kind is not None and kind.startswith("[$") and kind.endswith("]"):
        if regex is not None:
            raise ValueError(f"parser list reference pattern cannot also contain regex: {pattern!r}")

        parser_name = f"{upperSnake(kind[2:-1])}_PARSER"
        return f".pushRefs(new ParserRefs({parser_name}))"
    if kind is not None and kind.startswith("$"):
        if regex is not None:
            raise ValueError(f"parser reference pattern cannot also contain regex: {pattern!r}")

        parser_name = f"{upperSnake(kind[1:])}_PARSER"
        return f".pushRef({parser_name})"

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


def getOperations(config: JsonObject) -> list[JsonObject]:
    operations = config.get("operations", [])

    if not isinstance(operations, list):
        raise TypeError("operations must be a list")

    for operation in operations:
        if not isinstance(operation, dict):
            raise TypeError(f"operations item must be object: {operation!r}")

    return operations


def getRuleOperations(rule: JsonObject) -> list[JsonObject]:
    operations = rule.get("operations", [])

    if not isinstance(operations, list):
        raise TypeError(f"rule operations must be a list: {rule!r}")

    for operation in operations:
        if not isinstance(operation, dict):
            raise TypeError(f"rule operations item must be object: {operation!r}")

    return operations


def getSubRuleOperation(sub_rule: JsonObject) -> object:
    return sub_rule.get("operation", sub_rule.get("operator", None))


def hasOperations(config: JsonObject) -> bool:
    if getOperations(config):
        return True

    for rule in getRules(config):
        if getRuleOperations(rule):
            return True

        if getParserType(rule) != PRATT_PARSER:
            continue

        sub_rules = rule.get("sub_rules", rule.get("subRules", []))

        if not isinstance(sub_rules, list):
            continue

        for sub_rule in sub_rules:
            if isinstance(sub_rule, dict) and getSubRuleOperation(sub_rule) is not None:
                return True

    return False


def getOperationSymbol(operation: JsonObject) -> str:
    symbol = operation.get("symbol")

    if not isinstance(symbol, str) or not symbol:
        raise ValueError(f"operation must have a non-empty symbol: {operation!r}")

    return symbol


def genOperationFunctionName(operation: JsonObject) -> str:
    value = operation.get(
        "function_name",
        operation.get("functionName", operation.get("lowering_name", None)),
    )

    if value is None:
        return "null"

    if not isinstance(value, str) or not value:
        raise ValueError(f"operation function_name must be a non-empty string or null: {operation!r}")

    return json.dumps(value)


def genOperationExpr(value: object, name: str, operation: JsonObject) -> str:
    if not isinstance(value, str) or not value:
        raise ValueError(f"operation {name} must be a non-empty xlang expression: {operation!r}")

    return value


def genOperationPriority(operation: JsonObject) -> int:
    priority = operation.get("priority")

    if not isinstance(priority, int):
        raise TypeError(f"operation priority must be an int: {operation!r}")

    return priority


def genOperationId(operation: JsonObject, default_id: int) -> int:
    value = operation.get("id", default_id)

    if not isinstance(value, int):
        raise TypeError(f"operation id must be an int: {operation!r}")

    return value


def genOperationValueName(index: int) -> str:
    return f"OPERATION{index}"


def genPrattOperationValueName(rule: JsonObject, index: int) -> str:
    parser_name = getParserName(rule)

    if parser_name.endswith("_PARSER"):
        return f"{parser_name[:-7]}_OPERATION{index}"

    return f"{parser_name}_OPERATION{index}"


def getOperationValueName(operation: JsonObject, default_name: str) -> str:
    value = operation.get("name", operation.get("value_name", operation.get("valueName", None)))

    if value is None:
        return default_name

    if not isinstance(value, str) or not value:
        raise ValueError(f"operation name must be a non-empty string: {operation!r}")

    return value


def genOperationDecl(name: str, operation: JsonObject, default_id: int, tabs: int) -> str:
    indent = " " * (tabs * 4)

    return (
        f"{indent}val {name}: pointer<Operation> = "
        f"new Operation("
        f"{genOperationId(operation, default_id)}, "
        f"{json.dumps(getOperationSymbol(operation))}, "
        f"{genOperationExpr(operation.get('fixity'), 'fixity', operation)}, "
        f"{genOperationExpr(operation.get('associativity'), 'associativity', operation)}, "
        f"{genOperationPriority(operation)}, "
        f"{genOperationFunctionName(operation)})"
    )


def genOperationDecls(operations: list[JsonObject], tabs: int) -> str:
    if not operations:
        return ""

    indent = " " * (tabs * 4)
    lines: list[str] = []

    for index, operation in enumerate(operations):
        name = getOperationValueName(operation, genOperationValueName(index))
        lines.append(genOperationDecl(name, operation, index, tabs))

    return "\n".join(lines) + "\n\n\n"


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
    parser_name = rule.get("parser_name", rule.get("parserName", None))

    if isinstance(parser_name, str) and parser_name:
        return parser_name

    class_name = rule.get("class")

    if isinstance(class_name, str) and class_name:
        return class_name

    return repr(rule)


def getParserName(rule: JsonObject) -> str:
    value = rule.get("parser_name", rule.get("parserName", None))

    if value is not None:
        if not isinstance(value, str) or not value:
            raise ValueError(f"parser name must be a non-empty string: {rule!r}")

        return value

    return f"{upperSnake(getRuleClass(rule))}_PARSER"


def getParserIdName(rule: JsonObject) -> str:
    return f"{getParserName(rule)}_ID"


def genParserIdDecls(rules: list[JsonObject], tabs: int) -> str:
    if not rules:
        return ""

    indent = " " * (tabs * 4)
    lines: list[str] = []

    for index, rule in enumerate(rules):
        lines.append(f"{indent}private val {getParserIdName(rule)}: int = {index + 1}")

    return "\n".join(lines) + "\n\n\n"

def getParserType(rule: JsonObject) -> str:
    parser_type = rule.get("parser", RECURSIVELY_DOWN_PARSER)

    if not isinstance(parser_type, str) or not parser_type:
        raise ValueError(f"parser type must be a non-empty string: {rule!r}")

    return parser_type


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


def getSubRuleRole(sub_rule: JsonObject) -> str:
    role = sub_rule.get("role", "starter")

    if not isinstance(role, str) or not role:
        raise ValueError(f"parser sub rule role must be a non-empty string: {sub_rule!r}")

    if role not in {"starter", "continuation"}:
        raise ValueError(f"unsupported parser sub rule role: {role!r}")

    return role


def genSubRuleRoleExpr(sub_rule: JsonObject) -> str:
    role = getSubRuleRole(sub_rule)

    if role == "starter":
        return "Rule.STARTER_ROLE"

    return "Rule.CONTINUATION_ROLE"


def getSubRulePriority(sub_rule: JsonObject) -> int:
    priority = sub_rule.get("priority", 0)

    if not isinstance(priority, int):
        raise TypeError(f"parser sub rule priority must be an int: {sub_rule!r}")

    return priority

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
        "RecursiveParser rules require one shared result constructor; "
        f"got multiple actions for {getRuleLabel(rule)!r}: {actions!r}"
    )


def getPrattResultConstructor(rule: JsonObject) -> str:
    value = rule.get(
        "result_constructor",
        rule.get("resultConstructor", rule.get("constructor", None)),
    )

    if value is not None:
        if not isinstance(value, str) or not value:
            raise ValueError(f"pratt parser result constructor must be a non-empty string: {rule!r}")

        return value

    for sub_rule in getSubRules(rule):
        if not isinstance(sub_rule, dict):
            raise TypeError(f"sub_rules item must be object: {sub_rule!r}")

        action = getSubRuleAction(sub_rule)

        if action is not None:
            return action

    raise ValueError(f"pratt parser rule must have result_constructor or sub rule action: {rule!r}")


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


def commentLine(line: str, tabs: int) -> str:
    indent = " " * (tabs * 4)

    return f"{indent}// {line[len(indent):]}"


def getPrattSubRuleOperation(rule: JsonObject, sub_rule: JsonObject, index: int) -> str:
    operation = getSubRuleOperation(sub_rule)

    if operation is None:
        return "null"

    if isinstance(operation, dict):
        return getOperationValueName(operation, genPrattOperationValueName(rule, index))

    if not isinstance(operation, str) or not operation:
        raise ValueError(f"pratt sub rule operation must be a non-empty operation reference or null: {sub_rule!r}")

    function_matches: list[str] = []
    fallback_matches: list[str] = []

    for operation_index, operation_def in enumerate(getRuleOperations(rule)):
        operation_name = getOperationValueName(
            operation_def,
            genPrattOperationValueName(rule, operation_index),
        )
        function_name = operation_def.get(
            "function_name",
            operation_def.get("functionName", operation_def.get("lowering_name", None)),
        )

        if function_name == operation:
            function_matches.append(operation_name)

        if operation == operation_name or operation == getOperationSymbol(operation_def):
            fallback_matches.append(operation_name)

    matches = function_matches if function_matches else fallback_matches

    if len(matches) == 1:
        return matches[0]

    if len(matches) > 1:
        raise ValueError(f"ambiguous pratt sub rule operation {operation!r}: {sub_rule!r}")

    raise ValueError(f"unknown pratt sub rule operation {operation!r}: {sub_rule!r}")

def getOperationPriorityMap(rule: JsonObject) -> dict[str, int]:
    result: dict[str, int] = {}

    for operation_index, operation in enumerate(getRuleOperations(rule)):
        name = getOperationValueName(
            operation,
            genPrattOperationValueName(rule, operation_index),
        )
        result[name] = genOperationPriority(operation)

    return result


def getPrattSubRulePriority(rule: JsonObject, sub_rule: JsonObject, operation_priorities: dict[str, int]) -> int:
    explicit_priority = sub_rule.get("priority", None)

    if explicit_priority is not None:
        if not isinstance(explicit_priority, int):
            raise TypeError(f"parser sub rule priority must be an int: {sub_rule!r}")

        return explicit_priority

    operation = getSubRuleOperation(sub_rule)

    if isinstance(operation, dict):
        return genOperationPriority(operation)

    if isinstance(operation, str) and operation in operation_priorities:
        return operation_priorities[operation]

    return 0


def genParserDecls(rules: list[JsonObject], tabs: int) -> str:
    indent = " " * (tabs * 4)
    lines: list[str] = []
    emitted_operations: set[str] = set()

    # Parser references must exist before PatternList constructors can refer to
    # parsers declared later in the JSON rule list.
    for rule in rules:
        parser_type = getParserType(rule)
        parser_id_name = getParserIdName(rule)
        parser_name = getParserName(rule)

        if parser_type not in {RECURSIVELY_DOWN_PARSER, PRATT_PARSER}:
            raise ValueError(f"unsupported parser type for {getRuleLabel(rule)!r}: {parser_type!r}")

        sub_rules = getSubRules(rule)

        if not sub_rules:
            raise ValueError(f"parser rule must have at least one sub_rule: {rule!r}")

        if parser_type == PRATT_PARSER:
            specific_parser_name = f"{parser_name}_SPECIFIC"

            lines.append(f"{indent}private val {specific_parser_name}: pointer<PrattParser> = new PrattParser()")
            lines.append(
                f"{indent}val {parser_name}: pointer<ParserRef> = "
                f"ParserRef.fromPratt({parser_id_name}, {specific_parser_name})"
            )
        else:
            lines.append(
                f"{indent}val {parser_name}: pointer<ParserRef> = "
                f"ParserRef.fromRecursiveDown({parser_id_name})"
            )

        lines.append("")

    # Operations must exist before Rule constructors store pointers to them.
    for rule in rules:
        for operation_index, operation in enumerate(getRuleOperations(rule)):
            operation_expr = getOperationValueName(
                operation,
                genPrattOperationValueName(rule, operation_index),
            )

            if operation_expr not in emitted_operations:
                lines.append(genOperationDecl(operation_expr, operation, operation_index, tabs))
                emitted_operations.add(operation_expr)

        if getParserType(rule) != PRATT_PARSER:
            continue

        for sub_rule_index, sub_rule in enumerate(getSubRules(rule)):
            if not isinstance(sub_rule, dict):
                raise TypeError(f"sub_rules item must be object: {sub_rule!r}")

            operation = getSubRuleOperation(sub_rule)

            if not isinstance(operation, dict):
                continue

            operation_expr = getPrattSubRuleOperation(rule, sub_rule, sub_rule_index)

            if operation_expr not in emitted_operations:
                lines.append(genOperationDecl(operation_expr, operation, sub_rule_index, tabs))
                emitted_operations.add(operation_expr)

    if emitted_operations:
        lines.append("")

    # Rules can now safely reference any parser, including a later or cyclic one.
    for rule in rules:
        parser_type = getParserType(rule)
        sub_rules = getSubRules(rule)

        if parser_type == PRATT_PARSER:
            operation_priorities = getOperationPriorityMap(rule)

            for sub_rule_index, sub_rule in enumerate(sub_rules):
                if not isinstance(sub_rule, dict):
                    raise TypeError(f"sub_rules item must be object: {sub_rule!r}")

                rule_name = getRuleValueName(rule, sub_rule_index)
                pattern_expr = genPatternListExpr(validateSubRulePatterns(rule, sub_rule))
                operation = getSubRuleOperation(sub_rule)
                operation_expr = getPrattSubRuleOperation(rule, sub_rule, sub_rule_index)
                action = getSubRuleAction(sub_rule)
                priority = getPrattSubRulePriority(rule, sub_rule, operation_priorities)
                role_expr = genSubRuleRoleExpr(sub_rule)

                if action is None:
                    raise ValueError(f"pratt sub rule must have an action: {sub_rule!r}")

                if operation is None:
                    rule_ctor = f"new Rule({pattern_expr}, {action}, {role_expr}, {priority})"
                else:
                    rule_ctor = f"new Rule({pattern_expr}, {action}, {role_expr}, {operation_expr})"

                lines.append(
                    f"{indent}private val {rule_name}: pointer<Rule> = {rule_ctor}"
                )

            lines.append("")
            continue

        for sub_rule_index, sub_rule in enumerate(sub_rules):
            if not isinstance(sub_rule, dict):
                raise TypeError(f"sub_rules item must be object: {sub_rule!r}")

            rule_name = getRuleValueName(rule, sub_rule_index)
            pattern_expr = genPatternListExpr(validateSubRulePatterns(rule, sub_rule))
            action = getSubRuleAction(sub_rule)
            priority = getSubRulePriority(sub_rule)

            if action is None:
                raise ValueError(f"recursive parser sub rule must have an action: {sub_rule!r}")

            lines.append(
                f"{indent}private val {rule_name}: pointer<Rule> = "
                f"new Rule({pattern_expr}, {action}, Rule.STARTER_ROLE, {priority})"
            )

        lines.append("")

    # Registration is last so no parser observes a partially initialized rule.
    for rule in rules:
        parser_name = getParserName(rule)
        sub_rules = getSubRules(rule)
        setup_expr = parser_name

        for sub_rule_index, _ in enumerate(sub_rules):
            setup_expr += f".addRule({getRuleValueName(rule, sub_rule_index)})"

        lines.append(
            f"{indent}private val {parser_name}_SETUP: pointer<ParserRef> = {setup_expr}"
        )

    return "\n".join(lines).rstrip() + "\n\n\n"

def getParseFunctionName(rule: JsonObject) -> str:
    value = rule.get("parse_function", rule.get("parseFunction", None))

    if value is not None:
        if not isinstance(value, str) or not value:
            raise ValueError(f"parse function name must be a non-empty string: {rule!r}")

        return value

    return f"parse{getRuleClass(rule)}"


def getParserEntry(rule: JsonObject) -> str | None:
    value = rule.get("entry", rule.get("parser_entry", rule.get("parserEntry", None)))

    if value is None:
        return None

    if not isinstance(value, str) or not value:
        raise ValueError(f"parser entry must be a non-empty string: {rule!r}")

    return value


def genParseEntrypoints(rules: list[JsonObject], tabs: int) -> str:
    indent = " " * (tabs * 4)
    lines: list[str] = []

    for rule in rules:
        parser_type = getParserType(rule)
        function_name = getParseFunctionName(rule)
        result_type = getRuleClass(rule)

        if parser_type == RECURSIVELY_DOWN_PARSER or parser_type == PRATT_PARSER:
            parser_name = getParserName(rule)
            parser_id_name = getParserIdName(rule)
            parser_entry_name = f"{parser_name}_SETUP" if parser_type == PRATT_PARSER else parser_name
            entrypoint = [
                f"{indent}fun {function_name}(input: pointer<TokenList>) -> pointer<{result_type}>",
                f"{indent}{{",
                f"{indent}    if input == null:",
                f"{indent}        return null",
                "",
                f"{indent}    if {parser_entry_name}.doParse(input) < 0:",
                f"{indent}        return null",
                "",
                f"{indent}    val result: pointer<ParseContainer> = {parser_entry_name}.getResult()",
                "",
                f"{indent}    if result == null || result.isKind({parser_id_name}) == false:",
                f"{indent}        return null",
                "",
                f"{indent}    return result.getValue() as pointer<{result_type}>",
                f"{indent}}}",
                "",
            ]

            lines.extend(entrypoint)
            continue

        entry = getParserEntry(rule)

        if entry is not None:
            lines.extend([
                f"{indent}fun {function_name}(input: pointer<TokenList>) -> pointer<{result_type}> =",
                f"{indent}    {entry}(input)",
                "",
            ])
            continue

        raise ValueError(f"unsupported parser type for {getRuleLabel(rule)!r}: {parser_type!r}")

    if not lines:
        return ""

    return "\n".join(lines).rstrip() + "\n\n\n"


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
        genOperationDecls(getOperations(config), 0),
        genParserIdDecls(getRules(config), 0),
        genBlocks(getCodeBlocks(config, "others")),
        genParserDecls(getRules(config), 0),
        genParseEntrypoints(getRules(config), 0),
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





