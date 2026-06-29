from pathlib import Path
import argparse
import json
from typing import Any


JsonObject = dict[str, Any]
CodeBlock = str | list[str]
TokenDef = dict[str, str]
Defs = dict[str, str]


SYMBOLS_PATTERN_MACRO = "$symbols"
SYMBOL_ACTION_MACRO = "$eatSymbol"


DEFAULT_IMPORTS: set[str] = {
    "xlang.lex.Lex",
    "xlang.lex.LexState",
    "xlang.lex.LexPosition",
    "xlang.lex.Token",
    "xlang.lex.TokenizerHelper",
    "xlang.lex.TokenPosition"}


def read_json(path: Path) -> JsonObject:
    with path.open("r", encoding="utf-8") as file:
        return json.load(file)


def write_text(path: Path, content: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content, encoding="utf-8")


def getPackageName(rules: JsonObject) -> str:
    return rules.get("package", rules.get("package: ", "xlang.lex"))


def getClassName(rules: JsonObject, dest: Path) -> str:
    return rules.get("class", rules.get("class_name", dest.stem))


def getImports(rules: JsonObject) -> set[str]:
    imports = DEFAULT_IMPORTS | set(rules.get("imports", []))

    if rules.get("token_defs", []):
        imports.add("xlang.util.string.String")

    return imports


def alignTo(value: int, alignment: int) -> int:
    return ((value + alignment - 1) // alignment) * alignment


def getTokenDefs(rules: JsonObject) -> list[TokenDef]:
    token_defs = rules.get("token_defs", [])

    if not isinstance(token_defs, list):
        raise TypeError("token_defs must be a list")

    result: list[TokenDef] = []
    names: set[str] = set()
    patterns: set[str] = set()

    for item in token_defs:
        if not isinstance(item, dict):
            raise TypeError(f"token_defs item must be object: {item!r}")

        name = item.get("name", "")
        pattern = item.get("pattern", "")

        if not isinstance(name, str) or not name:
            raise ValueError(f"invalid token def name: {name!r}")

        if not isinstance(pattern, str) or not pattern:
            raise ValueError(f"invalid token def pattern for {name!r}: {pattern!r}")

        if name in names:
            raise ValueError(f"duplicate token def name: {name}")

        if pattern in patterns:
            raise ValueError(f"duplicate token def pattern: {pattern}")

        names.add(name)
        patterns.add(pattern)
        result.append({"name": name, "pattern": pattern})

    return sorted(result, key=lambda item: item["pattern"])


def getTokenDefStart(rules: JsonObject) -> int:
    token_def_start = rules.get("token_def_start", 100)

    if not isinstance(token_def_start, int):
        raise TypeError(f"token_def_start must be int: {token_def_start!r}")

    return token_def_start


def getSymbolDefs(rules: JsonObject) -> list[TokenDef]:
    symbol_defs = rules.get("symbol_defs", [])

    if not isinstance(symbol_defs, list):
        raise TypeError("symbol_defs must be a list")

    result: list[TokenDef] = []
    names: set[str] = set()
    patterns: set[str] = set()

    for item in symbol_defs:
        if not isinstance(item, dict):
            raise TypeError(f"symbol_defs item must be object: {item!r}")

        name = item.get("name", "")
        pattern = item.get("pattern", "")

        if not isinstance(name, str) or not name:
            raise ValueError(f"invalid symbol def name: {name!r}")

        if not isinstance(pattern, str) or not pattern:
            raise ValueError(f"invalid symbol def pattern for {name!r}: {pattern!r}")

        if name in names:
            raise ValueError(f"duplicate symbol def name: {name}")

        if pattern in patterns:
            raise ValueError(f"duplicate symbol def pattern: {pattern}")

        names.add(name)
        patterns.add(pattern)
        result.append({"name": name, "pattern": pattern})

    return result


def getSymbolDefStart(rules: JsonObject) -> int:
    symbol_def_start = rules.get("symbol_def_start", 200)

    if not isinstance(symbol_def_start, int):
        raise TypeError(f"symbol_def_start must be int: {symbol_def_start!r}")

    return symbol_def_start


def getDefs(rules: JsonObject) -> Defs:
    defs: Defs = {}

    for item in rules.get("defs", []):
        key = item.get("key", "")
        value = item.get("pattern", "")

        if not isinstance(key, str) or not key:
            raise ValueError(f"invalid def key: {key!r}")

        if not (key[0] == "_" or key[0].isalpha()) or any(
            not (ch == "_" or ch.isalnum()) for ch in key
        ):
            raise ValueError(f"invalid def key syntax: {key!r}")

        if key in defs:
            raise ValueError(f"duplicate def key: {key}")

        if not isinstance(value, str):
            raise ValueError(f"def value must be string for key {key!r}")

        defs[key] = expandPattern(value, defs, (key,))

    return defs


def expandPattern(pattern: str, defs: Defs, stack: tuple[str, ...] = ()) -> str:
    result: list[str] = []
    i = 0

    while i < len(pattern):
        if pattern[i] == "\\" and i + 1 < len(pattern):
            result.append(pattern[i])
            result.append(pattern[i + 1])
            i += 2
            continue

        if pattern[i] != "$":
            result.append(pattern[i])
            i += 1
            continue

        key_start = i + 1

        if key_start >= len(pattern) or not (
            pattern[key_start] == "_" or pattern[key_start].isalpha()
        ):
            raise ValueError(f"invalid def reference in pattern: {pattern!r}")

        key_end = key_start + 1

        while key_end < len(pattern) and (
            pattern[key_end] == "_" or pattern[key_end].isalnum()
        ):
            key_end += 1

        key = pattern[key_start:key_end]

        if key not in defs:
            raise ValueError(f"unknown def reference ${key} in pattern: {pattern!r}")

        if key in stack:
            chain = " -> ".join((*stack, key))
            raise ValueError(f"recursive def reference: {chain}")

        result.append(expandPattern(defs[key], defs, (*stack, key)))
        i = key_end

    return "".join(result)


def regexLiteral(pattern: str) -> str:
    special_chars = set("\\.^$|?*+()[]{}")
    result: list[str] = []

    for ch in pattern:
        if ch in special_chars:
            result.append("\\")

        result.append(ch)

    return "".join(result)


def getRulePatterns(rule: JsonObject) -> list[str]:
    patterns = rule["patterns"] if "patterns" in rule else [rule["pattern"]]

    if not isinstance(patterns, list) or any(not isinstance(pattern, str) for pattern in patterns):
        raise ValueError(f"rule patterns must be a string list: {rule!r}")

    return patterns


def symbolActionName(symbol_def: TokenDef) -> str:
    pascal_name = "".join(part.lower().capitalize() for part in symbol_def["name"].split("_"))
    return f"eat{pascal_name}"


def isSymbolMacroRule(rule: JsonObject) -> bool:
    patterns = getRulePatterns(rule)
    action = rule.get("action")
    uses_symbol_pattern = len(patterns) == 1 and patterns[0] == SYMBOLS_PATTERN_MACRO
    uses_symbol_action = action == SYMBOL_ACTION_MACRO

    if uses_symbol_pattern != uses_symbol_action:
        raise ValueError(
            f"symbol macro rule must use both {SYMBOLS_PATTERN_MACRO!r} and {SYMBOL_ACTION_MACRO!r}: {rule!r}"
        )

    return uses_symbol_pattern and uses_symbol_action


def expandRules(rules: list[JsonObject], symbol_defs: list[TokenDef]) -> list[JsonObject]:
    result: list[JsonObject] = []

    for rule in rules:
        if not isSymbolMacroRule(rule):
            result.append(rule)
            continue

        state = rule["state"]

        for symbol_def in symbol_defs:
            result.append({
                "state": state,
                "patterns": [regexLiteral(symbol_def["pattern"])],
                "action": symbolActionName(symbol_def),
            })

    return result


def genFileClass(class_name: str, tabs: int) -> str:
    indent = " " * (tabs * 4)

    return f'{indent}@file.class("{class_name}")\n'


def genPackage(package: str, tabs: int) -> str:
    indent = " " * (tabs * 4)

    return f"{indent}package {package}\n\n\n"


def genImports(imports: set[str], tabs: int) -> str:
    indent = " " * (tabs * 4)

    return "\n".join(f"{indent}import {item}" for item in sorted(imports)) + "\n\n\n"


def genConstants(constants: list[CodeBlock], tabs: int) -> str:
    if not constants:
        return ""

    indent = " " * (tabs * 4)
    lines: list[str] = []

    for item in constants:
        if isinstance(item, str):
            lines.append(f"{indent}{item}")
        elif isinstance(item, list):
            lines.extend(f"{indent}{line}" for line in item)
        else:
            raise TypeError(f"constants item must be string or string list: {item!r}")

    return "\n".join(lines) + "\n\n\n"


def genTokenDefs(token_defs: list[TokenDef], token_def_start: int, tabs: int) -> str:
    if not token_defs:
        return ""

    indent = " " * (tabs * 4)
    body_indent = " " * ((tabs + 1) * 4)
    max_pattern_length = max(len(item["pattern"]) for item in token_defs)
    slot_size = alignTo(max_pattern_length + 1, 8)
    token_def_length = len(token_defs)
    text_space_size = slot_size * token_def_length
    lines: list[str] = []

    lines.append(f"{indent}val tokenDefStart: int = {token_def_start}")

    for index, item in enumerate(token_defs):
        lines.append(f"{indent}val {item['name']}: int = {token_def_start + index}")

    lines.extend([
        "",
        "",
        f"{indent}val keywordListLength: int = {token_def_length}",
        f"{indent}val keywordListSlotSize: int = {slot_size}",
        f"{indent}val keywordTextSpace: blob[{text_space_size}]",
        f"{indent}val keywordTextList: pointer<char> = keywordTextSpace as pointer<char>",
        "",
        "",
        f"{indent}private inline fun getKeywordText(index: int) -> pointer<char> =",
        f"{body_indent}keywordTextList + index * keywordListSlotSize",
        "",
        "",
        f"{indent}private fun keywordListInit()",
        f"{indent}{{",
    ])

    for index, item in enumerate(token_defs):
        pattern = json.dumps(item["pattern"])
        lines.append(f"{body_indent}String.strcpy(getKeywordText({index}), {pattern} as pointer<char>)")

    lines.extend([
        f"{indent}}}",
    ])

    return "\n".join(lines) + "\n\n\n"


def genSymbolDefs(symbol_defs: list[TokenDef], symbol_def_start: int, tabs: int) -> str:
    if not symbol_defs:
        return ""

    indent = " " * (tabs * 4)
    lines: list[str] = [
        f"{indent}val symbolDefStart: int = {symbol_def_start}",
    ]

    for index, item in enumerate(symbol_defs):
        lines.append(f"{indent}val {item['name']}: int = {symbol_def_start + index}")

    lines.append("")
    lines.append("")

    for item in symbol_defs:
        pattern = json.dumps(item["pattern"])
        lines.append(f"{indent}val {item['name']}_PATTERN: pointer<char> = {pattern} as pointer<char>")

    return "\n".join(lines) + "\n\n\n"


def genRules(rules: list[JsonObject], defs: Defs, tabs: int) -> str:
    indent = " " * (tabs * 4)
    lines: list[str] = []

    total_rule_size: int = len(rules)

    lines.append(f"{indent}var tokenizerIsInit: bool = false")
    lines.append(f"{indent}val ruleLength: int = {total_rule_size}")
    lines.append(f"{indent}val rulesSpace: blob[sizeof(pointer<Rule>) * {total_rule_size}]")
    lines.append(f"{indent}val rulePtr: pointer<pointer<Rule>> = rulesSpace as pointer<pointer<Rule>>")

    for index, rule in enumerate(rules):
        state = str(rule["state"])
        patterns = getRulePatterns(rule)
        pattern = json.dumps("|".join(expandPattern(pattern, defs) for pattern in patterns))
        action = rule["action"]
        lines.append(f"{indent}val rule{index}: Rule = Rule({index}, {state}, {pattern} as pointer<char>, {action})")

    return "\n".join(lines) + "\n\n\n"


def genOthers(rules: list[CodeBlock]) -> str:
    lines: list[str] = []

    for rule in rules:
        if isinstance(rule, str):
            lines.append(rule + "\n")
        elif isinstance(rule, list):
            lines.extend(rule)
            lines.append("\n")
        else:
            raise TypeError(f"others item must be string or string list: {rule!r}")

    return "\n".join(lines) + "\n\n\n"


def genSymbolEatFunctions(symbol_defs: list[TokenDef]) -> str:
    if not symbol_defs:
        return ""

    lines: list[str] = []

    for symbol_def in symbol_defs:
        name = symbol_def["name"]
        lines.append(f"private fun {symbolActionName(symbol_def)}(input: LexInput, dest: LexState) -> Token =")
        lines.append(f"    eatToken(input, {name}, dest)")
        lines.append("")

    return "\n".join(lines) + "\n\n"


def genTokenizerInitFun(
    rules: list[JsonObject],
    init_lines: list[CodeBlock],
    token_defs: list[TokenDef],
    tabs: int
) -> str:
    indent = " " * (tabs * 4)
    body_indent = " " * ((tabs + 1) * 4)
    lines: list[str] = [
        f"{indent}private fun tokenizerInit()",
        f"{indent}{{",
    ]

    for index, _ in enumerate(rules):
        lines.append(f"{body_indent}rulePtr[{index}] = rule{index}")

    if token_defs:
        lines.append(f"{body_indent}keywordListInit()")

    for item in init_lines:
        if isinstance(item, str):
            lines.append(f"{body_indent}{item}")
        elif isinstance(item, list):
            lines.extend(f"{body_indent}{line}" for line in item)
        else:
            raise TypeError(f"init item must be string or string list: {item!r}")

    lines.append(f"{body_indent}tokenizerIsInit = true")
    lines.append(f"{indent}}}")

    return "\n".join(lines) + "\n\n\n"


def genTokenzeFun(tabs: int) -> str:
    indent = " " * (tabs * 4)
    body_indent = " " * ((tabs + 1) * 4)
    nested_indent = " " * ((tabs + 2) * 4)
    lines: list[str] = [
        f"{indent}fun tokenize(code: pointer<char>) -> TokenList",
        f"{indent}{{",
        f"{body_indent}if !tokenizerIsInit:",
        f"{nested_indent}tokenizerInit()",
        "",
    ]

    lines.append(f"{body_indent}return TokenizerHelper.tokenize(code, rulePtr, ruleLength)")
    lines.append(f"{indent}}}")

    return "\n".join(lines) + "\n\n\n"


def codegen(rules: JsonObject, dest: Path) -> str:
    package_name = getPackageName(rules)
    class_name = getClassName(rules, dest)
    imports = getImports(rules)
    defs = getDefs(rules)
    token_defs = getTokenDefs(rules)
    token_def_start = getTokenDefStart(rules)
    symbol_defs = getSymbolDefs(rules)
    symbol_def_start = getSymbolDefStart(rules)
    expanded_rules = expandRules(rules.get("rules", []), symbol_defs)

    sections: list[str] = [
        genFileClass(class_name, 0),
        genPackage(package_name, 0),
        genImports(imports, 0),
        genConstants(rules.get("constants", []), 0),
        genTokenDefs(token_defs, token_def_start, 0),
        genSymbolDefs(symbol_defs, symbol_def_start, 0),
        genRules(expanded_rules, defs, 0),
        genOthers(rules.get("others", [])),
        genSymbolEatFunctions(symbol_defs),
        genTokenizerInitFun(expanded_rules, rules.get("init", []), token_defs, 0),
        genTokenzeFun(0),
    ]

    return "".join(section for section in sections if section != "")


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Generate xlang tokenizer source from a token rules JSON file."
    )
    parser.add_argument(
        "-c",
        "--config",
        required=True,
        type=Path,
        help="Path to token rules JSON file.",
    )
    parser.add_argument(
        "-d",
        "--dest",
        required=True,
        type=Path,
        help="Path to generated xlang source file.",
    )

    args = parser.parse_args()
    rules = read_json(args.config)
    content = codegen(rules, args.dest)
    write_text(args.dest, content)


if __name__ == "__main__":
    main()
