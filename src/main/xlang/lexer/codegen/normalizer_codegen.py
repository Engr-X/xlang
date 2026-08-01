from pathlib import Path
import argparse
from itertools import product
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
from codegen.tokenizer_codegen import (
    expandPattern,
    getDefs as getTokenizerDefs,
    getSymbolDefStart,
    getSymbolDefs,
    getTokenDefStart,
    getTokenDefs,
    regexLiteral,
)


NORMALIZER_RULES_TYPE: str = "compiler_normalizer_rules"
TOKENIZER_RULES_TYPE: str = "compiler_tokenizer_rules"
PatternSpec = dict[str, int | str | None]

DEFAULT_IMPORTS: set[str] = {
    "xlang.System",
    "xlang.lexer.Token",
    "xlang.lexer.TokenList",
    "xlang.lexer.NormalizeFSM",
    "xlang.lexer.NormalizeRule",
    "xlang.util.ArrayList",
}


def getImports(config: JsonObject) -> set[str]:
    return DEFAULT_IMPORTS | set(config.get("imports", []))


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


def isIdent(value: str) -> bool:
    return (
        bool(value)
        and (value[0] == "_" or value[0].isalpha())
        and all(ch == "_" or ch.isalnum() for ch in value)
    )


def macroName(value: str) -> str | None:
    if not value.startswith("$"):
        return None

    name = value[1:-1] if value.endswith("$") and len(value) > 1 else value[1:]

    if not isIdent(name):
        return None

    return name


def getRegexDefs(tokenizer_config: JsonObject) -> dict[str, str]:
    defs = dict(getTokenizerDefs(tokenizer_config))
    symbol_defs = getSymbolDefs(tokenizer_config)
    token_defs = getTokenDefs(tokenizer_config)

    def add_def(name: str, pattern: str) -> None:
        if name in defs:
            raise ValueError(f"duplicate normalizer pattern macro: {name}")

        defs[name] = pattern

    for item in token_defs:
        add_def(item["name"], regexLiteral(item["pattern"]))

    for item in symbol_defs:
        add_def(item["name"], regexLiteral(item["pattern"]))

    if symbol_defs:
        add_def("symbols", "|".join(regexLiteral(item["pattern"]) for item in symbol_defs))

    return defs


def getPatternDefs(tokenizer_config: JsonObject) -> dict[str, PatternSpec]:
    result: dict[str, PatternSpec] = {}
    symbol_defs = getSymbolDefs(tokenizer_config)
    token_defs = getTokenDefs(tokenizer_config)
    symbol_def_start = getSymbolDefStart(tokenizer_config)
    token_def_start = getTokenDefStart(tokenizer_config)

    def add_def(name: str, kind: int, pattern: str) -> None:
        if name in result:
            raise ValueError(f"duplicate normalizer token pattern macro: {name}")

        result[name] = {
            "kind": kind,
            "pattern": pattern,
        }

    for index, item in enumerate(token_defs):
        add_def(item["name"], token_def_start + index, regexLiteral(item["pattern"]))

    for index, item in enumerate(symbol_defs):
        add_def(item["name"], symbol_def_start + index, regexLiteral(item["pattern"]))

    return result


def getPatternGroups(tokenizer_config: JsonObject) -> dict[str, list[PatternSpec]]:
    symbol_defs = getSymbolDefs(tokenizer_config)
    symbol_def_start = getSymbolDefStart(tokenizer_config)

    return {
        "symbols": [
            {
                "kind": symbol_def_start + index,
                "pattern": regexLiteral(item["pattern"]),
            }
            for index, item in enumerate(symbol_defs)
        ],
    }


def getKindValue(kind: object) -> str:
    if isinstance(kind, str) and kind:
        return kind

    raise ValueError(f"normalizer pattern kind must be string: {kind!r}")


def genKindExpr(kind: object) -> str:
    if isinstance(kind, str) and kind:
        return kind

    if not isinstance(kind, int):
        raise TypeError(f"generated normalizer kind must be int or string: {kind!r}")

    return str(kind)


def getRegexPatternOptions(
    pattern: str,
    pattern_groups: dict[str, list[PatternSpec]],
    regex_defs: dict[str, str],
) -> list[str]:
    name = macroName(pattern)

    if name is not None and name in pattern_groups:
        result: list[str] = []

        for item in pattern_groups[name]:
            regex_pattern = item["pattern"]

            if not isinstance(regex_pattern, str):
                raise TypeError(f"pattern group {name!r} contains non-regex pattern: {item!r}")

            result.append(regex_pattern)

        return result

    return [expandPattern(pattern, regex_defs)]


def getRulePatternOptions(
    pattern: object,
    pattern_defs: dict[str, PatternSpec],
    pattern_groups: dict[str, list[PatternSpec]],
    regex_defs: dict[str, str]
) -> list[PatternSpec]:
    if isinstance(pattern, str):
        name = macroName(pattern)

        if name is not None and name in pattern_groups:
            return [dict(item) for item in pattern_groups[name]]

        if name is None or name not in pattern_defs:
            raise ValueError(
                "normalizer string pattern must be a tokenizer token or symbol macro: "
                f"{pattern!r}"
            )

        return [dict(pattern_defs[name])]

    if isinstance(pattern, dict):
        kind = getKindValue(pattern.get("kind", None))
        regex_pattern = pattern.get("pattern", None)

        if regex_pattern is None:
            return [{
                "kind": kind,
                "pattern": None,
            }]

        if not isinstance(regex_pattern, str):
            raise ValueError(f"normalizer pattern regex must be string or null: {pattern!r}")

        return [
            {
                "kind": kind,
                "pattern": item,
            }
            for item in getRegexPatternOptions(regex_pattern, pattern_groups, regex_defs)
        ]

    raise TypeError(f"normalizer pattern must be string or object: {pattern!r}")


def getRules(
    config: JsonObject,
    pattern_defs: dict[str, PatternSpec],
    pattern_groups: dict[str, list[PatternSpec]],
    regex_defs: dict[str, str]
) -> list[JsonObject]:
    rules = config.get("rules", [])

    if not isinstance(rules, list):
        raise TypeError("rules must be a list")

    result: list[JsonObject] = []

    for index, rule in enumerate(rules):
        if not isinstance(rule, dict):
            raise TypeError(f"rules item must be object: {rule!r}")

        state = rule.get("state")
        action_value = rule.get("action", None)
        pivot_index = rule.get("pivot_index", rule.get("pivotIndex", 0))
        patterns = rule["patterns"] if "patterns" in rule else [rule.get("pattern")]

        if not isinstance(state, str) or not state:
            raise ValueError(f"invalid rule state at index {index}: {state!r}")

        if action_value is None:
            action = None
        elif isinstance(action_value, str) and action_value:
            action = action_value
        else:
            raise ValueError(f"invalid rule action at index {index}: {action_value!r}")

        if not isinstance(pivot_index, (int, str)):
            raise TypeError(f"pivot_index must be int or string at index {index}: {pivot_index!r}")

        if not isinstance(patterns, list):
            raise ValueError(f"rule patterns must be a list at index {index}: {rule!r}")

        if not patterns:
            raise ValueError(f"rule patterns must not be empty at index {index}: {rule!r}")

        pattern_options = [
            getRulePatternOptions(pattern, pattern_defs, pattern_groups, regex_defs)
            for pattern in patterns
        ]

        for expanded_patterns in product(*pattern_options):
            result.append({
                "state": state,
                "action": action,
                "pivot_index": str(pivot_index),
                "patterns": list(expanded_patterns),
            })

    return result


def getOptionalProcess(config: JsonObject, name: str, aliases: tuple[str, ...] = ()) -> str:
    value = ""

    for key in (name, f"{name}: ", *aliases):
        if key in config:
            value = config[key]
            break

    if value is None:
        return ""

    if not isinstance(value, str):
        raise TypeError(f"{name} must be a string or null: {value!r}")

    return value


def getPreprocess(config: JsonObject) -> str:
    return getOptionalProcess(config, "preprocess")


def getPostprocess(config: JsonObject) -> str:
    return getOptionalProcess(
        config,
        "postprocess",
        ("afterprocess", "after_process", "afterProcess"),
    )


def genRules(rules: list[JsonObject], tabs: int) -> str:
    indent = " " * (tabs * 4)
    slot_count = max(1, len(rules))
    lines: list[str] = [
        f"{indent}var normalizerIsInit: bool = false",
        f"{indent}val ruleLength: int = {len(rules)}",
        f"{indent}val rulesSpace: blob[sizeof(pointer<NormalizeRule>) * {slot_count}]",
        f"{indent}val rulePtr: pointer<pointer<NormalizeRule>> = rulesSpace as pointer<pointer<NormalizeRule>>",
    ]

    return "\n".join(lines) + "\n\n\n"


def genNormalizerInitFun(rules: list[JsonObject], tabs: int) -> str:
    indent = " " * (tabs * 4)
    body_indent = " " * ((tabs + 1) * 4)
    lines: list[str] = [
        f"{indent}private fun normalizerInit()",
        f"{indent}{{",
    ]

    for index, rule in enumerate(rules):
        state = rule["state"]
        action = rule["action"]

        if action is None:
            rule_expr = f"new NormalizeRule({index}, {state})"
        else:
            rule_expr = f"new NormalizeRule({index}, {state}, {action})"

        for pattern in rules[index]["patterns"]:
            kind = genKindExpr(pattern["kind"])
            regex_pattern = pattern["pattern"]

            if regex_pattern is None:
                rule_expr += f".addPattern({kind})"
            else:
                rule_expr += f".addPattern({kind}, {json.dumps(regex_pattern)})"

        rule_expr += f".setPivot({rules[index]['pivot_index']})"
        lines.append(f"{body_indent}rulePtr[{index}] = {rule_expr}")

    lines.append(f"{body_indent}normalizerIsInit = true")
    lines.append(f"{indent}}}")

    return "\n".join(lines) + "\n\n\n"


def genNormalizeFun(preprocess: str, postprocess: str, tabs: int) -> str:
    indent = " " * (tabs * 4)
    body_indent = " " * ((tabs + 1) * 4)
    nested_indent = " " * ((tabs + 2) * 4)
    source_expr = f"{preprocess}(list)" if preprocess else "list"
    result_expr = f"{postprocess}(result)" if postprocess else "result"
    lines: list[str] = [
        f"{indent}fun normalize(list: pointer<TokenList>) -> pointer<TokenList>",
        f"{indent}{{",
        f"{body_indent}if !normalizerIsInit:",
        f"{nested_indent}normalizerInit()",
        "",
        f"{body_indent}val normalized: pointer<TokenList> = {source_expr}",
        f"{body_indent}val fsm: pointer<NormalizeFSM> = new NormalizeFSM(normalized)",
        f"{body_indent}val result: pointer<TokenList> = fsm.apply(rulePtr, ruleLength)",
        "",
        f"{body_indent}return {result_expr}",
        f"{indent}}}",
    ]

    return "\n".join(lines) + "\n\n\n"


def codegen(config: JsonObject, tokenizer_config: JsonObject, dest: Path) -> str:
    ensure_type(config, NORMALIZER_RULES_TYPE, "normalizer config")
    ensure_type(tokenizer_config, TOKENIZER_RULES_TYPE, "tokenizer config")

    package_name = getPackageName(config)
    class_name = getClassName(config, dest)
    imports = getImports(config)
    pattern_defs = getPatternDefs(tokenizer_config)
    pattern_groups = getPatternGroups(tokenizer_config)
    regex_defs = getRegexDefs(tokenizer_config)
    rules = getRules(config, pattern_defs, pattern_groups, regex_defs)
    preprocess = getPreprocess(config)
    postprocess = getPostprocess(config)

    sections: list[str] = [
        genFileClass(class_name, 0),
        genPackage(package_name, 0),
        genImports(imports, 0),
        genConstants(getCodeBlocks(config, "constants"), 0),
        genRules(rules, 0),
        genBlocks(getCodeBlocks(config, "others")),
        genNormalizerInitFun(rules, 0),
        genNormalizeFun(preprocess, postprocess, 0),
    ]

    return "".join(section for section in sections if section != "")


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Generate xlang token normalizer source from a JSON file."
    )
    parser.add_argument(
        "-c",
        "--config",
        required=True,
        type=Path,
        help="Path to token normalizer JSON file.",
    )
    parser.add_argument(
        "-t",
        "--tokenizer-config",
        required=True,
        type=Path,
        help="Path to tokenizer JSON file used for shared defs and symbol defs.",
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
    tokenizer_config = read_json(args.tokenizer_config)
    content = codegen(config, tokenizer_config, args.dest)
    write_text(args.dest, content)


if __name__ == "__main__":
    main()
