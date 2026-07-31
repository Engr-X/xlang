from pathlib import Path
import json
from typing import Any


JsonObject = dict[str, Any]
CodeBlock = str | list[str]


def get_config_type(config: JsonObject) -> str:
    """Read the rule file type, accepting the historical ``type: `` key."""
    value = config.get("type", config.get("type:", config.get("type: ", "")))

    if not isinstance(value, str):
        raise TypeError(f"config type must be string: {value!r}")

    return value


def check_type(config: JsonObject, value: str) -> bool:
    """Return whether a codegen config has the expected rule file type."""
    return get_config_type(config) == value


def ensure_type(config: JsonObject, value: str, name: str) -> None:
    """Raise a clear error when a codegen config has the wrong rule type."""
    actual = get_config_type(config)

    if actual != value:
        raise ValueError(f"{name} must be {value!r}, got {actual!r}")


def read_json(path: Path) -> JsonObject:
    """Read a UTF-8 JSON file and return its top-level object."""
    with path.open("r", encoding="utf-8") as file:
        return json.load(file)


def write_text(path: Path, content: str) -> None:
    """Write generated UTF-8 text, creating the destination directory first."""
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content, encoding="utf-8")


def get_package_name(config: JsonObject) -> str:
    """Read the target xlang package name from a codegen config object."""
    return config.get("package", config.get("package: ", ""))


def get_class_name(config: JsonObject, dest: Path) -> str:
    """Read the generated file class name, falling back to the output stem."""
    return config.get("class", config.get("class: ", config.get("class_name", dest.stem)))


def align_to(value: int, alignment: int) -> int:
    """Round value up to the next multiple of alignment."""
    return ((value + alignment - 1) // alignment) * alignment


def indent_of(tabs: int) -> str:
    """Return a four-space-per-tab indentation string."""
    return " " * (tabs * 4)



def gen_file_class(class_name: str, tabs: int) -> str:
    """Generate the @file.class annotation for a generated xlang source file."""
    indent = indent_of(tabs)

    return f'{indent}@file.class("{class_name}")\n'


def gen_package(package: str, tabs: int) -> str:
    """Generate a package declaration with the standard section spacing."""
    indent = indent_of(tabs)

    if not package or package.isspace():
        return ""
    
    return f"{indent}package {package}\n\n\n"


def gen_imports(imports: set[str], tabs: int) -> str:
    """Generate sorted import declarations for deterministic generated output."""
    indent = indent_of(tabs)

    return "\n".join(f"{indent}import {item}" for item in sorted(imports)) + "\n\n\n"


def gen_constants(constants: list[CodeBlock], tabs: int) -> str:
    """Generate a constants section from string lines or multi-line blocks."""
    if not constants:
        return ""

    indent = indent_of(tabs)
    lines: list[str] = []

    for item in constants:
        if isinstance(item, str):
            lines.append(f"{indent}{item}")
        elif isinstance(item, list):
            lines.extend(f"{indent}{line}" for line in item)
        else:
            raise TypeError(f"constants item must be string or string list: {item!r}")

    return "\n".join(lines) + "\n\n\n"
