# Revision history for BytecodeToolkit

## Alpha-1.1.0 -- 2026-03-15

### Added / Supported

- SQLite metadata pipeline for Java libraries:
    - `--read` now supports `.db` / `.sqlite`
    - `--out` now supports `.db` output
    - built-in schema bootstrap for sqlite metadata files
- Import-driven metadata filtering:
    - `--imports` supports wildcard class patterns (for example: `java.lang.*`)
    - SQL-side filtering is applied when reading from `.db`
- Multi-source read in one process:
    - repeated `--read` is supported (for example: `--read a.db --read b.jar`)
    - merged output is emitted as one JSON/db result

### Improved

- Archive read performance:
    - `.jar` / `.jmod` reading supports class-name pre-filtering before bytecode parse
- Import pattern matching:
    - fixed wildcard matching behavior for `*`
- Compiler integration path:
    - non-json library inputs can be batched into a single BytecodeToolkit invocation
    - reduced repeated JVM startup overhead during library load

## Alpha-1.0.0 -- 2026-03-14

### Added / Supported

- JSON -> `.class` generation mode:
    - input from `--string/-s` or `--file/-f`
    - parallel class generation with `-j/--jobs`
- Bytecode -> JSON read mode:
    - supports `--read/-r` for `.class`, `.jar`, and `.jmod`
    - supports `--separate` split output for large read results
    - supports `--debug` progress output while reading archives
- Owner type metadata model:
    - supported `owner_type`: `xlang-class`, `xlang-top-level`
    - annotation descriptor: `Lxlang/annotation/Metadata;`
    - metadata is emitted only for non-class owner type (`xlang-top-level`)
- Operation set updates:
    - reference constant push op: `apush`
    - reference local store op: `astore`
    - compare branch families: `if_icmp*`, `if_lcmp*`, `if_fcmp*`, `if_dcmp*`
- JSON compatibility behavior:
    - accepts `"class"` as array or string in input envelope
    - supports `{"classes":[...]}` batch envelope and single-class object input
- Reader DTO output:
    - emits class/method/field DTOs with normalized `owner_type`
    - preserves package/class/function mapping for downstream compiler import loading
