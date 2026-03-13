# Revision history for BytecodeToolkit

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
