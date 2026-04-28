# Revision history for xlang

## Alpha-1.1.1 -- 2026-04-28

### Compiler & Language

- Added block-expression return behavior: the last expression in a block can be used as the block result.
- Removed ternary operator syntax and related parser sugar.
- Added pointer type support in language design (`pointer<T>` direction).

### Tooling & Design

- Added icon design/tooling work for project executable/icon pipeline.

### Runtime

- GC work is in progress (`runtime/gc` is under active development).

## Alpha-1.1.0 -- 2026-04-19

### Standard Library

- Added `log(int, int)` in standard math APIs.
- Moved math owner path from `xlang.math.Math` to `xlang.Math`.
- Added native std changelog under `libs/std/native/CHANGELOG.md`.

### Build & Packaging

- Root Makefile now supports a clearer end-to-end component pipeline:
  - `compile` (xlang compiler)
  - `tools` (BytecodeToolkit)
  - `java_lib` (Java std/runtime artifacts)
  - `native_lib` (native base/std libraries)
- Added out-of-tree-friendly configure + make flow (build wrapper Makefile generation).
- Native std/base artifacts are copied to build/runtime locations:
  - `libs/native/`
- Added Windows syslib staging from `--gcc-lib` into `build/native/`
  (currently `libkernel32.a`, `libmsvcrt.a`).
- BytecodeToolkit packaging/call path normalization:
  - toolkit jar artifact is unified as `BytecodeToolkit.jar` (non-versioned name)
  - compiler side now resolves `tools/BytecodeToolkit.jar`

### Native / x64

- Added/expanded x64 target build usage in project docs.
- Native std build flow under `libs/std/native` now includes both:
  - `libxlang-base`
  - `libxlang-std`
- Fixed native std Makefile path-resolution issue so `XLANG_EXE` resolves from repo root build output.

## Beta-1.0.0 -- 2026-03-20

### Release Description

Beta 1.0.0 focuses on language feature expansion and control-flow completeness,
including new short-circuit logical operators, richer loop forms (`loop`,
`repeat`, `until`, `do-while`, `do-until`, `for-else`), `if/elif/else`, comma
expression statements for `for` init/step, and expanded augmented assignment support.

Detailed updates are tracked in:

- `compiler/CHANGELOG.md`
- `libs/java/CHANGELOG.md`
- `tools/BytecodeToolkit/CHANGELOG.md`

## Alpha-1.0.0 -- 2026-03-14

### Release Description

This is the first public alpha release of the xlang toolchain, including:

- `xlang-compiler` (frontend, semantic checks, IR lowering, JVM codegen and CLI)
- `BytecodeToolkit` (JSON <-> JVM bytecode bridge)
- `libs/java` (xlang runtime/stdlib modules used by compiler output)

Key highlights of this alpha:

- JVM target pipeline available (`--target-jvm` / `--target-jvm<n>`)
- Updated import/package behavior with same-package multi-file symbol visibility
- Top-level function/variable lowering for JVM static model
- Core language syntax set (functions, control flow, operators, casts, typed declarations)
- Runtime and packaging flow for runnable JAR output

This root changelog is index-only. Detailed updates are tracked in:

- `compiler/CHANGELOG.md`
- `libs/java/CHANGELOG.md`
- `tools/BytecodeToolkit/CHANGELOG.md`
