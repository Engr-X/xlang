# Revision history for xlang

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
