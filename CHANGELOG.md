# Revision history for xlang

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
