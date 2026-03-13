# Revision history for xlang-compiler

## Alpha-1.0.0 -- 2026-03-14

### Added / Supported

- Source files: `.x` / `.xl` / `.xlang`

- Package and imports:
    - `import` must end with a class name or `*`
    - Default imports: `xlang.io.*`, `java.lang.*`
    - Top-level symbols from other files in the same package are visible by default (no extra import needed)
- Top-level declarations:
    - Top-level functions are lowered to static functions
    - Top-level variables are lowered to static fields
- Variable declarations and access control:
    - `var` / `val` / `const` / `final`
    - `public` / `private` (default is `public`)
    - Modern and classic declaration styles: `val a: int = 10`, `int a = 10`, `final int a = 10`
- Primitive and core types:
    - `byte/int8`, `short/int16`, `int/int32`, `long/int64`
    - `float/float32`, `double/float64`
    - `float128` (native-only; rejected on JVM target)
    - `bool`, `char`, `String` (mapped to `java.lang.String`)
- Expressions and operators:
    - Assignment and chained assignment
    - Ternary expression: `a if cond else b`
    - Arithmetic: `+ - * / % **`
    - Comparison: `== != > >= < <=`
    - Unary: `+ - !`
    - Explicit cast: `as`
- Control flow:
    - `if / else` (supports both block style and `:` single-statement style)
    - `while` and `while ... else`
    - `break` / `continue` / `return`
- Functions:
    - Modern/classic/sugar declaration styles
    - Single-statement function shorthand with `:`
- JVM target and packaging:
    - Supports `--target-jvm` and `--target-jvm<n>`
    - Compatibility alias: `--target=jvm<n>`
    - Supported JVM versions: `8 / 11 / 17 / 21 / 25`
    - Output to directory or JAR; `--include-runtime` can merge runtime dependencies
- Debug output:
    - `--debug` generates `debug.json` and `Ir.txt`
