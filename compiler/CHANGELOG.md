# Revision history for xlang-compiler

## Beta-1.0.0 -- 2026-03-20

### Added / Supported

- Logical short-circuit family:
    - `&&`, `||`
    - `!&&` (logical nand), `!||` (logical nor)
    - `->` (logical imply), `!->` (logical not-imply)
    - Lowered with short-circuit semantics in semantic/IR pipeline.

- New loop/control-flow statements:
    - `loop` (infinite loop)
    - `repeat` / `repeat ... else` (counted loop)
    - `until` / `until ... else`
    - `do ... while` / `do ... while ... else`
    - `do ... until` / `do ... until ... else`
    - `for ... else`

- `elif` syntax for `if` chains:
    - Parse-time desugaring to nested `if/else` blocks.

- Comma expression statements:
    - New `Exprs [Expression]` statement form.
    - Used by `for` init/step parts, e.g. `for (...; ...; i++, sum += i)`.

- Multi-variable declarations:
    - Modern style: `var a: int = 1, b: double = 2`
    - Modern inferred style: `var a = 10, b = 20`
    - Classic style: `int a = 10, b = 10` (same declared type for all items)

- Augmented assignment expansion support:
    - `+=`, `-=`, `*=`, `/=`, `%=`, `**=`
    - Unified parse desugaring to plain assignment form.

### Changed

- Parser precedence blocks were normalized and synchronized across:
    - `ParseExpr.ypp`
    - `ParseStmt.ypp`
    - `ParseProgm.ypp`
    - `ParseBlock.ypp`
  with assignment precedence kept consistent (`=`, `+=`, `-=`, `*=`, `/=`, `%=`, `**=`).

- `for` AST/semantic model was extended to support:
    - init/step as statement nodes (`Expr` / `Exprs` / declaration forms)
    - optional `else` block.

- Primitive/class normalization helpers were refactored to map-based lookup
  (cleaner maintenance for builtin alias tables).

### Fixed

- Resolved parser conflicts introduced during `elif`/assignment extensions
  by precedence cleanup (`IFX` / `KW_ELIF` / `KW_ELSE` handling).

- Fixed missing IR/semantic branches for new loop/control-flow statements
  (`Loop`, `Repeat`, `Until`, `DoWhile`, `DoUntil`, `For ... else`).

- Fixed repeat-loop JVM verification/local-slot issue in lowering.

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
