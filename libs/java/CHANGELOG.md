# Revision history for xlang Java libraries

## Alpha-1.0.0 -- 2026-03-14

### Added / Supported

- Kotlin-based xlang runtime library module with JVM 8 toolchain.
- xlang annotation package:
    - `xlang.annotation.Metadata`
    - binary-retained marker used by compiler/tooling to identify owner type semantics
- `xlang.io` console API (`ConsoleX`):
    - top-level `put(...)` and `putln(...)` overloads for:
        - `Boolean`, `Char`, `Byte`, `Short`, `Int`, `Long`, `Float`, `Double`, `String`, `Any`
    - top-level `print(...)` vararg overloads with:
        - `sep`, `end`, `file`, `flush`
    - top-level `input(prompt, file)` helper
    - thread-safe print path via lock
- Owner type annotation usage:
    - public top-level APIs in `xlang.io` are marked with `@Metadata("xlang-top-level")`
    - designed for direct top-level symbol import/call in xlang source
- xlang math module (`src/main/xlang/xlang/math/Math.x`):
    - predefined constants (`PI`, `E`, `LOG2E`, `LOG10E`, `LN2`, `LN10`, `PI_2`, `PI_4`, `ONE_PI`, `TWO_PI`, `TWO_SQRTPI`, `SQRT2`, `SQRT1_2`)
    - integer helpers: `succ` / `pred` for `int` and `long`
- Build integration:
    - Gradle task compiles `.x` sources under `src/main/xlang` using xlang compiler
    - runtime classpath jars are copied to `build/runtime-libs` for packaging

