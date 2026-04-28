# Revision history for xlang Java libraries

## Alpha-1.1.0 -- Unreleased

### Added

- `xlang.Math` class-level JavaDoc with usage example and behavior/performance notes.
- `log(double)` overload for natural logarithm.
- Native binding `IEEEremainder(double, double)` via `xlang_IEEEremainder`.

### Changed

- Math floating-point APIs use `@native(...) native inline` declarations for `StrictMath`-backed operations:
    - `sin`, `cos`, `tan`
    - `exp`, `ln`, `log10`
    - `sqrt`, `cbrt`
- Added copyright/SPDX file header to `src/main/xlang/xlang/Math.x`.
- Normalized JavaDoc structure and parameter formatting across `xlang.Math`.
- Corrected Math base-log API signature documentation to `log(double, double)` (not `log(int, int)`).
- Math owner path changed from `xlang.math.Math` to `xlang.Math`.

## Alpha-1.0.0 -- 2026-03-14

### Added / Supported

- Kotlin-based xlang runtime library module with JVM 8 toolchain.
- xlang annotation package:
    - `xlang.annotation.Metadata`
    - binary-retained marker used by compiler/tooling to identify owner type semantics
- `xlang.io` console API (`Console`):
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
    - predefined constants (`PI`, `E`, `PHI`, `TAU`, `LOG2E`, `LOG10E`, `LN2`, `LN10`, `PI_2`, `PI_4`, `ONE_PI`, `TWO_PI`, `TWO_SQRTPI`, `SQRT2`, `SQRT1_2`)
    - integer helpers: `succ` / `pred` for `int` and `long`
    - trigonometric APIs: `sin`, `cos`, `tan`, `sec`, `csc`, `cot`
    - inverse trigonometric APIs: `asin`, `acos`, `atan`, `asec`, `acsc`, `acot`
    - angle conversion: `toRadians`, `toDegrees`
    - logarithmic/exponential APIs: `ln`, `log(base, x)`, `log2`, `log10`, `exp`
    - root APIs: `sqrt`, `cbrt`
    - integer predicates: `isPrime`, `isEven`, `isOdd`
- Build integration:
    - Gradle task compiles `.x` sources under `src/main/xlang` using xlang compiler
    - runtime classpath jars are copied to `build/runtime-libs` for packaging
