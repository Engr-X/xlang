# Revision history for xlang native libraries

## Alpha-1.1.1 -- 2026-04-28

### Changed

- Updated native `xlang.Math` API comments to describe actual `xlang_*` C runtime bindings.
- Switched non-Windows native std object generation to `--compiler=as` to improve shared-library (`.so`/`.dylib`) PIC compatibility.

## Alpha-1.1.0 -- 2026-04-19

### Changed

- Added `log(int, int)` support in native std math surface.
- Updated math owner mapping from `xlang.math.Math` to `xlang.Math`.
