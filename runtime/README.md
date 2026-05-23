# xlang runtime (unified project)

This directory is the unified native runtime project.

## Layout

- `include/core`, `include/gc`, `include/exception`
- `src/core`, `src/gc`, `src/exception`

Current build compiles **core** only. `gc` and `exception` sources are kept here for incremental development.

## Build

```bash
./configure
make -j
```

Outputs (under `build/libs`):

- `libxlang-core.a`
- `libxlang-core.(dll|so|dylib)`

Root `Makefile` consumes these artifacts and keeps final output locations unchanged.
