# xlang runtime core library build

This directory builds the C runtime core used by xlang `@native(...)` calls.

## Outputs

- Windows: `build/libs/libxlang-core.dll`
- Linux: `build/libs/libxlang-core.so`
- macOS: `build/libs/libxlang-core.dylib`
- Static archive (all platforms): `build/libs/libxlang-core.a`

## Build with gcc + Makefile

```bash
./configure
make -j
make install
```

`make` now builds both:

- shared: `libxlang-core.(dll|so|dylib)`
- static: `libxlang-core.a`

Useful variants:

```bash
make shared
make static
make install
```
