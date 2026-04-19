# xlang native base library build

This directory builds the C native base runtime used by xlang `@native(...)` calls.

## Outputs

- Windows: `build/libs/libxlang-base.dll`
- Linux: `build/libs/libxlang-base.so`
- macOS: `build/libs/libxlang-base.dylib`
- Static archive (all platforms): `build/libs/libxlang-base.a`

## Build with gcc + Makefile

```bash
./configure
make -j
make install
```

`make` now builds both:

- shared: `libxlang-base.(dll|so|dylib)`
- static: `libxlang-base.a`

Useful variants:

```bash
make shared
make static
make install
```
