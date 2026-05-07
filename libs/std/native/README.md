# xlang native std library build

This directory builds native std libraries from xlang sources and links against
runtime core (`libxlang-core`).

## Outputs

- Windows: `build/libs/libxlang-std.dll`
- Linux: `build/libs/libxlang-std.so`
- macOS: `build/libs/libxlang-std.dylib`
- Static archive (all platforms): `build/libs/libxlang-std.a`

## Build with gcc + Makefile

```bash
./configure
make -j
make install
```

`make` now builds both:

- shared: `libxlang-std.(dll|so|dylib)`
- static: `libxlang-std.a`

Useful variants:

```bash
make shared
make static
make install
```
