# xlang

An language compiler targeting JVM bytecode.

## English

### Overview

`xlang` compiles `.x`, `.xl`, and `.xlang` source files to JVM bytecode and can package runnable JAR files.

This repository contains three main parts:

- `compiler/`: Haskell compiler frontend + IR + JVM lowering + CLI (`xlang`)
- `tools/BytecodeToolkit/`: Kotlin bytecode emitter/reader tool
- `libs/java/`: standard runtime library (including `xlang-native-alpha.jar`)

### Current Capabilities

- Parse and type-check xlang source files
- Generate JVM bytecode
- Build class output directory or output JAR
- Bundle runtime dependencies with `--include-runtime`
- Support multiple JVM targets (currently `8`, `11`, `17`, `21`, `25`)

### Prerequisites

- GHC + Cabal (for `compiler/`)
- JDK 8+ (JDK 8 is used by included Gradle projects by default)
- Gradle (wrapper included in Java subprojects)
- `7z` in PATH (only needed when downloading JDK metadata via `-download`)

### Build

#### Windows

```bat
build.bat all -j8
```

Artifacts:

- `build/xlang.exe`
- `build/tools/BytecodeToolkit-alpha.jar`
- `build/libs/java/*.jar` (includes `xlang-native-alpha.jar` and runtime jars)

#### Linux/macOS

```bash
make all -j8
```

Artifacts:

- `build/xlang`
- `build/tools/BytecodeToolkit-alpha.jar`
- `build/libs/java/*.jar`

### Quick Start

Compile an example to JAR and include runtime:

```bash
xlang --target-jvm8 examples/example1.x -d examples/example1.jar --include-runtime
java -jar examples/example1.jar
```

Compatibility aliases also work:

```bash
xlang --target=jvm25 examples/example1.x -d examples/example1.jar --include-runtime
```

### CLI Usage

```text
xlang --target-jvm<n> <files...> [-lib <libs...>] [--root <dir>] [-d <dir|jar>]
      [--main=<qname.main>] [-j <n>] [--include-runtime] [--debug]
```

Common options:

- `--target-jvm<n>`: target JVM version, e.g. `--target-jvm25`
- `-d <dir|jar>`: output directory or JAR path
- `--include-runtime`: merge runtime jars into output JAR (requires `-d *.jar`)
- `-lib <files...>`: extra `.jar`, `.class`, `.json` metadata inputs
- `-j <n>` / `--jobs <n>`: parallel jobs
- `--main=<qname.main>`: explicit entry for JAR `Main-Class`
- `--debug`: emit `debug.json` and `Ir.txt`
- `-download=<n>`: download JDK metadata archive (for target `n`)

### Project Layout

```text
.
|- compiler/                Haskell compiler and tests
|- tools/BytecodeToolkit/   JVM bytecode toolkit (Kotlin)
|- libs/java/               xlang runtime library project
|- examples/                sample xlang source files
|- build.bat                Windows build entry
|- Makefile                 Unix-like build entry
```

### Development Notes

If you want to add or modify grammar/syntax, start from:

- `compiler/src/Lex/Token.hs`
- `compiler/src/Lex/Tokenizer.x`
- `compiler/src/Parse/SyntaxTree.hs`
- `compiler/src/Parse/*`

### Contributing

Issues and PRs are welcome. Keep changes focused and include tests when possible.

### License

See [LICENSE](LICENSE).

---

## 中文

### 项目简介

`xlang` 是一个面向 JVM 字节码的语言编译器，可编译 `.x`、`.xl`、`.xlang` 文件，并支持打包可运行 JAR。

仓库主要由三部分组成：

- `compiler/`：Haskell 编译器前端、IR、JVM lowering、CLI（`xlang`）
- `tools/BytecodeToolkit/`：Kotlin 字节码生成/读取工具
- `libs/java/`：标准运行时库（包含 `xlang-native-alpha.jar`）

### 当前能力

- 语法解析与类型检查
- 生成 JVM 字节码
- 输出 class 目录或 JAR
- 通过 `--include-runtime` 合并运行时依赖
- 支持多个 JVM 目标版本（当前 `8`、`11`、`17`、`21`、`25`）

### 环境要求

- GHC + Cabal（用于 `compiler/`）
- JDK 8+（Java 子项目默认使用 JDK 8）
- Gradle（仓库内已带 wrapper）
- PATH 中有 `7z`（仅在 `-download` 下载 JDK metadata 时需要）

### 构建方式

#### Windows

```bat
build.bat all -j8
```

产物：

- `build/xlang.exe`
- `build/tools/BytecodeToolkit-alpha.jar`
- `build/libs/java/*.jar`（含 `xlang-native-alpha.jar` 与运行时依赖）

#### Linux/macOS

```bash
make all -j8
```

产物：

- `build/xlang`
- `build/tools/BytecodeToolkit-alpha.jar`
- `build/libs/java/*.jar`

### 快速开始

将示例编译为 JAR 并包含运行时：

```bash
xlang --target-jvm8 examples/example1.x -d examples/example1.jar --include-runtime
java -jar examples/example1.jar
```

兼容写法也支持：

```bash
xlang --target=jvm25 examples/example1.x -d examples/example1.jar --include-runtime
```

### 常用命令行参数

- `--target-jvm<n>`：指定 JVM 目标版本，例如 `--target-jvm25`
- `-d <dir|jar>`：输出目录或 JAR 路径
- `--include-runtime`：把运行时 jar 合并到输出 JAR（必须配合 `-d *.jar`）
- `-lib <files...>`：附加 `.jar`、`.class`、`.json` 元数据输入
- `-j <n>` / `--jobs <n>`：并行任务数
- `--main=<qname.main>`：显式指定 JAR 的 `Main-Class`
- `--debug`：输出 `debug.json` 和 `Ir.txt`
- `-download=<n>`：下载对应版本 JDK metadata

### 目录结构

```text
.
|- compiler/                Haskell 编译器与测试
|- tools/BytecodeToolkit/   JVM 字节码工具（Kotlin）
|- libs/java/               xlang 运行时库工程
|- examples/                示例源码
|- build.bat                Windows 构建入口
|- Makefile                 Unix-like 构建入口
```

### 开发提示

如果你要新增或调整语法，优先看这些位置：

- `compiler/src/Lex/Token.hs`
- `compiler/src/Lex/Tokenizer.x`
- `compiler/src/Parse/SyntaxTree.hs`
- `compiler/src/Parse/*`

### 贡献

欢迎提 Issue 和 PR。建议每次改动聚焦一个主题，并尽量补充测试。

### 许可证

见 [LICENSE](LICENSE)。
