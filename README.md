# xlang

An experimental programming language compiler targeting JVM bytecode.

## English

### Overview

`xlang` compiles `.x`, `.xl`, and `.xlang` source files to JVM bytecode, and can package runnable JAR files.

This repository has three main parts:

- `compiler/`: Haskell compiler frontend + semantic checks + IR + JVM lowering + CLI (`xlang`)
- `tools/BytecodeToolkit/`: Kotlin bytecode emitter/reader
- `libs/std/java/`: xlang standard/runtime libraries

### Current Status

- JVM target supported
- x64 target supported (native pipeline, NASM/LD toolchain flow is under active iteration)
- Import system updated:
  - `import` must end with a class name, or `*`
  - `import java.lang.Math` is valid
  - `import java.lang.*` is valid
  - `import java.lang` is invalid
- Default imports are always enabled:
  - `xlang.io.*`
  - `java.lang.*`
- Top-level wrapped functions can be called without class qualifier after class import (when metadata marks them as top-level).

### Build

#### Windows

```bash
make all -j8
```

#### Linux/macOS

```bash
make all -j8
```

### Configure + Out-of-Tree Build

```bash
mkdir -p build
cd build
../configure
make -j24
```

Windows (optional syslib staging for native link):

```bash
mkdir -p build
cd build
../configure --gcc-lib=D:/path/to/mingw/x86_64-w64-mingw32/lib
make -j24
```

### Build Outputs (Current Makefile)

- `build/xlang`: compiler executable (`xlang` / `xlang.exe`)
- `build/tools/*.jar`: BytecodeToolkit jars
- `build/libs/java/*`: Java std artifacts (`xlang-stdlib.jar`, `jdk<version>-stdlib.db`)
- `build/runtime/java/*`: Java runtime jars (`kotlin-stdlib-2.2.21.jar`, `annotations-13.0.jar`)
- `build/libs/native/*`: native static libs (`libxlang-base.a`, `libxlang-std.a`)
- `build/runtime/native/*`: native shared libs (`libxlang-base.*`, `libxlang-std.*`)
- `build/native/*.a`: staged system libs (Windows when `--gcc-lib` is set)

### Native Std (Standalone)

Build directly in `libs/std/native`:

```bash
cd libs/std/native/build
../configure
make -j24
```

This builds both:

- `libxlang-base` (C-side base library)
- `libxlang-std` (xlang std native objects packed as libs)

### Quick Start

Compile and run an example:

```bash
xlang --target-jvm25 examples/Example1.x -d examples/Example1.jar --include-runtime
java -jar examples/Example1.jar
```

Compatibility target form also works:

```bash
xlang --target=jvm25 examples/Example1.x -d examples/Example1.jar --include-runtime
```

Native x64 example:

```bash
xlang --target=x64 examples/Example1.x -d examples/Example1.exe
```

### CLI

```text
xlang --target-jvm<n> <files...> [-lib <libs...>] [--root <dir>] [-d <dir|jar>]
      [--main=<qname.main>] [-j <n>] [--include-runtime] [--debug]
```

Common options:

- `--target-jvm<n>`: target JVM version (`8`, `11`, `17`, `21`, `25`)
- `--target=jvm<n>`: compatibility alias
- `--target=x64`: native x64 backend
- `-d <dir|jar>`: output directory or JAR path
- `--include-runtime`: merge runtime jars into output JAR (requires `-d *.jar`)
- `-lib <files...>`: extra `.jar`, `.class`, `.json`
- `--main=<qname.main>`: override JAR `Main-Class`
- `-j <n>` / `--jobs <n>`: parallel jobs
- `--debug`: emit debug artifacts (`debug.json`, `Ir.txt`)

### Language Notes

- Semicolon at end of line is optional.
- On the same line, `;` can be used as a statement separator.
- Both modern and classic function declarations are supported.
- Typed declaration style supports both:
  - `val a: int = 10`
  - `int a = 10` / `final int a = 10` (syntax sugar)

### Examples

- `examples/Example1.x`: hello world + semicolon behavior
- `examples/Example2.x`: comments
- `examples/Example3.x`: primitive types, literals, naming rules
- `examples/Example4.x`: `public` / `private`
- `examples/Example5.x`: operators, precedence, cast, chained assignment
- `examples/Example6.x`: `if` / `else` styles
- `examples/Example7.x`: loop styles (`while`, `break`, `continue`, `while ... else`)
- `examples/Example8.x`: function declaration styles and syntax sugar
- `examples/Example9.x`: import rules and examples

### Project Layout

```text
.
|- compiler/
|- tools/BytecodeToolkit/
|- libs/std/java/
|- libs/std/native/
|- examples/
|- Makefile
```

### License

See [LICENSE](LICENSE).

The xlang name, logo, and related branding assets are trademarks or protected brand assets of the xlang project.
They are not licensed under the MIT License. You may not use them to imply endorsement, official association, or redistribution of modified versions without permission.


---

## 中文

### 项目简介

`xlang` 是一个面向 JVM 字节码的实验性编译器，支持将 `.x`、`.xl`、`.xlang` 源文件编译为 JVM 字节码，也支持打包可运行 JAR。

仓库主要包含三部分：

- `compiler/`：Haskell 编译器前端、语义检查、IR、JVM lowering、命令行工具 `xlang`
- `tools/BytecodeToolkit/`：Kotlin 字节码生成/读取工具
- `libs/std/java/`：xlang 标准库与运行时库

### 当前状态

- 已支持 JVM 目标
- `import` 规则已更新：
  - `import` 必须以“类名”或 `*` 结尾
  - `import java.lang.Math` 合法
  - `import java.lang.*` 合法
  - `import java.lang` 非法
- 默认自动导入：
  - `xlang.io.*`
  - `java.lang.*`
- 对于标记为 top-level 的包装函数，在类导入后可省略类名前缀调用。

### 构建

#### Windows

```bash
make all -j8
```

#### Linux/macOS

```bash
make all -j8
```

### 快速开始

编译并运行示例：

```bash
xlang --target-jvm25 examples/Example1.x -d examples/Example1.jar --include-runtime
java -jar examples/Example1.jar
```

兼容写法也可用：

```bash
xlang --target=jvm25 examples/Example1.x -d examples/Example1.jar --include-runtime
```

### 命令行参数

```text
xlang --target-jvm<n> <files...> [-lib <libs...>] [--root <dir>] [-d <dir|jar>]
      [--main=<qname.main>] [-j <n>] [--include-runtime] [--debug]
```

常用参数：

- `--target-jvm<n>`：目标 JVM 版本（`8`、`11`、`17`、`21`、`25`）
- `--target=jvm<n>`：兼容别名
- `-d <dir|jar>`：输出目录或 JAR 路径
- `--include-runtime`：把运行时依赖合并进输出 JAR（要求 `-d *.jar`）
- `-lib <files...>`：额外 `.jar`、`.class`、`.json`
- `--main=<qname.main>`：显式指定 JAR 的 `Main-Class`
- `-j <n>` / `--jobs <n>`：并行任务数
- `--debug`：输出调试产物（`debug.json`、`Ir.txt`）

### 语言说明

- 行尾分号可省略。
- 同一行可以用 `;` 分隔多个语句。
- 同时支持现代与经典两套函数声明风格。
- 类型声明支持：
  - `val a: int = 10`
  - `int a = 10` / `final int a = 10`（语法糖）

### 示例文件

- `examples/Example1.x`：Hello World 与分号规则
- `examples/Example2.x`：注释写法
- `examples/Example3.x`：基础类型、字面量、命名规则
- `examples/Example4.x`：`public` / `private`
- `examples/Example5.x`：运算符、优先级、类型转换、链式赋值
- `examples/Example6.x`：`if` / `else` 写法
- `examples/Example7.x`：循环写法（`while`、`break`、`continue`、`while ... else`）
- `examples/Example8.x`：函数声明三种风格与语法糖
- `examples/Example9.x`：`import` 规则与示例

### 目录结构

```text
.
|- compiler/
|- tools/BytecodeToolkit/
|- libs/std/java/
|- examples/
|- Makefile
```

### 许可证

见 [LICENSE](LICENSE)。

xlang 的名称、标志及相关品牌资产是 xlang 项目的商标或受保护的品牌资产。它们不在 MIT 许可证下授权
未经许可，您不得使用它们来暗示认可、官方关联或重新分发修改过的版本。