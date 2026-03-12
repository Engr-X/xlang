/*
Primary types (default/min/max).
基础类型(默认值/最小值/最大值).
"default" means declaration without initializer.
"default" 表示声明时不写初始化.

byte/int8 default 0, min -128, max 127.
byte/int8 默认 0, 最小 -128, 最大 127.
short/int16 default 0, min -32768, max 32767.
short/int16 默认 0, 最小 -32768, 最大 32767.
int/int32 default 0, min -2147483648, max 2147483647.
int/int32 默认 0, 最小 -2147483648, 最大 2147483647.
long/int64 default 0, min -9223372036854775808, max 9223372036854775807.
long/int64 默认 0, 最小 -9223372036854775808, 最大 9223372036854775807.

float/float32 default 0.0, min -3.4028235e38, max 3.4028235e38.
float/float32 默认 0.0, 最小 -3.4028235e38, 最大 3.4028235e38.
double/float64 default 0.0, min -1.7976931348623157e308, max 1.7976931348623157e308.
double/float64 默认 0.0, 最小 -1.7976931348623157e308, 最大 1.7976931348623157e308.
float128 is native-only; JVM backend does not support it.
float128 仅 native 支持; JVM 后端不支持.

char default '\0', min '\u0000'(0), max '\uffff'(65535).
char 默认 '\0', 最小 '\u0000'(0), 最大 '\uffff'(65535).
In native target, char is lowered as byte(int8).
在 native 目标里, char 按 byte(int8) 处理.

bool default false, values false/true.
bool 默认 false, 取值 false/true.
*/

// ---- Literal format examples ----
// ---- 字面量格式示例 ----

// Integer: decimal / hex / signed / long suffix L.
// 整数: 十进制 / 十六进制 / 带符号 / long 后缀 L.
val a0: byte = 100
val a1: int8 = -128
val a2: byte = 0x7f

val b0: short = 12345
val b1: int16 = -32768
val b2: short = 0x7fff

val c0: int = 123456
val c1: int32 = -2147483648
val c2: int = 0x7fffffff

val d0: long = 9223372036854775807L
val d1: int64 = -9223372036854775808L
val d2: long = 0x7fffffffffffffffL

// float/float32: use f/F suffix; scientific notation is supported.
// float/float32: 使用 f/F 后缀; 支持科学计数法.
val e0: float = 3.14f
val e1: float32 = .5f
val e2: float = 1e-3f
val e3: float = 6.02E+2f

// double/float64: scientific notation is supported.
// double/float64: 支持科学计数法.
val f0: double = 3.1415926
val f1: float64 = 6.02e23
val f2: double = 1e-9
val f3: double = 10.
val f4: double = .125

// float128 (native only): use l/L suffix.
// float128(仅 native): 使用 l/L 后缀.
// val g0: float128 = 3.1415926535897932384626L
// val g1: float128 = 1e-30L

// char / bool / String
// char / bool / String
val h0: char = 'A'
val h1: char = '\n'
val i0: bool = true
val i1: bool = false
val j0: String = "hello xlang"

// Type can be omitted, e.g. val k = 10.
// 类型可省略, 例如 val k = 10.
// If type is specified, value is implicitly cast when needed.
// 如果显式写类型, 必要时会做隐式转换.

// Variable naming rules (Java-like):
// 变量命名规则(类似 Java):
// Pattern: [A-Za-z_][A-Za-z0-9_]*
// 模式: [A-Za-z_][A-Za-z0-9_]*
// Cannot contain '@'.
// 不能包含 '@'.
// Keywords cannot be variable names.
// 关键字不能当变量名.
val userName: String = "Murphy"
val _tmp2: int = 2
val VALUE_1: int = 1

// Invalid examples (do not uncomment):
// 非法示例(不要取消注释):
// val a@b: int = 1
// val 1abc: int = 1
// val final: int = 1

// Java/C style declaration sugar is also supported.
// 也支持 Java/C 风格声明语法糖.
final int answer = 42
int counter = 0
final double pi = 3.1415926
