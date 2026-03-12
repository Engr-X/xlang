/*
primary types (default/min/max)
default value means declaration without initializer.

byte, int8:
    default: 0
    min: -128
    max: 127

short, int16:
    default: 0
    min: -32768
    max: 32767

int, int32:
    default: 0
    min: -2147483648
    max: 2147483647

long, int64:
    default: 0
    min: -9223372036854775808
    max: 9223372036854775807

float, float32:
    default: 0.0
    min: -3.4028235e38
    max: 3.4028235e38
    min positive normal: 1.17549435e-38

double, float64:
    default: 0.0
    min: -1.7976931348623157e308
    max: 1.7976931348623157e308
    min positive normal: 2.2250738585072014e-308

float128 (native only; JVM unsupported):
    default: 0.0
    min: -1.18973149535723176508575932662800702e4932
    max: 1.18973149535723176508575932662800702e4932
    min positive normal: 3.36210314311209350626267781732175260e-4932

char:
    default: '\0'
    min: '\u0000' (0)
    max: '\uffff' (65535)

    note: in native target, char is lowered as byte (int8)

bool:
    default: false
    values: false / true
*/

// ---- literal format examples ----
// integer: decimal / hex / signed / long suffix L
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

// float/float32: use f/F suffix, scientific notation supported
val e0: float = 3.14f
val e1: float32 = .5f
val e2: float = 1e-3f
val e3: float = 6.02E+2f

// double/float64: scientific notation supported
val f0: double = 3.1415926
val f1: float64 = 6.02e23
val f2: double = 1e-9
val f3: double = 10.
val f4: double = .125

// float128 (native only): use l/L suffix
// val g0: float128 = 3.1415926535897932384626L
// val g1: float128 = 1e-30L

// char / bool / String
val h0: char = 'A'
val h1: char = '\n'
val i0: bool = true
val i1: bool = false
val j0: String = "hello xlang"

// note:
// type can be omitted, for example: val k = 10
// if type is specified, value will be implicitly cast to that type when needed

// variable naming rules (Java-like):
// pattern: [A-Za-z_][A-Za-z0-9_]*
// cannot contain '@'
// keywords cannot be used as variable names
val userName: String = "Murphy"
val _tmp2: int = 2
val VALUE_1: int = 1
// invalid examples (do not uncomment):
// val a@b: int = 1
// val 1abc: int = 1
// val final: int = 1

// if you like Java/C style declarations:
final int answer = 42
int counter = 0
final double pi = 3.1415926
