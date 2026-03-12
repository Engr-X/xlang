package xlang.math


val PI: double = 3.14159265358979323846

val E: double = 2.7182818284590452354

val LOG2E: double = 1.4426950408889634074

val LOG10E: double = 0.43429448190325182765

val LN2: double = 0.69314718055994530942

val LN10: double = 2.30258509299404568402

val PI_2: double = 1.57079632679489661923

val PI_4: double = 0.78539816339744830962

val 1_PI: double = 0.31830988618379067154

val 2_PI: double = 0.63661977236758134308

val 2_SQRTPI: double = 1.12837916709551257390

val SQRT2: double = 1.41421356237309504880

val SQRT1_2: double = 0.70710678118654752440


fun succ(x: int) -> int = Math.addExact(x, 1)

fun succ(x: long) -> long = Math.addExact(x, 1 as long)


fun pred(x: int) -> int = Math.subtractExact(x, 1)

fun pred(x: long) -> long = Math.subtractExact(x, 1 as long)
