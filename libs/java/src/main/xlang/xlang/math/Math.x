package xlang.math


val PI: double = 3.14159265358979323846

val E: double = 2.7182818284590452354


fun succ(x: int) -> int = Math.addExact(x, 1)

fun succ(x: long) -> long = Math.addExact(x, 1 as long)


fun pred(x: int) -> int = Math.subtractExact(x, 1)

fun pred(x: long) -> long = Math.subtractExact(x, 1 as long)
