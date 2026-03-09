package xlang.math


val PI = 3.14159265358979323846

val E = 2.7182818284590452354


fun sign(x: int) -> byte
{
    if (x > 0):
        return (1 as byte)

    return (-1 as byte) if x < 0 else (0 as byte)
}

fun power(a: double, b: double) -> double = a ** b

