package xlang.math


fun sign(x: int) -> byte
{
    if (x > 0)
        return (1 as byte)

    return (-1 as byte) if x < 0 else 0 as byte
}

fun power(a: double, b: double) = a ** b

