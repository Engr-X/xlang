/*
function declaration styles

1) modern style (recommended):
   fun name(params) -> returnType { ... }

2) classic style (C/Java-like):
   returnType name(params) { ... }

3) syntax sugar:
   - single-statement body can use ':'
   - single-expression return can use '= expr'
*/

// 1) modern style
fun add(a: int, b: int) -> int
{
    return a + b
}

fun maxOf(a: int, b: int) -> int: return a if a > b else b

// 2) classic style
int sub(int a, int b)
{
    return a - b
}

int neg(int x): return -x

// 3) syntax sugar
fun square(a: int) -> int = a * a
int cube(int a) = a * a * a

int main()
{
    val r1: int = add(10, 2)
    val r2: int = sub(10, 2)
    val r3: int = maxOf(r1, r2)
    val r4: int = neg(5)
    val r5: int = square(4)
    val r6: int = cube(3)

    return r1 + r2 + r3 + r4 + r5 + r6
}
