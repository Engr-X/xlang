/*
Function declaration styles.
函数声明风格.

1) Modern style (recommended):
1) 现代风格(推荐):
fun name(params) -> returnType { ... }
fun name(params) -> returnType { ... }

2) Classic style (C/Java-like):
2) 经典风格(C/Java 类似):
returnType name(params) { ... }
returnType name(params) { ... }

3) Syntax sugar:
3) 语法糖:
- single statement body can use ':'
- 单语句函数体可用 ':'
- single expression return can use '= expr'
- 单表达式返回可用 '= expr'
*/

// 1) Modern style
// 1) 现代风格
fun add(a: int, b: int) -> int
{
    return a + b
}

fun maxOf(a: int, b: int) -> int: return a if a > b else b

// 2) Classic style
// 2) 经典风格
int sub(int a, int b)
{
    return a - b
}

int neg(int x): return -x

// 3) Syntax sugar
// 3) 语法糖
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
