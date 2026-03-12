/*
Operators and precedence (implemented examples).
运算符与优先级(已实现部分示例).

Precedence from low to high:
优先级从低到高:
1) assignment: =
1) 赋值: =
2) ternary: a if cond else b
2) 三元: a if cond else b
3) equality: ==, !=
3) 相等比较: ==, !=
4) relation: >, <
4) 关系比较: >, <
5) additive: +, -
5) 加减: +, -
6) multiplicative: *, /, %
6) 乘除模: *, /, %
7) power: **
7) 幂: **
8) unary: -x
8) 一元: -x
9) postfix cast: expr as Type
9) 后缀转型: expr as Type
*/

int main()
{
    // assignment
    // 赋值
    var a: int = 10
    var b: int = 3
    var c: int = 8
    a = b + 1
    a = b = c

    // equality and relation
    // 相等与关系比较
    val eq1: bool = a == 4
    val ne1: bool = a != b
    val gt1: bool = a > b
    val lt1: bool = b < a

    // additive / multiplicative / modulo
    // 加减 / 乘除 / 取模
    val add1: int = 1 + 2
    val sub1: int = 7 - 5
    val mul1: int = 3 * 4
    val div1: int = 7 / 2
    val mod1: int = 7 % 2

    // power
    // 幂运算
    val pow1: double = 2 ** 3
    val pow2: double = (1 + 2) ** 2

    // unary minus
    // 一元负号
    val neg1: int = -5
    val neg2: int = -(1 + 2)

    // precedence examples
    // 优先级示例
    val p1: int = 1 + 2 * 3
    val p2: int = (1 + 2) * 3
    val p3: bool = 1 + 2 * 3 == 7
    val p4: int = 100 if a > b else 200

    // cast examples
    // 转型示例
    val c1: double = a as double
    val c2: int = 3.7 as int
    val c3: int = (1 + 2.9) as int

    // consume values to avoid unused warnings
    // 使用变量避免未使用
    var sink: int = 0
    sink = sink + (1 if p3 else 0)
    sink = sink + c2 + c3 + div1 + mod1 + neg1 + neg2
    sink = sink + (1 if eq1 else 0)
    sink = sink + (1 if ne1 else 0)
    sink = sink + (1 if gt1 else 0)
    sink = sink + (1 if lt1 else 0)

    return sink + p1 + p2 + p4 + add1 + sub1 + mul1
}
