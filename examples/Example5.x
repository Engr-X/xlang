/*
operators & precedence (implemented examples)

precedence (low -> high):
1) assignment: =
2) ternary: a if cond else b
3) equality: ==, !=
4) relation: >, <
5) additive: +, -
6) multiplicative: *, /, %
7) power: **
8) unary: -x
9) cast (postfix): expr as Type

note:
- use parentheses to force order
- unary '+' and '!' are not included here
*/

int main()
{
    // assignment
    var a: int = 10
    var b: int = 3
    var c: int = 8
    a = b + 1
    a = b = c               // chained assignment (right-associative)

    // equality / relation
    val eq1: bool = a == 4
    val ne1: bool = a != b
    val gt1: bool = a > b
    val lt1: bool = b < a

    // additive / multiplicative / modulo
    val add1: int = 1 + 2
    val sub1: int = 7 - 5
    val mul1: int = 3 * 4
    val div1: int = 7 / 2
    val mod1: int = 7 % 2

    // power
    val pow1: double = 2 ** 3
    val pow2: double = (1 + 2) ** 2

    // unary minus
    val neg1: int = -5
    val neg2: int = -(1 + 2)

    // precedence examples
    val p1: int = 1 + 2 * 3          // 7
    val p2: int = (1 + 2) * 3        // 9
    val p3: bool = 1 + 2 * 3 == 7
    val p4: int = 100 if a > b else 200

    // cast (postfix)
    val c1: double = a as double
    val c2: int = 3.7 as int
    val c3: int = (1 + 2.9) as int

    // consume values
    var sink: int = 0
    sink = sink + (1 if p3 else 0)
    sink = sink + c2 + c3 + div1 + mod1 + neg1 + neg2

    // keep bool values used
    sink = sink + (1 if eq1 else 0)
    sink = sink + (1 if ne1 else 0)
    sink = sink + (1 if gt1 else 0)
    sink = sink + (1 if lt1 else 0)

    return sink + p1 + p2 + p4 + add1 + sub1 + mul1
}
