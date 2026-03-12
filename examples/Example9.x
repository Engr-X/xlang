/*
Example9: import rules

1) Import must end with a class name, or with '*'.
    valid:
        import java.lang.Math
        import java.lang.*
        import xlang.math.MathX
    invalid:
        import java.lang
        import xlang.math

2) Default imports (always available):
    import xlang.io.*
    import java.lang.*

3) Class import:
    import java.lang.Math
    then use:
    Math.abs(...)
    Math.max(...)

4) Wildcard import:
    import java.lang.*
    imports all classes in that package.

5) Top-level function shortcut:
    import xlang.math.MathX
    if function is top-level, both are allowed:
    MathX.succ(1)
    succ(1)
*/

import java.lang.Math
import xlang.math.MathX

void main() {
    val a: int = Math.abs(-10)
    val b: int = Math.max(3, 7)

    val c: int = MathX.succ(10)
    val d: int = succ(20)

    putln(a)
    putln(b)
    putln(c)
    putln(d)
}
