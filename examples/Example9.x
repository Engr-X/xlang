/*
Example9 - import rules.
示例9 - import 规则.

1) Import must end with class name or '*'.
1) import 必须以类名或 '*' 结尾.
valid:
合法:
import java.lang.Math
import java.lang.Math
import java.lang.*
import java.lang.*
import xlang.math.MathX
import xlang.math.MathX
invalid:
非法:
import java.lang
import java.lang
import xlang.math
import xlang.math

2) Default imports (always enabled):
2) 默认导入(始终启用):
xlang.io.*
xlang.io.*
java.lang.*
java.lang.*

3) Class import example:
3) 类导入示例:
import java.lang.Math
import java.lang.Math
then call Math.abs / Math.max.
然后可调用 Math.abs / Math.max.

4) Top-level shortcut:
4) top-level 省略类名:
import xlang.math.MathX
import xlang.math.MathX
both can work (if succ is top-level):
若 succ 是 top-level, 两种都可用:
MathX.succ(1)
MathX.succ(1)
succ(1)
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
