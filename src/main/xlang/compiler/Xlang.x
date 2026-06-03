@file.class("Xlang")
package xlang.compiler

import xlang.util.IO


fun main()
{
    IO.enableANSIColor()
    val buffer: blob[100]
    IO.coloredSprint(buffer as pointer<char>, "Hello, World" as pointer<char>, 32)
    putln(buffer as pointer<char>)
}
