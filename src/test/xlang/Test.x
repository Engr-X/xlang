@file.class("Test")
package xlang

import xlang.util.IO
import xlang.util.string.StringTest


fun main()
{
    IO.enableANSIColor()

    StringTest.TEST_GROUP.runTest()
}
