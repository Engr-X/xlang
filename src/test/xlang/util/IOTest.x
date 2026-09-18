/*
 * Copyright (c) 2026 Di Wang
 * SPDX-License-Identifier: MIT
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 */

#file.outerClass("IOTest")
package xlang.util

import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion
import xlang.util.string.StringBuilder


val TEST_GROUP: pointer<TestGroup> = genTest()


fun genTest() -> pointer<TestGroup>
{
    val result: pointer<TestGroup> = new TestGroup("xlang.util.IO")

    val getFullFileNameTC: pointer<TestCase> = new TestCase("getFullFileName", getFullFileNameTest)
    val getFileNameTC: pointer<TestCase> = new TestCase("getFileName", getFileNameTest)

    val testCaseSpace: blob[sizeof(pointer<TestCase>) * 2]
    val testCase: pointer<pointer<TestCase>> = testCaseSpace as pointer<pointer<TestCase>>
    val testCaseLength: int = 2

    testCase[0] = getFullFileNameTC
    testCase[1] = getFileNameTC

    for (var i = 0; i < testCaseLength; i++):
    {
        val tu: pointer<TestUnion> = new TestUnion(TestCase.TYPE, testCase[i], null)
        result.addTestUnion(tu)
    }

    return result
}


private fun builderEquals(builder: pointer<StringBuilder>, expected: pointer<char>) -> bool =
    builder != null && builder.equals(expected)


private fun getFullFileNameTest() -> int
{
    if !builderEquals(IO.getFullFileName("C:\\project\\src\\main.cpp"), "main.cpp"):
        return 1

    if !builderEquals(IO.getFullFileName("/home/user/test.txt"), "test.txt"):
        return 2

    if !builderEquals(IO.getFullFileName("example.data.bin"), "example.data.bin"):
        return 3

    if !builderEquals(IO.getFullFileName("main.cpp"), "main.cpp"):
        return 4

    if !builderEquals(IO.getFullFileName("C:\\project\\src\\"), ""):
        return 5

    if !builderEquals(IO.getFullFileName(null), ""):
        return 6

    return 0
}


private fun getFileNameTest() -> int
{
    if !builderEquals(IO.getFileName("C:\\project\\src\\main.cpp"), "main"):
        return 1

    if !builderEquals(IO.getFileName("/home/user/test.txt"), "test"):
        return 2

    if !builderEquals(IO.getFileName("archive.tar.gz"), "archive.tar"):
        return 3

    if !builderEquals(IO.getFileName("README"), "README"):
        return 4

    if !builderEquals(IO.getFileName(".gitignore"), ".gitignore"):
        return 5

    if !builderEquals(IO.getFileName("C:\\project\\src\\"), ""):
        return 6

    return 0
}
