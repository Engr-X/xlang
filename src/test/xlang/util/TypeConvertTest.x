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

@file.class("TypeConvertTest")
package xlang.util

import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion
import xlang.util.string.String


val TEST_GROUP: TestGroup = genTest()


fun genTest() -> TestGroup
{
    val result: TestGroup = new TestGroup("xlang.util.TypeConvert" as pointer<char>)

    val charToIntTC: pointer<TestCase> = new TestCase("charToInt" as pointer<char>, charToIntTest)
    val intToCharTC: pointer<TestCase> = new TestCase("intToChar" as pointer<char>, intToCharTest)
    val intToStringTC: pointer<TestCase> = new TestCase("intToString" as pointer<char>, intToStringTest)

    val testCaseSpace: blob[sizeof(pointer<TestCase>) * 100]
    val testCase: pointer<pointer<TestCase>> = testCaseSpace as pointer<pointer<TestCase>>
    val testCaseLength: int = 3

    testCase[0] = charToIntTC
    testCase[1] = intToCharTC
    testCase[2] = intToStringTC

    for (var i = 0; i < testCaseLength; i++)
    {
        val tu: TestUnion = new TestUnion(TestCase.TYPE, testCase[i], null)
        result.addTestUnion(tu)
    }

    return result
}


fun charToIntTest() -> int
{
    if TypeConvert.charToInt('0') != 0:
        return 1

    if TypeConvert.charToInt('5') != 5:
        return 2

    if TypeConvert.charToInt('9') != 9:
        return 3

    return 0
}


fun intToCharTest() -> int
{
    if TypeConvert.intToChar(0) != '0':
        return 1

    if TypeConvert.intToChar(5) != '5':
        return 2

    if TypeConvert.intToChar(9) != '9':
        return 3

    return 0
}


fun intToStringTest() -> int
{
    val buffer: blob[64]
    val text: pointer<char> = buffer as pointer<char>

    TypeConvert.intToString(text, 0)

    if !String.streq(text, "0" as pointer<char>):
        return 1

    if text[1] != String.NULL_CHAR:
        return 2

    TypeConvert.intToString(text, 12345)

    if !String.streq(text, "12345" as pointer<char>):
        return 3

    if text[5] != String.NULL_CHAR:
        return 4

    TypeConvert.intToString(text, -9876)

    if !String.streq(text, "-9876" as pointer<char>):
        return 5

    if text[5] != String.NULL_CHAR:
        return 6

    TypeConvert.intToString(text, 1000000000)

    if !String.streq(text, "1000000000" as pointer<char>):
        return 7

    if text[10] != String.NULL_CHAR:
        return 8

    return 0
}
