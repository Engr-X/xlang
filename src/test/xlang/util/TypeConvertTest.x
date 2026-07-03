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
    val result: TestGroup = new TestGroup("xlang.util.TypeConvert")

    val charToIntTC: pointer<TestCase> = new TestCase("charToInt", charToIntTest)
    val intToCharTC: pointer<TestCase> = new TestCase("intToChar", intToCharTest)
    val checkRadixTC: pointer<TestCase> = new TestCase("checkRadix", checkRadixTest)
    val stringToLongTC: pointer<TestCase> = new TestCase("stringToLong", stringToLongTest)
    val intToStringTC: pointer<TestCase> = new TestCase("intToString", intToStringTest)

    val testCaseSpace: blob[sizeof(pointer<TestCase>) * 100]
    val testCase: pointer<pointer<TestCase>> = testCaseSpace as pointer<pointer<TestCase>>
    val testCaseLength: int = 5

    testCase[0] = charToIntTC
    testCase[1] = intToCharTC
    testCase[2] = checkRadixTC
    testCase[3] = stringToLongTC
    testCase[4] = intToStringTC

    for (var i = 0; i < testCaseLength; i++)
    {
        val tu: TestUnion = new TestUnion(TestCase.TYPE, testCase[i], null)
        result.addTestUnion(tu)
    }

    return result
}


fun charToIntTest() -> int
{
    if TypeConvert.charToInt(('0' as int)) != 0:
        return 1

    if TypeConvert.charToInt(('5' as int)) != 5:
        return 2

    if TypeConvert.charToInt(('9' as int)) != 9:
        return 3

    return 0
}


fun intToCharTest() -> int
{
    if TypeConvert.intToChar(0) != ('0' as int):
        return 1

    if TypeConvert.intToChar(5) != ('5' as int):
        return 2

    if TypeConvert.intToChar(9) != ('9' as int):
        return 3

    return 0
}


fun checkRadixTest() -> int
{
    if !TypeConvert.checkRadix(2):
        return 1

    if !TypeConvert.checkRadix(10):
        return 2

    if !TypeConvert.checkRadix(36):
        return 3

    if TypeConvert.checkRadix(1):
        return 4

    if TypeConvert.checkRadix(37):
        return 5

    return 0
}


fun stringToLongTest() -> int
{
    if TypeConvert.stringToLong("0") != 0L:
        return 1

    if TypeConvert.stringToLong("12345") != 12345L:
        return 2

    if TypeConvert.stringToLong("-9876") != -9876L:
        return 3

    if TypeConvert.stringToLong("+42") != 42L:
        return 4

    if TypeConvert.stringToLong("42L") != 42L:
        return 5

    if TypeConvert.stringToLong("0x1f") != 31L:
        return 6

    if TypeConvert.stringToLong("0X2A") != 42L:
        return 7

    if TypeConvert.stringToLong("-0x2aL") != -42L:
        return 8

    if TypeConvert.stringToLong("0x") != 0L:
        return 9

    if TypeConvert.stringToLong("12z") != 0L:
        return 10

    return 0
}


fun intToStringTest() -> int
{
    val buffer: blob[64 * sizeof(char)]
    val text: pointer<char> = buffer as pointer<char>

    TypeConvert.intToString(text, 0, 10)

    if !String.streq(text, "0"):
        return 1

    if text[1] as int != String.NULL_CHAR:
        return 2

    TypeConvert.intToString(text, 12345, 10)

    if !String.streq(text, "12345"):
        return 3

    if text[5] as int != String.NULL_CHAR:
        return 4

    TypeConvert.intToString(text, -9876, 10)

    if !String.streq(text, "-9876"):
        return 5

    if text[5] as int != String.NULL_CHAR:
        return 6

    TypeConvert.intToString(text, 1000000000, 10)

    if !String.streq(text, "1000000000"):
        return 7

    if text[10] as int != String.NULL_CHAR:
        return 8

    TypeConvert.intToString(text, 255, 16)

    if !String.streq(text, "ff"):
        return 9

    if text[2] as int != String.NULL_CHAR:
        return 10

    TypeConvert.intToString(text, -10, 2)

    if !String.streq(text, "-1010"):
        return 11

    if text[5] as int != String.NULL_CHAR:
        return 12

    TypeConvert.intToString(text, 35, 36)

    if !String.streq(text, "z"):
        return 13

    if text[1] as int != String.NULL_CHAR:
        return 14

    text[0] = 'x'
    TypeConvert.intToString(text, 10, 1)

    if text[0] as int != String.NULL_CHAR:
        return 15

    return 0
}
