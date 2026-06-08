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
 *
 *
 */

@file.class("StringTest")
package xlang.util.string

import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion


val TEST_GROUP: TestGroup = genTest()


fun genTest() -> TestGroup
{
    val result: TestGroup = new TestGroup("xlang.util.string.String" as pointer<char>)

    val strlenTC: pointer<TestCase> = new TestCase("strlen" as pointer<char>, strlenTest)
    val streqTC: pointer<TestCase> = new TestCase("streq" as pointer<char>, streqTest)
    val strcpyTC: pointer<TestCase> = new TestCase("strcpy" as pointer<char>, strcpyTest)
    val strcatTC: pointer<TestCase> = new TestCase("strcat" as pointer<char>, strcatTest)

    val testCaseSpace: blob[sizeof(pointer<TestCase>) * 100]
    val testCase: pointer<pointer<TestCase>> = testCaseSpace as pointer<pointer<TestCase>>
    val testCaseLength: int = 4

    testCase[0] = strlenTC
    testCase[1] = streqTC
    testCase[2] = strcpyTC
    testCase[3] = strcatTC

    for (var i = 0; i < testCaseLength; i++)
    {
        val tu: TestUnion = new TestUnion(TestCase.TYPE, testCase[i], null)
        result.addTestUnion(tu)
    }

    return result
}


fun strlenTest() -> int
{
    val string1: pointer<char> = ""

    if String.strlen(string1) != 0:
        return 1

    val string2: pointer<char> = "Hello, World!"

    if String.strlen(string2) != 13:
        return 2

    val size: int = 1000
    val buffer: blob[size]
    val string3: pointer<char> = buffer as pointer<char>

    for (var i = 0; i < size - 1; i++):
        string3[i] = 'a'

    string3[size - 1] = String.NULL_CHAR

    if String.strlen(string3) != size - 1:
        return 3

    return 0
}


fun streqTest() -> int
{
    if !String.streq("" as pointer<char>, "" as pointer<char>):
        return 1

    if !String.streq("abc" as pointer<char>, "abc" as pointer<char>):
        return 2

    if String.streq("" as pointer<char>, "abc" as pointer<char>):
        return 3

    if String.streq("abc" as pointer<char>, "" as pointer<char>):
        return 4

    if String.streq("abc" as pointer<char>, "abcd" as pointer<char>):
        return 5

    if String.streq("abcd" as pointer<char>, "abc" as pointer<char>):
        return 6

    if String.streq("abc" as pointer<char>, "abd" as pointer<char>):
        return 7

    return 0
}


fun strcpyTest() -> int
{
    val size: int = 64
    val buffer: blob[size]
    val dest: pointer<char> = buffer as pointer<char>

    String.strcpy(dest, "Hello" as pointer<char>)

    if !String.streq(dest, "Hello" as pointer<char>):
        return 1

    String.strcpy(dest, "Hi" as pointer<char>)

    if !String.streq(dest, "Hi" as pointer<char>):
        return 2

    if dest[2] != String.NULL_CHAR:
        return 3

    String.strcpy(dest, "" as pointer<char>)

    if !String.streq(dest, "" as pointer<char>):
        return 4

    if dest[0] != String.NULL_CHAR:
        return 5

    return 0
}


fun strcatTest() -> int
{
    val size: int = 64
    val buffer: blob[size]
    val dest: pointer<char> = buffer as pointer<char>

    String.strcpy(dest, "Hello" as pointer<char>)
    String.strcat(dest, ", World!" as pointer<char>)

    if !String.streq(dest, "Hello, World!" as pointer<char>):
        return 1

    String.strcat(dest, "" as pointer<char>)

    if !String.streq(dest, "Hello, World!" as pointer<char>):
        return 2

    String.strcpy(dest, "" as pointer<char>)
    String.strcat(dest, "abc" as pointer<char>)

    if !String.streq(dest, "abc" as pointer<char>):
        return 3

    return 0
}
