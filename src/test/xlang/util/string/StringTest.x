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
    val result: TestGroup = new TestGroup("xlang.util.string.String")

    val strlenTC: pointer<TestCase> = new TestCase("strlen", strlenTest)
    val streqTC: pointer<TestCase> = new TestCase("streq", streqTest)
    val strcmpTC: pointer<TestCase> = new TestCase("strcmp", strcmpTest)
    val strcpyTC: pointer<TestCase> = new TestCase("strcpy", strcpyTest)
    val strdupTC: pointer<TestCase> = new TestCase("strdup", strdupTest)
    val strncpySafeTC: pointer<TestCase> = new TestCase("strncpy", strncpySafeTest)
    val strcatTC: pointer<TestCase> = new TestCase("strcat", strcatTest)
    val substringTC: pointer<TestCase> = new TestCase("substring", substringTest)
    val strMatchTC: pointer<TestCase> = new TestCase("strMatch", strMatchTest)

    val testCaseSpace: blob[sizeof(pointer<TestCase>) * 100]
    val testCase: pointer<pointer<TestCase>> = testCaseSpace as pointer<pointer<TestCase>>
    val testCaseLength: int = 9

    testCase[0] = strlenTC
    testCase[1] = streqTC
    testCase[2] = strcmpTC
    testCase[3] = strcpyTC
    testCase[4] = strdupTC
    testCase[5] = strncpySafeTC
    testCase[6] = strcatTC
    testCase[7] = substringTC
    testCase[8] = strMatchTC

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
    val buffer: blob[size * sizeof(char)]
    val string3: pointer<char> = buffer as pointer<char>

    for (var i = 0; i < size - 1; i++):
        string3[i] = 'a'

    string3[size - 1] = '\0'

    if String.strlen(string3) != size - 1:
        return 3

    return 0
}


fun streqTest() -> int
{
    if !String.streq("", ""):
        return 1

    if !String.streq("abc", "abc"):
        return 2

    if String.streq("", "abc"):
        return 3

    if String.streq("abc", ""):
        return 4

    if String.streq("abc", "abcd"):
        return 5

    if String.streq("abcd", "abc"):
        return 6

    if String.streq("abc", "abd"):
        return 7

    return 0
}


fun strcmpTest() -> int
{
    if String.strcmp("abc", "abc") != 0:
        return 1

    if String.strcmp("abc", "abd") >= 0:
        return 2

    if String.strcmp("abd", "abc") <= 0:
        return 3

    if String.strcmp("abc", "abcd") >= 0:
        return 4

    if String.strcmp("abcd", "abc") <= 0:
        return 5

    if String.strcmp("", "") != 0:
        return 6

    if String.strcmp("", "a") >= 0:
        return 7

    if String.strcmp("a", "") <= 0:
        return 8

    return 0
}


fun strcpyTest() -> int
{
    val size: int = 64
    val buffer: blob[size * sizeof(char)]
    val dest: pointer<char> = buffer as pointer<char>

    String.strcpy(dest, "Hello")

    if !String.streq(dest, "Hello"):
        return 1

    String.strcpy(dest, "Hi")

    if !String.streq(dest, "Hi"):
        return 2

    if dest[2] as int != String.NULL_CHAR:
        return 3

    String.strcpy(dest, "")

    if !String.streq(dest, ""):
        return 4

    if dest[0] as int != String.NULL_CHAR:
        return 5

    return 0
}


fun strdupTest() -> int
{
    val src1: pointer<char> = "Hello"
    val copy1: pointer<char> = String.strdup(src1)

    if copy1 == null:
        return 1

    if copy1 == src1:
        return 2

    if !String.streq(copy1, src1):
        return 3

    val src2: pointer<char> = ""
    val copy2: pointer<char> = String.strdup(src2)

    if copy2 == null:
        return 4

    if copy2 == src2:
        return 5

    if !String.streq(copy2, src2):
        return 6

    if copy2[0] as int != String.NULL_CHAR:
        return 7

    if String.strdup(null) != null:
        return 8

    return 0
}


fun strncpySafeTest() -> int
{
    val buffer1: blob[8 * sizeof(char)]
    val dest1: pointer<char> = buffer1 as pointer<char>

    if String.strncpy(dest1, "Hello", 5) != 5:
        return 1

    if !String.streq(dest1, "Hello"):
        return 2

    val buffer2: blob[8 * sizeof(char)]
    val dest2: pointer<char> = buffer2 as pointer<char>

    if String.strncpy(dest2, "Hello", 3) != 3:
        return 3

    if !String.streq(dest2, "Hel"):
        return 4

    val buffer3: blob[8 * sizeof(char)]
    val dest3: pointer<char> = buffer3 as pointer<char>

    if String.strncpy(dest3, "Hello", 2) != 2:
        return 5

    if !String.streq(dest3, "He"):
        return 6

    val buffer4: blob[1 * sizeof(char)]
    val dest4: pointer<char> = buffer4 as pointer<char>

    if String.strncpy(dest4, "Hello", 0) != 0:
        return 7

    if dest4[0] as int != String.NULL_CHAR:
        return 8

    val buffer5: blob[8 * sizeof(char)]
    val dest5: pointer<char> = buffer5 as pointer<char>

    if String.strncpy(dest5, "Hi", 5) != 2:
        return 9

    if !String.streq(dest5, "Hi"):
        return 10

    return 0
}


fun strcatTest() -> int
{
    val size: int = 64
    val buffer: blob[size * sizeof(char)]
    val dest: pointer<char> = buffer as pointer<char>

    String.strcpy(dest, "Hello")
    String.strcat(dest, ", World!")

    if !String.streq(dest, "Hello, World!"):
        return 1

    String.strcat(dest, "")

    if !String.streq(dest, "Hello, World!"):
        return 2

    String.strcpy(dest, "")
    String.strcat(dest, "abc")

    if !String.streq(dest, "abc"):
        return 3

    return 0
}


fun substringTest() -> int
{
    val buffer1: blob[16 * sizeof(char)]
    val dest1: pointer<char> = buffer1 as pointer<char>

    if String.substring(dest1, "Hello, World!", 7, 5) != 5:
        return 1

    if !String.streq(dest1, "World"):
        return 2

    val buffer2: blob[16 * sizeof(char)]
    val dest2: pointer<char> = buffer2 as pointer<char>

    if String.substring(dest2, "Hello, World!", 7, 20) != 6:
        return 3

    if !String.streq(dest2, "World!"):
        return 4

    val buffer3: blob[4 * sizeof(char)]
    val dest3: pointer<char> = buffer3 as pointer<char>

    if String.substring(dest3, "Hello, World!", 0, 3) != 3:
        return 5

    if !String.streq(dest3, "Hel"):
        return 6

    val buffer4: blob[8 * sizeof(char)]
    val dest4: pointer<char> = buffer4 as pointer<char>

    if String.substring(dest4, "Hello", 10, 2) != 0:
        return 7

    if !String.streq(dest4, ""):
        return 8

    if String.substring(dest4, "Hello", 1, 0) != 0:
        return 9

    if !String.streq(dest4, ""):
        return 10

    return 0
}


fun strMatchTest() -> int
{
    if String.strRegMatch("abc", "abc") != 3:
        return 1

    if String.strRegMatch("abc", "xabcx") != -1:
        return 2

    if String.strRegMatch("[0-9]+", "12345") != 5:
        return 3

    if String.strRegMatch("[0-9]+", "12a45") != 2:
        return 4

    if String.strRegMatch("a.c", "abc") != 3:
        return 5

    if String.strRegMatch("0[xX][0-9a-fA-F]+|[0-9]+", "0x1f") != 4:
        return 6

    if String.strRegMatch("0[xX][0-9a-fA-F]+|[0-9]+", "111") != 3:
        return 7

    if String.strRegMatch("(0[xX][0-9a-fA-F]+)|([0-9]+)", "0X2A") != 4:
        return 8

    if String.strRegMatch("(0[xX][0-9a-fA-F]+)|([0-9]+)", "1234") != 4:
        return 9

    if String.strRegMatch("([a-z]+|[0-9]+)", "abc") != 3:
        return 10

    if String.strRegMatch("(([a-z]+)|([0-9]+))", "789") != 3:
        return 11

    if String.strRegMatch("[a|b]+", "a|b") != 3:
        return 12

    if String.strRegMatch("a|ab", "ab") != 2:
        return 13

    return 0
}
