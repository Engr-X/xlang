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
    val strcmpTC: pointer<TestCase> = new TestCase("strcmp" as pointer<char>, strcmpTest)
    val strcpyTC: pointer<TestCase> = new TestCase("strcpy" as pointer<char>, strcpyTest)
    val strncpySafeTC: pointer<TestCase> = new TestCase("strncpy" as pointer<char>, strncpySafeTest)
    val strcatTC: pointer<TestCase> = new TestCase("strcat" as pointer<char>, strcatTest)
    val substringTC: pointer<TestCase> = new TestCase("substring" as pointer<char>, substringTest)
    val strMatchTC: pointer<TestCase> = new TestCase("strMatch" as pointer<char>, strMatchTest)

    val testCaseSpace: blob[sizeof(pointer<TestCase>) * 100]
    val testCase: pointer<pointer<TestCase>> = testCaseSpace as pointer<pointer<TestCase>>
    val testCaseLength: int = 8

    testCase[0] = strlenTC
    testCase[1] = streqTC
    testCase[2] = strcmpTC
    testCase[3] = strcpyTC
    testCase[4] = strncpySafeTC
    testCase[5] = strcatTC
    testCase[6] = substringTC
    testCase[7] = strMatchTC

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


fun strcmpTest() -> int
{
    if String.strcmp("abc" as pointer<char>, "abc" as pointer<char>) != 0:
        return 1

    if String.strcmp("abc" as pointer<char>, "abd" as pointer<char>) >= 0:
        return 2

    if String.strcmp("abd" as pointer<char>, "abc" as pointer<char>) <= 0:
        return 3

    if String.strcmp("abc" as pointer<char>, "abcd" as pointer<char>) >= 0:
        return 4

    if String.strcmp("abcd" as pointer<char>, "abc" as pointer<char>) <= 0:
        return 5

    if String.strcmp("" as pointer<char>, "" as pointer<char>) != 0:
        return 6

    if String.strcmp("" as pointer<char>, "a" as pointer<char>) >= 0:
        return 7

    if String.strcmp("a" as pointer<char>, "" as pointer<char>) <= 0:
        return 8

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


fun strncpySafeTest() -> int
{
    val buffer1: blob[8]
    val dest1: pointer<char> = buffer1 as pointer<char>

    if String.strncpy(dest1, "Hello" as pointer<char>, 5) != 5:
        return 1

    if !String.streq(dest1, "Hello" as pointer<char>):
        return 2

    val buffer2: blob[8]
    val dest2: pointer<char> = buffer2 as pointer<char>

    if String.strncpy(dest2, "Hello" as pointer<char>, 3) != 3:
        return 3

    if !String.streq(dest2, "Hel" as pointer<char>):
        return 4

    val buffer3: blob[8]
    val dest3: pointer<char> = buffer3 as pointer<char>

    if String.strncpy(dest3, "Hello" as pointer<char>, 2) != 2:
        return 5

    if !String.streq(dest3, "He" as pointer<char>):
        return 6

    val buffer4: blob[1]
    val dest4: pointer<char> = buffer4 as pointer<char>

    if String.strncpy(dest4, "Hello" as pointer<char>, 0) != 0:
        return 7

    if dest4[0] != String.NULL_CHAR:
        return 8

    val buffer5: blob[8]
    val dest5: pointer<char> = buffer5 as pointer<char>

    if String.strncpy(dest5, "Hi" as pointer<char>, 5) != 2:
        return 9

    if !String.streq(dest5, "Hi" as pointer<char>):
        return 10

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


fun substringTest() -> int
{
    val buffer1: blob[16]
    val dest1: pointer<char> = buffer1 as pointer<char>

    if String.substring(dest1, "Hello, World!" as pointer<char>, 7, 5) != 5:
        return 1

    if !String.streq(dest1, "World" as pointer<char>):
        return 2

    val buffer2: blob[16]
    val dest2: pointer<char> = buffer2 as pointer<char>

    if String.substring(dest2, "Hello, World!" as pointer<char>, 7, 20) != 6:
        return 3

    if !String.streq(dest2, "World!" as pointer<char>):
        return 4

    val buffer3: blob[4]
    val dest3: pointer<char> = buffer3 as pointer<char>

    if String.substring(dest3, "Hello, World!" as pointer<char>, 0, 3) != 3:
        return 5

    if !String.streq(dest3, "Hel" as pointer<char>):
        return 6

    val buffer4: blob[8]
    val dest4: pointer<char> = buffer4 as pointer<char>

    if String.substring(dest4, "Hello" as pointer<char>, 10, 2) != 0:
        return 7

    if !String.streq(dest4, "" as pointer<char>):
        return 8

    if String.substring(dest4, "Hello" as pointer<char>, 1, 0) != 0:
        return 9

    if !String.streq(dest4, "" as pointer<char>):
        return 10

    return 0
}


fun strMatchTest() -> int
{
    if String.strRegMatch("abc" as pointer<char>, "abc" as pointer<char>) != 3:
        return 1

    if String.strRegMatch("abc" as pointer<char>, "xabcx" as pointer<char>) != -1:
        return 2

    if String.strRegMatch("[0-9]+" as pointer<char>, "12345" as pointer<char>) != 5:
        return 3

    if String.strRegMatch("[0-9]+" as pointer<char>, "12a45" as pointer<char>) != 2:
        return 4

    if String.strRegMatch("a.c" as pointer<char>, "abc" as pointer<char>) != 3:
        return 5

    if String.strRegMatch("0[xX][0-9a-fA-F]+|[0-9]+" as pointer<char>, "0x1f" as pointer<char>) != 4:
        return 6

    if String.strRegMatch("0[xX][0-9a-fA-F]+|[0-9]+" as pointer<char>, "111" as pointer<char>) != 3:
        return 7

    if String.strRegMatch("(0[xX][0-9a-fA-F]+)|([0-9]+)" as pointer<char>, "0X2A" as pointer<char>) != 4:
        return 8

    if String.strRegMatch("(0[xX][0-9a-fA-F]+)|([0-9]+)" as pointer<char>, "1234" as pointer<char>) != 4:
        return 9

    if String.strRegMatch("([a-z]+|[0-9]+)" as pointer<char>, "abc" as pointer<char>) != 3:
        return 10

    if String.strRegMatch("(([a-z]+)|([0-9]+))" as pointer<char>, "789" as pointer<char>) != 3:
        return 11

    if String.strRegMatch("[a|b]+" as pointer<char>, "a|b" as pointer<char>) != 3:
        return 12

    if String.strRegMatch("a|ab" as pointer<char>, "ab" as pointer<char>) != 2:
        return 13

    return 0
}
