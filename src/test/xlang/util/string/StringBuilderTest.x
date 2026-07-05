@file.class("StringBuilderTest")
package xlang.util.string

import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion


val TEST_GROUP: TestGroup = genTest()


fun genTest() -> TestGroup
{
    val result: TestGroup = new TestGroup("xlang.util.string.StringBuilder")

    val appendCharTC: pointer<TestCase> = new TestCase("appendChar", appendCharTest)
    val appendStringTC: pointer<TestCase> = new TestCase("appendString", appendStringTest)
    val newlineTC: pointer<TestCase> = new TestCase("newline", newlineTest)
    val resizeTC: pointer<TestCase> = new TestCase("resize", resizeTest)
    val clearTC: pointer<TestCase> = new TestCase("clear", clearTest)

    val testCaseSpace: blob[sizeof(pointer<TestCase>) * 100]
    val testCase: pointer<pointer<TestCase>> = testCaseSpace as pointer<pointer<TestCase>>
    val testCaseLength: int = 5

    testCase[0] = appendCharTC
    testCase[1] = appendStringTC
    testCase[2] = newlineTC
    testCase[3] = resizeTC
    testCase[4] = clearTC

    for (var i = 0; i < testCaseLength; i++)
    {
        val tu: TestUnion = new TestUnion(TestCase.TYPE, testCase[i], null)
        result.addTestUnion(tu)
    }

    return result
}


fun appendCharTest() -> int
{
    val sb: pointer<StringBuilder> = StringBuilder()
    val buffer: blob[64 * sizeof(char)]
    val text: pointer<char> = buffer as pointer<char>

    sb.append('A')
    sb.append('B')
    sb.append('C')
    sb.get(text)

    if sb.length != 3:
        return 1

    if !String.streq(text, "ABC"):
        return 2

    if text[3] != String.NULL_CHAR:
        return 3

    return 0
}


fun appendStringTest() -> int
{
    val sb: pointer<StringBuilder> = StringBuilder()
    val buffer: blob[64 * sizeof(char)]
    val text: pointer<char> = buffer as pointer<char>

    sb.append("Hello")
    sb.append(", ")
    sb.append("World")
    sb.get(text)

    if sb.length != 12:
        return 1

    if !String.streq(text, "Hello, World"):
        return 2

    return 0
}


fun newlineTest() -> int
{
    val sb: pointer<StringBuilder> = StringBuilder()
    val buffer: blob[64 * sizeof(char)]
    val text: pointer<char> = buffer as pointer<char>

    sb.append("Hello")
    sb.newline()
    sb.append("World")
    sb.get(text)

    if sb.length != 11:
        return 1

    if text[5] != String.LINE_FEED:
        return 2

    if !String.streq(text, "Hello\nWorld"):
        return 3

    return 0
}


fun resizeTest() -> int
{
    val sb: pointer<StringBuilder> = StringBuilder()
    val buffer: blob[128 * sizeof(char)]
    val text: pointer<char> = buffer as pointer<char>

    sb.append("0123456789")
    sb.append("abcdefghij")
    sb.append("KLMNOPQRST")
    sb.append("uvwxyz!@#$")
    sb.get(text)

    if sb.length != 40:
        return 1

    if !String.streq(text, "0123456789abcdefghijKLMNOPQRSTuvwxyz!@#$"):
        return 2

    if text[40] != String.NULL_CHAR:
        return 3

    return 0
}


fun clearTest() -> int
{
    val sb: pointer<StringBuilder> = StringBuilder()
    val buffer: blob[64 * sizeof(char)]
    val text: pointer<char> = buffer as pointer<char>

    sb.append("Hello")
    sb.clear()
    sb.get(text)

    if sb.length != 0:
        return 1

    if text[0] != String.NULL_CHAR:
        return 2

    if !String.streq(text, ""):
        return 3

    sb.append("World")
    sb.get(text)

    if sb.length != 5:
        return 4

    if !String.streq(text, "World"):
        return 5

    return 0
}
