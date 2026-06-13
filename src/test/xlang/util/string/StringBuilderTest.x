@file.class("StringBuilderTest")
package xlang.util.string

import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion


val TEST_GROUP: TestGroup = genTest()


fun genTest() -> TestGroup
{
    val result: TestGroup = new TestGroup("xlang.util.string.StringBuilder" as pointer<char>)

    val appendCharTC: pointer<TestCase> = new TestCase("appendChar" as pointer<char>, appendCharTest)
    val appendStringTC: pointer<TestCase> = new TestCase("appendString" as pointer<char>, appendStringTest)
    val newlineTC: pointer<TestCase> = new TestCase("newline" as pointer<char>, newlineTest)
    val resizeTC: pointer<TestCase> = new TestCase("resize" as pointer<char>, resizeTest)

    val testCaseSpace: blob[sizeof(pointer<TestCase>) * 100]
    val testCase: pointer<pointer<TestCase>> = testCaseSpace as pointer<pointer<TestCase>>
    val testCaseLength: int = 4

    testCase[0] = appendCharTC
    testCase[1] = appendStringTC
    testCase[2] = newlineTC
    testCase[3] = resizeTC

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
    val buffer: blob[64]
    val text: pointer<char> = buffer as pointer<char>

    sb.append('A')
    sb.append('B')
    sb.append('C')
    sb.get(text)

    if sb.length != 3:
        return 1

    if !String.streq(text, "ABC" as pointer<char>):
        return 2

    if text[3] != String.NULL_CHAR:
        return 3

    return 0
}


fun appendStringTest() -> int
{
    val sb: pointer<StringBuilder> = StringBuilder()
    val buffer: blob[64]
    val text: pointer<char> = buffer as pointer<char>

    sb.append("Hello" as pointer<char>)
    sb.append(", " as pointer<char>)
    sb.append("World" as pointer<char>)
    sb.get(text)

    if sb.length != 12:
        return 1

    if !String.streq(text, "Hello, World" as pointer<char>):
        return 2

    return 0
}


fun newlineTest() -> int
{
    val sb: pointer<StringBuilder> = StringBuilder()
    val buffer: blob[64]
    val text: pointer<char> = buffer as pointer<char>

    sb.append("Hello" as pointer<char>)
    sb.newline()
    sb.append("World" as pointer<char>)
    sb.get(text)

    if sb.length != 11:
        return 1

    if text[5] != String.LINE_FEED:
        return 2

    if !String.streq(text, "Hello\nWorld" as pointer<char>):
        return 3

    return 0
}


fun resizeTest() -> int
{
    val sb: pointer<StringBuilder> = StringBuilder()
    val buffer: blob[128]
    val text: pointer<char> = buffer as pointer<char>

    sb.append("0123456789" as pointer<char>)
    sb.append("abcdefghij" as pointer<char>)
    sb.append("KLMNOPQRST" as pointer<char>)
    sb.append("uvwxyz!@#$" as pointer<char>)
    sb.get(text)

    if sb.length != 40:
        return 1

    if !String.streq(text, "0123456789abcdefghijKLMNOPQRSTuvwxyz!@#$" as pointer<char>):
        return 2

    if text[40] != String.NULL_CHAR:
        return 3

    return 0
}
