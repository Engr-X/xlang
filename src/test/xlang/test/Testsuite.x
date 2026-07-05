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

package xlang.test

import xlang.util.ArrayList
import xlang.util.string.String
import xlang.util.string.StringBuilder
import xlang.util.IO
import xlang.util.TypeConvert


val TEST_SUCCESS_STATUES: int = 0
private val TEST_CASE_NAME_TAB_WIDTH: int = 8
private val BUFFER_SPACE: blob[1024 * sizeof(char)]
private val TEXT_BUFFER: pointer<char> = BUFFER_SPACE as pointer<char>

private val PASS_MSG: pointer<char> = "PASS"
private val FAIL_MSG: pointer<char> = "FAIL"


private fun insertTabs(dest: StringBuilder, n: int)
{
    repeat n:
        dest.append("    ")
}


private fun padToTabs(dest: StringBuilder, text: pointer<char>, tabs: int)
{
    val targetLength: int = tabs * 4
    val textLength: int = String.strlen(text)

    if textLength >= targetLength:
        return

    repeat targetLength - textLength:
        dest.append(' ')
}


private fun printTabs(n: int)
{
    val sb: pointer<StringBuilder> = StringBuilder()

    repeat n * 4:
        sb.append(' ')

    sb.get(TEXT_BUFFER)
    put(TEXT_BUFFER)
}


struct TestRecord
{
    var correct: int
    var total: int


    fun __init__()
    {
        this.correct = 0
        this.total = 0
    }
}


struct TestCase
{
    static val TYPE: int = 0

    private var func: () -> int
    private var name: pointer<char>


    fun __init__(name: pointer<char>, func: () -> int)
    {
        this.name = name
        this.func = func
    }


    // TODO use real stringbuilder to improve efficiency
    fun runTest(n: int, record: pointer<TestRecord>)
    {
        val sb: pointer<StringBuilder> = StringBuilder()
        insertTabs(sb, n)
        sb.append(this.name)
        sb.append(": ")
        padToTabs(sb, this.name, TEST_CASE_NAME_TAB_WIDTH)

        val result: int = this.func()

        record.total++

        if result == TEST_SUCCESS_STATUES:
        {
            IO.coloredSprint(TEXT_BUFFER, PASS_MSG, 32)
            sb.append(TEXT_BUFFER)
            record.correct++
        }
        else:
        {
            IO.coloredSprint(TEXT_BUFFER, FAIL_MSG, 91)
            sb.append(TEXT_BUFFER)
            sb.append(" exit with code: ")
            TypeConvert.intToString(TEXT_BUFFER, result, 10)
            sb.append(TEXT_BUFFER)
        }

        sb.newline()
        sb.get(TEXT_BUFFER)
        put(TEXT_BUFFER)
    }
}


struct TestUnion
{
    var type: int
    var testCase: pointer<TestCase>
    var testGroup: pointer<TestGroup>


    fun __init__(type: int, testCase: pointer<TestCase>, testGroup: pointer<TestGroup>)
    {
        this.type = type
        this.testCase = testCase
        this.testGroup = testGroup
    }


    fun runTest(n: int, record: pointer<TestRecord>)
    {
        if this.type == TestCase.TYPE:
            this.testCase.runTest(n, record)
        else:
            this.testGroup.runTest(n, record)
    }
}


struct TestGroup
{
    static val TYPE: int = 1

    var length: int
    private var name: pointer<char>
    private var list: ArrayList


    fun __init__(name: pointer<char>)
    {
        this.length = 0
        this.name = name
        this.list = new ArrayList(TestUnion.memSize())
    }


    fun addTestUnion(testUnion: pointer<TestUnion>)
    {
        this.list.push(testUnion)
        this.length = this.list.length
    }


    fun runTest(n: int, record: pointer<TestRecord>)
    {
        printTabs(n)
        putln(this.name)

        for (var i = 0; i < this.length; i++):
        {
            val union: pointer<TestUnion> = this.list.get(i) as pointer<TestUnion>
            union.runTest(n + 1, record)
        }
    }


    fun runTest()
    {
        val sb: pointer<StringBuilder> = StringBuilder()
        val record: pointer<TestRecord> = TestRecord()
        this.runTest(0, record)

        sb.newline()
        TypeConvert.intToString(TEXT_BUFFER, record.correct, 10)
        sb.append(TEXT_BUFFER)
        sb.append(" out of ")
        TypeConvert.intToString(TEXT_BUFFER, record.total, 10)
        sb.append(TEXT_BUFFER)
        sb.append(" tests passed. ")

        if record.correct == record.total:
        {
            IO.coloredSprint(TEXT_BUFFER, "Congratulations!", 32)
            sb.append(TEXT_BUFFER)
        }

        sb.get(TEXT_BUFFER)
        put(TEXT_BUFFER)
    }
}
