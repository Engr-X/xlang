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

import xlang.System
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
        dest.append(' ' as int)
}


private fun printTabs(n: int)
{
    val sb: pointer<StringBuilder> = StringBuilder()

    repeat n * 4:
        sb.append(' ' as int)

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

    private static val INIT_CAPCITY: int = 16
    private static val LOAD_FACTOR: double = 0.75

    var length: int
    private var name: pointer<char>
    private val capacity: int;
    private var list: pointer<pointer<TestUnion>>


    fun __init__(name: pointer<char>)
    {
        this.length = 0
        this.name = name
        this.capacity = INIT_CAPCITY
        this.list = System.allocMemory(this.capacity * sizeof(pointer<TestUnion>))
    }


    private fun resize()
    {
        this.capacity *= 2
        this.list = System.reallocMemory(this.list, this.capacity * sizeof(pointer<TestUnion>))
    }


    // pre testUninon must allocate on heap
    fun addTestUnion(testUnion: pointer<TestUnion>)
    {
        if this.length + 1 >= ((this.capacity as double) * LOAD_FACTOR) as int:
            this.resize()

        this.list[this.length] = testUnion
        this.length++
    }


    fun runTest(n: int, record: pointer<TestRecord>)
    {
        printTabs(n)
        putln(this.name)

        for (var i = 0; i < this.length; i++):
        {
            val union: pointer<TestUnion> = this.list[i]
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
