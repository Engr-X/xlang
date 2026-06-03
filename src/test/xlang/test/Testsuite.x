package xlang.test

import xlang.System

import xlang.util.string.StringBuilder


val TEST_SUCCESS: int = 0
private val TEXT_BUFFER: blob[1024]


private fun insertTabs(dest: StringBuilder, n: int)
{
    repeat n * 4:
        dest.append(' ')
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


    // TODO use real stringbuilder to improve efficiency
    fun runTest(n: int, record: pointer<TestRecord>)
    {
        val sb: StringBuilder = StringBuilder()
        insertTabs(sb, n)
        sb.append(this.name)
        sb.append(' ')

        val result: int = this.func()

        record.total++

        if result == TEST_SUCCESS:
        {
            sb.append("pass" as pointer<char>)
            record.correct++
        }
        else:
            sb.append("fail" as pointer<char>)
    }
}


struct TestUnion
{
    var type: int
    var testCase: pointer<TestCase>
    var group: pointer<Testgroup>

    fun runTest(n: int, record: pointer<TestRecord>)
    {
        if this.type == TestCase.TYPE:
            this.testCase.runTest(n, record)
        else:
            this.group.runTest(n, record)
    }
}


struct Testgroup
{
    static val TYPE: int = 1

    private static val INIT_CAPCITY: int = 16
    private static val LOAD_FACTOR: double = 0.75

    var length: int

    private val capacity: int;
    private var list: pointer<pointer<TestUnion>>


    fun __init__(name: pointer<char>)
    {
        this.length = 0
        this.capacity = INIT_CAPCITY
        this.list = System.allocMemory(this.capacity * sizeof(pointer<TestUnion>))
    }


    private fun resize()
    {
        this.capacity *= 2
        this.list = System.reallocMemory(this.list, this.capacity * sizeof(pointer<TestUnion>))
    }


    // pre testUninon must allocate on heap
    fun addTestunion(testUnion: pointer<TestUnion>)
    {
        if this.length + 1 >= ((this.capacity as double) * LOAD_FACTOR) as int:
            this.resize()

        this.list[this.length] = testUnion
        this.length++
    }


    private fun runTest(n: int, record: pointer<TestRecord>)
    {
        
    }
}
