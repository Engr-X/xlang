package xlang.util.string

import xlang.System


struct StringBuilder
{
    private static val INIT_CAPACITY: int = 16
    private static val LOAD_FACTOR: double = 0.75

    var length: int
    private var capacity: int
    private var list: pointer<char>


    fun __init__()
    {
        this.length = 0
        this.capacity = INIT_CAPACITY
        this.list = System.allocMemory(this.capacity * sizeof(char))
        this.list.deref = '\0'
    }


    private fun resize(insertLength: int)
    {
        val requiredCapacity: int = insertLength + this.length + 1

        while this.capacity < requiredCapacity:
            this.capacity *= 2

        this.list = System.reallocMemory(this.list, this.capacity * sizeof(char))
    }


    fun append(ch: char)
    {
        if this.length + 1 >= ((this.capacity as double) * LOAD_FACTOR) as int:
            this.resize(1)
        
        this.list[this.length++] = ch
        this.list[this.length] = '\0'
    }


    fun append(str: pointer<char>)
    {
        val appendLength: int = String.strlen(str)

        if this.length + appendLength >= ((this.capacity as double) * LOAD_FACTOR) as int:
            this.resize(appendLength)

        String.strcpy(this.list + this.length, str)
        this.length += appendLength
    }


    fun get(dest: pointer<char>):
        String.strcpy(dest, this.list)
}
