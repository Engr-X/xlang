package xlang.compiler.lex

import xlang.System


struct LexPosition
{
    static val START_POSITION: LexPosition = LexPosition(0, 1, 1)

    // for computer
    var offset: int

    var line: int
    var column: int
}


struct LexInput
{
    var pos: LexPosition
    var prevChar: char
    var text: pointer<char>
    var textLength: int


    fun toTokenPosition(dest: TokenPosition, length: int)
    {
        dest.line = this.pos.line
        dest.offset = this.pos.offset
        dest.column = this.pos.column
        dest.length = length
    }
}


struct LexState
{
    var state: int
    var accumulator: CharList
    var cursorPos: LexPosition
    var action: (LexState, LexInput, LexState) -> void
}


struct CharList
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
    }


    private fun resize()
    {
        this.capacity *= 2
        this.list = System.reallocMemory(this.list, this.capacity * sizeof(char))
    }


    fun append(ch: char)
    {
        if (this.length + 1) as double >= (this.capacity as double) * LOAD_FACTOR:
            this.resize()
        
        this.list[this.length] = ch
        this.length++
    }
}
