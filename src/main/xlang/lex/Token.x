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

package xlang.lex

import xlang.System
import xlang.util.string.String




struct TokenPosition
{
    var offset: int // for computer (offset in one dimenstion)
    var line: int
    var column: int
    var length: int


    fun __init__(offset: int, line: int, column: int, length: int)
    {
        this.offset = offset
        this.line = line
        this.column = column
        this.length = length
    }
}


struct Token
{
    static val EOF_KIND: int = 0
    
    var kind: int
    var pos: TokenPosition
    var text: pointer<char>
    var errorInfo: pointer<char>


    fun __init__(kind: int, pos: TokenPosition, text: pointer<char>)
    {
        this.kind = kind
        this.pos = pos

        if kind == EOF_KIND:
        {
            this.text = null
            return
        }

        this.text = String.strdup(text)
        this.errorInfo = null
    }


    fun __init__(kind: int, pos: TokenPosition, text: pointer<char>, errorInfo: pointer<char>)
    {
        this.kind = kind
        this.pos = pos

        if kind == EOF_KIND:
        {
            this.text = null
            return
        }

        this.text = String.strdup(text)
        this.errorInfo = String.strdup(errorInfo)
    }


    fun isEOF() -> bool = this.kind == EOF_KIND
}


struct TokenList
{
    private static val INIT_CAPACITY: int = 16
    private static val LOAD_FACTOR: double = 0.75

    private var capacity: int
    private var tokens: pointer<pointer<Token>>
    private var size: int


    fun __init__()
    {
        this.capacity = INIT_CAPACITY
        this.tokens = System.allocMemory(this.capacity * sizeof(pointer<Token>))
        this.size = 0
    }


    private fun resize()
    {
        this.capacity *= 2
        this.tokens = System.reallocMemory(this.tokens, this.capacity * sizeof(pointer<Token>))
    }


    fun add(token: pointer<Token>)
    {
        if this.size + 1 >= ((this.capacity as double) * LOAD_FACTOR) as int:
            this.resize()

        this.tokens[this.size] = token
        this.size++
    }


    fun length() -> int
    {
        return this.size
    }


    fun get(index: int) -> pointer<Token>
    {
        return this.tokens[index]
    }
}
