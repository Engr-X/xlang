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

package xlang.lexer

import xlang.System
import xlang.util.string.String
import xlang.util.ArrayList


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
    static val EOF_STRING: pointer<char> = "<EOF>"

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
            this.text = EOF_STRING
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
            this.text = EOF_STRING
            return
        }

        this.text = String.strdup(text)
        this.errorInfo = String.strdup(errorInfo)
    }


    fun isEOF() -> bool = this.kind == EOF_KIND
}


struct TokenList
{
    private var tokens: ArrayList


    fun __init__():
        this.tokens = new ArrayList(Token.memSize())


    fun push(token: pointer<Token>):
        this.tokens.push(token)


    fun length() -> int = this.tokens.length


    fun get(index: int) -> pointer<Token> =
        this.tokens.get(index) as pointer<Token>
}
