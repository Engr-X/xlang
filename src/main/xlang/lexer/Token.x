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
import xlang.util.string.StringBuilder
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
    static val AnyKind: int = -2147483647 - 1
    static val EOF_STRING: pointer<char> = "<EOF>"

    var kind: int
    var pos: pointer<TokenPosition>
    var text: pointer<char>
    var errorInfo: pointer<char>


    fun __init__(kind: int, pos: pointer<TokenPosition>, text: pointer<char>)
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


    fun __init__(kind: int, pos: pointer<TokenPosition>, text: pointer<char>, errorInfo: pointer<char>)
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
    private var tokens: pointer<ArrayList>


    fun __init__():
        this.tokens = new ArrayList(sizeof(Token))


    fun push(token: pointer<Token>):
        this.tokens.push(token)


    fun length() -> int = this.tokens.length


    fun get(index: int) -> pointer<Token> = this.tokens.get(index) as pointer<Token>


    fun array() -> pointer<ArrayList> = this.tokens


    fun toArray() -> pointer<ArrayList> = this.tokens.clone()


    fun toString(newlineKind: int) -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        for (var i: int = 0; i < this.tokens.length; i++):
        {
            val token: pointer<Token> = this.get(i)

            if token.kind == newlineKind:
                sb.append("\n")
            elif token.isEOF():
                sb.append(Token.EOF_STRING)
            elif token.text != null:
            {
                sb.append(token.text)
                sb.append(' ')
            }
        }

        return sb
    }
}
