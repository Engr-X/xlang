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
import xlang.TokenPosition


struct LexPosition
{
    static val START_POSITION: pointer<LexPosition> = new LexPosition(0, 1, 1)

    static fun copy(pos: pointer<LexPosition>) -> pointer<LexPosition> = new LexPosition(pos.offset, pos.line, pos.column)

    // for computer
    var offset: int

    var line: int
    var column: int


    fun __init__(offset: int, line: int, column: int)
    {
        this.offset = offset
        this.line = line
        this.column = column
    }


    inline fun toTokenPosition(length: int) -> pointer<TokenPosition> = new TokenPosition(this.offset, this.line, this.column, length)
}


struct LexInput
{
    var pos: pointer<LexPosition>
    var prevChar: char
    var text: pointer<char>
    var textLength: int


    fun __init__(pos: pointer<LexPosition>, prevChar: char, text: pointer<char>, textLength: int)
    {
        this.pos = pos
        this.prevChar = prevChar
        this.text = text
        this.textLength = textLength
    }


    fun toTokenPosition(dest: pointer<TokenPosition>, length: int)
    {
        dest.line = this.pos.line
        dest.offset = this.pos.offset
        dest.column = this.pos.column
        dest.length = length
    }
}
