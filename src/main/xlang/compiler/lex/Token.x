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

package xlang.compiler.lex


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
    var type: int
    var pos: TokenPosition
    var text: pointer<char>
}
