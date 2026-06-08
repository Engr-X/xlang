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
