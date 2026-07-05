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
import xlang.util.string.String
import xlang.util.string.StringBuilder


struct LexPosition
{
    static val START_POSITION: LexPosition = LexPosition(0, 1, 1)

    static fun copy(pos: LexPosition) -> LexPosition = new LexPosition(pos.offset, pos.line, pos.column)

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


    inline fun toTokenPosition(length: int) -> TokenPosition = new TokenPosition(this.offset, this.line, this.column, length)
}


struct LexInput
{
    var pos: LexPosition
    var prevChar: char
    var text: pointer<char>
    var textLength: int


    fun __init__(pos: LexPosition, prevChar: char, text: pointer<char>, textLength: int)
    {
        this.pos = pos
        this.prevChar = prevChar
        this.text = text
        this.textLength = textLength
    }


    fun toTokenPosition(dest: TokenPosition, length: int)
    {
        dest.line = this.pos.line
        dest.offset = this.pos.offset
        dest.column = this.pos.column
        dest.length = length
    }
}


struct Rule
{
    var id: int
    var state: int
    var pattern: pointer<char>
    var action: (LexInput, LexState) -> Token


    fun __init__(id: int, state: int, pattern: pointer<char>, action: (LexInput, LexState) -> Token)
    {
        this.id = id
        this.state = state
        this.pattern = pattern
        this.action = action
    }
}


struct LexState
{
    static val DEFAULT: int = 1

    var code: pointer<char>
    var state: int
    var accumulator: StringBuilder
    var cursorPos: LexPosition


    fun __init__(code: pointer<char>)
    {
        this.code = code
        this.state = DEFAULT
        this.accumulator = new StringBuilder()
        this.cursorPos = new LexPosition(0, 1, 1)
    }
    

    fun setState(state: int):
        this.state = state


    fun getState(): int = this.state


    fun append(c: char):
        this.accumulator.append(c)


    fun append(str: pointer<char>):
        this.accumulator.append(str)


    fun getCursorPtr() -> LexPosition = this.cursorPos


    fun updateCursor(n: int)
    {
        this.cursorPos.offset += n
        this.cursorPos.column += n
    }


    /**
     * Applies the first matching rule for the current lexer state and cursor.
     *
     * @param rules             pointer to the ordered rule table
     * @param rulesLength       number of rules available in the table
     * @return                  matched token, null for skipped input, or EOF at default-state end
     */
    fun apply(rules: pointer<pointer<Rule>>, rulesLength: int) -> Token
    {
        val currentPtr: pointer<char> = this.code + this.cursorPos.offset;

        if currentPtr.deref == String.NULL_CHAR && this.state == DEFAULT:
            return new Token(Token.EOF_KIND, null, null)

        for (var i: int = 0; i < rulesLength; i++):
        {
            val rule: Rule = rules[i]

            if rule.state == this.state:
            {
                val matchLength: int = String.strRegMatch(rule.pattern, currentPtr)

                if matchLength >= 0:
                {
                    val preChar: char = if this.cursorPos.offset == 0: -1 else: this.code[this.cursorPos.offset - 1] as int
                    val token: pointer<char> = System.allocMemory((matchLength + 1) * sizeof(char)) as pointer<char>
                    String.strncpy(token, currentPtr, matchLength)

                    val inputPos: LexPosition = LexPosition.copy(this.cursorPos)
                    val input: LexInput = LexInput(inputPos, preChar, token, matchLength)
                    val result: Token = rule.action(input, this)
                    return result
                }
            }
        }

        return null
    }
}
