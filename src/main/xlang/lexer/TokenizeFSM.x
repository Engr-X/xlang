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
 */

package xlang.lexer

import xlang.System
import xlang.util.string.String
import xlang.util.string.StringBuilder


struct TokenizeRule
{
    var id: int
    var state: int
    var pattern: pointer<char>
    var action: (pointer<LexInput>, pointer<TokenizeFSM>) -> pointer<Token>


    fun __init__(id: int, state: int, pattern: pointer<char>, action: (pointer<LexInput>, pointer<TokenizeFSM>) -> pointer<Token>)
    {
        this.id = id
        this.state = state
        this.pattern = pattern
        this.action = action
    }
}


struct TokenizeFSM
{
    static val DEFAULT: int = 1

    var code: pointer<char>
    var state: int
    var accumulator: pointer<StringBuilder>
    var cursorPos: pointer<LexPosition>
    private var inputPos: pointer<LexPosition>
    private var input: pointer<LexInput>


    fun __init__(code: pointer<char>)
    {
        this.code = code
        this.state = DEFAULT
        this.accumulator = new StringBuilder()
        this.cursorPos = new LexPosition(0, 1, 1)
        this.inputPos = new LexPosition(0, 1, 1)
        this.input = new LexInput(this.inputPos, 0 as char, null as pointer<char>, 0)
    }


    fun setState(state: int):
        this.state = state


    fun getState(): int = this.state


    fun append(c: char):
        this.accumulator.append(c)


    fun append(str: pointer<char>):
        this.accumulator.append(str)


    fun getCursorPtr() -> pointer<LexPosition> = this.cursorPos


    fun updateCursor(n: int)
    {
        this.cursorPos.offset += n
        this.cursorPos.column += n
    }


    /**
     * Applies the first matching tokenize rule for the current FSM state and cursor.
     *
     * @param rules             pointer to the ordered rule table
     * @param rulesLength       number of rules available in the table
     * @return                  matched token, null for skipped input, or EOF at default-state end
     */
    fun apply(rules: pointer<pointer<TokenizeRule>>, rulesLength: int) -> pointer<Token>
    {
        val currentPtr: pointer<char> = this.code + this.cursorPos.offset

        for (var i: int = 0; i < rulesLength; i++):
        {
            val rule: pointer<TokenizeRule> = rules[i]

            if rule.state == this.state:
            {
                val matchLength: int = String.strRegMatch(rule.pattern, currentPtr)

                if matchLength >= 0:
                {
                    val preChar: char = if this.cursorPos.offset == 0: -1 else: this.code[this.cursorPos.offset - 1] as int
                    val token: pointer<char> = System.allocMemory((matchLength + 1) * sizeof(char)) as pointer<char>
                    String.strncpy(token, currentPtr, matchLength)

                    this.inputPos.offset = this.cursorPos.offset
                    this.inputPos.line = this.cursorPos.line
                    this.inputPos.column = this.cursorPos.column

                    this.input.prevChar = preChar
                    this.input.text = token
                    this.input.textLength = matchLength

                    val result: pointer<Token> = rule.action(this.input, this)
                    return result
                }
            }
        }

        return null
    }
}
