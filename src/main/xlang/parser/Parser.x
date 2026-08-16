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

@file.class("Parser")
package xlang.parser

import xlang.Diagnostic
import xlang.lexer.TokenList


struct Parser
{
    static val RECURSIVE_DOWN: int = 0


    private var type: int

    private var host: pointer<*>


    fun __init__(type: int, host: pointer<*>)
    {
        this.type = type
        this.host = host
    }


    fun parse(tokens: pointer<TokenList>, index: int) -> int
    {
        if this.type == RECURSIVE_DOWN:
        {
            val parser: pointer<RecursiveParser> = this.host as pointer<RecursiveParser>
            return parser.parse(tokens, index)
        }

        return -1
    }


    fun doParse(input: pointer<TokenList>) -> int
    {
        if input == null:
            return -1

        val consumed: int = this.parse(input, 0)

        if consumed <= 0 || !this.lastTrySuccess() || consumed > input.length():
            return -1

        input.remove(0, consumed)
        return consumed
    }


    fun getResult() -> pointer<*>
    {
        if this.type == RECURSIVE_DOWN:
        {
            val parser: pointer<RecursiveParser> = this.host as pointer<RecursiveParser>
            return parser.getResult()
        }

        return null
    }


    fun lastTrySuccess() -> bool
    {
        if this.type == RECURSIVE_DOWN:
        {
            val parser: pointer<RecursiveParser> = this.host as pointer<RecursiveParser>
            return parser.lastTrySuccess()
        }

        return false
    }


    fun getLastError() -> pointer<Diagnostic>
    {
        if this.type == RECURSIVE_DOWN:
        {
            val parser: pointer<RecursiveParser> = this.host as pointer<RecursiveParser>
            return parser.getLastError()
        }

        return null
    }


    fun getHost() -> pointer<*> = this.host
}
