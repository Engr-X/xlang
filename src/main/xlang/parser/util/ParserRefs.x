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

@file.class("ParserRefs")
package xlang.parser.util

import xlang.lexer.TokenList
import xlang.parser.ParseContainer
import xlang.util.ArrayList


struct ParserRefs
{
    private var parser: pointer<ParserRef>

    private var results: pointer<ArrayList>


    fun __init__(parser: pointer<ParserRef>)
    {
        this.parser = parser
        this.results = new ArrayList(sizeof(pointer<*>))
    }


    fun parse(tokens: pointer<TokenList>, index: int) -> int
    {
        this.results = new ArrayList(sizeof(pointer<*>))

        if this.parser == null || tokens == null || index < 0 || index >= tokens.length():
            return 0

        var consumed: int = 0

        while index + consumed < tokens.length():
        {
            val innerConsumed: int = this.parser.parse(tokens, index + consumed)

            if this.parser.haveError(innerConsumed):
                break

            val result: pointer<ParseContainer> = this.parser.getResult()

            this.results.push(result.ref)
            consumed += innerConsumed
        }

        return consumed
    }


    fun getResult() -> pointer<ParseContainer> =
        new ParseContainer(ParseContainer.ARRAY_LIST_KIND, this.results)
}
