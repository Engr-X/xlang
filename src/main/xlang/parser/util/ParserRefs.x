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

#file.outerClass("ParserRefs")
package xlang.parser.util

import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.parser.ParseContainer
import xlang.util.ArrayList


struct ParserRefs
{
    private var parser: pointer<ParserRef>

    private var splitBy: pointer<PatternList>

    private var allowTrailing: bool

    private var extraTokens: pointer<ArrayList>

    private var results: pointer<ArrayList>


    constructor(parser: pointer<ParserRef>)
    {
        this.parser = parser
        this.splitBy = null
        this.allowTrailing = false
        this.extraTokens = new ArrayList(sizeof(Token))
        this.results = new ArrayList(sizeof(pointer<*>))
    }


    constructor(parser: pointer<ParserRef>, splitBy: pointer<PatternAtom>)
    {
        this.parser = parser
        this.splitBy = if splitBy == null:
                null
            else:
                new PatternList(splitBy)

        this.allowTrailing = true
        this.extraTokens = new ArrayList(sizeof(Token))
        this.results = new ArrayList(sizeof(pointer<*>))
    }


    constructor(parser: pointer<ParserRef>, splitBy: pointer<PatternList>)
    {
        this.parser = parser
        this.splitBy = splitBy
        this.allowTrailing = true
        this.extraTokens = new ArrayList(sizeof(Token))
        this.results = new ArrayList(sizeof(pointer<*>))
    }


    constructor(parser: pointer<ParserRef>, splitBy: pointer<PatternAtom>, allowTrailing: bool)
    {
        this.parser = parser
        this.splitBy = if splitBy == null:
                null
            else:
                new PatternList(splitBy)

        this.allowTrailing = allowTrailing
        this.extraTokens = new ArrayList(sizeof(Token))
        this.results = new ArrayList(sizeof(pointer<*>))
    }


    constructor(parser: pointer<ParserRef>, splitBy: pointer<PatternList>, allowTrailing: bool)
    {
        this.parser = parser
        this.splitBy = splitBy
        this.allowTrailing = allowTrailing
        this.extraTokens = new ArrayList(sizeof(Token))
        this.results = new ArrayList(sizeof(pointer<*>))
    }


    private fun parseFirst(tokens: pointer<TokenList>, index: int) -> int
    {
        val consumed: int = this.parser.parse(tokens, index)

        if this.parser.haveError(consumed) || consumed <= 0:
            return 0

        val result: pointer<ParseContainer> = this.parser.getResult()

        this.results.push(result.ref)
        return consumed
    }


    private fun parseWithoutSplit(tokens: pointer<TokenList>, index: int, firstConsumed: int) -> int
    {
        var consumed: int = firstConsumed

        while index + consumed < tokens.length():
        {
            val innerConsumed: int = this.parser.parse(tokens, index + consumed)

            if this.parser.haveError(innerConsumed) || innerConsumed <= 0:
                break

            val result: pointer<ParseContainer> = this.parser.getResult()

            this.results.push(result.ref)
            consumed += innerConsumed
        }

        return consumed
    }


    private fun parseWithSplit(tokens: pointer<TokenList>, index: int, firstConsumed: int) -> int
    {
        var consumed: int = firstConsumed

        if this.splitBy.length() <= 0:
            return consumed

        while index + consumed < tokens.length():
        {
            if !this.splitBy.regMatch(tokens, index + consumed):
                break

            val splitStart: int = index + consumed
            val splitLength: int = this.splitBy.length()
            val nextIndex: int = index + consumed + splitLength

            if nextIndex >= tokens.length():
                break

            val innerConsumed: int = this.parser.parse(tokens, nextIndex)

            if this.parser.haveError(innerConsumed) || innerConsumed <= 0:
                break

            val result: pointer<ParseContainer> = this.parser.getResult()

            this.results.push(result.ref)

            for (var i = 0; i < splitLength; i++):
                this.extraTokens.push(tokens.get(splitStart + i))

            consumed += splitLength + innerConsumed
        }

        if this.allowTrailing && this.splitBy.regMatch(tokens, index + consumed):
        {
            val splitStart: int = index + consumed

            for (var i = 0; i < this.splitBy.length(); i++):
                this.extraTokens.push(tokens.get(splitStart + i))

            consumed += this.splitBy.length()
        }

        return consumed
    }


    fun parse(tokens: pointer<TokenList>, index: int) -> int
    {
        this.results = new ArrayList(sizeof(pointer<*>))
        this.extraTokens = new ArrayList(sizeof(Token))

        if this.parser == null || tokens == null || index < 0 || index >= tokens.length():
            return 0

        val firstConsumed: int = this.parseFirst(tokens, index)

        if firstConsumed <= 0:
            return 0

        return if this.splitBy == null:
                this.parseWithoutSplit(tokens, index, firstConsumed)
            else:
                this.parseWithSplit(tokens, index, firstConsumed)
    }


    fun getResult() -> pointer<ParseContainer> =
        new ParseContainer(ParseContainer.ARRAY_LIST_KIND, this.results)


    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens


    fun clone() -> pointer<ParserRefs> =
        if this.splitBy == null:
            new ParserRefs(this.parser.clone())
        else:
            new ParserRefs(this.parser.clone(), this.splitBy, this.allowTrailing)
}
