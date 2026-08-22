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

@file.class("ParserRef")
package xlang.parser.util

import xlang.Diagnostic
import xlang.lexer.TokenList
import xlang.parser.ParseContainer
import xlang.parser.PrattParser
import xlang.parser.RecursiveParser
import xlang.parser.TypeParser


struct ParserRef
{
    static val RECURSIVE_DOWN: int = 0

    static val PRATT: int = 1

    static val TYPE_PARSER: int = 2


    private var id: int

    private var type: int

    private var host: pointer<*>


    static fun fromRecursiveDown(id: int) -> pointer<ParserRef> =
        new ParserRef(id, RECURSIVE_DOWN, new RecursiveParser(id))


    static fun fromPratt(id: int, host: pointer<PrattParser>) -> pointer<ParserRef> =
        new ParserRef(id, PRATT, host.setId(id))


    static fun fromType(id: int) -> pointer<ParserRef> =
        new ParserRef(id, TYPE_PARSER, new TypeParser(id))


    private fun __init__(id: int, type: int, host: pointer<*>)
    {
        this.id = id
        this.type = type
        this.host = host
    }


    fun getId() -> int = this.id


    fun addRule(rule: pointer<Rule>) -> pointer<ParserRef>
    {
        if this.type == RECURSIVE_DOWN:
        {
            val parser: pointer<RecursiveParser> = this.host as pointer<RecursiveParser>
            parser.addRule(rule)
        }

        if this.type == PRATT:
        {
            val parser: pointer<PrattParser> = this.host as pointer<PrattParser>

            if rule.role == Rule.STARTER_ROLE:
                parser.addStarterRule(rule)
            else:
                parser.addContinuationRule(rule)
        }

        return this
    }


    fun parse(tokens: pointer<TokenList>, index: int) -> int
    {
        if this.type == RECURSIVE_DOWN:
        {
            val parser: pointer<RecursiveParser> = this.host as pointer<RecursiveParser>
            return parser.parse(tokens, index)
        }

        if this.type == PRATT:
        {
            val parser: pointer<PrattParser> = this.host as pointer<PrattParser>
            return parser.parse(tokens, index)
        }

        if this.type == TYPE_PARSER:
        {
            val parser: pointer<TypeParser> = this.host as pointer<TypeParser>
            return parser.parse(tokens, index)
        }

        return -1
    }


    fun doParse(input: pointer<TokenList>) -> int
    {
        if input == null:
            return -1

        val consumed: int = this.parse(input, 0)

        if this.haveError(consumed) || consumed > input.length():
            return -1

        input.remove(0, consumed)
        return consumed
    }


    fun haveError(eaten: int) -> bool
    {
        if this.type == RECURSIVE_DOWN:
        {
            val parser: pointer<RecursiveParser> = this.host as pointer<RecursiveParser>
            return parser.haveError(eaten)
        }

        if this.type == PRATT:
        {
            val parser: pointer<PrattParser> = this.host as pointer<PrattParser>
            return parser.haveError(eaten)
        }

        if this.type == TYPE_PARSER:
        {
            val parser: pointer<TypeParser> = this.host as pointer<TypeParser>
            return parser.haveError(eaten)
        }

        return true
    }


    fun getResult() -> pointer<ParseContainer>
    {
        if this.type == RECURSIVE_DOWN:
        {
            val parser: pointer<RecursiveParser> = this.host as pointer<RecursiveParser>
            return parser.getResult()
        }

        if this.type == PRATT:
        {
            val parser: pointer<PrattParser> = this.host as pointer<PrattParser>
            return parser.getResult() as pointer<ParseContainer>
        }

        if this.type == TYPE_PARSER:
        {
            val parser: pointer<TypeParser> = this.host as pointer<TypeParser>
            return parser.getResult()
        }

        return null
    }


    fun getError() -> pointer<Diagnostic>
    {
        if this.type == RECURSIVE_DOWN:
        {
            val parser: pointer<RecursiveParser> = this.host as pointer<RecursiveParser>
            return parser.getError()
        }

        if this.type == PRATT:
        {
            val parser: pointer<PrattParser> = this.host as pointer<PrattParser>
            return parser.getLastError()
        }

        if this.type == TYPE_PARSER:
        {
            val parser: pointer<TypeParser> = this.host as pointer<TypeParser>
            return parser.getError()
        }

        return null
    }


    fun getHost() -> pointer<*> = this.host


    fun clone() -> pointer<ParserRef>
    {
        if this.type == RECURSIVE_DOWN:
        {
            val parser: pointer<RecursiveParser> = this.host as pointer<RecursiveParser>
            return new ParserRef(this.id, this.type, parser.clone())
        }

        if this.type == PRATT:
        {
            val parser: pointer<PrattParser> = this.host as pointer<PrattParser>
            return new ParserRef(this.id, this.type, parser.clone())
        }

        if this.type == TYPE_PARSER:
        {
            val parser: pointer<TypeParser> = this.host as pointer<TypeParser>
            return new ParserRef(this.id, this.type, parser.clone())
        }

        return null
    }
}
