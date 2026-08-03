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

@file.class("ParsedObjects")
package xlang.parser

import xlang.lexer.PatternList
import xlang.lexer.TokenList
import xlang.util.ArrayList


/**
 * Repeatedly parses objects using one shared ParsedObject parser.
 *
 * ParsedObjects applies the same parser-rule set multiple times to the
 * beginning of an input TokenList. Each successful iteration consumes one
 * matching token prefix and constructs one result object.
 *
 * Parsing continues until the input becomes empty or the internal parser
 * cannot consume a positive number of tokens.
 *
 * A failed single-object parse is treated as the normal end of the repeated
 * sequence. Therefore, this structure implements zero-or-more parsing rather
 * than requiring at least one successful match.
 *
 * Produced objects are stored as pointer<*> values. Their concrete types and
 * ownership rules are determined by the supplied result constructor.
 *
 * PatternList rules and result objects are referenced by pointer and are not
 * deeply copied.
 */
struct ParsedObjects
{
    /**
     * Stores result objects produced by the most recent parse operation.
     *
     * Each ArrayList element contains one pointer<*> value returned by the
     * internal ParsedObject result constructor.
     *
     * The pointed objects are not copied by ParsedObjects.
     */
    private var results: pointer<ArrayList>

    /**
     * Stores the parser used to consume one object per iteration.
     *
     * All pattern rules and the result constructor are delegated to this
     * ParsedObject.
     */
    private var parser: pointer<ParsedObject>


    /**
     * Initializes an empty repeated-object parser.
     *
     * A new result list and internal ParsedObject are allocated. The supplied
     * result constructor is stored by the internal parser and is invoked after
     * each successfully consumed token prefix.
     *
     * The result constructor may create an expression, statement, declaration
     * or any other parser-owned object.
     *
     * The caller must provide a valid result constructor and keep it callable
     * while this ParsedObjects instance is used.
     *
     * @warning                     A null or invalid result constructor causes undefined behavior
     *                              when getResult is invoked by the internal parser.
     */
    fun __init__(singleParser: pointer<ParsedObject>)
    {
        this.parser = singleParser
        this.results = new ArrayList(sizeof(pointer<*>))
    }


    fun parse(token: pointer<TokenList>, index: int) -> int
    {
        this.results = new ArrayList(sizeof(pointer<*>))

        if this.parser == null || token == null || index < 0 || index >= token.length():
            return 0

        var consumed: int = 0

        while index + consumed < token.length():
        {
            val innerConsumed: int = this.parser.parse(token, index + consumed)

            if innerConsumed <= 0 || !this.parser.lastTrySuccess():
                break

            val resultSpace: blob[sizeof(pointer<*>)]
            val resultSlot: pointer<pointer<*>> = resultSpace as pointer<pointer<*>>

            resultSlot.deref = this.parser.getResult()
            this.results.push(resultSlot)
            consumed += innerConsumed
        }

        return consumed
    }


    fun getResult() -> pointer<ArrayList> = this.results
}
