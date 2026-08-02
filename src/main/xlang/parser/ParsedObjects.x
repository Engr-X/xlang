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

import xlang.lexer.TokenList
import xlang.util.ArrayList


/**
 * Represents a parsed fragment made of multiple token-list groups.
 *
 * ParsedObjects is the plural form of ParsedObject. It repeatedly applies one
 * parse function and stores each consumed token-list group.
 *
 * The array is represented by ArrayList until the language has a generic
 * Array<TokenList> type. Each stored element is a byte copy of a TokenList
 * value.
 */
struct ParsedObjects
{
    /**
     * Stores token-list groups covered by this parsed object group.
     *
     * This ArrayList stores TokenList values, not pointer<TokenList> slots.
     */
    private var tokens: pointer<ArrayList>

    /**
     * Parses one token-list group from the input prefix.
     *
     * A null result means there is no next group.
     */
    private var parseFunction: (pointer<TokenList>) -> pointer<TokenList>


    /**
     * Initializes a parsed object group with a parse function.
     *
     * @param parseFunction     function that returns the next token-list group
     */
    fun __init__(parseFunction: (pointer<TokenList>) -> pointer<TokenList>)
    {
        this.tokens = new ArrayList(sizeof(TokenList))
        this.parseFunction = parseFunction
    }


    /**
     * Appends a token-list group.
     *
     * The TokenList value is copied into the backing ArrayList.
     *
     * @param tokens            token-list group to append
     *
     * @return                  this object group for chained calls
     */
    fun push(tokens: pointer<TokenList>) -> pointer<ParsedObjects>
    {
        this.tokens.push(tokens)
        return this
    }


    /**
     * Returns a token-list group by index.
     *
     * @param index             zero-based group index
     *
     * @return                  token-list group pointer, or null for an invalid index
     */
    fun get(index: int) -> pointer<TokenList> =
        this.tokens.get(index) as pointer<TokenList>


    /**
     * Parses token-list groups from a token list.
     *
     * The parse function is called repeatedly on the current input.
     * Each non-null result is treated as one consumed token-list group,
     * appended to tokens, and immediately removed from input by group length.
     *
     * The parse function must not mutate input. This function owns the input
     * cursor movement through remove().
     *
     * A null result means the repeated parse is finished. Empty groups or
     * groups larger than the remaining input are treated as invalid.
     *
     * @param input             source token list
     *
     * @return                  total consumed token count, or -1 if a group is invalid
     */
    fun doParse(input: pointer<TokenList>) -> int
    {
        var consumed: int = 0
        this.tokens = new ArrayList(sizeof(TokenList))

        while input.length() > 0:
        {
            val tokens: pointer<TokenList> = this.parseFunction(input)

            if tokens == null:
                break

            val index: int = tokens.length()

            if index <= 0 || index > input.length():
                return -1

            this.tokens.push(tokens)
            input.remove(0, index)
            consumed += index
        }

        return consumed
    }


    /**
     * Returns the number of token-list groups covered by this object group.
     *
     * @return                  token-list group count
     */
    fun length() -> int = this.tokens.length
}
