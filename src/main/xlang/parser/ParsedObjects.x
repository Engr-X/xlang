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
 * ParsedObjects is the plural form of ParsedObject. The main difference is
 * that tokens stores an array of TokenList values instead of one TokenList
 * pointer.
 *
 * The array is represented by ArrayList until the language has a generic
 * Array<TokenList> type. Each stored element is a byte copy of a TokenList
 * value.
 */
struct ParsedObjects
{
    /**
     * Identifies an object whose parser kind has not been assigned yet.
     */
    static val UNKNOWNKIND: int = -1


    /**
     * Stores the parser-defined kind of this object group.
     */
    var kind: int

    /**
     * Stores token-list groups covered by this parsed object group.
     *
     * This ArrayList stores TokenList values, not pointer<TokenList> slots.
     */
    var tokens: pointer<ArrayList>

    /**
     * Computes how many tokens this parsed object group consumes.
     */
    var parseFunction: (pointer<TokenList>) -> int


    /**
     * Initializes a parsed object group with a kind and parse function.
     *
     * @param kind              parser-defined object kind
     * @param parseFunction     function that returns how many tokens to consume
     */
    fun __init__(kind: int, parseFunction: (pointer<TokenList>) -> int)
    {
        this.kind = kind
        this.tokens = new ArrayList(sizeof(TokenList))
        this.parseFunction = parseFunction
    }


    /**
     * Appends a token-list group.
     *
     * The TokenList value is copied into the backing ArrayList.
     *
     * @param tokens            token-list group to append
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
     * @return                  token-list group pointer, or null for an invalid index
     */
    fun get(index: int) -> pointer<TokenList> =
        this.tokens.get(index) as pointer<TokenList>


    /**
     * Parses token-list groups from a token list.
     *
     * The parse function is called repeatedly on the current input.
     * Each positive result creates one TokenList group from the input prefix,
     * appends that group to tokens, and immediately removes the same prefix
     * from input.
     *
     * The parse function must not mutate input. This function owns the input
     * cursor movement through remove().
     *
     * A -1 result means the repeated parse is finished. Other negative values,
     * zero, or ranges larger than the remaining input are treated as invalid.
     *
     * @param input             source token list
     * @return                  total consumed token count, or -1 if a count is invalid
     */
    fun doParse(input: pointer<TokenList>) -> int
    {
        var consumed: int = 0
        this.tokens = new ArrayList(sizeof(TokenList))

        while input.length() > 0:
        {
            val index: int = this.parseFunction(input)

            if index == -1:
                break

            if index <= 0 || index > input.length():
                return -1

            val tokens: pointer<TokenList> = input.subToken(0, index)

            if tokens == null:
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
    fun length() -> int =
        this.tokens.length
}
