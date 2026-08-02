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

@file.class("ParsedObject")
package xlang.parser

import xlang.lexer.Token
import xlang.lexer.TokenList

/**
 * Represents a parsed fragment of source tokens.
 *
 * ParsedObject is a lightweight parser executor and result container. It
 * records the tokens consumed by one parse function.
 *
 * The token list is stored as a TokenList pointer. Token objects inside
 * the list are still referenced by pointer and are not deeply copied.
 */
struct ParsedObject
{
    /**
     * Stores the tokens covered by this parsed object.
     */
    private var tokens: pointer<TokenList>

    /**
     * Parses the token prefix covered by this object.
     *
     * The function receives a TokenList pointer and returns the token prefix
     * that should be eaten by the parser. A null result means parse failure.
     */
    private var parseFunction: (pointer<TokenList>) -> pointer<TokenList>


    /**
     * Initializes a parsed object with a parse function.
     *
     * @param parseFunction     function that returns the token prefix to consume
     */
    fun __init__(parseFunction: (pointer<TokenList>) -> pointer<TokenList>)
    {
        this.tokens = new TokenList()
        this.parseFunction = parseFunction
    }


    /**
     * Initializes a parsed object with a token list and parse function.
     *
     * @param tokens            tokens covered by this parsed object
     * @param parseFunction     function that returns the token prefix to consume
     */
    fun __init__(tokens: pointer<TokenList>, parseFunction: (pointer<TokenList>) -> pointer<TokenList>)
    {
        this.tokens = tokens
        this.parseFunction = parseFunction
    }


    /**
     * Appends a token to this object's token list.
     *
     * The token object is referenced directly by the TokenList.
     *
     * @param token             token to append
     *
     * @return                  this object for chained calls
     */
    fun push(token: pointer<Token>) -> pointer<ParsedObject>
    {
        this.tokens.push(token)
        return this
    }


    /**
     * Parses from a token list and stores the consumed token prefix.
     *
     * The parse function is called first. Its returned TokenList is treated
     * as the consumed prefix. The prefix length decides how many source tokens
     * are removed from input.
     *
     * Token objects are not deeply copied. The destination list stores
     * copies of Token values that reference the same token text and position
     * data as the source tokens.
     *
     * @param input             source token list
     *
     * @return                  consumed token count, or -1 if the count is invalid
     */
    fun doParse(input: pointer<TokenList>) -> int
    {
        val tokens: pointer<TokenList> = this.parseFunction(input)

        if tokens == null:
            return -1

        val index: int = tokens.length()

        if index < 0 || index > input.length():
            return -1

        this.tokens = tokens
        input.remove(0, index)

        return index
    }


    /**
     * Returns the number of tokens covered by this object.
     *
     * @return                  token count
     */
    fun length() -> int =
        this.tokens.length()
}
