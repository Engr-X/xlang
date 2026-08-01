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
 * ParsedObject is a lightweight parser result container. It records the
 * parser-defined kind of the fragment and the tokens covered by that
 * fragment.
 *
 * The token list is stored as a TokenList pointer. Token objects inside
 * the list are still referenced by pointer and are not deeply copied.
 */
struct ParsedObject
{
    /**
     * Identifies an object whose parser kind has not been assigned yet.
     */
    static val UNKNOWNKIND: int = -1
    

    /**
     * Stores the parser-defined kind of this object.
     *
     * The meaning of the value is owned by the parser layer.
     */
    var kind: int

    /**
     * Stores the tokens covered by this parsed object.
     */
    var tokens: pointer<TokenList>

    /**
     * Computes how many tokens this parsed object consumes.
     *
     * The function receives a TokenList pointer and returns the number
     * of tokens that should be eaten by the parser.
     */
    var parseFunction: (pointer<TokenList>) -> int


    /**
     * Initializes a parsed object with a kind and parse function.
     *
     * @param kind              parser-defined object kind
     * @param parseFunction     function that returns how many tokens to consume
     */
    fun __init__(kind: int, parseFunction: (pointer<TokenList>) -> int)
    {
        this.kind = kind
        this.tokens = new TokenList()
        this.parseFunction = parseFunction
    }


    /**
     * Initializes a parsed object with a kind, token list and parse function.
     *
     * @param kind              parser-defined object kind
     * @param tokens            tokens covered by this parsed object
     * @param parseFunction     function that returns how many tokens to consume
     */
    fun __init__(kind: int, tokens: pointer<TokenList>, parseFunction: (pointer<TokenList>) -> int)
    {
        this.kind = kind
        this.tokens = tokens
        this.parseFunction = parseFunction
    }


    /**
     * Appends a token to this object's token list.
     *
     * The token object is referenced directly by the TokenList.
     *
     * @param token             token to append
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
     * The parse function is called first. Its result is treated as the
     * number of tokens to consume from the beginning of input. The consumed
     * tokens are copied into this object's TokenList.
     *
     * Token objects are not deeply copied. The destination list stores
     * copies of Token values that reference the same token text and position
     * data as the source tokens.
     *
     * @param input             source token list
     * @return                  consumed token count, or -1 if the count is invalid
     */
    fun doParse(input: pointer<TokenList>) -> int
    {
        val index: int = this.parseFunction(input)

        if index < 0 || index > input.length():
            return -1

        val tokens: pointer<TokenList> = input.subToken(0, index)

        if tokens == null:
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
