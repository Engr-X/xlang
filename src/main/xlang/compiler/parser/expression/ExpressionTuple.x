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

package xlang.compiler.parser.expression

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList


/**
 * Represents an ordered collection of expressions grouped as a tuple.
 *
 * <p>An {@code ExpressionTuple} stores multiple {@code Expression} references
 * together with additional syntax tokens associated with the tuple itself.
 *
 * <p>The expression list preserves insertion order. Additional tokens may be
 * used to retain syntax such as parentheses, commas, or other delimiters that
 * are not directly owned by the contained expressions.
 *
 * <p>All source tokens belonging to the tuple can be collected in source order
 * using {@code getAllTokens()}.
 */
struct ExpressionTuple
{
    /**
     * The ordered collection of expressions contained in this tuple.
     *
     * <p>Each entry is stored as a pointer to an {@code Expression}.
     */
    private var list: pointer<ArrayList>

    /**
     * Additional syntax tokens associated with this tuple.
     *
     * <p>This collection may contain delimiters or other tokens that are not
     * directly owned by the contained expressions.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates an empty expression tuple.
     *
     * <p>A new expression list and a new extra-token list are allocated.
     */
    constructor()
    {
        this.list = new ArrayList(sizeof(pointer<Expression>))
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Creates an expression tuple backed by the specified expression list.
     *
     * <p>The supplied list is stored by reference and is not copied. A new
     * empty collection is created for additional syntax tokens.
     *
     * @param list              a pointer to the expression list used by this tuple
     */
    constructor(list: pointer<ArrayList>)
    {
        this.list = list
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Appends an expression to this tuple.
     *
     * <p>The expression pointer is stored in the internal list and the
     * expression itself is not copied.
     *
     * @param expression        a pointer to the expression to append
     *
     * @return                  this {@code ExpressionTuple} instance
     */
    fun addExpression(expression: pointer<Expression>) -> pointer<ExpressionTuple>
    {
        this.list.push(expression.ref)
        return this
    }


    /**
     * Returns the expression list contained in this tuple.
     *
     * @return                  a pointer to the internal expression list
     */
    fun getList() -> pointer<ArrayList> = this.list


    /**
     * Returns the additional syntax tokens associated with this tuple.
     *
     * @return a pointer to the extra-token collection
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens


    /**
     * Adds an additional syntax token to this tuple.
     *
     * <p>If {@code token} is {@code null}, no token is added.
     *
     * <p>The token is stored by reference and is not copied.
     *
     * @param token             a pointer to the token to add
     *
     * @return                  this {@code ExpressionTuple} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<ExpressionTuple>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Adds all tokens from the specified collection to this tuple.
     *
     * <p>If {@code tokens} is {@code null}, no changes are made.
     *
     * <p>The token references are appended to the internal extra-token
     * collection and are not cloned.
     *
     * @param tokens            a pointer to the token collection to append
     * @return                  this {@code ExpressionTuple} instance
     */
    fun addExtraTokens(tokens: pointer<ArrayList>) -> pointer<ExpressionTuple>
    {
        if tokens != null:
            this.extraTokens.pushAll(tokens)

        return this
    }


    /**
     * Returns all tokens associated with this expression tuple.
     *
     * <p>The tokens of every valid expression in the tuple are collected first.
     * Null expression slots and null expression references are ignored.
     *
     * <p>The collected expression tokens are combined with the additional syntax
     * tokens stored by this tuple.
     *
     * <p>The final token collection is sorted according to source position using
     * {@code TokenPosition.compareToken}.
     *
     * @return                  a newly allocated list containing all tokens associated with this
     *                          tuple in source order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = this.extraTokens.clone()

        for (var i = 0; i < this.list.length; i++):
        {
            val slot: pointer<pointer<Expression>> = this.list.get(i) as pointer<pointer<Expression>>

            if slot == null || slot.deref == null:
                continue

            val expression: pointer<Expression> = slot.deref
            val tokens: pointer<ArrayList> = expression.getAllTokens()

            if tokens == null:
                continue

            result.pushAll(tokens)
        }

        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }
}
