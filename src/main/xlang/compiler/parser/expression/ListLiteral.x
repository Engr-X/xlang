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
 */

package xlang.compiler.parser.expression

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList


/**
 * Represents a list literal composed of multiple expressions.
 *
 * <p>A {@code ListLiteral} stores an ordered collection of expressions together
 * with additional syntax tokens associated with the literal itself.
 *
 * <p>The expression list preserves insertion order. Additional tokens may be
 * used to retain delimiters such as brackets, commas, or other syntax that is
 * not directly owned by the contained expressions.
 *
 * <p>All tokens belonging to the literal can be collected in source order using
 * {@code getAllTokens()}.
 */
struct ListLiteral
{
    /**
     * The ordered collection of expressions contained in this list literal.
     *
     * <p>Each entry is stored as a pointer to an {@code Expression}.
     */
    private var list: pointer<ArrayList>

    /**
     * Additional syntax tokens associated with this list literal.
     *
     * <p>This collection may contain brackets, commas, or other delimiters that
     * are not directly owned by the contained expressions.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates an empty list literal.
     *
     * <p>A new expression list and a new extra-token collection are allocated.
     */
    constructor()
    {
        this.list = new ArrayList(sizeof(pointer<Expression>))
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Creates a list literal backed by the specified expression list.
     *
     * <p>The supplied list is stored by reference and is not copied. A new
     * empty collection is created for additional syntax tokens.
     *
     * @param list              a pointer to the expression list used by this literal
     */
    constructor(list: pointer<ArrayList>)
    {
        this.list = list
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Appends an expression to this list literal.
     *
     * <p>The expression pointer is stored in the internal list and the
     * expression itself is not copied.
     *
     * @param expression        a pointer to the expression to append
     *
     * @return                  this {@code ListLiteral} instance
     */
    fun addExpression(expression: pointer<Expression>) -> pointer<ListLiteral>
    {
        this.list.push(expression.ref)
        return this
    }


    /**
     * Returns the expression list contained in this literal.
     *
     * @return                  a pointer to the internal expression list
     */
    fun getList() -> pointer<ArrayList> = this.list


    /**
     * Returns the additional syntax tokens associated with this literal.
     *
     * @return                  a pointer to the extra-token collection
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens


    /**
     * Adds an additional syntax token to this expression.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The token is stored by reference and is not copied or cloned.
     *
     * @param token             a pointer to the syntax token to add
     *
     * @return                  this {@code NewIdentifier} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<NewIdentifier>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }
    

    /**
     * Adds an additional syntax token to this list literal.
     *
     * <p>If {@code token} is {@code null}, no token is added.
     *
     * <p>The token is stored by reference and is not copied.
     *
     * @param                   token a pointer to the token to add
     *
     * @return                  this {@code ListLiteral} instance
     */
    fun addExtraTokens(tokens: pointer<ArrayList>) -> pointer<ListLiteral>
    {
        if tokens != null:
            this.extraTokens.pushAll(tokens)

        return this
    }


    /**
     * Returns all tokens associated with this list literal.
     *
     * <p>The tokens belonging to each valid expression in the list are collected
     * first. Null expression slots and null expression references are ignored.
     *
     * <p>The collected expression tokens are combined with the additional syntax
     * tokens stored by this literal.
     *
     * <p>The final token collection is sorted according to source position using
     * {@code TokenPosition.compareToken}.
     *
     * @return                  a newly allocated list containing all tokens associated with this
     *                          list literal in source order
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
