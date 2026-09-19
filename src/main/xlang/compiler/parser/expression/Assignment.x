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
import xlang.util.string.StringBuilder


/**
 * Represents an assignment expression.
 *
 * <p>An {@code Assignment} stores the target expression on the left-hand side
 * and the value expression on the right-hand side.
 *
 * <p>Additional syntax tokens associated with the assignment, such as the
 * assignment operator itself, may be stored separately in {@code extraTokens}.
 *
 * <p>The original source tokens of both expressions and all extra tokens can be
 * collected in source order using {@code getAllTokens()}.
 */
struct Assignment
{
    /**
     * The target expression that receives the assigned value.
     */
    private var target: pointer<Expression>

    /**
     * The expression whose result is assigned to the target.
     */
    private var value: pointer<Expression>

    /**
     * Additional syntax tokens associated with this assignment expression.
     *
     * <p>This collection may contain tokens that are not directly owned by the
     * target or value expressions, such as the assignment operator.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates an assignment expression with the specified target and value.
     *
     * <p>The target and value expressions are stored by reference and are not
     * copied. A new empty collection is created for additional syntax tokens.
     *
     * @param target            a pointer to the target expression
     * @param value             a pointer to the value expression
     */
    constructor(target: pointer<Expression>, value: pointer<Expression>)
    {
        this.target = target
        this.value = value
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Returns the target expression of this assignment.
     *
     * @return                  a pointer to the target expression
     */
    fun getTarget() -> pointer<Expression> = this.target


    /**
     * Returns the value expression of this assignment.
     *
     * @return                  a pointer to the assigned value expression
     */
    fun getValue() -> pointer<Expression> = this.value


    /**
     * Adds an additional syntax token to this assignment expression.
     *
     * <p>If {@code token} is {@code null}, no token is added.
     *
     * <p>The token is stored by reference and is not copied.
     *
     * @param token             a pointer to the token to add
     *
     * @return                  this {@code Assignment} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<Assignment>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Returns all tokens associated with this assignment expression.
     *
     * <p>The returned collection contains the tokens belonging to the target
     * expression, the value expression, and all additional syntax tokens stored
     * by this assignment.
     *
     * <p>Tokens are sorted according to their source positions using
     * {@code TokenPosition.compareToken}.
     *
     * @return                  a newly allocated list containing all associated tokens in source
     *                          order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.target != null:
        {
            val tokens: pointer<ArrayList> = this.target.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        if this.value != null:
        {
            val tokens: pointer<ArrayList> = this.value.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns a textual representation of this assignment expression.
     *
     * <p>The generated representation has the form:
     *
     * <pre>
     * (target = value)
     * </pre>
     *
     * <p>If either the target or value expression is {@code null}, its textual
     * representation is omitted while the assignment operator and surrounding
     * parentheses are still produced.
     *
     * @return                  a pointer to a newly created {@code StringBuilder} containing the
     *                          textual representation of this assignment
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder("(")

        if this.target != null:
            sb.append(this.target.toString())

        sb.append(" = ")

        if this.value != null:
            sb.append(this.value.toString())

        sb.append(')')
        return sb
    }
}
