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

package xlang.compiler.parser.statement

import xlang.compiler.parser.expression.Expression
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


/**
 * Represents a statement whose contents consist of a single expression.
 *
 * <p>An {@code ExprStatement} acts as a lightweight statement wrapper around an
 * {@code Expression}. It allows an expression node to participate in statement
 * contexts without changing the expression itself.
 *
 * <p>The wrapped expression is stored by reference and is not copied or cloned
 * by this structure.
 *
 * <p>Additional syntax tokens are retained separately and may contain statement
 * separators or other lexical elements that are not directly owned by the
 * wrapped expression.
 *
 * <p>All tokens associated with the expression statement can be collected in
 * lexical source order using {@code getAllTokens()}.
 */
struct ExprStatement
{
    /**
     * The expression represented by this statement.
     *
     * <p>The expression is stored by reference and may be {@code null} if the
     * statement was constructed without a valid expression.
     */
    private val expr: pointer<Expression>

    /**
     * Additional syntax tokens associated with this expression statement.
     *
     * <p>This collection may contain statement separators or other lexical
     * tokens that are not directly owned by the wrapped expression.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates an expression statement wrapping the specified expression.
     *
     * <p>The supplied expression is stored directly and is not copied or cloned.
     * A {@code null} expression is permitted by the current implementation and
     * represents an expression statement without an underlying expression.
     *
     * <p>A new empty collection is allocated for additional syntax tokens.
     *
     * @param expr              a pointer to the expression represented by this
     *                          statement, or {@code null} if no expression is
     *                          available
     */
    constructor(expr: pointer<Expression>)
    {
        this.expr = expr
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Returns the expression represented by this statement.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code Expression} object and is not copied or cloned.
     *
     * <p>The result may be {@code null} if this statement was constructed
     * without a valid expression.
     *
     * @return                  a pointer to the internally stored expression, or
     *                          {@code null} if no expression is available
     */
    fun getExpression() -> pointer<Expression> = this.expr


    /**
     * Adds an additional syntax token to this expression statement.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The supplied token is appended to the internal extra-token collection
     * and is stored by reference rather than cloned.
     *
     * <p>Tokens added through this method are included when
     * {@code getAllTokens()} constructs the complete token collection associated
     * with the statement.
     *
     * @param token             a pointer to the syntax token to add
     *
     * @return                  this {@code ExprStatement} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<ExprStatement>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Returns all tokens associated with this expression statement.
     *
     * <p>If an expression is available, {@code Expression.getAllTokens()} is
     * invoked first. If the returned token collection is not {@code null}, all
     * of its tokens are appended to the result.
     *
     * <p>If the wrapped expression is {@code null}, no expression tokens are
     * added.
     *
     * <p>The additional syntax tokens stored directly by this statement are then
     * appended to the result.
     *
     * <p>The complete token collection is sorted according to source position
     * using {@code TokenPosition.compareToken}. This restores the original
     * lexical ordering even though tokens are collected independently from the
     * wrapped expression and the statement itself.
     *
     * <p>A new list is allocated for the result. The individual token objects
     * are referenced rather than recursively cloned.
     *
     * @return                  a newly allocated list containing all tokens
     *                          associated with this expression statement in
     *                          source order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = this.extraTokens.clone()

        if this.expr != null:
        {
            val tokens: pointer<ArrayList> = this.expr.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns the textual representation of this expression statement.
     *
     * <p>If the wrapped expression is {@code null}, a newly allocated empty
     * {@code StringBuilder} is returned.
     *
     * <p>Otherwise, textual representation is delegated directly to
     * {@code Expression.toString()}, and the builder returned by the expression
     * is returned without adding any additional statement-level text.
     *
     * <p>This method therefore does not append statement terminators,
     * delimiters, or additional syntax tokens to the textual representation.
     *
     * @return                  the textual representation returned by the
     *                          wrapped expression, or a newly created empty
     *                          {@code StringBuilder} if no expression is present
     */
    fun toString() -> pointer<StringBuilder> = 
        if this.expr == null:
            new StringBuilder()
        else: this.expr.toString()
}
