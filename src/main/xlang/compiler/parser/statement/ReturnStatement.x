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
 * Represents a {@code return} statement in the statement abstract syntax tree.
 *
 * <p>A {@code ReturnStatement} may either return without a value or contain an
 * expression whose evaluated value is returned from the current function.
 *
 * <p>The optional return expression is stored by reference and is not copied or
 * cloned by this structure.
 *
 * <p>Additional syntax tokens are retained separately and may contain the
 * {@code return} keyword or other lexical elements that are not directly owned
 * by the return expression.
 *
 * <p>The presence of a return value can be queried through
 * {@code haveReturnValue()}, while all source tokens associated with the
 * statement can be collected in lexical order through {@code getAllTokens()}.
 */
struct ReturnStatement
{
    /**
     * The optional expression whose value is returned by this statement.
     *
     * <p>A {@code null} value represents a return statement without an explicit
     * return value.
     *
     * <p>The expression is stored by reference and is not copied or cloned.
     */
    private val expr: pointer<Expression>

    /**
     * Additional syntax tokens associated with this return statement.
     *
     * <p>This collection may contain the {@code return} keyword or other lexical
     * tokens that are not directly owned by the optional return expression.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates a return statement without a return value.
     *
     * <p>The internal expression pointer is initialized to {@code null},
     * representing a plain {@code return} statement.
     *
     * <p>A new empty collection is allocated for additional syntax tokens.
     */
    constructor()
    {
        this.expr = null
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Creates a return statement with the specified return expression.
     *
     * <p>The supplied expression is stored directly and is not copied or cloned.
     *
     * <p>If {@code expr} is {@code null}, the resulting statement behaves the
     * same as a return statement without an explicit return value.
     *
     * <p>A new empty collection is allocated for additional syntax tokens.
     *
     * @param expr              a pointer to the expression whose value should
     *                          be returned, or {@code null} if no return value
     *                          is specified
     */
    constructor(expr: pointer<Expression>)
    {
        this.expr = expr
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Returns whether this return statement contains an explicit return value.
     *
     * <p>The statement is considered to have a return value whenever its stored
     * expression pointer is not {@code null}.
     *
     * @return                  {@code true} if a return expression is present;
     *                          {@code false} otherwise
     */
    fun haveReturnValue() -> bool = this.expr != null


    /**
     * Returns the expression associated with this return statement.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code Expression} object and is not copied or cloned.
     *
     * <p>The result is {@code null} when this statement represents a plain
     * {@code return} without a return value.
     *
     * @return                  a pointer to the return expression, or
     *                          {@code null} if no return value is present
     */
    fun getExpression() -> pointer<Expression> = this.expr


    /**
     * Returns all tokens associated with this return statement.
     *
     * <p>If a return expression is present, its token collection is obtained
     * through {@code Expression.getAllTokens()}. If the returned collection is
     * not {@code null}, all of its tokens are appended to the result.
     *
     * <p>If no return expression is present, no expression tokens are added.
     *
     * <p>The additional syntax tokens stored directly by this statement are then
     * appended. These tokens may include the {@code return} keyword or other
     * lexical elements retained during parsing.
     *
     * <p>The complete token collection is sorted according to source position
     * using {@code TokenPosition.compareToken}. This restores the original
     * lexical ordering even though tokens are collected independently from the
     * return expression and the statement itself.
     *
     * <p>A new list is allocated for the result. The individual token objects
     * are referenced rather than recursively cloned.
     *
     * @return                  a newly allocated list containing all tokens
     *                          associated with this return statement in source
     *                          order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.expr != null:
        {
            val tokens: pointer<ArrayList> = this.expr.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Adds an additional syntax token to this return statement.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The supplied token is appended to the internal extra-token collection
     * and is stored by reference rather than cloned.
     *
     * <p>Tokens added through this method are included when
     * {@code getAllTokens()} constructs the complete token collection associated
     * with this statement.
     *
     * @param token             a pointer to the syntax token to add
     *
     * @return                  this {@code ReturnStatement} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<ReturnStatement>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Returns the textual representation of this return statement.
     *
     * <p>The generated representation always begins with the
     * {@code "return"} keyword.
     *
     * <p>If a return expression is present, a single space is appended after the
     * keyword followed by the textual representation returned by
     * {@code Expression.toString()}.
     *
     * <p>A return statement without a value is therefore represented as:
     *
     * <pre>return</pre>
     *
     * while a statement with a return value generally has the form:
     *
     * <pre>return expression</pre>
     *
     * <p>The returned {@code StringBuilder} is newly allocated and modifying its
     * contents does not modify the underlying return-statement AST node.
     *
     * @return                  a pointer to a newly created {@code StringBuilder}
     *                          containing the textual representation of this
     *                          return statement
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder("return")

        if this.expr != null:
        {
            sb.append(" ")
            sb.append(this.expr.toString())
        }

        return sb
    }
}
