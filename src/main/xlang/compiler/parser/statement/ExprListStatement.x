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


/**
 * Represents a statement containing an ordered list of expressions.
 *
 * <p>An {@code ExprListStatement} groups multiple expressions into a single
 * intermediate statement node. This representation can be useful during
 * parsing when several expressions are recognized together before being
 * expanded into independent expression statements.
 *
 * <p>The expression collection preserves insertion order. Expressions may be
 * added individually, appended from an {@code ArrayList}, or merged from
 * another {@code ExprListStatement}.
 *
 * <p>The {@code expand()} method converts each valid stored expression into a
 * separate {@code ExprStatement}, wraps it as a {@code Statement}, and returns
 * the resulting statement collection.
 *
 * <p>Additional syntax tokens are stored separately and participate in
 * {@code getAllTokens()}, but they are not transferred into the expression
 * statements produced by {@code expand()}.
 */
struct ExprListStatement
{
    /**
     * The ordered collection of expressions represented by this statement.
     *
     * <p>The collection is allocated when the statement is constructed and
     * remains associated with this instance for its lifetime.
     *
     * <p>Expressions are stored by reference and are not copied or cloned when
     * they are inserted into the list.
     */
    private val exprList: pointer<ArrayList>

    /**
     * Additional syntax tokens associated with this expression-list statement.
     *
     * <p>This collection may contain separators, delimiters, or other lexical
     * tokens that are not directly owned by the individual expressions.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates an expression-list statement containing an initial expression.
     *
     * <p>A new expression collection and a new extra-token collection are
     * allocated.
     *
     * <p>The supplied expression is then appended directly to the internal list.
     * The current implementation does not perform a null check before inserting
     * the initial expression.
     *
     * <p>The expression object is stored by reference and is not copied or
     * cloned.
     *
     * @param expr              a pointer to the initial expression
     */
    constructor(expr: pointer<Expression>)
    {
        this.exprList = new ArrayList(sizeof(Expression)) 
        this.extraTokens = new ArrayList(sizeof(Token))
        this.exprList.push(expr)
    }


    /**
     * Appends an expression to this expression-list statement.
     *
     * <p>If {@code expr} is {@code null}, no modification is performed.
     *
     * <p>A valid expression is appended to the end of the internal collection,
     * preserving the order in which expressions are added.
     *
     * <p>The expression is stored by reference and is not copied or cloned.
     *
     * @param expr              a pointer to the expression to append
     *
     * @return                  this {@code ExprListStatement} instance
     */
    fun addExpression(expr: pointer<Expression>) -> pointer<ExprListStatement>
    {
        if expr != null:
            this.exprList.push(expr)

        return this
    }


    /**
     * Appends all expressions from the specified collection.
     *
     * <p>If {@code exprs} is {@code null}, no modification is performed.
     *
     * <p>Otherwise, all entries in the supplied collection are appended to the
     * internal expression list using {@code ArrayList.pushAll()}. Their existing
     * order is preserved.
     *
     * <p>The supplied collection itself is not modified and the contained
     * expressions are not recursively copied or cloned.
     *
     * <p>This method does not individually filter null entries that may already
     * exist inside the supplied collection. Such entries are ignored later by
     * operations such as {@code expand()}, {@code getAllTokens()}, and
     * {@code toString()}.
     *
     * @param exprs             a pointer to the expression collection to append
     *
     * @return                  this {@code ExprListStatement} instance
     */
    fun addExpressions(exprs: pointer<ArrayList>) -> pointer<ExprListStatement>
    {
        if exprs != null:
            this.exprList.pushAll(exprs)

        return this
    }


    /**
     * Appends all expressions stored by another expression-list statement.
     *
     * <p>If {@code other} is {@code null}, no modification is performed.
     *
     * <p>Otherwise, all entries from {@code other.exprList} are appended to this
     * statement's expression collection in their existing order.
     *
     * <p>The other statement and its internal list are not modified. The
     * expression objects themselves are referenced rather than recursively
     * cloned.
     *
     * <p>Only expressions are merged by this method. Additional syntax tokens
     * stored by {@code other} are not copied into this statement.
     *
     * @param other             a pointer to the expression-list statement whose
     *                          expressions should be appended
     *
     * @return                  this {@code ExprListStatement} instance
     */
    fun addExpressions(other: pointer<ExprListStatement>) -> pointer<ExprListStatement>
    {
        if other != null:
            this.exprList.pushAll(other.exprList)

        return this
    }


    /**
     * Returns the expression collection stored by this statement.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Modifications performed through the returned list therefore affect the
     * same expression collection referenced internally by this
     * {@code ExprListStatement}.
     *
     * @return                  a pointer to the internally stored expression
     *                          collection
     */
    fun getExpressions() -> pointer<ArrayList> = this.exprList


    /**
     * Adds an additional syntax token to this expression-list statement.
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
     * @return                  this {@code ExprListStatement} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<ExprListStatement>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Expands this expression-list statement into individual expression
     * statements.
     *
     * <p>A new {@code ArrayList} capable of storing {@code Statement} objects is
     * allocated for the result.
     *
     * <p>The internal expression collection is traversed in its stored order.
     * Null expression entries are ignored.
     *
     * <p>For every valid expression, a new {@code ExprStatement} is created using
     * the expression as its underlying expression node. That expression
     * statement is then wrapped using {@code Statement.fromExprStatement()} and
     * appended to the result.
     *
     * <p>The original {@code Expression} objects are reused by reference. They
     * are not copied or cloned during expansion.
     *
     * <p>The additional syntax tokens stored by this
     * {@code ExprListStatement} are not transferred to the newly created
     * expression statements.
     *
     * <p>The resulting statement collection preserves the order of the valid
     * expressions in the original list.
     *
     * @return                  a newly allocated list containing one
     *                          {@code Statement} for each valid expression
     */
    fun expand() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Statement))

        for (var i = 0; i < this.exprList.length; i++):
        {
            val expression: pointer<Expression> = this.exprList.get(i) as pointer<Expression>

            if expression != null:
            {
                val exprStatement: pointer<ExprStatement> = new ExprStatement(expression)
                result.push(Statement.fromExprStatement(exprStatement))
            }
        }

        return result
    }


    /**
     * Returns all tokens associated with this expression-list statement.
     *
     * <p>The expression collection is traversed in its stored order. Null
     * expression entries are skipped.
     *
     * <p>For each valid expression, {@code Expression.getAllTokens()} is called.
     * If the returned token collection is not {@code null}, its contents are
     * appended to the result.
     *
     * <p>After tokens from all valid expressions have been collected, the
     * additional syntax tokens stored directly by this statement are appended.
     *
     * <p>The complete token collection is sorted according to source position
     * using {@code TokenPosition.compareToken}. This restores the original
     * lexical ordering even though tokens are collected independently from the
     * contained expressions and the statement itself.
     *
     * <p>A new list is allocated for the result. The individual token objects
     * are referenced rather than recursively cloned.
     *
     * @return                  a newly allocated list containing all tokens
     *                          associated with this expression-list statement in
     *                          source order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = this.extraTokens.clone()

        for (var i = 0; i < this.exprList.length; i++):
        {
            val expression: pointer<Expression> = this.exprList.get(i) as pointer<Expression>

            if expression == null:
                continue

            val tokens: pointer<ArrayList> = expression.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns the textual representation of this expression-list statement.
     *
     * <p>The internal expression collection is traversed in its stored order.
     * Null expression entries are skipped.
     *
     * <p>For every valid expression, the textual representation returned by
     * {@code Expression.toString()} is appended, followed by {@code ",\n"}.
     *
     * <p>The resulting representation therefore generally has the form:
     *
     * <pre>
     * expression1,
     * expression2,
     * expression3,
     * </pre>
     *
     * <p>The current implementation emits a comma and newline after every valid
     * expression, including the final expression.
     *
     * <p>If the collection contains no valid expressions, the returned
     * {@code StringBuilder} remains empty.
     *
     * <p>The returned builder is newly allocated and modifying its contents does
     * not modify the expressions stored by this statement.
     *
     * @return                  a pointer to a newly created {@code StringBuilder}
     *                          containing the textual representation of this
     *                          expression-list statement
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        for (var i = 0; i < this.exprList.length; i++):
        {
            val expression: pointer<Expression> = this.exprList.get(i) as pointer<Expression>

            if expression == null:
                continue

            sb.append(expression.toString())
            sb.append(",\n")
        }

        return sb
    }
}
