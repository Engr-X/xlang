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

package xlang.compiler.parser.stmtexpr

import xlang.compiler.parser.expression.Expression
import xlang.compiler.parser.statement.Statement
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


/**
 * Represents an if expression containing a condition and an ordered collection
 * of statements forming its body.
 *
 * <p>An {@code IfExpression} stores the condition expression, the statements
 * executed when that condition succeeds, and additional syntax tokens retained
 * from the original source.
 *
 * <p>The condition and statement objects are stored by reference and are not
 * copied or cloned.
 *
 * <p>This structure represents only the if portion of a conditional expression.
 * It can be converted into an {@code IfElseExpression} through
 * {@code toIfElseExpr()}, which preserves the existing condition, statements,
 * and additional syntax tokens.
 *
 * <p>All tokens associated with the condition, body statements, and expression
 * itself can be collected in lexical source order through
 * {@code getAllTokens()}.
 */
struct IfExpression
{
    /**
     * The condition expression controlling execution of the if body.
     *
     * <p>The expression is stored by reference and is not copied or cloned.
     *
     * <p>A {@code null} value indicates that no valid condition expression is
     * currently associated with this node.
     */
    private var condition: pointer<Expression>

    /**
     * The ordered collection of statements forming the body of this if
     * expression.
     *
     * <p>The collection may either be supplied directly to the constructor or
     * allocated internally when a single statement is provided.
     *
     * <p>The contained statements are stored by reference and are not
     * recursively copied or cloned.
     */
    private val statements: pointer<ArrayList>

    /**
     * Additional syntax tokens associated with this if expression.
     *
     * <p>This collection may contain the {@code if} keyword, condition
     * separators, punctuation, or other lexical elements that are not directly
     * owned by the condition or body statements.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates an if expression with the specified condition and statement
     * collection.
     *
     * <p>The supplied condition and statement collection are stored directly and
     * are not copied or cloned.
     *
     * <p>This constructor does not normalize a {@code null} statement collection
     * to an empty list. Because methods such as {@code getAllTokens()} access
     * {@code statements} directly, callers are expected to provide a valid
     * collection.
     *
     * <p>A new empty collection is allocated for additional syntax tokens.
     *
     * @param condition         a pointer to the condition expression, or
     *                          {@code null} if no condition is available
     * @param statements        a pointer to the ordered statement collection
     *                          forming the if body
     */
    constructor(condition: pointer<Expression>, statements: pointer<ArrayList>)
    {
        this.condition = condition
        this.statements = statements
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Creates an if expression containing an optional initial statement.
     *
     * <p>The supplied condition is stored directly and is not copied or cloned.
     *
     * <p>A new empty statement collection and a new empty extra-token collection
     * are allocated.
     *
     * <p>If {@code statement} is not {@code null}, it is appended to the newly
     * created statement collection. A {@code null} statement therefore produces
     * an if expression with an empty body.
     *
     * <p>The supplied statement is stored by reference and is not copied or
     * cloned.
     *
     * @param condition         a pointer to the condition expression, or
     *                          {@code null} if no condition is available
     * @param statement         a pointer to the initial body statement, or
     *                          {@code null} to create an empty body
     */
    constructor(condition: pointer<Expression>, statement: pointer<Statement>)
    {
        this.condition = condition
        this.statements = new ArrayList(sizeof(Statement))
        this.extraTokens = new ArrayList(sizeof(Token))

        if statement != null:
            this.statements.push(statement)
    }


    /**
     * Returns the condition expression associated with this if expression.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code Expression} object and is not copied or cloned.
     *
     * <p>The result may be {@code null} if no valid condition expression is
     * available.
     *
     * @return                  a pointer to the internally stored condition
     *                          expression, or {@code null} if no condition is
     *                          available
     */
    fun getCondition() -> pointer<Expression> = this.condition


    /**
     * Returns the statement collection forming the body of this if expression.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Changes made through the returned list therefore affect the same body
     * collection referenced internally by this {@code IfExpression}.
     *
     * @return                  a pointer to the internally stored body-statement
     *                          collection
     */
    fun getStatements() -> pointer<ArrayList> = this.statements


    /**
     * Adds an additional syntax token to this if expression.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The supplied token is appended to the internal extra-token collection
     * and is stored by reference rather than cloned.
     *
     * <p>Tokens added through this method are included when
     * {@code getAllTokens()} constructs the complete token collection associated
     * with the expression.
     *
     * @param token             a pointer to the syntax token to add
     *
     * @return                  this {@code IfExpression} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<IfExpression>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Appends all tokens from the specified collection to this if expression.
     *
     * <p>If {@code tokens} is {@code null}, no modification is performed.
     *
     * <p>Otherwise, all entries are appended to the internal extra-token
     * collection in their existing order using {@code ArrayList.pushAll()}.
     *
     * <p>The supplied collection itself is not modified, and the individual
     * token objects are not recursively cloned.
     *
     * @param tokens            a pointer to the syntax-token collection to
     *                          append
     *
     * @return                  this {@code IfExpression} instance
     */
    fun addExtraTokens(tokens: pointer<ArrayList>) -> pointer<IfExpression>
    {
        if tokens != null:
            this.extraTokens.pushAll(tokens)

        return this
    }


    /**
     * Returns the additional syntax-token collection associated with this if
     * expression.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Changes made through the returned list affect the same token collection
     * referenced internally by this {@code IfExpression}.
     *
     * @return                  a pointer to the internally stored additional
     *                          syntax-token collection
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens


    /**
     * Converts this if expression into an {@code IfElseExpression}.
     *
     * <p>A new empty {@code IfElseExpression} is created first.
     *
     * <p>The current condition is assigned through
     * {@code IfElseExpression.setCondition()}. The same
     * {@code Expression} object is therefore referenced by the resulting
     * conditional expression and is not copied or cloned.
     *
     * <p>The current statement collection is then obtained through
     * {@code getStatements()} and appended to the new object's if branch using
     * {@code IfElseExpression.addIfStatements()}.
     *
     * <p>The statement collection itself is not installed as the new object's
     * internal list. Instead, its entries are appended to the
     * {@code IfElseExpression}'s independently allocated if-statement
     * collection. The individual {@code Statement} objects remain shared by
     * reference.
     *
     * <p>The additional syntax tokens are processed in the same manner through
     * {@code addExtraTokens()}: the token entries are appended to the new
     * expression's token collection, while the individual {@code Token} objects
     * remain referenced rather than recursively cloned.
     *
     * <p>The resulting {@code IfElseExpression} initially contains no
     * else-branch statements.
     *
     * @return                  a newly created {@code IfElseExpression}
     *                          containing this condition, if-body statements,
     *                          and additional syntax tokens
     */
    fun toIfElseExpr() -> pointer<IfElseExpression> =
        new IfElseExpression()
            .setCondition(this.condition)
            .addIfStatements(this.getStatements())
            .addExtraTokens(this.getExtraTokens())


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.condition != null:
        {
            val tokens: pointer<ArrayList> = this.condition.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        for (var i = 0; i < this.statements.length; i++):
        {
            val statement: pointer<Statement> = this.statements.get(i) as pointer<Statement>

            if statement == null:
                continue

            val tokens: pointer<ArrayList> = statement.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }
}
