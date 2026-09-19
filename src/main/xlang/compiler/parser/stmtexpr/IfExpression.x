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
 * Represents an if construct used as a statement-expression AST node.
 *
 * <p>An {@code IfExpression} stores a condition expression, an ordered
 * collection of statements forming the if body, and additional syntax tokens
 * retained from the original source.
 *
 * <p>The condition and contained statements are stored by reference and are not
 * copied or cloned.
 *
 * <p>This structure represents only the if portion of a conditional expression.
 * It can be converted into a complete {@code IfElseExpression} using
 * {@code toIfElseExpr()}.
 *
 * <p>During conversion, a new {@code IfElseExpression} container is created,
 * while the existing condition, statement references, and token references are
 * reused.
 *
 * <p>All tokens associated with the condition, body statements, and this node
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
     * <p>The collection may either be supplied directly by the caller or
     * allocated internally when a single statement is provided to the
     * constructor.
     *
     * <p>The individual {@code Statement} objects stored in the collection are
     * referenced directly and are not recursively copied or cloned.
     */
    private val statements: pointer<ArrayList>

    /**
     * Additional syntax tokens associated directly with this if expression.
     *
     * <p>This collection may contain the {@code if} keyword, condition
     * separators, punctuation, parentheses, or other lexical elements not
     * directly represented by the condition or body statements.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates an if expression using the specified condition and statement
     * collection.
     *
     * <p>The supplied condition and statement collection are stored directly and
     * are not copied or cloned.
     *
     * <p>This constructor does not normalize a {@code null} statement collection
     * to an empty list. Because methods such as {@code getAllTokens()} access
     * {@code statements} directly, callers are expected to provide a valid
     * {@code ArrayList}.
     *
     * <p>A new empty collection is allocated independently for additional syntax
     * tokens.
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
     * created statement collection.
     *
     * <p>If {@code statement} is {@code null}, the resulting if expression has
     * an empty body.
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
     * <p>The result may be {@code null} if this node does not currently contain
     * a valid condition expression.
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
     * <p>Structural modifications performed through the returned list therefore
     * affect the same statement collection referenced internally by this
     * {@code IfExpression}.
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
     * <p>Tokens added through this method participate in
     * {@code getAllTokens()} when the complete token sequence for the
     * expression is constructed.
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
     * {@code Token} objects are not recursively cloned.
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
     * <p>The current condition is assigned to the new expression through
     * {@code IfElseExpression.setCondition()}. The same condition object is
     * therefore referenced by both structures and is not copied or cloned.
     *
     * <p>The current statement collection is obtained through
     * {@code getStatements()} and passed to
     * {@code IfElseExpression.addIfStatements()}. That method appends the
     * statement entries into the new expression's independently allocated
     * if-statement collection.
     *
     * <p>The statement collection itself is therefore not shared as the same
     * {@code ArrayList}, but the individual {@code Statement} objects remain
     * shared by reference.
     *
     * <p>The additional syntax tokens are transferred in the same manner through
     * {@code addExtraTokens()}. The destination owns a separate token-list
     * container, while the individual {@code Token} objects remain referenced
     * rather than recursively cloned.
     *
     * <p>The resulting expression initially contains no else-branch statements.
     *
     * @return                  a newly created {@code IfElseExpression}
     *                          containing this condition, body statements, and
     *                          additional syntax tokens
     */
    fun toIfElseExpr() -> pointer<IfElseExpression> =
        new IfElseExpression()
            .setCondition(this.condition)
            .addIfStatements(this.getStatements())
            .addExtraTokens(this.getExtraTokens())


    /**
     * Returns all tokens associated with this if expression and its child AST
     * nodes.
     *
     * <p>The result collection is initialized by cloning the internal
     * {@code extraTokens} list. The returned list is therefore a separate
     * collection object, while the individual {@code Token} objects referenced
     * by that list are not recursively cloned.
     *
     * <p>If a condition expression is present, its token collection is obtained
     * through {@code Expression.getAllTokens()}. If the returned collection is
     * not {@code null}, all of its entries are appended to the result.
     *
     * <p>The body-statement collection is then traversed in its stored order.
     * Null statement entries are ignored.
     *
     * <p>For every valid statement, {@code Statement.getAllTokens()} is invoked.
     * If the returned collection is not {@code null}, its token entries are
     * appended to the result.
     *
     * <p>After all block-level, condition, and child-statement tokens have been
     * collected, {@code TokenPosition.compareToken} is installed as the
     * comparator and the result is sorted according to source position.
     *
     * <p>This sorting step restores lexical source order regardless of the order
     * in which tokens were gathered from the individual AST components.
     *
     * <p>This method assumes that {@code statements} references a valid
     * collection.
     *
     * @return                  a cloned and extended token collection containing
     *                          all tokens associated with this if expression in
     *                          source order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = this.extraTokens.clone()

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

        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns the textual representation of this if expression.
     *
     * <p>If a condition expression is present, the representation begins with
     * {@code "(if "}, followed by the textual representation returned by
     * {@code Expression.toString()}.
     *
     * <p>The sequence {@code ":\n"} is appended after the optional condition
     * header.
     *
     * <p>The statement collection is then traversed in its stored order. Each entry
     * is interpreted as a {@code Statement}. Null entries are skipped.
     *
     * <p>Each valid statement contributes the textual representation returned by
     * {@code Statement.toString()}, followed by a newline.
     *
     * <p>After all valid statements have been emitted, the representation is closed
     * using {@code ")\n"}.
     *
     * <p>A normal if expression therefore generally has the form:
     *
     * <pre>
     * (if condition:
     * statement1
     * statement2
     * )
     * </pre>
     *
     * <p>If {@code condition} is {@code null}, the current implementation does not
     * emit the opening {@code "(if "} text, but still appends {@code ":\n"}, the
     * available statements, and the final closing parenthesis.
     *
     * <p>The current implementation does not insert indentation before nested
     * statements.
     *
     * <p>The returned {@code StringBuilder} is newly allocated and modifying its
     * contents does not modify this {@code IfExpression} or its child AST nodes.
     *
     * @return                  a pointer to a newly created {@code StringBuilder}
     *                          containing the textual representation of this if
     *                          expression
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        if this.condition != null:
        {
            sb.append("(if ")
            sb.append(this.condition.toString())
        }

        sb.append(":\n")

        for (var i: int = 0; i < this.statements.length; i++):
        {
            val statement: pointer<Statement> =
                this.statements.get(i) as pointer<Statement>

            if statement == null:
                continue

            sb.append(statement.toString())
            sb.append("\n")
        }

        sb.append(")\n")
        return sb
    }
}
