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
 * Represents a {@code while} loop statement in the statement abstract syntax
 * tree.
 *
 * <p>A {@code WhileStatement} stores the loop condition, the ordered collection
 * of statements forming the loop body, an optional else-body collection, and
 * additional syntax tokens retained from the original source.
 *
 * <p>The condition expression is stored by reference and may be {@code null}.
 * The body and else-body collections are also stored by reference when supplied
 * to the constructors.
 *
 * <p>An empty else-body collection represents a while loop without an
 * {@code else} branch. The presence of an else branch can be queried through
 * {@code haveElseStatement()}.
 *
 * <p>Additional syntax tokens may contain the {@code while} keyword, branch
 * delimiters, separators, or other lexical elements that are not directly owned
 * by the condition or nested statements.
 *
 * <p>All tokens belonging to the loop and its child AST nodes can be collected
 * in lexical source order through {@code getAllTokens()}.
 */
struct WhileStatement
{
    /**
     * The optional condition expression controlling execution of this loop.
     *
     * <p>The expression is stored by reference and is not copied or cloned.
     *
     * <p>A {@code null} value indicates that no valid condition expression is
     * currently associated with the loop.
     */
    private var condition: pointer<Expression>

    /**
     * The ordered collection of statements forming the main loop body.
     *
     * <p>The collection supplied to the constructor is stored directly and is
     * not copied or cloned.
     *
     * <p>The current implementation assumes this pointer references a valid
     * {@code ArrayList} when methods such as {@code getAllTokens()} and
     * {@code toString()} are invoked.
     */
    private val bodyStmts: pointer<ArrayList>

    /**
     * The ordered collection of statements forming the optional else branch.
     *
     * <p>An empty collection represents the absence of an effective else body.
     *
     * <p>When explicitly supplied to the constructor, the collection is stored
     * directly and is not copied or cloned.
     */
    private val elseStmts: pointer<ArrayList>

    /**
     * Additional syntax tokens associated with this while statement.
     *
     * <p>This collection may contain the {@code while} keyword, condition
     * delimiters, branch punctuation, or other lexical tokens not directly owned
     * by the child AST nodes.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates a while statement with the specified condition and body.
     *
     * <p>The supplied condition and body collection are stored directly and are
     * not copied or cloned.
     *
     * <p>A new empty else-body collection is allocated, so the resulting loop
     * initially contains no else-body statements.
     *
     * <p>A new empty collection is also allocated for additional syntax tokens.
     *
     * <p>This constructor does not normalize a {@code null} body collection to
     * an empty list. The current implementations of {@code getAllTokens()} and
     * {@code toString()} access {@code bodyStmts} directly, so callers are
     * expected to provide a valid body collection.
     *
     * @param condition         a pointer to the loop condition expression, or
     *                          {@code null} if no condition is available
     * @param bodyStmts         a pointer to the ordered loop-body statement
     *                          collection
     */
    constructor(condition: pointer<Expression>, bodyStmts: pointer<ArrayList>)
    {
        this.condition = condition
        this.bodyStmts = bodyStmts
        this.elseStmts = new ArrayList(sizeof(Statement))
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Creates a while statement with explicit body and else-body collections.
     *
     * <p>The supplied condition, body collection, and else-body collection are
     * stored directly and are not copied or cloned.
     *
     * <p>A new empty collection is allocated for additional syntax tokens.
     *
     * <p>This constructor does not normalize {@code null} body or else-body
     * collections. Because later methods access both collections directly,
     * callers are expected to provide valid {@code ArrayList} instances.
     *
     * @param condition         a pointer to the loop condition expression, or
     *                          {@code null} if no condition is available
     * @param bodyStmts         a pointer to the ordered loop-body statement
     *                          collection
     * @param elseStmts         a pointer to the ordered else-body statement
     *                          collection
     */
    constructor(condition: pointer<Expression>, bodyStmts: pointer<ArrayList>, elseStmts: pointer<ArrayList>)
    {
        this.condition = condition
        this.bodyStmts = bodyStmts
        this.elseStmts = elseStmts
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Returns the condition expression associated with this while statement.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code Expression} object and is not copied or cloned.
     *
     * <p>The result may be {@code null} if no valid condition expression is
     * stored.
     *
     * @return                  a pointer to the loop condition expression, or
     *                          {@code null} if no condition is available
     */
    fun getCondition() -> pointer<Expression> = this.condition


    /**
     * Returns the statement collection forming the main body of this loop.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Changes performed through the returned list therefore affect the same
     * body collection referenced internally by this {@code WhileStatement}.
     *
     * @return                  a pointer to the internally stored loop-body
     *                          statement collection
     */
    fun getBodyStatements() -> pointer<ArrayList> = this.bodyStmts


    /**
     * Returns the statement collection forming the optional else branch.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Changes performed through the returned list affect the same else-body
     * collection referenced internally by this {@code WhileStatement}.
     *
     * @return                  a pointer to the internally stored else-body
     *                          statement collection
     */
    fun getElseStatements() -> pointer<ArrayList> = this.elseStmts


    /**
     * Returns whether this while statement contains at least one else-body
     * statement.
     *
     * <p>The result is determined exclusively from the length of the internally
     * stored else-body collection.
     *
     * <p>This method assumes that {@code elseStmts} references a valid
     * collection.
     *
     * @return                  {@code true} if at least one else-body statement
     *                          is stored; {@code false} otherwise
     */
    fun haveElseStatement() -> bool = this.elseStmts.length > 0


    /**
     * Appends all tokens from the specified collection to this while statement.
     *
     * <p>If {@code tokens} is {@code null}, no modification is performed.
     *
     * <p>Otherwise, all entries are appended to the internal extra-token
     * collection in their existing order using {@code ArrayList.pushAll()}.
     *
     * <p>The supplied collection itself is not modified, and the contained token
     * objects are not recursively cloned.
     *
     * @param tokens            a pointer to the syntax-token collection to
     *                          append
     *
     * @return                  this {@code WhileStatement} instance
     */
    fun addExtraTokens(tokens: pointer<ArrayList>) -> pointer<WhileStatement>
    {
        if tokens != null:
            this.extraTokens.pushAll(tokens)

        return this
    }


    /**
     * Returns the additional syntax-token collection associated with this while
     * statement.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Changes made through the returned list affect the same token collection
     * referenced internally by this {@code WhileStatement}.
     *
     * @return                  a pointer to the internally stored additional
     *                          syntax-token collection
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens


    /**
     * Returns all tokens associated with this while statement and its child AST
     * nodes.
     *
     * <p>If a condition expression is present, its token collection is obtained
     * through {@code Expression.getAllTokens()}. If that collection is not
     * {@code null}, all of its tokens are appended to the result.
     *
     * <p>The main body collection is then traversed in its stored order. Each
     * entry is interpreted as a {@code Statement}. Null entries are skipped.
     *
     * <p>For every valid body statement, {@code Statement.getAllTokens()} is
     * invoked and the returned tokens are appended when that collection is not
     * {@code null}.
     *
     * <p>The else-body collection is processed in the same manner after the main
     * body.
     *
     * <p>Finally, the additional syntax tokens stored directly by this while
     * statement are appended to the result.
     *
     * <p>The complete token collection is sorted according to source position
     * using {@code TokenPosition.compareToken}. This restores the original
     * lexical ordering even though tokens are gathered independently from the
     * condition, body statements, else statements, and loop-level token
     * collection.
     *
     * <p>A new list is allocated for the result. The individual token objects
     * are referenced rather than recursively cloned.
     *
     * <p>This method assumes that both {@code bodyStmts} and
     * {@code elseStmts} reference valid collections.
     *
     * @return                  a newly allocated list containing all tokens
     *                          associated with this while statement in source
     *                          order
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

        for (var i: int = 0; i < this.bodyStmts.length; i++):
        {
            val statement: pointer<Statement> = this.bodyStmts.get(i) as pointer<Statement>

            if statement == null:
                continue

            val tokens: pointer<ArrayList> = statement.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        for (var i: int = 0; i < this.elseStmts.length; i++):
        {
            val statement: pointer<Statement> = this.elseStmts.get(i) as pointer<Statement>

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
     * Returns the textual representation of this while statement.
     *
     * <p>The representation begins with {@code "while "}. If a condition
     * expression is present, its textual representation is appended immediately
     * after the keyword.
     *
     * <p>The loop header is then terminated using {@code ":\n"}.
     *
     * <p>The main body collection is traversed in order. Null statements are
     * skipped. Each valid statement contributes the representation returned by
     * {@code Statement.toString()}, followed by a newline.
     *
     * <p>If {@code haveElseStatement()} returns {@code true}, an
     * {@code "else:\n"} section is appended. The else-body statements are then
     * emitted in their stored order using the same rules as the main body.
     *
     * <p>The resulting representation generally has the form:
     *
     * <pre>
     * while condition:
     * bodyStatement1
     * bodyStatement2
     * else:
     * elseStatement1
     * </pre>
     *
     * <p>If {@code condition} is {@code null}, the current implementation still
     * emits the surrounding while syntax and therefore begins with:
     *
     * <pre>
     * while :
     * </pre>
     *
     * <p>The current implementation does not add indentation before body or
     * else-body statements.
     *
     * <p>This method assumes that both {@code bodyStmts} and
     * {@code elseStmts} reference valid collections.
     *
     * <p>The returned {@code StringBuilder} is newly allocated and modifying its
     * contents does not modify the underlying while-statement AST node.
     *
     * @return                  a pointer to a newly created {@code StringBuilder}
     *                          containing the textual representation of this
     *                          while statement
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        sb.append("while ")

        if this.condition != null:
            sb.append(this.condition.toString())

        sb.append(":\n")

        for (var i: int = 0; i < this.bodyStmts.length; i++):
        {
            val statement: pointer<Statement> = this.bodyStmts.get(i) as pointer<Statement>

            if statement == null:
                continue

            sb.append(statement.toString())
            sb.append("\n")
        }

        if this.haveElseStatement():
        {
            sb.append("else:\n")

            for (var i: int = 0; i < this.elseStmts.length; i++):
            {
                val statement: pointer<Statement> = this.elseStmts.get(i) as pointer<Statement>

                if statement == null:
                    continue

                sb.append(statement.toString())
                sb.append("\n")
            }
        }

        return sb
    }
}
