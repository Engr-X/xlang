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

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


/**
 * Represents an {@code else} branch in the statement abstract syntax tree.
 *
 * <p>An {@code ElseStatement} stores an ordered collection of statements that
 * form the body of the else branch together with additional syntax tokens
 * retained from the original source.
 *
 * <p>The statement collection preserves source order and may be initialized as
 * an empty list, supplied directly by the caller, or created from a single
 * statement.
 *
 * <p>Additional syntax tokens may contain the {@code else} keyword, the branch
 * separator, or other lexical elements that are not directly owned by the
 * nested statements.
 *
 * <p>All tokens belonging to the else branch and its nested statements can be
 * collected in lexical source order through {@code getAllTokens()}.
 */
struct ElseStatement
{
    /**
     * The ordered collection of statements contained in this else branch.
     *
     * <p>Each entry is expected to contain a {@code Statement}. The collection
     * is traversed in order when tokens or textual representations are
     * generated.
     */
    private var statements: pointer<ArrayList>

    /**
     * Additional syntax tokens associated with this else branch.
     *
     * <p>This collection may contain the {@code else} keyword, the branch
     * separator, or other syntax tokens that are not represented directly by
     * the nested statements.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates an empty else branch.
     *
     * <p>A new empty statement collection and a new empty extra-token collection
     * are allocated.
     *
     * <p>Statements may later be added by modifying the list returned by
     * {@code getStatements()}.
     */
    constructor()
    {
        this.statements = new ArrayList(sizeof(Statement))
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Creates an else branch backed by the specified statement collection.
     *
     * <p>The supplied collection is stored directly and is not copied or cloned.
     * Changes made to that collection are therefore reflected by this
     * {@code ElseStatement}.
     *
     * <p>This constructor does not normalize a {@code null} argument to an empty
     * list. The current implementations of {@code getAllTokens()} and
     * {@code toString()} access {@code statements} directly, so callers are
     * expected to provide a valid collection.
     *
     * <p>A new empty extra-token collection is allocated independently from the
     * supplied statement list.
     *
     * @param statements        a pointer to the statement collection used as the
     *                          body of this else branch
     */
    constructor(statements: pointer<ArrayList>)
    {
        this.statements = statements
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Creates an else branch containing an optional initial statement.
     *
     * <p>A new empty statement collection and a new empty extra-token collection
     * are allocated.
     *
     * <p>If {@code statement} is not {@code null}, it is appended to the newly
     * created statement collection. A {@code null} statement produces an empty
     * else branch.
     *
     * <p>The statement object is stored by reference and is not copied or
     * cloned.
     *
     * @param statement        a pointer to the initial statement, or
     *                          {@code null} to create an empty branch
     */
    constructor(statement: pointer<Statement>)
    {
        this.statements = new ArrayList(sizeof(Statement))
        this.extraTokens = new ArrayList(sizeof(Token))

        if statement != null:
            this.statements.push(statement)
    }


    /**
     * Adds an additional syntax token to this else branch.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The supplied token is appended to the internal extra-token collection
     * and is stored by reference rather than cloned.
     *
     * <p>Tokens added through this method are included when
     * {@code getAllTokens()} constructs the complete token collection associated
     * with the else branch.
     *
     * @param token             a pointer to the syntax token to add
     *
     * @return                  this {@code ElseStatement} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<ElseStatement>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Returns the statement collection contained in this else branch.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Modifications performed through the returned list affect the same
     * statement collection referenced internally by this
     * {@code ElseStatement}.
     *
     * @return                  a pointer to the internally stored statement
     *                          collection
     */
    fun getStatements() -> pointer<ArrayList> = this.statements


    /**
     * Returns the additional syntax-token collection associated with this else
     * branch.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Changes made through the returned list affect the same token collection
     * referenced internally by this {@code ElseStatement}.
     *
     * @return                  a pointer to the internally stored additional
     *                          syntax-token collection
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens


    /**
     * Returns all tokens associated with this else branch and its nested
     * statements.
     *
     * <p>The statement collection is traversed in its stored order. Each entry is
     * interpreted as a {@code Statement}. Null statement entries are skipped.
     *
     * <p>For every valid statement, {@code Statement.getAllTokens()} is invoked.
     * If the returned token collection is not {@code null}, its contents are
     * appended to the result.
     *
     * <p>After all nested statement tokens have been collected, the additional
     * syntax tokens stored directly by this else branch are appended.
     *
     * <p>The final collection is sorted according to source position using
     * {@code TokenPosition.compareToken}. This restores the original lexical
     * ordering even though tokens are collected independently from nested
     * statements and the else branch itself.
     *
     * <p>A new list is allocated for the result. The individual token objects
     * are referenced rather than recursively cloned.
     *
     * <p>This method assumes that {@code statements} references a valid
     * collection.
     *
     * @return                  a newly allocated list containing all tokens
     *                          associated with this else branch in source order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = this.extraTokens.clone()

        for (var i: int = 0; i < this.statements.length; i++):
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
     * Returns the textual representation of this else branch.
     *
     * <p>The generated representation begins with {@code "else:\n"}.
     *
     * <p>The nested statement collection is then traversed in order. Null
     * statements are skipped. Each valid statement contributes the textual
     * representation returned by {@code Statement.toString()}, followed by a
     * newline.
     *
     * <p>The resulting representation therefore generally has the form:
     *
     * <pre>
     * else:
     * statement1
     * statement2
     * </pre>
     *
     * <p>The current implementation does not add indentation before nested
     * statements. Any indentation required by source reconstruction must
     * therefore be handled elsewhere.
     *
     * <p>The returned representation ends with a newline whenever at least the
     * initial {@code else} header has been emitted.
     *
     * <p>This method assumes that {@code statements} references a valid
     * collection.
     *
     * <p>The returned {@code StringBuilder} is newly allocated and modifying its
     * contents does not modify the underlying else-branch AST node.
     *
     * @return                  a pointer to a newly created {@code StringBuilder}
     *                          containing the textual representation of this
     *                          else branch
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        sb.append("else:\n")

        for (var i: int = 0; i < this.statements.length; i++):
        {
            val statement: pointer<Statement> = this.statements.get(i) as pointer<Statement>

            if statement == null:
                continue

            sb.append(statement.toString())
            sb.append("\n")
        }

        return sb
    }
}
