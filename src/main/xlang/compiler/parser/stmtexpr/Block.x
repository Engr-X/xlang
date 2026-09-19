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

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


/**
 * Represents a block expression containing an ordered sequence of statements.
 *
 * <p>A {@code Block} groups multiple {@code Statement} nodes into a single
 * brace-delimited AST structure.
 *
 * <p>The statement collection preserves insertion order and is used when
 * generating both the complete token sequence and the textual representation
 * of the block.
 *
 * <p>Additional syntax tokens are stored separately and may contain the opening
 * and closing braces or other lexical elements associated with the block that
 * are not directly owned by the nested statements.
 *
 * <p>All tokens associated with the block and its nested statements can be
 * collected in lexical source order through {@code getAllTokens()}.
 */
struct Block
{
    /**
     * The ordered collection of statements contained in this block.
     *
     * <p>The collection may be allocated internally by the default constructor
     * or supplied directly through the statement-list constructor.
     *
     * <p>Statements stored in this collection are referenced directly and are
     * not recursively copied or cloned.
     */
    private var statements: pointer<ArrayList>

    /**
     * Additional syntax tokens associated directly with this block.
     *
     * <p>This collection may contain the opening and closing braces or other
     * lexical tokens that are not directly represented by the nested
     * statements.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates an empty block.
     *
     * <p>A new empty statement collection and a new empty extra-token collection
     * are allocated.
     *
     * <p>Statements may later be appended through {@code addStatement()} or by
     * modifying the list returned by {@code getStatements()}.
     */
    constructor()
    {
        this.statements = new ArrayList(sizeof(Statement))
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Creates a block backed by the specified statement collection.
     *
     * <p>The supplied collection is stored directly and is not copied or
     * cloned. Structural modifications made through the original list are
     * therefore reflected by this block.
     *
     * <p>This constructor does not normalize a {@code null} collection to an
     * empty list. Because {@code getAllTokens()} and {@code toString()} access
     * {@code statements} directly, callers are expected to provide a valid
     * {@code ArrayList}.
     *
     * <p>A new empty collection is allocated for additional syntax tokens.
     *
     * @param statements        a pointer to the ordered statement collection
     *                          contained by this block
     */
    constructor(statements: pointer<ArrayList>)
    {
        this.statements = statements
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Appends a statement to this block.
     *
     * <p>If {@code statement} is {@code null}, no modification is performed.
     *
     * <p>A valid statement is appended to the end of the internal statement
     * collection, preserving insertion order.
     *
     * <p>The statement is stored by reference and is not copied or cloned.
     *
     * @param statement         a pointer to the statement to append
     *
     * @return                  this {@code Block} instance
     */
    fun addStatement(statement: pointer<Statement>) -> pointer<Block>
    {
        if statement != null:
            this.statements.push(statement)

        return this
    }


    /**
     * Returns the statement collection contained in this block.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Changes made through the returned list therefore affect the same
     * statement collection referenced internally by this {@code Block}.
     *
     * @return                  a pointer to the internally stored statement
     *                          collection
     */
    fun getStatements() -> pointer<ArrayList> = this.statements


    /**
     * Returns the additional syntax-token collection associated with this block.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Changes made through the returned list affect the same token collection
     * referenced internally by this {@code Block}.
     *
     * @return                  a pointer to the internally stored additional
     *                          syntax-token collection
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens


    /**
     * Adds an additional syntax token to this block.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The supplied token is appended to the internal extra-token collection
     * and is stored by reference rather than cloned.
     *
     * <p>Tokens added through this method are included when
     * {@code getAllTokens()} constructs the complete token collection associated
     * with the block.
     *
     * @param token             a pointer to the syntax token to add
     *
     * @return                  this {@code Block} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<Block>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Returns all tokens associated with this block and its nested statements.
     *
     * <p>The internal statement collection is traversed in its stored order.
     * Each entry is interpreted as a {@code Statement}. Null entries are
     * skipped.
     *
     * <p>For every valid statement, {@code Statement.getAllTokens()} is invoked.
     * If the returned token collection is not {@code null}, its contents are
     * appended to the result.
     *
     * <p>After tokens from all nested statements have been collected, the
     * additional syntax tokens stored directly by this block are appended.
     *
     * <p>The complete token collection is sorted according to source position
     * using {@code TokenPosition.compareToken}. This restores the original
     * lexical ordering even though tokens are collected independently from the
     * nested statements and the block itself.
     *
     * <p>A new list is allocated for the result. The individual token objects
     * are referenced rather than recursively cloned.
     *
     * <p>This method assumes that {@code statements} references a valid
     * collection.
     *
     * @return                  a newly allocated list containing all tokens
     *                          associated with this block in source order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        for (var i: int = 0; i < this.statements.length; i++):
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


    /**
     * Returns the textual representation of this block.
     *
     * <p>The representation begins with an opening brace followed by a newline.
     *
     * <p>The internal statement collection is then traversed in its stored
     * order. Null statements are skipped.
     *
     * <p>Each valid statement contributes the textual representation returned by
     * {@code Statement.toString()}, followed by a newline.
     *
     * <p>After all valid statements have been emitted, the closing brace and a
     * final newline are appended.
     *
     * <p>The resulting representation generally has the form:
     *
     * <pre>
     * {
     * statement1
     * statement2
     * }
     * </pre>
     *
     * <p>If the block contains no valid statements, the current implementation
     * still emits an empty brace-delimited block:
     *
     * <pre>
     * {
     * }
     * </pre>
     *
     * <p>The current implementation does not add indentation before nested
     * statements. Any indentation required for source reconstruction must be
     * handled separately.
     *
     * <p>This method assumes that {@code statements} references a valid
     * collection.
     *
     * <p>The returned {@code StringBuilder} is newly allocated and modifying its
     * contents does not modify the underlying block AST node.
     *
     * @return                  a pointer to a newly created {@code StringBuilder}
     *                          containing the textual representation of this
     *                          block
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        sb.append("{\n")

        for (var i: int = 0; i < this.statements.length; i++):
        {
            val statement: pointer<Statement> = this.statements.get(i) as pointer<Statement>

            if statement == null:
                continue

            sb.append(statement.toString())
            sb.append("\n")
        }

        sb.append("}\n")
        return sb
    }
}
