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
 * <p>A {@code Block} groups zero or more {@code Statement} nodes into a single
 * brace-delimited statement-expression structure.
 *
 * <p>The statements are retained in their stored order and are used when
 * collecting source tokens and when reconstructing the textual representation
 * of the block.
 *
 * <p>Additional syntax tokens are stored separately from the contained
 * statements. These tokens may include the opening and closing braces or other
 * lexical elements associated directly with the block.
 *
 * <p>The statement collection and additional token collection are exposed
 * directly through their corresponding getter methods. Modifications performed
 * through those returned collections therefore affect this {@code Block}
 * instance.
 *
 * <p>All tokens associated with the block and its nested statements can be
 * collected in lexical source order through {@code getAllTokens()}.
 */
struct Block
{
    /**
     * The ordered collection of statements contained in this block.
     *
     * <p>Each entry is expected to contain a {@code Statement}. Null statement
     * entries are tolerated by token collection and textual reconstruction and
     * are skipped when encountered.
     *
     * <p>The collection is either allocated internally by the default
     * constructor or stored directly from the collection supplied to the
     * statement-list constructor.
     */
    private var statements: pointer<ArrayList>

    /**
     * Additional syntax tokens associated directly with this block.
     *
     * <p>This collection may contain the opening brace, closing brace, or other
     * lexical elements that are not directly owned by the nested statements.
     *
     * <p>The collection is always allocated internally when the block is
     * constructed.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates an empty block.
     *
     * <p>A new empty {@code ArrayList} capable of storing {@code Statement}
     * entries is allocated for the block body.
     *
     * <p>A separate empty {@code ArrayList} is also allocated for additional
     * syntax tokens.
     */
    constructor()
    {
        this.statements = new ArrayList(sizeof(Statement))
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Creates a block using the specified statement collection.
     *
     * <p>The supplied collection is stored directly and is not copied or cloned.
     * Structural changes made through the original collection are therefore
     * visible through this block.
     *
     * <p>This constructor does not normalize a {@code null} statement collection
     * to an empty list. Methods such as {@code getAllTokens()} and
     * {@code toString()} access {@code statements} directly and therefore assume
     * that this pointer references a valid {@code ArrayList}.
     *
     * <p>A new empty collection is allocated independently for additional syntax
     * tokens.
     *
     * @param statements        a pointer to the ordered statement collection
     *                          that should form the contents of this block
     */
    constructor(statements: pointer<ArrayList>)
    {
        this.statements = statements
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Appends a statement to the end of this block.
     *
     * <p>If {@code statement} is {@code null}, no modification is performed.
     *
     * <p>A valid statement is appended to the internal statement collection in
     * insertion order.
     *
     * <p>The supplied statement is stored by reference and is not copied or
     * cloned.
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
     * <p>Modifications performed through the returned list affect the same
     * statement collection referenced internally by this {@code Block}.
     *
     * @return                  a pointer to the internally stored statement
     *                          collection
     */
    fun getStatements() -> pointer<ArrayList> = this.statements


    /**
     * Returns the additional syntax-token collection associated with this
     * block.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Modifications performed through the returned list affect the same token
     * collection referenced internally by this {@code Block}.
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
     * <p>Tokens added through this method participate in
     * {@code getAllTokens()} when the complete source-token collection for the
     * block is constructed.
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
     * <p>The result collection is initialized by cloning the internal
     * {@code extraTokens} list. The resulting list is therefore a separate
     * collection object, while the individual {@code Token} objects referenced
     * by that list are not recursively cloned.
     *
     * <p>The statement collection is then traversed in its stored order. Each
     * entry is interpreted as a {@code Statement}. Null statement entries are
     * ignored.
     *
     * <p>For every valid statement, {@code Statement.getAllTokens()} is invoked.
     * If the returned token collection is not {@code null}, all of its entries
     * are appended to the result.
     *
     * <p>After the block-level and nested-statement tokens have been collected,
     * {@code TokenPosition.compareToken} is installed as the comparator and the
     * complete collection is sorted according to source position.
     *
     * <p>This sorting step restores lexical source order regardless of the order
     * in which tokens were originally appended to the block or collected from
     * its nested statements.
     *
     * <p>This method assumes that {@code statements} references a valid
     * collection.
     *
     * @return                  a cloned and extended token collection containing
     *                          all tokens associated with this block in source
     *                          order
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
     * Returns the textual representation of this block.
     *
     * <p>The representation begins with an opening brace followed by a newline.
     *
     * <p>The statement collection is then traversed in its stored order. Null
     * entries are skipped.
     *
     * <p>Each valid statement contributes the textual representation returned by
     * {@code Statement.toString()}, followed by a newline.
     *
     * <p>After all valid statements have been emitted, a closing brace followed
     * by another newline is appended.
     *
     * <p>A block containing statements therefore generally has the form:
     *
     * <pre>
     * {
     * statement1
     * statement2
     * }
     * </pre>
     *
     * <p>An empty block is represented as:
     *
     * <pre>
     * {
     * }
     * </pre>
     *
     * <p>The current implementation does not insert indentation before nested
     * statements. Any indentation required for formatted source reconstruction
     * must therefore be handled separately.
     *
     * <p>The representation always ends with a newline because {@code "}\n"} is
     * appended unconditionally.
     *
     * <p>This method assumes that {@code statements} references a valid
     * collection.
     *
     * <p>The returned {@code StringBuilder} is newly allocated and modifying its
     * contents does not modify this block or its nested statements.
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
