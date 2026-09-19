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
 * Represents an index-access expression.
 *
 * <p>An {@code IndexAccess} stores the host expression being indexed together
 * with one or more index expressions.
 *
 * <p>The index expressions are stored in an ordered list. Additional syntax
 * tokens, such as brackets or separators, are retained separately in
 * {@code extraTokens}.
 *
 * <p>If the instance is created from a {@code ListLiteral}, the expression
 * list and extra tokens of that literal are reused as the initial index data.
 */
struct IndexAccess
{
    /**
     * The expression being indexed.
     */
    private var host: pointer<Expression>

    /**
     * The ordered collection of index expressions.
     *
     * <p>Each entry is stored as a pointer to an {@code Expression}.
     */
    private var indices: pointer<ArrayList>

    /**
     * Additional syntax tokens associated with this index-access expression.
     *
     * <p>This collection may contain tokens such as brackets, commas, or other
     * delimiters that are not directly owned by the host or index expressions.
     */
    private var extraTokens: pointer<ArrayList>

     /**
     * Creates an index-access expression.
     *
     * <p>The host expression is stored by reference and is not copied.
     *
     * <p>If {@code indices} is {@code null}, a new empty index list is created.
     * Otherwise, the expression list owned by the supplied {@code ListLiteral}
     * is reused and its extra tokens are copied into this instance.
     *
     * @param host a pointer to the expression being indexed
     * @param indices a pointer to the list literal containing the initial index
     *                expressions, or {@code null} if no indices are available
     */
    constructor(host: pointer<Expression>, indices: pointer<ListLiteral>)
    {
        this.host = host
        this.extraTokens = new ArrayList(sizeof(Token))

        if indices == null:
            this.indices = new ArrayList(sizeof(pointer<Expression>))
        else:
        {
            this.indices = indices.getList()
            this.extraTokens.pushAll(indices.getExtraTokens())
        }
    }


    /**
     * Appends an index expression to this index access.
     *
     * <p>The expression pointer is stored in the internal index list and the
     * expression itself is not copied.
     *
     * @param index             a pointer to the index expression to append
     *
     * @return                  this {@code IndexAccess} instance
     */
    fun addIndex(index: pointer<Expression>) -> pointer<IndexAccess>
    {
        this.indices.push(index.ref)
        return this
    }


    /**
     * Adds an additional syntax token to this index-access expression.
     *
     * <p>If {@code token} is {@code null}, no token is added.
     *
     * <p>The token is stored by reference and is not copied.
     *
     * @param token             a pointer to the token to add
     *
     * @return                  this {@code IndexAccess} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<IndexAccess>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Returns the host expression being indexed.
     *
     * @return                  a pointer to the host expression
     */
    fun getHost() -> pointer<Expression> = this.host


    /**
     * Returns the index expression at the specified position.
     *
     * <p>If the stored slot is {@code null}, this method returns {@code null}.
     *
     * @param index             the position of the index expression to retrieve
     *
     * @return                  a pointer to the index expression, or {@code null} if the slot
     *                          does not contain a valid expression pointer
     */
    fun getIndex(index: int) -> pointer<Expression>
    {
        val slot: pointer<pointer<Expression>> = this.indices.get(index) as pointer<pointer<Expression>>

        if slot == null:
            return null

        return slot.deref
    }


    /**
     * Returns the number of index-expression slots stored by this access.
     *
     * @return                  the number of stored indices
     */
    fun indicesCount() -> int = this.indices.length


    /**
     * Returns all tokens associated with this index-access expression.
     *
     * <p>The returned collection contains the tokens belonging to the host
     * expression, all valid index expressions, and the additional syntax tokens
     * stored by this instance.
     *
     * <p>The combined token list is sorted according to source position using
     * {@code TokenPosition.compareToken}.
     *
     * @return                  a newly allocated list containing all associated tokens in source
     *                          order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.host != null:
        {
            val tokens: pointer<ArrayList> = this.host.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        for (var i = 0; i < this.indices.length; i++):
        {
            val index: pointer<Expression> = this.getIndex(i)

            if index == null:
                continue

            val tokens: pointer<ArrayList> = index.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns a debug-oriented textual representation of this index-access
     * expression.
     *
     * <p>The generated representation has the form:
     *
     * <pre>
     * IndexAccess(host, [index1, index2, ...])
     * </pre>
     *
     * <p>Null index entries are skipped. If the host expression is
     * {@code null}, the host portion is left empty.
     *
     * @return                  a pointer to a newly created {@code StringBuilder} containing the
     *                          textual representation of this index access
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        sb.append("IndexAccess(")

        if this.host != null:
            sb.append(this.host.toString())

        sb.append(", [")

        var appendedIndex: bool = false

        for (var i = 0; i < this.indices.length; i++):
        {
            val index: pointer<Expression> = this.getIndex(i)

            if index == null:
                continue

            if appendedIndex:
                sb.append(", ")

            sb.append(index.toString())
            appendedIndex = true
        }

        sb.append("])")
        return sb
    }
}
