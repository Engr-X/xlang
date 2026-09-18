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

package xlang.compiler

import xlang.compiler.parser.expression.Expression
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


/**
 * Represents a blob type whose storage size is described by an expression.
 *
 * A blob type is written in source code using the form
 * {@code blob[expression]}. The expression between the square brackets is
 * preserved as part of the syntax tree rather than being converted
 * immediately into an integer value.
 *
 * This allows expressions such as compile-time constants or other constant
 * expressions to remain available until semantic analysis resolves them.
 * Once resolved, the resulting storage size is stored separately in
 * {@link #memSize}.
 *
 * BlobType is intentionally separate from normal named types because the
 * bracket expression represents storage size rather than an ordinary generic
 * or type argument.
 *
 * The node also keeps track of syntax tokens that belong directly to the blob
 * declaration, such as delimiters. Tokens belonging to the size expression
 * itself are owned by the expression node.
 */
struct BlobType
{
    /**
     * Expression written between the square brackets of the blob type.
     *
     * For example, in {@code blob[SIZE * 4]}, this field represents the
     * {@code SIZE * 4} expression.
     *
     * The expression may remain unresolved until a later semantic-analysis
     * stage evaluates it as a compile-time constant.
     */
    private var blobSize: pointer<Expression>

    /**
     * Resolved storage size of this blob.
     *
     * The value is normally produced after the size expression has been
     * evaluated during semantic analysis. A value of zero indicates that no
     * resolved size is currently available.
     */
    private var memSize: int

    /**
     * Tokens owned directly by the blob type node.
     *
     * This collection contains syntax tokens that are not owned by the
     * {@link #blobSize} expression itself, such as the {@code blob} keyword
     * and square-bracket delimiters.
     *
     * The tokens are retained so that the original source structure can be
     * reconstructed or inspected later.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates a new blob type.
     *
     * The supplied expression is retained directly and is not evaluated by
     * this constructor. Semantic analysis may later resolve the expression and
     * provide the corresponding storage size through {@code memSize}.
     *
     * An empty token collection is created for syntax tokens owned directly by
     * this node.
     *
     * @param blobSize          expression that determines the blob storage size
     * @param memSize           resolved storage size, or zero if the size has not yet
     *                          been evaluated
     */
    constructor(blobSize: pointer<Expression>, memSize: int)
    {
        this.blobSize = blobSize
        this.memSize = memSize
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Returns the expression used to determine the blob size.
     *
     * The returned expression is the same expression node supplied when this
     * BlobType was constructed.
     *
     * @return                  the blob size expression
     */
    fun getBlobSize() -> pointer<Expression> = this.blobSize


    /**
     * Returns the resolved storage size of this blob.
     *
     * If semantic evaluation has not yet resolved the size expression, the
     * value is zero.
     *
     * @return                  the resolved storage size, or zero when resolution is pending
     */
    fun getMemSize() -> int = this.memSize


    /**
     * Adds a syntax token owned directly by this blob type.
     *
     * Tokens belonging to the size expression should normally remain owned by
     * the expression node and do not need to be added here.
     *
     * A null token is ignored. The method returns this object so calls may be
     * chained while constructing the syntax tree.
     *
     * @param token             token to associate with this blob type
     * @return                  this BlobType instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<BlobType>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Returns the collection of syntax tokens owned directly by this node.
     *
     * The returned collection does not automatically include tokens belonging
     * to the size expression. Use {@link #getAllTokens()} when a complete
     * source-ordered token list is required.
     *
     * @return                  the directly owned token collection
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens


    /**
     * Creates a shallow copy of this blob type node.
     *
     * The resulting BlobType receives a new container for its extra tokens,
     * but the individual token objects are shared with the original node.
     * The size expression is also shared rather than cloned.
     *
     * Consequently, modifications to the token collection itself do not affect
     * the original collection, while modifications to shared expression or
     * token objects may be visible from both nodes.
     *
     * @return                  a shallow copy of this BlobType
     */
    fun clone() -> pointer<BlobType>
    {
        val result: pointer<BlobType> = new BlobType(this.blobSize, this.memSize)
        result.extraTokens.pushAll(this.extraTokens)
        return result
    }


    /**
     * Collects all source tokens associated with this blob type.
     *
     * Tokens from the size expression are combined with tokens owned directly
     * by this BlobType. The resulting list is then sorted according to each
     * token's original source position.
     *
     * Sorting is necessary because expression tokens and directly owned syntax
     * tokens are stored separately even though they may be interleaved in the
     * original source text.
     *
     * The returned collection is newly allocated and may therefore be modified
     * without changing the internal token collection of this node.
     *
     * @return                  a new list containing all tokens in source order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.blobSize != null:
            result.pushAll(this.blobSize.getAllTokens())

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Builds a source-like string representation of this blob type.
     *
     * The generated representation uses the form
     * {@code blob[expression]}. If no size expression is available, the
     * resulting representation is {@code blob[]}.
     *
     * This method reconstructs the logical representation of the type rather
     * than reproducing the original token stream exactly. Formatting details
     * such as whitespace therefore depend on the size expression's own
     * {@code toString()} implementation.
     *
     * @return                  a builder containing the source-like representation
     */
    fun toString() -> pointer<StringBuilder>
    {
        val result: pointer<StringBuilder> = new StringBuilder("blob[")

        if this.blobSize != null:
            result.append(this.blobSize.toString())

        result.append(']')
        return result
    }
}
