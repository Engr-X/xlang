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
 * Represents a generic statement-expression node using a tagged host
 * representation.
 *
 * <p>A {@code StatementExpression} combines an integer kind discriminator with
 * an untyped {@code host} pointer. The {@code kind} value identifies the
 * concrete statement-expression structure expected to be referenced by
 * {@code host}.
 *
 * <p>The currently defined kinds represent block expressions and conditional
 * expressions.
 *
 * <p>The host object is stored by reference and is not copied or cloned.
 * Correct interpretation therefore depends on the stored kind matching the
 * actual concrete type referenced by {@code host}.
 *
 * <p>Additional syntax tokens may be stored directly by this wrapper. In the
 * current implementation, {@code getAllTokens()} returns only these
 * wrapper-level tokens and does not dispatch to the host object.
 *
 * <p>The current {@code toString()} implementation also does not dispatch
 * according to the statement-expression kind and instead returns an empty
 * textual representation.
 */
struct StatementExpression
{
    // Identifies a statement expression whose host represents a {@code Block}.
    static val BLOCK_KIND: int = 1

    /**
     * Identifies a statement expression whose host represents an if-style
     * conditional expression.
     */
    static val IF_KIND: int = 2

    /**
     * The discriminator identifying the concrete statement-expression type
     * referenced by {@code host}.
     *
     * <p>The value is expected to correspond to one of the kind constants
     * defined by this structure.
     */
    private var kind: int

    /**
     * The concrete AST object represented by this statement-expression wrapper.
     *
     * <p>The pointer is intentionally untyped. Its expected concrete type is
     * determined by {@code kind}.
     *
     * <p>The host object is stored by reference and is not copied or cloned.
     *
     * <p>The value may be {@code null} because the constructor performs no
     * validation on the supplied host pointer.
     */
    private var host: pointer<*>

    /**
     * Additional syntax tokens associated directly with this
     * statement-expression wrapper.
     *
     * <p>These tokens are stored independently from any tokens that may belong
     * to the concrete host object.
     *
     * <p>In the current implementation, these are the only tokens returned by
     * {@code getAllTokens()}.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates a statement-expression wrapper with the specified kind and host
     * object.
     *
     * <p>The supplied kind is stored directly and is not validated against the
     * predefined statement-expression kind constants.
     *
     * <p>The host pointer is also stored directly and is not copied or cloned.
     * Correct use therefore requires the caller to ensure that the host type is
     * compatible with the supplied kind.
     *
     * <p>A new empty collection is allocated for additional syntax tokens.
     *
     * @param kind              the statement-expression kind discriminator
     * @param host              an untyped pointer to the concrete host object,
     *                          or {@code null}
     */
    constructor(kind: int, host: pointer<*>)
    {
        this.kind = kind
        this.host = host
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Returns the statement-expression kind discriminator stored by this
     * wrapper.
     *
     * <p>The returned value identifies the concrete type that is expected to be
     * referenced by {@code host}.
     *
     * @return                  the integer statement-expression kind
     *                          discriminator
     */
    fun getKind() -> int = this.kind


    /**
     * Returns the untyped host object represented by this statement expression.
     *
     * <p>The returned pointer refers directly to the internally stored host and
     * is not copied or cloned.
     *
     * <p>The caller is responsible for interpreting the pointer according to the
     * value returned by {@code getKind()}.
     *
     * <p>The result may be {@code null} if this wrapper was constructed without
     * a valid host object.
     *
     * @return                  the internally stored untyped host pointer, or
     *                          {@code null} if no host object is available
     */
    fun getHost() -> pointer<*> = this.host


    /**
     * Adds an additional syntax token to this statement-expression wrapper.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The supplied token is appended to the internal extra-token collection
     * and is stored by reference rather than cloned.
     *
     * <p>Tokens added through this method are included by
     * {@code getAllTokens()}.
     *
     * @param token             a pointer to the syntax token to add
     *
     * @return                  this {@code StatementExpression} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<StatementExpression>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Appends all tokens from the specified collection to this
     * statement-expression wrapper.
     *
     * <p>If {@code tokens} is {@code null}, no modification is performed.
     *
     * <p>Otherwise, all entries are appended to the internal extra-token
     * collection in their existing order using {@code ArrayList.pushAll()}.
     *
     * <p>The supplied collection itself is not modified and the individual
     * token objects are not recursively cloned.
     *
     * @param tokens            a pointer to the syntax-token collection to
     *                          append
     *
     * @return                  this {@code StatementExpression} instance
     */
    fun addExtraTokens(tokens: pointer<ArrayList>) -> pointer<StatementExpression>
    {
        if tokens != null:
            this.extraTokens.pushAll(tokens)

        return this
    }


    /**
     * Returns all tokens currently stored directly by this
     * statement-expression wrapper.
     *
     * <p>A new token collection is allocated and all entries from
     * {@code extraTokens} are appended to it.
     *
     * <p>The current implementation does not inspect {@code kind} and does not
     * collect tokens from the concrete object referenced by {@code host}.
     * Consequently, tokens belonging to a {@code Block}, conditional
     * expression, or another host object are not included unless they were also
     * added explicitly to {@code extraTokens}.
     *
     * <p>The resulting collection is sorted according to source position using
     * {@code TokenPosition.compareToken}.
     *
     * <p>A new list is allocated for the result. The individual token objects
     * are referenced rather than recursively cloned.
     *
     * @return                  a newly allocated list containing the additional
     *                          syntax tokens stored by this wrapper in source
     *                          order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns the textual representation of this statement expression.
     *
     * <p>The current implementation does not inspect {@code kind} or delegate
     * text generation to the concrete object referenced by {@code host}.
     *
     * <p>A newly allocated empty {@code StringBuilder} is therefore returned
     * regardless of the stored statement-expression kind or host object.
     *
     * @return                  a pointer to a newly created empty
     *                          {@code StringBuilder}
     */
    fun toString() -> pointer<StringBuilder> = new StringBuilder()
}
