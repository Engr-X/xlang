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
import xlang.util.string.String
import xlang.util.string.StringBuilder


/**
 * Represents a field-access expression.
 *
 * <p>A {@code FieldAccess} associates a host expression with the name of a
 * field accessed from that expression.
 *
 * <p>The field name is duplicated when the instance is created, while the host
 * expression is stored by reference.
 *
 * <p>Additional syntax tokens associated with the access expression may be
 * stored separately in {@code extraTokens}.
 */
struct FieldAccess
{
    /**
     * The expression from which the field is accessed.
     *
     * <p>This value may be {@code null} when the field access does not have an
     * explicit host expression.
     */
    private var host: pointer<Expression>

    /**
     * The duplicated null-terminated name of the accessed field.
     */
    private var fieldName: pointer<char>

    /**
     * Additional syntax tokens associated with this field-access expression.
     *
     * <p>This collection may contain tokens such as the member-access operator
     * or other syntax tokens that are not directly owned by the host
     * expression.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates a field-access expression.
     *
     * <p>The host expression is stored by reference and is not copied.
     *
     * <p>The supplied field name is duplicated using {@code String.strdup}, so
     * the stored name does not depend on the lifetime of the original character
     * string.
     *
     * @param host              a pointer to the expression from which the field is accessed
     * @param fieldName         a pointer to the null-terminated field name
     */
    constructor(host: pointer<Expression>, fieldName: pointer<char>)
    {
        this.host = host
        this.fieldName = String.strdup(fieldName)
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Adds an additional syntax token to this field-access expression.
     *
     * <p>If {@code token} is {@code null}, no token is added.
     *
     * <p>The token is stored by reference and is not copied.
     *
     * @param token             a pointer to the token to add
     *
     * @return                  this {@code FieldAccess} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<FieldAccess>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Returns all tokens associated with this field-access expression.
     *
     * <p>The tokens belonging to the host expression are collected first,
     * followed by the additional syntax tokens stored by this field access.
     *
     * <p>The combined token collection is sorted according to source position
     * using {@code TokenPosition.compareToken}.
     *
     * @return                  a newly allocated list containing all associated tokens in source
     *                          order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = this.extraTokens.clone()

        if this.host != null:
        {
            val tokens: pointer<ArrayList> = this.host.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns the textual representation of this field-access expression.
     *
     * <p>If a host expression is available, the generated representation has
     * the form:
     *
     * <pre>
     * host.fieldName
     * </pre>
     *
     * <p>If no host expression is present, only the field name is returned.
     *
     * @return                  a pointer to a newly created {@code StringBuilder} containing the
     *                          textual representation of this field access
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        if this.host != null:
        {
            sb.append(this.host.toString())
            sb.append('.')
        }

        sb.append(this.fieldName)
        return sb
    }
}
