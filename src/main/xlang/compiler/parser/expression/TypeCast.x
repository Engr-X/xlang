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

import xlang.compiler.type.Type
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


/**
 * Represents an explicit type-cast expression.
 *
 * <p>A {@code TypeCast} associates an input expression with the target type to
 * which that expression should be converted.
 *
 * <p>Both the source expression and target type are stored by reference when
 * the node is created. The target type can later be retrieved through
 * {@code getTargetType()}, which returns a cloned type rather than exposing the
 * internally stored type object directly.
 *
 * <p>Additional syntax tokens associated with the cast are stored separately.
 * These tokens may include parentheses or other syntax elements that are not
 * directly owned by either the source expression or the target type.
 *
 * <p>All tokens belonging to the cast can be collected in source order using
 * {@code getAllTokens()}.
 */
struct TypeCast
{
    /**
     * The expression whose value is being converted to another type.
     *
     * <p>The expression is stored by reference and is not copied or cloned by
     * the constructor.
     */
    private var expression: pointer<Expression>

    /**
     * The target type of this explicit cast.
     *
     * <p>The type is stored by reference internally. External callers can obtain
     * an independent clone of this type through {@code getTargetType()}.
     */
    private var targetType: pointer<Type>

    /**
     * Additional syntax tokens associated with this type-cast expression.
     *
     * <p>This collection may contain parentheses or other syntax tokens that are
     * not directly owned by the source expression or target type.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates a type-cast expression using the specified source expression and
     * target type.
     *
     * <p>The supplied expression and target type are both stored by reference.
     * Neither object is copied or cloned during construction.
     *
     * <p>A new empty collection is allocated for additional syntax tokens
     * associated with the cast.
     *
     * @param expression        a pointer to the expression whose value is being cast
     * @param targetType        a pointer to the destination type of the cast
     */
    constructor(expression: pointer<Expression>, targetType: pointer<Type>)
    {
        this.expression = expression
        this.targetType = targetType
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Adds an additional syntax token to this type-cast expression.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The supplied token is appended to the internal extra-token collection
     * by reference and is not copied or cloned.
     *
     * <p>Tokens added through this method are included when
     * {@code getAllTokens()} constructs the complete token collection for the
     * cast expression.
     *
     * @param token             a pointer to the syntax token to add
     *
     * @return                  this {@code TypeCast} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<TypeCast>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Returns the source expression associated with this type cast.
     *
     * <p>The returned pointer refers directly to the expression stored
     * internally by this instance. The expression is not copied or cloned.
     *
     * <p>The result may be {@code null} if this type-cast node was constructed
     * without a valid source expression.
     *
     * @return                  a pointer to the source expression, or {@code null} if no
     *                          expression is associated with this cast
     */
    fun getExpression() -> pointer<Expression> = this.expression


    /**
     * Returns the source expression associated with this type cast.
     *
     * <p>The returned pointer refers directly to the expression stored
     * internally by this instance. The expression is not copied or cloned.
     *
     * <p>The result may be {@code null} if this type-cast node was constructed
     * without a valid source expression.
     *
     * @return                  a pointer to the source expression, or {@code null} if no
     *                          expression is associated with this cast
     */
    fun getTargetType() -> pointer<Type>
    {
        if this.targetType == null:
            return null

        return this.targetType.clone()
    }


    /**
     * Returns a copy of the target type associated with this cast.
     *
     * <p>If no target type is currently stored, this method returns
     * {@code null}.
     *
     * <p>When a target type is available, {@code Type.clone()} is used to
     * produce the returned value. The caller therefore receives a distinct type
     * object rather than a direct reference to the internally stored target
     * type.
     *
     * <p>Modifications made to the returned type do not directly replace the
     * target-type pointer stored by this {@code TypeCast}.
     *
     * @return                  a pointer to a cloned target type, or {@code null} if no target
     *                          type is associated with this cast
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = this.extraTokens.clone()

        if this.expression != null:
        {
            val tokens: pointer<ArrayList> = this.expression.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        if this.targetType != null:
        {
            val typeTokens: pointer<ArrayList> = this.targetType.getAllTokens()

            if typeTokens != null:
                result.pushAll(typeTokens)
        }

        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns all tokens associated with this type-cast expression.
     *
     * <p>If a source expression is available, all tokens belonging to that
     * expression are collected first. A {@code null} token collection returned
     * by the source expression is ignored.
     *
     * <p>If a target type is available, all tokens belonging to the type are
     * also collected. This allows the returned collection to represent both the
     * value being cast and the type syntax used by the cast.
     *
     * <p>The expression and type tokens are then combined with the additional
     * syntax tokens stored by this instance. These additional tokens may
     * include parentheses or other delimiters that belong to the complete cast
     * syntax.
     *
     * <p>The final token collection is sorted according to source position using
     * {@code TokenPosition.compareToken}, ensuring that tokens obtained from
     * different components of the AST node are restored to their original
     * lexical order.
     *
     * <p>A new list is allocated for the result. The contained token objects
     * themselves are referenced rather than cloned.
     *
     * @return                  a newly allocated list containing all tokens associated with this
     *                          type-cast expression in source order
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        sb.append('(')
        sb.append(this.targetType.toString())
        sb.append(")(")
        sb.append(this.expression.toString())
        sb.append(')')

        return sb
    }
}
