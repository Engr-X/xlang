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
@file.class("TypeCast")
package xlang.compiler.parser

import xlang.compiler.Type
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct TypeCast
{
    private var expression: pointer<Expression>

    private var targetType: pointer<Type>

    private var extraTokens: pointer<ArrayList>


    fun __init__(expression: pointer<Expression>, targetType: pointer<Type>)
    {
        this.expression = expression
        this.targetType = targetType
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun addExtraToken(token: pointer<Token>) -> pointer<TypeCast>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun getExpression() -> pointer<Expression> = this.expression


    fun getTargetType() -> pointer<Type>
    {
        if this.targetType == null:
            return null

        return this.targetType.copy()
    }


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.expression != null:
        {
            val tokens: pointer<ArrayList> = this.expression.getAllTokens()

            if tokens != null:
                result.addAll(result.length, tokens)
        }

        if this.targetType != null:
        {
            val typeTokens: pointer<ArrayList> = this.targetType.getAllTokens()

            if typeTokens != null:
                result.addAll(result.length, typeTokens)
        }

        result.addAll(result.length, this.extraTokens)
        result.setCmparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        sb.append('(')
        sb.append(this.targetType.getTypeName())
        sb.append(")(")
        sb.append(this.expression.toString())
        sb.append(')')

        return sb
    }
}
