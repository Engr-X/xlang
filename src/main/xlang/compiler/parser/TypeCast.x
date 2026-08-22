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
 *
 */
package xlang.compiler.parser

import xlang.compiler.Type
import xlang.util.string.StringBuilder


struct TypeCast
{
    private var expression: pointer<Expression>

    private var targetType: pointer<Type>


    fun __init__(expression: pointer<Expression>, targetType: pointer<Type>)
    {
        this.expression = expression
        this.targetType = targetType
    }


    fun getExpression() -> pointer<Expression> = this.expression


    fun getTargetType() -> pointer<Type>
    {
        if this.targetType == null:
            return null

        return this.targetType.copy()
    }


    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        sb.append('(')

        if this.targetType != null:
            sb.append(this.targetType.getTypeName())

        sb.append(")(")

        if this.expression != null:
            sb.append(this.expression.toString())

        sb.append(')')

        return sb
    }
}