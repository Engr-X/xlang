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

import xlang.parser.ParseContainer
import xlang.util.ArrayList


struct SExpression
{
    static fun unwrap(sExpressions: pointer<ArrayList>) -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(pointer<Expression>))

        if sExpressions == null:
            return result

        for (var i = 0; i < sExpressions.length; i++):
        {
            val slot: pointer<pointer<ParseContainer>> = sExpressions.get(i) as pointer<pointer<ParseContainer>>

            if slot == null || slot.deref == null:
                continue

            val container: pointer<ParseContainer> = slot.deref
            val sExpression: pointer<SExpression> = container.getValue() as pointer<SExpression>

            if sExpression == null:
                continue

            val expression: pointer<Expression> = sExpression.unwrap()

            if expression != null:
                result.push(expression.ref)
        }

        return result
    }


    private var expression: pointer<Expression>


    fun __init__(expression: pointer<Expression>):
        this.expression = expression


    fun unwrap() -> pointer<Expression> = this.expression
}