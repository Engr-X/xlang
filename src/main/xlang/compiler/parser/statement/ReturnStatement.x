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
@file.class("ReturnStatement")
package xlang.compiler.parser.statement

import xlang.compiler.parser.expression.Expression
import xlang.util.ArrayList


struct ReturnStatement
{
    private val expr: pointer<Expression>

    private var extraTokens: pointer<ArrayList>


    fun __init__()
    {
        this.expr = null
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun __init__(expr: pointer<Expression>)
    {
        this.expr = expr
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun haveReturnValue() -> bool = this.expr != null


    fun getExpression() -> pointer<Expression> = this.expr


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.expr != null:
        {
            val tokens: pointer<ArrayList> = this.expr.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.pushAll(this.extraTokens)
        result.setCmparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    fun addExtraToken(token: pointer<Token>) -> pointer<ReturnStatement>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }
}
