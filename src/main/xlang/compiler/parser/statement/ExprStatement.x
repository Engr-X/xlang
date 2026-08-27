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
@file.class("ExprStatement")
package xlang.compiler.parser.statement

import xlang.compiler.parser.expression.Expression
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct ExprStatement
{
    private val expr: pointer<Expression>

    private var extraTokens: pointer<ArrayList>


    fun __init__(expr: pointer<Expression>)
    {
        this.expr = expr
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun getExpression() -> pointer<Expression> = this.expr


    fun addExtraToken(token: pointer<Token>) -> pointer<ExprStatement>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


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


    fun toString() -> pointer<StringBuilder> = 
        if this.expr == null:
            new StringBuilder()
        else: this.expr.toString()
}
