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
@file.class("SExpression")
package xlang.compiler.parser.expression

import xlang.lexer.Token
import xlang.lexer.TokenPosition
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


    static fun unwrapExtraTokens(sExpressions: pointer<ArrayList>) -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if sExpressions == null:
            return result

        for (var i = 0; i < sExpressions.length; i++):
        {
            val slot: pointer<pointer<ParseContainer>> = sExpressions.get(i) as pointer<pointer<ParseContainer>>

            if slot == null || slot.deref == null:
                continue

            val container: pointer<ParseContainer> = slot.deref
            val sExpression: pointer<SExpression> = container.getValue() as pointer<SExpression>

            if sExpression != null:
                result.addAll(result.length, sExpression.extraTokens)
        }

        return result
    }


    private var expression: pointer<Expression>

    private var extraTokens: pointer<ArrayList>


    fun __init__(expression: pointer<Expression>)
    {
        this.expression = expression
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun addExtraToken(token: pointer<Token>) -> pointer<SExpression>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun unwrap() -> pointer<Expression> = this.expression


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.expression != null:
        {
            val tokens: pointer<ArrayList> = this.expression.getAllTokens()

            if tokens != null:
                result.addAll(result.length, tokens)
        }

        result.addAll(result.length, this.extraTokens)
        result.setCmparator(TokenPosition.compareToken)
        result.sort()
        return result
    }
}