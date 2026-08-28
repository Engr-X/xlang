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
@file.class("Expressions")
package xlang.compiler.parser.expression

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct Expressions
{
    private val expressions: pointer<ArrayList>

    private var extraTokens: pointer<ArrayList>


    fun __init__()
    {
        this.expressions = new ArrayList(sizeof(pointer<Expression>))
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun __init__(expression: pointer<Expression>)
    {
        this.expressions = new ArrayList(sizeof(pointer<Expression>))
        this.extraTokens = new ArrayList(sizeof(Token))

        if expression != null:
            this.expressions.push(expression.ref)
    }


    fun addExpression(expression: pointer<Expression>) -> pointer<Expressions>
    {
        if expression != null:
            this.expressions.push(expression.ref)

        return this
    }


    fun addExpressions(exprs: pointer<Expressions>) -> pointer<Expressions>
    {
        if exprs != null && exprs.expressions != null:
        {
            this.expressions.pushAll(exprs.expressions)
            this.extraTokens.pushAll(exprs.extraTokens)
        }

        return this
    }


    fun addExtraToken(token: pointer<Token>) -> pointer<Expressions>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun getExpressions() -> pointer<ArrayList> = this.expressions.clone()


    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        for (var i = 0; i < this.expressions.length; i++):
        {
            val slot: pointer<pointer<Expression>> = this.expressions.get(i) as pointer<pointer<Expression>>

            if slot == null || slot.deref == null:
                continue

            val expression: pointer<Expression> = slot.deref
            val tokens: pointer<ArrayList> = expression.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        for (var i = 0; i < this.expressions.length; i++):
        {
            val slot: pointer<pointer<Expression>> = this.expressions.get(i) as pointer<pointer<Expression>>

            if slot == null || slot.deref == null:
                continue

            val expression: pointer<Expression> = slot.deref
            sb.append(expression.toString())
            sb.append("\n")
        }

        return sb
    }
}
