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
@file.class("ListLiteral")
package xlang.compiler.parser.expression

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList


struct ListLiteral
{
    private var list: pointer<ArrayList>


    private var extraTokens: pointer<ArrayList>


    fun __init__()
    {
        this.list = new ArrayList(sizeof(pointer<Expression>))
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun __init__(list: pointer<ArrayList>)
    {
        this.list = list
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun addExpression(expression: pointer<Expression>) -> pointer<ListLiteral>
    {
        this.list.push(expression.ref)
        return this
    }


    fun getList() -> pointer<ArrayList> = this.list


    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    fun addExtraToken(token: pointer<Token>) -> pointer<ListLiteral>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun addExtraTokens(tokens: pointer<ArrayList>) -> pointer<ListLiteral>
    {
        if tokens != null:
            this.extraTokens.pushAll(tokens)

        return this
    }


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        for (var i = 0; i < this.list.length; i++):
        {
            val slot: pointer<pointer<Expression>> = this.list.get(i) as pointer<pointer<Expression>>

            if slot == null || slot.deref == null:
                continue

            val expression: pointer<Expression> = slot.deref
            val tokens: pointer<ArrayList> = expression.getAllTokens()

            if tokens == null:
                continue

            result.pushAll(tokens)
        }

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }
}
