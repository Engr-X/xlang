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
@file.class("Assignment")
package xlang.compiler.parser.expression

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct Assignment
{
    private var target: pointer<Expression>

    private var value: pointer<Expression>

    private var extraTokens: pointer<ArrayList>


    fun __init__(target: pointer<Expression>, value: pointer<Expression>)
    {
        this.target = target
        this.value = value
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun getTarget() -> pointer<Expression> = this.target


    fun getValue() -> pointer<Expression> = this.value


    fun addExtraToken(token: pointer<Token>) -> pointer<Assignment>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.target != null:
        {
            val tokens: pointer<ArrayList> = this.target.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        if this.value != null:
        {
            val tokens: pointer<ArrayList> = this.value.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.pushAll(this.extraTokens)
        result.setCmparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder("(")

        if this.target != null:
            sb.append(this.target.toString())

        sb.append(" = ")

        if this.value != null:
            sb.append(this.value.toString())

        sb.append(')')
        return sb
    }
}
