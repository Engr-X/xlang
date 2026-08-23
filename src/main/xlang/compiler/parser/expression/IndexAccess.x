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
@file.class("IndexAccess")
package xlang.compiler.parser.expression

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct IndexAccess
{
    private var host: pointer<Expression>

    private var indices: pointer<ArrayList>

    private var extraTokens: pointer<ArrayList>


    fun __init__(host: pointer<Expression>, indices: pointer<ListLiteral>)
    {
        this.host = host
        this.extraTokens = new ArrayList(sizeof(Token))

        if indices == null:
            this.indices = new ArrayList(sizeof(pointer<Expression>))
        else:
        {
            this.indices = indices.getList()
            this.extraTokens.addAll(this.extraTokens.length, indices.getExtraTokens())
        }
    }


    fun addIndex(index: pointer<Expression>) -> pointer<IndexAccess>
    {
        this.indices.push(index.ref)
        return this
    }


    fun addExtraToken(token: pointer<Token>) -> pointer<IndexAccess>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun getHost() -> pointer<Expression> = this.host


    fun getIndex(index: int) -> pointer<Expression>
    {
        val slot: pointer<pointer<Expression>> = this.indices.get(index) as pointer<pointer<Expression>>

        if slot == null:
            return null

        return slot.deref
    }


    fun indicesCount() -> int = this.indices.length


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.host != null:
        {
            val tokens: pointer<ArrayList> = this.host.getAllTokens()

            if tokens != null:
                result.addAll(result.length, tokens)
        }

        for (var i = 0; i < this.indices.length; i++):
        {
            val index: pointer<Expression> = this.getIndex(i)

            if index == null:
                continue

            val tokens: pointer<ArrayList> = index.getAllTokens()

            if tokens != null:
                result.addAll(result.length, tokens)
        }

        result.addAll(result.length, this.extraTokens)
        result.setCmparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        sb.append("IndexAccess(")

        if this.host != null:
            sb.append(this.host.toString())

        sb.append(", [")

        var appendedIndex: bool = false

        for (var i = 0; i < this.indices.length; i++):
        {
            val index: pointer<Expression> = this.getIndex(i)

            if index == null:
                continue

            if appendedIndex:
                sb.append(", ")

            sb.append(index.toString())
            appendedIndex = true
        }

        sb.append("])")
        return sb
    }
}
