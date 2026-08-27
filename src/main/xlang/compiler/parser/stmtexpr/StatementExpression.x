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
@file.class("StatementExpression")
package xlang.compiler.parser.stmtexpr

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct StatementExpression
{
    static fun compareTokenPosition(left: pointer<*>, right: pointer<*>) -> int =
        TokenPosition.compareToken(left, right)


    static val BLOCK_KIND: int = 1

    static val IF_KIND: int = 2

    private var kind: int

    private var host: pointer<*>

    private var extraTokens: pointer<ArrayList>


    fun __init__(kind: int, host: pointer<*>)
    {
        this.kind = kind
        this.host = host
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun getKind() -> int = this.kind


    fun getHost() -> pointer<*> = this.host


    fun addExtraToken(token: pointer<Token>) -> pointer<StatementExpression>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun addExtraTokens(tokens: pointer<ArrayList>) -> pointer<StatementExpression>
    {
        if tokens != null:
            this.extraTokens.pushAll(tokens)

        return this
    }


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        result.pushAll(this.extraTokens)
        result.setCmparator(StatementExpression.compareTokenPosition)
        result.sort()
        return result
    }


    fun toString() -> pointer<StringBuilder> = new StringBuilder()
}
