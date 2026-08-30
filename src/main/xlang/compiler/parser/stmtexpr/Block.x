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
@file.class("Block")
package xlang.compiler.parser.stmtexpr

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct Block
{
    private var statements: pointer<ArrayList>

    private var extraTokens: pointer<ArrayList>


    fun __init__()
    {
        this.statements = new ArrayList(sizeof(Statement))
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun __init__(statements: pointer<ArrayList>)
    {
        this.statements = statements
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun addStatement(statement: pointer<Statement>) -> pointer<Block>
    {
        if statement != null:
            this.statements.push(statement)

        return this
    }


    fun getStatements() -> pointer<ArrayList> = this.statements.clone()


    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    fun addExtraToken(token: pointer<Token>) -> pointer<Block>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        for (var i: int = 0; i < this.statements.length; i++):
        {
            val statement: pointer<Statement> = this.statements.get(i) as pointer<Statement>

            if statement == null:
                continue

            val tokens: pointer<ArrayList> = statement.getAllTokens()

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

        sb.append("{\n")

        for (var i: int = 0; i < this.statements.length; i++):
        {
            val statement: pointer<Statement> = this.statements.get(i) as pointer<Statement>

            if statement == null:
                continue

            sb.append(statement.toString())
            sb.append("\n")
        }

        sb.append("}\n")
        return sb
    }
}
