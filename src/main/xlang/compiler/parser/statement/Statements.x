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
@file.class("Statements")
package xlang.compiler.parser.statement

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct Statements
{
    static fun compareTokenPosition(left: pointer<*>, right: pointer<*>) -> int =
        TokenPosition.compareToken(left, right)


    private val statements: pointer<ArrayList>

    private var extraTokens: pointer<ArrayList>


    fun __init__()
    {
        this.statements = new ArrayList(sizeof(Statement))
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun __init__(statement: pointer<Statement>)
    {
        this.statements = new ArrayList(sizeof(Statement))
        this.extraTokens = new ArrayList(sizeof(Token))

        if statement != null:
            this.statements.push(statement)
    }


    fun addStatement(statement: pointer<Statement>) -> pointer<Statements>
    {
        if statement != null:
            this.statements.push(statement)

        return this
    }


    fun addStatements(stmts: pointer<Statements>) -> pointer<Statements>
    {
        if stmts != null && stmts.statements != null:
            this.statements.pushAll(stmts.statements)

        return this
    }


    fun addExtraToken(token: pointer<Token>) -> pointer<Statements>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun getStatements() -> pointer<ArrayList> = this.statements.clone()


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        for (var i = 0; i < this.statements.length; i++):
        {
            val statement: pointer<Statement> = this.statements.get(i) as pointer<Statement>

            if statement == null:
                continue

            val tokens: pointer<ArrayList> = statement.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.pushAll(this.extraTokens)
        result.setCmparator(Statements.compareTokenPosition)
        result.sort()
        return result
    }


    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        for (var i = 0; i < this.statements.length; i++):
        {
            val statement: pointer<Statement> = this.statements.get(i) as pointer<Statement>

            if statement == null:
                continue

            sb.append(statement.toString())
            sb.append("\n")
        }

        return sb
    }
}
