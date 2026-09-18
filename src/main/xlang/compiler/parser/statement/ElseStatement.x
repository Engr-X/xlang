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

package xlang.compiler.parser.statement

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct ElseStatement
{
    private var statements: pointer<ArrayList>

    private var extraTokens: pointer<ArrayList>


    constructor()
    {
        this.statements = new ArrayList(sizeof(Statement))
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    constructor(statements: pointer<ArrayList>)
    {
        this.statements = statements
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    constructor(statement: pointer<Statement>)
    {
        this.statements = new ArrayList(sizeof(Statement))
        this.extraTokens = new ArrayList(sizeof(Token))

        if statement != null:
            this.statements.push(statement)
    }


    fun addExtraToken(token: pointer<Token>) -> pointer<ElseStatement>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun getStatements() -> pointer<ArrayList> = this.statements


    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens


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

        sb.append("else:\n")

        for (var i: int = 0; i < this.statements.length; i++):
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
