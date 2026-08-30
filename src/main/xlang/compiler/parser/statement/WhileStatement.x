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
@file.class("WhileStatement")
package xlang.compiler.parser.statement

import xlang.compiler.parser.expression.Expression
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct WhileStatement
{
    private var condition: pointer<Expression>

    private val bodyStatements: pointer<ArrayList>

    private val elseStatements: pointer<ArrayList>

    private var extraTokens: pointer<ArrayList>


    fun __init__(condition: pointer<Expression>, bodyStatements: pointer<ArrayList>)
    {
        this.condition = condition
        this.bodyStatements = bodyStatements
        this.elseStatements = new ArrayList(sizeof(Statement))
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun __init__(condition: pointer<Expression>, bodyStatements: pointer<ArrayList>, elseStatements: pointer<ArrayList>)
    {
        this.condition = condition
        this.bodyStatements = bodyStatements
        this.elseStatements = elseStatements
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun getCondition() -> pointer<Expression> = this.condition


    fun getBodyStatements() -> pointer<ArrayList> = this.bodyStatements.clone()


    fun getElseStatements() -> pointer<ArrayList> = this.elseStatements.clone()


    fun haveElseStatement() -> bool = this.elseStatements.length > 0


    fun addExtraTokens(tokens: pointer<ArrayList>) -> pointer<WhileStatement>
    {
        if tokens != null:
            this.extraTokens.pushAll(tokens)

        return this
    }


    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.condition != null:
        {
            val tokens: pointer<ArrayList> = this.condition.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        for (var i: int = 0; i < this.bodyStatements.length; i++):
        {
            val statement: pointer<Statement> = this.bodyStatements.get(i) as pointer<Statement>

            if statement == null:
                continue

            val tokens: pointer<ArrayList> = statement.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        for (var i: int = 0; i < this.elseStatements.length; i++):
        {
            val statement: pointer<Statement> = this.elseStatements.get(i) as pointer<Statement>

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

        sb.append("while ")

        if this.condition != null:
            sb.append(this.condition.toString())

        sb.append(":\n")

        for (var i: int = 0; i < this.bodyStatements.length; i++):
        {
            val statement: pointer<Statement> = this.bodyStatements.get(i) as pointer<Statement>

            if statement == null:
                continue

            sb.append(statement.toString())
            sb.append("\n")
        }

        if this.haveElseStatement():
        {
            sb.append("else:\n")

            for (var i: int = 0; i < this.elseStatements.length; i++):
            {
                val statement: pointer<Statement> = this.elseStatements.get(i) as pointer<Statement>

                if statement == null:
                    continue

                sb.append(statement.toString())
                sb.append("\n")
            }
        }

        return sb
    }
}
