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
@file.class("IfElseBranch")
package xlang.compiler.parser.stmtexpr

import xlang.compiler.parser.expression.Expression
import xlang.compiler.parser.statement.Statement
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct IfElseBranch
{
    private var condition: pointer<Expression>

    private val ifStatements: pointer<ArrayList>

    private val elseStatements: pointer<ArrayList>

    private var extraTokens: pointer<ArrayList>


    fun __init__()
    {
        this.condition = null
        this.ifStatements = new ArrayList(sizeof(Statement))
        this.elseStatements = new ArrayList(sizeof(Statement))
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun __init__(elseStatements: pointer<ArrayList>)
    {
        this.condition = null
        this.ifStatements = new ArrayList(sizeof(Statement))
        this.elseStatements = elseStatements
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun __init__(elseStatement: pointer<Statement>)
    {
        this.condition = null
        this.ifStatements = new ArrayList(sizeof(Statement))
        this.elseStatements = new ArrayList(sizeof(Statement))
        this.extraTokens = new ArrayList(sizeof(Token))

        if elseStatement != null:
            this.elseStatements.push(elseStatement)
    }


    fun setCondition(expr: pointer<Expression>) -> pointer<IfElseBranch>
    {
        this.condition = expr
        return this
    }


    fun addIfStatement(statement: pointer<Statement>) -> pointer<IfElseBranch>
    {
        if statement != null:
            this.ifStatements.push(statement)

        return this
    }


    fun addIfStatements(statement: pointer<ArrayList>) -> pointer<IfElseBranch>
    {
        if statement != null:
            this.ifStatements.pushAll(statement)

        return this
    }


    fun haveIfStatement() -> bool = this.ifStatements.length > 0


    fun haveElseStatement() -> bool = this.elseStatements.length > 0


    fun getCondition() -> pointer<Expression> = this.condition


    fun getIfStatements() -> pointer<ArrayList> = this.ifStatements.clone()


    fun getElseStatements() -> pointer<ArrayList> = this.elseStatements.clone()


    fun addExtraToken(token: pointer<Token>) -> pointer<IfElseBranch>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun addExtraTokens(tokens: pointer<ArrayList>) -> pointer<IfElseBranch>
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

        for (var i = 0; i < this.ifStatements.length; i++):
        {
            val statement: pointer<Statement> = this.ifStatements.get(i) as pointer<Statement>

            if statement == null:
                continue

            val tokens: pointer<ArrayList> = statement.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        for (var i = 0; i < this.elseStatements.length; i++):
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

        if this.condition != null:
        {
            sb.append("(if ")
            sb.append(this.condition.toString())
        }

        sb.append(":\n")

        for (var i = 0; i < this.ifStatements.length; i++):
        {
            val statement: pointer<Statement> = this.ifStatements.get(i) as pointer<Statement>

            if statement == null:
                continue

            sb.append(statement.toString())
            sb.append("\n")
        }


        if this.haveElseStatement()
        {
            sb.append("else:\n")

            for (var i = 0; i < this.elseStatements.length; i++):
            {
                val statement: pointer<Statement> = this.elseStatements.get(i) as pointer<Statement>

                if statement == null:
                    continue

                sb.append(statement.toString())
                sb.append("\n")
            }
        }

        sb.append(")\n")

        return sb
    }
}
