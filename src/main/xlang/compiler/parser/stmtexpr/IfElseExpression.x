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
@file.class("IfElseExpression")
package xlang.compiler.parser.stmtexpr

import xlang.compiler.parser.expression.Expression
import xlang.compiler.parser.statement.Statement
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct IfElseExpression
{
    private var condition: pointer<Expression>

    private val ifStmts: pointer<ArrayList>

    private val elseStmts: pointer<ArrayList>

    private var extraTokens: pointer<ArrayList>


    fun __init__()
    {
        this.condition = null
        this.ifStmts = new ArrayList(sizeof(Statement))
        this.elseStmts = new ArrayList(sizeof(Statement))
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun __init__(elseStmts: pointer<ArrayList>)
    {
        this.condition = null
        this.ifStmts = new ArrayList(sizeof(Statement))
        this.elseStmts = elseStmts
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun __init__(elseStatement: pointer<Statement>)
    {
        this.condition = null
        this.ifStmts = new ArrayList(sizeof(Statement))
        this.elseStmts = new ArrayList(sizeof(Statement))
        this.extraTokens = new ArrayList(sizeof(Token))

        if elseStatement != null:
            this.elseStmts.push(elseStatement)
    }


    fun setCondition(expr: pointer<Expression>) -> pointer<IfElseExpression>
    {
        this.condition = expr
        return this
    }


    fun addIfStatement(statement: pointer<Statement>) -> pointer<IfElseExpression>
    {
        if statement != null:
            this.ifStmts.push(statement)

        return this
    }


    fun addIfStatements(statement: pointer<ArrayList>) -> pointer<IfElseExpression>
    {
        if statement != null:
            this.ifStmts.pushAll(statement)

        return this
    }


    fun haveIfStatement() -> bool = this.ifStmts.length > 0


    fun haveElseStatement() -> bool = this.elseStmts.length > 0


    fun getCondition() -> pointer<Expression> = this.condition


    fun getIfStatements() -> pointer<ArrayList> = this.ifStmts.clone()


    fun getElseStatements() -> pointer<ArrayList> = this.elseStmts.clone()


    fun addExtraToken(token: pointer<Token>) -> pointer<IfElseExpression>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun addExtraTokens(tokens: pointer<ArrayList>) -> pointer<IfElseExpression>
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

        for (var i = 0; i < this.ifStmts.length; i++):
        {
            val statement: pointer<Statement> = this.ifStmts.get(i) as pointer<Statement>

            if statement == null:
                continue

            val tokens: pointer<ArrayList> = statement.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        for (var i = 0; i < this.elseStmts.length; i++):
        {
            val statement: pointer<Statement> = this.elseStmts.get(i) as pointer<Statement>

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

        for (var i = 0; i < this.ifStmts.length; i++):
        {
            val statement: pointer<Statement> = this.ifStmts.get(i) as pointer<Statement>

            if statement == null:
                continue

            sb.append(statement.toString())
            sb.append("\n")
        }


        if this.haveElseStatement()
        {
            sb.append("else:\n")

            for (var i = 0; i < this.elseStmts.length; i++):
            {
                val statement: pointer<Statement> = this.elseStmts.get(i) as pointer<Statement>

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
