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
@file.class("IfBranch")
package xlang.compiler.parser.stmtexpr

import xlang.compiler.parser.expression.Expression
import xlang.compiler.parser.statement.Statement
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct IfBranch
{
    private var condition: pointer<Expression>

    private val statements: pointer<ArrayList>

    private var extraTokens: pointer<ArrayList>


    fun __init__(condition: pointer<Expression>, statements: pointer<ArrayList>)
    {
        this.condition = condition
        this.statements = statements
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun __init__(condition: pointer<Expression>, statement: pointer<Statement>)
    {
        this.condition = condition
        this.statements = new ArrayList(sizeof(Statement))
        this.extraTokens = new ArrayList(sizeof(Token))

        if statement != null:
            this.statements.push(statement)
    }


    fun getCondition() -> pointer<Expression> = this.condition


    fun getStatements() -> pointer<ArrayList> = this.statements.clone()


    fun addExtraToken(token: pointer<Token>) -> pointer<IfBranch>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun addExtraTokens(tokens: pointer<ArrayList>) -> pointer<IfBranch>
    {
        if tokens != null:
            this.extraTokens.pushAll(tokens)

        return this
    }


    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    fun toIfElseBranch() -> pointer<IfElseBranch> =
        new IfElseBranch()
            .setCondition(this.condition)
            .addIfStatements(this.getStatements())
            .addExtraTokens(this.getExtraTokens())


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.condition != null:
        {
            val tokens: pointer<ArrayList> = this.condition.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

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
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }
}
