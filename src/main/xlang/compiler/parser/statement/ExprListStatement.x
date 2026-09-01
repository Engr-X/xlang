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
@file.class("ExprListStatement")
package xlang.compiler.parser.statement

import xlang.compiler.parser.expression.Expression
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList


struct ExprListStatement
{
    private val exprList: pointer<ArrayList>

    private var extraTokens: pointer<ArrayList>


    fun __init__(expr: pointer<Expression>)
    {
        this.exprList = new ArrayList(sizeof(Expression)) 
        this.extraTokens = new ArrayList(sizeof(Token))
        this.exprList.push(expr)
    }


    fun addExpression(expr: pointer<Expression>) -> pointer<ExprListStatement>
    {
        if expr != null:
            this.exprList.push(expr)

        return this
    }


    fun addExpressions(exprs: pointer<ArrayList>) -> pointer<ExprListStatement>
    {
        if exprs != null:
            this.exprList.pushAll(exprs)

        return this
    }


    fun addExpressions(other: pointer<ExprListStatement>) -> pointer<ExprListStatement>
    {
        if other != null:
            this.exprList.pushAll(other.exprList)

        return this
    }


    fun getExpressions() -> pointer<ArrayList> = this.exprList.clone()


    fun addExtraToken(token: pointer<Token>) -> pointer<ExprListStatement>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun expand() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Statement))

        for (var i = 0; i < this.exprList.length; i++):
        {
            val expression: pointer<Expression> = this.exprList.get(i) as pointer<Expression>

            if expression != null:
            {
                val exprStatement: pointer<ExprStatement> = new ExprStatement(expression)
                result.push(Statement.fromExprStatement(exprStatement))
            }
        }

        return result
    }


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        for (var i = 0; i < this.exprList.length; i++):
        {
            val expression: pointer<Expression> = this.exprList.get(i) as pointer<Expression>

            if expression == null:
                continue

            val tokens: pointer<ArrayList> = expression.getAllTokens()

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

        for (var i = 0; i < this.exprList.length; i++)
        {
            val expression: pointer<Expression> = this.exprList.get(i) as pointer<Expression>

            if expression == null:
                continue

            sb.append(expression.toString())
            sb.append(",\n")
        }

        return sb
    }
}
