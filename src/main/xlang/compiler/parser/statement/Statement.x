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

import xlang.compiler.parser.expression.Expression
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList


struct Statement
{
    static val EXPRESSION_TYPE: int = 0

    static val RETURN_TYPE: int = 1


    static fun fromExprStatement(expr: pointer<ExprStatement>) -> pointer<Statement> =
        new Statement(EXPRESSION_TYPE, expr)

    static fun fromReturnStatement(statement: pointer<ReturnStatement>) -> pointer<Statement> =
        new Statement(RETURN_TYPE, statement)


    private var kind: int

    private var root: pointer<*>

    private var extraTokens: pointer<ArrayList>


    private fun __init__(kind: int, root: pointer<*>)
    {
        this.kind = kind
        this.root = root
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun addExtraToken(token: pointer<Token>) -> pointer<Statement>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        result.addAll(result.length, this.extraTokens)

        if this.root != null:
        {
            var tokens: pointer<ArrayList> = if this.kind == EXPRESSION_TYPE:
            {
                val statement: pointer<ExprStatement> = this.root as pointer<ExprStatement>
                tokens = statement.getAllTokens()
            }
            elif this.kind == RETURN_TYPE:
            {
                val statement: pointer<ReturnStatement> = this.root as pointer<ReturnStatement>
                tokens = statement.getAllTokens()
            }
            else: null


            if tokens != null:
                result.addAll(result.length, tokens)
        }

        result.setCmparator(TokenPosition.compareToken)
        result.sort()
        return result
    }
}


struct ExprStatement
{
    private val expr: pointer<Expression>

    private var extraTokens: pointer<ArrayList>


    fun __init__(expr: pointer<Expression>)
    {
        this.expr = expr
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun getExpression() -> pointer<Expression> = this.expr


    fun addExtraToken(token: pointer<Token>) -> pointer<ExprStatement>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.expr != null:
        {
            val tokens: pointer<ArrayList> = this.expr.getAllTokens()

            if tokens != null:
                result.addAll(result.length, tokens)
        }

        result.addAll(result.length, this.extraTokens)
        result.setCmparator(TokenPosition.compareToken)
        result.sort()
        return result
    }
}


struct ReturnStatement
{
    private val expr: pointer<Expression>

    private var extraTokens: pointer<ArrayList>


    fun __init__()
    {
        this.expr = null
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun __init__(expr: pointer<Expression>)
    {
        this.expr = expr
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun haveReturnValue() -> bool = this.expr != null


    fun getExpression() -> pointer<Expression> = this.expr


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.expr != null:
        {
            val tokens: pointer<ArrayList> = this.expr.getAllTokens()

            if tokens != null:
                result.addAll(result.length, tokens)
        }

        result.addAll(result.length, this.extraTokens)
        result.setCmparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    fun addExtraToken(token: pointer<Token>) -> pointer<ReturnStatement>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }
}
