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
@file.class("Statement")
package xlang.compiler.parser.statement

import xlang.compiler.parser.expression.Expression
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct Statement
{
    static val EXPRESSION_TYPE: int = 0

    static val EXPRESSION_LIST_TYPE: int = 1

    static val VARIABLE_DEFINE_TYPE: int = 2

    static val VARIABLE_DEFINES_TYPE: int = 3

    static val RETURN_TYPE: int = 3


    static fun fromExprStatement(expr: pointer<ExprStatement>) -> pointer<Statement> =
        new Statement(EXPRESSION_TYPE, expr)


    static fun fromExprListStatement(expr: pointer<ExprListStatement>) -> pointer<Statement> =
        new Statement(EXPRESSION_LIST_TYPE, expr)


    static fun fromVariableDefine(variableDefine: pointer<VariableDefine>) -> pointer<Statement> =
        new Statement(VARIABLE_DEFINE_TYPE, variableDefine)


    static fun fromVariableDefines(variableDefines: pointer<VariableDefines>) -> pointer<Statement> =
        new Statement(VARIABLE_DEFINES_TYPE, variableDefines)


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


    fun getKind() -> int = this.kind


    fun getRoot() -> pointer<*> = this.root


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        result.pushAll(this.extraTokens)

        if this.root != null:
        {
            var tokens: pointer<ArrayList> = null

            if this.kind == EXPRESSION_TYPE:
            {
                val statement: pointer<ExprStatement> = this.root as pointer<ExprStatement>
                tokens = statement.getAllTokens()
            }
            elif this.kind == EXPRESSION_LIST_TYPE:
            {
                val statement: pointer<ExprListStatement> = this.root as pointer<ExprListStatement>
                tokens = statement.getAllTokens()
            }
            elif this.kind == VARIABLE_DEFINE_TYPE:
            {
                val statement: pointer<VariableDefines> = this.root as pointer<VariableDefines>
                tokens = statement.getAllTokens()
            }
            elif this.kind == RETURN_TYPE:
            {
                val statement: pointer<ReturnStatement> = this.root as pointer<ReturnStatement>
                tokens = statement.getAllTokens()
            }


            if tokens != null:
                result.pushAll(tokens)
        }

        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    fun toString() -> pointer<StringBuilder> = 
        if this.root == null:
            new StringBuilder()
        elif this.kind == EXPRESSION_TYPE:
        {
            val statement: pointer<ExprStatement> = this.root as pointer<ExprStatement>
            statement.toString()
        }
        elif this.kind == EXPRESSION_LIST_TYPE:
        {
            val statement: pointer<ExprListStatement> = this.root as pointer<ExprListStatement>
            statement.toString()
        }
        elif this.kind == VARIABLE_DEFINE_TYPE:
        {
            val statement: pointer<VariableDefine> = this.root as pointer<VariableDefine> 
            statement.toString()
        }
        elif this.kind == VARIABLE_DEFINES_TYPE:
        {
            val statement: pointer<VariableDefines> = this.root as pointer<VariableDefines>
            statement.toString()
        }
        elif this.kind == RETURN_TYPE:
        {
            val statement: pointer<ReturnStatement> = this.root as pointer<ReturnStatement>
            statement.toString()
        }
        else: new StringBuilder()
}
