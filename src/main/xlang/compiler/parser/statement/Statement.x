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
import xlang.util.string.StringBuilder


struct Statement
{
    static val EXPRESSION_TYPE: int = 0

    static val EXPRESSION_LIST_TYPE: int = 1

    static val VARIABLE_DEFINE_TYPE: int = 2

    static val VARIABLE_DEFINES_TYPE: int = 3

    static val WHILE_TYPE: int = 4

    static val FOR_TYPE: int = 5

    static val RETURN_TYPE: int = 6

    static val BREAK_TYPE: int = 7

    static val CONTINUE_TYPE: int = 8

    static val PASS_TYPE: int = 9


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


    static fun fromWhileStatement(statement: pointer<WhileStatement>) -> pointer<Statement> =
        new Statement(WHILE_TYPE, statement)


    static fun fromForStatement(statement: pointer<ForStatement>) -> pointer<Statement> =
        new Statement(FOR_TYPE, statement)


    static fun fromBreakStatement(statement: pointer<BreakStatement>) -> pointer<Statement> =
        new Statement(BREAK_TYPE, statement)


    static fun fromContinueStatement(statement: pointer<ContinueStatement>) -> pointer<Statement> =
        new Statement(CONTINUE_TYPE, statement)


    static fun fromPassStatement(statement: pointer<PassStatement>) -> pointer<Statement> =
        new Statement(PASS_TYPE, statement)


    private var kind: int

    private var root: pointer<*>

    private var extraTokens: pointer<ArrayList>


    private constructor(kind: int, root: pointer<*>)
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


    fun expand() -> pointer<ArrayList> =
        if this.kind == VARIABLE_DEFINES_TYPE:
        {
            val statement: pointer<VariableDefines> = this.root as pointer<VariableDefines>
            statement.expand()
        }
        elif this.kind == EXPRESSION_LIST_TYPE:
        {
            val statement: pointer<ExprListStatement> = this.root as pointer<ExprListStatement>
            statement.expand()
        }
        elif this.kind == FOR_TYPE:
        {
            val statement: pointer<ForStatement> = this.root as pointer<ForStatement>
            statement.expand()
        }
        else: new ArrayList(sizeof(Statement)).push(this)


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        result.pushAll(this.extraTokens)

        if this.root != null:
        {
            var tokens: pointer<ArrayList> = if this.kind == EXPRESSION_TYPE:
            {
                val statement: pointer<ExprStatement> = this.root as pointer<ExprStatement>
                statement.getAllTokens()
            }
            elif this.kind == EXPRESSION_LIST_TYPE:
            {
                val statement: pointer<ExprListStatement> = this.root as pointer<ExprListStatement>
                statement.getAllTokens()
            }
            elif this.kind == VARIABLE_DEFINE_TYPE:
            {
                val statement: pointer<VariableDefine> = this.root as pointer<VariableDefine>
                statement.getAllTokens()
            }
            elif this.kind == VARIABLE_DEFINES_TYPE:
            {
                val statement: pointer<VariableDefines> = this.root as pointer<VariableDefines>
                statement.getAllTokens()
            }
            elif this.kind == WHILE_TYPE:
            {
                val statement: pointer<WhileStatement> = this.root as pointer<WhileStatement>
                statement.getAllTokens()
            }
            elif this.kind == FOR_TYPE:
            {
                val statement: pointer<ForStatement> = this.root as pointer<ForStatement>
                statement.getAllTokens()
            }
            elif this.kind == RETURN_TYPE:
            {
                val statement: pointer<ReturnStatement> = this.root as pointer<ReturnStatement>
                statement.getAllTokens()
            }
            elif this.kind == BREAK_TYPE:
            {
                val statement: pointer<BreakStatement> = this.root as pointer<BreakStatement>
                statement.getAllTokens()
            }
            elif this.kind == CONTINUE_TYPE:
            {
                val statement: pointer<ContinueStatement> = this.root as pointer<ContinueStatement>
                statement.getAllTokens()
            }
            elif this.kind == PASS_TYPE:
            {
                val statement: pointer<PassStatement> = this.root as pointer<PassStatement>
                statement.getAllTokens()
            }
            else: null


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
        elif this.kind == WHILE_TYPE:
        {
            val statement: pointer<WhileStatement> = this.root as pointer<WhileStatement>
            statement.toString()
        }
        elif this.kind == FOR_TYPE:
        {
            val statement: pointer<ForStatement> = this.root as pointer<ForStatement>
            statement.toString()
        }
        elif this.kind == RETURN_TYPE:
        {
            val statement: pointer<ReturnStatement> = this.root as pointer<ReturnStatement>
            statement.toString()
        }
        elif this.kind == BREAK_TYPE:
        {
            val statement: pointer<BreakStatement> = this.root as pointer<BreakStatement>
            statement.toString()
        }
        elif this.kind == CONTINUE_TYPE:
        {
            val statement: pointer<ContinueStatement> = this.root as pointer<ContinueStatement>
            statement.toString()
        }
        elif this.kind == PASS_TYPE:
        {
            val statement: pointer<PassStatement> = this.root as pointer<PassStatement>
            statement.toString()
        }
        else: new StringBuilder()
}
