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
@file.class("ForStatement")
package xlang.compiler.parser.statement

import xlang.compiler.lexer.Tokenizer
import xlang.compiler.parser.expression.Atom
import xlang.compiler.parser.expression.Expression
import xlang.compiler.parser.stmtexpr.Block
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct ForHeader
{
    private var initStmt: pointer<Statement>

    private var condition: pointer<Expression>

    private var stepStmt: pointer<Statement>

    private var extraTokens: pointer<ArrayList>


    fun __init__()
    {
        this.initStmt = null
        this.condition = null
        this.stepStmt = null
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun __init__(
        initStmt: pointer<Statement>,
        condition: pointer<Expression>,
        stepStmt: pointer<Statement>)
    {
        this.initStmt = initStmt
        this.condition = condition
        this.stepStmt = stepStmt
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun getInitStatement() -> pointer<Statement> = this.initStmt


    fun getCondition() -> pointer<Expression> = this.condition


    fun getStepStatement() -> pointer<Statement> = this.stepStmt


    fun addExtraToken(token: pointer<Token>) -> pointer<ForHeader>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun addExtraTokens(tokens: pointer<ArrayList>) -> pointer<ForHeader>
    {
        if tokens != null:
            this.extraTokens.pushAll(tokens)

        return this
    }


    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.initStmt != null:
        {
            val tokens: pointer<ArrayList> = this.initStmt.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        if this.condition != null:
        {
            val tokens: pointer<ArrayList> = this.condition.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        if this.stepStmt != null:
        {
            val tokens: pointer<ArrayList> = this.stepStmt.getAllTokens()

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

        if this.initStmt != null:
            sb.append(this.initStmt.toString())

        sb.append("; ")

        if this.condition != null:
            sb.append(this.condition.toString())

        sb.append("; ")

        if this.stepStmt != null:
            sb.append(this.stepStmt.toString())

        return sb
    }
}


struct ForStatement
{
    private static fun makeDefaultCondition() -> pointer<Expression>
    {
        val position: pointer<TokenPosition> = TokenPosition.autoGenPos()
        val token: pointer<Token> = new Token(Tokenizer.KW_TRUE, position, "true")
        val tokens: pointer<ArrayList> = new ArrayList(sizeof(pointer<*>))
        val resultItem: pointer<*> = token as pointer<*>

        tokens.push(resultItem.ref)
        return Expression.fromAtom(new Atom(Atom.BOOL_IMM_KIND, tokens))
    }


    private var header: pointer<ForHeader>

    private val bodyStmts: pointer<ArrayList>

    private val elseStmts: pointer<ArrayList>

    private var extraTokens: pointer<ArrayList>


    fun __init__(
        header: pointer<ForHeader>,
        bodyStmts: pointer<ArrayList>)
    {
        this.header = if header == null:
            new ForHeader()
        else:
            header

        this.bodyStmts = if bodyStmts == null:
            new ArrayList(sizeof(Statement))
        else:
            bodyStmts

        this.elseStmts = new ArrayList(sizeof(Statement))
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun __init__(
        header: pointer<ForHeader>,
        bodyStmts: pointer<ArrayList>,
        elseStmts: pointer<ArrayList>)
    {
        this.header = if header == null:
            new ForHeader()
        else:
            header

        this.bodyStmts = if bodyStmts == null:
            new ArrayList(sizeof(Statement))
        else:
            bodyStmts

        this.elseStmts = if elseStmts == null:
            new ArrayList(sizeof(Statement))
        else:
            elseStmts

        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun getInitStatement() -> pointer<Statement> = this.header.getInitStatement()


    fun getCondition() -> pointer<Expression> = this.header.getCondition()


    fun getStepStatement() -> pointer<Statement> = this.header.getStepStatement()


    fun getBodyStatements() -> pointer<ArrayList> = this.bodyStmts.clone()


    fun getElseStatements() -> pointer<ArrayList> = this.elseStmts.clone()


    fun haveElseStatement() -> bool = this.elseStmts.length > 0


    fun addExtraToken(token: pointer<Token>) -> pointer<ForStatement>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun addExtraTokens(tokens: pointer<ArrayList>) -> pointer<ForStatement>
    {
        if tokens != null:
            this.extraTokens.pushAll(tokens)

        return this
    }


    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    fun expand() -> pointer<ArrayList>
    {
        val whileBodyStmts: pointer<ArrayList> = new ArrayList(sizeof(Statement))
        whileBodyStmts.pushAll(this.bodyStmts)

        if this.header.getStepStatement() != null:
            whileBodyStmts.push(this.header.getStepStatement())

        val condition: pointer<Expression> = if this.header.getCondition() == null:
            ForStatement.makeDefaultCondition()
        else:
            this.header.getCondition()

        val whileStmt: pointer<WhileStatement> =
            new WhileStatement(condition, whileBodyStmts, this.elseStmts.clone())

        val resultStmts: pointer<ArrayList> = new ArrayList(sizeof(Statement))

        if this.header.getInitStatement() != null:
            resultStmts.push(this.header.getInitStatement())

        resultStmts.push(Statement.fromWhileStatement(whileStmt))

        val block: pointer<Block> = new Block(resultStmts)
        val headerTokens: pointer<ArrayList> = this.header.getExtraTokens()

        for (var i: int = 0; i < headerTokens.length; i++):
            block.addExtraToken(headerTokens.get(i) as pointer<Token>)

        for (var i: int = 0; i < this.extraTokens.length; i++):
            block.addExtraToken(this.extraTokens.get(i) as pointer<Token>)

        val blockExpr: pointer<Expression> = Expression.fromBlockExpr(block)
        val blockStmt: pointer<Statement> = Statement.fromExprStatement(new ExprStatement(blockExpr))
        val result: pointer<ArrayList> = new ArrayList(sizeof(Statement))
        result.push(blockStmt)
        return result
    }


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))
        val headerTokens: pointer<ArrayList> = this.header.getAllTokens()

        if headerTokens != null:
            result.pushAll(headerTokens)

        for (var i: int = 0; i < this.bodyStmts.length; i++):
        {
            val statement: pointer<Statement> = this.bodyStmts.get(i) as pointer<Statement>

            if statement == null:
                continue

            val tokens: pointer<ArrayList> = statement.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        for (var i: int = 0; i < this.elseStmts.length; i++):
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

        sb.append("for (")
        sb.append(this.header.toString())
        sb.append("):\n")

        for (var i: int = 0; i < this.bodyStmts.length; i++):
        {
            val statement: pointer<Statement> = this.bodyStmts.get(i) as pointer<Statement>

            if statement == null:
                continue

            sb.append(statement.toString())
            sb.append("\n")
        }

        if this.haveElseStatement():
        {
            sb.append("else:\n")

            for (var i: int = 0; i < this.elseStmts.length; i++):
            {
                val statement: pointer<Statement> = this.elseStmts.get(i) as pointer<Statement>

                if statement == null:
                    continue

                sb.append(statement.toString())
                sb.append("\n")
            }
        }

        return sb
    }
}
