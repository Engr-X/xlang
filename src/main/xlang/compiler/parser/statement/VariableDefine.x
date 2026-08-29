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
@file.class("VariableDefine")
package xlang.compiler.parser.statement

import xlang.compiler.lexer.Tokenizer
import xlang.compiler.parser.expression.Assignment
import xlang.compiler.parser.expression.Atom
import xlang.compiler.parser.expression.Expression
import xlang.compiler.Type
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct VariableDefine
{
    private static val CONST_MODIFIER = 0
    private static val MUT_MODIFIER = 1


    private var modifier: int

    private var declaredType: pointer<Type>

    private var varName: pointer<char>

    private var assignExpr: pointer<Expression>

    private var extraTokens: pointer<ArrayList>


    fun __init__(varName: pointer<char>, assignExpr: pointer<Expression>)
    {
        this.modifier = CONST_MODIFIER
        this.declaredType = null
        this.varName = varName
        this.assignExpr = assignExpr
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun __init__(declaredType: pointer<Type>, varName: pointer<char>, assignExpr: pointer<Expression>)
    {
        this.modifier = CONST_MODIFIER
        this.declaredType = declaredType
        this.varName = varName
        this.assignExpr = assignExpr
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun markAsMut() -> pointer<VariableDefine>
    {
        this.modifier = MUT_MODIFIER
        return this
    }


    fun markAsConst() -> pointer<VariableDefine>
    {
        this.modifier = CONST_MODIFIER
        return this
    }


    fun addExtraToken(token: pointer<Token>) -> pointer<VariableDefine>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun canModified() -> bool = this.modifier == MUT_MODIFIER


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))


        if this.declaredType != null:
        {
            val tokens: pointer<ArrayList> = this.declaredType.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        if this.assignExpr != null:
        {
            val tokens: pointer<ArrayList> = this.assignExpr.getAllTokens()

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
        val sb: pointer<StringBuilder> = if this.canModified():
            new StringBuilder("var ")
        else:
            new StringBuilder("val ")

        if this.varName != null:
            sb.append(this.varName)

        if this.declaredType != null:
        {
            sb.append(": ")
            sb.append(this.declaredType.toString())
        }

        if this.assignExpr != null:
        {
            sb.append(" = ")
            sb.append(this.assignExpr.toString())
        }

        return sb
    }
}
