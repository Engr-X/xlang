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

#file.outerClass("VariableDefine")
package xlang.compiler.parser.statement

import xlang.compiler.lexer.Tokenizer
import xlang.compiler.parser.expression.Assignment
import xlang.compiler.parser.expression.Atom
import xlang.compiler.parser.expression.Expression
import xlang.compiler.parser.program.Field
import xlang.compiler.Type
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct VariableDefine
{
    private var modifier: int

    private var declaredType: pointer<Type>

    private var varName: pointer<char>

    private var assignExpr: pointer<Expression>

    private var extraTokens: pointer<ArrayList>


    constructor(varName: pointer<char>, assignExpr: pointer<Expression>)
    {
        this.modifier = Field.constModifier()
        this.declaredType = null
        this.varName = varName
        this.assignExpr = assignExpr
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    constructor(declaredType: pointer<Type>, varName: pointer<char>, assignExpr: pointer<Expression>)
    {
        this.modifier = Field.constModifier()
        this.declaredType = declaredType
        this.varName = varName
        this.assignExpr = assignExpr
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun haveDeclaredType() -> bool = this.declaredType != null


    fun haveInitialValue() -> bool = this.assignExpr != null


    fun markAsMut() -> pointer<VariableDefine>
    {
        this.modifier = Field.mutModifier()
        return this
    }


    fun markAsConst() -> pointer<VariableDefine>
    {
        this.modifier = Field.constModifier()
        return this
    }


    fun addExtraToken(token: pointer<Token>) -> pointer<VariableDefine>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun canModified() -> bool = this.modifier == Field.mutModifier()


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
