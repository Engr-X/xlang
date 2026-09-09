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
#file.class("Struct")
package xlang.compiler.parser.program

import xlang.compiler.parser.expression.Expression
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct StructConstructor
{
    private var modifiers: pointer<ModifierList>

    private var params: pointer<FunctionParams>

    private var bodyExpr: pointer<Expression>

    private var extraTokens: pointer<ArrayList>


    constructor(params: pointer<FunctionParams>, bodyExpr: pointer<Expression>)
    {
        this.modifiers = new ModifierList()
        this.params = if params == null:
                new FunctionParams()
            else:
                params

        this.bodyExpr = bodyExpr
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    constructor(modifiers: pointer<ModifierList>, params: pointer<FunctionParams>, bodyExpr: pointer<Expression>)
    {
        this.modifiers = if modifiers == null:
                new ModifierList()
            else:
                modifiers

        this.params = if params == null:
                new FunctionParams()
            else:
                params

        this.bodyExpr = bodyExpr
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun getModifiers() -> pointer<ModifierList> = this.modifiers


    fun getParams() -> pointer<FunctionParams> = this.params


    fun getBodyExpr() -> pointer<Expression> = this.bodyExpr


    fun addExtraToken(token: pointer<Token>) -> pointer<StructConstructor>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.modifiers != null:
            result.pushAll(this.modifiers.getAllTokens())

        if this.params != null:
            result.pushAll(this.params.getAllTokens())

        if this.bodyExpr != null:
            result.pushAll(this.bodyExpr.getAllTokens())

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        if this.modifiers != null && this.modifiers.length() > 0:
        {
            sb.append(this.modifiers.toString())
            sb.append(' ')
        }

        sb.append("constructor(")

        if this.params != null:
            sb.append(this.params.toString())

        sb.append(") = ")

        if this.bodyExpr != null:
            sb.append(this.bodyExpr.toString())

        return sb
    }
}


struct Struct
{
}
