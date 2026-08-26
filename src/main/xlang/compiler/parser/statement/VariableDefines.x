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
@file.class("VariableDefines")
package xlang.compiler.parser.statement

import xlang.compiler.lexer.Tokenizer
import xlang.compiler.parser.expression.Assignment
import xlang.compiler.parser.expression.Atom
import xlang.compiler.parser.expression.Expression
import xlang.compiler.Type
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList


struct VariableDefines
{
    private static val CONST_MODIFIER = 0
    private static val MUT_MODIFIER = 1


    private var defines: pointer<ArrayList>

    private var extraTokens: pointer<ArrayList>


    fun __init__(varDef: pointer<VariableDefine>)
    {
        this.defines = new ArrayList(sizeof(VariableDefine))
        this.extraTokens = new ArrayList(sizeof(Token))
        this.defines.push(varDef)
    }


    fun addExtraToken(token: pointer<Token>) -> pointer<VariableDefines>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun markAsConst() -> pointer<VariableDefines>
    {
        for (var i = 0; i < this.defines.length; i++):
        {
            val variableDefine: pointer<VariableDefine> = this.defines.get(i) as pointer<VariableDefine>

            if variableDefine != null:
                variableDefine.markAsConst()
        }

        return this
    }


    fun markAsMut() -> pointer<VariableDefines>
    {
        for (var i = 0; i < this.defines.length; i++):
        {
            val variableDefine: pointer<VariableDefine> = this.defines.get(i) as pointer<VariableDefine>

            if variableDefine != null:
                variableDefine.markAsMut()
        }

        return this
    }


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        for (var i = 0; i < this.defines.length; i++):
        {
            val variableDefine: pointer<VariableDefine> = this.defines.get(i) as pointer<VariableDefine>

            if variableDefine == null:
                continue

            val tokens: pointer<ArrayList> = variableDefine.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.pushAll(this.extraTokens)
        result.setCmparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    fun canModified() -> bool
    {
        if this.defines.length <= 0:
            return false

        val variableDefine: pointer<VariableDefine> = this.defines.get(0) as pointer<VariableDefine>

        return variableDefine != null && variableDefine.canModified()
    }
}
