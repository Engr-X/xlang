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
package xlang.compiler.parser.program

import xlang.compiler.Type
import xlang.compiler.parser.expression.Expression
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct FunctionParam
{
    private var paramName: pointer<char>

    private var paramType: pointer<Type>

    private var extraTokens: pointer<ArrayList>


    fun __init__(paramName: pointer<char>, paramType: pointer<Type>)
    {
        this.paramName = paramName
        this.paramType = paramType
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun getParamName() -> pointer<char> = this.paramName


    fun getParamType() -> pointer<Type> =
        if this.paramType == null:
            null
        else:
            this.paramType.clone()


    fun addExtraToken(token: pointer<Token>) -> pointer<FunctionParam>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.paramType != null:
            result.pushAll(this.paramType.getAllTokens())

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        if this.paramName != null:
            sb.append(this.paramName)

        if this.paramType != null:
        {
            sb.append(": ")
            sb.append(this.paramType.toString())
        }

        return sb
    }
}


struct FunctionParams
{
    private val params: pointer<ArrayList>

    private val extraTokens: pointer<ArrayList>


    fun __init__()
    {
        this.params = new ArrayList(sizeof(FunctionParam))
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun __init__(param: pointer<FunctionParam>)
    {
        this.params = new ArrayList(sizeof(FunctionParam))
        this.extraTokens = new ArrayList(sizeof(Token))
        this.push(param)
    }


    fun push(param: pointer<FunctionParam>) -> pointer<FunctionParams>
    {
        if param != null:
            this.params.push(param)

        return this
    }


    fun pushAll(params: pointer<FunctionParams>) -> pointer<FunctionParams>
    {
        if params != null && params.params != null:
        {
            this.params.pushAll(params.params)

            if params.extraTokens != null:
                this.extraTokens.pushAll(params.extraTokens)
        }

        return this
    }


    fun addExtraToken(token: pointer<Token>) -> pointer<FunctionParams>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun length() -> int = this.params.length


    fun get(index: int) -> pointer<FunctionParam>
    {
        if index < 0 || index >= this.params.length:
            return null

        return this.params.get(index) as pointer<FunctionParam>
    }


    fun getParams() -> pointer<ArrayList> = this.params.clone()


    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        for (var i = 0; i < this.params.length; i++):
        {
            val param: pointer<FunctionParam> = this.get(i)

            if param == null:
                continue

            val tokens: pointer<ArrayList> = param.getAllTokens()

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
        var appendedParam: bool = false

        for (var i = 0; i < this.params.length; i++):
        {
            val param: pointer<FunctionParam> = this.get(i)

            if param == null:
                continue

            if appendedParam:
                sb.append(", ")

            sb.append(param.toString())
            appendedParam = true
        }

        return sb
    }
}


struct FunctionParamsMaybe
{
    private var params: pointer<FunctionParams>


    fun __init__(params: pointer<FunctionParams>):
        this.params = if params == null:
                new FunctionParams()
            else:
                params


    fun toFunctionParams() -> pointer<FunctionParams> = this.params
}


struct Function
{
    private var annotations: pointer<Annotations>

    private var modifiers: pointer<ModifierList>

    private var functionName: pointer<QualifiedName>

    private var params: pointer<FunctionParams>

    private var returnType: pointer<Type>

    private var bodyExpr: pointer<Expression>

    private var extraTokens: pointer<ArrayList>


    fun __init__(functionName: pointer<QualifiedName>, params: pointer<FunctionParams>, bodyExpr: pointer<Expression>)
    {
        this.annotations = new Annotations()
        this.modifiers = new ModifierList()
        this.functionName = functionName
        this.params = params
        this.returnType = null
        this.bodyExpr = bodyExpr
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun __init__(
        annotations: pointer<Annotations>,
        modifiers: pointer<ModifierList>,
        functionName: pointer<QualifiedName>,
        params: pointer<FunctionParams>,
        returnType: pointer<Type>,
        bodyExpr: pointer<Expression>)
    {
        this.annotations = if annotations == null:
                new Annotations()
            else:
                annotations

        this.modifiers = if modifiers == null:
                new ModifierList()
            else:
                modifiers

        this.functionName = functionName
        this.params = params
        this.returnType = returnType
        this.bodyExpr = bodyExpr
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun getAnnotations() -> pointer<Annotations> = this.annotations


    fun getModifiers() -> pointer<ModifierList> = this.modifiers


    fun getFunctionName() -> pointer<QualifiedName> = this.functionName


    fun getParams() -> pointer<FunctionParams> = this.params


    fun getReturnType() -> pointer<Type> =
        if this.returnType == null:
            null
        else:
            this.returnType.clone()


    fun getBodyExpr() -> pointer<Expression> = this.bodyExpr


    fun addExtraToken(token: pointer<Token>) -> pointer<Function>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.annotations != null:
            result.pushAll(this.annotations.getAllTokens())

        if this.modifiers != null:
            result.pushAll(this.modifiers.getAllTokens())

        if this.functionName != null:
            result.pushAll(this.functionName.getAllTokens())

        if this.params != null:
            result.pushAll(this.params.getAllTokens())

        if this.returnType != null:
            result.pushAll(this.returnType.getAllTokens())

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

        if this.annotations != null && this.annotations.length() > 0:
        {
            sb.append(this.annotations.toString())
            sb.newline()
        }

        if this.modifiers != null && this.modifiers.length() > 0:
        {
            sb.append(this.modifiers.toString())
            sb.append(' ')
        }

        sb.append("fun ")

        if this.functionName != null:
            sb.append(this.functionName.toString())

        sb.append('(')

        if this.params != null:
            sb.append(this.params.toString())

        sb.append(")")

        if this.returnType != null:
        {
            sb.append(" -> ")
            sb.append(this.returnType.toString())
        }

        sb.append(" = ")

        if this.bodyExpr != null:
            sb.append(this.bodyExpr.toString())

        return sb
    }
}
