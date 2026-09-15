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
    private var modifier: int

    private var paramName: pointer<char>

    private var paramType: pointer<Type>

    private var extraTokens: pointer<ArrayList>


    constructor(paramName: pointer<char>, paramType: pointer<Type>)
    {
        this.modifier = Field.constModifier()
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


    fun markAsMut() -> pointer<FunctionParam>
    {
        this.modifier = Field.mutModifier()
        return this
    }


    fun markAsConst() -> pointer<FunctionParam>
    {
        this.modifier = Field.constModifier()
        return this
    }


    fun canModified() -> bool = this.modifier == Field.mutModifier()


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


    constructor()
    {
        this.params = new ArrayList(sizeof(FunctionParam))
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    constructor(param: pointer<FunctionParam>)
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


    constructor(params: pointer<FunctionParams>):
        this.params = if params == null:
                new FunctionParams()
            else:
                params


    fun toFunctionParams() -> pointer<FunctionParams> = this.params
}


struct Function
{
    private var annotations: pointer<ArrayList>

    private var modifiers: pointer<ArrayList>

    private var functionName: pointer<char>

    private var params: pointer<FunctionParams>

    private var returnType: pointer<Type>

    private var bodyExpr: pointer<Expression>

    private var extraTokens: pointer<ArrayList>


    constructor(functionName: pointer<char>, params: pointer<FunctionParams>, bodyExpr: pointer<Expression>)
    {
        this.annotations = new ArrayList(sizeof(Annotation))
        this.modifiers = new ArrayList(sizeof(Modifier))
        this.functionName = functionName
        this.params = params
        this.returnType = null
        this.bodyExpr = bodyExpr
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun haveBody() -> bool = this.bodyExpr != null


    fun getAnnotations() -> pointer<ArrayList> = this.annotations


    fun setAnnotations(annotations: pointer<ArrayList>) -> pointer<Function>
    {
        this.annotations = if annotations == null:
                new ArrayList(sizeof(Annotation))
            else:
                annotations

        return this
    }


    fun getModifiers() -> pointer<ArrayList> = this.modifiers


    fun setModifiers(modifiers: pointer<ArrayList>) -> pointer<Function>
    {
        this.modifiers = if modifiers == null:
                new ArrayList(sizeof(Modifier))
            else:
                modifiers

        return this
    }


    fun getFunctionName() -> pointer<char> = this.functionName


    fun getParams() -> pointer<FunctionParams> = this.params


    fun getReturnType() -> pointer<Type> =
        if this.returnType == null:
            null
        else:
            this.returnType.clone()


    fun setReturnType(returnType: pointer<Type>) -> pointer<Function>
    {
        this.returnType = returnType
        return this
    }


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
        {
            for (var i = 0; i < this.annotations.length; i++):
            {
                val annotation: pointer<Annotation> = this.annotations.get(i) as pointer<Annotation>

                if annotation != null:
                    result.pushAll(annotation.getAllTokens())
            }
        }

        if this.modifiers != null:
        {
            for (var i = 0; i < this.modifiers.length; i++):
            {
                val modifier: pointer<Modifier> = this.modifiers.get(i) as pointer<Modifier>

                if modifier != null:
                    result.pushAll(modifier.getAllTokens())
            }
        }

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

        if this.annotations != null && this.annotations.length > 0:
        {
            for (var i = 0; i < this.annotations.length; i++):
            {
                val annotation: pointer<Annotation> = this.annotations.get(i) as pointer<Annotation>

                if annotation != null:
                {
                    sb.append(annotation.toString())
                    sb.newline()
                }
            }
        }

        if this.modifiers != null && this.modifiers.length > 0:
        {
            var appendedModifier: bool = false

            for (var i = 0; i < this.modifiers.length; i++):
            {
                val modifier: pointer<Modifier> = this.modifiers.get(i) as pointer<Modifier>

                if modifier == null:
                    continue

                if appendedModifier:
                    sb.append(' ')

                sb.append(modifier.toString())
                appendedModifier = true
            }

            if appendedModifier:
                sb.append(' ')
        }

        sb.append("fun ")

        if this.functionName != null:
            sb.append(this.functionName)

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
