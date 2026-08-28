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
package xlang.compiler.parser.expression

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.String
import xlang.util.string.StringBuilder


struct NewIdentifier
{
    private var identifier: pointer<char>

    private var extraTokens: pointer<ArrayList>


    fun __init__(identifier: pointer<char>)
    {
        this.identifier = String.strdup(identifier)
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun getIdentifier() -> pointer<char> = String.strdup(this.identifier)


    fun addExtraToken(token: pointer<Token>) -> pointer<NewIdentifier>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun addExtraTokens(tokens: pointer<ArrayList>) -> pointer<NewIdentifier>
    {
        if tokens != null:
            this.extraTokens.pushAll(tokens)

        return this
    }


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder("new ")
        sb.append(this.identifier)
        return sb
    }
}


struct NewFunction
{
    private var host: pointer<char>

    private var arguments: pointer<ArrayList>

    private var extraTokens: pointer<ArrayList>


    fun __init__(host: pointer<char>)
    {
        this.host = String.strdup(host)
        this.arguments = new ArrayList(sizeof(pointer<Expression>))
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun getHost() -> pointer<char> = String.strdup(this.host)


    fun addArgument(argument: pointer<Expression>) -> pointer<NewFunction>
    {
        if argument != null:
            this.arguments.push(argument.ref)

        return this
    }


    fun setArguments(arguments: pointer<ExpressionTuple>) -> pointer<NewFunction>
    {
        if arguments != null:
        {
            this.arguments = arguments.getList()
            this.extraTokens.pushAll(arguments.getExtraTokens())
        }

        return this
    }


    fun addExtraToken(token: pointer<Token>) -> pointer<NewFunction>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun addExtraTokens(tokens: pointer<ArrayList>) -> pointer<NewFunction>
    {
        if tokens != null:
            this.extraTokens.pushAll(tokens)

        return this
    }


    fun getArgument(index: int) -> pointer<Expression>
    {
        val slot: pointer<pointer<Expression>> = this.arguments.get(index) as pointer<pointer<Expression>>

        if slot == null:
            return null

        return slot.deref
    }


    fun argumentsCount() -> int = this.arguments.length


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        for (var i = 0; i < this.arguments.length; i++):
        {
            val argument: pointer<Expression> = this.getArgument(i)

            if argument == null:
                continue

            val tokens: pointer<ArrayList> = argument.getAllTokens()

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
        val sb: pointer<StringBuilder> = new StringBuilder("new ")

        sb.append(this.host)
        sb.append('(')

        var appendedArgument: bool = false

        for (var i = 0; i < this.arguments.length; i++):
        {
            val argument: pointer<Expression> = this.getArgument(i)

            if argument == null:
                continue

            if appendedArgument:
                sb.append(", ")

            sb.append(argument.toString())
            appendedArgument = true
        }

        sb.append(')')
        return sb
    }
}
