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
 *
 */
package xlang.compiler.parser

import xlang.Operation
import xlang.util.ArrayList
import xlang.util.string.String
import xlang.util.string.StringBuilder


struct MethodCall
{
    // static fun fromCompare(exp1: pointer<Expression>)


    private var host: pointer<Expression>

    private var callName: pointer<char>
    
    private var arguments: pointer<ArrayList>


    fun __init__(host: pointer<Expression>, callName: pointer<char>)
    {
        this.host = host
        this.callName = String.strdup(callName)
        this.arguments = new ArrayList(sizeof(pointer<Expression>))
    }


    fun __init__(host: pointer<Expression>, op: pointer<Operation>)
    {
        this.host = host
        this.callName = op.getFunctionName()
        this.arguments = new ArrayList(sizeof(pointer<Expression>))
    }


    fun addArgument(argument: pointer<Expression>) -> pointer<MethodCall>
    {
        this.arguments.push(argument.ref)
        return this
    }


    fun setArguments(arguments: pointer<ExpressionTuple>) -> pointer<MethodCall>
    {
        this.arguments = arguments.getList()
        return this
    }
    
    
    fun getHost() -> pointer<Expression> = this.host


    fun getCallName() -> pointer<char> = String.strdup(this.callName)


    fun getArgument(index: int) -> pointer<Expression>
    {
        val slot: pointer<pointer<Expression>> = this.arguments.get(index) as pointer<pointer<Expression>>

        if slot == null:
            return null

        return slot.deref
    }


    fun argumentsCount() -> int = this.arguments.length


    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        if this.host != null:
        {
            sb.append(this.host.toString())
            sb.append('.')
        }

        sb.append(this.callName)
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