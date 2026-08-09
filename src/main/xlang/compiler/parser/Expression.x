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
 *
 *
 */

package xlang.compiler.parser

import xlang.compiler.Operation
import xlang.util.ArrayList
import xlang.util.string.String


struct Expression
{
    static val ATOM_KIND: int = 1
    static val STATEMENT_KIND: int = 2
    static val METHOD_CALL_KIND: int = 3
    static val FIELD_ACCESS_KIND: int = 4

    private var kind: int
    private var root: pointer<*>


    static fun fromAtom(atom: pointer<Atom>) -> pointer<Expression> = new Expression(ATOM_KIND, atom)

    static fun fromBinary(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val call: pointer<MethodCall> = new MethodCall(exp1, op).addArgument(exp2)
        return new Expression(METHOD_CALL_KIND, call)
    }

    static fun fromFieldAccess(host: pointer<Expression>, fieldName: pointer<char>) -> pointer<Expression>
    {
        val access: pointer<FieldAccess> = new FieldAccess(host, fieldName)
        return new Expression(FIELD_ACCESS_KIND, access)
    }

    private fun __init__(kind: int, root: pointer<*>)
    {
        this.kind = kind
        this.root = root
    }
}


struct FieldAccess
{
    private var host: pointer<Expression>
    private var fieldName: pointer<char>


    fun __init__(host: pointer<Expression>, fieldName: pointer<char>)
    {
        this.host = host
        this.fieldName = String.strdup(fieldName)
    }
}


struct MethodCall
{
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


    fun argumentsCount() -> int = this.arguments.length
}
