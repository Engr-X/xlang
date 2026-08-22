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
import xlang.util.string.StringBuilder


struct Expression
{
    static val ATOM_KIND: int = 1
    static val STATEMENT_KIND: int = 2
    static val FIELD_ACCESS_KIND: int = 3
    static val METHOD_CALL_KIND: int = 4
    static val INDEX_ACCESS_KIND: int = 5
    static val TYPE_CAST_KIND: int = 6


    static fun fromAtom(atom: pointer<Atom>) -> pointer<Expression> = new Expression(ATOM_KIND, atom)


    inline static fun fromBinary(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val call: pointer<MethodCall> = new MethodCall(null, op).addArgument(exp1).addArgument(exp2)
        return new Expression(METHOD_CALL_KIND, call)
    }


    inline static fun fromPrefix(op: pointer<Operation>, exp: pointer<Expression>) -> pointer<Expression>
    {
        val call: pointer<MethodCall> = new MethodCall(null, op).addArgument(exp)
        return new Expression(METHOD_CALL_KIND, call)
    }


    inline static fun fromPostfix(op: pointer<Operation>, exp: pointer<Expression>) -> pointer<Expression>
    {
        val call: pointer<MethodCall> = new MethodCall(null, op).addArgument(exp)
        return new Expression(METHOD_CALL_KIND, call)
    }


    inline static fun fromFieldAccess(host: pointer<Expression>, fieldName: pointer<char>) -> pointer<Expression>
    {
        val access: pointer<FieldAccess> = new FieldAccess(host, fieldName)
        return new Expression(FIELD_ACCESS_KIND, access)
    }


    inline static fun fromMethodCall(method: pointer<MethodCall>) -> pointer<Expression> = new Expression(METHOD_CALL_KIND, method) 


    inline static fun fromIndexAccess(access: pointer<IndexAccess>) -> pointer<Expression> = new Expression(INDEX_ACCESS_KIND, access)


    inline static fun fromTypeCast(castTo: pointer<TypeCast>) -> pointer<Expression> = new Expression(TYPE_CAST_KIND, castTo)


    private var kind: int

    private var root: pointer<*>


    private fun __init__(kind: int, root: pointer<*>)
    {
        this.kind = kind
        this.root = root
    }


    fun getKind() -> int = this.kind


    fun getRoot() -> pointer<*> = this.root


    fun toString() -> pointer<StringBuilder>
    {
        if this.kind == ATOM_KIND:
        {
            val atom: pointer<Atom> = this.root as pointer<Atom>
            return atom.toString()
        }

        if this.kind == METHOD_CALL_KIND:
        {
            val call: pointer<MethodCall> = this.root as pointer<MethodCall>
            return call.toString()
        }

        if this.kind == FIELD_ACCESS_KIND:
        {
            val access: pointer<FieldAccess> = this.root as pointer<FieldAccess>
            return access.toString()
        }

        if this.kind == INDEX_ACCESS_KIND:
        {
            val access: pointer<IndexAccess> = this.root as pointer<IndexAccess>
            return access.toString()
        }

        if this.kind == TYPE_CAST_KIND:
        {
            val cast: pointer<TypeCast> = this.root as pointer<TypeCast>
            return cast.toString()
        }

        return new StringBuilder()
    }
}