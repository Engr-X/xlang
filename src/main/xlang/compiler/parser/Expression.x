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

import xlang.Operation
import xlang.compiler.Type
import xlang.parser.ParseContainer
import xlang.util.ArrayList
import xlang.util.string.String
import xlang.util.string.StringBuilder


struct SExpression
{
    static fun unwrap(sExpressions: pointer<ArrayList>) -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(pointer<Expression>))

        if sExpressions == null:
            return result

        for (var i = 0; i < sExpressions.length; i++):
        {
            val slot: pointer<pointer<ParseContainer>> = sExpressions.get(i) as pointer<pointer<ParseContainer>>

            if slot == null || slot.deref == null:
                continue

            val container: pointer<ParseContainer> = slot.deref
            val sExpression: pointer<SExpression> = container.getValue() as pointer<SExpression>

            if sExpression == null:
                continue

            val expression: pointer<Expression> = sExpression.unwrap()

            if expression != null:
                result.push(expression.ref)
        }

        return result
    }


    private var expression: pointer<Expression>


    fun __init__(expression: pointer<Expression>):
        this.expression = expression


    fun unwrap() -> pointer<Expression> = this.expression
}


struct ExpressionTuple
{
    private var list: pointer<ArrayList>


    fun __init__():
        this.list = new ArrayList(sizeof(pointer<Expression>))


    fun __init__(list: pointer<ArrayList>)
    {
        this.list = list
    }


    fun addExpression(expression: pointer<Expression>) -> pointer<ExpressionTuple>
    {
        this.list.push(expression.ref)
        return this
    }


    fun getList() -> pointer<ArrayList> = this.list
}


struct ListLiteral
{
    private var list: pointer<ArrayList>


    fun __init__():
        this.list = new ArrayList(sizeof(pointer<Expression>))


    fun __init__(list: pointer<ArrayList>)
    {
        this.list = list
    }


    fun addExpression(expression: pointer<Expression>) -> pointer<ListLiteral>
    {
        this.list.push(expression.ref)
        return this
    }


    fun getList() -> pointer<ArrayList> = this.list
}


struct Expression
{
    static val ATOM_KIND: int = 1
    static val STATEMENT_KIND: int = 2
    static val FIELD_ACCESS_KIND: int = 3
    static val METHOD_CALL_KIND: int = 4
    static val INDEX_ACCESS_KIND: int = 5
    static val TYPE_CAST_KIND: int = 6

    private var kind: int
    private var root: pointer<*>


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


struct TypeCast
{
    private var expression: pointer<Expression>

    private var targetType: pointer<Type>


    fun __init__(expression: pointer<Expression>, targetType: pointer<Type>)
    {
        this.expression = expression
        this.targetType = targetType
    }


    fun getExpression() -> pointer<Expression> = this.expression


    fun getTargetType() -> pointer<Type>
    {
        if this.targetType == null:
            return null

        return this.targetType.copy()
    }


    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        sb.append('(')

        if this.targetType != null:
            sb.append(this.targetType.getTypeName())

        sb.append(")(")

        if this.expression != null:
            sb.append(this.expression.toString())

        sb.append(')')

        return sb
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


    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        if this.host != null:
        {
            sb.append(this.host.toString())
            sb.append('.')
        }

        sb.append(this.fieldName)
        return sb
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


struct IndexAccess
{
    private var host: pointer<Expression>

    private var indices: pointer<ArrayList>


    fun __init__(host: pointer<Expression>, indices: pointer<ListLiteral>)
    {
        this.host = host
        this.indices = indices.getList()
    }


    fun addIndex(index: pointer<Expression>) -> pointer<IndexAccess>
    {
        this.indices.push(index.ref)
        return this
    }


    fun getHost() -> pointer<Expression> = this.host


    fun getIndex(index: int) -> pointer<Expression>
    {
        val slot: pointer<pointer<Expression>> = this.indices.get(index) as pointer<pointer<Expression>>

        if slot == null:
            return null

        return slot.deref
    }


    fun indicesCount() -> int = this.indices.length


    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        sb.append("IndexAccess(")

        if this.host != null:
            sb.append(this.host.toString())

        sb.append(", [")

        var appendedIndex: bool = false

        for (var i = 0; i < this.indices.length; i++):
        {
            val index: pointer<Expression> = this.getIndex(i)

            if index == null:
                continue

            if appendedIndex:
                sb.append(", ")

            sb.append(index.toString())
            appendedIndex = true
        }

        sb.append("])")
        return sb
    }
}
