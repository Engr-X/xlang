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

#file.outerClass("Struct")
package xlang.compiler.parser.program

import xlang.compiler.parser.expression.Expression
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct StructConstructor
{
    private var modifiers: pointer<ArrayList>

    private var params: pointer<FunctionParams>

    private var bodyExpr: pointer<Expression>

    private var extraTokens: pointer<ArrayList>


    constructor(params: pointer<FunctionParams>, bodyExpr: pointer<Expression>)
    {
        this.modifiers = new ArrayList(sizeof(Modifier))
        this.params = if params == null:
                new FunctionParams()
            else:
                params

        this.bodyExpr = bodyExpr
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun getModifiers() -> pointer<ArrayList> = this.modifiers


    fun setModifiers(modifiers: pointer<ArrayList>) -> pointer<StructConstructor>
    {
        this.modifiers = if modifiers == null:
                new ArrayList(sizeof(Modifier))
            else:
                modifiers

        return this
    }


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
    private var annotations: pointer<ArrayList>

    private var modifiers: pointer<ArrayList>

    private var structName: pointer<char>

    private val members: pointer<ArrayList>

    private var extraTokens: pointer<ArrayList>


    constructor(structName: pointer<char>, members: pointer<ArrayList>)
    {
        this.annotations = new ArrayList(sizeof(Annotation))
        this.modifiers = new ArrayList(sizeof(Modifier))
        this.structName = structName
        this.members = if members == null:
                new ArrayList(sizeof(Member))
            else:
                members

        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun getAnnotations() -> pointer<ArrayList> = this.annotations


    fun setAnnotations(annotations: pointer<ArrayList>) -> pointer<Struct>
    {
        this.annotations = if annotations == null:
                new ArrayList(sizeof(Annotation))
            else:
                annotations

        return this
    }


    fun getModifiers() -> pointer<ArrayList> = this.modifiers


    fun setModifiers(modifiers: pointer<ArrayList>) -> pointer<Struct>
    {
        this.modifiers = if modifiers == null:
                new ArrayList(sizeof(Modifier))
            else:
                modifiers

        return this
    }


    fun getStructName() -> pointer<char> = this.structName


    fun addMember(member: pointer<Member>) -> pointer<Struct>
    {
        if member != null:
            this.members.push(member)

        return this
    }


    fun addMembers(members: pointer<ArrayList>) -> pointer<Struct>
    {
        if members != null:
            this.members.pushAll(members)

        return this
    }


    fun getMembers() -> pointer<ArrayList> = this.members.clone()


    fun addExtraToken(token: pointer<Token>) -> pointer<Struct>
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

        for (var i = 0; i < this.members.length; i++):
        {
            val member: pointer<Member> = this.members.get(i) as pointer<Member>

            if member == null:
                continue

            val tokens: pointer<ArrayList> = member.getAllTokens()

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

        sb.append("struct ")

        if this.structName != null:
            sb.append(this.structName)

        sb.append(" {")

        for (var i = 0; i < this.members.length; i++):
        {
            val member: pointer<Member> = this.members.get(i) as pointer<Member>

            if member == null:
                continue

            sb.newline()
            sb.append(member.toString())
        }

        sb.newline()
        sb.append('}')
        return sb
    }
}
