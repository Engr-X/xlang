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

#file.outerClass("Field")
package xlang.compiler.parser.program

import xlang.compiler.Type
import xlang.compiler.parser.expression.Expression
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.String
import xlang.util.string.StringBuilder


struct Field
{
    private static val CONST_MODIFIER: int = 0

    private static val MUT_MODIFIER: int = 1

    private var modifier: int

    private var annotations: pointer<Annotations>

    private var modifiers: pointer<ModifierList>

    private var fieldName: pointer<char>

    private var fieldType: pointer<Type>

    private var initialValue: pointer<Expression>

    private var extraTokens: pointer<ArrayList>


    static fun constModifier() -> int = CONST_MODIFIER


    static fun mutModifier() -> int = MUT_MODIFIER


    constructor(fieldName: pointer<char>, fieldType: pointer<Type>)
    {
        this.modifier = CONST_MODIFIER
        this.annotations = new Annotations()
        this.modifiers = new ModifierList()
        this.fieldName = fieldName
        this.fieldType = fieldType
        this.initialValue = null
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun getAnnotations() -> pointer<Annotations> = this.annotations


    fun setAnnotations(annotations: pointer<Annotations>) -> pointer<Field>
    {
        this.annotations = if annotations == null:
                new Annotations()
            else:
                annotations

        return this
    }


    fun getModifiers() -> pointer<ModifierList> = this.modifiers


    fun setModifiers(modifiers: pointer<ModifierList>) -> pointer<Field>
    {
        this.modifiers = if modifiers == null:
                new ModifierList()
            else:
                modifiers

        return this
    }


    fun getFieldName() -> pointer<char> = this.fieldName


    fun getFieldType() -> pointer<Type> =
        if this.fieldType == null:
            null
        else:
            this.fieldType.clone()


    fun getInitialValue() -> pointer<Expression> = this.initialValue


    fun markAsMut() -> pointer<Field>
    {
        this.modifier = MUT_MODIFIER
        return this
    }


    fun markAsConst() -> pointer<Field>
    {
        this.modifier = CONST_MODIFIER
        return this
    }


    fun canModified() -> bool = this.modifier == MUT_MODIFIER


    fun setInitialValue(value: pointer<Expression>) -> pointer<Field>
    {
        this.initialValue = value
        return this
    }


    fun addExtraToken(token: pointer<Token>) -> pointer<Field>
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

        if this.fieldType != null:
            result.pushAll(this.fieldType.getAllTokens())

        if this.initialValue != null:
            result.pushAll(this.initialValue.getAllTokens())

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

        if this.canModified():
            sb.append("var ")
        else:
            sb.append("val ")

        if this.fieldName != null:
            sb.append(this.fieldName)

        if this.fieldType != null:
        {
            sb.append(": ")
            sb.append(this.fieldType.toString())
        }

        if this.initialValue != null:
        {
            sb.append(" = ")
            sb.append(this.initialValue.toString())
        }

        return sb
    }
}
