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
#file.class("Struct")
package xlang.compiler.parser.program

import xlang.compiler.Type
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.String
import xlang.util.string.StringBuilder


struct StructField
{
    private var modifiers: pointer<ModifierList>

    private var fieldName: pointer<char>

    private var fieldType: pointer<Type>

    private var extraTokens: pointer<ArrayList>


    constructor(fieldName: pointer<char>, fieldType: pointer<Type>)
    {
        this.modifiers = new ModifierList()
        this.fieldName = fieldName
        this.fieldType = fieldType
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    constructor(modifiers: pointer<ModifierList>, fieldName: pointer<char>, fieldType: pointer<Type>)
    {
        this.modifiers = if modifiers == null:
                new ModifierList()
            else:
                modifiers

        this.fieldName = fieldName
        this.fieldType = fieldType
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun getModifiers() -> pointer<ModifierList> = this.modifiers


    fun getFieldName() -> pointer<char> = this.fieldName


    fun getFieldType() -> pointer<Type> =
        if this.fieldType == null:
            null
        else:
            this.fieldType.clone()


    fun isStatic() -> bool
    {
        if this.modifiers == null:
            return false

        for (var i = 0; i < this.modifiers.length(); i++):
        {
            val modifier: pointer<Modifier> = this.modifiers.get(i)

            if modifier == null:
                continue

            val keyword: pointer<char> = modifier.getKeyword()

            if keyword != null && String.streq(keyword, "static"):
                return true
        }

        return false
    }


    fun addExtraToken(token: pointer<Token>) -> pointer<StructField>
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
            result.pushAll(this.modifiers.getAllTokens())

        if this.fieldType != null:
            result.pushAll(this.fieldType.getAllTokens())

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        if this.modifiers != null && this.modifiers.length() > 0:
        {
            sb.append(this.modifiers.toString())
            sb.append(' ')
        }

        if this.fieldName != null:
            sb.append(this.fieldName)

        if this.fieldType != null:
        {
            sb.append(": ")
            sb.append(this.fieldType.toString())
        }

        return sb
    }
}


struct Struct
{
}
