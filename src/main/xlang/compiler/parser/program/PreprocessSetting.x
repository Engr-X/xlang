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
#file.class("PreprocessSetting")
package xlang.compiler.parser.program

import xlang.compiler.parser.expression.Atom
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct PreprocessSetting
{
    private var name: pointer<QualifiedName>

    private var value: pointer<ArrayList>

    private var extraTokens: pointer<ArrayList>


    fun __init__(name: pointer<QualifiedName>, value: pointer<ArrayList>)
    {
        this.name = name
        this.value = if value == null: new ArrayList(sizeof(pointer<Atom>)) else: value
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun getName() -> pointer<QualifiedName> = this.name


    fun getValue() -> pointer<ArrayList> = this.value.clone()


    fun addExtraToken(token: pointer<Token>) -> pointer<PreprocessSetting>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        result.pushAll(this.extraTokens)

        if this.name != null:
            result.pushAll(this.name.getAllTokens())

        for (var i = 0; i < this.value.length; i++):
        {
            val slot: pointer<pointer<Atom>> = this.value.get(i) as pointer<pointer<Atom>>

            if slot == null || slot.deref == null:
                continue

            val atom: pointer<Atom> = slot.deref
            result.pushAll(atom.getAllTokens())
        }

        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder("#")

        if this.name != null:
            sb.append(this.name.toString())

        sb.append('(')
        var appendedAtom: bool = false

        for (var i = 0; i < this.value.length; i++):
        {
            val slot: pointer<pointer<Atom>> = this.value.get(i) as pointer<pointer<Atom>>

            if slot == null || slot.deref == null:
                continue

            if appendedAtom:
                sb.append(", ")

            val atom: pointer<Atom> = slot.deref
            sb.append(atom.toString())
            appendedAtom = true
        }

        sb.append(')')
        return sb
    }
}
