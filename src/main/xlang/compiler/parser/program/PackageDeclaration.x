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
@file.class("PackageDeclaration")
package xlang.compiler.parser.program

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct PackageDeclaration
{
    private var qualifiedName: pointer<ArrayList>

    private var extraTokens: pointer<ArrayList>


    fun __init__(qualifiedName: pointer<ArrayList>)
    {
        this.qualifiedName = if qualifiedName == null:
            new ArrayList(sizeof(pointer<char>))
        else:
            qualifiedName

        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun getQualifiedName() -> pointer<ArrayList> = this.qualifiedName.clone()


    fun addExtraToken(token: pointer<Token>) -> pointer<PackageDeclaration>
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
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder("package ")

        for (var i = 0; i < this.qualifiedName.length; i++):
        {
            val slot: pointer<pointer<char>> = this.qualifiedName.get(i) as pointer<pointer<char>>

            if slot == null || slot.deref == null:
                continue

            if i > 0:
                sb.append('.')

            sb.append(slot.deref)
        }

        return sb
    }
}
