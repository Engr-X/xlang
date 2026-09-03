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
@file.class("QualifiedName")
package xlang.compiler.parser.program

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.String
import xlang.util.string.StringBuilder


struct QualifiedName
{
    private val parts: pointer<ArrayList>

    private var extraTokens: pointer<ArrayList>



    fun __init__(part: pointer<char>)
    {
        this.parts = new ArrayList(sizeof(pointer<char>))
        this.extraTokens = new ArrayList(sizeof(Token))
        this.push(part)
    }


    fun push(part: pointer<char>) -> pointer<QualifiedName>
    {
        val copied: pointer<char> = String.strdup(part)

        if copied != null:
            this.parts.push(copied.ref)

        return this
    }


    fun pushFront(part: pointer<char>) -> pointer<QualifiedName>
    {
        val copied: pointer<char> = String.strdup(part)

        if copied != null:
            this.parts.pushFront(copied.ref)

        return this
    }


    fun getPart(index: int) -> pointer<char>
    {
        if index < 0 || index >= this.parts.length:
            return null

        val slot: pointer<pointer<char>> = this.parts.get(index) as pointer<pointer<char>>

        if slot == null:
            return null

        return String.strdup(slot.deref)
    }


    fun addExtraToken(token: pointer<Token>) -> pointer<QualifiedName>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    fun toPackageDecl() -> pointer<PackageDeclaration>
    {
        val result: pointer<PackageDeclaration> = new PackageDeclaration(this.parts.clone())

        for (var i = 0; i < this.extraTokens.length; i++):
            result.addExtraToken(this.extraTokens.get(i) as pointer<Token>)

        return result
    }


    fun toImportDeclaration() -> pointer<ImportDeclaration>
    {
        val result: pointer<ImportDeclaration> = new ImportDeclaration(this.parts.clone())

        for (var i = 0; i < this.extraTokens.length; i++):
            result.addExtraToken(this.extraTokens.get(i) as pointer<Token>)

        return result
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
        val sb: pointer<StringBuilder> = new StringBuilder()
        var appendedPart: bool = false

        for (var i = 0; i < this.parts.length; i++):
        {
            val slot: pointer<pointer<char>> = this.parts.get(i) as pointer<pointer<char>>

            if slot == null || slot.deref == null:
                continue

            if appendedPart:
                sb.append('.')

            sb.append(slot.deref)
            appendedPart = true
        }

        return sb
    }
}
