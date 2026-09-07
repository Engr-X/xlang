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
#file.class("ImportDeclarations")
package xlang.compiler.parser.program

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct ImportDeclarations
{
    private val qualifiedNames: pointer<ArrayList>

    private var extraTokens: pointer<ArrayList>


    fun __init__()
    {
        this.qualifiedNames = new ArrayList(sizeof(QualifiedName))
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun __init__(qualifiedName: pointer<QualifiedName>)
    {
        this.qualifiedNames = new ArrayList(sizeof(QualifiedName))
        this.extraTokens = new ArrayList(sizeof(Token))
        this.push(qualifiedName)
    }


    fun push(qualifiedName: pointer<QualifiedName>) -> pointer<ImportDeclarations>
    {
        if qualifiedName != null:
            this.qualifiedNames.push(qualifiedName)

        return this
    }


    fun push(imports: pointer<ImportDeclarations>) -> pointer<ImportDeclarations>
    {
        if imports != null && imports.qualifiedNames != null:
        {
            this.qualifiedNames.pushAll(imports.qualifiedNames)

            if imports.extraTokens != null:
                this.extraTokens.pushAll(imports.extraTokens)
        }

        return this
    }


    fun pushImport(importDeclaration: pointer<ImportDeclaration>) -> pointer<ImportDeclarations>
    {
        if importDeclaration == null:
            return this

        val parts: pointer<ArrayList> = importDeclaration.getQualifiedName()

        if parts != null && parts.length > 0:
        {
            val firstSlot: pointer<pointer<char>> = parts.get(0) as pointer<pointer<char>>

            if firstSlot != null && firstSlot.deref != null:
            {
                val qualifiedName: pointer<QualifiedName> = new QualifiedName(firstSlot.deref)

                for (var i = 1; i < parts.length; i++):
                {
                    val partSlot: pointer<pointer<char>> = parts.get(i) as pointer<pointer<char>>

                    if partSlot != null && partSlot.deref != null:
                        qualifiedName.push(partSlot.deref)
                }

                this.push(qualifiedName)
            }
        }

        val tokens: pointer<ArrayList> = importDeclaration.getExtraTokens()

        if tokens != null:
            this.extraTokens.pushAll(tokens)

        return this
    }


    fun length() -> int = this.qualifiedNames.length


    fun get(index: int) -> pointer<QualifiedName>
    {
        if index < 0 || index >= this.qualifiedNames.length:
            return null

        return this.qualifiedNames.get(index) as pointer<QualifiedName>
    }


    fun getQualifiedNames() -> pointer<ArrayList> = this.qualifiedNames.clone()


    fun getExtraTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        for (var i = 0; i < this.qualifiedNames.length; i++):
        {
            val qualifiedName: pointer<QualifiedName> = this.get(i)

            if qualifiedName == null:
                continue

            val tokens: pointer<ArrayList> = qualifiedName.getExtraTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        for (var i = 0; i < this.qualifiedNames.length; i++):
        {
            val qualifiedName: pointer<QualifiedName> = this.get(i)

            if qualifiedName == null:
                continue

            val tokens: pointer<ArrayList> = qualifiedName.getAllTokens()

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
        var appendedImport: bool = false

        for (var i = 0; i < this.qualifiedNames.length; i++):
        {
            val qualifiedName: pointer<QualifiedName> = this.get(i)

            if qualifiedName == null:
                continue

            if appendedImport:
                sb.newline()

            sb.append("import ")
            sb.append(qualifiedName.toString())
            appendedImport = true
        }

        return sb
    }
}
