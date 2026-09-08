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


    fun push(importDeclaration: pointer<ImportDeclaration>) -> pointer<ImportDeclarations>
    {
        if importDeclaration != null:
            this.push(importDeclaration.getQualifiedName())

        return this
    }


    fun pushAll(imports: pointer<ImportDeclarations>) -> pointer<ImportDeclarations>
    {
        if imports != null && imports.qualifiedNames != null:
        {
            this.qualifiedNames.pushAll(imports.qualifiedNames)

            if imports.extraTokens != null:
                this.extraTokens.pushAll(imports.extraTokens)
        }

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


struct ImportDeclarationsMaybe
{
    private var imports: pointer<ImportDeclarations>


    fun __init__(imports: pointer<ImportDeclarations>):
        this.imports = if imports == null:
                new ImportDeclarations()
            else:
                imports


    fun toImportDeclarations() -> pointer<ImportDeclarations> = this.imports
}
