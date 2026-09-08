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
#file.class("ImportDeclaration")
package xlang.compiler.parser.program

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct ImportDeclaration
{
    private var qualifiedName: pointer<QualifiedName>

    private var extraTokens: pointer<ArrayList>


    fun __init__(qualifiedName: pointer<QualifiedName>)
    {
        this.qualifiedName = qualifiedName
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun getQualifiedName() -> pointer<QualifiedName> = this.qualifiedName


    fun addExtraToken(token: pointer<Token>) -> pointer<ImportDeclaration>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.qualifiedName != null:
            result.pushAll(this.qualifiedName.getAllTokens())

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder("import ")

        if this.qualifiedName != null:
            sb.append(this.qualifiedName.toString())

        return sb
    }
}
