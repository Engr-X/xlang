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

#file.outerClass("Program")
package xlang.compiler.parser.program

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct Program
{
    private var preprocessSettings: pointer<PreprocessSettings>

    private var packageDeclaration: pointer<PackageDeclaration>

    private var importDeclarations: pointer<ImportDeclarations>

    private val members: pointer<ArrayList>

    private var extraTokens: pointer<ArrayList>


    constructor(members: pointer<ArrayList>)
    {
        this.preprocessSettings = new PreprocessSettings()
        this.packageDeclaration = null
        this.importDeclarations = new ImportDeclarations()
        this.members = if members == null:
                new ArrayList(sizeof(Member))
            else:
                members

        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun getPreprocessSettings() -> pointer<PreprocessSettings> = this.preprocessSettings


    fun setPreprocessSettings(settings: pointer<PreprocessSettings>) -> pointer<Program>
    {
        this.preprocessSettings = if settings == null:
                new PreprocessSettings()
            else:
                settings

        return this
    }


    fun getPackageDeclaration() -> pointer<PackageDeclaration> = this.packageDeclaration


    fun setPackageDeclaration(packageDeclaration: pointer<PackageDeclaration>) -> pointer<Program>
    {
        this.packageDeclaration = packageDeclaration
        return this
    }


    fun getImportDeclarations() -> pointer<ImportDeclarations> = this.importDeclarations


    fun setImportDeclarations(imports: pointer<ImportDeclarations>) -> pointer<Program>
    {
        this.importDeclarations = if imports == null:
                new ImportDeclarations()
            else:
                imports

        return this
    }


    fun addMember(member: pointer<Member>) -> pointer<Program>
    {
        if member != null:
            this.members.push(member)

        return this
    }


    fun addMembers(members: pointer<ArrayList>) -> pointer<Program>
    {
        if members != null:
            this.members.pushAll(members)

        return this
    }


    fun length() -> int = this.members.length


    fun get(index: int) -> pointer<Member>
    {
        if index < 0 || index >= this.members.length:
            return null

        return this.members.get(index) as pointer<Member>
    }


    fun getMembers() -> pointer<ArrayList> = this.members.clone()


    fun addExtraToken(token: pointer<Token>) -> pointer<Program>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.preprocessSettings != null:
            result.pushAll(this.preprocessSettings.getAllTokens())

        if this.packageDeclaration != null:
            result.pushAll(this.packageDeclaration.getAllTokens())

        if this.importDeclarations != null:
            result.pushAll(this.importDeclarations.getAllTokens())

        for (var i = 0; i < this.members.length; i++):
        {
            val member: pointer<Member> = this.get(i)

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
        var appendedSection: bool = false

        if this.preprocessSettings != null && this.preprocessSettings.length() > 0:
        {
            sb.append(this.preprocessSettings.toString())
            appendedSection = true
        }

        if this.packageDeclaration != null:
        {
            if appendedSection:
                sb.newline()

            sb.append(this.packageDeclaration.toString())
            appendedSection = true
        }

        if this.importDeclarations != null && this.importDeclarations.length() > 0:
        {
            if appendedSection:
                sb.newline()

            sb.append(this.importDeclarations.toString())
            appendedSection = true
        }

        for (var i = 0; i < this.members.length; i++):
        {
            val member: pointer<Member> = this.get(i)

            if member == null:
                continue

            if appendedSection:
                sb.newline()

            sb.append(member.toString())
            appendedSection = true
        }

        return sb
    }
}
