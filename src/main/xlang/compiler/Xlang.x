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
 *
 *
 */

#file.outerClass("Xlang")

package xlang.compiler

import xlang.System
import xlang.compiler.lexer.Tokenizer
import xlang.compiler.parser.ASTNormalizer
import xlang.compiler.parser.NormalizedProgram
import xlang.compiler.parser.program.ImportDeclaration
import xlang.compiler.parser.program.Member
import xlang.compiler.parser.program.NamespaceImport
import xlang.compiler.parser.program.PackageDeclaration
import xlang.compiler.parser.program.Program
import xlang.compiler.parser.program.QualifiedName
import xlang.compiler.parser.program.SelectiveImports
import xlang.compiler.parser.program.Struct
import xlang.compiler.parser.expression.Expression
import xlang.compiler.parser.Parser
import xlang.compiler.parser.statement.Statement
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.util.ArrayList
import xlang.util.HashMap
import xlang.util.IO
import xlang.util.MapEntry
import xlang.util.string.StringBuilder


fun main()
{
    normalizeProgramDemo()
}


private fun printBuilder(builder: pointer<StringBuilder>)
{
    if builder == null:
        return

    val output: pointer<char> = System.allocMemory((builder.length + 1) * sizeof(char)) as pointer<char>
    builder.toString(output)
    IO.println(output)
}


private fun appendImport(builder: pointer<StringBuilder>, importDecl: pointer<ImportDeclaration>)
{
    if builder == null || importDecl == null:
        return

    if importDecl.getKind() == ImportDeclaration.NAMESPACE_TYPE:
        builder.append(importDecl.toString())
    elif importDecl.getKind() == ImportDeclaration.SELECTIVE_TYPE:
    {
        val selectiveImports: pointer<SelectiveImports> = importDecl.getHost() as pointer<SelectiveImports>

        if selectiveImports != null && selectiveImports.getQualifiedName() != null:
        {
            builder.append("from ")
            builder.append(selectiveImports.getQualifiedName().toString())
            builder.append(" import *")
        }
    }
}


private fun printNormalizedProgram(program: pointer<NormalizedProgram>)
{
    if program == null:
        return

    val builder: pointer<StringBuilder> = new StringBuilder()

    if program.getPackageDeclaration() != null:
    {
        builder.append(program.getPackageDeclaration().toString())
        builder.newline()
        builder.newline()
    }

    val imports: pointer<ArrayList> = program.getImports()

    for (var i = 0; i < imports.length; i++):
    {
        appendImport(builder, imports.get(i) as pointer<ImportDeclaration>)
        builder.newline()
    }

    if imports.length > 0:
        builder.newline()

    if program.getBodyType() == NormalizedProgram.STRUCT_TYPE:
    {
        val structBody: pointer<Struct> = program.getHost() as pointer<Struct>

        if structBody != null:
            builder.append(structBody.toString())
    }

    printBuilder(builder)
    IO.println("")
}


private fun normalizeProgramDemo()
{
    val innerStruct: pointer<Struct> = new Struct("B", null)
    val outerStruct: pointer<Struct> = new Struct("A", null)
    outerStruct.addMember(Member.fromStruct(innerStruct))

    val members: pointer<ArrayList> = new ArrayList(sizeof(Member))
    members.push(Member.fromStruct(outerStruct))

    val program: pointer<Program> = new Program(members)
    program.setPackageDeclaration(new QualifiedName("some").toPackageDecl())

    val importPath: pointer<QualifiedName> = new QualifiedName("xlang")
    importPath.push("util")
    importPath.push("ArrayList")

    val sourceImports: pointer<ArrayList> = new ArrayList(sizeof(ImportDeclaration))
    sourceImports.push(ImportDeclaration.fromNamespace(NamespaceImport.fromSingle(importPath)))
    program.setImportDeclarations(sourceImports)

    val programs: pointer<ArrayList> = new ArrayList(sizeof(Program))
    val extraImports: pointer<ArrayList> = new ArrayList(sizeof(ImportDeclaration))
    programs.push(program)

    val normalized: pointer<HashMap> = ASTNormalizer.normalizeProgram(programs, new Imports(), extraImports)

    if normalized == null:
    {
        IO.println("normalizeProgram failed")
        return
    }

    val entries: pointer<ArrayList> = normalized.getEntries()

    for (var i = 0; i < entries.length; i++):
    {
        val entry: pointer<MapEntry> = entries.get(i) as pointer<MapEntry>

        if entry == null:
            continue

        val normalizedPrograms: pointer<ArrayList> = entry.value as pointer<ArrayList>

        if normalizedPrograms == null:
            continue

        for (var j = 0; j < normalizedPrograms.length; j++):
            printNormalizedProgram(normalizedPrograms.get(j) as pointer<NormalizedProgram>)
    }
}


private fun skipLineTerminators(tokens: pointer<TokenList>)
{
    while tokens != null && tokens.length() > 0:
    {
        val token: pointer<Token> = tokens.get(0)

        if token == null || token.kind != Tokenizer.TK_LINE_TERMINATOR:
            return

        tokens.remove(0, 1)
    }
}


fun parseExpressionLoop()
{
    val inputSpace: blob[1024]
    val input: pointer<char> = inputSpace as pointer<char>

    while true:
    {
        val length: int = IO.readLine(input, 1024)

        if length < 0:
            return

        if length == 0:
            continue

        val tokens: pointer<TokenList> = Tokenizer.fullTokenize(input)
        val expression: pointer<Expression> = Parser.parseExpression(tokens)

        if expression == null:
        {
            IO.println("failed to parse expression")
            continue
        }

        // skipLineTerminators(tokens)

        if tokens.length() > 0 && !tokens.get(0).isEOF():
        {
            val unexpected: pointer<Token> = tokens.get(0)
            val builder: pointer<StringBuilder> = new StringBuilder("failed to parse expression: unexpected token ")

            if unexpected != null && unexpected.text != null:
                builder.append(unexpected.text)
            else:
                builder.append("<unknown>")

            val output: pointer<char> = System.allocMemory((builder.length + 1) * sizeof(char)) as pointer<char>

            builder.toString(output)
            IO.println(output)
            continue
        }

        val builder: pointer<StringBuilder> = expression.toString()
        val output: pointer<char> = System.allocMemory((builder.length + 1) * sizeof(char)) as pointer<char>

        builder.toString(output)
        IO.println(output)
    }
}


fun parseStatementLoop()
{
    val inputSpace: blob[1024]
    val input: pointer<char> = inputSpace as pointer<char>

    while true:
    {
        val length: int = IO.readLine(input, 1024)

        if length < 0:
            return

        if length == 0:
            continue

        if length + 1 >= 1024:
        {
            IO.println("failed to parse statement: input too long")
            continue
        }

        input[length] = '\n'
        input[length + 1] = 0

        val tokens: pointer<TokenList> = Tokenizer.fullTokenize(input)
        val statement: pointer<Statement> = Parser.parseStatement(tokens)

        if statement == null:
        {
            IO.println("failed to parse statement")
            continue
        }

        // skipLineTerminators(tokens)

        if tokens.length() > 0 && !tokens.get(0).isEOF():
        {
            val unexpected: pointer<Token> = tokens.get(0)
            val builder: pointer<StringBuilder> = new StringBuilder("failed to parse statement: unexpected token ")

            if unexpected != null && unexpected.text != null:
                builder.append(unexpected.text)
            else:
                builder.append("<unknown>")

            val output: pointer<char> = System.allocMemory((builder.length + 1) * sizeof(char)) as pointer<char>

            builder.toString(output)
            IO.println(output)
            continue
        }

        val builder: pointer<StringBuilder> = statement.toString()
        val output: pointer<char> = System.allocMemory((builder.length + 1) * sizeof(char)) as pointer<char>

        builder.toString(output)
        IO.println(output)
    }
}
