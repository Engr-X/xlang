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

#file.outerClass("ASTNormalizer")
package xlang.compiler.parser

import xlang.compiler.setting.CompilerSettings
import xlang.compiler.parser.program.Field
import xlang.compiler.parser.program.Function
import xlang.compiler.parser.program.ImportDeclaration
import xlang.compiler.parser.program.Member
import xlang.compiler.parser.program.Modifier
import xlang.compiler.parser.program.NamespaceImport
import xlang.compiler.parser.program.PackageDeclaration
import xlang.compiler.parser.program.PreprocessSetting
import xlang.compiler.parser.program.Program
import xlang.compiler.parser.program.QualifiedName
import xlang.compiler.parser.program.SelectiveImports
import xlang.compiler.parser.program.Struct
import xlang.util.ArrayList
import xlang.util.HashMap
import xlang.util.MapEntry
import xlang.util.string.String


private fun initPreprocessSettings(path: pointer<char>, preprocessSettings: pointer<ArrayList>) -> pointer<CompilerSettings>
{
    val settings: pointer<CompilerSettings> = new CompilerSettings(path)

    for (var i = 0; i < preprocessSettings.length; i++):
    {
        var config: pointer<PreprocessSetting> = preprocessSettings.get(i) as pointer<PreprocessSetting>
        settings.set(config)
    }

    return settings
}


private fun getStructs(members: pointer<ArrayList>) -> pointer<ArrayList>
{
    val result: pointer<ArrayList> = new ArrayList(sizeof(Member))

    for (var i = 0; i < members.length; i++):
    {
        val member: pointer<Member> = members.get(i) as pointer<Member>

        if member.isStruct():
            result.push(member)
    }

    return result
}


private fun getFunField(members: pointer<ArrayList>) -> pointer<ArrayList>
{
    val result: pointer<ArrayList> = new ArrayList(sizeof(Member))

    for (var i = 0; i < members.length; i++):
    {
        val member: pointer<Member> = members.get(i) as pointer<Member>

        if member.isFunction() || member.isField():
            result.push(member)

    }

    return result
}


private fun splitProgram(program: pointer<Program>) -> pointer<ArrayList>
{
    val result: pointer<ArrayList> = new ArrayList(sizeof(NormalizedProgram))

    if program == null:
        return result

    val members: pointer<ArrayList> = program.getMembers()
    val structs: pointer<ArrayList> = getStructs(members)
    val fieldFunction: pointer<ArrayList> = getFunField(members)

    val preprocessSettings: pointer<ArrayList> = program.getPreprocessSettings()
    val config: pointer<CompilerSettings> = initPreprocessSettings(null, preprocessSettings)
    val packageDeclaration: pointer<PackageDeclaration> = program.getPackageDeclaration()
    val imports: pointer<ArrayList> = program.getImportDeclarations()


    // other program (struct)
    val extraImportPath: pointer<QualifiedName> = new QualifiedName(packageDeclaration.getQualifiedName())
    extraImportPath.push(config.getOuterClass())
    
    val extraImport: pointer<ImportDeclaration> = ImportDeclaration.fromSelective(SelectiveImports.fromAll(extraImportPath)) 

    for (var i = 0; i < structs.length; i++):
    {
        var structItem: pointer<Struct> = structs.get(i) as pointer<Struct>
        val structImports: pointer<ArrayList> = imports.clone()

        structImports.push(extraImport)

        var nProgram: pointer<NormalizedProgram> = NormalizedProgram.fromStruct(
            preprocessSettings, packageDeclaration, structImports, structItem)

        result.push(nProgram)
    }


    // exposed field and function
    for (var i = 0; i < fieldFunction.length; i++):
    {
        val member: pointer<Member> = fieldFunction.get(i) as pointer<Member>

        if member == null:
            continue

        if member.isField():
        {
            val field: pointer<Field> = member.getHost() as pointer<Field>

            if field != null:
                field.addModifier(Modifier.fromStatic())
        }
        elif member.isFunction():
        {
            val function: pointer<Function> = member.getHost() as pointer<Function>

            if function != null:
                function.addModifier(Modifier.fromStatic())
        }
    }

    val warpedClass: pointer<Struct> = new Struct(config.getOuterClass(), null)
    warpedClass.addMembers(fieldFunction)

    val warpedProgram: pointer<NormalizedProgram> = NormalizedProgram.fromStruct(
        preprocessSettings, packageDeclaration, imports, warpedClass)
    result.push(warpedProgram)

    return result
}


// hashmap of ArrayList<String>, ArrayList<NormalizedProgram>
private fun classifyProgram(program: pointer<NormalizedProgram>, dest: pointer<HashMap>)
{
    if program == null || dest == null:
        return

    val packageDeclaration: pointer<PackageDeclaration> = program.getPackageDeclaration()
    val packageName: pointer<ArrayList> = if packageDeclaration == null:
            new ArrayList(sizeof(pointer<char>))
        else:
            packageDeclaration.getQualifiedName()

    if dest.containsKey(packageName):
    {
        val programs: pointer<ArrayList> = dest.get(packageName) as pointer<ArrayList>

        if programs != null:
            programs.push(program)

        return
    }

    val programs: pointer<ArrayList> = new ArrayList(sizeof(NormalizedProgram))
    programs.push(program)
    dest.put(packageName, programs)
}


private fun stringListCmp(left: pointer<*>, right: pointer<*>) -> int
{
    val lhs: pointer<ArrayList> = left as pointer<ArrayList>
    val rhs: pointer<ArrayList> = right as pointer<ArrayList>

    if lhs == rhs:
        return 0

    if lhs == null:
        return -1

    if rhs == null:
        return 1

    val length: int = if lhs.length < rhs.length:
            lhs.length
        else:
            rhs.length

    for (var i: int = 0; i < length; i++):
    {
        val lhsSlot: pointer<pointer<char>> =
            lhs.get(i) as pointer<pointer<char>>

        val rhsSlot: pointer<pointer<char>> =
            rhs.get(i) as pointer<pointer<char>>

        val lhsStr: pointer<char> = lhsSlot.deref
        val rhsStr: pointer<char> = rhsSlot.deref

        if lhsStr == rhsStr:
            continue

        if lhsStr == null:
            return -1

        if rhsStr == null:
            return 1

        var j: int = 0

        while lhsStr[j] != String.NULL_CHAR && rhsStr[j] != String.NULL_CHAR:
        {
            if lhsStr[j] != rhsStr[j]:
                return (lhsStr[j] as int) - (rhsStr[j] as int)

            j++
        }

        if lhsStr[j] != rhsStr[j]:
            return (lhsStr[j] as int) - (rhsStr[j] as int)
    }

    return lhs.length - rhs.length
}


private fun stringListHash(item: pointer<*>) -> int
{
    val list: pointer<ArrayList> = item as pointer<ArrayList>

    if list == null:
        return 0

    var hash: int = 1

    for (var i: int = 0; i < list.length; i++):
    {
        val strSlot: pointer<pointer<char>> =
            list.get(i) as pointer<pointer<char>>

        val str: pointer<char> =
            if strSlot == null: null else: strSlot.deref

        hash = hash * 31 + if str == null:
                0
            else:
                String.strHash(str)
    }

    return hash
}


private fun classifyPrograsm(programs: pointer<ArrayList>) -> pointer<HashMap>
{
    val result: pointer<HashMap> = new HashMap(stringListCmp, stringListHash)

    if programs == null:
        return result

    for (var i = 0; i < programs.length; i++):
    {
        val program: pointer<NormalizedProgram> = programs.get(i) as pointer<NormalizedProgram>
        classifyProgram(program, result)
    }

    return result
}


private fun addAllPackagesImport(nPrograms: pointer<ArrayList>) -> pointer<ArrayList>
{
    val result: pointer<ArrayList> = new ArrayList(sizeof(ImportDeclaration))

    if nPrograms == null:
        return result

    for (var i = 0; i < nPrograms.length; i++):
    {
        val nProgram: pointer<NormalizedProgram> = nPrograms.get(i) as pointer<NormalizedProgram>

        if nProgram == null:
            continue

        val fullPath: pointer<ArrayList> = nProgram.getFullpath()

        if fullPath == null || fullPath.length <= 0:
            continue

        val qualifiedName: pointer<QualifiedName> = new QualifiedName(fullPath)
        val importDecl: pointer<ImportDeclaration> = ImportDeclaration.fromNamespace(
            NamespaceImport.fromSingle(qualifiedName)
        )

        result.push(importDecl)
    }

    return result
}


private fun addImportsInSamePackage(classifiedProgram: pointer<HashMap>)
{
    if classifiedProgram == null:
        return

    val entries: pointer<ArrayList> = classifiedProgram.getEntries()


    for (var i = 0; i < entries.length; i++):
    {
        val entry: pointer<MapEntry> = entries.get(i) as pointer<MapEntry>
        val packageName: pointer<ArrayList> = entry.key as pointer<ArrayList>
        val nPrograms: pointer<ArrayList> = entry.value as pointer<ArrayList>

        val newImports: pointer<ArrayList> = addAllPackagesImport(nPrograms)
        

        for (var j = 0; j < nPrograms.length; j++):
        {
            val nProgram: pointer<NormalizedProgram> = nPrograms.get(j) as pointer<NormalizedProgram>
            val imports: pointer<ArrayList> = nProgram.getImports()


            // for (var k = 0; k < imports.length; )
        }
    }
}

