/*
 * Copyright (c) 2026 Di Wang
 * SPDX-License-Identifier: MIT
 */

#file.outerClass("ProgramSplitter")
package xlang.compiler.normalizer

import xlang.compiler.setting.CompilerSettings
import xlang.compiler.parser.program.Field
import xlang.compiler.parser.program.Function
import xlang.compiler.parser.program.ImportDeclaration
import xlang.compiler.parser.program.Member
import xlang.compiler.parser.program.Modifier
import xlang.compiler.parser.program.PackageDeclaration
import xlang.compiler.parser.program.PreprocessSetting
import xlang.compiler.parser.program.Program
import xlang.compiler.parser.program.QualifiedName
import xlang.compiler.parser.program.SelectiveImports
import xlang.compiler.parser.program.Struct
import xlang.util.ArrayList


private fun initPreprocessSettings(
    path: pointer<char>, preprocessSettings: pointer<ArrayList>
) -> pointer<CompilerSettings>
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

    val extraImport: pointer<ImportDeclaration> = ImportDeclaration.fromSelective(
        SelectiveImports.fromAll(extraImportPath))

    for (var i = 0; i < structs.length; i++):
    {
        val structMember: pointer<Member> = structs.get(i) as pointer<Member>

        if structMember == null:
            continue

        var structItem: pointer<Struct> = structMember.getHost() as pointer<Struct>

        if structItem == null:
            continue

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


fun splitPrograms(programs: pointer<ArrayList>) -> pointer<ArrayList>
{
    val result: pointer<ArrayList> = new ArrayList(sizeof(NormalizedProgram))

    if programs == null:
        return result

    for (var i = 0; i < programs.length; i++):
    {
        val program: pointer<Program> = programs.get(i) as pointer<Program>

        if program != null:
            result.pushAll(splitProgram(program))
    }

    return result
}
