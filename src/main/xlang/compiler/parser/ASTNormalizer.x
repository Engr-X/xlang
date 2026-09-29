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

import xlang.Diagnostic
import xlang.SourceLocation
import xlang.System
import xlang.compiler.ImportAPI
import xlang.compiler.Imports
import xlang.compiler.setting.CompilerSettings
import xlang.compiler.parser.program.Field
import xlang.compiler.parser.program.Function
import xlang.compiler.parser.program.ImportDeclaration
import xlang.compiler.parser.program.Member
import xlang.compiler.parser.program.MemberRegistry
import xlang.compiler.parser.program.Modifier
import xlang.compiler.parser.program.NamespaceImport
import xlang.compiler.parser.program.PackageDeclaration
import xlang.compiler.parser.program.PreprocessSetting
import xlang.compiler.parser.program.Program
import xlang.compiler.parser.program.QualifiedName
import xlang.compiler.parser.program.SelectiveImports
import xlang.compiler.parser.program.Struct
import xlang.compiler.type.NormalType
import xlang.compiler.type.Type
import xlang.lexer.Token
import xlang.util.ArrayList
import xlang.util.HashMap
import xlang.util.HashSet
import xlang.util.MapEntry
import xlang.util.string.String
import xlang.util.string.StringBuilder


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


private fun flattenNestedIt(nProgram: pointer<NormalizedProgram>, dest: pointer<ArrayList>)
{
    if nProgram == null || dest == null:
        return

    val proccessedSettings: pointer<ArrayList> = nProgram.getPreprocessSettings()
    val packagePath: pointer<ArrayList> = nProgram.getPackageName()
    val imports: pointer<ArrayList> = nProgram.getImports()
    val bodyType: int = nProgram.getBodyType()


    if bodyType == NormalizedProgram.STRUCT_TYPE:
    {
        val structBody: pointer<Struct> = nProgram.getHost() as pointer<Struct>

        if structBody == null:
            return

        val members: pointer<ArrayList> = structBody.getMembers()
        val removedIndexes: pointer<HashSet> = new HashSet(sizeof(int), importIndexCmp, importIndexHash)
        var hasNestedStruct: bool = false

        for (var i = 0; i < members.length; i++):
        {
            val member: pointer<Member> = members.get(i) as pointer<Member>
            val memberType: int = if member == null:
                    0
                else:
                    member.getKind()


            if memberType == Member.STRUCT_TYPE:
            {
                hasNestedStruct = true
                removedIndexes.add(i.ref)
                val innerStruct: pointer<Struct> = member.getHost() as pointer<Struct>

                if innerStruct != null:
                {
                    val structName: pointer<StringBuilder> = new StringBuilder(structBody.getStructName())
                    structName.append(Type.CLASS_SEPERAtOR)
                    structName.append(innerStruct.getStructName())
                    val newStructName: pointer<char> = System.allocMemory((structName.length + 1) * sizeof(char)) as pointer<char>
                    structName.toString(newStructName)

                    val parentName: pointer<char> = nProgram.getBodyName()
                    val innerImports: pointer<ArrayList> = imports.clone()

                    if parentName != null:
                    {
                        val parentPath: pointer<ArrayList> = packagePath.clone()
                        parentPath.push(parentName.ref)

                        val fromParentStruct: pointer<ImportDeclaration> = ImportDeclaration.fromSelective(
                            SelectiveImports.fromAll(new QualifiedName(parentPath)))
                        innerImports.push(fromParentStruct)
                    }

                    val innerProgram: pointer<NormalizedProgram> = NormalizedProgram.fromStruct(
                        proccessedSettings,
                        new PackageDeclaration(packagePath.clone()),
                        innerImports,
                        new Struct(newStructName, innerStruct.getMembers()))
                    val innerPrograms: pointer<ArrayList> = new ArrayList(sizeof(NormalizedProgram))

                    flattenNestedIt(innerProgram, innerPrograms)
                    dest.pushAll(innerPrograms)
                }
            }
            elif memberType == NormalizedProgram.ANNOTATION_TYPE:
            {
            }
            elif memberType == NormalizedProgram.CLASS_TYPE:
            {
            }
            elif memberType == NormalizedProgram.INTERFACE_TYPE:
            {
            }
        }

        if !hasNestedStruct:
        {
            dest.push(nProgram)
            return
        }

        val bodyMembers: pointer<ArrayList> = new ArrayList(sizeof(Member))

        for (var i = 0; i < members.length; i++):
        {
            if removedIndexes.contains(i.ref):
                continue

            val member: pointer<Member> = members.get(i) as pointer<Member>

            if member != null:
                bodyMembers.push(member)
        }

        val body: pointer<Struct> = new Struct(structBody.getStructName(), bodyMembers)
        val flattenedProgram: pointer<NormalizedProgram> = NormalizedProgram.fromStruct(
            proccessedSettings,
            new PackageDeclaration(packagePath),
            imports.clone(),
            body)

        dest.push(flattenedProgram)
    }
    elif bodyType == NormalizedProgram.ANNOTATION_TYPE:
    {
    }
    elif bodyType == NormalizedProgram.CLASS_TYPE:
    {
    }
    elif bodyType == NormalizedProgram.INTERFACE_TYPE:
    {
    }
}


fun flattenNested(nProgram: pointer<NormalizedProgram>) -> pointer<ArrayList>
{
    val result: pointer<ArrayList> = new ArrayList(sizeof(NormalizedProgram))

    flattenNestedIt(nProgram, result)
    return result
}


private fun classifyPrograsm(programs: pointer<ArrayList>) -> pointer<HashMap>
{
    val result: pointer<HashMap> = new HashMap(String.stringListCmp, String.stringListHash)

    if programs == null:
        return result

    for (var i = 0; i < programs.length; i++):
    {
        val program: pointer<NormalizedProgram> = programs.get(i) as pointer<NormalizedProgram>

        if program == null:
            continue

        val packageDeclaration: pointer<PackageDeclaration> = program.getPackageDeclaration()
        val packagePath: pointer<ArrayList> = if packageDeclaration == null:
                new ArrayList(sizeof(pointer<char>))
            else:
                packageDeclaration.getQualifiedName()

        if result.containsKey(packagePath):
        {
            val classifiedPrograms: pointer<ArrayList> = result.get(packagePath) as pointer<ArrayList>

            if classifiedPrograms != null:
                classifiedPrograms.push(program)
        }
        else:
        {
            val classifiedPrograms: pointer<ArrayList> = new ArrayList(sizeof(NormalizedProgram))
            classifiedPrograms.push(program)
            result.put(packagePath, classifiedPrograms)
        }
    }

    return result
}


private fun getImports(nPrograms: pointer<ArrayList>, importAPIs: pointer<ArrayList>) -> pointer<ArrayList>
{
    val result: pointer<ArrayList> = new ArrayList(sizeof(ImportDeclaration))

    if nPrograms != null:
    {
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
    }

    if importAPIs == null:
        return result

    for (var i = 0; i < importAPIs.length; i++):
    {
        val importAPI: pointer<ImportAPI> = importAPIs.get(i) as pointer<ImportAPI>

        if importAPI == null:
            continue

        val fullPath: pointer<ArrayList> = importAPI.getPackageName().clone()
        val bodyName: pointer<char> = importAPI.getBodyName()

        if bodyName == null:
            continue

        fullPath.push(bodyName.ref)

        val qualifiedName: pointer<QualifiedName> = new QualifiedName(fullPath)
        val importDecl: pointer<ImportDeclaration> = ImportDeclaration.fromNamespace(
            NamespaceImport.fromSingle(qualifiedName)
        )

        result.push(importDecl)
    }

    return result
}


private fun getImports(classifiedProgram: pointer<HashMap>, imports: pointer<Imports>, qname: pointer<ArrayList>) -> pointer<ArrayList>
{
    val nPrograms: pointer<ArrayList> =
        if classifiedProgram == null:
            null
        else:
            classifiedProgram.get(qname) as pointer<ArrayList>

    val importAPIs: pointer<ArrayList> =
        if imports == null:
            null
        else:
            imports.get(qname)

    return getImports(nPrograms, importAPIs)
}


private fun importIndexCmp(left: pointer<*>, right: pointer<*>) -> int =
    (left as pointer<int>).deref - (right as pointer<int>).deref


private fun importIndexHash(value: pointer<*>) -> int =
    (value as pointer<int>).deref


private fun expandNamespaceImport(
    classifiedProgram: pointer<HashMap>, imports: pointer<Imports>, otherImports: pointer<ArrayList>
) -> pointer<ArrayList>
{
    val diagnostics: pointer<ArrayList> = new ArrayList(sizeof(Diagnostic))

    if classifiedProgram == null:
        return diagnostics

    val entries: pointer<ArrayList> = classifiedProgram.getEntries()

    for (var i = 0; i < entries.length; i++):
    {
        val entry: pointer<MapEntry> = entries.get(i) as pointer<MapEntry>
        val nPrograms: pointer<ArrayList> = entry.value as pointer<ArrayList>


        for (var j = 0; j < nPrograms.length; j++):
        {
            val nProgram: pointer<NormalizedProgram> = nPrograms.get(j) as pointer<NormalizedProgram>
            val programImports: pointer<ArrayList> = nProgram.getImports()
            val removeIndexes: pointer<HashSet> = new HashSet(sizeof(int), importIndexCmp, importIndexHash)
            val expandedImports: pointer<ArrayList> = new ArrayList(sizeof(ImportDeclaration))
            val rewrittenImports: pointer<ArrayList> = new ArrayList(sizeof(ImportDeclaration))


            for (var k = 0; k < programImports.length; k++):
            {
                val importDecl: pointer<ImportDeclaration> = programImports.get(k) as pointer<ImportDeclaration>

                if importDecl != null && importDecl.getKind() == ImportDeclaration.NAMESPACE_TYPE:
                {
                    val namespaceImport: pointer<NamespaceImport> = importDecl.getHost() as pointer<NamespaceImport>

                    if namespaceImport != null && namespaceImport.isAll():
                    {
                        val qname: pointer<QualifiedName> = namespaceImport.getQualifiedName()

                        if qname != null:
                        {
                            val newImports: pointer<ArrayList> = getImports(classifiedProgram, imports, qname.toPackageDecl().getQualifiedName())

                            if newImports.length <= 0:
                                diagnostics.push(
                                    Diagnostic.makeError(Diagnostic.CANNOT_PARSE, new ArrayList(sizeof(SourceLocation)),
                                    "namespace import does not exist"
                                ))
                            else:
                            {
                                removeIndexes.add(k.ref)
                                expandedImports.pushAll(newImports)
                            }
                        }
                    }
                }
            }

            // expand all import
            for (var k = 0; k < programImports.length; k++):
            {
                if !removeIndexes.contains(k.ref):
                    rewrittenImports.push(programImports.get(k))
            }

            // add imports in samepackage
            rewrittenImports.pushAll(getImports(nPrograms, new ArrayList(sizeof(ImportAPI))))

            rewrittenImports.pushAll(expandedImports)
            rewrittenImports.pushAll(otherImports)

            programImports.length = 0
            programImports.pushAll(rewrittenImports)
        }
    }

    return diagnostics
}


private fun getVariableDef(nProgram: pointer<NormalizedProgram>, name: pointer<char>) -> pointer<Field>
{
    if nProgram == null || name == null:
        return null

    val registry: pointer<MemberRegistry> = nProgram.getVariableRegistry()

    if registry == null:
        return null

    val members: pointer<ArrayList> = registry.getMembers()

    if members == null:
        return null

    for (var i = 0; i < members.length; i++):
    {
        val member: pointer<Member> = members.get(i) as pointer<Member>

        if member == null || !member.isField():
            continue

        val field: pointer<Field> = member.getHost() as pointer<Field>

        if field != null && String.streq(field.getFieldName(), name):
            return field
    }

    return null
}


private fun getFuncDef(nProgram: pointer<NormalizedProgram>, name: pointer<char>) -> pointer<Function>
{
    if nProgram == null || name == null:
        return null

    val registry: pointer<MemberRegistry> = nProgram.getFunctionRegistry()

    if registry == null:
        return null

    val members: pointer<ArrayList> = registry.getMembers()

    if members == null:
        return null

    for (var i = 0; i < members.length; i++):
    {
        val member: pointer<Member> = members.get(i) as pointer<Member>

        if member == null || !member.isFunction():
            continue

        val function: pointer<Function> = member.getHost() as pointer<Function>

        if function != null && String.streq(function.getFunctionName(), name):
            return function
    }

    return null
}


private fun expandSelectiveImport(classifiedProgram: pointer<HashMap>, imports: pointer<Imports>)
{

}


fun normalizeProgram(
    allProgram: pointer<ArrayList>,
    imports: pointer<Imports>,
    extraImport: pointer<ArrayList>
) -> pointer<HashMap>
{
    val allNormalizedProgram: pointer<ArrayList> = new ArrayList(sizeof(NormalizedProgram))

    if allProgram != null:
    {
        for (var i = 0; i < allProgram.length; i++):
        {
            val program: pointer<Program> = allProgram.get(i) as pointer<Program>

            if program == null:
                continue

            allNormalizedProgram.pushAll(splitProgram(program))
        }
    }

    val flattenedPrograms: pointer<ArrayList> = new ArrayList(sizeof(NormalizedProgram))

    for (var i = 0; i < allNormalizedProgram.length; i++):
        flattenedPrograms.pushAll(flattenNested(allNormalizedProgram.get(i) as pointer<NormalizedProgram>))

    val classifiedProgram: pointer<HashMap> = classifyPrograsm(flattenedPrograms)
    val diagnostics: pointer<ArrayList> = expandNamespaceImport(
        classifiedProgram,
        imports,
        extraImport)

    if diagnostics != null && diagnostics.length > 0:
    {
        for (var i = 0; i < diagnostics.length; i++):
        {
            val diagnostic: pointer<Diagnostic> = diagnostics.get(i) as pointer<Diagnostic>

            if diagnostic != null:
                diagnostic.print()
        }

        return null
    }

    return classifiedProgram
}
