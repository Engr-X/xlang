/*
 * Copyright (c) 2026 Di Wang
 * SPDX-License-Identifier: MIT
 */

#file.outerClass("ClassFlattener")
package xlang.compiler.normalizer

import xlang.System
import xlang.compiler.parser.program.ImportDeclaration
import xlang.compiler.parser.program.Member
import xlang.compiler.parser.program.PackageDeclaration
import xlang.compiler.parser.program.QualifiedName
import xlang.compiler.parser.program.SelectiveImports
import xlang.compiler.parser.program.Struct
import xlang.compiler.type.Type
import xlang.util.ArrayList
import xlang.util.HashSet
import xlang.util.string.StringBuilder


private fun indexCmp(left: pointer<*>, right: pointer<*>) -> int =
    (left as pointer<int>).deref - (right as pointer<int>).deref


private fun indexHash(value: pointer<*>) -> int =
    (value as pointer<int>).deref


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
        val removedIndexes: pointer<HashSet> = new HashSet(sizeof(int), indexCmp, indexHash)
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
                    val newStructName: pointer<char> = System.allocMemory(
                        (structName.length + 1) * sizeof(char)) as pointer<char>
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


fun flattenPrograms(programs: pointer<ArrayList>) -> pointer<ArrayList>
{
    val result: pointer<ArrayList> = new ArrayList(sizeof(NormalizedProgram))

    if programs == null:
        return result

    for (var i = 0; i < programs.length; i++):
    {
        val program: pointer<NormalizedProgram> = programs.get(i) as pointer<NormalizedProgram>

        if program != null:
            result.pushAll(flattenNested(program))
    }

    return result
}
