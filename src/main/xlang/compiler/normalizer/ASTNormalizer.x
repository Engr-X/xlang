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
package xlang.compiler.normalizer

import xlang.compiler.Imports
import xlang.compiler.parser.program.Field
import xlang.compiler.parser.program.Function
import xlang.compiler.parser.program.FunctionParam
import xlang.compiler.parser.program.FunctionParams
import xlang.compiler.parser.program.ImportDeclaration
import xlang.compiler.parser.program.ImportedSymbol
import xlang.compiler.parser.program.Member
import xlang.compiler.parser.program.MemberRegistry
import xlang.compiler.parser.program.SelectiveImports
import xlang.compiler.type.Type
import xlang.util.ArrayList
import xlang.util.HashMap
import xlang.util.MapEntry


private fun resolveDeclarationTypes(namespaceExpandedPrograms: pointer<HashMap>)
{
    if namespaceExpandedPrograms == null:
        return

    val entries: pointer<ArrayList> = namespaceExpandedPrograms.getEntries()

    if entries == null:
        return

    for (var i = 0; i < entries.length; i++):
    {
        val entry: pointer<MapEntry> = entries.get(i) as pointer<MapEntry>

        if entry == null:
            continue

        val programs: pointer<ArrayList> = entry.value as pointer<ArrayList>

        if programs == null:
            continue

        for (var j = 0; j < programs.length; j++):
        {
            val nProgram: pointer<NormalizedProgram> = programs.get(j) as pointer<NormalizedProgram>

            if nProgram == null:
                continue


            // expand import
            val programImports: pointer<ArrayList> = nProgram.getImports()

            if programImports == null:
                continue

            for (var k = 0; k < programImports.length; k++):
            {
                val importDecl: pointer<ImportDeclaration> = programImports.get(k) as pointer<ImportDeclaration>

                if importDecl == null || importDecl.getKind() != ImportDeclaration.SELECTIVE_TYPE:
                    continue

                val selectiveImports: pointer<SelectiveImports> = importDecl.getHost() as pointer<SelectiveImports>

                if selectiveImports == null || selectiveImports.isAll():
                    continue

                val importedSymbols: pointer<ArrayList> = selectiveImports.getImportedFunctions()

                if importedSymbols == null:
                    continue

                for (var l = 0; l < importedSymbols.length; l++):
                {
                    val importedSymbol: pointer<ImportedSymbol> = importedSymbols.get(l) as pointer<ImportedSymbol>

                    if importedSymbol == null:
                        continue

                    val parameterTypes: pointer<ArrayList> = importedSymbol.getParameterTypes()

                    if parameterTypes == null:
                        continue

                    for (var m = 0; m < parameterTypes.length; m++):
                    {
                        val parameterType: pointer<Type> = parameterTypes.get(m) as pointer<Type>

                        if parameterType != null:
                            parameterType.resolve(programImports)
                    }
                }
            }


            // expand field
            val variableRegistry: pointer<MemberRegistry> = nProgram.getVariableRegistry()

            if variableRegistry == null:
                continue

            val fields: pointer<ArrayList> = variableRegistry.getMembers()

            if fields == null:
                continue

            for (var k = 0; k < fields.length; k++):
            {
                val member: pointer<Member> = fields.get(k) as pointer<Member>

                if member == null || !member.isField():
                    continue

                val field: pointer<Field> = member.getHost() as pointer<Field>

                if field == null:
                    continue

                val fieldType: pointer<Type> = field.getFieldType()

                if fieldType != null:
                    fieldType.resolve(programImports)
            }


            // expand function
            val functionRegistry: pointer<MemberRegistry> = nProgram.getFunctionRegistry()

            if functionRegistry == null:
                continue

            val functions: pointer<ArrayList> = functionRegistry.getMembers()

            if functions == null:
                continue

            for (var k = 0; k < functions.length; k++):
            {
                val member: pointer<Member> = functions.get(k) as pointer<Member>

                if member == null || !member.isFunction():
                    continue

                val function: pointer<Function> = member.getHost() as pointer<Function>

                if function == null:
                    continue

                val returnType: pointer<Type> = function.getReturnType()

                if returnType != null:
                {
                    returnType.resolve(programImports)
                    function.setReturnType(returnType)
                }

                val params: pointer<FunctionParams> = function.getParams()

                if params == null:
                    continue

                for (var l = 0; l < params.length(); l++):
                {
                    val param: pointer<FunctionParam> = params.get(l)

                    if param == null:
                        continue

                    val paramType: pointer<Type> = param.getParamType()

                    if paramType != null:
                    {
                        paramType.resolve(programImports)
                        param.setParamType(paramType)
                    }
                }
            }
        }
    }
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
    val allNormalizedProgram: pointer<ArrayList> = ProgramSplitter.splitPrograms(allProgram)
    val flattenedPrograms: pointer<ArrayList> = ClassFlattener.flattenPrograms(allNormalizedProgram)
    val classifiedProgram: pointer<HashMap> = ProgramClassifier.classifyPrograms(flattenedPrograms)

    if !NamespaceIpNormalizer.expandNamespaceImport(classifiedProgram, imports, extraImport):
        return null

    return classifiedProgram
}
