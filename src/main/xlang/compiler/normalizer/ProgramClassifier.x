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

#file.outerClass("ProgramClassifier")
package xlang.compiler.normalizer

import xlang.compiler.parser.program.PackageDeclaration
import xlang.util.ArrayList
import xlang.util.HashMap
import xlang.util.string.String


fun classifyPrograms(programs: pointer<ArrayList>) -> pointer<HashMap>
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
