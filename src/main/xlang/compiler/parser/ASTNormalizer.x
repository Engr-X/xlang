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

import xlang.compiler.parser.program.Member
import xlang.compiler.parser.program.NormalizedProgram
import xlang.compiler.parser.program.PackageDeclaration
import xlang.compiler.parser.program.Program
import xlang.compiler.parser.program.Struct
import xlang.util.ArrayList


private fun initPreprocessSettings(preprocessSettings: pointer<ArrayList>)
{
    
}


fun splitProgram(program: pointer<Program>) -> pointer<ArrayList>
{
    val result: pointer<ArrayList> = new ArrayList(sizeof(NormalizedProgram))

    if program == null:
        return result

    val preprocessSettings: pointer<ArrayList> = program.getPreprocessSettings()
    val packageDeclaration: pointer<PackageDeclaration> = program.getPackageDeclaration()
    val imports: pointer<ArrayList> = program.getImportDeclarations()

    return result
}
