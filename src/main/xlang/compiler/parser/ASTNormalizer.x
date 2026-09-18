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
import xlang.compiler.parser.program.Member
import xlang.compiler.parser.program.NormalizedProgram
import xlang.compiler.parser.program.PackageDeclaration
import xlang.compiler.parser.program.PreprocessSetting
import xlang.compiler.parser.program.Program
import xlang.compiler.parser.program.Struct
import xlang.util.ArrayList


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


// fun splitProgram(program: pointer<Program>) -> pointer<ArrayList>
// {
//     val result: pointer<ArrayList> = new ArrayList(sizeof(NormalizedProgram))

//     if program == null:
//         return result

//     val preprocessSettings: pointer<ArrayList> = program.getPreprocessSettings()

//     val config: pointer<CompilerSettings> = initPreprocessSettings(, preprocessSettings)
//     val packageDeclaration: pointer<PackageDeclaration> = program.getPackageDeclaration()
//     val imports: pointer<ArrayList> = program.getImportDeclarations()

//     return result
// }
