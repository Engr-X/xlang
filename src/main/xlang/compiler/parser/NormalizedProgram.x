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

#file.outerClass("NormalizedProgram")
package xlang.compiler.parser

import xlang.compiler.parser.program.ImportDeclaration
import xlang.compiler.parser.program.PackageDeclaration
import xlang.compiler.parser.program.PreprocessSetting
import xlang.compiler.parser.program.Struct
import xlang.util.ArrayList


struct NormalizedProgram
{
    private var preprocessSettings: pointer<ArrayList>

    private var packageDeclaration: pointer<PackageDeclaration>

    private var imports: pointer<ArrayList>

    private var structDeclaration: pointer<Struct>


    constructor(
        preprocessSettings: pointer<ArrayList>,
        packageDeclaration: pointer<PackageDeclaration>,
        imports: pointer<ArrayList>,
        structDeclaration: pointer<Struct>
    )
    {
        this.preprocessSettings = if preprocessSettings == null:
                new ArrayList(sizeof(PreprocessSetting))
            else:
                preprocessSettings

        this.packageDeclaration = packageDeclaration
        this.imports = if imports == null:
                new ArrayList(sizeof(ImportDeclaration))
            else:
                imports

        this.structDeclaration = structDeclaration
    }


    fun getPreprocessSettings() -> pointer<ArrayList> = this.preprocessSettings


    fun getPackageDeclaration() -> pointer<PackageDeclaration> = this.packageDeclaration


    fun getImports() -> pointer<ArrayList> = this.imports


    fun getStruct() -> pointer<Struct> = this.structDeclaration
}
