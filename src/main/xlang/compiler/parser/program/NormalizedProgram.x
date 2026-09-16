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
package xlang.compiler.parser.program

import xlang.util.ArrayList


struct NormalizedProgram
{
    private var packageDeclaration: pointer<PackageDeclaration>

    private var imports: pointer<ArrayList>

    private var structDeclaration: pointer<Struct>


    constructor(
        packageDeclaration: pointer<PackageDeclaration>,
        imports: pointer<ArrayList>,
        structDeclaration: pointer<Struct>
    )
    {
        this.packageDeclaration = packageDeclaration
        this.imports = if imports == null:
                new ArrayList(sizeof(ImportDeclaration))
            else:
                imports

        this.structDeclaration = structDeclaration
    }


    fun getPackageDeclaration() -> pointer<PackageDeclaration> = this.packageDeclaration


    fun getImports() -> pointer<ArrayList> = this.imports


    fun getStruct() -> pointer<Struct> = this.structDeclaration
}
