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

#file.outerClass("SelectiveImport")
package xlang.compiler.parser.program

import xlang.compiler.Type
import xlang.util.ArrayList


struct ImportedFunction
{
    private var name: pointer<char>

    private var parameterTypes: pointer<ArrayList>

    private var aliasName: pointer<char>


    constructor(name: pointer<char>, parameterTypes: pointer<ArrayList>, aliasName: pointer<char>)
    {
        this.name = name
        this.parameterTypes = if parameterTypes == null:
                new ArrayList(sizeof(Type))
            else:
                parameterTypes

        this.aliasName = aliasName
    }


    fun getName() -> pointer<char> = this.name


    fun getParameterTypes() -> pointer<ArrayList> = this.parameterTypes


    fun getAliasName() -> pointer<char> = this.aliasName
}


struct ImportedFunctionAliasMaybe
{
    private var aliasName: pointer<char>


    constructor():
        this.aliasName = null


    constructor(aliasName: pointer<char>):
        this.aliasName = aliasName


    fun getAliasName() -> pointer<char> = this.aliasName
}


struct SelectiveImports
{
    private var qualifiedName: pointer<QualifiedName>

    private var importedFunctions: pointer<ArrayList>


    constructor(qualifiedName: pointer<QualifiedName>, importedFunctions: pointer<ArrayList>)
    {
        this.qualifiedName = qualifiedName
        this.importedFunctions = if importedFunctions == null:
                new ArrayList(sizeof(ImportedFunction))
            else:
                importedFunctions
    }


    fun getQualifiedName() -> pointer<QualifiedName> = this.qualifiedName


    fun getImportedFunctions() -> pointer<ArrayList> = this.importedFunctions
}
