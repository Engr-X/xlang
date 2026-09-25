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
 *
 */

package xlang.compiler

import xlang.compiler.parser.program.MemberRegistry
import xlang.util.ArrayList
import xlang.util.HashMap
import xlang.util.string.String


private fun importPackageNameCmp(left: pointer<*>, right: pointer<*>) -> int
{
    val lhs: pointer<ArrayList> = left as pointer<ArrayList>
    val rhs: pointer<ArrayList> = right as pointer<ArrayList>

    if lhs == rhs:
        return 0

    if lhs == null:
        return -1

    if rhs == null:
        return 1

    val length: int = if lhs.length < rhs.length:
            lhs.length
        else:
            rhs.length

    for (var i: int = 0; i < length; i++):
    {
        val lhsSlot: pointer<pointer<char>> = lhs.get(i) as pointer<pointer<char>>
        val rhsSlot: pointer<pointer<char>> = rhs.get(i) as pointer<pointer<char>>

        val lhsStr: pointer<char> = if lhsSlot == null:
                null
            else:
                lhsSlot.deref

        val rhsStr: pointer<char> = if rhsSlot == null:
                null
            else:
                rhsSlot.deref

        if lhsStr == rhsStr:
            continue

        if lhsStr == null:
            return -1

        if rhsStr == null:
            return 1

        val cmp: int = String.strcmp(lhsStr, rhsStr)

        if cmp != 0:
            return cmp
    }

    return lhs.length - rhs.length
}


private fun importPackageNameHash(item: pointer<*>) -> int
{
    val packageName: pointer<ArrayList> = item as pointer<ArrayList>

    if packageName == null:
        return 0

    var hash: int = 1

    for (var i = 0; i < packageName.length; i++):
    {
        val slot: pointer<pointer<char>> = packageName.get(i) as pointer<pointer<char>>
        val part: pointer<char> = if slot == null:
                null
            else:
                slot.deref

        hash = hash * 31 + if part == null:
                0
            else:
                String.strHash(part)
    }

    return hash
}


struct ImportAPI
{
    static val STRUCT_TYPE: int = 1

    static val CLASS_TYPE: int = 2

    static val INTERFACE_TYPE: int = 3

    static val ANNOTATION_TYPE: int = 4


    private var packageName: pointer<ArrayList>

    private var bodyType: int

    private var bodyName: pointer<char>

    private var functionRegistry: pointer<MemberRegistry>

    private var variableRegistry: pointer<MemberRegistry>

    private var constructorRegistry: pointer<MemberRegistry>


    constructor(packageName: pointer<ArrayList>, bodyType: int, bodyName: pointer<char>)
    {
        this.packageName = if packageName == null:
                new ArrayList(sizeof(pointer<char>))
            else:
                packageName

        this.bodyType = bodyType
        this.bodyName = bodyName
        this.functionRegistry = new MemberRegistry(null)
        this.variableRegistry = new MemberRegistry(null)
        this.constructorRegistry = new MemberRegistry(null)
    }


    fun getPackageName() -> pointer<ArrayList> = this.packageName


    fun getBodyType() -> int = this.bodyType


    fun getBodyName() -> pointer<char> = this.bodyName


    fun getFunctionRegistry() -> pointer<MemberRegistry> = this.functionRegistry


    fun getVariableRegistry() -> pointer<MemberRegistry> = this.variableRegistry


    fun getConstructorRegistry() -> pointer<MemberRegistry> = this.constructorRegistry
}


struct Imports
{
    private var packages: pointer<HashMap>


    constructor()
    {
        this.packages = new HashMap(importPackageNameCmp, importPackageNameHash)
    }


    fun getPackages() -> pointer<HashMap> = this.packages


    fun getEntries() -> pointer<ArrayList> = this.packages.getEntries()


    fun containsPackageName(packageName: pointer<ArrayList>) -> bool =
        this.packages.containsKey(packageName)


    fun get(packageName: pointer<ArrayList>) -> pointer<ArrayList> =
        this.packages.get(packageName) as pointer<ArrayList>


    fun put(packageName: pointer<ArrayList>, imports: pointer<ArrayList>) -> pointer<Imports>
    {
        if packageName != null && imports != null:
            this.packages.put(packageName, imports)

        return this
    }


    fun push(importAPI: pointer<ImportAPI>) -> pointer<Imports>
    {
        if importAPI == null:
            return this

        val packageName: pointer<ArrayList> = importAPI.getPackageName()
        var imports: pointer<ArrayList> = this.get(packageName)

        if imports == null:
        {
            imports = new ArrayList(sizeof(ImportAPI))
            this.packages.put(packageName, imports)
        }

        imports.push(importAPI)
        return this
    }
}
