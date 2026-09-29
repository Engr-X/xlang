/*
 * Copyright (c) 2026 Di Wang
 * SPDX-License-Identifier: MIT
 */

#file.outerClass("ASTNormalizerTest")
package xlang.compiler.parser

import xlang.compiler.parser.program.ImportDeclaration
import xlang.compiler.parser.program.Member
import xlang.compiler.parser.program.NamespaceImport
import xlang.compiler.parser.program.PackageDeclaration
import xlang.compiler.parser.program.PreprocessSetting
import xlang.compiler.parser.program.QualifiedName
import xlang.compiler.parser.program.SelectiveImports
import xlang.compiler.parser.program.Struct
import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion
import xlang.util.ArrayList
import xlang.util.string.String


val TEST_GROUP: pointer<TestGroup> = genTest()


fun genTest() -> pointer<TestGroup>
{
    val result: pointer<TestGroup> = new TestGroup("xlang.compiler.parser.ASTNormalizer")

    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("flattenNestedStruct", flattenNestedStructTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("flattenNestedStructRecursive", flattenNestedStructRecursiveTest), null))

    return result
}


private fun qname1(a: pointer<char>) -> pointer<QualifiedName> =
    new QualifiedName(a)


private fun qname2(a: pointer<char>, b: pointer<char>) -> pointer<QualifiedName>
{
    val result: pointer<QualifiedName> = new QualifiedName(a)
    result.push(b)
    return result
}


private fun qname3(a: pointer<char>, b: pointer<char>, c: pointer<char>) -> pointer<QualifiedName>
{
    val result: pointer<QualifiedName> = new QualifiedName(a)
    result.push(b)
    result.push(c)
    return result
}


private fun qname4(a: pointer<char>, b: pointer<char>, c: pointer<char>, d: pointer<char>) -> pointer<QualifiedName>
{
    val result: pointer<QualifiedName> = new QualifiedName(a)
    result.push(b)
    result.push(c)
    result.push(d)
    return result
}


private fun packagePath(qname: pointer<QualifiedName>) -> pointer<ArrayList>
{
    val packageDeclaration: pointer<PackageDeclaration> = qname.toPackageDecl()
    return packageDeclaration.getQualifiedName()
}


private fun makeImports() -> pointer<ArrayList>
{
    val result: pointer<ArrayList> = new ArrayList(sizeof(ImportDeclaration))
    result.push(ImportDeclaration.fromNamespace(NamespaceImport.fromSingle(qname3("com", "wangdi", "ArrayList"))))
    return result
}


private fun makeOneLevelStruct() -> pointer<Struct>
{
    val inner: pointer<Struct> = new Struct("B", null)
    val outer: pointer<Struct> = new Struct("A", null)

    outer.addMember(Member.fromStruct(inner))
    return outer
}


private fun makeTwoLevelStruct() -> pointer<Struct>
{
    val leaf: pointer<Struct> = new Struct("C", null)
    val inner: pointer<Struct> = new Struct("B", null)
    val outer: pointer<Struct> = new Struct("A", null)

    inner.addMember(Member.fromStruct(leaf))
    outer.addMember(Member.fromStruct(inner))

    return outer
}


private fun flattenStruct(structBody: pointer<Struct>) -> pointer<ArrayList>
{
    val preprocessSettings: pointer<ArrayList> = new ArrayList(sizeof(PreprocessSetting))
    val packageDeclaration: pointer<PackageDeclaration> = qname1("here").toPackageDecl()
    val nProgram: pointer<NormalizedProgram> = NormalizedProgram.fromStruct(
        preprocessSettings,
        packageDeclaration,
        makeImports(),
        structBody)

    return ASTNormalizer.flattenNested(nProgram)
}


private fun findProgram(programs: pointer<ArrayList>, packageName: pointer<ArrayList>, bodyName: pointer<char>) -> pointer<NormalizedProgram>
{
    if programs == null:
        return null

    for (var i = 0; i < programs.length; i++):
    {
        val program: pointer<NormalizedProgram> = programs.get(i) as pointer<NormalizedProgram>

        if program != null && String.stringListCmp(program.getPackageName(), packageName) == 0 &&
            String.streq(program.getBodyName(), bodyName):
            return program
    }

    return null
}


private fun hasSelectiveImport(imports: pointer<ArrayList>, qualifiedName: pointer<ArrayList>) -> bool
{
    if imports == null || qualifiedName == null:
        return false

    for (var i = 0; i < imports.length; i++):
    {
        val importDecl: pointer<ImportDeclaration> = imports.get(i) as pointer<ImportDeclaration>

        if importDecl == null || importDecl.getKind() != ImportDeclaration.SELECTIVE_TYPE:
            continue

        val selectiveImports: pointer<SelectiveImports> = importDecl.getHost() as pointer<SelectiveImports>

        if selectiveImports == null || selectiveImports.getQualifiedName() == null:
            continue

        val selectiveName: pointer<QualifiedName> = selectiveImports.getQualifiedName()
        val packageDeclaration: pointer<PackageDeclaration> = selectiveName.toPackageDecl()
        val actual: pointer<ArrayList> = packageDeclaration.getQualifiedName()

        if String.stringListCmp(actual, qualifiedName) == 0:
            return true
    }

    return false
}


private fun flattenNestedStructTest() -> int
{
    val programs: pointer<ArrayList> = flattenStruct(makeOneLevelStruct())
    val outer: pointer<NormalizedProgram> = findProgram(programs, packagePath(qname1("here")), "A")
    val inner: pointer<NormalizedProgram> = findProgram(programs, packagePath(qname1("here")), "A$B")

    if outer == null:
        return 1

    if inner == null:
        return 2

    if !hasSelectiveImport(inner.getImports(), packagePath(qname2("here", "A"))):
        return 3

    return 0
}


private fun flattenNestedStructRecursiveTest() -> int
{
    val programs: pointer<ArrayList> = flattenStruct(makeTwoLevelStruct())
    val outer: pointer<NormalizedProgram> = findProgram(programs, packagePath(qname1("here")), "A")
    val middle: pointer<NormalizedProgram> = findProgram(programs, packagePath(qname1("here")), "A$B")
    val leaf: pointer<NormalizedProgram> = findProgram(programs, packagePath(qname1("here")), "A$B$C")

    if outer == null:
        return 1

    if middle == null:
        return 2

    if leaf == null:
        return 3

    if !hasSelectiveImport(middle.getImports(), packagePath(qname2("here", "A"))):
        return 4

    if !hasSelectiveImport(leaf.getImports(), packagePath(qname2("here", "A$B"))):
        return 5

    return 0
}
