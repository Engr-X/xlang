/*
 * Copyright (c) 2026 Di Wang
 * SPDX-License-Identifier: MIT
 */

#file.outerClass("NormalTypeTest")
package xlang.compiler.type

import xlang.compiler.parser.program.ImportDeclaration
import xlang.compiler.parser.program.NamespaceImport
import xlang.compiler.parser.program.QualifiedName
import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion
import xlang.util.ArrayList
import xlang.util.string.String


val TEST_GROUP: pointer<TestGroup> = genTest()


fun genTest() -> pointer<TestGroup>
{
    val result: pointer<TestGroup> = new TestGroup("xlang.compiler.type.NormalType")
    val canResolveTG: pointer<TestGroup> = new TestGroup("canResolve")
    val resolveTG: pointer<TestGroup> = new TestGroup("resolve")

    canResolveTG.addTestUnion(new TestUnion(
        TestCase.TYPE,
        new TestCase("qualifiedNestedType", qualifiedNestedTypeCanResolveTest),
        null))
    canResolveTG.addTestUnion(new TestUnion(
        TestCase.TYPE,
        new TestCase("rootNestedType", rootNestedTypeCanResolveTest),
        null))

    resolveTG.addTestUnion(new TestUnion(
        TestCase.TYPE,
        new TestCase("singleImport", singleImportResolveTest),
        null))
    resolveTG.addTestUnion(new TestUnion(
        TestCase.TYPE,
        new TestCase("duplicateImport", duplicateImportResolveTest),
        null))
    resolveTG.addTestUnion(new TestUnion(
        TestCase.TYPE,
        new TestCase("ambiguousNestedType", ambiguousNestedTypeResolveTest),
        null))

    result.addTestUnion(new TestUnion(TestGroup.TYPE, null, canResolveTG))
    result.addTestUnion(new TestUnion(TestGroup.TYPE, null, resolveTG))
    return result
}


private fun packagePartEquals(parts: pointer<ArrayList>, index: int, expected: pointer<char>) -> bool
{
    if parts == null || index < 0 || index >= parts.length:
        return false

    val slot: pointer<pointer<char>> = parts.get(index) as pointer<pointer<char>>
    return slot != null && String.streq(slot.deref, expected)
}


private fun qualifiedNestedTypeCanResolveTest() -> int
{
    val normalType: pointer<NormalType> = NormalType.unresolved("Node")
    val qualifiedName: pointer<QualifiedName> = new QualifiedName("com")
        .push("wangdi")
        .push("ArrayList$Node")
    val namespaceImport: pointer<NamespaceImport> = NamespaceImport.fromSingle(qualifiedName)

    if !normalType.canResolve(namespaceImport):
        return 1

    val packageName: pointer<ArrayList> = normalType.getPackageName()

    if packageName == null || packageName.length != 2:
        return 2

    if !packagePartEquals(packageName, 0, "com") || !packagePartEquals(packageName, 1, "wangdi"):
        return 3

    if !String.streq(normalType.getTypeName(), "ArrayList$Node"):
        return 4

    return 0
}


private fun rootNestedTypeCanResolveTest() -> int
{
    val normalType: pointer<NormalType> = NormalType.unresolved("Node")
    val namespaceImport: pointer<NamespaceImport> = NamespaceImport.fromSingle(
        new QualifiedName("ArrayList$Node"))

    if !normalType.canResolve(namespaceImport):
        return 1

    val packageName: pointer<ArrayList> = normalType.getPackageName()

    if packageName == null || packageName.length != 0:
        return 2

    if !String.streq(normalType.getTypeName(), "ArrayList$Node"):
        return 3

    return 0
}


private fun singleImportResolveTest() -> int
{
    val normalType: pointer<NormalType> = NormalType.unresolved("Node")
    val imports: pointer<ArrayList> = new ArrayList(sizeof(ImportDeclaration))
    val qualifiedName: pointer<QualifiedName> = new QualifiedName("com")
        .push("wangdi")
        .push("ArrayList$Node")

    imports.push(ImportDeclaration.fromNamespace(NamespaceImport.fromSingle(qualifiedName)))

    if normalType.resolve(imports) != null:
        return 1

    val packageName: pointer<ArrayList> = normalType.getPackageName()

    if packageName == null || packageName.length != 2:
        return 2

    if !packagePartEquals(packageName, 0, "com") || !packagePartEquals(packageName, 1, "wangdi"):
        return 3

    if !String.streq(normalType.getTypeName(), "ArrayList$Node"):
        return 4

    return 0
}


private fun duplicateImportResolveTest() -> int
{
    val normalType: pointer<NormalType> = NormalType.unresolved("Node")
    val imports: pointer<ArrayList> = new ArrayList(sizeof(ImportDeclaration))

    imports.push(ImportDeclaration.fromNamespace(NamespaceImport.fromSingle(
        new QualifiedName("com").push("wangdi").push("ArrayList$Node"))))
    imports.push(ImportDeclaration.fromNamespace(NamespaceImport.fromSingle(
        new QualifiedName("com").push("wangdi").push("ArrayList$Node"))))

    if normalType.resolve(imports) != null:
        return 1

    if !String.streq(normalType.getPackageNameText(), "com.wangdi"):
        return 2

    if !String.streq(normalType.getTypeName(), "ArrayList$Node"):
        return 3

    return 0
}


private fun ambiguousNestedTypeResolveTest() -> int
{
    val normalType: pointer<NormalType> = NormalType.unresolved("Node")
    val imports: pointer<ArrayList> = new ArrayList(sizeof(ImportDeclaration))

    imports.push(ImportDeclaration.fromNamespace(NamespaceImport.fromSingle(
        new QualifiedName("com").push("wangdi").push("ArrayList$Node"))))
    imports.push(ImportDeclaration.fromNamespace(NamespaceImport.fromSingle(
        new QualifiedName("com").push("wangdi").push("LinkedList$Node"))))

    if normalType.resolve(imports) == null:
        return 1

    if !String.streq(normalType.getPackageNameText(), "com.wangdi"):
        return 2

    if !String.streq(normalType.getTypeName(), "ArrayList$Node"):
        return 3

    return 0
}
