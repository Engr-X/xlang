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

#file.outerClass("NamespaceIpNormalizer")
package xlang.compiler.normalizer

import xlang.Diagnostic
import xlang.SourceLocation
import xlang.compiler.ImportAPI
import xlang.compiler.Imports
import xlang.compiler.parser.program.ImportDeclaration
import xlang.compiler.parser.program.NamespaceImport
import xlang.compiler.parser.program.QualifiedName
import xlang.util.ArrayList
import xlang.util.HashMap
import xlang.util.HashSet
import xlang.util.MapEntry


private fun getImports(nPrograms: pointer<ArrayList>, importAPIs: pointer<ArrayList>) -> pointer<ArrayList>
{
    val result: pointer<ArrayList> = new ArrayList(sizeof(ImportDeclaration))

    if nPrograms != null:
    {
        for (var i = 0; i < nPrograms.length; i++):
        {
            val nProgram: pointer<NormalizedProgram> = nPrograms.get(i) as pointer<NormalizedProgram>

            if nProgram == null:
                continue

            val fullPath: pointer<ArrayList> = nProgram.getFullpath()

            if fullPath == null || fullPath.length <= 0:
                continue

            val qualifiedName: pointer<QualifiedName> = new QualifiedName(fullPath)
            val importDecl: pointer<ImportDeclaration> = ImportDeclaration.fromNamespace(
                NamespaceImport.fromSingle(qualifiedName)
            )

            result.push(importDecl)
        }
    }

    if importAPIs == null:
        return result

    for (var i = 0; i < importAPIs.length; i++):
    {
        val importAPI: pointer<ImportAPI> = importAPIs.get(i) as pointer<ImportAPI>

        if importAPI == null:
            continue

        val fullPath: pointer<ArrayList> = importAPI.getPackageName().clone()
        val bodyName: pointer<char> = importAPI.getBodyName()

        if bodyName == null:
            continue

        fullPath.push(bodyName.ref)

        val qualifiedName: pointer<QualifiedName> = new QualifiedName(fullPath)
        val importDecl: pointer<ImportDeclaration> = ImportDeclaration.fromNamespace(
            NamespaceImport.fromSingle(qualifiedName)
        )

        result.push(importDecl)
    }

    return result
}


private fun getImports(
    classifiedProgram: pointer<HashMap>, imports: pointer<Imports>, qname: pointer<ArrayList>
) -> pointer<ArrayList>
{
    val nPrograms: pointer<ArrayList> =
        if classifiedProgram == null:
            null
        else:
            classifiedProgram.get(qname) as pointer<ArrayList>

    val importAPIs: pointer<ArrayList> =
        if imports == null:
            null
        else:
            imports.get(qname)

    return getImports(nPrograms, importAPIs)
}


private fun indexCmp(left: pointer<*>, right: pointer<*>) -> int =
    (left as pointer<int>).deref - (right as pointer<int>).deref


private fun indexHash(value: pointer<*>) -> int =
    (value as pointer<int>).deref


private fun innerExpandNamespaceImport(
    classifiedProgram: pointer<HashMap>,
    imports: pointer<Imports>, 
    otherImports: pointer<ArrayList>
) -> pointer<ArrayList>
{
    val diagnostics: pointer<ArrayList> = new ArrayList(sizeof(Diagnostic))

    if classifiedProgram == null:
        return diagnostics

    val entries: pointer<ArrayList> = classifiedProgram.getEntries()

    for (var i = 0; i < entries.length; i++):
    {
        val entry: pointer<MapEntry> = entries.get(i) as pointer<MapEntry>
        val nPrograms: pointer<ArrayList> = entry.value as pointer<ArrayList>

        for (var j = 0; j < nPrograms.length; j++):
        {
            val nProgram: pointer<NormalizedProgram> = nPrograms.get(j) as pointer<NormalizedProgram>
            val programImports: pointer<ArrayList> = nProgram.getImports()
            val removeIndexes: pointer<HashSet> = new HashSet(sizeof(int), indexCmp, indexHash)
            val expandedImports: pointer<ArrayList> = new ArrayList(sizeof(ImportDeclaration))
            val rewrittenImports: pointer<ArrayList> = new ArrayList(sizeof(ImportDeclaration))

            for (var k = 0; k < programImports.length; k++):
            {
                val importDecl: pointer<ImportDeclaration> = programImports.get(k) as pointer<ImportDeclaration>

                if importDecl != null && importDecl.getKind() == ImportDeclaration.NAMESPACE_TYPE:
                {
                    val namespaceImport: pointer<NamespaceImport> = importDecl.getHost() as pointer<NamespaceImport>

                    if namespaceImport != null && namespaceImport.isAll():
                    {
                        val qname: pointer<QualifiedName> = namespaceImport.getQualifiedName()

                        if qname != null:
                        {
                            val newImports: pointer<ArrayList> = getImports(
                                classifiedProgram,
                                imports,
                                qname.toPackageDecl().getQualifiedName())

                            if newImports.length <= 0:
                                diagnostics.push(Diagnostic.makeError(
                                    Diagnostic.CANNOT_PARSE,
                                    new ArrayList(sizeof(SourceLocation)),
                                    "namespace import does not exist"))
                            else:
                            {
                                removeIndexes.add(k.ref)
                                expandedImports.pushAll(newImports)
                            }
                        }
                    }
                }
            }

            for (var k = 0; k < programImports.length; k++):
            {
                if !removeIndexes.contains(k.ref):
                    rewrittenImports.push(programImports.get(k))
            }

            rewrittenImports.pushAll(getImports(nPrograms, new ArrayList(sizeof(ImportAPI))))
            rewrittenImports.pushAll(expandedImports)
            rewrittenImports.pushAll(otherImports)

            programImports.length = 0
            programImports.pushAll(rewrittenImports)
        }
    }

    return diagnostics
}


fun expandNamespaceImport(
    classifiedProgram: pointer<HashMap>,
    imports: pointer<Imports>,
    extraImport: pointer<ArrayList>
) -> bool
{
    val diagnostics: pointer<ArrayList> = innerExpandNamespaceImport(
        classifiedProgram,
        imports,
        extraImport)

    if diagnostics == null || diagnostics.length <= 0:
        return true

    for (var i = 0; i < diagnostics.length; i++):
    {
        val diagnostic: pointer<Diagnostic> = diagnostics.get(i) as pointer<Diagnostic>

        if diagnostic != null:
            diagnostic.print()
    }

    return false
}
