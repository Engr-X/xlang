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

package xlang.compiler.parser.program

import xlang.lexer.Token
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct ImportDeclaration
{
    static val NAMESPACE_TYPE: int = 1

    static val SELECTIVE_TYPE: int = 2


    static fun fromNamespace(namespaceImport: pointer<NamespaceImport>) -> pointer<ImportDeclaration> =
        new ImportDeclaration(NAMESPACE_TYPE, namespaceImport)


    static fun fromSelective(selectiveImports: pointer<SelectiveImports>) -> pointer<ImportDeclaration> =
        new ImportDeclaration(SELECTIVE_TYPE, selectiveImports)


    private var kind: int

    private var host: pointer<*>


    constructor(kind: int, host: pointer<*>)
    {
        this.kind = kind
        this.host = host
    }


    fun getKind() -> int = this.kind


    fun getHost() -> pointer<*> = this.host


    fun getAllTokens() -> pointer<ArrayList> =
        if this.host == null:
            new ArrayList(sizeof(Token))
        elif this.kind == NAMESPACE_TYPE:
        {
            val namespaceImport: pointer<NamespaceImport> = this.host as pointer<NamespaceImport>
            namespaceImport.getAllTokens()
        }
        else:
            new ArrayList(sizeof(Token))


    fun toString() -> pointer<StringBuilder> =
        if this.host == null:
            new StringBuilder()
        elif this.kind == NAMESPACE_TYPE:
        {
            val namespaceImport: pointer<NamespaceImport> = this.host as pointer<NamespaceImport>
            namespaceImport.toString()
        }
        else:
            new StringBuilder()
}


struct ImportDeclarationsMaybe
{
    private var imports: pointer<ArrayList>


    constructor():
        this.imports = new ArrayList(sizeof(NamespaceImport))


    constructor(imports: pointer<ArrayList>):
        this.imports = if imports == null:
                new ArrayList(sizeof(NamespaceImport))
            else:
                imports


    fun toImportDeclarations() -> pointer<ArrayList> = this.imports
}
