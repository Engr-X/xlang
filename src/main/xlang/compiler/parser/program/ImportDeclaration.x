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


/**
 * Represents a single import declaration in the program abstract syntax tree.
 *
 * <p>An {@code ImportDeclaration} acts as a tagged wrapper around one of the
 * concrete import structures supported by the language. The {@code kind} field
 * identifies the concrete type referenced by {@code host}.
 *
 * <p>A namespace import is represented by {@code NAMESPACE_TYPE} and stores a
 * {@code NamespaceImport} as its host. A selective import is represented by
 * {@code SELECTIVE_TYPE} and stores a {@code SelectiveImports} as its host.
 *
 * <p>Factory methods are provided for constructing each supported import form
 * without requiring callers to manually specify the corresponding kind value.
 *
 * <p>Operations that require access to the concrete import object must interpret
 * {@code host} according to {@code kind} before casting it to the appropriate
 * type.
 */
struct ImportDeclaration
{
    /**
     * Identifies an import declaration backed by a {@code NamespaceImport}.
     *
     * <p>When an import has this kind, {@code host} is expected to reference a
     * {@code NamespaceImport} instance.
     */
    static val NAMESPACE_TYPE: int = 1

    /**
     * Identifies an import declaration backed by {@code SelectiveImports}.
     *
     * <p>When an import has this kind, {@code host} is expected to reference a
     * {@code SelectiveImports} instance.
     */
    static val SELECTIVE_TYPE: int = 2


    /**
     * Creates an import declaration from a namespace import.
     *
     * <p>The supplied namespace import is stored by reference and is not copied
     * or cloned. The resulting declaration is tagged with
     * {@code NAMESPACE_TYPE}, allowing later operations to interpret the
     * untyped host pointer correctly.
     *
     * <p>If {@code namespaceImport} is {@code null}, the resulting declaration
     * still retains the namespace-import kind, but its host pointer is
     * {@code null}.
     *
     * @param namespaceImpor    a pointer to the namespace import to wrap,
     * 					        or {@code null} if no import object is available
     *
     * @return				    a newly created namespace import declaration
     */
    static fun fromNamespace(namespaceImport: pointer<NamespaceImport>) -> pointer<ImportDeclaration> =
        new ImportDeclaration(NAMESPACE_TYPE, namespaceImport)


    /**
     * Creates an import declaration from a selective-import structure.
     *
     * <p>The supplied selective import object is stored by reference and is not
     * copied or cloned. The resulting declaration is tagged with
     * {@code SELECTIVE_TYPE}, allowing the untyped host pointer to be interpreted
     * as {@code SelectiveImports}.
     *
     * <p>If {@code selectiveImports} is {@code null}, the resulting declaration
     * still retains the selective-import kind, but its host pointer is
     * {@code null}.
     *
     * @param selectiveImports  a pointer to the selective-import structure
     * 					        to wrap, or {@code null} if no import object is
     * 					        available
     *
     * @return				    a newly created selective import declaration
     */
    static fun fromSelective(selectiveImports: pointer<SelectiveImports>) -> pointer<ImportDeclaration> =
        new ImportDeclaration(SELECTIVE_TYPE, selectiveImports)


    /**
     * The kind identifier describing the concrete import represented by this
     * declaration.
     *
     * <p>The value determines how {@code host} must be interpreted. Known values
     * include {@code NAMESPACE_TYPE} and {@code SELECTIVE_TYPE}.
     */
    private var kind: int

    /**
     * A pointer to the concrete import object wrapped by this declaration.
     *
     * <p>The pointer is intentionally untyped because different import kinds
     * store different concrete structures.
     *
     * <p>The actual pointer type must be determined from {@code kind} before the
     * value is dereferenced or used as a concrete import object.
     */
    private var host: pointer<*>


    /**
     * Creates an import declaration using the specified kind and host object.
     *
     * <p>Both values are stored directly. The host object is referenced rather
     * than copied or cloned.
     *
     * <p>This constructor does not validate that {@code host} actually matches
     * the type indicated by {@code kind}. Callers are therefore responsible for
     * maintaining the relationship between the tag and the stored object.
     *
     * @param kind			    the kind identifier describing the import
     * @param host			    a pointer to the concrete import object, or
     * 					        {@code null} if no host object is available
     */
    constructor(kind: int, host: pointer<*>)
    {
        this.kind = kind
        this.host = host
    }


    /**
     * Returns the kind identifier of this import declaration.
     *
     * <p>The returned value can be used to determine the concrete type referenced
     * by {@code getHost()}.
     *
     * <p>For example, {@code NAMESPACE_TYPE} indicates that the host is expected
     * to represent a {@code NamespaceImport}, while {@code SELECTIVE_TYPE}
     * indicates a {@code SelectiveImports} host.
     *
     * @return				    the kind identifier of this import declaration
     */
    fun getKind() -> int = this.kind


    /**
     * Returns the concrete import object wrapped by this declaration.
     *
     * <p>The returned pointer is untyped and refers directly to the internally
     * stored host object. The object is not copied or cloned.
     *
     * <p>Callers should inspect {@code getKind()} before casting the returned
     * pointer to a concrete import type.
     *
     * <p>The result may be {@code null} if this declaration was constructed
     * without a host object.
     *
     * @return				a pointer to the internally stored import object,
     * 					or {@code null} if no host is available
     */
    fun getHost() -> pointer<*> = this.host


    /**
     * Returns all tokens associated with this import declaration.
     *
     * <p>If {@code host} is {@code null}, a new empty token list is returned
     * because no concrete import node is available from which tokens can be
     * collected.
     *
     * <p>For {@code NAMESPACE_TYPE}, the host pointer is interpreted as a
     * {@code NamespaceImport} and token collection is delegated directly to
     * {@code NamespaceImport.getAllTokens()}.
     *
     * <p>The current implementation does not yet dispatch
     * {@code SELECTIVE_TYPE} to {@code SelectiveImports}. Selective imports and
     * any other unhandled kind therefore fall through to the default branch and
     * produce a newly allocated empty token list.
     *
     * <p>For a namespace import, the returned list is the collection produced by
     * the underlying {@code NamespaceImport}; this method does not clone or
     * re-sort that collection.
     *
     * @return				    the token collection produced by the wrapped
     * 					        namespace import, or a newly allocated empty token
     * 					        list when the host is absent or the kind is not
     * 					        currently handled
     */
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

    /**
     * Returns the textual representation of this import declaration.
     *
     * <p>If {@code host} is {@code null}, a newly allocated empty
     * {@code StringBuilder} is returned.
     *
     * <p>For {@code NAMESPACE_TYPE}, the host pointer is interpreted as a
     * {@code NamespaceImport} and textual representation is delegated directly
     * to {@code NamespaceImport.toString()}.
     *
     * <p>The current implementation does not yet dispatch
     * {@code SELECTIVE_TYPE} to {@code SelectiveImports}. Selective imports and
     * any other unhandled kinds therefore produce an empty
     * {@code StringBuilder}.
     *
     * <p>For a namespace import, this method returns the builder produced by the
     * underlying namespace-import node rather than constructing an additional
     * wrapper representation.
     *
     * @return				    a pointer to the textual representation produced by
     * 					        the wrapped namespace import, or a newly created
     * 					        empty {@code StringBuilder} when the host is absent
     * 					        or the kind is not currently handled
     */
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


/**
 * Represents an optional collection of import declarations.
 *
 * <p>{@code ImportDeclarationsMaybe} is primarily used by parser productions in
 * which a sequence of imports may be absent.
 *
 * <p>Instead of retaining {@code null} to represent the absence of imports, this
 * structure normalizes the value to an empty {@code ArrayList}. Consumers can
 * therefore obtain an import collection without repeatedly checking whether the
 * list itself exists.
 *
 * <p>If an existing collection is supplied to the constructor, that collection
 * is stored directly and is not copied or cloned.
 */
struct ImportDeclarationsMaybe
{
    /**
     * The normalized collection of import declarations.
     *
     * <p>This field contains either the list supplied to the constructor or a
     * newly allocated empty list when no import collection was provided.
     */
    private var imports: pointer<ArrayList>


    /**
     * Creates an empty optional import collection.
     *
     * <p>A new empty {@code ArrayList} is allocated so that the absence of
     * imports can be represented without retaining a {@code null} list pointer.
     */
    constructor():
        this.imports = new ArrayList(sizeof(NamespaceImport))


    /**
     * Creates an optional import collection from the specified list.
     *
     * <p>If {@code imports} is {@code null}, a new empty list is allocated.
     * Otherwise, the supplied collection is stored directly and is not copied
     * or cloned.
     *
     * <p>This normalization ensures that {@code imports} references an
     * {@code ArrayList} after construction regardless of whether the parser
     * originally produced an import sequence.
     *
     * @param imports			a pointer to the import collection, or
     * 					        {@code null} to represent an empty collection
     */
    constructor(imports: pointer<ArrayList>):
        this.imports = if imports == null:
                new ArrayList(sizeof(NamespaceImport))
            else:
                imports


    /**
     * Returns the normalized import collection represented by this wrapper.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Because the constructors normalize an absent list to an empty
     * collection, this method normally returns a valid list even when no import
     * declarations were present in the parsed source.
     *
     * <p>Modifications made through the returned list affect the same collection
     * referenced internally by this {@code ImportDeclarationsMaybe} instance.
     *
     * @return				    a pointer to the internally stored import collection
     */
    fun toImportDeclarations() -> pointer<ArrayList> = this.imports
}
