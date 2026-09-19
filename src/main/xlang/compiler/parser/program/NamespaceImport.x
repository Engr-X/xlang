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
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


/**
 * Represents a namespace import declaration in the program abstract syntax
 * tree.
 *
 * <p>A {@code NamespaceImport} stores the qualified name being imported
 * together with any additional syntax tokens associated with the import
 * declaration.
 *
 * <p>The qualified name is stored by reference and is not copied or cloned when
 * the instance is created.
 *
 * <p>Additional syntax tokens may contain the {@code import} keyword or other
 * lexical elements that are not directly owned by the qualified name.
 *
 * <p>All tokens belonging to the import declaration can be collected in source
 * order using {@code getAllTokens()}.
 */
struct NamespaceImport
{
    /**
     * The qualified name identifying the namespace being imported.
     *
     * <p>The object is stored by reference and may be {@code null} if no valid
     * qualified name was provided.
     */
    private var qualifiedName: pointer<QualifiedName>

    /**
     * Additional syntax tokens associated with this import declaration.
     *
     * <p>This collection may contain the {@code import} keyword or other syntax
     * tokens that are not directly owned by the qualified name.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates a namespace import declaration for the specified qualified name.
     *
     * <p>The supplied qualified name is stored by reference and is not copied or
     * cloned.
     *
     * <p>A new empty collection is allocated for additional syntax tokens
     * associated with the import declaration.
     *
     * @param qualifiedName		a pointer to the qualified namespace name, or
     * 					        {@code null} if no qualified name is available
     */
    constructor(qualifiedName: pointer<QualifiedName>)
    {
        this.qualifiedName = qualifiedName
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Returns the qualified name associated with this namespace import.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code QualifiedName} object and is not copied or cloned.
     *
     * <p>The result may be {@code null} if this import declaration was created
     * without a valid qualified name.
     *
     * @return				    a pointer to the internally stored qualified name,
     * 					        or {@code null} if no name is available
     */
    fun getQualifiedName() -> pointer<QualifiedName> = this.qualifiedName


    /**
     * Adds an additional syntax token to this namespace import.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The supplied token is appended to the internal extra-token collection
     * and is stored by reference rather than cloned.
     *
     * <p>Tokens added through this method are included when
     * {@code getAllTokens()} constructs the complete token collection for the
     * import declaration.
     *
     * @param token			    a pointer to the syntax token to add
     *
     * @return				    this {@code NamespaceImport} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<NamespaceImport>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Returns a copy of the additional syntax-token collection.
     *
     * <p>The internal {@code ArrayList} is cloned before being returned, so
     * structural changes to the returned collection do not directly modify the
     * list stored by this namespace import.
     *
     * <p>The individual {@code Token} objects referenced by the list are not
     * recursively cloned.
     *
     * @return				    a pointer to a cloned list containing the additional
     * 					        syntax tokens associated with this import
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    /**
     * Returns all tokens associated with this namespace import declaration.
     *
     * <p>If a qualified name is available, all tokens returned by
     * {@code QualifiedName.getAllTokens()} are added to the result.
     *
     * <p>The tokens belonging to the qualified name are then combined with the
     * additional syntax tokens stored directly by this import declaration.
     * These additional tokens may include the {@code import} keyword or other
     * lexical elements retained during parsing.
     *
     * <p>The final token collection is sorted according to source position using
     * {@code TokenPosition.compareToken}. This restores the original lexical
     * ordering even though tokens are collected from separate AST components.
     *
     * <p>A new list is allocated for the result. The individual token objects
     * are referenced rather than recursively cloned.
     *
     * @return				    a newly allocated list containing all tokens
     * 					        associated with this namespace import in source order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.qualifiedName != null:
            result.pushAll(this.qualifiedName.getAllTokens())

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns the textual representation of this namespace import declaration.
     *
     * <p>The generated representation always begins with the
     * {@code "import "} prefix.
     *
     * <p>If a qualified name is available, its textual representation is
     * appended immediately after the import keyword, producing a form similar
     * to:
     *
     * <pre>
     * import some.namespace.Name
     * </pre>
     *
     * <p>If {@code qualifiedName} is {@code null}, the returned representation
     * contains only the {@code "import "} prefix.
     *
     * <p>The returned {@code StringBuilder} is newly allocated and modifying its
     * contents does not modify the underlying import declaration.
     *
     * @return				    a pointer to a newly created {@code StringBuilder}
     * 					        containing the textual representation of this
     * 					        namespace import
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder("import ")

        if this.qualifiedName != null:
            sb.append(this.qualifiedName.toString())

        return sb
    }
}
