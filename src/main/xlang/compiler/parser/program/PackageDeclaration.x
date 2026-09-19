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
 * Represents a package declaration in the program abstract syntax tree.
 *
 * <p>A {@code PackageDeclaration} stores the qualified package name as an
 * ordered collection of null-terminated name components together with any
 * additional syntax tokens associated with the declaration.
 *
 * <p>Each qualified-name component is expected to be represented by a
 * {@code pointer<char>}. When converted to text, valid components are joined
 * using {@code '.'} separators and are prefixed with the {@code package}
 * keyword.
 *
 * <p>If no qualified-name collection is supplied to the constructor, the
 * declaration normalizes the value to an empty {@code ArrayList}.
 *
 * <p>Additional syntax tokens are retained separately and may contain the
 * {@code package} keyword, separators, or other lexical information preserved
 * while parsing the declaration.
 */
struct PackageDeclaration
{
    /**
     * The ordered collection of qualified package-name components.
     *
     * <p>Each entry is expected to contain a pointer to a null-terminated
     * character sequence representing one component of the package name.
     *
     * <p>For example, a declaration such as:
     *
     * <pre>
     * package xlang.compiler.parser
     * </pre>
     *
     * may be represented internally by the components {@code "xlang"},
     * {@code "compiler"}, and {@code "parser"}.
     */
    private var qualifiedName: pointer<ArrayList>

    /**
     * Additional syntax tokens associated with this package declaration.
     *
     * <p>This collection may contain the {@code package} keyword, qualification
     * separators, or other tokens that are not represented directly by the
     * stored name components.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates a package declaration from the specified qualified-name
     * collection.
     *
     * <p>If {@code qualifiedName} is {@code null}, a new empty
     * {@code ArrayList} capable of storing {@code pointer<char>} entries is
     * allocated.
     *
     * <p>If a valid collection is supplied, the collection is stored directly
     * and is not copied or cloned.
     *
     * <p>A new empty collection is also allocated for additional syntax tokens.
     *
     * @param qualifiedName		a pointer to the ordered package-name component
     * 					        list, or {@code null} to create an empty package
     * 					        name
     */
    constructor(qualifiedName: pointer<ArrayList>)
    {
        this.qualifiedName = if qualifiedName == null:
            new ArrayList(sizeof(pointer<char>))
        else:
            qualifiedName

        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Returns a copy of the qualified package-name collection.
     *
     * <p>The internal {@code ArrayList} is cloned before being returned, so
     * structural changes to the returned list do not directly replace or resize
     * the collection stored by this package declaration.
     *
     * <p>The character sequences referenced by the list entries are not
     * recursively duplicated. The operation therefore clones the collection
     * structure rather than cloning every package-name component.
     *
     * @return				    a pointer to a cloned list containing the qualified
     * 					        package-name components
     */
    fun getQualifiedName() -> pointer<ArrayList> = this.qualifiedName.clone()


    /**
     * Adds an additional syntax token to this package declaration.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The supplied token is appended to the internal extra-token collection
     * and is stored by reference rather than cloned.
     *
     * <p>Tokens added through this method are included when
     * {@code getAllTokens()} constructs the complete token collection associated
     * with the package declaration.
     *
     * @param token			    a pointer to the syntax token to add
     *
     * @return				    this {@code PackageDeclaration} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<PackageDeclaration>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Returns a copy of the additional syntax-token collection.
     *
     * <p>The internal {@code ArrayList} is cloned before being returned, so
     * structural modifications to the returned list do not directly modify the
     * collection stored by this package declaration.
     *
     * <p>The individual {@code Token} objects referenced by the list are not
     * recursively cloned.
     *
     * @return				    a pointer to a cloned list containing the additional
     * 					        syntax tokens associated with this package
     * 					        declaration
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    /**
     * Returns all tokens associated with this package declaration.
     *
     * <p>The additional syntax tokens stored directly by this declaration are
     * copied into a newly allocated result list.
     *
     * <p>The qualified-name components themselves are stored as character
     * sequences rather than AST nodes and therefore do not independently
     * contribute tokens through this method. Their corresponding lexical tokens
     * must be present in {@code extraTokens} if they are required in the result.
     *
     * <p>The final token collection is sorted according to source position using
     * {@code TokenPosition.compareToken}. This ensures that tokens are returned
     * in their original lexical order even if they were added to the declaration
     * in a different order.
     *
     * <p>A new list is allocated for the result. The contained token objects are
     * referenced rather than recursively cloned.
     *
     * @return				    a newly allocated list containing all tokens
     * 					        associated with this package declaration in source
     * 					        order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = this.extraTokens.clone()
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns the textual representation of this package declaration.
     *
     * <p>The generated representation always begins with the
     * {@code "package "} prefix.
     *
     * <p>The qualified-name collection is then traversed in order. Each valid
     * entry is interpreted as a pointer slot containing a null-terminated
     * character sequence.
     *
     * <p>Null slots and null character pointers are skipped. Valid name
     * components are appended to the resulting builder and qualification
     * components are separated using {@code '.'}.
     *
     * <p>A normal package declaration therefore has a representation similar to:
     *
     * <pre>
     * package xlang.compiler.parser
     * </pre>
     *
     * <p>If the qualified-name collection is empty, or contains no valid name
     * components, the resulting representation contains only the
     * {@code "package "} prefix.
     *
     * <p>The returned {@code StringBuilder} is newly allocated and modifying its
     * contents does not modify the package-name collection stored by this AST
     * node.
     *
     * @return				    a pointer to a newly created {@code StringBuilder}
     * 					        containing the textual representation of this
     * 					        package declaration
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder("package ")

        for (var i = 0; i < this.qualifiedName.length; i++):
        {
            val slot: pointer<pointer<char>> = this.qualifiedName.get(i) as pointer<pointer<char>>

            if slot == null || slot.deref == null:
                continue

            if i > 0:
                sb.append('.')

            sb.append(slot.deref)
        }

        return sb
    }
}


/**
 * Represents an optional package declaration.
 *
 * <p>{@code PackageDeclarationMaybe} is used by parser productions in which a
 * package declaration may be absent.
 *
 * <p>Unlike wrappers that normalize a missing value into an empty AST object,
 * this structure preserves the absence of a package declaration using a
 * {@code null} pointer.
 *
 * <p>The wrapper therefore allows callers to distinguish between a source file
 * that explicitly contains a package declaration and one that contains no
 * package declaration at all.
 */
struct PackageDeclarationMaybe
{
    /**
     * The optional package declaration represented by this wrapper.
     *
     * <p>A {@code null} value indicates that no package declaration was present.
     */
    private var packageDeclaration: pointer<PackageDeclaration>


    /**
     * Creates an empty optional package declaration.
     *
     * <p>The internal package-declaration pointer is initialized to
     * {@code null}, explicitly representing the absence of a package
     * declaration.
     */
    constructor():
        this.packageDeclaration = null


    /**
     * Creates an optional package-declaration wrapper around the supplied
     * declaration.
     *
     * <p>The supplied pointer is stored directly and is not copied or cloned.
     *
     * <p>If {@code packageDeclaration} is {@code null}, this wrapper represents
     * the absence of a package declaration.
     *
     * @param packageDeclaration	a pointer to the package declaration to
     * 					            store, or {@code null} if no declaration is
     * 					            present
     */
    constructor(packageDeclaration: pointer<PackageDeclaration>):
        this.packageDeclaration = packageDeclaration


    /**
     * Returns the package declaration represented by this wrapper.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code PackageDeclaration} and is not copied or cloned.
     *
     * <p>The result is {@code null} when the parser did not produce a package
     * declaration.
     *
     * @return				    a pointer to the stored package declaration, or
     * 					        {@code null} if no package declaration is present
     */
    fun toPackageDeclaration() -> pointer<PackageDeclaration> = this.packageDeclaration
}
