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
import xlang.util.string.String
import xlang.util.string.StringBuilder


/**
 * Represents a qualified name composed of one or more ordered name parts.
 *
 * <p>A {@code QualifiedName} stores each name component as a separately
 * allocated null-terminated character sequence. Components are maintained in
 * their logical qualification order and are joined using {@code '.'} when the
 * name is converted to text.
 *
 * <p>For example, a qualified name such as:
 *
 * <pre>
 * xlang.compiler.parser
 * </pre>
 *
 * is represented internally by the ordered parts {@code "xlang"},
 * {@code "compiler"}, and {@code "parser"}.
 *
 * <p>Name components supplied through the constructor, {@code push()}, or
 * {@code pushFront()} are duplicated using {@code String.strdup} before being
 * stored. The internal representation therefore does not directly depend on the
 * lifetime of the caller's original character buffers.
 *
 * <p>Additional syntax tokens are retained separately and may contain identifier
 * tokens, qualification separators, or other lexical information associated
 * with the qualified name.
 */
struct QualifiedName
{
    /**
     * The ordered collection of name components forming this qualified name.
     *
     * <p>Each entry stores a pointer to a null-terminated character sequence.
     * Components are inserted as duplicated strings rather than direct references
     * to the character pointers supplied by callers.
     *
     * <p>The list object itself is allocated during construction and remains
     * associated with this {@code QualifiedName} for its lifetime.
     */
    private val parts: pointer<ArrayList>

    /**
     * Additional syntax tokens associated with this qualified name.
     *
     * <p>This collection may contain identifier tokens, {@code '.'} separators,
     * or other lexical tokens retained while parsing the qualified name.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates a qualified name containing the specified initial name part.
     *
     * <p>A new empty part collection and a new empty extra-token collection are
     * allocated first.
     *
     * <p>The supplied part is then added through {@code push()}, which duplicates
     * the character sequence using {@code String.strdup} before storing it.
     *
     * <p>If the supplied part cannot be duplicated and {@code String.strdup}
     * returns {@code null}, the resulting qualified name remains empty.
     *
     * @param part              a pointer to the initial null-terminated name
     *                          component
     */
    constructor(part: pointer<char>)
    {
        this.parts = new ArrayList(sizeof(pointer<char>))
        this.extraTokens = new ArrayList(sizeof(Token))
        this.push(part)
    }


    /**
     * Appends a name component to the end of this qualified name.
     *
     * <p>The supplied character sequence is duplicated using
     * {@code String.strdup}. If duplication succeeds, the resulting pointer is
     * stored at the end of the internal part collection.
     *
     * <p>The original input pointer is not stored directly, so later changes to
     * the caller's character buffer do not replace the internally stored name
     * component.
     *
     * <p>If {@code String.strdup} returns {@code null}, no part is appended.
     *
     * @param part              a pointer to the null-terminated name component
     *                          to append
     *
     * @return                  this {@code QualifiedName} instance
     */
    fun push(part: pointer<char>) -> pointer<QualifiedName>
    {
        val copied: pointer<char> = String.strdup(part)

        if copied != null:
            this.parts.push(copied.ref)

        return this
    }


    /**
     * Inserts a name component at the beginning of this qualified name.
     *
     * <p>The supplied character sequence is duplicated using
     * {@code String.strdup}. If duplication succeeds, the resulting pointer is
     * inserted at the front of the internal part collection.
     *
     * <p>All previously stored components therefore move one logical position
     * toward the end of the qualified name.
     *
     * <p>The original input pointer is not stored directly. If
     * {@code String.strdup} returns {@code null}, no modification is performed.
     *
     * @param part              a pointer to the null-terminated name component
     *                          to insert at the beginning
     *
     * @return                  this {@code QualifiedName} instance
     */
    fun pushFront(part: pointer<char>) -> pointer<QualifiedName>
    {
        val copied: pointer<char> = String.strdup(part)

        if copied != null:
            this.parts.pushFront(copied.ref)

        return this
    }


    /**
     * Returns a copy of the name component stored at the specified index.
     *
     * <p>The index is validated before accessing the internal part collection.
     * If the index is negative or greater than or equal to the number of stored
     * components, this method returns {@code null}.
     *
     * <p>The selected list entry is interpreted as a pointer slot containing a
     * {@code pointer<char>}. If the slot itself is {@code null}, the method also
     * returns {@code null}.
     *
     * <p>For a valid entry, the stored character sequence is duplicated using
     * {@code String.strdup}. The caller therefore receives a separate character
     * buffer rather than direct access to the internally stored name component.
     *
     * @param index             the zero-based index of the name component to
     *                          retrieve
     *
     * @return                  a pointer to a duplicated name component, or
     *                          {@code null} if the index or stored entry is
     *                          invalid
     */
    fun getPart(index: int) -> pointer<char>
    {
        if index < 0 || index >= this.parts.length:
            return null

        val slot: pointer<pointer<char>> = this.parts.get(index) as pointer<pointer<char>>

        if slot == null:
            return null

        return String.strdup(slot.deref)
    }


    /**
     * Adds an additional syntax token to this qualified name.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The supplied token is appended to the internal extra-token collection
     * and is stored by reference rather than cloned.
     *
     * <p>Tokens added through this method are included when
     * {@code getAllTokens()} constructs the complete token collection associated
     * with the qualified name.
     *
     * @param token             a pointer to the syntax token to add
     *
     * @return                  this {@code QualifiedName} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<QualifiedName>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Returns a copy of the additional syntax-token collection.
     *
     * <p>The internal {@code ArrayList} is cloned before being returned, so
     * structural modifications to the returned collection do not directly
     * replace or resize the list stored by this qualified name.
     *
     * <p>The individual {@code Token} objects referenced by the collection are
     * not recursively cloned.
     *
     * @return                  a pointer to a cloned list containing the
     *                          additional syntax tokens associated with this
     *                          qualified name
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    /**
     * Converts this qualified name into a package declaration.
     *
     * <p>The internal part collection is cloned and supplied to a newly created
     * {@code PackageDeclaration}. The package declaration therefore receives a
     * separate list object rather than the exact {@code ArrayList} used
     * internally by this qualified name.
     *
     * <p>The individual name-component pointers contained by that list are not
     * recursively duplicated by this method.
     *
     * <p>All additional syntax tokens stored by this qualified name are then
     * copied into the resulting package declaration through
     * {@code PackageDeclaration.addExtraToken()}.
     *
     * <p>The original qualified name is not modified by the conversion.
     *
     * @return                  a newly created {@code PackageDeclaration}
     *                          representing this qualified name
     */
    fun toPackageDecl() -> pointer<PackageDeclaration>
    {
        val result: pointer<PackageDeclaration> = new PackageDeclaration(this.parts.clone())

        for (var i = 0; i < this.extraTokens.length; i++):
            result.addExtraToken(this.extraTokens.get(i) as pointer<Token>)

        return result
    }


    /**
     * Converts this qualified name into a namespace import declaration.
     *
     * <p>A new {@code NamespaceImport} is created using this
     * {@code QualifiedName} instance as its qualified-name object. The current
     * object is therefore referenced directly and is not cloned during this
     * conversion.
     *
     * <p>The resulting namespace import is then wrapped in an
     * {@code ImportDeclaration} tagged as a namespace import through
     * {@code ImportDeclaration.fromNamespace()}.
     *
     * <p>The additional tokens stored by this qualified name remain owned by
     * this node and are exposed indirectly through its token collection.
     *
     * @return                  a newly created namespace
     *                          {@code ImportDeclaration} representing this
     *                          qualified name
     */
    fun toImportDeclaration() -> pointer<ImportDeclaration>
        = ImportDeclaration.fromNamespace(NamespaceImport.fromSingle(this))


    /**
     * Returns all tokens associated with this qualified name.
     *
     * <p>The additional syntax tokens stored by this node are copied into a newly
     * allocated result list.
     *
     * <p>The textual name parts themselves are stored as character sequences and
     * therefore do not independently contribute tokens through this method.
     * Identifier and qualification-separator tokens must be present in
     * {@code extraTokens} if they are required in the returned token sequence.
     *
     * <p>The final collection is sorted according to source position using
     * {@code TokenPosition.compareToken}. This ensures that tokens are returned
     * in their original lexical order even if they were added in another order.
     *
     * <p>A new list is allocated for the result. The individual token objects
     * are referenced rather than recursively cloned.
     *
     * @return                  a newly allocated list containing all tokens
     *                          associated with this qualified name in source
     *                          order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = this.extraTokens.clone()
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns the textual representation of this qualified name.
     *
     * <p>The stored name components are traversed in order. Each list entry is
     * interpreted as a pointer slot containing a null-terminated character
     * sequence.
     *
     * <p>Null slots and null character pointers are skipped.
     *
     * <p>Valid components are appended in order and are separated by a single
     * {@code '.'}. The {@code appendedPart} flag ensures that a separator is
     * inserted only after at least one valid component has already been emitted,
     * so skipped entries do not produce leading or duplicated separators.
     *
     * <p>For example, components {@code "xlang"}, {@code "compiler"}, and
     * {@code "parser"} produce:
     *
     * <pre>
     * xlang.compiler.parser
     * </pre>
     *
     * <p>If the collection contains no valid components, the returned
     * {@code StringBuilder} remains empty.
     *
     * <p>The returned builder is newly allocated and modifying its contents does
     * not modify the name components stored by this {@code QualifiedName}.
     *
     * @return                  a pointer to a newly created {@code StringBuilder}
     *                          containing the dot-separated qualified name
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()
        var appendedPart: bool = false

        for (var i = 0; i < this.parts.length; i++):
        {
            val slot: pointer<pointer<char>> = this.parts.get(i) as pointer<pointer<char>>

            if slot == null || slot.deref == null:
                continue

            if appendedPart:
                sb.append('.')

            sb.append(slot.deref)
            appendedPart = true
        }

        return sb
    }
}
