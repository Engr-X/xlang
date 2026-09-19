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

import xlang.compiler.parser.expression.Atom
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


/**
 * Represents a preprocessing setting in the program abstract syntax tree.
 *
 * <p>A {@code PreprocessSetting} consists of a qualified setting name, an
 * ordered collection of atomic values, and additional syntax tokens retained
 * from the original source.
 *
 * <p>The setting is represented textually using a leading {@code '#'} followed
 * by its qualified name and a parenthesized value list, for example:
 *
 * <pre>
 * #file.outerClass(MyClass)
 * </pre>
 *
 * <p>The qualified name and value collection are stored by reference. If no
 * value collection is supplied, the constructor normalizes the value to an
 * empty {@code ArrayList}.
 *
 * <p>Each value entry is expected to contain a pointer to an {@code Atom}.
 * Additional syntax tokens may contain the preprocessing marker, parentheses,
 * commas, or other lexical elements that are not directly owned by the name or
 * value atoms.
 *
 * <p>All source tokens associated with the setting can be collected in lexical
 * order using {@code getAllTokens()}.
 */
struct PreprocessSetting
{
    /**
     * The qualified name identifying this preprocessing setting.
     *
     * <p>The qualified-name object is stored by reference and is not copied or
     * cloned by the constructor.
     *
     * <p>This value may be {@code null} if the setting was constructed without
     * a valid name.
     */
    private var name: pointer<QualifiedName>


    /**
     * The ordered collection of values supplied to this preprocessing setting.
     *
     * <p>Each entry is expected to contain a pointer to an {@code Atom}.
     *
     * <p>The collection is normalized to an empty list when a {@code null}
     * value is supplied to the constructor.
     */
    private var value: pointer<ArrayList>

    /**
     * Additional syntax tokens associated with this preprocessing setting.
     *
     * <p>This collection may contain the {@code '#'} marker, parentheses,
     * commas, or other tokens that are not directly owned by the qualified name
     * or individual value atoms.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates a preprocessing setting with the specified qualified name and
     * value collection.
     *
     * <p>The supplied qualified name is stored by reference and is not copied or
     * cloned.
     *
     * <p>If {@code value} is {@code null}, a new empty {@code ArrayList}
     * capable of storing {@code pointer<Atom>} entries is allocated. Otherwise,
     * the supplied collection is stored directly and is not copied or cloned.
     *
     * <p>A new empty collection is also allocated for additional syntax tokens.
     *
     * @param name			    a pointer to the qualified preprocessing-setting
     * 					        name, or {@code null} if no name is available
     * @param value			    a pointer to the ordered atom-value collection,
     * 					        or {@code null} to create an empty value list
     */
    constructor(name: pointer<QualifiedName>, value: pointer<ArrayList>)
    {
        this.name = name
        this.value = if value == null: new ArrayList(sizeof(pointer<Atom>)) else: value
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Returns the qualified name associated with this preprocessing setting.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code QualifiedName} object and is not copied or cloned.
     *
     * <p>The result may be {@code null} if this setting was constructed without
     * a valid name.
     *
     * @return				    a pointer to the internally stored qualified name,
     * 					        or {@code null} if no name is available
     */
    fun getName() -> pointer<QualifiedName> = this.name


    /**
     * Returns the value collection associated with this preprocessing setting.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Because the constructor normalizes a {@code null} value collection to
     * an empty list, this method normally returns a valid {@code ArrayList}.
     *
     * <p>Modifications performed through the returned list affect the same
     * collection referenced internally by this {@code PreprocessSetting}
     * instance.
     *
     * @return				    a pointer to the internally stored preprocessing
     * 					        value collection
     */
    fun getValue() -> pointer<ArrayList> = this.value


    /**
     * Adds an additional syntax token to this preprocessing setting.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The supplied token is appended to the internal extra-token collection
     * and is stored by reference rather than cloned.
     *
     * <p>Tokens added through this method are included when
     * {@code getAllTokens()} constructs the complete token collection associated
     * with the setting.
     *
     * @param token			    a pointer to the syntax token to add
     *
     * @return				    this {@code PreprocessSetting} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<PreprocessSetting>
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
     * replace or resize the list stored by this setting.
     *
     * <p>The individual {@code Token} objects referenced by the collection are
     * not recursively cloned.
     *
     * @return				    a pointer to a cloned list containing the additional
     * 					        syntax tokens associated with this preprocessing
     * 					        setting
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    /**
     * Returns all tokens associated with this preprocessing setting.
     *
     * <p>The additional syntax tokens stored directly by this setting are added
     * to the result first.
     *
     * <p>If a qualified setting name is available, all tokens returned by
     * {@code QualifiedName.getAllTokens()} are also included.
     *
     * <p>The value collection is then traversed in order. Each entry is
     * interpreted as a pointer slot containing an {@code Atom}. Null slots and
     * null atom references are ignored.
     *
     * <p>For every valid atom, all tokens returned by
     * {@code Atom.getAllTokens()} are appended to the result.
     *
     * <p>After tokens from the setting name, values, and additional syntax have
     * been collected, the complete result is sorted according to source
     * position using {@code TokenPosition.compareToken}. This restores the
     * original lexical ordering regardless of the order in which tokens were
     * gathered from the different AST components.
     *
     * <p>A new list is allocated for the result. The individual token objects
     * are referenced rather than recursively cloned.
     *
     * @return				    a newly allocated list containing all tokens
     * 					        associated with this preprocessing setting in source
     * 					        order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = this.extraTokens.clone()

        if this.name != null:
            result.pushAll(this.name.getAllTokens())

        for (var i = 0; i < this.value.length; i++):
        {
            val slot: pointer<pointer<Atom>> = this.value.get(i) as pointer<pointer<Atom>>

            if slot == null || slot.deref == null:
                continue

            val atom: pointer<Atom> = slot.deref
            result.pushAll(atom.getAllTokens())
        }

        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns the textual representation of this preprocessing setting.
     *
     * <p>The generated representation always begins with the {@code '#'}
     * preprocessing marker.
     *
     * <p>If a qualified setting name is available, its textual representation
     * is appended immediately after the marker.
     *
     * <p>A parenthesized value list is always emitted. Valid atom values are
     * written in their stored order and are separated by {@code ", "}. Null
     * value slots and null atom references are skipped and do not produce
     * separators.
     *
     * <p>The resulting representation generally has the form:
     *
     * <pre>
     * #qualified.name(value1, value2, ...)
     * </pre>
     *
     * <p>If the value collection contains no valid atoms, an empty pair of
     * parentheses is still emitted:
     *
     * <pre>
     * #qualified.name()
     * </pre>
     *
     * <p>If {@code name} is {@code null}, the representation begins directly
     * with the preprocessing marker followed by the value parentheses.
     *
     * <p>The returned {@code StringBuilder} is newly allocated and modifying its
     * contents does not modify the underlying preprocessing-setting AST node.
     *
     * @return				    a pointer to a newly created {@code StringBuilder}
     * 					        containing the textual representation of this
     * 					        preprocessing setting
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder("#")

        if this.name != null:
            sb.append(this.name.toString())

        sb.append('(')
        var appendedAtom: bool = false

        for (var i = 0; i < this.value.length; i++):
        {
            val slot: pointer<pointer<Atom>> = this.value.get(i) as pointer<pointer<Atom>>

            if slot == null || slot.deref == null:
                continue

            if appendedAtom:
                sb.append(", ")

            val atom: pointer<Atom> = slot.deref
            sb.append(atom.toString())
            appendedAtom = true
        }

        sb.append(')')
        return sb
    }
}


/**
 * Represents an optional collection of preprocessing settings.
 *
 * <p>{@code PreprocessSettingsMaybe} is intended for parser productions in
 * which a sequence of preprocessing settings may be absent.
 *
 * <p>Instead of preserving a {@code null} list to represent the absence of
 * settings, this structure normalizes the state to an empty
 * {@code ArrayList}. Consumers can therefore obtain a preprocessing-setting
 * collection without repeatedly checking whether the list itself exists.
 *
 * <p>If an existing setting collection is supplied to the constructor, that
 * collection is stored directly and is not copied or cloned.
 */
struct PreprocessSettingsMaybe
{
    /**
     * The normalized collection of preprocessing settings.
     *
     * <p>This field contains either the collection supplied to the constructor
     * or a newly allocated empty list when no setting collection was provided.
     */
    private var settings: pointer<ArrayList>


    /**
     * Creates an empty optional preprocessing-setting collection.
     *
     * <p>A new empty {@code ArrayList} is allocated so that the absence of
     * preprocessing settings can be represented without retaining a
     * {@code null} list pointer.
     */
    constructor():
        this.settings = new ArrayList(sizeof(PreprocessSetting))


    /**
     * Creates an optional preprocessing-setting collection from the specified
     * list.
     *
     * <p>If {@code settings} is {@code null}, a new empty preprocessing-setting
     * collection is allocated. Otherwise, the supplied list is stored directly
     * and is not copied or cloned.
     *
     * <p>This normalization ensures that the internal setting collection is
     * represented by a valid {@code ArrayList} after construction.
     *
     * @param settings			a pointer to the preprocessing-setting
     * 					        collection, or {@code null} to represent an empty
     * 					        collection
     */
    constructor(settings: pointer<ArrayList>):
        this.settings = if settings == null:
                new ArrayList(sizeof(PreprocessSetting))
            else:
                settings


    /**
     * Returns the normalized preprocessing-setting collection represented by
     * this wrapper.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Because both constructors normalize an absent setting list to an empty
     * collection, this method normally returns a valid list even when no
     * preprocessing settings were present in the parsed source.
     *
     * <p>Modifications performed through the returned list affect the same
     * collection referenced internally by this {@code PreprocessSettingsMaybe}
     * instance.
     *
     * @return				a pointer to the internally stored preprocessing
     * 					setting collection
     */
    fun toPreprocessSettings() -> pointer<ArrayList> = this.settings
}
