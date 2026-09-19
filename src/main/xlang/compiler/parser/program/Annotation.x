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
 * Represents an annotation attached to a program declaration or other
 * annotatable language construct.
 *
 * <p>An {@code Annotation} consists of a qualified annotation name, an ordered
 * collection of atomic argument values, and additional syntax tokens retained
 * from the original source.
 *
 * <p>The {@code hasParentheses} flag records whether parentheses were explicitly
 * present in the source representation. This distinction is preserved even when
 * the annotation contains no values, allowing {@code @Name} and
 * {@code @Name()} to remain structurally distinguishable.
 *
 * <p>Annotation values are represented by {@code Atom} nodes and are stored in
 * their original order. Additional tokens may contain the {@code @} symbol,
 * parentheses, commas, or other syntax elements that are not directly owned by
 * the annotation name or value atoms.
 *
 * <p>All source tokens belonging to the annotation can be collected in lexical
 * order using {@code getAllTokens()}.
 */
struct Annotation
{
   /**
     * The qualified name identifying this annotation.
     *
     * <p>The name is stored by reference and is not copied or cloned by either
     * constructor.
     */
    private var name: pointer<QualifiedName>

    /**
     * The ordered collection of atomic values supplied to this annotation.
     *
     * <p>Each entry is expected to contain a pointer to an {@code Atom}.
     * The collection may be empty when the annotation has no arguments.
     */
    private var value: pointer<ArrayList>

    /**
     * Indicates whether the annotation explicitly contains an argument
     * parenthesis pair.
     *
     * <p>This flag distinguishes an annotation written without parentheses,
     * such as:
     *
     * <pre>@Name</pre>
     * from one written with an explicitly empty argument list:
     * <pre>@Name()</pre>
     *
     * even though both forms may contain zero value atoms.
     */
    private var hasParentheses: bool

    /**
     * Additional syntax tokens associated with this annotation.
     *
     * <p>This collection may contain the annotation marker, parentheses, commas,
     * or other source tokens that are not directly owned by the qualified name
     * or individual value atoms.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates an annotation without an explicit argument list.
     *
     * <p>The supplied qualified name is stored by reference and is not copied or
     * cloned.
     *
     * <p>A new empty value list is allocated and {@code hasParentheses} is set
     * to {@code false}, meaning that the annotation is represented in the form
     * {@code @Name} rather than {@code @Name()}.
     *
     * <p>A new empty collection is also allocated for additional syntax tokens.
     *
     * @param name			    a pointer to the qualified annotation name
     */
    constructor(name: pointer<QualifiedName>)
    {
        this.name = name
        this.value = new ArrayList(sizeof(pointer<Atom>))
        this.hasParentheses = false
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Creates an annotation with an explicit argument list.
     *
     * <p>The supplied qualified name is stored by reference and is not copied or
     * cloned.
     *
     * <p>If {@code value} is {@code null}, a new empty value list is allocated.
     * Otherwise, the supplied list is stored directly and is not copied.
     *
     * <p>{@code hasParentheses} is always set to {@code true} by this
     * constructor. Therefore, even a {@code null} or empty value list represents
     * an annotation written with explicit parentheses, such as
     * {@code @Name()}.
     *
     * <p>A new empty collection is allocated for additional syntax tokens.
     *
     * @param name			    a pointer to the qualified annotation name
     * @param value			    a pointer to the annotation value list, or
     * 					        {@code null} to create an empty value list
     */
    constructor(name: pointer<QualifiedName>, value: pointer<ArrayList>)
    {
        this.name = name
        this.value = if value == null: new ArrayList(sizeof(pointer<Atom>)) else: value
        this.hasParentheses = true
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Returns the qualified name identifying this annotation.
     *
     * <p>The returned pointer refers directly to the name object stored
     * internally by this annotation. The qualified name is not copied or cloned.
     *
     * <p>The result may be {@code null} if this annotation was constructed
     * without a valid name.
     *
     * @return				    a pointer to the internally stored qualified name,
     * 					        or {@code null} if no name is available
     */
    fun getName() -> pointer<QualifiedName> = this.name


    /**
     * Returns a copy of the annotation value collection.
     *
     * <p>The internal {@code ArrayList} is cloned before being returned, so
     * modifications to the returned list structure do not directly modify the
     * list object stored by this annotation.
     *
     * <p>The contained {@code Atom} objects are still represented through the
     * pointer values stored by the list. This operation therefore clones the
     * collection rather than recursively cloning every annotation value.
     *
     * @return				    a pointer to a cloned list containing the annotation
     * 					        value entries
     */
    fun getValue() -> pointer<ArrayList> = this.value.clone()


    /**
     * Adds an additional syntax token to this annotation.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The token is appended to the internal extra-token collection and is
     * stored by reference rather than cloned.
     *
     * <p>Tokens added through this method are included when
     * {@code getAllTokens()} reconstructs the complete token set associated with
     * this annotation.
     *
     * @param token			    a pointer to the syntax token to add
     *
     * @return				    this {@code Annotation} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<Annotation>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Returns a copy of the additional syntax-token collection.
     *
     * <p>The internal token list is cloned before being returned. The returned
     * list can therefore be structurally modified without directly replacing or
     * resizing the list stored by this annotation.
     *
     * <p>The individual {@code Token} objects referenced by the collection are
     * not recursively cloned.
     *
     * @return				    a pointer to a cloned list containing the additional
     * 					        syntax tokens associated with this annotation
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    /**
     * Returns all tokens associated with this annotation.
     *
     * <p>The additional syntax tokens stored directly by the annotation are
     * added to the result first.
     *
     * <p>If a qualified annotation name is available, all tokens returned by
     * {@code QualifiedName.getAllTokens()} are also included.
     *
     * <p>The value list is then traversed in order. Each entry is interpreted as
     * a pointer slot containing an {@code Atom}. Null slots and null atom
     * references are ignored. For every valid atom, all tokens returned by
     * {@code Atom.getAllTokens()} are appended to the result.
     *
     * <p>After tokens from all components have been collected, the complete
     * result is sorted according to source position using
     * {@code TokenPosition.compareToken}. This restores the original lexical
     * order regardless of the order in which tokens were gathered from the
     * different AST components.
     *
     * <p>A new list is allocated for the result. The contained token objects are
     * referenced rather than recursively cloned.
     *
     * @return				    a newly allocated list containing all tokens
     * 					        associated with this annotation in source order
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
     * Returns the textual representation of this annotation.
     *
     * <p>The generated representation always begins with the {@code @} marker.
     * If a qualified name is available, its textual representation is appended
     * immediately afterward.
     *
     * <p>If {@code hasParentheses} is {@code false}, the method returns at that
     * point, producing a representation equivalent to:
     *
     * <pre>
     * @Name
     * </pre>
     *
     * <p>If {@code hasParentheses} is {@code true}, an argument list is emitted
     * inside parentheses. Valid atom values are written in their stored order
     * and separated by {@code ", "}. Null value slots and null atom references
     * are skipped and do not produce separators.
     *
     * <p>An empty value list still produces a pair of parentheses when
     * {@code hasParentheses} is {@code true}. This preserves the structural
     * difference between {@code @Name} and {@code @Name()}.
     *
     * <p>The returned {@code StringBuilder} is newly allocated and modifying its
     * contents does not modify the annotation AST node.
     *
     * @return				    a pointer to a newly created {@code StringBuilder}
     * 					        containing the textual representation of this
     * 					        annotation
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder("@")

        if this.name != null:
            sb.append(this.name.toString())

        if !this.hasParentheses:
            return sb

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
 * Represents an optional collection of annotations.
 *
 * <p>{@code AnnotationsMaybe} provides a lightweight wrapper around an
 * annotation list for parser productions in which annotations may or may not
 * be present.
 *
 * <p>Instead of representing the absence of annotations with a {@code null}
 * list, this structure normalizes the state to an empty {@code ArrayList}.
 * Consumers can therefore retrieve an annotation collection without separately
 * handling the absence of the list itself.
 */
struct AnnotationsMaybe
{
    /**
     * The collection of annotations represented by this wrapper.
     *
     * <p>This field is initialized to an empty collection when no annotation
     * list is supplied.
     */
    private var annotations: pointer<ArrayList>


    /**
     * Creates an empty optional annotation collection.
     *
     * <p>A new empty {@code ArrayList} is allocated to represent the absence of
     * annotations without using a {@code null} collection.
     */
    constructor():
        this.annotations = new ArrayList(sizeof(Annotation))


    /**
     * Creates an optional annotation collection from the specified list.
     *
     * <p>If {@code annotations} is {@code null}, a new empty collection is
     * allocated. Otherwise, the supplied list is stored directly and is not
     * copied or cloned.
     *
     * <p>This normalization ensures that the internal annotation collection is
     * always represented by an {@code ArrayList} after construction.
     *
     * @param annotations		a pointer to the annotation list, or {@code null}
     * 					        to represent an empty annotation collection
     */
    constructor(annotations: pointer<ArrayList>):
        this.annotations =
            if annotations == null:
                new ArrayList(sizeof(Annotation))
            else:
                annotations


    /**
     * Returns the annotation collection represented by this wrapper.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Because both constructors normalize missing annotations to an empty
     * list, this method normally returns a valid collection even when no
     * annotations were present in the parsed source.
     *
     * <p>Modifications made through the returned list therefore affect the same
     * collection referenced internally by this {@code AnnotationsMaybe}
     * instance.
     *
     * @return				    a pointer to the internally stored annotation
     * 					        collection
     */
    fun toAnnotations() -> pointer<ArrayList> = this.annotations
}