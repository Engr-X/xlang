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

import xlang.compiler.Type
import xlang.compiler.parser.expression.Expression
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.String
import xlang.util.string.StringBuilder


/**
 * Represents a field declaration in the program abstract syntax tree.
 *
 * <p>A {@code Field} stores the declaration name, optional declared type,
 * optional initial value, annotations, declaration modifiers, mutability state,
 * and additional syntax tokens associated with the field.
 *
 * <p>Field mutability is represented internally by one of the modifier constants
 * defined by this structure. A field is created as constant by default and can
 * later be changed to mutable using {@code markAsMut()}.
 *
 * <p>Annotations and modifiers are maintained as ordered collections and are
 * included in both token collection and textual reconstruction of the
 * declaration.
 *
 * <p>Additional syntax tokens may contain declaration keywords, punctuation,
 * separators, assignment operators, or other source tokens that are not
 * directly owned by the field type, initial value, annotations, or modifiers.
 */
struct Field
{
    /**
     * Internal modifier value representing an immutable field declaration.
     *
     * <p>A field using this modifier is rendered with the {@code val} keyword
     * and {@code canModified()} returns {@code false}.
     */
    private static val CONST_MODIFIER: int = 0

    /**
     * Internal modifier value representing a mutable field declaration.
     *
     * <p>A field using this modifier is rendered with the {@code var} keyword
     * and {@code canModified()} returns {@code true}.
     */
    private static val MUT_MODIFIER: int = 1

    /**
     * The internal mutability modifier of this field.
     *
     * <p>The value is expected to be either {@code CONST_MODIFIER} or
     * {@code MUT_MODIFIER}.
     */
    private var modifier: int

    /**
     * The ordered collection of annotations attached to this field.
     *
     * <p>The collection is initialized to an empty list when the field is
     * created and may later be replaced using {@code setAnnotations()}.
     */
    private var annotations: pointer<ArrayList>


    /**
     * The ordered collection of declaration modifiers attached to this field.
     *
     * <p>These modifiers are separate from the internal mutability flag used to
     * select between {@code val} and {@code var}.
     */
    private var modifiers: pointer<ArrayList>

    /**
     * The null-terminated name of this field.
     *
     * <p>The supplied pointer is stored directly by the constructor and is not
     * duplicated or cloned.
     */
    private var fieldName: pointer<char>

    /**
     * The declared type of this field.
     *
     * <p>The type is stored by reference internally. External callers can obtain
     * a cloned type through {@code getFieldType()}.
     *
     * <p>This value may be {@code null} when the field does not contain an
     * explicit type declaration.
     */
    private var fieldType: pointer<Type>

    /**
     * The optional expression used to initialize this field.
     *
     * <p>The expression is stored by reference and may be {@code null} when no
     * initializer is present.
     */
    private var initialValue: pointer<Expression>

    /**
     * Additional syntax tokens associated with this field declaration.
     *
     * <p>This collection may contain declaration keywords, punctuation,
     * assignment operators, or other syntax tokens that are not directly owned
     * by the structural AST components of the field.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Returns the internal modifier value representing an immutable field.
     *
     * <p>This value is used internally to identify declarations that should be
     * represented with the {@code val} keyword.
     *
     * @return				    the modifier value representing an immutable field
     */
    static fun constModifier() -> int = CONST_MODIFIER


    /**
     * Returns the internal modifier value representing a mutable field.
     *
     * <p>This value is used internally to identify declarations that should be
     * represented with the {@code var} keyword.
     *
     * @return				    the modifier value representing a mutable field
     */
    static fun mutModifier() -> int = MUT_MODIFIER


    /**
     * Creates a field declaration with the specified name and declared type.
     *
     * <p>The field is initialized as immutable by setting its internal modifier
     * to {@code CONST_MODIFIER}.
     *
     * <p>New empty annotation and declaration-modifier collections are
     * allocated. The initial value is set to {@code null}, meaning that the
     * field does not initially contain an initializer expression.
     *
     * <p>The supplied field name and type are stored by reference and are not
     * copied or cloned by the constructor.
     *
     * <p>A new empty collection is also allocated for additional syntax tokens.
     *
     * @param fieldName			a pointer to the null-terminated field name
     * @param fieldType			a pointer to the declared field type, or
     * 					        {@code null} if no explicit type is available
     */
    constructor(fieldName: pointer<char>, fieldType: pointer<Type>)
    {
        this.modifier = CONST_MODIFIER
        this.annotations = new ArrayList(sizeof(Annotation))
        this.modifiers = new ArrayList(sizeof(Modifier))
        this.fieldName = fieldName
        this.fieldType = fieldType
        this.initialValue = null
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Returns the annotation collection associated with this field.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Modifications performed through the returned list therefore affect the
     * same annotation collection referenced by this {@code Field}.
     *
     * @return				    a pointer to the internally stored annotation
     * 					        collection
     */
    fun getAnnotations() -> pointer<ArrayList> = this.annotations


    /**
     * Replaces the annotation collection associated with this field.
     *
     * <p>If {@code annotations} is {@code null}, a new empty annotation list is
     * allocated. Otherwise, the supplied list is stored directly and is not
     * copied or cloned.
     *
     * <p>This normalization ensures that the internal annotation collection is
     * represented by a valid list after this method returns.
     *
     * @param annotations		a pointer to the annotation collection, or
     * 					        {@code null} to replace it with an empty list
     *
     * @return				    this {@code Field} instance
     */
    fun setAnnotations(annotations: pointer<ArrayList>) -> pointer<Field>
    {
        this.annotations = if annotations == null:
                new ArrayList(sizeof(Annotation))
            else:
                annotations

        return this
    }


    /**
     * Returns the declaration-modifier collection associated with this field.
     *
     * <p>The returned pointer refers directly to the internally stored
     * collection and is not copied or cloned.
     *
     * <p>These modifiers are independent from the field's internal mutability
     * state, which determines whether the declaration uses {@code val} or
     * {@code var}.
     *
     * @return				    a pointer to the internally stored modifier
     * 					        collection
     */
    fun getModifiers() -> pointer<ArrayList> = this.modifiers


    /**
     * Adds a declaration modifier to this field.
     *
     * <p>The specified modifier is appended directly to the internally stored
     * modifier collection.
     *
     * <p>This operation modifies the current field instance and returns the
     * same instance, allowing method chaining.
     *
     * @param modifier          a pointer point to modifier to add
     * @return                  a pointer to this field
     */
    fun addModifier(modifier: pointer<Modifier>) -> pointer<Field>
    {
        this.modifiers.push(modifier)
        return this
    }


    /**
     * Replaces the declaration-modifier collection associated with this field.
     *
     * <p>If {@code modifiers} is {@code null}, a new empty modifier list is
     * allocated. Otherwise, the supplied collection is stored directly and is
     * not copied or cloned.
     *
     * <p>The field mutability state represented by {@code modifier} is not
     * changed by this method.
     *
     * @param modifiers			a pointer to the declaration-modifier
     * 					        collection, or {@code null} to use an empty list
     *
     * @return				    this {@code Field} instance
     */
    fun setModifiers(modifiers: pointer<ArrayList>) -> pointer<Field>
    {
        this.modifiers = if modifiers == null:
                new ArrayList(sizeof(Modifier))
            else:
                modifiers

        return this
    }


    /**
     * Returns the name of this field.
     *
     * <p>The returned pointer refers directly to the character sequence stored
     * internally by this field. The name is not duplicated or cloned.
     *
     * <p>The result may be {@code null} if the field was constructed without a
     * valid field name.
     *
     * @return				    a pointer to the internally stored null-terminated
     * 					        field name, or {@code null} if no name is available
     */
    fun getFieldName() -> pointer<char> = this.fieldName


    /**
     * Returns a copy of the declared type associated with this field.
     *
     * <p>If no field type is stored, this method returns {@code null}.
     *
     * <p>When a type is available, {@code Type.clone()} is used to create the
     * returned value. The caller therefore receives a separate type object
     * instead of direct access to the internally stored type reference.
     *
     * @return				    a pointer to a cloned field type, or {@code null}
     * 					        if the field has no declared type
     */
    fun getFieldType() -> pointer<Type> =
        if this.fieldType == null:
            null
        else:
            this.fieldType.clone()


    /**
     * Returns the initializer expression associated with this field.
     *
     * <p>The returned pointer refers directly to the expression stored
     * internally and is not copied or cloned.
     *
     * <p>The result is {@code null} when the field does not contain an
     * initializer.
     *
     * @return				    a pointer to the initializer expression, or
     * 					        {@code null} if no initializer is present
     */
    fun getInitialValue() -> pointer<Expression> = this.initialValue


    /**
     * Marks this field as mutable.
     *
     * <p>The internal modifier is changed to {@code MUT_MODIFIER}. After this
     * operation, {@code canModified()} returns {@code true} and the textual
     * representation produced by {@code toString()} uses the {@code var}
     * keyword.
     *
     * <p>No other field properties are modified.
     *
     * @return				    this {@code Field} instance
     */
    fun markAsMut() -> pointer<Field>
    {
        this.modifier = MUT_MODIFIER
        return this
    }


    /**
     * Marks this field as immutable.
     *
     * <p>The internal modifier is changed to {@code CONST_MODIFIER}. After this
     * operation, {@code canModified()} returns {@code false} and the textual
     * representation produced by {@code toString()} uses the {@code val}
     * keyword.
     *
     * <p>No other field properties are modified.
     *
     * @return				    this {@code Field} instance
     */
    fun markAsConst() -> pointer<Field>
    {
        this.modifier = CONST_MODIFIER
        return this
    }


    /**
     * Returns whether this field is currently marked as mutable.
     *
     * <p>The field is considered mutable only when the internal modifier is
     * exactly equal to {@code MUT_MODIFIER}. Any other value is treated as
     * non-mutable by this method.
     *
     * @return				    {@code true} if the field is mutable;
     * 					        {@code false} otherwise
     */
    fun canModified() -> bool = this.modifier == MUT_MODIFIER


    /**
     * Sets the initializer expression of this field.
     *
     * <p>The supplied expression is stored by reference and is not copied or
     * cloned.
     *
     * <p>Passing {@code null} removes the current initializer and causes the
     * field to be represented without an assignment expression.
     *
     * @param value			    a pointer to the initializer expression, or
     * 					        {@code null} to remove the current initializer
     *
     * @return				    this {@code Field} instance
     */
    fun setInitialValue(value: pointer<Expression>) -> pointer<Field>
    {
        this.initialValue = value
        return this
    }


    /**
     * Adds an additional syntax token to this field declaration.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The token is appended to the internal extra-token collection and is
     * stored by reference rather than cloned.
     *
     * <p>Tokens added through this method are included when
     * {@code getAllTokens()} collects the complete source token set of the
     * field.
     *
     * @param token			    a pointer to the syntax token to add
     *
     * @return				    this {@code Field} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<Field>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Returns a copy of the additional syntax-token collection.
     *
     * <p>The internal {@code ArrayList} is cloned before being returned, so
     * modifications to the returned list structure do not directly resize or
     * replace the collection stored by this field.
     *
     * <p>The contained {@code Token} objects are still represented through the
     * references stored by the list and are not recursively cloned.
     *
     * @return				    a pointer to a cloned list containing the additional
     * 					        syntax tokens associated with this field
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    /**
     * Returns all tokens associated with this field declaration.
     *
     * <p>Tokens belonging to all valid annotations are collected first. Null
     * annotation entries are ignored.
     *
     * <p>The declaration modifiers are then traversed and tokens belonging to
     * each valid modifier are appended to the result. Null modifier entries are
     * ignored.
     *
     * <p>If a declared field type is available, all tokens returned by
     * {@code Type.getAllTokens()} are included. If an initializer expression is
     * present, all tokens returned by {@code Expression.getAllTokens()} are also
     * appended.
     *
     * <p>The collected structural tokens are combined with the additional syntax
     * tokens stored directly by this field.
     *
     * <p>The final collection is sorted according to source position using
     * {@code TokenPosition.compareToken}. This restores the original lexical
     * ordering even though tokens are gathered from several independent AST
     * components.
     *
     * <p>A new list is allocated for the result. The individual token objects
     * are referenced rather than recursively cloned.
     *
     * @return				    a newly allocated list containing all tokens
     * 					        associated with this field in source order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = this.extraTokens.clone()

        if this.annotations != null:
        {
            for (var i = 0; i < this.annotations.length; i++):
            {
                val annotation: pointer<Annotation> = this.annotations.get(i) as pointer<Annotation>

                if annotation != null:
                    result.pushAll(annotation.getAllTokens())
            }
        }

        if this.modifiers != null:
        {
            for (var i = 0; i < this.modifiers.length; i++):
            {
                val modifier: pointer<Modifier> = this.modifiers.get(i) as pointer<Modifier>

                if modifier != null:
                    result.pushAll(modifier.getAllTokens())
            }
        }

        if this.fieldType != null:
            result.pushAll(this.fieldType.getAllTokens())

        if this.initialValue != null:
            result.pushAll(this.initialValue.getAllTokens())

        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        if this.annotations != null && this.annotations.length > 0:
        {
            for (var i = 0; i < this.annotations.length; i++):
            {
                val annotation: pointer<Annotation> = this.annotations.get(i) as pointer<Annotation>

                if annotation != null:
                {
                    sb.append(annotation.toString())
                    sb.newline()
                }
            }
        }

        if this.modifiers != null && this.modifiers.length > 0:
        {
            var appendedModifier: bool = false

            for (var i = 0; i < this.modifiers.length; i++):
            {
                val modifier: pointer<Modifier> = this.modifiers.get(i) as pointer<Modifier>

                if modifier == null:
                    continue

                if appendedModifier:
                    sb.append(' ')

                sb.append(modifier.toString())
                appendedModifier = true
            }

            if appendedModifier:
                sb.append(' ')
        }

        if this.canModified():
            sb.append("var ")
        else:
            sb.append("val ")

        if this.fieldName != null:
            sb.append(this.fieldName)

        if this.fieldType != null:
        {
            sb.append(": ")
            sb.append(this.fieldType.toString())
        }

        if this.initialValue != null:
        {
            sb.append(" = ")
            sb.append(this.initialValue.toString())
        }

        return sb
    }
}
