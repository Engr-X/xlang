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

import xlang.compiler.parser.expression.Expression
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.HashSet
import xlang.util.string.StringBuilder


/**
 * Represents a constructor declared inside a structure.
 *
 * <p>A {@code StructConstructor} stores declaration modifiers, an ordered
 * function-parameter list, an optional body expression, and additional syntax
 * tokens associated with the constructor declaration.
 *
 * <p>The parameter collection is normalized to an empty
 * {@code FunctionParams} instance when no parameter object is supplied. The
 * body expression remains nullable so that a constructor without a parsed body
 * can still be represented.
 *
 * <p>Declaration modifiers are stored separately from the parameter list and
 * body expression. Additional syntax tokens may contain the
 * {@code constructor} keyword, parentheses, assignment syntax, or other lexical
 * elements not directly owned by the child AST nodes.
 *
 * <p>All tokens belonging to the constructor can be collected in source order
 * using {@code getAllTokens()}.
 */
struct StructConstructor
{
    /**
     * The set of declaration modifiers attached to this
     * constructor.
     *
     * <p>The collection is initialized to an empty set during construction and
     * may later be replaced through {@code setModifiers()}.
     */
    private var modifiers: pointer<HashSet>


    /**
     * The parameter collection declared by this constructor.
     *
     * <p>The constructor normalizes a missing parameter object to an empty
     * {@code FunctionParams} instance.
     */
    private var params: pointer<FunctionParams>


    /**
     * The optional expression forming the body of this constructor.
     *
     * <p>A {@code null} value indicates that no body expression is currently
     * associated with the constructor.
     */
    private var bodyExpr: pointer<Expression>


    /**
     * Additional syntax tokens associated with this constructor declaration.
     *
     * <p>This collection may contain the constructor keyword, parentheses,
     * assignment syntax, or other tokens not directly represented by the
     * parameter list or body expression.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates a structure constructor with the specified parameter collection
     * and body expression.
     *
     * <p>A new empty modifier collection is allocated.
     *
     * <p>If {@code params} is {@code null}, a new empty
     * {@code FunctionParams} instance is allocated. Otherwise, the supplied
     * parameter object is stored directly and is not copied or cloned.
     *
     * <p>The supplied body expression is stored by reference and may be
     * {@code null}.
     *
     * <p>A new empty collection is also allocated for additional syntax tokens.
     *
     * @param params            a pointer to the constructor parameter
     *                          collection, or {@code null} to create an empty
     *                          parameter collection
     * @param bodyExpr          a pointer to the constructor body expression, or
     *                          {@code null} if no body is available
     */
    constructor(params: pointer<FunctionParams>, bodyExpr: pointer<Expression>)
    {
        this.modifiers = new HashSet(sizeof(Modifier), Modifier.compareModifier)
        this.params = if params == null:
                new FunctionParams()
            else:
                params

        this.bodyExpr = bodyExpr
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Returns the declaration-modifier collection associated with this
     * constructor.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code HashSet}. The collection is not copied or cloned.
     *
     * <p>Modifications performed through the returned list affect the same
     * modifier collection referenced by this {@code StructConstructor}.
     *
     * @return                  a pointer to the internally stored modifier
     *                          collection
     */
    fun getModifiers() -> pointer<HashSet> = this.modifiers


    /**
     * Replaces the declaration-modifier collection associated with this
     * constructor.
     *
     * <p>If {@code modifiers} is {@code null}, a new empty modifier set is
     * allocated. Otherwise, the supplied set is stored directly and is not
     * copied or cloned.
     *
     * <p>This normalization ensures that the constructor retains a valid
     * modifier collection after the operation.
     *
     * @param modifiers         a pointer to the declaration-modifier
     *                          collection, or {@code null} to use an empty list
     *
     * @return                  this {@code StructConstructor} instance
     */
    fun setModifiers(modifiers: pointer<HashSet>) -> pointer<StructConstructor>
    {
        this.modifiers = if modifiers == null:
                new HashSet(sizeof(Modifier), Modifier.compareModifier)
            else:
                modifiers

        return this
    }


    /**
     * Returns the parameter collection declared by this constructor.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code FunctionParams} object and is not copied or cloned.
     *
     * <p>Because the constructor normalizes a {@code null} parameter object to
     * an empty {@code FunctionParams}, this method normally returns a valid
     * parameter collection.
     *
     * @return                  a pointer to the internally stored constructor
     *                          parameter collection
     */
    fun getParams() -> pointer<FunctionParams> = this.params


    /**
     * Returns the body expression associated with this constructor.
     *
     * <p>The returned pointer refers directly to the internally stored
     * expression and is not copied or cloned.
     *
     * <p>The result is {@code null} when no constructor body is available.
     *
     * @return                  a pointer to the constructor body expression, or
     *                          {@code null} if no body is present
     */
    fun getBodyExpr() -> pointer<Expression> = this.bodyExpr


    /**
     * Adds an additional syntax token to this constructor declaration.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The supplied token is appended to the internal extra-token collection
     * and is stored by reference rather than cloned.
     *
     * <p>Tokens added through this method are included when
     * {@code getAllTokens()} constructs the complete token collection associated
     * with the constructor.
     *
     * @param token             a pointer to the syntax token to add
     *
     * @return                  this {@code StructConstructor} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<StructConstructor>
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
     * replace or resize the list stored by this constructor.
     *
     * <p>The individual {@code Token} objects referenced by the list are not
     * recursively cloned.
     *
     * @return                  a pointer to a cloned list containing the
     *                          additional syntax tokens associated with this
     *                          constructor
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    /**
     * Returns all tokens associated with this structure constructor.
     *
     * <p>The modifier collection is traversed first. For each valid
     * {@code Modifier}, all tokens returned by {@code Modifier.getAllTokens()}
     * are appended to the result. Null modifier entries are ignored.
     *
     * <p>If a parameter collection is available, all tokens returned by
     * {@code FunctionParams.getAllTokens()} are then included.
     *
     * <p>If a body expression is present, all tokens returned by
     * {@code Expression.getAllTokens()} are appended.
     *
     * <p>The tokens collected from these structural components are combined with
     * the additional syntax tokens stored directly by this constructor.
     *
     * <p>The complete collection is sorted according to source position using
     * {@code TokenPosition.compareToken}. This restores the original lexical
     * ordering even though tokens are collected independently from modifiers,
     * parameters, the body expression, and the constructor itself.
     *
     * <p>A new list is allocated for the result. The individual token objects
     * are referenced rather than recursively cloned.
     *
     * @return                  a newly allocated list containing all tokens
     *                          associated with this constructor in source order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.modifiers != null:
        {
            for (var i = 0; i < this.modifiers.length; i++):
            {
                val modifier: pointer<Modifier> = this.modifiers.get(i) as pointer<Modifier>

                if modifier != null:
                    result.pushAll(modifier.getAllTokens())
            }
        }

        if this.params != null:
            result.pushAll(this.params.getAllTokens())

        if this.bodyExpr != null:
            result.pushAll(this.bodyExpr.getAllTokens())

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns the textual representation of this structure constructor.
     *
     * <p>Declaration modifiers are emitted first in their stored order. Null
     * modifier entries are skipped, and valid modifiers are separated using a
     * single space. If at least one modifier is emitted, another space is added
     * before the constructor declaration.
     *
     * <p>The {@code constructor} keyword is followed by parentheses containing
     * the textual representation of the parameter collection.
     *
     * <p>The method then always appends {@code " = "}. If a body expression is
     * available, its textual representation is appended after that separator.
     *
     * <p>A typical result therefore has the form:
     *
     * <pre>
     * modifier constructor(param1: Type1, param2: Type2) = body
     * </pre>
     *
     * <p>If no body expression is present, the current implementation still
     * leaves the representation ending with {@code " = "}.
     *
     * <p>The returned {@code StringBuilder} is newly allocated and modifying its
     * contents does not modify the underlying constructor AST node.
     *
     * @return                  a pointer to a newly created {@code StringBuilder}
     *                          containing the textual representation of this
     *                          structure constructor
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

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

        sb.append("constructor(")

        if this.params != null:
            sb.append(this.params.toString())

        sb.append(") = ")

        if this.bodyExpr != null:
            sb.append(this.bodyExpr.toString())

        return sb
    }
}


/**
 * Represents a structure declaration in the program abstract syntax tree.
 *
 * <p>A {@code Struct} stores its annotations, declaration modifiers, structure
 * name, ordered member collection, and additional syntax tokens retained from
 * the original source.
 *
 * <p>Structure members are represented through {@code Member} wrappers and may
 * therefore contain fields, functions, constructors, nested structures, or
 * other member kinds supported by the language.
 *
 * <p>The member collection is normalized to an empty list when no collection is
 * supplied to the constructor. Annotations and declaration modifiers are also
 * initialized as empty collections.
 *
 * <p>All tokens belonging to the structure and its child declarations can be
 * collected in lexical source order using {@code getAllTokens()}.
 */
struct Struct
{
    /**
     * The ordered collection of annotations attached to this structure.
     *
     * <p>The collection is initialized to an empty list and may later be
     * replaced using {@code setAnnotations()}.
     */
    private var annotations: pointer<ArrayList>

    /**
     * The set of declaration modifiers attached to this
     * structure.
     *
     * <p>The collection is initialized to an empty set and may later be
     * replaced using {@code setModifiers()}.
     */
    private var modifiers: pointer<HashSet>

    /**
     * The null-terminated name of this structure.
     *
     * <p>The supplied character pointer is stored directly by the constructor
     * and is not duplicated or cloned.
     */
    private var structName: pointer<char>

    /**
     * The ordered collection of members declared inside this structure.
     *
     * <p>The list object is assigned during construction and remains associated
     * with this structure, while its contents may be extended using
     * {@code addMember()} and {@code addMembers()}.
     */
    private val members: pointer<ArrayList>

    /**
     * Additional syntax tokens associated with this structure declaration.
     *
     * <p>This collection may contain the {@code struct} keyword, braces,
     * punctuation, separators, or other lexical elements not directly owned by
     * the child AST nodes.
     */
    private var extraTokens: pointer<ArrayList>

    /**
     * Creates a structure declaration with the specified name and member
     * collection.
     *
     * <p>New empty annotation and declaration-modifier collections are
     * allocated.
     *
     * <p>The supplied structure name is stored directly and is not duplicated or
     * cloned.
     *
     * <p>If {@code members} is {@code null}, a new empty member collection is
     * allocated. Otherwise, the supplied list is stored directly and is not
     * copied or cloned.
     *
     * <p>A new empty collection is also allocated for additional syntax tokens.
     *
     * @param structName        a pointer to the null-terminated structure name
     * @param members           a pointer to the initial structure-member
     *                          collection, or {@code null} to create an empty
     *                          member list
     */
    constructor(structName: pointer<char>, members: pointer<ArrayList>)
    {
        this.annotations = new ArrayList(sizeof(Annotation))
        this.modifiers = new HashSet(sizeof(Modifier), Modifier.compareModifier)
        this.structName = structName
        this.members = if members == null:
                new ArrayList(sizeof(Member))
            else:
                members

        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Returns the annotation collection attached to this structure.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Modifications performed through the returned list affect the same
     * annotation collection referenced internally by this structure.
     *
     * @return                  a pointer to the internally stored annotation
     *                          collection
     */
    fun getAnnotations() -> pointer<ArrayList> = this.annotations


    /**
     * Replaces the annotation collection attached to this structure.
     *
     * <p>If {@code annotations} is {@code null}, a new empty annotation
     * collection is allocated. Otherwise, the supplied list is stored directly
     * and is not copied or cloned.
     *
     * <p>This normalization ensures that the structure retains a valid
     * annotation list after the operation.
     *
     * @param annotations       a pointer to the annotation collection, or
     *                          {@code null} to replace it with an empty list
     *
     * @return                  this {@code Struct} instance
     */
    fun setAnnotations(annotations: pointer<ArrayList>) -> pointer<Struct>
    {
        this.annotations = if annotations == null:
                new ArrayList(sizeof(Annotation))
            else:
                annotations

        return this
    }


    /**
     * Returns the declaration-modifier collection attached to this structure.
     *
     * <p>The returned pointer refers directly to the internally stored set and
     * is not copied or cloned.
     *
     * <p>Changes made through the returned collection therefore affect the same
     * modifier set referenced by this structure.
     *
     * @return                  a pointer to the internally stored modifier
     *                          collection
     */
    fun getModifiers() -> pointer<HashSet> = this.modifiers


    /**
     * Replaces the declaration-modifier collection attached to this structure.
     *
     * <p>If {@code modifiers} is {@code null}, a new empty modifier set is
     * allocated. Otherwise, the supplied set is stored directly and is not
     * copied or cloned.
     *
     * @param modifiers         a pointer to the declaration-modifier
     *                          collection, or {@code null} to use an empty list
     *
     * @return                  this {@code Struct} instance
     */
    fun setModifiers(modifiers: pointer<HashSet>) -> pointer<Struct>
    {
        this.modifiers = if modifiers == null:
                new HashSet(sizeof(Modifier), Modifier.compareModifier)
            else:
                modifiers

        return this
    }


    /**
     * Returns the name of this structure.
     *
     * <p>The returned pointer refers directly to the internally stored character
     * sequence and is not duplicated or cloned.
     *
     * <p>The result may be {@code null} if the structure was created without a
     * valid name.
     *
     * @return                  a pointer to the internally stored structure name,
     *                          or {@code null} if no name is available
     */
    fun getStructName() -> pointer<char> = this.structName


    /**
     * Appends a member to this structure.
     *
     * <p>If {@code member} is {@code null}, no modification is performed.
     *
     * <p>A valid member is appended to the end of the internal member collection
     * and is stored by reference rather than cloned.
     *
     * <p>The order in which members are added is preserved by the collection.
     *
     * @param member            a pointer to the structure member to append
     *
     * @return                  this {@code Struct} instance
     */
    fun addMember(member: pointer<Member>) -> pointer<Struct>
    {
        if member != null:
            this.members.push(member)

        return this
    }


    /**
     * Appends all members from the specified collection to this structure.
     *
     * <p>If {@code members} is {@code null}, no modification is performed.
     *
     * <p>Otherwise, all entries are appended to the internal member collection
     * in their existing order using {@code ArrayList.pushAll()}.
     *
     * <p>The supplied collection is not modified and the contained member
     * objects are not recursively cloned.
     *
     * @param members           a pointer to the member collection to append
     *
     * @return                  this {@code Struct} instance
     */
    fun addMembers(members: pointer<ArrayList>) -> pointer<Struct>
    {
        if members != null:
            this.members.pushAll(members)

        return this
    }


    /**
     * Returns a copy of the member collection declared by this structure.
     *
     * <p>The internal {@code ArrayList} is cloned before being returned, so
     * structural modifications to the returned collection do not directly
     * replace or resize the list stored by this structure.
     *
     * <p>The individual {@code Member} objects referenced by the collection are
     * not recursively cloned.
     *
     * @return                  a pointer to a cloned list containing the
     *                          structure members
     */
    fun getMembers() -> pointer<ArrayList> = this.members.clone()


    /**
     * Adds an additional syntax token to this structure declaration.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The supplied token is appended to the internal extra-token collection
     * and is stored by reference rather than cloned.
     *
     * <p>Tokens added through this method are included by
     * {@code getAllTokens()} when the complete token collection for the
     * structure is constructed.
     *
     * @param token             a pointer to the syntax token to add
     *
     * @return                  this {@code Struct} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<Struct>
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
     * collection stored by this structure.
     *
     * <p>The individual {@code Token} objects referenced by the collection are
     * not recursively cloned.
     *
     * @return                  a pointer to a cloned list containing the
     *                          additional syntax tokens associated with this
     *                          structure
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    /**
     * Returns all tokens associated with this structure declaration and its
     * child members.
     *
     * <p>The annotation collection is traversed first. For each valid
     * {@code Annotation}, all tokens returned by
     * {@code Annotation.getAllTokens()} are appended to the result. Null
     * annotation entries are ignored.
     *
     * <p>The declaration-modifier collection is then traversed. Tokens from each
     * valid {@code Modifier} are appended while null entries are skipped.
     *
     * <p>All structure members are subsequently traversed in their stored order.
     * For every valid member, the token collection returned by
     * {@code Member.getAllTokens()} is appended when that collection is not
     * {@code null}.
     *
     * <p>The tokens collected from annotations, modifiers, and members are then
     * combined with the additional syntax tokens stored directly by this
     * structure.
     *
     * <p>The final result is sorted according to source position using
     * {@code TokenPosition.compareToken}. This restores the original lexical
     * ordering of tokens collected independently from the different AST
     * components.
     *
     * <p>A new result list is allocated. The individual token objects are
     * referenced rather than recursively cloned.
     *
     * @return                  a newly allocated list containing all tokens
     *                          associated with this structure in source order
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

        for (var i = 0; i < this.members.length; i++):
        {
            val member: pointer<Member> = this.members.get(i) as pointer<Member>

            if member == null:
                continue

            val tokens: pointer<ArrayList> = member.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns the textual representation of this structure declaration.
     *
     * <p>Annotations are emitted first in their stored order. Each valid
     * annotation is followed by a newline.
     *
     * <p>Declaration modifiers are emitted next. Null modifiers are skipped and
     * valid modifiers are separated by a single space. If at least one modifier
     * is emitted, an additional space is inserted before the {@code struct}
     * keyword.
     *
     * <p>The structure header is then emitted using the {@code struct} keyword,
     * followed by the structure name when one is available, and an opening
     * brace.
     *
     * <p>Each valid member is written on a new line using the representation
     * returned by {@code Member.toString()}. Null members are skipped.
     *
     * <p>After all members have been emitted, another newline is appended,
     * followed by the closing brace. The method does not currently apply
     * indentation to member representations.
     *
     * <p>A typical result therefore has the form:
     *
     * <pre>
     * @Annotation
     * modifier struct Name {
     * member1
     * member2
     * }
     * </pre>
     *
     * <p>If the structure contains no valid members, the current implementation
     * still inserts a newline between the opening and closing braces.
     *
     * <p>The returned {@code StringBuilder} is newly allocated and modifying its
     * contents does not modify the underlying structure AST node.
     *
     * @return                  a pointer to a newly created {@code StringBuilder}
     *                          containing the textual representation of this
     *                          structure declaration
     */
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

        sb.append("struct ")

        if this.structName != null:
            sb.append(this.structName)

        sb.append(" {")

        for (var i = 0; i < this.members.length; i++):
        {
            val member: pointer<Member> = this.members.get(i) as pointer<Member>

            if member == null:
                continue

            sb.newline()
            sb.append(member.toString())
        }

        sb.newline()
        sb.append('}')
        return sb
    }
}
