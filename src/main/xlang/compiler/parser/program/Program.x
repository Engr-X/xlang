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
 * Represents the root node of a parsed source program.
 *
 * <p>A {@code Program} combines all top-level components of a source file,
 * including preprocessing settings, an optional package declaration, import
 * declarations, top-level members, and additional syntax tokens retained during
 * parsing.
 *
 * <p>Preprocessing settings and import declarations are normalized to empty
 * collections when no corresponding values are supplied. The package
 * declaration remains nullable so that the absence of an explicit package can
 * be represented directly.
 *
 * <p>Top-level declarations are stored as {@code Member} objects. A member may
 * represent a field, function, structure constructor, structure declaration, or
 * another member kind supported by {@code Member}.
 *
 * <p>The program also provides convenience operations for collecting specific
 * categories of members, retrieving individual members, reconstructing the
 * complete source-token sequence, and generating a textual representation of
 * the parsed program.
 */
struct Program
{
    /**
     * The ordered collection of preprocessing settings declared before the main
     * program contents.
     *
     * <p>The collection is initialized to an empty list by the constructor and
     * may later be replaced using {@code setPreprocessSettings()}.
     */
    private var preprocessSettings: pointer<ArrayList>


    /**
     * The optional package declaration associated with this program.
     *
     * <p>A {@code null} value indicates that the source program does not contain
     * an explicit package declaration.
     */
    private var packageDeclaration: pointer<PackageDeclaration>


    /**
     * The ordered collection of import declarations associated with this
     * program.
     *
     * <p>The collection is initialized to an empty list by the constructor and
     * may later be replaced using {@code setImportDeclarations()}.
     */
    private var importDeclarations: pointer<ArrayList>

    /**
     * The ordered collection of top-level program members.
     *
     * <p>The list object itself is assigned only during construction, but its
     * contents remain mutable and may be extended through {@code addMember()}
     * and {@code addMembers()}.
     */
    private val members: pointer<ArrayList>

    /**
     * Additional syntax tokens associated with the program as a whole.
     *
     * <p>This collection may contain tokens that are not owned by preprocessing
     * settings, package declarations, imports, or individual program members.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates a program containing the specified top-level member collection.
     *
     * <p>A new empty preprocessing-setting list and import-declaration list are
     * allocated. The package declaration is initialized to {@code null}.
     *
     * <p>If {@code members} is {@code null}, a new empty member collection is
     * allocated. Otherwise, the supplied list is stored directly and is not
     * copied or cloned.
     *
     * <p>A new empty collection is also allocated for additional syntax tokens
     * associated with the program.
     *
     * @param members           a pointer to the initial top-level member
     *                          collection, or {@code null} to create an empty
     *                          member list
     */
    constructor(members: pointer<ArrayList>)
    {
        this.preprocessSettings = new ArrayList(sizeof(PreprocessSetting))
        this.packageDeclaration = null
        this.importDeclarations = new ArrayList(sizeof(ImportDeclaration))
        this.members = if members == null:
                new ArrayList(sizeof(Member))
            else:
                members

        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Collects all top-level structure declarations contained in this program.
     *
     * <p>The member collection is traversed in source order. Each valid
     * {@code Member} is inspected using {@code Member.isStruct()}, and members
     * that represent structures are added to the result.
     *
     * <p>Members that are {@code null} or represent another member kind are
     * ignored.
     *
     * <p>A new collection is allocated for the result, so adding or removing
     * entries from the returned list does not modify the program's original
     * member collection.
     *
     * @return                  a newly allocated list containing all top-level
     *                          structure declarations represented by the program
     */
    fun collectStructs() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Struct))

        for (var i = 0; i < this.members.length; i++):
        {
            val member: pointer<Member> = this.get(i)

            if member != null && member.isStruct():
                result.push(member as pointer<Struct>)
        }

        return result
    }


    /**
     * Collects all top-level function declarations contained in this program.
     *
     * <p>The program member list is traversed in its stored order. Each valid
     * member is tested using {@code Member.isFunction()}, and matching entries
     * are added to a newly allocated result collection.
     *
     * <p>Null members and members representing fields, structures, constructors,
     * or other declaration kinds are ignored.
     *
     * <p>The returned collection is independent from the program's internal
     * member list, although the contained declaration objects are represented by
     * references rather than recursively cloned.
     *
     * @return                  a newly allocated list containing all top-level
     *                          function declarations represented by the program
     */
    fun collectFunctions() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Function))

        for (var i = 0; i < this.members.length; i++):
        {
            val member: pointer<Member> = this.get(i)

            if member != null && member.isFunction():
                result.push(member as pointer<Function>)
        }

        return result
    }


    /**
     * Collects all top-level field declarations contained in this program.
     *
     * <p>The program member collection is traversed in source order. Each valid
     * member is inspected using {@code Member.isField()}, and matching members
     * are added to the result collection.
     *
     * <p>Null members and members representing functions, structures,
     * constructors, or other declaration kinds are ignored.
     *
     * <p>A new result list is allocated and changes to that list do not modify
     * the program's internal member collection.
     *
     * @return                  a newly allocated list containing all top-level
     *                          field declarations represented by the program
     */
    fun collectFields() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Field))

        for (var i = 0; i < this.members.length; i++):
        {
            val member: pointer<Member> = this.get(i)

            if member != null && member.isField():
                result.push(member as pointer<Variable>)
        }

        return result
    }


    /**
     * Returns the preprocessing-setting collection associated with this program.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Modifications performed through the returned list therefore affect the
     * same preprocessing-setting collection referenced by this program.
     *
     * @return                  a pointer to the internally stored preprocessing-
     *                          setting collection
     */
    fun getPreprocessSettings() -> pointer<ArrayList> = this.preprocessSettings


    /**
     * Replaces the preprocessing-setting collection associated with this
     * program.
     *
     * <p>If {@code settings} is {@code null}, a new empty collection is
     * allocated. Otherwise, the supplied list is stored directly and is not
     * copied or cloned.
     *
     * <p>This normalization ensures that the internal preprocessing-setting
     * collection remains represented by a valid {@code ArrayList}.
     *
     * @param settings          a pointer to the preprocessing-setting
     *                          collection, or {@code null} to use an empty list
     *
     * @return                  this {@code Program} instance
     */
    fun setPreprocessSettings(settings: pointer<ArrayList>) -> pointer<Program>
    {
        this.preprocessSettings = if settings == null:
                new ArrayList(sizeof(PreprocessSetting))
            else:
                settings

        return this
    }


    /**
     * Returns the package declaration associated with this program.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code PackageDeclaration} object and is not copied or cloned.
     *
     * <p>The result is {@code null} when the source program does not contain an
     * explicit package declaration.
     *
     * @return                  a pointer to the package declaration, or
     *                          {@code null} if no package declaration is present
     */
    fun getPackageDeclaration() -> pointer<PackageDeclaration> = this.packageDeclaration


    /**
     * Replaces the package declaration associated with this program.
     *
     * <p>The supplied declaration is stored by reference and is not copied or
     * cloned.
     *
     * <p>Passing {@code null} removes the current package declaration and
     * represents a program without an explicit package.
     *
     * @param packageDeclaration
     *                          a pointer to the package declaration, or
     *                          {@code null} to remove the current declaration
     *
     * @return                  this {@code Program} instance
     */
    fun setPackageDeclaration(packageDeclaration: pointer<PackageDeclaration>) -> pointer<Program>
    {
        this.packageDeclaration = packageDeclaration
        return this
    }


    /**
     * Returns the import-declaration collection associated with this program.
     *
     * <p>The returned pointer refers directly to the internally stored list and
     * is not copied or cloned.
     *
     * <p>Changes made through the returned collection therefore affect the same
     * import list referenced by this program.
     *
     * @return                  a pointer to the internally stored import-
     *                          declaration collection
     */
    fun getImportDeclarations() -> pointer<ArrayList> = this.importDeclarations


    /**
     * Replaces the import-declaration collection associated with this program.
     *
     * <p>If {@code imports} is {@code null}, a new empty import-declaration list
     * is allocated. Otherwise, the supplied collection is stored directly and
     * is not copied or cloned.
     *
     * <p>This normalization allows later code to work with an import collection
     * without requiring a separate null-list representation.
     *
     * @param imports           a pointer to the import-declaration collection,
     *                          or {@code null} to replace it with an empty list
     *
     * @return                  this {@code Program} instance
     */
    fun setImportDeclarations(imports: pointer<ArrayList>) -> pointer<Program>
    {
        this.importDeclarations = if imports == null:
                new ArrayList(sizeof(ImportDeclaration))
            else:
                imports

        return this
    }


    /**
     * Appends a top-level member to this program.
     *
     * <p>If {@code member} is {@code null}, no modification is performed.
     *
     * <p>A valid member is appended to the end of the internal member
     * collection, preserving the order in which top-level declarations are
     * added.
     *
     * <p>The member is stored by reference and is not copied or cloned.
     *
     * @param member            a pointer to the top-level member to append
     *
     * @return                  this {@code Program} instance
     */
    fun addMember(member: pointer<Member>) -> pointer<Program>
    {
        if member != null:
            this.members.push(member)

        return this
    }


    /**
     * Appends all entries from the specified member collection to this program.
     *
     * <p>If {@code members} is {@code null}, no modification is performed.
     *
     * <p>Otherwise, all entries are appended to the internal member collection
     * in their existing order using {@code ArrayList.pushAll()}.
     *
     * <p>The supplied collection itself is not modified and its member objects
     * are not recursively cloned.
     *
     * @param members           a pointer to the member collection to append
     *
     * @return                  this {@code Program} instance
     */
    fun addMembers(members: pointer<ArrayList>) -> pointer<Program>
    {
        if members != null:
            this.members.pushAll(members)

        return this
    }


    /**
     * Returns the number of top-level members currently stored by this program.
     *
     * <p>The returned value corresponds directly to the length of the internal
     * member collection.
     *
     * @return                  the number of top-level program members
     */
    fun length() -> int = this.members.length


    /**
     * Returns the program member stored at the specified index.
     *
     * <p>The index is validated before the internal collection is accessed. A
     * negative index or an index greater than or equal to the number of stored
     * members produces {@code null}.
     *
     * <p>For a valid index, the member object is returned directly and is not
     * copied or cloned.
     *
     * @param index             the zero-based index of the member to retrieve
     *
     * @return                  a pointer to the member at the specified index,
     *                          or {@code null} if the index is outside the valid
     *                          range
     */
    fun get(index: int) -> pointer<Member>
    {
        if index < 0 || index >= this.members.length:
            return null

        return this.members.get(index) as pointer<Member>
    }


    /**
     * Returns a copy of the top-level member collection.
     *
     * <p>The internal {@code ArrayList} is cloned before being returned, so
     * structural modifications to the returned collection do not directly
     * replace or resize the member list stored by this program.
     *
     * <p>The individual {@code Member} objects contained by the list are not
     * recursively cloned.
     *
     * @return                  a pointer to a cloned list containing the
     *                          top-level members of this program
     */
    fun getMembers() -> pointer<ArrayList> = this.members


    /**
     * Adds an additional syntax token to this program.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The supplied token is appended to the program-level extra-token
     * collection and is stored by reference rather than cloned.
     *
     * <p>Tokens added through this method are included when
     * {@code getAllTokens()} constructs the complete token collection for the
     * program.
     *
     * @param token             a pointer to the syntax token to add
     *
     * @return                  this {@code Program} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<Program>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Returns a copy of the additional program-level syntax-token collection.
     *
     * <p>The internal {@code ArrayList} is cloned before being returned, so
     * structural modifications to the returned list do not directly modify the
     * collection stored by this program.
     *
     * <p>The individual {@code Token} objects referenced by the list are not
     * recursively cloned.
     *
     * @return                  a pointer to a cloned list containing the
     *                          additional syntax tokens associated with this
     *                          program
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    /**
     * Returns all tokens associated with this program and its child AST nodes.
     *
     * <p>The preprocessing-setting collection is traversed first. For each valid
     * {@code PreprocessSetting}, all tokens returned by
     * {@code PreprocessSetting.getAllTokens()} are appended to the result.
     * Null setting entries are ignored.
     *
     * <p>If a package declaration is present, all tokens associated with that
     * declaration are then included.
     *
     * <p>The import-declaration collection is traversed next. Tokens from each
     * valid {@code ImportDeclaration} are appended while null entries are
     * ignored.
     *
     * <p>All top-level program members are then traversed in their stored order.
     * For every valid member, the collection returned by
     * {@code Member.getAllTokens()} is appended when that collection is not
     * {@code null}.
     *
     * <p>After tokens from all structural AST components have been collected,
     * the program-level additional syntax tokens are appended.
     *
     * <p>The complete result is sorted according to source position using
     * {@code TokenPosition.compareToken}. This restores the original lexical
     * ordering even though tokens are collected independently from preprocessing
     * settings, the package declaration, imports, members, and the program
     * itself.
     *
     * <p>A new list is allocated for the result. The individual token objects
     * are referenced rather than recursively cloned.
     *
     * @return                  a newly allocated list containing all tokens
     *                          associated with this program in source order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = this.extraTokens.clone()

        if this.preprocessSettings != null:
        {
            for (var i = 0; i < this.preprocessSettings.length; i++):
            {
                val setting: pointer<PreprocessSetting> =
                    this.preprocessSettings.get(i) as pointer<PreprocessSetting>

                if setting != null:
                    result.pushAll(setting.getAllTokens())
            }
        }

        if this.packageDeclaration != null:
            result.pushAll(this.packageDeclaration.getAllTokens())

        if this.importDeclarations != null:
        {
            for (var i = 0; i < this.importDeclarations.length; i++):
            {
                val importDeclaration: pointer<ImportDeclaration> =
                    this.importDeclarations.get(i) as pointer<ImportDeclaration>

                if importDeclaration != null:
                    result.pushAll(importDeclaration.getAllTokens())
            }
        }

        for (var i = 0; i < this.members.length; i++):
        {
            val member: pointer<Member> = this.get(i)

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
     * Returns the textual representation of the complete program.
     *
     * <p>The program is emitted as a sequence of logical source sections.
     * Preprocessing settings are written first in their stored order. Null
     * settings are skipped.
     *
     * <p>If a package declaration is present, it is emitted after preprocessing
     * settings.
     *
     * <p>Import declarations are emitted after the package declaration. Valid
     * imports are written in their stored order while null entries are skipped.
     *
     * <p>Top-level members are emitted last. Each valid member contributes the
     * textual representation returned by {@code Member.toString()}.
     *
     * <p>The {@code appendedSection} flag records whether any previous section
     * has already contributed text. A newline is inserted before subsequent
     * sections so that independently generated declarations do not run together.
     *
     * <p>Within the preprocessing-setting collection, each valid setting after
     * the first emitted section is separated by a newline. Import declarations
     * are likewise separated according to their position in the import list.
     * Each valid top-level member is separated from preceding program contents
     * by a newline.
     *
     * <p>If the program contains no valid preprocessing settings, package
     * declaration, imports, or members, the returned builder remains empty.
     *
     * <p>The returned {@code StringBuilder} is newly allocated and modifying its
     * contents does not modify the underlying program AST.
     *
     * @return                  a pointer to a newly created {@code StringBuilder}
     *                          containing the textual representation of this
     *                          program
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()
        var appendedSection: bool = false

        if this.preprocessSettings != null && this.preprocessSettings.length > 0:
        {
            for (var i = 0; i < this.preprocessSettings.length; i++):
            {
                val setting: pointer<PreprocessSetting> =
                    this.preprocessSettings.get(i) as pointer<PreprocessSetting>

                if setting == null:
                    continue

                if appendedSection:
                    sb.newline()

                sb.append(setting.toString())
                appendedSection = true
            }
        }

        if this.packageDeclaration != null:
        {
            if appendedSection:
                sb.newline()

            sb.append(this.packageDeclaration.toString())
            appendedSection = true
        }

        if this.importDeclarations != null && this.importDeclarations.length > 0:
        {
            if appendedSection:
                sb.newline()

            for (var i = 0; i < this.importDeclarations.length; i++):
            {
                val importDeclaration: pointer<ImportDeclaration> =
                    this.importDeclarations.get(i) as pointer<ImportDeclaration>

                if importDeclaration == null:
                    continue

                if i > 0:
                    sb.newline()

                sb.append(importDeclaration.toString())
            }

            appendedSection = true
        }

        for (var i = 0; i < this.members.length; i++):
        {
            val member: pointer<Member> = this.get(i)

            if member == null:
                continue

            if appendedSection:
                sb.newline()

            sb.append(member.toString())
            appendedSection = true
        }

        return sb
    }
}
