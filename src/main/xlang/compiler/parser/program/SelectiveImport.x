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
import xlang.util.ArrayList


/**
 * Represents a function imported through a selective import declaration.
 *
 * <p>An {@code ImportedFunction} stores the original function name, the ordered
 * collection of parameter types used to identify the imported function
 * signature, and an optional alias name.
 *
 * <p>The parameter-type collection is normalized to an empty
 * {@code ArrayList} when no collection is supplied.
 *
 * <p>The function name and alias name are stored by reference and are not copied
 * or duplicated by this structure.
 */
struct ImportedFunction
{
    /**
     * The original name of the imported function.
     *
     * <p>The character pointer is stored directly and is not duplicated by the
     * constructor.
     */
    private var name: pointer<char>

    /**
     * The ordered collection of parameter types identifying the imported
     * function signature.
     *
     * <p>The collection is normalized to an empty list when a {@code null}
     * value is supplied to the constructor.
     */
    private var parameterTypes: pointer<ArrayList>

    /**
     * The optional alias assigned to the imported function.
     *
     * <p>A {@code null} value indicates that the function is imported without an
     * explicit alias.
     *
     * <p>The alias character pointer is stored directly and is not duplicated.
     */
    private var aliasName: pointer<char>


    /**
     * Creates an imported-function descriptor with the specified name,
     * parameter types, and optional alias.
     *
     * <p>The supplied function name and alias name are stored directly and are
     * not copied or duplicated.
     *
     * <p>If {@code parameterTypes} is {@code null}, a new empty
     * {@code ArrayList} capable of storing {@code Type} entries is allocated.
     * Otherwise, the supplied collection is stored directly and is not copied
     * or cloned.
     *
     * @param name              a pointer to the null-terminated original
     *                          function name
     * @param parameterTypes    a pointer to the ordered parameter-type
     *                          collection, or {@code null} to create an empty
     *                          parameter list
     * @param aliasName         a pointer to the optional null-terminated alias,
     *                          or {@code null} if no alias is specified
     */
    constructor(name: pointer<char>, parameterTypes: pointer<ArrayList>, aliasName: pointer<char>)
    {
        this.name = name
        this.parameterTypes = if parameterTypes == null:
                new ArrayList(sizeof(Type))
            else:
                parameterTypes

        this.aliasName = aliasName
    }


    /**
     * Returns the original name of the imported function.
     *
     * <p>The returned pointer refers directly to the internally stored character
     * sequence and is not duplicated or cloned.
     *
     * <p>The result may be {@code null} if the imported-function descriptor was
     * created without a valid function name.
     *
     * @return                  a pointer to the internally stored function name,
     *                          or {@code null} if no name is available
     */
    fun getName() -> pointer<char> = this.name


    /**
     * Returns the parameter-type collection associated with this imported
     * function.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Because the constructor normalizes a {@code null} parameter-type
     * collection to an empty list, this method normally returns a valid
     * {@code ArrayList}.
     *
     * <p>Modifications performed through the returned collection affect the same
     * parameter-type list referenced internally by this
     * {@code ImportedFunction}.
     *
     * @return                  a pointer to the internally stored parameter-type
     *                          collection
     */
    fun getParameterTypes() -> pointer<ArrayList> = this.parameterTypes


    /**
     * Returns the alias assigned to this imported function.
     *
     * <p>The returned pointer refers directly to the internally stored character
     * sequence and is not duplicated or cloned.
     *
     * <p>The result is {@code null} when the function was imported without an
     * alias.
     *
     * @return                  a pointer to the internally stored alias name, or
     *                          {@code null} if no alias is specified
     */
    fun getAliasName() -> pointer<char> = this.aliasName
}


/**
 * Represents an optional alias for an imported function.
 *
 * <p>{@code ImportedFunctionAliasMaybe} is intended for parser productions in
 * which an imported-function alias may be omitted.
 *
 * <p>Unlike wrappers that normalize an absent value to an empty object, this
 * structure preserves the absence of an alias using a {@code null} pointer.
 */
struct ImportedFunctionAliasMaybe
{
    /**
     * The optional alias name represented by this wrapper.
     *
     * <p>A {@code null} value indicates that no explicit imported-function alias
     * was present.
     */
    private var aliasName: pointer<char>


    /**
     * Creates an empty imported-function alias wrapper.
     *
     * <p>The internal alias pointer is initialized to {@code null}, explicitly
     * representing the absence of an alias.
     */
    constructor():
        this.aliasName = null


    /**
     * Creates an imported-function alias wrapper around the specified alias.
     *
     * <p>The supplied character pointer is stored directly and is not duplicated
     * or copied.
     *
     * <p>If {@code aliasName} is {@code null}, the resulting wrapper represents
     * the absence of an alias.
     *
     * @param aliasName         a pointer to the null-terminated alias name, or
     *                          {@code null} if no alias is present
     */
    constructor(aliasName: pointer<char>):
        this.aliasName = aliasName


    /**
     * Returns the optional imported-function alias represented by this wrapper.
     *
     * <p>The returned pointer refers directly to the internally stored character
     * sequence and is not duplicated.
     *
     * <p>The result is {@code null} when no alias was supplied.
     *
     * @return                  a pointer to the stored alias name, or
     *                          {@code null} if no alias is present
     */
    fun getAliasName() -> pointer<char> = this.aliasName
}


/**
 * Represents a selective import declaration.
 *
 * <p>A {@code SelectiveImports} associates a qualified namespace or declaration
 * name with an ordered collection of specific functions to import from that
 * target.
 *
 * <p>Each imported function is represented by an {@code ImportedFunction}
 * object, allowing the selective import to retain the original function name,
 * parameter-type signature, and optional alias information.
 *
 * <p>The qualified name is stored by reference. The imported-function
 * collection is also stored directly when supplied, while a missing collection
 * is normalized to an empty {@code ArrayList}.
 */
struct SelectiveImports
{
    /**
     * The qualified name identifying the namespace or declaration from which
     * functions are selectively imported.
     *
     * <p>The object is stored by reference and is not copied or cloned by the
     * constructor.
     */
    private var qualifiedName: pointer<QualifiedName>


    /**
     * The ordered collection of functions selected for import.
     *
     * <p>The collection is normalized to an empty list when no imported-function
     * collection is supplied to the constructor.
     */
    private var importedFunctions: pointer<ArrayList>


    /**
     * Creates a selective-import declaration with the specified qualified name
     * and imported-function collection.
     *
     * <p>The supplied qualified name is stored by reference and is not copied or
     * cloned.
     *
     * <p>If {@code importedFunctions} is {@code null}, a new empty
     * {@code ArrayList} capable of storing {@code ImportedFunction} entries is
     * allocated. Otherwise, the supplied list is stored directly and is not
     * copied or cloned.
     *
     * @param qualifiedName     a pointer to the qualified import target, or
     *                          {@code null} if no target name is available
     * @param importedFunctions
     *                          a pointer to the imported-function collection, or
     *                          {@code null} to create an empty collection
     */
    constructor(qualifiedName: pointer<QualifiedName>, importedFunctions: pointer<ArrayList>)
    {
        this.qualifiedName = qualifiedName
        this.importedFunctions = if importedFunctions == null:
                new ArrayList(sizeof(ImportedFunction))
            else:
                importedFunctions
    }

    /**
     * Returns the qualified name associated with this selective import.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code QualifiedName} object and is not copied or cloned.
     *
     * <p>The result may be {@code null} if the selective import was constructed
     * without a valid qualified name.
     *
     * @return                  a pointer to the internally stored qualified name,
     *                          or {@code null} if no name is available
     */
    fun getQualifiedName() -> pointer<QualifiedName> = this.qualifiedName


    /**
     * Returns the collection of functions selected for import.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Because the constructor normalizes a {@code null} collection to an
     * empty list, this method normally returns a valid {@code ArrayList}.
     *
     * <p>Modifications performed through the returned collection affect the same
     * imported-function list referenced internally by this
     * {@code SelectiveImports} instance.
     *
     * @return                  a pointer to the internally stored
     *                          imported-function collection
     */
    fun getImportedFunctions() -> pointer<ArrayList> = this.importedFunctions
}
