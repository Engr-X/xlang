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

import xlang.compiler.type.Type
import xlang.util.ArrayList


/**
 * Represents a function imported through a selective import declaration.
 *
 * <p>An {@code ImportedSymbol} stores the original function name, the ordered
 * collection of parameter types used to identify the imported function
 * signature, and an optional alias name.
 *
 * <p>The parameter-type collection is normalized to an empty
 * {@code ArrayList} when no collection is supplied.
 *
 * <p>The function name and alias name are stored by reference and are not copied
 * or duplicated by this structure.
 */
struct ImportedSymbol
{
    
    // Identifies an imported variable.
    private static val VARIABLE_KIND: int = 1

    // Identifies an imported function.
    private static val FUNCTION_KIND: int = 2


    /**
     * Creates an imported function descriptor.
     *
     * <p>The resulting object is marked with {@code FUNCTION_KIND}.
     *
     * <p>The supplied function name and alias name are stored directly and are not
     * copied or duplicated.
     *
     * <p>If {@code parameterTypes} is {@code null}, the private constructor
     * normalizes it to a newly allocated empty {@code ArrayList} capable of storing
     * {@code Type} entries.
     *
     * <p>Otherwise, the supplied parameter-type collection is stored directly and
     * is not copied or cloned.
     *
     * @param name              a pointer to the null-terminated original function
     *                          name
     * @param parameterTypes    a pointer to the ordered parameter-type collection,
     *                          or {@code null} if no parameter types are available
    * @param aliasName         a pointer to the optional null-terminated alias, or
    *                          {@code null} if no alias is specified
    *
    * @return                  a newly created imported-symbol descriptor
    *                          representing a function
    */
    static fun fromFunction(name: pointer<char>, parameterTypes: pointer<ArrayList>, aliasName: pointer<char>) -> pointer<ImportedSymbol> =
        new ImportedSymbol(FUNCTION_KIND, name, parameterTypes, aliasName)


    /**
     * Creates an imported variable descriptor.
     *
     * <p>The resulting object is marked with {@code VARIABLE_KIND}.
     *
     * <p>The supplied variable name and alias name are stored directly and are not
     * copied or duplicated.
     *
     * <p>An imported variable does not require function parameter types. A
     * {@code null} parameter-type collection is therefore passed to the private
     * constructor, which normalizes it to an empty {@code ArrayList}.
     *
     * @param name              a pointer to the null-terminated original variable
     *                          name
     * @param aliasName         a pointer to the optional null-terminated alias, or
     *                          {@code null} if no alias is specified
     *
     * @return                  a newly created imported-symbol descriptor
     *                          representing a variable
     */
    static fun fromVariable(name: pointer<char>, aliasName: pointer<char>) -> pointer<ImportedSymbol> =
        new ImportedSymbol(VARIABLE_KIND, name, null, aliasName)


    /**
    * The discriminator identifying the kind of imported symbol represented by
    * this object.
    *
    * <p>The value is expected to be either {@code VARIABLE_KIND} or
    * {@code FUNCTION_KIND}.
    *
    * <p>The field is initialized exclusively by the private constructor and is
    * therefore determined by the factory method used to create the object.
    */
    private var kind: int

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
     * Creates an imported-symbol descriptor with the specified kind, name,
     * parameter types, and optional alias.
     *
     * <p>This constructor is private so callers must create imported symbols through
     * {@code fromFunction()} or {@code fromVariable()}. This keeps the stored kind
     * consistent with the semantic form of the imported symbol.
     *
     * <p>The supplied name and alias name are stored directly and are not copied or
     * duplicated.
     *
     * <p>If {@code parameterTypes} is {@code null}, a new empty
     * {@code ArrayList} capable of storing {@code Type} entries is allocated.
     * Otherwise, the supplied collection is stored directly and is not copied or
     * cloned.
     *
     * <p>For function imports, {@code parameterTypes} describes the ordered
     * parameter types used to identify the imported function.
     *
     * <p>For variable imports, the parameter-type collection is normally empty and
     * has no semantic meaning.
     *
     * @param kind              the imported-symbol kind discriminator
     * @param name              a pointer to the null-terminated original symbol
     *                          name
     * @param parameterTypes    a pointer to the ordered function parameter-type
     *                          collection, or {@code null} to create an empty list
     * @param aliasName         a pointer to the optional null-terminated alias, or
     *                          {@code null} if no alias is specified
     */
    private constructor(kind: int, name: pointer<char>,
        parameterTypes: pointer<ArrayList>, aliasName: pointer<char>
    )
    {
        this.kind = kind
        this.name = name
        
        this.parameterTypes =
            if parameterTypes == null:
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
     * {@code ImportedSymbol}.
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
 * <p>{@code ImportedSymbolAliasMaybe} is intended for parser productions in
 * which an imported-function alias may be omitted.
 *
 * <p>Unlike wrappers that normalize an absent value to an empty object, this
 * structure preserves the absence of an alias using a {@code null} pointer.
 */
struct ImportedSymbolAliasMaybe
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
 * <p>Each imported function is represented by an {@code ImportedSymbol}
 * object, allowing the selective import to retain the original function name,
 * parameter-type signature, and optional alias information.
 *
 * <p>The qualified name is stored by reference. The imported-function
 * collection is also stored directly when supplied, while a missing collection
 * is normalized to an empty {@code ArrayList}.
 */
struct SelectiveImports
{
    // Indicates that only explicitly specified symbols are imported.
    private static var CERTAIN_KIND: int = 0

    // Indicates that all applicable symbols are imported.
    private static var ALL_KIND: int = 1


    /**
     * Creates a selective-import declaration that imports only the explicitly
     * specified symbols.
     *
     * <p>The supplied qualified name and imported-symbol collection are stored by
     * reference and are not copied or cloned.
     *
     * <p>If {@code importedFunctions} is {@code null}, the created declaration
     * contains a new empty imported-symbol collection.
     * 
     * @param qualifiedName     a pointer to the qualified import target, or
     *                          {@code null} if no target name is available
     * @param importedFunctions a pointer to the explicitly imported symbols, or
     *                          {@code null} to create an empty collection
     *
     * @return                   a pointer to the newly created selective import
     */
    static fun fromCertain(qualifiedName: pointer<QualifiedName>, importedFunctions: pointer<ArrayList>) -> pointer<SelectiveImports> = 
        new SelectiveImports(qualifiedName, CERTAIN_KIND, importedFunctions)


    /**
     * Creates a selective-import declaration that imports all applicable symbols
     * from the specified qualified target.
     *
     * <p>The supplied qualified name is stored by reference and is not copied or
     * cloned.
     *
     * @param qualifiedName     a pointer to the qualified import target, or
     *                          {@code null} if no target name is available
     *
     * @return                  a pointer to the newly created all-symbol import
     */
    static fun fromAll(qualifiedName: pointer<QualifiedName>) -> pointer<SelectiveImports> =
        new SelectiveImports(qualifiedName, ALL_KIND, null)


    /**
     * The kind of this selective import declaration.
     *
     * <p>The value is typically either {@link #CERTAIN_KIND}, indicating that
     * only explicitly specified symbols are imported, or {@link #ALL_KIND},
     * indicating that all applicable symbols are imported.
     */
    private var type: int


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
     * Creates a selective-import declaration with the specified qualified name,
     * import kind, and imported-symbol collection.
     *
     * <p>The supplied qualified name is stored by reference and is not copied or
     * cloned.
     *
     * <p>If {@code importedFunctions} is {@code null}, a new empty
     * {@code ArrayList} capable of storing {@code ImportedSymbol} entries is
     * allocated. Otherwise, the supplied list is stored directly and is not
     * copied or cloned.
     *
     * @param qualifiedName     a pointer to the qualified import target, or
     *                          {@code null} if no target name is available
     * @param type              the import kind, typically
     *                          {@link #CERTAIN_KIND} or {@link #ALL_KIND}
     * @param importedFunctions
     *                          a pointer to the imported-symbol collection, or
     *                          {@code null} to create an empty collection
     */
    private constructor(
        qualifiedName: pointer<QualifiedName>,
        type: int,
        importedFunctions: pointer<ArrayList>
    )
    {
        this.qualifiedName = qualifiedName
        this.type = type
        this.importedFunctions = if importedFunctions == null:
                new ArrayList(sizeof(ImportedSymbol))
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
