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

package xlang.compiler.parser

import xlang.compiler.parser.program.ImportDeclaration
import xlang.compiler.parser.program.PackageDeclaration
import xlang.compiler.parser.program.PreprocessSetting
import xlang.compiler.parser.program.Struct
import xlang.util.ArrayList


/**
 * Represents a normalized view of a parsed program.
 *
 * <p>A {@code NormalizedProgram} stores the major top-level components produced
 * after program normalization: preprocessing settings, an optional package
 * declaration, import declarations, and a structure declaration representing
 * the normalized program contents.
 *
 * <p>The preprocessing-setting and import collections are normalized to empty
 * {@code ArrayList} instances when {@code null} values are supplied to the
 * constructor. Consumers can therefore access these collections without using
 * a separate null-list representation.
 *
 * <p>The package declaration and structure declaration are not normalized.
 * Their {@code null} values are preserved and may therefore represent missing
 * or unavailable program components.
 *
 * <p>All non-null objects and collections supplied to the constructor are stored
 * directly and are not copied or cloned.
 */
struct NormalizedProgram
{
    /**
     * The ordered collection of preprocessing settings associated with the
     * normalized program.
     *
     * <p>If no preprocessing-setting collection is supplied during
     * construction, this field is initialized with a newly allocated empty
     * {@code ArrayList}.
     *
     * <p>When a non-null collection is supplied, that collection is stored
     * directly and is not copied or cloned.
     */
    private var preprocessSettings: pointer<ArrayList>

    /**
     * The optional package declaration associated with the normalized program.
     *
     * <p>The declaration is stored by reference and is not copied or cloned.
     *
     * <p>A {@code null} value indicates that no package declaration is
     * associated with the normalized program.
     */
    private var packageDeclaration: pointer<PackageDeclaration>

    /**
     * The ordered collection of import declarations associated with the
     * normalized program.
     *
     * <p>If no import collection is supplied during construction, this field is
     * initialized with a newly allocated empty {@code ArrayList}.
     *
     * <p>When a non-null collection is supplied, that collection is stored
     * directly and is not copied or cloned.
     */
    private var imports: pointer<ArrayList>

    /**
     * The structure declaration representing the normalized program contents.
     *
     * <p>The structure is stored by reference and is not copied or cloned.
     *
     * <p>A {@code null} value indicates that no normalized structure declaration
     * is currently associated with the program.
     */
    private var structDeclaration: pointer<Struct>


    /**
     * Creates a normalized program from the specified top-level program
     * components.
     *
     * <p>If {@code preprocessSettings} is {@code null}, a new empty
     * {@code ArrayList} capable of storing {@code PreprocessSetting} entries is
     * allocated. Otherwise, the supplied collection is stored directly and is
     * not copied or cloned.
     *
     * <p>The supplied package declaration is stored directly and may be
     * {@code null}.
     *
     * <p>If {@code imports} is {@code null}, a new empty {@code ArrayList}
     * capable of storing {@code ImportDeclaration} entries is allocated.
     * Otherwise, the supplied import collection is stored directly and is not
     * copied or cloned.
     *
     * <p>The supplied structure declaration is stored directly and may be
     * {@code null}.
     *
     * @param preprocessSettings
     *                          a pointer to the preprocessing-setting
     *                          collection, or {@code null} to create an empty
     *                          collection
     * @param packageDeclaration
     *                          a pointer to the package declaration, or
     *                          {@code null} if no package declaration is
     *                          available
     * @param imports           a pointer to the import-declaration collection,
     *                          or {@code null} to create an empty collection
     * @param structDeclaration
     *                          a pointer to the normalized structure
     *                          declaration, or {@code null} if no structure is
     *                          available
     */
    constructor(
        preprocessSettings: pointer<ArrayList>,
        packageDeclaration: pointer<PackageDeclaration>,
        imports: pointer<ArrayList>,
        structDeclaration: pointer<Struct>
    )
    {
        this.preprocessSettings = if preprocessSettings == null:
                new ArrayList(sizeof(PreprocessSetting))
            else:
                preprocessSettings

        this.packageDeclaration = packageDeclaration
        this.imports = if imports == null:
                new ArrayList(sizeof(ImportDeclaration))
            else:
                imports

        this.structDeclaration = structDeclaration
    }


    /**
     * Returns the preprocessing-setting collection associated with this
     * normalized program.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Because the constructor normalizes a {@code null} preprocessing-setting
     * collection to an empty list, this method normally returns a valid
     * {@code ArrayList}.
     *
     * <p>Modifications performed through the returned list affect the same
     * preprocessing-setting collection referenced internally by this
     * {@code NormalizedProgram}.
     *
     * @return                  a pointer to the internally stored
     *                          preprocessing-setting collection
     */
    fun getPreprocessSettings() -> pointer<ArrayList> = this.preprocessSettings


    /**
     * Returns the package declaration associated with this normalized program.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code PackageDeclaration} object and is not copied or cloned.
     *
     * <p>The result is {@code null} when the normalized program does not contain
     * an associated package declaration.
     *
     * @return                  a pointer to the internally stored package
     *                          declaration, or {@code null} if no package
     *                          declaration is available
     */
    fun getPackageDeclaration() -> pointer<PackageDeclaration> = this.packageDeclaration


    /**
     * Returns the import-declaration collection associated with this normalized
     * program.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Because the constructor normalizes a {@code null} import collection to
     * an empty list, this method normally returns a valid {@code ArrayList}.
     *
     * <p>Modifications performed through the returned list affect the same
     * import collection referenced internally by this
     * {@code NormalizedProgram}.
     *
     * @return                  a pointer to the internally stored
     *                          import-declaration collection
     */
    fun getImports() -> pointer<ArrayList> = this.imports


    /**
     * Returns the structure declaration representing the normalized program.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code Struct} object and is not copied or cloned.
     *
     * <p>The result may be {@code null} when normalization has not produced a
     * structure declaration or no structure is associated with this program.
     *
     * @return                  a pointer to the internally stored normalized
     *                          structure declaration, or {@code null} if no
     *                          structure is available
     */
    fun getStruct() -> pointer<Struct> = this.structDeclaration
}
