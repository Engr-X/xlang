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
     * Indicates that the normalized program represents a {@code struct}
     * declaration body.
     */
    private static val STRUCT_TYPE: int = 0

    /**
     * Indicates that the normalized program represents a {@code class}
     * declaration body.
     */
    private static val CLASS_TYPE: int = 1

    /**
     * Indicates that the normalized program represents an {@code interface}
     * declaration body.
     */
    private static val INTERFACE_TYPE: int = 2

    /**
     * Indicates that the normalized program represents an {@code annotation}
     * declaration body.
     */
    private static val ANNOTATION_TYPE: int = 3


    /**
     * Creates a normalized program whose body is a structure declaration.
     *
     * @param preprocessSettings
     *                          a pointer to the preprocessing-setting collection,
     *                           or {@code null} to create an empty collection
     * @param packageDeclaration
     *                          a pointer to the package declaration, or
     *                          {@code null} if no package declaration is available
     * @param imports           a pointer to the import-declaration collection, or
     *                          {@code null} to create an empty collection
     * @param body              a pointer to the structure declaration, or
     *                          {@code null} if no structure body is available
     * @return                  a pointer to the newly created normalized program
     */
    static fun fromStruct(
        preprocessSettings: pointer<ArrayList>,
        packageDeclaration: pointer<PackageDeclaration>,
        imports: pointer<ArrayList>,
        body: pointer<Struct>
    ) -> pointer<NormalizedProgram> = new NormalizedProgram(
        preprocessSettings,
        packageDeclaration,
        imports,
        STRUCT_TYPE,
        body
    )


    /**
     * The type of declaration body represented by this normalized program.
     *
     * <p>The value is one of {@link #STRUCT_TYPE}, {@link #CLASS_TYPE},
     * {@link #INTERFACE_TYPE}, or {@link #ANNOTATION_TYPE}.
     */
    private var bodyType: int

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
    private var body: pointer<Struct>


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
     * <p>The supplied body is stored by reference and is not copied or cloned.
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
     * @param bodyType          the type of declaration body
     * @param body              a pointer to the normalized declaration body, or
     *                          {@code null} if no body is available
     */
    private constructor(
        preprocessSettings: pointer<ArrayList>,
        packageDeclaration: pointer<PackageDeclaration>,
        imports: pointer<ArrayList>,
        bodyType: int,
        body: pointer<*>
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

        this.bodyType = bodyType
        this.body = body
    }


    /**
     * Builds and returns the fully qualified path of the current declaration.
     *
     * <p>The returned path is composed of the qualified package name, if a
     * package declaration is present, followed by the name of the declaration
     * body when the body has a valid name.</p>
     *
     * <p>If no package declaration exists, a new empty {@link ArrayList} is
     * created and used as the base path. When the current body represents a
     * struct, its struct name is appended to the end of the path.</p>
     *
     * <p>For example, a struct named {@code Example} declared inside the package
     * {@code foo.bar} produces a path equivalent to:</p>
     *
     * <pre>
     * ["foo", "bar", "Example"]
     * </pre>
     *
     * @return                  a list containing the components of the fully qualified path;
     *                          never {@code null}
     */
    fun getFullpath() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = if this.packageDeclaration == null:
                new ArrayList(sizeof(pointer<char>))
            else:
                this.packageDeclaration.getQualifiedName()

        val bodyName: pointer<char> = this.getBodyName()

        if bodyName != null:
            result.push(bodyName.ref)

        return result
    }


    /**
     * Returns the name associated with the current declaration body.
     *
     * <p>Currently, only struct bodies provide a body name. If the current body
     * type is {@code STRUCT_TYPE} and a body object is available, the struct name
     * is obtained from the underlying struct declaration.</p>
     *
     * <p>If the body does not represent a struct, or if no body has been assigned,
     * this function returns {@code null}.</p>
     *
     * @return                  the name of the current struct body, or {@code null} if the current
     *                          body has no applicable name
     */
    fun getBodyName() -> pointer<char> =
        if this.bodyType == STRUCT_TYPE && this.body != null:
            this.body.getStructName()
        else:
            null


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
    fun getStruct() -> pointer<Struct> = this.body
}
