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
 *
 *
 *
 */

package xlang.compiler.type

import xlang.Diagnostic
import xlang.SourceLocation
import xlang.compiler.parser.program.ImportDeclaration
import xlang.compiler.parser.program.NamespaceImport
import xlang.compiler.parser.program.QualifiedName
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.String
import xlang.util.string.StringBuilder


/**
 * Describes a resolved compiler type.
 *
 * A NormalType stores the simple type name, optional package-name components,
 * nested type arguments and the runtime memory size used by values of this type.
 *
 * NormalType arguments make compound types representable without inventing a new
 * struct for every shape. For example, pointer<char> can be represented by a
 * NormalType named `pointer` with one type argument named `char`.
 *
 * The textual names are duplicated when the object is created.
 */
struct NormalType
{
    /**
     * Builds a dotted package-name string from package-name components.
     *
     * @param packageName       package-name components
     *
     * @return                  newly duplicated dotted package name, or null
     */
    private static fun packageNameToString(packageName: pointer<ArrayList>) -> pointer<char>
    {
        if packageName == null || packageName.length <= 0:
            return null

        val builder: pointer<StringBuilder> = new StringBuilder()

        for (var i = 0; i < packageName.length; i++):
        {
            val slot: pointer<pointer<char>> = packageName.get(i) as pointer<pointer<char>>

            if slot == null || slot.deref == null:
                continue

            if builder.length > 0:
                builder.append('.')

            builder.append(slot.deref)
        }

        if builder.length <= 0:
            return null

        val packageNameSpace: blob[(builder.length + 1) * sizeof(char)]
        val packageNameText: pointer<char> = packageNameSpace as pointer<char>

        builder.toString(packageNameText)
        return String.strdup(packageNameText)
    }


    /**
     * Creates a resolved normal type using the specified package name,
     * type name, and memory size.
     *
     * The package name is expected to be represented as a dot-separated
     * string, such as {@code "xlang.lang.core"}. It is split into individual
     * package components before the {@link NormalType} instance is created.
     *
     * A type created by this method is considered resolved because its
     * package information is already known.
     *
     * @param packageName       the dot-separated package name of the type
     * @param typeName          the name of the type
     * @param memSize           the memory size associated with the type
     *
     * @return                  a newly created resolved {@link NormalType}
     */
    static fun resolved(packageName: pointer<char>, typeName: pointer<char>, memSize: int) -> pointer<NormalType> =
        new NormalType(String.split(packageName, "."), typeName, memSize)


    /**
    * Creates a resolved normal type using an already parsed package name.
    *
    * Unlike {@link resolved(pointer<char>, pointer<char>, int)}, this overload
    * accepts the package name as an {@link ArrayList} containing its individual
    * package components. No additional package-name parsing is performed.
    *
    * A type created by this method is considered resolved because its
    * package information is already available.
    *
    * @param packageName        the parsed package components of the type
    * @param typeName           the name of the type
    * @param memSize            the memory size associated with the type
    *
    * @return                   a newly created resolved {@link NormalType}
    */
    static fun resolved(packageName: pointer<ArrayList>, typeName: pointer<char>, memSize: int) -> pointer<NormalType> =
        new NormalType(packageName, typeName, memSize)


    /**
    * Creates an unresolved normal type with the specified type name
    * and memory size.
    *
    * The package name of the resulting type is left unset by assigning
    * {@code null}. This indicates that the package information has not yet
    * been resolved and may be determined during a later resolution phase.
    *
    * This method is useful when a type reference is encountered before
    * sufficient context is available to determine its fully qualified name.
    *
    * @param typeName           the name of the unresolved type
    * @param memSize            the memory size associated with the type
    *
    * @return                   a newly created unresolved {@link NormalType}
    */
    static fun unresolved(typeName: pointer<char>, memSize: int) -> pointer<NormalType>
    {
        val packageName: pointer<ArrayList> = null
        return new NormalType(packageName, typeName, memSize)
    }


    /**
    * Creates an unresolved normal type with the specified type name.
    *
    * The package information is left unresolved, and the memory size is
    * initialized to {@code 0}. This is a convenience overload of
    * {@link unresolved(pointer<char>, int)}.
    *
    * @param typeName           the name of the unresolved type
    *
    * @return                   a newly created unresolved {@link NormalType} with a memory size of {@code 0}
    */
    static fun unresolved(typeName: pointer<char>) -> pointer<NormalType> =
        NormalType.unresolved(typeName, 0)


    /**
     * Creates the built-in void type descriptor.
     *
     * The void type represents the absence of a value and cannot store any data.
     *
     * The memory size is zero bytes.
     *
     * The void type is used for functions that do not return a value and for
     * operations where no value is produced.
     */
    static fun voidType() -> pointer<NormalType> = NormalType.resolved("xlang.primary", "void", 0)


    /**
     * Creates the built-in boolean type descriptor.
     *
     * The memory size is one byte.
     *
     * Boolean values are represented as a single byte value. A value of zero
     * represents false, while any non-zero value represents true.
     */
    static fun boolType() -> pointer<NormalType> = NormalType.resolved("xlang.primary", "bool", 1)


    /**
     * Creates the built-in signed char type descriptor.
     *
     * The memory size is one byte.
     *
     * The char type represents an 32-bit signed character value.
     */
    static fun charType() -> pointer<NormalType> = NormalType.resolved("xlang.primary", "char", 8)


    /**
     * Creates the built-in signed byte type descriptor.
     *
     * The memory size is one byte.
     *
     * The byte type represents an 8-bit signed integer value.
     */
    static fun byteType() -> pointer<NormalType> = NormalType.resolved("xlang.primary", "byte", 1)


    /**
     * Creates the built-in signed short integer type descriptor.
     *
     * The memory size is two bytes.
     *
     * The short type represents a 16-bit signed integer value.
     */
    static fun shortType() -> pointer<NormalType> = NormalType.resolved("xlang.primary", "short", 2)


    /**
     * Creates the built-in signed integer type descriptor.
     *
     * The memory size is four bytes.
     *
     * The int type represents a 32-bit signed integer value.
     */
    static fun intType() -> pointer<NormalType> = NormalType.resolved("xlang.primary", "int", 4)


    /**
     * Creates the built-in signed long integer type descriptor.
     *
     * The memory size is eight bytes.
     *
     * The long type represents a 64-bit signed integer value.
     */
    static fun longType() -> pointer<NormalType> = NormalType.resolved("xlang.primary", "long", 8)


    /**
     * Creates the built-in single-precision floating-point type descriptor.
     *
     * The memory size is four bytes.
     *
     * The float type follows the IEEE 754 single-precision floating-point format.
     */
    static fun floatType() -> pointer<NormalType> = NormalType.resolved("xlang.primary", "float", 4)


    /**
     * Creates the built-in double-precision floating-point type descriptor.
     *
     * The memory size is eight bytes.
     *
     * The double type follows the IEEE 754 double-precision floating-point format.
     */
    static fun doubleType() -> pointer<NormalType> = NormalType.resolved("xlang.primary", "double", 8)


    /**
     * Creates the built-in pointer type descriptor.
     *
     * The memory size is eight bytes for the current target model.
     *
     * Pointer values store memory addresses and are used to reference objects,
     * structures, functions, or other memory locations.
     */
    static fun pointerType() -> pointer<NormalType> =
        NormalType.resolved("xlang.primary", "pointer", 8).addTypeArgument(Type.fromNormal(voidType()))


    /* Returns the primitive string type used by the compiler bootstrap stage.
     *
     * Early string is represented as pointer<char>.
     * The pointer size is 8 bytes and points to a null-terminated character array.
     *
     * This is a temporary low-level representation before the standard String type
     * is initialized.
     */
    static fun earlyStringType() -> pointer<NormalType> =
        NormalType.resolved("xlang.primary", "pointer", 8).addTypeArgument(Type.fromNormal(charType()))


    /**
     * Points to the null-terminated simple type name.
     *
     * Examples include `int`, `Token` and `ArrayList`.
     */
    private val typeName: pointer<char>

    /**
     * Stores package-name components.
     *
     * Each element stores one pointer<char> component. A null package list means
     * the package is unresolved; an empty list is a resolved empty package.
     */
    private var packageName: pointer<ArrayList>

    /**
     * Stores nested type arguments.
     *
     * Each element slot stores one Type value. Using the public Type wrapper lets
     * normal types contain normal, function and blob type arguments.
     */
    private val typeArguments: pointer<ArrayList>

    /**
     * Stores source tokens owned directly by this NormalType layer.
     *
     * Nested type argument tokens are stored by their own NormalType objects and are
     * merged recursively by getAllTokens().
     */
    private val tokens: pointer<ArrayList>

    /**
     * Stores the runtime memory size in bytes.
     */
    private val memSize: int

    /**
     * Stores the number of nested type arguments.
     *
     * This mirrors typeArguments.length so callers can check arity directly on
     * NormalType. For pointer<char>, length is 1.
     */
    var length: int


    /**
     * Initializes a type with package-name components.
     *
     * typeName is duplicated. packageName storage is cloned, while component
     * string pointers are shared.
     *
     * @param packageName       package-name components, or null.
     * @param typeName          the null-terminated simple type name.
     * @param memSize           the runtime memory size in bytes.
     */
    private constructor(packageName: pointer<ArrayList>, typeName: pointer<char>, memSize: int)
    {
        this.typeName = typeName
        this.packageName = packageName
        this.typeArguments = new ArrayList(sizeof(Type))
        this.tokens = new ArrayList(sizeof(Token))
        this.memSize = memSize
        this.length = 0
    }


    /**
     * Checks whether the package of this type has not been resolved.
     *
     * @return true if the package name is not available; otherwise, false
     */
    fun isPackageUnresolved() -> bool = this.packageName == null


    /**
     * Adds one source token owned by this NormalType layer.
     *
     * Null tokens are ignored. The token is stored directly because token
     * lifetime is managed by the parser/token list that produced it.
     *
     * @param token             source token to append
     *
     * @return                  this NormalType for chained construction
     */
    fun addToken(token: pointer<Token>) -> pointer<NormalType>
    {
        if token != null:
            this.tokens.push(token)

        return this
    }


    /**
     * Adds one nested type argument and returns this NormalType.
     *
     * The argument is copied into typeArguments as a Type value. This is useful
     * for chained construction:
     *     pointerType.addTypeArgument(charType)
     *
     * Null arguments are ignored.
     *
     * @param typeArgument      type argument to append
     *
     * @return                  this NormalType for chained construction
     */
    fun addTypeArgument(typeArgument: pointer<Type>) -> pointer<NormalType>
    {
        if typeArgument != null:
        {
            this.typeArguments.push(typeArgument)
            this.length = this.typeArguments.length
        }

        return this
    }


    /**
     * Creates an independent clone of this NormalType.
     *
     * The copied NormalType duplicates pointer fields instead of sharing this NormalType's
     * internal strings or type argument list.
     *
     * @return                  copied NormalType
     */
    fun clone() -> pointer<NormalType>
    {
        val result: pointer<NormalType> = new NormalType(this.packageName, this.typeName, this.memSize)

        for (var i: int = 0; i < this.tokens.length; i++):
        {
            val token: pointer<Token> = this.tokens.get(i) as pointer<Token>
            result.addToken(token)
        }

        for (var i: int = 0; i < this.length; i++):
        {
            val typeArgument: pointer<Type> = this.typeArguments.get(i) as pointer<Type>

            if typeArgument != null:
            {
                val copiedArgument: pointer<Type> = typeArgument.clone()
                result.addTypeArgument(copiedArgument)
            }
        }

        return result
    }


    /**
     * Returns a clone of the simple type name.
     *
     * @return                  copied null-terminated simple type name
     */
    fun getTypeName() -> pointer<char> = String.strdup(this.typeName)


    /**
     * Returns a clone of the package-name components.
     *
     * @return                  copied package-name component list
     */
    fun getPackageName() -> pointer<ArrayList> =
        if this.packageName == null:
            new ArrayList(sizeof(pointer<char>))
        else:
            this.packageName.clone()


    /**
     * Returns a dotted package-name string.
     *
     * @return                  copied null-terminated package name, or null when absent
     */
    fun getPackageNameText() -> pointer<char> = NormalType.packageNameToString(this.packageName)


    /**
     * Returns the runtime memory size in bytes.
     *
     * @return                  runtime memory size in bytes
     */
    fun getMemSize() -> int = this.memSize


    /**
     * Collects source tokens owned by this NormalType and its nested type arguments.
     *
     * This NormalType owns only the tokens for its own layer, such as the type name
     * and delimiters like `<`, `,` and `>`. Nested type arguments recursively
     * contribute their own tokens. The merged list is sorted by source position
     * before it is returned.
     *
     * @return                  all source tokens belonging to this NormalType tree
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        result.pushAll(this.tokens)

        for (var i: int = 0; i < this.length; i++):
        {
            val typeArgument: pointer<Type> = this.typeArguments.get(i) as pointer<Type>

            if typeArgument == null:
                continue

            val tokens: pointer<ArrayList> = typeArgument.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns the nested type argument at index.
     *
     * The returned Type is an independent clone. Mutating it does not modify the
     * Type stored inside this object's internal type argument list.
     *
     * @param index             type argument index
     *
     * @return                  copied Type pointer, or null when index is invalid
     */
    fun getTypeArgument(index: int) -> pointer<Type>
    {
        val typeArgument: pointer<Type> = this.typeArguments.get(index) as pointer<Type>

        if typeArgument == null:
            return null

        return typeArgument.clone()
    }


    /**
     * Replaces this type's package-name components.
     *
     * <p>The package collection is expected to store {@code pointer<char>}
     * slots, one for each package-name component. The list storage is cloned,
     * but the contained string pointers are shared.
     *
     * <p>A null package list is replaced by an empty list. The type name and
     * type arguments are left unchanged.
     *
     * @param packageParts      package-name components, such as
     *                          {@code ["xlang", "util"]}
     *
     * @return                  this NormalType instance
     */
    fun setPackageName(packageParts: pointer<ArrayList>) -> pointer<NormalType>
    {
        this.packageName = if packageParts == null:
                new ArrayList(sizeof(pointer<char>))
            else:
                packageParts.clone()

        return this
    }


    /**
     * Determines whether this type is equal to another type.
     *
     * Equality is determined primarily by the type kind. Types with different
     * kinds are always considered different. If both types have the same kind,
     * their underlying type representations are compared according to the
     * semantics of that kind.
     *
     * For {@link #NORMAL_KIND}, the underlying {@link NormalType} instances are
     * compared using {@link NormalType#equals}. For {@link #FUNCTION_KIND}, the
     * underlying {@link FunctionType} instances are compared using
     * {@link FunctionType#equals}. For {@link #BLOB_KIND}, the underlying
     * {@link BlobType} instances are compared using {@link BlobType#equals}.
     *
     * If either underlying host is null, the types are considered equal only when
     * both hosts refer to the same null value. For kinds without a specialized
     * comparison rule, equality falls back to direct host-pointer equality.
     *
     * This method compares the semantic type representation and does not consider
     * source-level information such as tokens, source positions, or formatting.
     *
     * @param other             type to compare with this instance
     *
     * @return                  {@code true} if both instances represent the
     *                          same type; {@code false} otherwise
     */
    fun resolve(imports: pointer<ArrayList>) -> pointer<Diagnostic>
    {
        if imports == null || this.typeName == null || !this.isPackageUnresolved():
            return null

        var matchedCount: int = 0
        var matchedPackageName: pointer<ArrayList> = null
        val locations: pointer<ArrayList> = new ArrayList(sizeof(SourceLocation))

        for (var i = 0; i < imports.length; i++):
        {
            val importDecl: pointer<ImportDeclaration> = imports.get(i) as pointer<ImportDeclaration>

            if importDecl == null || importDecl.getKind() != ImportDeclaration.NAMESPACE_TYPE:
                continue

            val namespaceImport: pointer<NamespaceImport> = importDecl.getHost() as pointer<NamespaceImport>

            if namespaceImport == null || !namespaceImport.isSingle():
                continue

            val qualifiedName: pointer<QualifiedName> = namespaceImport.getQualifiedName()

            if qualifiedName == null:
                continue

            val path: pointer<ArrayList> = qualifiedName.toPackageDecl().getQualifiedName()

            if path == null || path.length <= 0:
                continue

            val nameSlot: pointer<pointer<char>> = path.get(path.length - 1) as pointer<pointer<char>>
            val name: pointer<char> = if nameSlot == null:
                    null
                else:
                    nameSlot.deref

            if name == null || !String.streq(name, this.typeName):
                continue

            if matchedPackageName == null || String.stringListCmp(matchedPackageName, path) != 0:
                matchedCount++

            val tokens: pointer<ArrayList> = qualifiedName.getAllTokens()

            if tokens != null && tokens.length > 0:
            {
                val token: pointer<Token> = tokens.get(tokens.length - 1) as pointer<Token>

                if token != null && token.pos != null:
                    locations.push(new SourceLocation(
                        null,
                        token.pos.offset,
                        token.pos.line,
                        token.pos.column,
                        token.pos.length))
            }

            if matchedCount == 1:
                matchedPackageName = path
        }

        if matchedCount > 1:
            return Diagnostic.makeError(
                Diagnostic.CANNOT_PARSE,
                locations,
                "ambiguous import")

        if matchedCount == 1:
            this.setPackageName(if matchedPackageName.length <= 1:
                    new ArrayList(sizeof(pointer<char>))
                else:
                    matchedPackageName.sublist(0, matchedPackageName.length - 1))

        return null
    }


    /**
     * Generates the mangled representation of this pointer type.
     *
     * If no pointed-to type is specified, this method returns the pointer
     * prefix {@code P}.
     *
     * Otherwise, the first type argument is treated as the pointed-to type.
     * The pointed-to type must be a normal type and must provide a valid
     * mangled representation. The resulting encoding consists of the
     * pointer prefix {@code P} followed by the mangled representation of
     * the inner type.
     *
     * For example:
     *
     * {@code pointer<int>} -> {@code Pi}
     * {@code pointer<double>} -> {@code Pd}
     *
     * Blob types, function types, and other unsupported type kinds cannot
     * currently be mangled by this method.
     *
     * @return                  a builder containing the mangled representation of
     *                          this pointer type, or {@code null} if the pointed-to
     *                          type is invalid or unsupported
     */
    private fun pointerMangling() -> pointer<StringBuilder>
    {
        if this.length <= 0:
            return new StringBuilder("P")

        val inner: pointer<Type> = this.typeArguments.get(0) as pointer<Type>

        if inner == null:
            return null

        if inner.getKind() == Type.BLOB_KIND:
            return null

        if inner.getKind() == Type.FUNCTION_KIND:
            return null

        if inner.getKind() != Type.NORMAL_KIND:
            return null

        val innerType: pointer<NormalType> = inner.getHost() as pointer<NormalType>

        if innerType == null:
            return null

        val innerMangling: pointer<StringBuilder> = innerType.getMangling()

        if innerMangling == null:
            return null

        val result: pointer<StringBuilder> = new StringBuilder("P")
        result.append(innerMangling)
        return result
    }


    /**
     * Returns the mangled representation of this type.
     *
     * Primitive types in the {@code xlang.primary} package are encoded
     * using Itanium C++ ABI-compatible type codes:
     *
     * {@code bool}   -> {@code b}
     * {@code byte}   -> {@code a}
     * {@code short}  -> {@code s}
     * {@code char}   -> {@code c}
     * {@code int}    -> {@code i}
     * {@code long}   -> {@code l}
     * {@code double} -> {@code d}
     *
     * Pointer types are encoded using {@link #pointerMangling()}.
     *
     * Class and other non-primitive type mangling is not currently
     * supported.
     *
     * @return                  a builder containing the mangled representation of
     *                          this type, or {@code null} if the type is not
     *                          currently supported
     */
    fun getMangling() -> pointer<StringBuilder>
    {
        val packageName: pointer<char> = this.getPackageNameText()

        return if String.streq(packageName, "xlang.primary") && String.streq(this.typeName, "bool"):
            new StringBuilder("b")
        elif String.streq(packageName, "xlang.primary") && String.streq(this.typeName, "byte"):
            new StringBuilder("a")
        elif String.streq(packageName, "xlang.primary") && String.streq(this.typeName, "short"):
            new StringBuilder("s")
        elif String.streq(packageName, "xlang.primary") && String.streq(this.typeName, "char"):
            new StringBuilder("c")
        elif String.streq(packageName, "xlang.primary") && String.streq(this.typeName, "int"):
            new StringBuilder("i")
        elif String.streq(packageName, "xlang.primary") && String.streq(this.typeName, "long"):
            new StringBuilder("l")
        elif String.streq(packageName, "xlang.primary") && String.streq(this.typeName, "double"):
            new StringBuilder("d")
        elif String.streq(packageName, "xlang.primary") && String.streq(this.typeName, "pointer"):
            this.pointerMangling()
        // TODO support class type mangling.
        else: null
    }


    /**
     * Checks whether another NormalType refers to the same resolved name.
     *
     * Both types must already be resolved. The simple type names must be equal,
     * and the package-name component lists must contain the same components in
     * the same order.
     *
     * Type arguments and memory size are not compared here.
     *
     * @param other             NormalType to compare with this NormalType
     *
     * @return                  true when both resolved names are equivalent
     */
    fun equals(other: pointer<NormalType>) -> bool
    {
        if other == null:
            return false

        if this.isPackageUnresolved() || other.isPackageUnresolved():
            return false

        if !String.streq(this.typeName, other.typeName):
            return false

        return String.stringListCmp(this.packageName, other.packageName) == 0
    }


    /**
     * Builds the textual representation of this type.
     *
     * The returned string contains the simple type name followed by the package
     * name, separated by a dot.
     *
     * For example, a NormalType with type name `int` and package name `xlang.primary`
     * is represented as:
     *     int.xlang.primary
     *
     * @return                  string builder containing the textual type representation
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder(this.typeName)
        val packageName: pointer<char> = this.getPackageNameText()

        if packageName != null:
        {
            sb.append('.')
            sb.append(packageName)
        }

        return sb
    }
}
