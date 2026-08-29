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

@file.class("NormalType")
package xlang.compiler

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.String


/**
 * Describes a resolved compiler type.
 *
 * A NormalType stores the simple type name, an optional package name, nested type
 * arguments and the runtime memory size used by values of this type.
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
     * Creates the built-in void type descriptor.
     *
     * The void type represents the absence of a value and cannot store any data.
     *
     * The memory size is zero bytes.
     *
     * The void type is used for functions that do not return a value and for
     * operations where no value is produced.
     */
    static fun voidType() -> pointer<NormalType> = new NormalType("xlang.primary", "void", 0)


    /**
     * Creates the built-in boolean type descriptor.
     *
     * The memory size is one byte.
     *
     * Boolean values are represented as a single byte value. A value of zero
     * represents false, while any non-zero value represents true.
     */
    static fun boolType() -> pointer<NormalType> = new NormalType("xlang.primary", "bool", 1)


    /**
     * Creates the built-in signed char type descriptor.
     *
     * The memory size is one byte.
     *
     * The char type represents an 32-bit signed character value.
     */
    static fun charType() -> pointer<NormalType> = new NormalType("xlang.primary", "char", 8)


    /**
     * Creates the built-in signed byte type descriptor.
     *
     * The memory size is one byte.
     *
     * The byte type represents an 8-bit signed integer value.
     */
    static fun byteType() -> pointer<NormalType> = new NormalType("xlang.primary", "byte", 1)


    /**
     * Creates the built-in signed short integer type descriptor.
     *
     * The memory size is two bytes.
     *
     * The short type represents a 16-bit signed integer value.
     */
    static fun shortType() -> pointer<NormalType> = new NormalType("xlang.primary", "short", 2)


    /**
     * Creates the built-in signed integer type descriptor.
     *
     * The memory size is four bytes.
     *
     * The int type represents a 32-bit signed integer value.
     */
    static fun intType() -> pointer<NormalType> = new NormalType("xlang.primary", "int", 4)


    /**
     * Creates the built-in signed long integer type descriptor.
     *
     * The memory size is eight bytes.
     *
     * The long type represents a 64-bit signed integer value.
     */
    static fun longType() -> pointer<NormalType> = new NormalType("xlang.primary", "long", 8)


    /**
     * Creates the built-in single-precision floating-point type descriptor.
     *
     * The memory size is four bytes.
     *
     * The float type follows the IEEE 754 single-precision floating-point format.
     */
    static fun floatType() -> pointer<NormalType> = new NormalType("xlang.primary", "float", 4)


    /**
     * Creates the built-in double-precision floating-point type descriptor.
     *
     * The memory size is eight bytes.
     *
     * The double type follows the IEEE 754 double-precision floating-point format.
     */
    static fun doubleType() -> pointer<NormalType> = new NormalType("xlang.primary", "double", 8)


    /**
     * Creates the built-in pointer type descriptor.
     *
     * The memory size is eight bytes for the current target model.
     *
     * Pointer values store memory addresses and are used to reference objects,
     * structures, functions, or other memory locations.
     */
    static fun pointerType() -> pointer<NormalType> = new NormalType("xlang.primary", "pointer", 8).addTypeArgument(voidType())


    /**
     * Creates a built-in fixed-size blob type descriptor.
     *
     * A blob represents a raw block of memory with a fixed size and no predefined
     * interpretation.
     *
     * @param memSize number of bytes occupied by the blob value.
     *
     * Postconditions:
     * - The returned type descriptor occupies exactly memSize bytes.
     * - The blob contents are managed by the owner of the value.
     */
    static fun blobType(memSize: int) -> pointer<NormalType> = new NormalType("xlang.primary", "blob", memSize)


    /* Returns the primitive string type used by the compiler bootstrap stage.
     *
     * Early string is represented as pointer<char>.
     * The pointer size is 8 bytes and points to a null-terminated character array.
     *
     * This is a temporary low-level representation before the standard String type
     * is initialized.
     */
    static fun earlyStringType() -> pointer<NormalType> = new NormalType("xlang.primary", "pointer", 8).addTypeArgument(charType())


    /**
     * Points to the null-terminated simple type name.
     *
     * Examples include `int`, `Token` and `ArrayList`.
     */
    private val typeName: pointer<char>

    /**
     * Points to the null-terminated package name.
     *
     * This value may be null for built-in types, unresolved package names or
     * types where only the simple name is needed.
     */
    private val packageName: pointer<char>

    /**
     * Stores nested type arguments.
     *
     * Each element slot stores one NormalType value. The list copies NormalType bytes when
     * addTypeArgument() is called, so callers may pass stack or heap NormalType
     * objects.
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
     * Initializes a type with package information.
     *
     * Both packageName and typeName are duplicated. The caller may still pass
     * null for packageName when a package is intentionally absent.
     *
     * @param packageName       the null-terminated package name.
     * @param typeName          the null-terminated simple type name.
     * @param memSize           the runtime memory size in bytes.
     */
    fun __init__(packageName: pointer<char>, typeName: pointer<char>, memSize: int)
    {
        this.typeName = String.strdup(typeName)
        this.packageName = String.strdup(packageName)
        this.typeArguments = new ArrayList(sizeof(NormalType))
        this.tokens = new ArrayList(sizeof(Token))
        this.memSize = memSize
        this.length = 0
    }


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
     * The argument is copied into typeArguments as a NormalType value. This is useful
     * for chained construction:
     *     pointerType.addTypeArgument(charType)
     *
     * Null arguments are ignored.
     *
     * @param typeArgument      type argument to append
     *
     * @return                  this NormalType for chained construction
     */
    fun addTypeArgument(typeArgument: pointer<NormalType>) -> pointer<NormalType>
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
            val typeArgument: pointer<NormalType> = this.typeArguments.get(i) as pointer<NormalType>

            if typeArgument != null:
            {
                val copiedArgument: pointer<NormalType> = typeArgument.clone()
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
     * Returns a clone of the package name.
     *
     * @return                  copied null-terminated package name, or null when absent
     */
    fun getPackageName() -> pointer<char> = String.strdup(this.packageName)


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
            val typeArgument: pointer<NormalType> = this.typeArguments.get(i) as pointer<NormalType>

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
     * The returned NormalType is an independent clone. Mutating it does not modify the
     * NormalType stored inside this object's internal type argument list.
     *
     * @param index             type argument index
     *
     * @return                  copied NormalType pointer, or null when index is invalid
     */
    fun getTypeArgument(index: int) -> pointer<NormalType>
    {
        val typeArgument: pointer<NormalType> = this.typeArguments.get(index) as pointer<NormalType>

        if typeArgument == null:
            return null

        return typeArgument.clone()
    }


    /**
     * Returns whether another NormalType describes the same type shape.
     *
     * Package name and simple type name are compared by string content. NormalType
     * arguments are compared recursively in order.
     *
     * @param other             NormalType to compare with this NormalType
     *
     * @return                  true when both NormalType values are equivalent
     */
    fun equals(other: pointer<NormalType>) -> bool
    {
        if other == null:
            return false

        if !String.streq(this.packageName, other.packageName) || !String.streq(this.typeName, other.typeName):
            return false

        if this.memSize != other.memSize || this.length != other.length:
            return false

        for (var i: int = 0; i < this.length; i++):
        {
            val left: pointer<NormalType> = this.typeArguments.get(i) as pointer<NormalType>
            val right: pointer<NormalType> = other.typeArguments.get(i) as pointer<NormalType>

            if left == null || right == null:
            {
                if left != right:
                    return false

                continue
            }

            if !left.equals(right):
                return false
        }

        return true
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
        sb.append('.')
        sb.append(this.packageName)
        return sb
    }
}
