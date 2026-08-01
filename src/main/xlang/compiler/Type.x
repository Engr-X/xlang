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

@file.class("Type")
package xlang.compiler

import xlang.util.ArrayList
import xlang.util.string.String


/**
 * Describes a resolved compiler type.
 *
 * A Type stores a compact numeric kind, the simple type name, an optional
 * package name, nested type arguments and the runtime memory size used by
 * values of this type.
 *
 * Type arguments make compound types representable without inventing a new
 * struct for every shape. For example, pointer<char> can be represented by a
 * Type named `pointer` with one type argument named `char`.
 *
 * The textual names are duplicated when the object is created.
 */
struct Type
{
    /**
     * Stores the numeric kind of this type.
     *
     * The compiler can use this value for fast comparisons before falling
     * back to textual package and type names.
     */
    val type: int

    /**
     * Points to the null-terminated simple type name.
     *
     * Examples include `int`, `Token` and `ArrayList`.
     */
    val typeName: pointer<char>

    /**
     * Points to the null-terminated package name.
     *
     * This value may be null for built-in types, unresolved package names or
     * types where only the simple name is needed.
     */
    val packageName: pointer<char>

    /**
     * Stores nested type arguments.
     *
     * Each element slot stores one Type value. The list copies Type bytes when
     * addTypeArgument() is called, so callers may pass stack or heap Type
     * objects.
     */
    val typeArguments: pointer<ArrayList>

    /**
     * Stores the number of nested type arguments.
     *
     * This mirrors typeArguments.length so callers can check arity directly on
     * Type. For pointer<char>, length is 1.
     */
    var length: int

    /**
     * Stores the runtime memory size of this type in bytes.
     *
     * For value-like types this should be the real value size. For pointer-like
     * types this can be the pointer size, depending on semantic lowering.
     */
    val memSize: int


    /**
     * Initializes a type with package information.
     *
     * Both packageName and typeName are duplicated. The caller may still pass
     * null for packageName when a package is intentionally absent.
     *
     * @param                   type the numeric type kind.
     * @param                   packageName the null-terminated package name.
     * @param                   typeName the null-terminated simple type name.
     * @param                   memSize the runtime memory size in bytes.
     */
    fun __init__(type: int, packageName: pointer<char>, typeName: pointer<char>, memSize: int)
    {
        this.type = type
        this.typeName = String.strdup(typeName)
        this.packageName = String.strdup(packageName)
        this.typeArguments = new ArrayList(sizeof(Type))
        this.length = 0
        this.memSize = memSize
    }


    /**
     * Adds one nested type argument and returns this Type.
     *
     * The argument is copied into typeArguments as a Type value. This is useful
     * for chained construction:
     *     pointerType.addTypeArgument(charType)
     *
     * Null arguments are ignored.
     *
     * @param                   typeArgument type argument to append
     * @return                  this Type for chained construction
     */
    fun addTypeArgument(typeArgument: pointer<Type>) -> pointer<Type>
    {
        if typeArgument != null:
        {
            this.typeArguments.push(typeArgument)
            this.length = this.typeArguments.length
        }

        return this
    }


    /**
     * Returns the nested type argument at index.
     *
     * The returned pointer belongs to the internal ArrayList storage and may be
     * invalidated if later addTypeArgument() calls resize the list.
     *
     * @param                   index type argument index
     * @return                  stored Type pointer, or null when index is invalid
     */
    fun getTypeArgument(index: int) -> pointer<Type> =
        this.typeArguments.get(index) as pointer<Type>
}
