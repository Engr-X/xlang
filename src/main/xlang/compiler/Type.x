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
 * A Type stores the simple type name, an optional package name, nested type
 * arguments and the runtime memory size used by values of this type.
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
     * Each element slot stores one Type value. The list copies Type bytes when
     * addTypeArgument() is called, so callers may pass stack or heap Type
     * objects.
     */
    private val typeArguments: pointer<ArrayList>

    /**
     * Stores the runtime memory size in bytes.
     */
    private val memSize: int

    /**
     * Stores the number of nested type arguments.
     *
     * This mirrors typeArguments.length so callers can check arity directly on
     * Type. For pointer<char>, length is 1.
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
        this.typeArguments = new ArrayList(sizeof(Type))
        this.memSize = memSize
        this.length = 0
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
     * @param typeArgument      type argument to append
     *
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
     * Creates an independent copy of this Type.
     *
     * The copied Type duplicates pointer fields instead of sharing this Type's
     * internal strings or type argument list.
     *
     * @return                  copied Type
     */
    fun copy() -> pointer<Type>
    {
        val result: pointer<Type> = new Type(this.packageName, this.typeName, this.memSize)

        for (var i: int = 0; i < this.length; i++):
        {
            val typeArgument: pointer<Type> = this.typeArguments.get(i) as pointer<Type>

            if typeArgument != null:
            {
                val copiedArgument: pointer<Type> = typeArgument.copy()
                result.addTypeArgument(copiedArgument)
            }
        }

        return result
    }


    /**
     * Returns a copy of the simple type name.
     *
     * @return                  copied null-terminated simple type name
     */
    fun getTypeName() -> pointer<char> =
        String.strdup(this.typeName)


    /**
     * Returns a copy of the package name.
     *
     * @return                  copied null-terminated package name, or null when absent
     */
    fun getPackageName() -> pointer<char> =
        String.strdup(this.packageName)


    fun getMemSize() -> int = this.memSize


    /**
     * Returns the nested type argument at index.
     *
     * The returned Type is an independent copy. Mutating it does not modify the
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

        return typeArgument.copy()
    }
}
