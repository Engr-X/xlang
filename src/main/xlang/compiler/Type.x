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
 */

@file.class("Type")
package xlang.compiler

import xlang.lexer.Token
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


/**
 * Wraps one concrete type representation.
 *
 * Type is the public type node used by parser and semantic-facing structures.
 * For now it only wraps a Type host. Keeping this wrapper small makes it
 * possible to add other type shapes later, such as function types, without
 * forcing every parser node to know about every concrete representation.
 *
 * Type does not own extra source tokens. The concrete host keeps its own tokens,
 * and this wrapper only exposes an empty extra-token list for callers that use a
 * common token-collection interface.
 */
struct Type
{
    /**
     * Identifies a Type wrapper whose host is a NormalType.
     */
    private static val NORMAL_KIND: int = 1


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
    static fun voidType() -> pointer<Type> = fromNormal(NormalType.voidType())


    /**
     * Creates the built-in boolean type descriptor.
     *
     * The memory size is one byte.
     *
     * Boolean values are represented as a single byte value. A value of zero
     * represents false, while any non-zero value represents true.
     */
    static fun boolType() -> pointer<Type> = fromNormal(NormalType.boolType())


    /**
     * Creates the built-in signed char type descriptor.
     *
     * The memory size is one byte.
     *
     * The char type represents an 32-bit signed character value.
     */
    static fun charType() -> pointer<Type> = fromNormal(NormalType.charType())


    /**
     * Creates the built-in signed byte type descriptor.
     *
     * The memory size is one byte.
     *
     * The byte type represents an 8-bit signed integer value.
     */
    static fun byteType() -> pointer<Type> = fromNormal(NormalType.byteType())


    /**
     * Creates the built-in signed short integer type descriptor.
     *
     * The memory size is two bytes.
     *
     * The short type represents a 16-bit signed integer value.
     */
    static fun shortType() -> pointer<Type> = fromNormal(NormalType.shortType())


    /**
     * Creates the built-in signed integer type descriptor.
     *
     * The memory size is four bytes.
     *
     * The int type represents a 32-bit signed integer value.
     */
    static fun intType() -> pointer<Type> = fromNormal(NormalType.intType())


    /**
     * Creates the built-in signed long integer type descriptor.
     *
     * The memory size is eight bytes.
     *
     * The long type represents a 64-bit signed integer value.
     */
    static fun longType() -> pointer<Type> = fromNormal(NormalType.longType())


    /**
     * Creates the built-in single-precision floating-point type descriptor.
     *
     * The memory size is four bytes.
     *
     * The float type follows the IEEE 754 single-precision floating-point format.
     */
    static fun floatType() -> pointer<Type> = fromNormal(NormalType.floatType())


    /**
     * Creates the built-in double-precision floating-point type descriptor.
     *
     * The memory size is eight bytes.
     *
     * The double type follows the IEEE 754 double-precision floating-point format.
     */
    static fun doubleType() -> pointer<Type> = fromNormal(NormalType.doubleType())


    /**
     * Creates the built-in pointer type descriptor.
     *
     * The memory size is eight bytes for the current target model.
     *
     * Pointer values store memory addresses and are used to reference objects,
     * structures, functions, or other memory locations.
     */
    static fun pointerType() -> pointer<Type> = fromNormal(NormalType.pointerType())


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
    static fun blobType(memSize: int) -> pointer<Type> = fromNormal(NormalType.blobType(memSize))


    /* Returns the primitive string type used by the compiler bootstrap stage.
     *
     * Early string is represented as pointer<char>.
     * The pointer size is 8 bytes and points to a null-terminated character array.
     *
     * This is a temporary low-level representation before the standard String type
     * is initialized.
     */
    static fun earlyStringType() -> pointer<Type> = fromNormal(NormalType.earlyStringType())


    /**
     * Creates a Type wrapper from a Type value.
     *
     * This is the named constructor used by parser code when it has already
     * parsed a normal named type and only needs to lift it into the public Type
     * abstraction.
     *
     * @param normalType        normal named type to wrap
     *
     * @return                  Type wrapper containing normalType
     */
    static fun fromNormal(normalType: pointer<NormalType>) -> pointer<Type> =
        new Type(NORMAL_KIND, normalType)

    
    /**
     * Stores the concrete type representation wrapped by this Type.
     *
     * The current implementation accepts only Type. Later variants can be
     * added by widening this wrapper rather than changing every AST node that
     * refers to Type.
     */
    private var host: pointer<*>

    /**
     * Stores the concrete host kind wrapped by this Type.
     */
    private var kind: int

    /**
     * Initializes a Type wrapper around a Type host.
     *
     * The host pointer is stored directly. Ownership and copying stay with the
     * caller or with the concrete type object.
     *
     * @param host              concrete normal type to wrap
     */
    fun __init__(kind: int, host: pointer<*>)
    {
        this.host = host
        this.kind = kind
    }


    /**
     * Returns tokens owned directly by this wrapper.
     *
     * Type currently owns no syntax token by itself. The wrapped Type keeps
     * the actual source tokens, so this method returns an empty list instead of
     * storing a separate extraTokens field.
     *
     * @return                  empty token list
     */
    fun getExtraTokens() -> pointer<ArrayList> = new ArrayList(sizeof(Token))


    /**
     * Collects all source tokens that belong to this Type tree.
     *
     * Type itself does not own syntax tokens. For a normal type wrapper, token
     * ownership stays in the wrapped NormalType, so this method delegates to the
     * host and returns its collected tokens.
     *
     * @return                  all source tokens for the wrapped type
     */
    fun getAllTokens() -> pointer<ArrayList> =
        if this.host == null:
            new ArrayList(sizeof(Token))

        elif this.kind == NORMAL_KIND:
        {
            val type: pointer<NormalType> = this.host as pointer<NormalType>
            type.getAllTokens()
        }
        else:  new ArrayList(sizeof(Token))


    /**
     * Creates an independent copy of this Type wrapper.
     *
     * The wrapped NormalType is cloned so callers can mutate the returned Type
     * without sharing the normal type payload with this object.
     *
     * @return                  copied Type wrapper
     */
    fun clone() -> pointer<Type> =
        if this.host == null:
            new Type(this.kind, null)
        elif this.kind == NORMAL_KIND:
        {
            val type: pointer<NormalType> = this.host as pointer<NormalType>
            Type.fromNormal(type.clone())
        }
        else: new Type(this.kind, this.host)


    /**
     * Builds the textual representation of the wrapped type.
     *
     * The formatting is delegated to the concrete host. If the wrapper has no
     * host, an empty builder is returned so debug printing remains safe.
     *
     * @return                  string builder containing the wrapped type text
     */
    fun toString() -> pointer<StringBuilder> =
        if this.host == null:
            new StringBuilder()
        elif this.kind == NORMAL_KIND:
        {
            val type: pointer<NormalType> = this.host as pointer<NormalType>
            type.toString()
        }
        else: new StringBuilder()
}
