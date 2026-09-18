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

package xlang.compiler

import xlang.compiler.setting.CompilerSettings
import xlang.compiler.setting.SystemBits
import xlang.lexer.Token
import xlang.util.ArrayList
import xlang.util.string.String
import xlang.util.string.StringBuilder


/**
 * Wraps one concrete type representation.
 *
 * Type is the public type node used by parser and semantic-facing structures.
 * It can wrap a normal named type or a function type. Keeping this wrapper
 * small makes it possible to add other type shapes later without forcing every
 * parser node to know about every concrete representation.
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
    static val NORMAL_KIND: int = 1

    /**
     * Identifies a Type wrapper whose host is a FunctionType.
     */
    static val FUNCTION_KIND: int = 2

    /**
     * Identifies a Type wrapper whose host is a BlobType.
     */
    static val BLOB_KIND: int = 3


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
     * Creates a blob type whose size has already been resolved.
     *
     * @param memSize           resolved storage size in bytes
     *
     * @return                  Type wrapper containing a BlobType
     */
    static fun blobType(memSize: int) -> pointer<Type> =
        fromBlob(new BlobType(null, memSize))


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
     * Creates a Type wrapper from a function type value.
     *
     * This is the named constructor used when parser or semantic code has built
     * a FunctionType payload and needs to lift it into the public Type
     * abstraction.
     *
     * @param functionType      function type to wrap
     *
     * @return                  Type wrapper containing functionType
     */
    static fun fromFunction(functionType: pointer<FunctionType>) -> pointer<Type> =
        new Type(FUNCTION_KIND, functionType)


    /**
     * Creates a Type wrapper from a blob type value.
     *
     * @param blobType          blob type to wrap
     *
     * @return                  Type wrapper containing blobType
     */
    static fun fromBlob(blobType: pointer<BlobType>) -> pointer<Type> =
        new Type(BLOB_KIND, blobType)


    /**
     * Maps a NormalType host to its built-in conversion function name.
     *
     * The caller must only pass a host that is known to be a NormalType. This
     * helper checks the simple type name and returns the matching conversion
     * helper used by cast/desugar code, such as `toByte` for byte. Pointer,
     * blob, void and non-primary names intentionally have no conversion helper
     * here and return null.
     *
     * @param host              NormalType host stored inside a Type wrapper
     *
     * @return                  conversion function name, or null when absent
     */
    static fun normalTypeFunction(host: pointer<*>) -> pointer<char>
    {
        val type: pointer<NormalType> = host as pointer<NormalType>
        val typeName: pointer<char> = type.getTypeName()

        return if String.streq(typeName, "bool"):
            "toBool"
        elif String.streq(typeName, "char"):
            "toChar"
        elif String.streq(typeName, "byte"):
            "toByte"
        elif String.streq(typeName, "short"):
            "toShort"
        elif String.streq(typeName, "int"):
            "toInt"
        elif String.streq(typeName, "long"):
            "toLong"
        elif String.streq(typeName, "float"):
            "toFloat"
        elif String.streq(typeName, "double"):
            "toDouble"
        else: null
    }

    
    /**
     * Stores the concrete type representation wrapped by this Type.
     *
     * The kind field decides whether this pointer is interpreted as NormalType,
     * FunctionType or a future concrete type payload.
     */
    private var host: pointer<*>

    /**
     * Stores the concrete host kind wrapped by this Type.
     */
    private var kind: int


    /**
     * Initializes a Type wrapper around a concrete host.
     *
     * The host pointer is stored directly. Ownership and copying stay with the
     * caller or with the concrete type object.
     *
     * @param host              concrete normal type to wrap
     */
    constructor(kind: int, host: pointer<*>)
    {
        this.host = host
        this.kind = kind
    }


    fun getKind() -> int = this.kind


    fun getHost() -> pointer<*> = this.host


    /**
     * Returns the storage size of this type in bytes.
     *
     * Function types and normal pointer types use the configured target pointer
     * width. Blob and normal types delegate to their concrete type information.
     * Parsed built-in normal types are recognized by name because their memory
     * size may not have been resolved by TypeParser yet.
     *
     * @return                  storage size in bytes, or zero when unavailable
     */
    fun getMemSize() -> int
    {
        if this.host == null:
            return 0

        if this.kind == FUNCTION_KIND:
            return CompilerSettings.getInstance().getSystemBits() / SystemBits.BITS_PER_BYTE

        if this.kind == BLOB_KIND:
        {
            val blobType: pointer<BlobType> = this.host as pointer<BlobType>
            return blobType.getMemSize()
        }

        if this.kind != NORMAL_KIND:
            return 0

        val normalType: pointer<NormalType> = this.host as pointer<NormalType>
        val typeName: pointer<char> = normalType.getTypeName()

        return if String.streq(typeName, "pointer"):
            CompilerSettings.getInstance().getSystemBits() / SystemBits.BITS_PER_BYTE
        elif String.streq(typeName, "void"): 0
        elif String.streq(typeName, "bool") || String.streq(typeName, "byte"): 1
        elif String.streq(typeName, "short"): 2
        elif String.streq(typeName, "int") || String.streq(typeName, "float"): 4
        elif String.streq(typeName, "char") || String.streq(typeName, "long") || String.streq(typeName, "double"): 8
        else: normalType.getMemSize()
    }


    /**
     * Returns whether this Type is one of the built-in primary normal types.
     *
     * This check only applies to NormalType hosts. FunctionType hosts and null
     * hosts return false. The package name is intentionally ignored so a type
     * can still be treated as primary when only its built-in type name is
     * available during parsing or early semantic analysis.
     *
     * @return                  true for built-in primitive and pointer names
     */
    fun isPrimary() -> bool
    {
        if this.host == null || this.kind != NORMAL_KIND:
            return false

        val type: pointer<NormalType> = this.host as pointer<NormalType>
        val typeName: pointer<char> = type.getTypeName()

        return String.streq(typeName, "void") ||
            String.streq(typeName, "bool") ||
            String.streq(typeName, "char") ||
            String.streq(typeName, "byte") ||
            String.streq(typeName, "short") ||
            String.streq(typeName, "int") ||
            String.streq(typeName, "long") ||
            String.streq(typeName, "float") ||
            String.streq(typeName, "double") ||
            String.streq(typeName, "pointer")
    }


    /**
     * Returns the built-in conversion function name for this Type.
     *
     * Only scalar primary normal types have conversion helper names. Pointer,
     * blob, void, function types and unknown hosts return null because they do
     * not map to a simple `to...` conversion function.
     *
     * @return                  conversion function name, or null when absent
     */
    fun getFunction() -> pointer<char> =
        if this.host == null || this.kind != NORMAL_KIND:
            null
        else:
            Type.normalTypeFunction(this.host)


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
        elif this.kind == FUNCTION_KIND:
        {
            val type: pointer<FunctionType> = this.host as pointer<FunctionType>
            type.getAllTokens()
        }
        elif this.kind == BLOB_KIND:
        {
            val type: pointer<BlobType> = this.host as pointer<BlobType>
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
        elif this.kind == FUNCTION_KIND:
        {
            val type: pointer<FunctionType> = this.host as pointer<FunctionType>
            Type.fromFunction(type.clone())
        }
        elif this.kind == BLOB_KIND:
        {
            val type: pointer<BlobType> = this.host as pointer<BlobType>
            Type.fromBlob(type.clone())
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
        elif this.kind == FUNCTION_KIND:
        {
            val type: pointer<FunctionType> = this.host as pointer<FunctionType>
            type.toString()
        }
        elif this.kind == BLOB_KIND:
        {
            val type: pointer<BlobType> = this.host as pointer<BlobType>
            type.toString()
        }
        else: new StringBuilder()
}
