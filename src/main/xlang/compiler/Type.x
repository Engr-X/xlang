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
 * For now it only wraps a NormalType host. Keeping this wrapper small makes it
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
     * Creates a Type wrapper from a NormalType value.
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
        new Type(normalType)


    /**
     * Stores the concrete type representation wrapped by this Type.
     *
     * The current implementation accepts only NormalType. Later variants can be
     * added by widening this wrapper rather than changing every AST node that
     * refers to Type.
     */
    private var host: pointer<NormalType>


    /**
     * Initializes a Type wrapper around a NormalType host.
     *
     * The host pointer is stored directly. Ownership and copying stay with the
     * caller or with the concrete type object.
     *
     * @param host              concrete normal type to wrap
     */
    fun __init__(host: pointer<NormalType>):
        this.host = host


    /**
     * Returns tokens owned directly by this wrapper.
     *
     * Type currently owns no syntax token by itself. The wrapped NormalType keeps
     * the actual source tokens, so this method returns an empty list instead of
     * storing a separate extraTokens field.
     *
     * @return                  empty token list
     */
    fun getExtraTokens() -> pointer<ArrayList> = new ArrayList(sizeof(Token))


    /**
     * Builds the textual representation of the wrapped type.
     *
     * The formatting is delegated to the concrete host. If the wrapper has no
     * host, an empty builder is returned so debug printing remains safe.
     *
     * @return                  string builder containing the wrapped type text
     */
    fun toString() -> pointer<StringBuilder> = if this.host == null:
            new StringBuilder()
        else:
            this.host.toString()
}
