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

@file.class("FunctionType")
package xlang.compiler

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


/**
 * Describes the type of a callable value.
 *
 * <p>A FunctionType is the structural part of a function pointer or function
 * value type. It records the ordered parameter type list and the return type.
 * For example, a source type such as {@code (int, bool) -> void} can be
 * represented as a FunctionType whose parameter list contains {@code int} and
 * {@code bool}, and whose return type is {@code void}.</p>
 *
 * <p>This struct intentionally does not inherit from Type. The outer Type layer
 * can later store a FunctionType as a specialized kind, while this struct keeps
 * only function-specific information. This keeps ordinary named types and
 * function types separate enough for parsing, semantic checks and code
 * generation to reason about them cleanly.</p>
 *
 * <p>Parameter entries currently use {@code Type} values because the compiler
 * still has a single Type representation. After Type is split into SimpleType
 * and higher-level Type wrappers, this list can become an ArrayList of
 * SimpleType without changing the general FunctionType layout.</p>
 *
 * @see Type
 */
struct FunctionType
{
    /**
     * Stores the ordered parameter type list.
     *
     * <p>Each element is expected to be a Type value in declaration order. The
     * first element corresponds to the first callable parameter, the second
     * element corresponds to the second parameter, and so on.</p>
     *
     * <p>The list pointer is stored directly by the constructor. FunctionType
     * does not deep-copy the list because parser constructors often already
     * produce owned ArrayList instances.</p>
     */
    private val parameterTypes: pointer<ArrayList>

    /**
     * Stores the return type.
     *
     * <p>The return type pointer is stored directly. A null return type is
     * tolerated by helper methods, but a fully parsed function type should
     * normally provide an explicit Type such as {@code void}.</p>
     */
    private var returnType: pointer<Type>

    /**
     * Stores source tokens owned directly by this function type layer.
     *
     * <p>This list is intended for punctuation and syntax tokens that belong to
     * the function type shape itself, such as {@code (}, {@code ,}, {@code )} and
     * {@code ->}. Tokens owned by parameter types and the return type remain in
     * those Type objects and are merged by getAllTokens().</p>
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Initializes a function type from parameter types and a return type.
     *
     * <p>The parameter type list and return type are stored directly. This is
     * consistent with the parser AST style in this package: parser actions build
     * short-lived intermediate containers, then pass their owned lists into the
     * final AST node. Callers that need ownership isolation should pass cloned
     * values before calling this constructor.</p>
     *
     * <p>A null parameterTypes value is allowed and is treated as an empty
     * parameter list by parameterCount(), getParameterTypes(), getAllTokens() and
     * toString(). A null returnType is also allowed, although it usually means
     * the function type is incomplete.</p>
     *
     * @param parameterTypes    ordered parameter Type list, or null for no parameters
     * @param returnType        function return Type, or null when incomplete
     */
    fun __init__(parameterTypes: pointer<ArrayList>, returnType: pointer<Type>)
    {
        this.parameterTypes = parameterTypes
        this.returnType = returnType
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Adds one source token owned by this function type layer.
     *
     * <p>This method is intended for syntax tokens around the function type
     * itself. For example, the parser may add the left parenthesis, comma tokens,
     * right parenthesis and arrow token here. Parameter type name tokens should
     * stay inside the corresponding Type objects.</p>
     *
     * <p>Null tokens are ignored so parser actions can chain this method without
     * defensive checks at every call site.</p>
     *
     * @param token             source token to append; may be null
     *
     * @return                  this FunctionType for chained construction
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<FunctionType>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Creates an independent copy of this FunctionType.
     *
     * <p>The returned FunctionType owns a new parameter list. Every non-null
     * parameter Type is cloned before being inserted into that list. The return
     * Type is cloned as well. Extra syntax tokens are copied as token pointers,
     * following the same ownership convention used by other parser nodes.</p>
     *
     * @return                  cloned function type
     */
    fun clone() -> pointer<FunctionType>
    {
        val parameters: pointer<ArrayList> = new ArrayList(sizeof(Type))

        if this.parameterTypes != null:
        {
            for (var i = 0; i < this.parameterTypes.length; i++):
            {
                val parameterType: pointer<Type> = this.parameterTypes.get(i) as pointer<Type>

                if parameterType != null:
                    parameters.push(parameterType.clone())
            }
        }

        var returnType: pointer<Type> = null

        if this.returnType != null:
            returnType = this.returnType.clone()

        val result: pointer<FunctionType> = new FunctionType(parameters, returnType)

        if this.extraTokens != null:
        {
            for (var i = 0; i < this.extraTokens.length; i++):
            {
                val token: pointer<Token> = this.extraTokens.get(i) as pointer<Token>
                result.addExtraToken(token)
            }
        }

        return result
    }


    /**
     * Returns a cloned parameter type list.
     *
     * <p>The returned ArrayList is a separate list object. Mutating the returned
     * list does not replace the FunctionType's internal list. The contained Type
     * values follow ArrayList clone semantics, so callers should not assume this
     * method performs a deep copy of every Type tree.</p>
     *
     * <p>If this FunctionType was constructed with a null parameter list, this
     * method returns a new empty ArrayList.</p>
     *
     * @return                  cloned parameter Type list
     */
    fun getParameterTypes() -> pointer<ArrayList> =
        if this.parameterTypes == null:
            new ArrayList(sizeof(Type))
        else:
            this.parameterTypes.clone()


    /**
     * Returns the return type pointer.
     *
     * <p>The returned pointer is the same Type object stored by this
     * FunctionType. This mirrors other AST nodes that expose child pointers
     * directly when the child is conceptually owned by the node.</p>
     *
     * @return                  return Type, or null
     */
    fun getReturnType() -> pointer<Type> = this.returnType


    /**
     * Returns whether this function type has a return type.
     *
     * <p>This is mostly a convenience check for parser and semantic code that
     * wants to guard incomplete FunctionType values before inspecting
     * returnType.</p>
     *
     * @return                  true when returnType is not null
     */
    fun haveReturnType() -> bool = this.returnType != null


    /**
     * Returns the number of parameter types.
     *
     * <p>A null parameter list is treated as an empty parameter list.</p>
     *
     * @return                  parameter count
     */
    fun parameterCount() -> int =
        if this.parameterTypes == null:
            0
        else:
            this.parameterTypes.length


    /**
     * Returns all source tokens belonging to this function type tree.
     *
     * <p>The result contains three groups of tokens:</p>
     *
     * <p>First, tokens from every parameter Type are collected in parameter
     * order. Second, tokens from the return Type are collected. Third, tokens
     * directly owned by this FunctionType layer are appended. The final list is
     * sorted by TokenPosition so callers receive source order rather than
     * construction order.</p>
     *
     * <p>The returned ArrayList is newly allocated. Mutating it does not mutate
     * this FunctionType.</p>
     *
     * @return                  sorted token list
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.parameterTypes != null:
        {
            for (var i = 0; i < this.parameterTypes.length; i++):
            {
                val parameterType: pointer<Type> = this.parameterTypes.get(i) as pointer<Type>

                if parameterType == null:
                    continue

                val tokens: pointer<ArrayList> = parameterType.getAllTokens()

                if tokens != null:
                    result.pushAll(tokens)
            }
        }

        if this.returnType != null:
        {
            val tokens: pointer<ArrayList> = this.returnType.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Converts this function type into a readable string.
     *
     * <p>The output is intended for diagnostics, debugging and AST tests. It is
     * source-like but not guaranteed to preserve every original token or spacing
     * choice.</p>
     *
     * <p>Examples:</p>
     *
     * <p>{@code () -> void}</p>
     *
     * <p>{@code (int, bool) -> char}</p>
     *
     * @return                  string builder containing the type text
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        sb.append("(")

        if this.parameterTypes != null:
        {
            for (var i = 0; i < this.parameterTypes.length; i++):
            {
                if i > 0:
                    sb.append(", ")

                val parameterType: pointer<Type> = this.parameterTypes.get(i) as pointer<Type>

                if parameterType != null:
                    sb.append(parameterType.toString())
            }
        }

        sb.append(") -> ")

        if this.returnType != null:
            sb.append(this.returnType.toString())

        return sb
    }
}
