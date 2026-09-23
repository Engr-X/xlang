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

package xlang.compiler.parser.statement

import xlang.compiler.lexer.Tokenizer
import xlang.compiler.parser.expression.Assignment
import xlang.compiler.parser.expression.Atom
import xlang.compiler.parser.expression.Expression
import xlang.compiler.parser.program.Field
import xlang.compiler.type.Type
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


/**
 * Represents a local variable-definition statement in the statement abstract
 * syntax tree.
 *
 * <p>A {@code VariableDefine} stores the mutability of the declared variable,
 * an optional explicitly declared type, the variable name, an optional
 * initialization expression, and additional syntax tokens retained from the
 * original source.
 *
 * <p>The mutability state reuses the modifier constants exposed by
 * {@code Field}. A newly created variable definition is immutable by default
 * and therefore uses {@code Field.constModifier()}. The state may later be
 * changed through {@code markAsMut()} or {@code markAsConst()}.
 *
 * <p>The declared type is optional. When no type is supplied, the variable may
 * rely on type inference or another later compiler phase to determine its type.
 *
 * <p>The initialization expression is also optional, allowing both initialized
 * and uninitialized variable declarations to be represented.
 *
 * <p>The variable name, declared type, and initialization expression are stored
 * by reference and are not copied or cloned by the constructors.
 */
struct VariableDefine
{
    /**
     * The mutability modifier associated with this variable declaration.
     *
     * <p>The value is expected to correspond to either
     * {@code Field.constModifier()} or {@code Field.mutModifier()}.
     *
     * <p>New variable definitions are initialized as constant declarations.
     */
    private var modifier: int

    /**
     * The optional explicitly declared type of the variable.
     *
     * <p>A {@code null} value indicates that no explicit type annotation was
     * supplied.
     *
     * <p>The type object is stored by reference and is not cloned by the
     * constructors.
     */
    private var declaredType: pointer<Type>

    /**
     * The null-terminated name of the declared variable.
     *
     * <p>The character pointer is stored directly by the constructors and is not
     * duplicated using {@code String.strdup}.
     *
     * <p>The value may be {@code null} if the declaration was constructed
     * without a valid variable name.
     */
    private var varName: pointer<char>

    /**
     * The optional expression used to initialize the declared variable.
     *
     * <p>A {@code null} value indicates that the variable declaration does not
     * contain an initializer.
     *
     * <p>The expression is stored by reference and is not copied or cloned.
     */
    private var assignExpr: pointer<Expression>

    /**
     * Additional syntax tokens associated with this variable declaration.
     *
     * <p>This collection may contain the {@code val} or {@code var} keyword,
     * identifier tokens, type separators, assignment syntax, or other lexical
     * elements that are not directly represented by the stored AST nodes.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates a variable declaration without an explicitly declared type.
     *
     * <p>The declaration is initialized as immutable using
     * {@code Field.constModifier()}.
     *
     * <p>The declared type is initialized to {@code null}. The supplied variable
     * name and initialization expression are stored directly and are not copied
     * or cloned.
     *
     * <p>The initialization expression may itself be {@code null}, representing
     * a declaration without an initial value.
     *
     * <p>A new empty collection is allocated for additional syntax tokens.
     *
     * @param varName           a pointer to the null-terminated variable name
     * @param assignExpr        a pointer to the initialization expression, or
     *                          {@code null} if no initial value is specified
     */
    constructor(varName: pointer<char>, assignExpr: pointer<Expression>)
    {
        this.modifier = Field.constModifier()
        this.declaredType = null
        this.varName = varName
        this.assignExpr = assignExpr
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Creates a variable declaration with an optional explicitly declared type.
     *
     * <p>The declaration is initialized as immutable using
     * {@code Field.constModifier()}.
     *
     * <p>The supplied declared type, variable name, and initialization
     * expression are stored directly and are not copied or cloned.
     *
     * <p>Both {@code declaredType} and {@code assignExpr} may be {@code null},
     * allowing the declaration to omit either its type annotation or its
     * initializer.
     *
     * <p>A new empty collection is allocated for additional syntax tokens.
     *
     * @param declaredType      a pointer to the explicitly declared variable
     *                          type, or {@code null} if no type is specified
     * @param varName           a pointer to the null-terminated variable name
     * @param assignExpr        a pointer to the initialization expression, or
     *                          {@code null} if no initial value is specified
     */
    constructor(declaredType: pointer<Type>, varName: pointer<char>, assignExpr: pointer<Expression>)
    {
        this.modifier = Field.constModifier()
        this.declaredType = declaredType
        this.varName = varName
        this.assignExpr = assignExpr
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Returns whether this variable declaration contains an explicit type.
     *
     * <p>The declaration is considered explicitly typed whenever
     * {@code declaredType} is not {@code null}.
     *
     * @return                  {@code true} if an explicit declared type is
     *                          present; {@code false} otherwise
     */
    fun haveDeclaredType() -> bool = this.declaredType != null


    /**
     * Returns whether this variable declaration contains an initialization
     * expression.
     *
     * <p>The declaration is considered initialized whenever
     * {@code assignExpr} is not {@code null}.
     *
     * @return                  {@code true} if an initialization expression is
     *                          present; {@code false} otherwise
     */
    fun haveInitialValue() -> bool = this.assignExpr != null


    /**
     * Marks this variable declaration as mutable.
     *
     * <p>The internal modifier value is replaced with the value returned by
     * {@code Field.mutModifier()}.
     *
     * <p>No other declaration state is modified.
     *
     * @return                  this {@code VariableDefine} instance
     */
    fun markAsMut() -> pointer<VariableDefine>
    {
        this.modifier = Field.mutModifier()
        return this
    }


    /**
     * Marks this variable declaration as immutable.
     *
     * <p>The internal modifier value is replaced with the value returned by
     * {@code Field.constModifier()}.
     *
     * <p>No other declaration state is modified.
     *
     * @return                  this {@code VariableDefine} instance
     */
    fun markAsConst() -> pointer<VariableDefine>
    {
        this.modifier = Field.constModifier()
        return this
    }


    /**
     * Adds an additional syntax token to this variable declaration.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The supplied token is appended to the internal extra-token collection
     * and is stored by reference rather than cloned.
     *
     * <p>Tokens added through this method are included when
     * {@code getAllTokens()} constructs the complete token collection associated
     * with this declaration.
     *
     * @param token             a pointer to the syntax token to add
     *
     * @return                  this {@code VariableDefine} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<VariableDefine>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Returns whether this variable declaration may be modified after
     * initialization.
     *
     * <p>The result is determined by comparing the internal modifier value
     * directly with {@code Field.mutModifier()}.
     *
     * <p>Any modifier value other than the mutable modifier is treated as
     * non-modifiable by this method.
     *
     * @return                  {@code true} if this declaration uses the mutable
     *                          modifier; {@code false} otherwise
     */
    fun canModified() -> bool = this.modifier == Field.mutModifier()


    /**
     * Returns all tokens associated with this variable declaration.
     *
     * <p>If an explicit declared type is present, its token collection is
     * obtained through {@code Type.getAllTokens()}. If the returned collection
     * is not {@code null}, its tokens are appended to the result.
     *
     * <p>If an initialization expression is present, all tokens returned by
     * {@code Expression.getAllTokens()} are appended when that collection is not
     * {@code null}.
     *
     * <p>The additional syntax tokens stored directly by this declaration are
     * then appended. These tokens may contain the declaration keyword, variable
     * identifier, type separator, assignment operator, or other lexical
     * elements retained during parsing.
     *
     * <p>The complete token collection is sorted according to source position
     * using {@code TokenPosition.compareToken}. This restores the original
     * lexical ordering even though tokens are collected independently from the
     * type, initialization expression, and declaration-level token collection.
     *
     * <p>A new list is allocated for the result. The individual token objects
     * are referenced rather than recursively cloned.
     *
     * @return                  a newly allocated list containing all tokens
     *                          associated with this variable declaration in
     *                          source order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = this.extraTokens.clone()


        if this.declaredType != null:
        {
            val tokens: pointer<ArrayList> = this.declaredType.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        if this.assignExpr != null:
        {
            val tokens: pointer<ArrayList> = this.assignExpr.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns the textual representation of this variable declaration.
     *
     * <p>The representation begins with {@code "var "} when
     * {@code canModified()} returns {@code true}. Otherwise, it begins with
     * {@code "val "}.
     *
     * <p>If a variable name is available, it is appended immediately after the
     * declaration keyword.
     *
     * <p>If an explicit declared type is present, {@code ": "} is appended,
     * followed by the textual representation returned by
     * {@code Type.toString()}.
     *
     * <p>If an initialization expression is present, {@code " = "} is appended,
     * followed by the textual representation returned by
     * {@code Expression.toString()}.
     *
     * <p>The resulting representation may therefore have forms such as:
     *
     * <pre>
     * val value
     * val value: int
     * val value = expression
     * var value: int = expression
     * </pre>
     *
     * <p>If {@code varName} is {@code null}, the declaration keyword is still
     * emitted and any available type or initialization expression is appended
     * according to the normal formatting rules.
     *
     * <p>The returned {@code StringBuilder} is newly allocated and modifying its
     * contents does not modify the underlying variable-declaration AST node.
     *
     * @return                  a pointer to a newly created {@code StringBuilder}
     *                          containing the textual representation of this
     *                          variable declaration
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = if this.canModified():
            new StringBuilder("var ")
        else:
            new StringBuilder("val ")

        if this.varName != null:
            sb.append(this.varName)

        if this.declaredType != null:
        {
            sb.append(": ")
            sb.append(this.declaredType.toString())
        }

        if this.assignExpr != null:
        {
            sb.append(" = ")
            sb.append(this.assignExpr.toString())
        }

        return sb
    }
}
