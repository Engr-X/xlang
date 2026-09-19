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
import xlang.compiler.Type
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


/**
 * Represents a grouped variable-definition statement containing multiple
 * {@code VariableDefine} nodes.
 *
 * <p>A {@code VariableDefines} stores an ordered collection of variable
 * definitions that were parsed as a single grouped declaration together with
 * additional syntax tokens retained from the original source.
 *
 * <p>The contained variable definitions are stored by reference and preserve
 * their insertion order. Additional definitions may be appended individually or
 * merged from another {@code VariableDefines} instance.
 *
 * <p>The mutability of the entire group can be changed through
 * {@code markAsConst()} or {@code markAsMut()}. These operations propagate the
 * requested modifier to every valid variable definition contained in the
 * collection.
 *
 * <p>The grouped declaration can be expanded through {@code expand()} into a
 * collection of independent {@code Statement} wrappers, with one statement
 * produced for each valid {@code VariableDefine}.
 *
 * <p>Additional syntax tokens are stored separately and participate in
 * {@code getAllTokens()}, but are not transferred to the individual statements
 * created by {@code expand()}.
 */
struct VariableDefines
{
    /**
     * The ordered collection of variable definitions represented by this
     * grouped declaration.
     *
     * <p>Each entry is expected to contain a {@code VariableDefine}. The
     * collection is allocated during construction and may later be extended
     * through {@code addDefine()} or {@code addDefines()}.
     */
    private var defines: pointer<ArrayList>

    /**
     * Additional syntax tokens associated with this grouped variable
     * declaration.
     *
     * <p>This collection may contain separators, declaration syntax, or other
     * lexical tokens that are not directly owned by the individual variable
     * definitions.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates a grouped variable declaration containing an initial variable
     * definition.
     *
     * <p>A new variable-definition collection and a new extra-token collection
     * are allocated.
     *
     * <p>The supplied {@code varDef} is appended directly to the definition
     * collection. The current implementation does not perform a null check
     * before inserting the initial value.
     *
     * <p>The variable definition is stored by reference and is not copied or
     * cloned.
     *
     * @param varDef            a pointer to the initial variable definition
     */
    constructor(varDef: pointer<VariableDefine>)
    {
        this.defines = new ArrayList(sizeof(VariableDefine))
        this.extraTokens = new ArrayList(sizeof(Token))
        this.defines.push(varDef)
    }


    /**
     * Appends a variable definition to this grouped declaration.
     *
     * <p>If {@code varDef} is {@code null}, no modification is performed.
     *
     * <p>A valid variable definition is appended to the end of the internal
     * collection, preserving insertion order.
     *
     * <p>The supplied variable definition is stored by reference and is not
     * copied or cloned.
     *
     * @param varDef            a pointer to the variable definition to append
     *
     * @return                  this {@code VariableDefines} instance
     */
    fun addDefine(varDef: pointer<VariableDefine>) -> pointer<VariableDefines>
    {
        if varDef != null:
            this.defines.push(varDef)

        return this
    }


    /**
     * Appends all variable definitions stored by another grouped declaration.
     *
     * <p>If {@code varDefs} is {@code null}, no modification is performed.
     *
     * <p>If the supplied object exists but its internal definition collection is
     * {@code null}, no modification is performed.
     *
     * <p>Otherwise, all entries from {@code varDefs.defines} are appended to
     * this declaration's internal collection using
     * {@code ArrayList.pushAll()}.
     *
     * <p>The source {@code VariableDefines} object and its collection are not
     * modified. The contained {@code VariableDefine} objects are referenced
     * rather than recursively copied or cloned.
     *
     * <p>Additional syntax tokens stored by {@code varDefs} are not copied by
     * this method.
     *
     * @param varDefs           a pointer to the grouped variable declaration
     *                          whose definitions should be appended
     *
     * @return                  this {@code VariableDefines} instance
     */
    fun addDefines(varDefs: pointer<VariableDefines>) -> pointer<VariableDefines>
    {
        if varDefs != null && varDefs.defines != null:
            this.defines.pushAll(varDefs.defines)

        return this
    }


    /**
     * Adds an additional syntax token to this grouped variable declaration.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The supplied token is appended to the internal extra-token collection
     * and is stored by reference rather than cloned.
     *
     * <p>Tokens added through this method are included when
     * {@code getAllTokens()} constructs the complete token collection associated
     * with this grouped declaration.
     *
     * @param token             a pointer to the syntax token to add
     *
     * @return                  this {@code VariableDefines} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<VariableDefines>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Marks every valid variable definition in this group as immutable.
     *
     * <p>The internal definition collection is traversed in order. Null entries
     * are ignored.
     *
     * <p>For each valid {@code VariableDefine},
     * {@code VariableDefine.markAsConst()} is invoked, causing the contained
     * declaration to use the constant modifier.
     *
     * <p>The contained variable definitions are modified directly; no copies or
     * replacement objects are created.
     *
     * @return                  this {@code VariableDefines} instance
     */
    fun markAsConst() -> pointer<VariableDefines>
    {
        for (var i = 0; i < this.defines.length; i++):
        {
            val variableDefine: pointer<VariableDefine> = this.defines.get(i) as pointer<VariableDefine>

            if variableDefine != null:
                variableDefine.markAsConst()
        }

        return this
    }


    /**
     * Marks every valid variable definition in this group as mutable.
     *
     * <p>The internal definition collection is traversed in order. Null entries
     * are ignored.
     *
     * <p>For each valid {@code VariableDefine},
     * {@code VariableDefine.markAsMut()} is invoked, causing the contained
     * declaration to use the mutable modifier.
     *
     * <p>The contained variable definitions are modified directly; no copies or
     * replacement objects are created.
     *
     * @return                  this {@code VariableDefines} instance
     */
    fun markAsMut() -> pointer<VariableDefines>
    {
        for (var i = 0; i < this.defines.length; i++):
        {
            val variableDefine: pointer<VariableDefine> = this.defines.get(i) as pointer<VariableDefine>

            if variableDefine != null:
                variableDefine.markAsMut()
        }

        return this
    }


    /**
     * Returns whether this grouped declaration is considered mutable.
     *
     * <p>If the internal definition collection is empty, this method returns
     * {@code false}.
     *
     * <p>Otherwise, only the first entry in the collection is inspected. If that
     * entry contains a valid {@code VariableDefine}, the result of
     * {@code VariableDefine.canModified()} is returned.
     *
     * <p>If the first entry is {@code null}, this method returns {@code false}
     * even if later definitions are mutable.
     *
     * <p>This behavior assumes that all variable definitions belonging to the
     * same grouped declaration share the same mutability state.
     *
     * @return                  {@code true} if the first valid stored position
     *                          represents a mutable variable definition;
     *                          {@code false} otherwise
     */
    fun canModified() -> bool
    {
        if this.defines.length <= 0:
            return false

        val variableDefine: pointer<VariableDefine> = this.defines.get(0) as pointer<VariableDefine>

        return variableDefine != null && variableDefine.canModified()
    }


    /**
     * Expands this grouped variable declaration into independent statement
     * wrappers.
     *
     * <p>A new {@code ArrayList} capable of storing {@code Statement} objects is
     * allocated for the result.
     *
     * <p>The internal variable-definition collection is traversed in its stored
     * order. Null entries are ignored.
     *
     * <p>Each valid {@code VariableDefine} is wrapped through
     * {@code Statement.fromVariableDefine()} and appended to the result.
     *
     * <p>The original variable-definition objects are reused by reference and
     * are not copied or cloned during expansion.
     *
     * <p>The resulting statement collection therefore preserves the order of the
     * valid variable definitions contained in this grouped declaration.
     *
     * <p>The additional syntax tokens stored directly by this
     * {@code VariableDefines} object are not transferred to the individual
     * statements produced by this method.
     *
     * @return                  a newly allocated list containing one
     *                          {@code Statement} wrapper for each valid variable
     *                          definition
     */
    fun expand() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Statement))

        for (var i = 0; i < this.defines.length; i++):
        {
            val variableDefine: pointer<VariableDefine> = this.defines.get(i) as pointer<VariableDefine>

            if variableDefine != null:
                result.push(Statement.fromVariableDefine(variableDefine))
        }

        return result
    }


    /**
     * Returns all tokens associated with this grouped variable declaration.
     *
     * <p>The internal definition collection is traversed in its stored order.
     * Null variable-definition entries are skipped.
     *
     * <p>For every valid {@code VariableDefine},
     * {@code VariableDefine.getAllTokens()} is invoked. If the returned token
     * collection is not {@code null}, all of its tokens are appended to the
     * result.
     *
     * <p>After tokens from all valid variable definitions have been collected,
     * the additional syntax tokens stored directly by this grouped declaration
     * are appended.
     *
     * <p>The complete token collection is sorted according to source position
     * using {@code TokenPosition.compareToken}. This restores the original
     * lexical ordering even though tokens are gathered independently from the
     * individual declarations and the grouped declaration itself.
     *
     * <p>A new list is allocated for the result. The individual token objects
     * are referenced rather than recursively cloned.
     *
     * @return                  a newly allocated list containing all tokens
     *                          associated with this grouped variable declaration
     *                          in source order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        for (var i = 0; i < this.defines.length; i++):
        {
            val variableDefine: pointer<VariableDefine> = this.defines.get(i) as pointer<VariableDefine>

            if variableDefine == null:
                continue

            val tokens: pointer<ArrayList> = variableDefine.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns the textual representation of this grouped variable declaration.
     *
     * <p>The internal variable-definition collection is traversed in its stored
     * order. Null entries are skipped.
     *
     * <p>For every valid {@code VariableDefine}, the textual representation
     * returned by {@code VariableDefine.toString()} is appended, followed by
     * {@code ",\n"}.
     *
     * <p>The resulting representation therefore generally has the form:
     *
     * <pre>
     * val first = expression,
     * val second = expression,
     * val third = expression,
     * </pre>
     *
     * <p>The current implementation appends a comma and newline after every
     * valid variable definition, including the final one.
     *
     * <p>If the collection contains no valid variable definitions, the returned
     * {@code StringBuilder} remains empty.
     *
     * <p>The returned builder is newly allocated and modifying its contents does
     * not modify the variable definitions stored by this grouped declaration.
     *
     * @return                  a pointer to a newly created {@code StringBuilder}
     *                          containing the textual representation of this
     *                          grouped variable declaration
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        for (var i = 0; i < this.defines.length; i++):
        {
            val variableDefine: pointer<VariableDefine> = this.defines.get(i) as pointer<VariableDefine>

            if variableDefine == null:
                continue

            sb.append(variableDefine.toString())
            sb.append(",\n")
        }

        return sb
    }
}
