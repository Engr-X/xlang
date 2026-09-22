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

package xlang.compiler.parser.expression

import xlang.lexer.Token
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


/**
 * Represents an atomic expression.
 *
 * <p>An {@code Atom} is one of the smallest expression units recognized by the
 * expression parser. It may represent a literal value, an identifier, or another
 * atomic construct such as a statement expression.
 *
 * <p>The atom stores the tokens that produced it together with a kind identifier
 * describing how those tokens should be interpreted.
 *
 * <p>For simple literal atoms, {@code simpleInfer()} can determine the
 * corresponding built-in type without requiring further semantic analysis.
 */
struct Atom
{
    // Identifies the {@code null} literal.
    static val NULL_IMM_KIND: int = 0

    // Identifies a boolean literal.
    static val BOOL_IMM_KIND: int = 1

    // Identifies a character literal.
    static val CHAR_IMM_KIND: int = 2

    // Identifies an integer literal.
    static val INTEGER_IMM_KIND: int = 3

    // Identifies a long integer literal.
    static val LONG_IMM_KIND: int = 4

    // Identifies a single-precision floating-point literal.
    static val FLOAT_IMM_KIND: int = 5

    // Identifies a double-precision floating-point literal.
    static val DOUBLE_IMM_KIND: int = 6

    // Identifies a string literal.
    static val STRING_IMM_KIND: int = 7

    // Identifies an identifier atom.
    static val IDENTIFIER_KIND: int = 8

    // Identifies an atom backed by a statement-like construct.
    static val STATEMENT_ATOM_KIND: int = 9

    // The kind identifier describing this atomic expression.
    private val kind: int

    /**
     * The tokens associated with this atom.
     *
     * <p>The list stores generic pointer slots whose referenced values are
     * expected to point to {@code Token} instances.
     */
    private val tokens: pointer<ArrayList>

    /**
     * The type inferred or explicitly assigned to this atom.
     *
     * <p>This value may be {@code null} when the type has not yet been resolved
     * or when the atom kind cannot be inferred by {@code simpleInfer()}.
     */
    private val inferredType: pointer<Type>


    /**
     * Creates an atomic expression of the specified kind.
     *
     * <p>The supplied token list is stored by reference and is not copied.
     *
     * @param kind              the kind identifier of the atom
     * @param tokens            a pointer to the token collection associated with the atom
     */
    constructor(kind: int, tokens: pointer<ArrayList>)
    {
        this.kind = kind
        this.tokens = tokens
    }


    /**
     * Assigns an inferred type to this atom.
     *
     * <p>The supplied type is stored by reference and is not copied.
     *
     * @param inferredType      a pointer to the type to associate with this atom
     *
     * @return                  this {@code Atom} instance
     */
    fun setType(inferredType: pointer<Type>) -> pointer<Atom>
    {
        this.inferredType = inferredType
        return this
    }


    /**
     * Performs simple type inference based solely on the kind of this atom.
     *
     * <p>This method handles literal atoms whose types can be determined
     * directly without symbol lookup or additional semantic information.
     *
     * <p>The following mappings are currently supported:
     * <ul>
     *     <li>{@code NULL_IMM_KIND} - {@code void}</li>
     *     <li>{@code BOOL_IMM_KIND} - {@code bool}</li>
     *     <li>{@code CHAR_IMM_KIND} - {@code char}</li>
     *     <li>{@code INTEGER_IMM_KIND} - {@code int}</li>
     *     <li>{@code LONG_IMM_KIND} - {@code long}</li>
     *     <li>{@code FLOAT_IMM_KIND} - {@code float}</li>
     *     <li>{@code DOUBLE_IMM_KIND} - the configured floating-point type</li>
     *     <li>{@code STRING_IMM_KIND} - the early string type</li>
     * </ul>
     *
     * <p>If the atom kind cannot be inferred directly, {@code null} is assigned
     * to {@code inferredType}.
     *
     * @return                  a pointer to the inferred type, or {@code null} if no simple type
     *                          can be determined
     */
    fun simpleInfer() -> pointer<Type> =
        this.inferredType = if this.kind == NULL_IMM_KIND:
                Type.voidType()
            elif this.kind == BOOL_IMM_KIND:
                Type.boolType()
            elif this.kind == CHAR_IMM_KIND:
                Type.charType()
            elif this.kind == INTEGER_IMM_KIND:
                Type.intType()
            elif this.kind == LONG_IMM_KIND:
                Type.longType()
            elif this.kind == FLOAT_IMM_KIND:
                Type.floatType()
            elif this.kind == DOUBLE_IMM_KIND:
                Type.floatType()
            elif this.kind == STRING_IMM_KIND:
                Type.earlyStringType()
            else: null


    /**
     * Returns all tokens associated with this atom.
     *
     * <p>Each entry in the internal token list is interpreted as a pointer slot
     * containing a {@code Token} pointer. Null slots and null token references
     * are ignored.
     *
     * <p>The returned list contains the token references themselves; the tokens
     * are not cloned.
     *
     * @return                  a newly allocated list containing all valid tokens associated with
     *                          this atom
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        for (var i = 0; i < this.tokens.length; i++):
        {
            val slot: pointer<pointer<*>> = this.tokens.get(i) as pointer<pointer<*>>

            if slot == null || slot.deref == null:
                continue

            val token: pointer<Token> = slot.deref as pointer<Token>

            if token != null:
                result.push(token)
        }

        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns the textual representation of this atom.
     *
     * <p>The text of each valid token is appended in the order in which the
     * tokens appear in the internal token list.
     *
     * <p>Null token slots, null token references, and tokens without textual
     * content are ignored.
     *
     * @return                  a pointer to a newly created {@code StringBuilder} containing the
     *                          textual representation of this atom
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        for (var i = 0; i < this.tokens.length; i++):
        {
            val slot: pointer<pointer<*>> = this.tokens.get(i) as pointer<pointer<*>>

            if slot == null:
                continue

            val token: pointer<Token> = slot.deref as pointer<Token>

            if token == null || token.text == null:
                continue

            sb.append(token.text)
        }

        return sb
    }
}
