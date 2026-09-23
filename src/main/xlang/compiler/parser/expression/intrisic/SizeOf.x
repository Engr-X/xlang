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

package xlang.compiler.parser.expression.intrisic

import xlang.System
import xlang.compiler.type.Type
import xlang.compiler.lexer.Tokenizer
import xlang.compiler.parser.expression.Atom
import xlang.compiler.parser.expression.Expression
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.TypeConvert


/**
 * Represents the intrinsic {@code sizeof(Type)} operation.
 *
 * <p>This structure stores the target type together with the syntax tokens
 * associated with the intrinsic expression.
 *
 * <p>Calling {@code toExpression()} evaluates the memory size of the target
 * type and converts the result into an automatically generated integer literal
 * expression.
 *
 * <p>The original tokens associated with the {@code sizeof} expression are
 * preserved and attached to the generated expression as extra tokens.
 */
struct SizeOf
{
    /**
     * The type whose memory size is requested.
     */
    private var targetType: pointer<Type>

    /**
     * The syntax tokens associated with this intrinsic expression.
     *
     * <p>This collection may contain tokens such as the {@code sizeof} keyword
     * and surrounding parentheses that are not directly owned by
     * {@code targetType}.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates a {@code sizeof} intrinsic for the specified target type.
     *
     * <p>A new empty collection is created for additional syntax tokens.
     *
     * @param                   targetType a pointer to the type whose memory size should be
     *                          evaluated
     */
    constructor(targetType: pointer<Type>)
    {
        this.targetType = targetType
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Returns the target type of this {@code sizeof} operation.
     *
     * @return a pointer to the target type
     */
    fun getTargetType() -> pointer<Type> = this.targetType


    /**
     * Adds an additional syntax token to this intrinsic expression.
     *
     * <p>If {@code token} is {@code null}, no token is added.
     *
     * <p>The token is stored by reference and is not copied.
     *
     * @param token             a pointer to the token to add
     *
     * @return                  this {@code SizeOf} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<SizeOf>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Returns the additional syntax tokens associated with this intrinsic.
     *
     * @return                  a pointer to the extra-token collection
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens


    /**
     * Returns all tokens associated with this {@code sizeof} expression.
     *
     * <p>The returned list contains all tokens belonging to the target type
     * together with the additional syntax tokens stored by this instance.
     *
     * <p>The collected tokens are sorted according to their source positions
     * using {@code TokenPosition.compareToken}.
     *
     * @return                  a newly allocated list containing all associated tokens in source
     *                          order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = this.extraTokens.clone()

        if this.targetType != null:
            result.pushAll(this.targetType.getAllTokens())

        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Converts this {@code sizeof} operation into an integer literal expression.
     *
     * <p>The memory size of {@code targetType} is obtained through
     * {@code Type.getMemSize()} and converted into an automatically generated
     * integer literal.
     *
     * <p>All tokens associated with the original {@code sizeof} expression are
     * attached to the generated expression as extra tokens so that source
     * information can be preserved after constant folding.
     *
     * <p>If no target type is available, this method returns {@code null}.
     *
     * @return                  a pointer to the generated integer expression, or {@code null}
     *                          if the target type is unavailable
     */
    fun toExpression() -> pointer<Expression>
    {
        if this.targetType == null:
            return null

        val result: pointer<Expression> = SizeOf.intExpression(this.targetType.getMemSize())

        val tokens: pointer<ArrayList> = this.getAllTokens()

        for (var i = 0; i < tokens.length; i++):
            result.addExtraToken(tokens.get(i) as pointer<Token>)

        return result
    }


    /**
     * Creates an automatically generated integer literal expression.
     *
     * <p>The supplied integer value is converted to its decimal textual
     * representation and stored in an automatically generated integer token.
     *
     * <p>The generated token uses {@code TokenPosition.autoGenPos()} because it
     * does not directly correspond to a token written in the original source.
     *
     * <p>The token is then wrapped in an integer immediate {@code Atom} and
     * converted into an {@code Expression}.
     *
     * @param                   value the integer value represented by the generated expression
     *
     * @return                  a pointer to the generated integer literal expression
     */
    private static fun intExpression(value: int) -> pointer<Expression>
    {
        val text: pointer<char> = System.allocMemory(16 * sizeof(char)) as pointer<char>
        TypeConvert.intToString(text, value, 10)

        val token: pointer<Token> = new Token(
            Tokenizer.TK_INTEGER,
            TokenPosition.autoGenPos(),
            text)
        val tokens: pointer<ArrayList> = new ArrayList(sizeof(pointer<*>))
        val tokenValue: pointer<*> = token as pointer<*>

        tokens.push(tokenValue.ref)
        return Expression.fromAtom(new Atom(Atom.INTEGER_IMM_KIND, tokens))
    }
}
