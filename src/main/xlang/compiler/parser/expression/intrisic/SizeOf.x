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
import xlang.compiler.Type
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
 * The parsed type and surrounding syntax tokens are retained here. Calling
 * {@link #toExpression()} resolves the type size into the corresponding
 * integer expression.
 */
struct SizeOf
{
    private var targetType: pointer<Type>

    private var extraTokens: pointer<ArrayList>


    constructor(targetType: pointer<Type>)
    {
        this.targetType = targetType
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun getTargetType() -> pointer<Type> = this.targetType


    fun addExtraToken(token: pointer<Token>) -> pointer<SizeOf>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.targetType != null:
            result.pushAll(this.targetType.getAllTokens())

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


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


    /** Creates an automatically generated integer literal expression. */
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
