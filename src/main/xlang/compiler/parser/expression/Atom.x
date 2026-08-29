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
@file.class("Atom")
package xlang.compiler.parser.expression

import xlang.lexer.Token
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct Atom
{
    static val NULL_IMM_KIND: int = 0
    static val BOOL_IMM_KIND: int = 1
    static val CHAR_IMM_KIND: int = 2
    static val INTEGER_IMM_KIND: int = 3
    static val LONG_IMM_KIND: int = 4
    static val FLOAT_IMM_KIND: int = 5
    static val DOUBLE_IMM_KIND: int = 6
    static val STRING_IMM_KIND: int = 7
    static val IDENTIFIER_KIND: int = 8
    static val STATEMENT_ATOM_KIND: int = 9


    private val kind: int
    
    private val tokens: pointer<ArrayList>

    private val inferredType: pointer<NormalType>
    


    fun __init__(kind: int, tokens: pointer<ArrayList>)
    {
        this.kind = kind
        this.tokens = tokens
    }


    fun setType(inferredType: pointer<NormalType>) -> pointer<Atom>
    {
        this.inferredType = inferredType
        return this
    }


    fun simpleInfer() -> pointer<NormalType> =
        this.inferredType = if this.kind == NULL_IMM_KIND:
                NormalType.voidType()
            elif this.kind == BOOL_IMM_KIND:
                NormalType.boolType()
            elif this.kind == CHAR_IMM_KIND:
                NormalType.charType()
            elif this.kind == INTEGER_IMM_KIND:
                NormalType.intType()
            elif this.kind == LONG_IMM_KIND:
                NormalType.longType()
            elif this.kind == FLOAT_IMM_KIND:
                NormalType.floatType()
            elif this.kind == DOUBLE_IMM_KIND:
                NormalType.floatType()
            elif this.kind == STRING_IMM_KIND:
                NormalType.earlyStringType()
            else: null


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        for (var i = 0; i < this.tokens.length; i++):
        {
            val slot: pointer<pointer<*>> = this.tokens.get(i) as pointer<pointer<*>>

            if slot == null:
                continue

            val token: pointer<Token> = slot.deref as pointer<Token>

            if token != null:
                result.push(token)
        }

        return result
    }


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
