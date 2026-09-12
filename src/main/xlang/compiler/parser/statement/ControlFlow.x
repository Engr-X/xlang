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

#file.outerClass("ControlFlow")
package xlang.compiler.parser.statement

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct PassStatement
{
    private var extraTokens: pointer<ArrayList>


    constructor():
        this.extraTokens = new ArrayList(sizeof(Token))


    fun addExtraToken(token: pointer<Token>) -> pointer<PassStatement>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    fun toString() -> pointer<StringBuilder> = new StringBuilder("pass")
}


struct BreakStatement
{
    private var extraTokens: pointer<ArrayList>


    constructor():
        this.extraTokens = new ArrayList(sizeof(Token))


    fun addExtraToken(token: pointer<Token>) -> pointer<BreakStatement>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    fun toString() -> pointer<StringBuilder> = new StringBuilder("break")
}


struct ContinueStatement
{
    private var extraTokens: pointer<ArrayList>


    constructor():
        this.extraTokens = new ArrayList(sizeof(Token))


    fun addExtraToken(token: pointer<Token>) -> pointer<ContinueStatement>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    fun toString() -> pointer<StringBuilder> = new StringBuilder("continue")
}
