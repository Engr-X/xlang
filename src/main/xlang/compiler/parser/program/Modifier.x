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

package xlang.compiler.parser.program

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.String
import xlang.util.string.StringBuilder


struct Modifier
{
    private var keyword: pointer<char>

    private var extraTokens: pointer<ArrayList>


    constructor(keyword: pointer<char>)
    {
        this.keyword = String.strdup(keyword)
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun getKeyword() -> pointer<char> = String.strdup(this.keyword)


    fun addExtraToken(token: pointer<Token>) -> pointer<Modifier>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    fun toString() -> pointer<StringBuilder> = new StringBuilder(this.keyword)
}


struct ModifierListMaybe
{
    private var list: pointer<ArrayList>


    constructor():
        this.list = new ArrayList(sizeof(Modifier))


    constructor(list: pointer<ArrayList>):
        this.list = if list == null:
                new ArrayList(sizeof(Modifier))
            else:
                list


    fun toModifierList() -> pointer<ArrayList> = this.list
}
