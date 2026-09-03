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
@file.class("ModifierList")
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


    fun __init__(keyword: pointer<char>)
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


struct ModifierList
{
    private var list: pointer<ArrayList>


    fun __init__():
        this.list = new ArrayList(sizeof(pointer<Modifier>))


    fun __init__(modifier: pointer<Modifier>)
    {
        this.list = new ArrayList(sizeof(pointer<Modifier>))
        this.push(modifier)
    }


    fun push(modifier: pointer<Modifier>) -> pointer<ModifierList>
    {
        if modifier != null:
            this.list.push(modifier.ref)

        return this
    }


    fun pushFront(modifier: pointer<Modifier>) -> pointer<ModifierList>
    {
        if modifier != null:
            this.list.pushFront(modifier.ref)

        return this
    }


    fun length() -> int = this.list.length


    fun get(index: int) -> pointer<Modifier>
    {
        if index < 0 || index >= this.list.length:
            return null

        val slot: pointer<pointer<Modifier>> = this.list.get(index) as pointer<pointer<Modifier>>

        if slot == null:
            return null

        return slot.deref
    }


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        for (var i = 0; i < this.list.length; i++):
        {
            val modifier: pointer<Modifier> = this.get(i)

            if modifier == null:
                continue

            val tokens: pointer<ArrayList> = modifier.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()
        var appendedToken: bool = false

        for (var i = 0; i < this.list.length; i++):
        {
            val modifier: pointer<Modifier> = this.get(i)

            if modifier == null:
                continue

            if appendedToken:
                sb.append(' ')

            sb.append(modifier.toString())
            appendedToken = true
        }

        return sb
    }
}
