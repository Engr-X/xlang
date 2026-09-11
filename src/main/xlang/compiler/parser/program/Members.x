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

#file.outerClass("Members")
package xlang.compiler.parser.program

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct Members
{
    private val members: pointer<ArrayList>


    constructor():
        this.members = new ArrayList(sizeof(Member))


    constructor(member: pointer<Member>)
    {
        this.members = new ArrayList(sizeof(Member))
        this.push(member)
    }


    fun push(member: pointer<Member>) -> pointer<Members>
    {
        if member != null:
            this.members.push(member)

        return this
    }


    fun pushAll(members: pointer<Members>) -> pointer<Members>
    {
        if members != null && members.members != null:
            this.members.pushAll(members.members)

        return this
    }


    fun length() -> int = this.members.length


    fun get(index: int) -> pointer<Member>
    {
        if index < 0 || index >= this.members.length:
            return null

        return this.members.get(index) as pointer<Member>
    }


    fun getMembers() -> pointer<ArrayList> = this.members.clone()


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        for (var i = 0; i < this.members.length; i++):
        {
            val member: pointer<Member> = this.get(i)

            if member == null:
                continue

            val tokens: pointer<ArrayList> = member.getAllTokens()

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
        var appendedMember: bool = false

        for (var i = 0; i < this.members.length; i++):
        {
            val member: pointer<Member> = this.get(i)

            if member == null:
                continue

            if appendedMember:
                sb.newline()

            sb.append(member.toString())
            appendedMember = true
        }

        return sb
    }
}
