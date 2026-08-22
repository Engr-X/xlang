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
 */
package xlang.compiler.parser

import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct IndexAccess
{
    private var host: pointer<Expression>

    private var indices: pointer<ArrayList>


    fun __init__(host: pointer<Expression>, indices: pointer<ListLiteral>)
    {
        this.host = host
        this.indices = indices.getList()
    }


    fun addIndex(index: pointer<Expression>) -> pointer<IndexAccess>
    {
        this.indices.push(index.ref)
        return this
    }


    fun getHost() -> pointer<Expression> = this.host


    fun getIndex(index: int) -> pointer<Expression>
    {
        val slot: pointer<pointer<Expression>> = this.indices.get(index) as pointer<pointer<Expression>>

        if slot == null:
            return null

        return slot.deref
    }


    fun indicesCount() -> int = this.indices.length


    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        sb.append("IndexAccess(")

        if this.host != null:
            sb.append(this.host.toString())

        sb.append(", [")

        var appendedIndex: bool = false

        for (var i = 0; i < this.indices.length; i++):
        {
            val index: pointer<Expression> = this.getIndex(i)

            if index == null:
                continue

            if appendedIndex:
                sb.append(", ")

            sb.append(index.toString())
            appendedIndex = true
        }

        sb.append("])")
        return sb
    }
}