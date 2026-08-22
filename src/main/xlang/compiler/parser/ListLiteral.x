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


struct ListLiteral
{
    private var list: pointer<ArrayList>


    fun __init__():
        this.list = new ArrayList(sizeof(pointer<Expression>))


    fun __init__(list: pointer<ArrayList>)
    {
        this.list = list
    }


    fun addExpression(expression: pointer<Expression>) -> pointer<ListLiteral>
    {
        this.list.push(expression.ref)
        return this
    }


    fun getList() -> pointer<ArrayList> = this.list
}