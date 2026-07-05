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

@file.class("Operation")
package xlang.compiler.parser

import xlang.util.string.String


struct Operation
{
    private static val MAX_OPERATION_LENGTH: int = 4

    static val PREFIX_TYPE: int = 0
    static val INFIX_TYPE: int = 1
    static val POSTFIX_TYPE: int = 2

    static val LEFT_ASSOC: int = 0
    static val RIGHT_ASSOC: int = 1
    static val NO_ASSOC: int = 2


    val id: int
    val symbol: pointer<char>
    val fixity: int
    val associativity: int
    val priority: int


    fun __init__(id: int, symbol: pointer<char>, fixity: int, associativity: int, priority: int)
    {
        this.id = id
        this.symbol = String.strdup(symbol)
        this.fixity = fixity
        this.priority = priority
        this.associativity = associativity
    }
}
