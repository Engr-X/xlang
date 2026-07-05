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

package xlang.compiler.parser

import xlang.util.string.String


struct ExpressionAtom
{
    static val VARIABLE_KIND: int = 0

    static val BOOL_IMM_KIND: int = 1
    static val CHAR_IMM_KIND: int = 2
    static val INTEGER_IMM_KIND: int = 3
    static val LONG_IMM_KIND: int = 4
    static val FLOAT_IMM_KIND: int = 5
    static val DOUBLE_IMM_KIND: int = 6
    static val STRING_IMM_KIND: int = 7
    static val NULL_IMM_KIND: int = 8

    
    val kind: int
    
    val value: pointer<char>


    fun __init__(kind: int, value: pointer<char>)
    {
        this.kind = kind
        this.value = String.strdup(value)
    }
}


// struct Expression
// {
//     val operationId: int
//     val argc: int
//     val 
// }
