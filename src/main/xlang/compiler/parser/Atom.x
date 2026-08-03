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

import xlang.util.ArrayList


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

    static val VARIABLE_KIND: int = 8
    static val STATEMENT_ATOM_KIND: int = 9

    
    private val kind: int
    
    private val tokens: pointer<ArrayList>


    fun __init__(kind: int, tokens: pointer<ArrayList>)
    {
        this.kind = kind
        this.tokens = tokens
    }
}
