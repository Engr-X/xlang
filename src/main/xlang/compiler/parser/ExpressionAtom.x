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

@file.class("ExpressionAtom")
package xlang.compiler.parser


struct ExpressionAtom
{
    static val BOOL_IMM_KIND: int = 0
    static val INTEGER_IMM_KIND: int = 1
    static val LONG_IMM_KIND: int = 2
    static val FLOAT_IMM_KIND: int = 3
    static val DOUBLE_IMM_KIND: int = 4
    

    val kind: int
    val value: pointer<char>
}
