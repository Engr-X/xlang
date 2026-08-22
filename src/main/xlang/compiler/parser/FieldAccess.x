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

import xlang.util.string.String
import xlang.util.string.StringBuilder


struct FieldAccess
{
    private var host: pointer<Expression>

    private var fieldName: pointer<char>


    fun __init__(host: pointer<Expression>, fieldName: pointer<char>)
    {
        this.host = host
        this.fieldName = String.strdup(fieldName)
    }


    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        if this.host != null:
        {
            sb.append(this.host.toString())
            sb.append('.')
        }

        sb.append(this.fieldName)
        return sb
    }
}