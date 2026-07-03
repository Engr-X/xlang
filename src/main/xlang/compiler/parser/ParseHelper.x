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


@file.class("ParseHelper")
package xlang.compiler.parser


import xlang.util.string.String


// private fun 


fun unescapeChar(string: pointer<char>) -> char
{
    if String.strlen(string) == 1:
        return string[0]

    if string[0] != '\\':
        return (-1) as char

    return (
        if string[1] == 'n': '\n'
        elif string[1] == 't': '\t'
        elif string[1] == 'r': '\r'
        elif string[1] == '\\': '\\'
        elif string[1] == '\'': '\''
        elif string[1] == '"': '"'
        elif string[1] == '0': '\0'
        elif string[1] == 'a': '\a'
        elif string[1] == 'b': '\b'
        elif string[1] == 'f': '\f'
        elif string[1] == 'v': '\v'
        else: (-1) as char
    )
}
