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

@file.class("TypeConvert")
package xlang.util

import xlang.util.string.String
import xlang.functional.FList


inline fun charToInt(ch: char) -> int = ch as int - '0' as int

inline fun intToChar(v: int) -> char = (v + '0' as int) as char


// suitable for (INT_MIN, INT_MAX]
fun intToString(mut dest: pointer<char>, mut value: int)
{
    if value == 0:
    {
        dest[0] = '0'
        dest[1] = '\0'
        return
    }

    if value < 0:
    {
        value = -value
        dest[0] = '-'
        dest++
    }

    var offset: int = 0

    while value > 0:
    {
        val number: int = value % 10
        dest[offset++] = intToChar(number)
        value /= 10
    }

    // then reverse [0 .. offset)
    FList.reverse(dest, offset, sizeof(char))
    dest[offset] = String.NULL_CHAR
}
