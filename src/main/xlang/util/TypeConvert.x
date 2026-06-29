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

/**
 * Provides basic low-level type conversion utilities.
 *
 * This module contains simple conversions between primitive values and
 * C-style null-terminated strings.
 *
 * The string conversion functions write into caller-provided buffers.
 * The caller must ensure that the destination buffer has enough capacity for
 * the generated characters and the final null terminator.
 */

import xlang.util.string.String
import xlang.functional.FList


/**
 * Converts an ASCII digit character to its integer value.
 *
 * This function assumes that the input character is between '0' and '9'.
 * No validation is performed.
 *
 * @param ch ASCII digit character
 * @return integer value represented by the digit
 */
inline fun charToInt(ch: char) -> int = ch as int - '0' as int


/**
 * Converts a single decimal digit to its ASCII character.
 *
 * This function assumes that the input value is between 0 and 9.
 * No validation is performed.
 *
 * @param v decimal digit value
 * @return ASCII digit character
 */
inline fun intToChar(v: int) -> char = (v + '0' as int) as char


/**
 * Converts an integer to a null-terminated decimal string.
 *
 * The result is written into the destination buffer.
 * The destination buffer must have enough capacity to store the sign,
 * all decimal digits, and the final null terminator.
 *
 * This function supports values in the range (INT_MIN, INT_MAX].
 * INT_MIN is not supported because negating INT_MIN overflows in two's
 * complement integer representation.
 *
 * @param dest pointer to the destination character buffer
 * @param value integer value to convert
 */
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
