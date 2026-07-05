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
 * Smallest supported radix for integer string conversion.
 */
private val MIN_RADIX: int = 2


/**
 * Largest supported radix for integer string conversion.
 *
 * Radix 36 is the largest base that can be represented with decimal digits
 * plus the English alphabet.
 */
private val MAX_RADIX: int = 36


/**
 * Converts an ASCII digit character to its integer value.
 *
 * This function assumes that the input character is between '0' and '9'.
 * No validation is performed.
 *
 * @param ch                    ASCII digit character
 * @return                      integer value represented by the digit
 */
inline fun charToInt(ch: char) -> int = (ch - '0') as int


/**
 * Converts a single decimal digit to its ASCII character.
 *
 * This function assumes that the input value is between 0 and 9.
 * No validation is performed.
 *
 * @param v                     decimal digit value
 * @return                      ASCII digit character
 */
inline fun intToChar(v: int) -> char = v + '0'


/**
 * Checks whether a radix can be represented by decimal digits plus letters.
 *
 * Supported radices are 2 through 36.
 *
 * @param radix                 numeric base to validate
 * @return                      true when the radix is supported
 */
inline fun checkRadix(radix: int) -> bool = MIN_RADIX <= radix && radix <= MAX_RADIX


/**
 * Converts one radix digit value to its character representation.
 *
 * Values 0 through 9 are converted to '0' through '9'. Values 10 through 35
 * are converted to lowercase letters 'a' through 'z'.
 *
 * This function assumes that the input value is already in the range allowed
 * by the caller's radix.
 *
 * @param v                     digit value to convert
 * @return                      character representation of the digit
 */
private fun intToRadixChar(v: int) -> char
{
    if v < 10:
        return intToChar(v)

    return v - 10 + 'a'
}


private fun radixCharToInt(ch: char) -> int
{
    if '0' <= ch && ch <= '9':
        return (ch - '0') as int

    if 'a' <= ch && ch <= 'z':
        return (ch - 'a' + 10) as int

    if 'A' <= ch && ch <= 'Z':
        return (ch - 'A' + 10) as int

    return -1
}


/**
 * Converts a raw digit sequence to a long using the given radix.
 *
 * The raw text must not include a sign, radix prefix such as 0x/0X, or suffix
 * such as L/l. The caller passes the exact number of characters to parse.
 *
 * This function returns 0L when the radix is invalid or a digit does not fit
 * the radix. Overflow is not checked.
 *
 * @param text                  raw digit sequence
 * @param radix                 numeric base used for conversion
 * @param length                number of characters to parse
 * @return                      parsed long value
 */
private fun rawStringToLong(text: pointer<char>, radix: int, length: int) -> long
{
    if !checkRadix(radix):
        return 0L

    var result: long = 0L

    for (var i = 0; i < length; i++)
    {
        val digit: int = radixCharToInt(text[i])

        if digit < 0 || digit >= radix:
            return 0L

        result = result * (radix as long) + (digit as long)
    }

    return result
}


/**
 * Converts a null-terminated integer string to a long.
 *
 * Decimal strings are parsed with radix 10. Strings prefixed by 0x or 0X are
 * parsed with radix 16. A leading + or - sign and a trailing L/l suffix are
 * accepted.
 *
 * This function returns 0L when the input cannot be parsed. Overflow is not
 * checked.
 *
 * @param text                  integer string to parse
 * @return                      parsed long value
 */
fun stringToLong(mut text: pointer<char>) -> long
{
    var length: int = String.strlen(text)

    if length <= 0:
        return 0L

    var sign: long = 1L

    if text[0] == '-':
    {
        sign = -1L
        text++
        length--
    }
    elif text[0] == '+':
    {
        text++
        length--
    }

    if length <= 0:
        return 0L

    if text[length - 1] == 'l' || text[length - 1] == 'L':
        length--

    if length <= 0:
        return 0L

    var radix: int = 10

    if length >= 2 && text[0] == '0' && (text[1] == 'x' || text[1] == 'X'):
    {
        radix = 16
        text+=2
        length -= 2
    }

    if length <= 0:
        return 0L

    return sign * rawStringToLong(text, radix, length)
}


/**
 * Converts an integer to a null-terminated string using the given radix.
 *
 * The result is written into the destination buffer.
 * The destination buffer must have enough capacity to store the sign,
 * all digits, and the final null terminator.
 *
 * Supported radices are 2 through 36. For invalid radices, this function
 * writes an empty string and returns.
 *
 * This function supports values in the range (INT_MIN, INT_MAX].
 * INT_MIN is not supported because negating INT_MIN overflows in two's
 * complement integer representation.
 *
 * @param dest                  pointer to the destination character buffer
 * @param value                 integer value to convert
 * @param radix                 numeric base used for conversion
 */
fun intToString(mut dest: pointer<char>, mut value: int, radix: int)
{
    if !checkRadix(radix):
    {
        dest[0] = '\0'
        return
    }

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
        val number: int = value % radix
        dest[offset] = intToRadixChar(number) as char
        offset++
        value /= radix
    }

    // then reverse [0 .. offset)
    FList.reverse(dest, offset, sizeof(char))
    dest[offset] = '\0'
}
