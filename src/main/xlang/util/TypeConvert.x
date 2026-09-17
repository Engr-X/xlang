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

#file.outerClass("TypeConvert")
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
 * @param ch                ASCII digit character
 *
 * @return                  integer value represented by the digit
 */
inline fun charToInt(ch: char) -> int = (ch - '0') as int


/**
 * Converts a single decimal digit to its ASCII character.
 *
 * This function assumes that the input value is between 0 and 9.
 * No validation is performed.
 *
 * @param v                 decimal digit value
 *
 * @return                  ASCII digit character
 */
inline fun intToChar(v: int) -> char = v + '0'


/**
 * Checks whether a radix can be represented by decimal digits plus letters.
 *
 * Supported radices are 2 through 36.
 *
 * @param radix             numeric base to validate
 *
 * @return                  true when the radix is supported
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
 * @param v                 digit value to convert
 *
 * @return                  character representation of the digit
 */
private fun intToRadixChar(v: int) -> char
{
    if v < 10:
        return intToChar(v)

    return v - 10 + 'a'
}


/**
 * Converts a radix character to its corresponding integer value.
 *
 * <p>The following mappings are supported:
 * <ul>
 *     <li>{@code '0' - '9'} maps to {@code 0 - 9}</li>
 *     <li>{@code 'a' - 'z'} maps to {@code 10 - 35}</li>
 *     <li>{@code 'A' - 'Z'} maps to {@code 10 - 35}</li>
 * </ul>
 *
 * @param ch the radix character to convert
 * @return the corresponding integer value, or {@code -1} if the character is invalid
 */
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
 * @param dest              pointer to the destination character buffer
 * @param value             integer value to convert
 * @param radix             numeric base used for conversion
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


/**
 * Converts a null-terminated character string to a signed integer value.
 *
 * <p>The conversion follows the behavior of the underlying C standard library
 * integer conversion routine. Leading whitespace characters are ignored, and
 * an optional {@code '+'} or {@code '-'} sign may appear before the numeric
 * sequence. Conversion continues until the first character that cannot be
 * interpreted as part of a decimal integer is encountered.
 *
 * <p>For example:
 * <ul>
 *     <li>{@code "123"} produces {@code 123}</li>
 *     <li>{@code "-42"} produces {@code -42}</li>
 *     <li>{@code "  +17"} produces {@code 17}</li>
 *     <li>{@code "123abc"} produces {@code 123}</li>
 * </ul>
 *
 * <p>The input must point to a valid, null-terminated character sequence.
 * Passing a null pointer, an invalid pointer, or a non-null-terminated buffer
 * results in undefined native behavior.
 *
 * <p>If no valid numeric conversion can be performed, the result depends on
 * the underlying C library routine. Overflow and underflow behavior likewise
 * follows the semantics of the native implementation.
 *
 * @param text                  pointer to the null-terminated character string to convert
 * @return                      the converted signed integer value
 */
@Native("stringToInt")
native inline fun stringToInt(text: pointer<char>) -> int


/**
 * Converts a null-terminated character string to a signed long integer value.
 *
 * <p>The conversion follows the behavior of the underlying C standard library
 * integer conversion routine. Leading whitespace characters are ignored, and
 * an optional {@code '+'} or {@code '-'} sign may appear before the numeric
 * sequence. Conversion continues until the first character that cannot be
 * interpreted as part of a decimal integer is encountered.
 *
 * <p>For example:
 * <ul>
 *     <li>{@code "123456"} produces {@code 123456}</li>
 *     <li>{@code "-9000"} produces {@code -9000}</li>
 *     <li>{@code "  +64"} produces {@code 64}</li>
 *     <li>{@code "500xyz"} produces {@code 500}</li>
 * </ul>
 *
 * <p>The input must point to a valid, null-terminated character sequence.
 * Passing a null pointer, an invalid pointer, or a non-null-terminated buffer
 * results in undefined native behavior.
 *
 * <p>If no valid numeric conversion can be performed, the result depends on
 * the underlying C library routine. Overflow and underflow behavior likewise
 * follows the semantics of the native implementation.
 *
 * @param text                  pointer to the null-terminated character string to convert
 * @return                      the converted signed long integer value
 */
@Native("stringToLong")
native inline fun stringToLong(text: pointer<char>) -> long


/**
 * Converts a null-terminated character string to a single-precision
 * floating-point value.
 *
 * <p>The conversion follows the behavior of the underlying C standard library
 * floating-point conversion routine. Leading whitespace characters are ignored,
 * and an optional {@code '+'} or {@code '-'} sign may precede the numeric value.
 * Decimal fractions and exponent notation are supported when recognized by the
 * native C library. Conversion stops at the first character that is not part of
 * the floating-point representation.
 *
 * <p>For example:
 * <ul>
 *     <li>{@code "3.14"} produces approximately {@code 3.14f}</li>
 *     <li>{@code "-0.5"} produces {@code -0.5f}</li>
 *     <li>{@code "1.25e2"} produces approximately {@code 125.0f}</li>
 *     <li>{@code "3.14abc"} produces approximately {@code 3.14f}</li>
 * </ul>
 *
 * <p>The input must point to a valid, null-terminated character sequence.
 * Passing a null pointer, an invalid pointer, or a non-null-terminated buffer
 * results in undefined native behavior.
 *
 * <p>Special values such as infinity or NaN may be accepted when supported by
 * the underlying C implementation. Rounding, overflow, underflow, and invalid
 * conversion behavior follow the semantics of the native C library routine.
 *
 * @param text                  pointer to the null-terminated character string to convert
 * @return                      the converted single-precision floating-point value
 */
@Native("stringToFloat")
native inline fun stringToFloat(text: pointer<char>) -> float


/**
 * Converts a null-terminated character string to a double-precision
 * floating-point value.
 *
 * <p>The conversion follows the behavior of the underlying C standard library
 * floating-point conversion routine. Leading whitespace characters are ignored,
 * and an optional {@code '+'} or {@code '-'} sign may precede the numeric value.
 * Decimal fractions and exponent notation are supported when recognized by the
 * native C library. Conversion stops at the first character that is not part of
 * the floating-point representation.
 *
 * <p>For example:
 * <ul>
 *     <li>{@code "3.1415926535"} produces approximately {@code 3.1415926535}</li>
 *     <li>{@code "-0.001"} produces {@code -0.001}</li>
 *     <li>{@code "6.02e23"} produces approximately {@code 6.02e23}</li>
 *     <li>{@code "1.5xyz"} produces approximately {@code 1.5}</li>
 * </ul>
 *
 * <p>The input must point to a valid, null-terminated character sequence.
 * Passing a null pointer, an invalid pointer, or a non-null-terminated buffer
 * results in undefined native behavior.
 *
 * <p>Special values such as infinity or NaN may be accepted when supported by
 * the underlying C implementation. Rounding, overflow, underflow, and invalid
 * conversion behavior follow the semantics of the native C library routine.
 *
 * @param text                  pointer to the null-terminated character string to convert
 * @return                      the converted double-precision floating-point value
 */
@Native("stringToDouble")
native inline fun stringToDouble(text: pointer<char>) -> double
