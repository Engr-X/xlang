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

@file.class("String")
package xlang.util.string

/**
 * Provides basic utilities for C-style null-terminated strings.
 *
 * This module treats a string as a sequence of characters ended by '\0'.
 * Null input is handled defensively: read-only helpers return a stable empty
 * or ordering result, write helpers do nothing when source or destination is
 * null, and strdup returns null.
 *
 * Most functions in this module do not allocate memory.
 * Functions that write to a destination buffer require the caller to ensure
 * that the destination has enough capacity.
 * strdup allocates a new string buffer and returns ownership to the caller.
 *
 * Utilities included:
 * - strlen: returns the length of a null-terminated string
 * - streq: compares two null-terminated strings
 * - strcpy: copies a null-terminated string into a destination buffer
 * - strdup: duplicates a null-terminated string into newly allocated memory
 * - strncpySafe: copies at most n characters and always null-terminates
 * - strcat: appends one null-terminated string to another
 * - substring: copies a substring into a destination buffer
 */
import xlang.System


/**
 * Null terminator used by C-style strings.
 *
 * A string is considered ended when this character is reached.
 */
val NULL_CHAR: int = 0


/**
 * Line feed character.
 *
 * This is the newline character used by text utilities.
 */
val LINE_FEED: int = 10


/**
 * Returns the length of a null-terminated string.
 *
 * The returned length does not include the null terminator.
 *
 * @param str pointer to the first character of a null-terminated string
 * @return number of characters before the first null terminator
 */
fun strlen(str: pointer<char>) -> int
{
    if str == null:
        return 0

    var count: int = 0

    for (var ptr: pointer<char> = str; ptr.deref != '\0'; ptr++, count++);

    return count
}


/**
 * Compares two null-terminated strings for equality.
 *
 * Two strings are equal if they contain the same characters in the same order
 * and end at the same position.
 *
 * @param str1                  pointer to the first null-terminated string
 * @param str2                  pointer to the second null-terminated string
 * @return true if both strings are equal, false otherwise
 */
fun streq(str1: pointer<char>, str2: pointer<char>) -> bool
{
    if str1 == null || str2 == null:
        return str1 == str2

    var ptr1: pointer<char> = str1
    var ptr2: pointer<char> = str2

    for (;
        ptr1.deref != '\0' && ptr2.deref != '\0';
        ptr1++, ptr2++)
    {
        if ptr1.deref != ptr2.deref:
            return false
    }

    return ptr1.deref == ptr2.deref
}


/**
 * Compares two null-terminated strings lexicographically.
 *
 * The return value follows the usual strcmp ordering contract, but is
 * normalized to -1, 0, or 1 instead of returning the raw character difference.
 * Null is treated as smaller than any non-null string, and two null pointers
 * compare equal.
 *
 * @param str1                  pointer to the first null-terminated string
 * @param str2                  pointer to the second null-terminated string
 * @return                      -1 if str1 < str2, 0 if equal, 1 if str1 > str2
 */
fun strcmp(str1: pointer<char>, str2: pointer<char>) -> int
{
    if str1 == str2:
        return 0

    if str1 == null:
        return -1

    if str2 == null:
        return 1

    var ptr1: pointer<char> = str1
    var ptr2: pointer<char> = str2

    for (;
        ptr1.deref != '\0' && ptr2.deref != '\0';
        ptr1++, ptr2++)
    {
        val ch1: int = ptr1.deref as int
        val ch2: int = ptr2.deref as int

        if ch1 < ch2:
            return -1

        if ch1 > ch2:
            return 1
    }

    if ptr1.deref == ptr2.deref:
        return 0

    if ptr1.deref == '\0':
        return -1

    return 1
}


/**
 * Copies a null-terminated string into a destination buffer.
 *
 * The null terminator is copied as well.
 * The destination buffer must have enough capacity to store the whole source
 * string and its null terminator.
 *
 * @param dest                  pointer to the destination buffer
 * @param src                   pointer to the source null-terminated string
 */
fun strcpy(dest: pointer<char>, src: pointer<char>)
{
    if dest == null || src == null:
        return

    var destPtr: pointer<char> = dest
    var srcPtr: pointer<char> = src

    for (;srcPtr.deref != '\0'; destPtr++, srcPtr++):
        destPtr.deref = srcPtr.deref

    destPtr.deref = '\0'
}


/**
 * Duplicates a null-terminated string into newly allocated memory.
 *
 * The returned buffer contains the source string and its null terminator.
 * The caller owns the returned buffer.
 *
 * @param src                   pointer to the source null-terminated string
 * @return                      pointer to the duplicated null-terminated string
 */
fun strdup(src: pointer<char>) -> pointer<char>
{
    if src == null:
        return null

    val textLength: int = strlen(src)
    val dest: pointer<char> = System.allocMemory((textLength + 1) * sizeof(char)) as pointer<char>

    strcpy(dest, src)

    return dest
}


/**
 * Copies at most maxCopyLength characters into a destination buffer safely.
 *
 * The copy stops when either the requested length is reached or the source
 * null terminator is encountered.
 *
 * The destination is always null-terminated.
 * The caller must ensure that the destination buffer can hold at least
 * maxCopyLength characters plus the final null terminator.
 *
 * @param                       dest pointer to the destination buffer
 * @param                       src pointer to the source null-terminated string
 * @param                       maxCopyLength maximum number of source characters to copy
 * @return                      number of characters copied, excluding the null terminator
 */
fun strncpy(dest: pointer<char>, src: pointer<char>, maxCopyLength: int) -> int
{
    if dest == null:
        return 0

    if src == null || maxCopyLength <= 0:
    {
        dest[0] = '\0'
        return 0
    }

    var copiedLength: int = 0

    while copiedLength < maxCopyLength && src[copiedLength] != '\0':
    {
        dest[copiedLength] = src[copiedLength]
        copiedLength++
    }

    dest[copiedLength] = '\0'

    return copiedLength
}


/**
 * Appends a null-terminated string to the end of another string.
 *
 * The source string is copied after the current end of the destination string.
 * The destination buffer must have enough capacity to store the original
 * destination content, the source string, and the final null terminator.
 *
 * @param dest                  pointer to the destination null-terminated string buffer
 * @param src                   pointer to the source null-terminated string
 */
fun strcat(dest: pointer<char>, src: pointer<char>)
{
    if dest == null || src == null:
        return

    val destLength: int = strlen(dest)
    strcpy(dest + destLength, src)
}


/**
 * Copies a substring into a destination buffer safely.
 *
 * The substring starts at start and spans at most length characters.
 * If start is outside the source string, or length is not positive, the
 * destination becomes an empty string.
 *
 * @param                       dest pointer to the destination buffer
 * @param                       src pointer to the source null-terminated string
 * @param                       start zero-based start index in the source string
 * @param                       length maximum number of characters to copy
 * @return                      number of characters copied, excluding the null terminator
 */
fun substring(dest: pointer<char>, src: pointer<char>, start: int, length: int) -> int
{
    if dest == null:
        return 0

    if src == null || start < 0 || length <= 0:
    {
        dest[0] = '\0'
        return 0
    }

    val srcLength: int = strlen(src)

    if start >= srcLength:
    {
        dest[0] = '\0'
        return 0
    }

    return strncpy(dest, src + start, length)
}


/**
 * Matches a regular expression pattern against the beginning of a string.
 *
 * This is a native wrapper around the tiny-regex based matcher. The match must
 * start at str[0]; a pattern that only matches later in the string is treated
 * as no match. The returned value is the number of characters matched.
 *
 * The native wrapper also handles tokenizer-specific helpers such as top-level
 * pattern alternatives and the standalone EOF pattern "\\0".
 *
 * @param pattern               pointer to the null-terminated regex pattern
 * @param str                   pointer to the null-terminated input string
 * @return                      matched length, or -1 if the pattern does not match
 */
@native("regex_match")
native inline fun strRegMatch(pattern: pointer<char>, str: pointer<char>) -> int

