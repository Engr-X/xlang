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

@file.class("CharUtil")
package xlang.util


/**
 * Returns whether ch is an ASCII decimal digit.
 *
 * The input is an integer character code. Only '0' through '9' are accepted.
 */
fun isDigit(ch: int) -> bool = ('0') <= ch && ch <= ('9')


/**
 * Returns whether ch is an ASCII hexadecimal digit.
 *
 * Accepted characters are decimal digits plus 'a' through 'f' and
 * 'A' through 'F'.
 */
fun isHexDigit(ch: int) -> bool =
    (('0') <= ch && ch <= ('9')) ||
    (('a') <= ch && ch <= ('f')) ||
    (('A') <= ch && ch <= ('F'))


/**
 * Returns whether ch is an ASCII whitespace character.
 *
 * This follows the C-style whitespace set: space, tab, line feed, carriage
 * return, vertical tab, and form feed.
 */
fun isSpace(ch: int) -> bool =
    ch == (' ') || ch == ('\t') || ch == ('\n') || ch == ('\r') || ch == ('\v') || ch == ('\f')
