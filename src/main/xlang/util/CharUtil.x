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


fun isDigit(ch: int) -> bool = ('0') <= ch && ch <= ('9')


fun isHexDigit(ch: int) -> bool =
    (('0') <= ch && ch <= ('9')) ||
    (('a') <= ch && ch <= ('f')) ||
    (('A') <= ch && ch <= ('F'))


fun isSpace(ch: int) -> bool =
    ch == (' ') || ch == ('\t') || ch == ('\n') || ch == ('\r') || ch == ('\v') || ch == ('\f')
