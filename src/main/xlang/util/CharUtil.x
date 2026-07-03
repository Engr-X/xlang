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


fun isDigit(ch: int) -> bool = ('0' as int) <= ch && ch <= ('9' as int)


fun isHexDigit(ch: int) -> bool =
    (('0' as int) <= ch && ch <= ('9' as int)) ||
    (('a' as int) <= ch && ch <= ('f' as int)) ||
    (('A' as int) <= ch && ch <= ('F' as int))


fun isSpace(ch: int) -> bool =
    ch == (' ' as int) || ch == ('\t' as int) || ch == ('\n' as int) || ch == ('\r' as int) || ch == ('\v' as int) || ch == ('\f' as int)
