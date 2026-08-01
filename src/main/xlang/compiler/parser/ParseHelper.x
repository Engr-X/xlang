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


import xlang.parser.ParserUtil


fun unescapeChar(string: pointer<char>) -> char =
    ParserUtil.unescapeChar(string)
