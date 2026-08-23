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
 */
@file.class("Statement")
package xlang.compiler.parser.statement


struct Statement
{
    private var kind: int

    private var root: pointer<*>

    private var extraTokens: pointer<ArrayList>
}
