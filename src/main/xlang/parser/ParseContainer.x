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
 */

package xlang.parser


struct ParseContainer
{
    static val ARRAY_LIST_KIND: int = 0


    private var kind: int
    private var value: pointer<*>


    constructor(kind: int, value: pointer<*>)
    {
        this.kind = kind
        this.value = value
    }


    fun getKind() -> int = this.kind


    fun getValue() -> pointer<*> = this.value


    fun isKind(kind: int) -> bool = this.kind == kind
}
