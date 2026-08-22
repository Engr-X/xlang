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

@file.class("Rule")
package xlang.parser.util

import xlang.Operation
import xlang.util.ArrayList


/**
 * Describes one parser rule and its matching priority.
 *
 * The pattern is referenced directly and is not copied. A larger or smaller
 * priority has no predefined meaning until a parser chooses a precedence
 * convention.
 */
struct Rule
{
    static val STARTER_ROLE: int = 0

    static val CONTINUATION_ROLE: int = 1


    private val pattern: pointer<PatternList>

    private var resultConstructor: (pointer<ArrayList>) -> pointer<*>

    private var operation: pointer<Operation>

    var priority: int

    var role: int


    fun __init__(pattern: pointer<PatternList>, resultConstructor: (pointer<ArrayList>) -> pointer<*>, role: int, operation: pointer<Operation>)
    {
        this.pattern = pattern
        this.resultConstructor = resultConstructor
        this.operation = operation
        this.priority = operation.priority
        this.role = role
    }


    fun __init__(pattern: pointer<PatternList>, resultConstructor: (pointer<ArrayList>) -> pointer<*>, role: int, priority: int)
    {
        this.pattern = pattern
        this.resultConstructor = resultConstructor
        this.operation = null
        this.priority = priority
        this.role = role
    }


    fun constructResult(results: pointer<ArrayList>) -> pointer<*>
        = this.resultConstructor(results)



    fun getAssociativity() -> int = if this.operation == null:
            Operation.LEFT_ASSOC
        else:
            this.operation.associativity


    fun getFixity() -> int = if this.operation == null:
            Operation.INFIX_TYPE
        else:
            this.operation.fixity


    fun getPattern() -> pointer<PatternList> = this.pattern
}
