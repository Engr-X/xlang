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

@file.class("Operation")
package xlang

import xlang.util.string.String


/**
 * Describes an operator supported by the expression parser.
 *
 * An operation definition contains a unique identifier, textual symbol,
 * lowering function name, fixity, associativity and precedence priority.
 *
 * The parser may use these properties to determine where the operator
 * appears and how an expression containing multiple operators is grouped.
 *
 * The operator symbol is copied when the object is created.
 */
struct Operation
{
    /**
     * Identifies an operator that appears before its operand.
     *
     * Examples include unary negation and logical negation.
     */
    static val PREFIX_TYPE: int = 0

    /**
     * Identifies an operator that appears between two operands.
     *
     * Examples include addition, multiplication and assignment.
     */
    static val INFIX_TYPE: int = 1

    /**
     * Identifies an operator that appears after its operand.
     *
     * Examples may include postfix increment or other postfix syntax.
     */
    static val POSTFIX_TYPE: int = 2

    /**
     * Identifies a left-associative operator.
     *
     * Repeated operators with the same priority are grouped from left
     * to right.
     *
     * For example, `a - b - c` is interpreted as `(a - b) - c`.
     */
    static val LEFT_ASSOC: int = 0

    /**
     * Identifies a right-associative operator.
     *
     * Repeated operators with the same priority are grouped from right
     * to left.
     *
     * For example, `a = b = c` is interpreted as `a = (b = c)`.
     */
    static val RIGHT_ASSOC: int = 1

    /**
     * Identifies a non-associative operator.
     *
     * Repeating the operator at the same priority normally requires
     * explicit grouping or should be rejected by the parser.
     */
    static val NO_ASSOC: int = 2


    /**
     * Stores the unique identifier of this operator.
     *
     * The parser may use this value to distinguish operators without
     * comparing their textual symbols.
     */
    val id: int

    /**
     * Points to the null-terminated textual symbol of this operator.
     *
     * The constructor creates an independent clone of the supplied string.
     */
    private val symbol: pointer<char>

    /**
     * Points to the null-terminated function name used when lowering this
     * operator into a call-like form.
     *
     * For example, the `+` operator may use `plus`, so `a + b` can later be
     * represented as a call equivalent to `a.plus(b)`.
     *
     * The value may be null when the operator has not been assigned a lowering
     * function name or must be handled by special semantic logic.
     */
    private val functionName: pointer<char>

    /**
     * Stores the position of the operator relative to its operands.
     *
     * This value is normally PREFIX_TYPE, INFIX_TYPE or POSTFIX_TYPE.
     */
    val fixity: int

    /**
     * Stores how operators of the same priority are grouped.
     *
     * This value is normally LEFT_ASSOC, RIGHT_ASSOC or NO_ASSOC.
     */
    val associativity: int

    /**
     * Stores the precedence priority of this operator.
     *
     * The parser uses this value to determine which operators bind more
     * tightly when parentheses are not present.
     *
     * The meaning of larger and smaller values depends on the convention
     * used by the expression parser.
     */
    val priority: int


    /**
     * Initializes an operation definition with a lowering function name.
     *
     * Both the operator symbol and function name are duplicated with
     * String.strdup. All other values are stored directly without validation
     * or normalization.
     *
     * The function name is intended for semantic or lowering stages that map
     * operator syntax onto call-like forms.
     *
     * @param id                the unique operator identifier.
     * @param symbol            the null-terminated operator symbol.
     * @param fixity            the operator position relative to its operands.
     * @param associativity     the grouping direction for equal-priority operators.
     * @param priority          the precedence priority of the operator.
     * @param functionName      the null-terminated lowering function name.
     *
     * @note                    The created Operation owns independent copies of
     *                          symbol and functionName.
     * @warning                 Passing an invalid symbol or functionName pointer
     *                          may cause undefined behavior in String.strdup.
     */
    fun __init__(
        id: int,
        symbol: pointer<char>,
        fixity: int,
        associativity: int,
        priority: int,
        functionName: pointer<char>)
    {
        this.id = id
        this.symbol = String.strdup(symbol)
        this.functionName = String.strdup(functionName)
        this.fixity = fixity
        this.priority = priority
        this.associativity = associativity
    }


    /**
     * Returns a clone of the textual operator symbol.
     *
     * @return                  copied null-terminated operator symbol
     */
    fun getSymbol() -> pointer<char> = String.strdup(this.symbol)


    /**
     * Returns a clone of the lowering function name for this operator.
     *
     * @return                  copied null-terminated function name, or null when absent
     */
    fun getFunctionName() -> pointer<char> = String.strdup(this.functionName)
}
