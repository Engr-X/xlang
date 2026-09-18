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

package xlang.parser.util

import xlang.Operation
import xlang.util.ArrayList


/**
 * Describes a parser rule, including its matching pattern, result constructor,
 * parser role, priority, and optional operator metadata.
 *
 * <p>The rule pattern is referenced directly and is not copied. When an
 * {@code Operation} is associated with the rule, its priority, associativity,
 * and fixity are used as the operator properties of this rule.
 *
 * <p>Rules may act either as starter rules or continuation rules depending on
 * the value of {@code role}.
 */
struct Rule
{
    /**
     * Default post-processing function used when no custom function is supplied.
     *
     * <p>This implementation performs no transformation and simply returns the
     * supplied token list unchanged.
     *
     * @param input             a pointer to the token list
     * @param index             the current token index
     *
     * @return                  the original token list
     */
    private static fun identAfter(input: pointer<TokenList>, index: int) -> pointer<TokenList> = input


    // Identifies a rule that may begin a parse expression.
    static val STARTER_ROLE: int = 0


    // Identifies a rule that continues an already parsed expression.
    static val CONTINUATION_ROLE: int = 1


    /**
     * The token pattern matched by this rule.
     *
     * <p>The pattern is stored by reference and is not copied.
     */
    private val pattern: pointer<PatternList>


    /**
     * The function used to construct the semantic result of this rule.
     *
     * <p>The function receives the list of parsed pattern results and returns
     * the resulting object produced by this rule.
     */
    private var resultConstructor: (pointer<ArrayList>) -> pointer<*>


    /**
     * The function invoked after the rule has been processed.
     *
     * <p>The function receives the current token list and token index and may
     * return a modified or replacement token list.
     *
     * <p>By default, {@code identAfter} is used and the input token list is
     * returned unchanged.
     */
    private var afterFun: (pointer<TokenList>, int) -> pointer<TokenList>


    /**
     * The optional operator metadata associated with this rule.
     *
     * <p>If this value is {@code null}, default associativity and fixity values
     * are used by {@code getAssociativity()} and {@code getFixity()}.
     */
    private var operation: pointer<Operation>


    /**
     * The parsing priority of this rule.
     *
     * <p>When the rule is created from an {@code Operation}, this value is
     * initialized from the operation priority.
     */
    var priority: int


    /**
     * The role of this rule within the parser.
     *
     * <p>The value is typically either {@code STARTER_ROLE} or
     * {@code CONTINUATION_ROLE}.
     */
    var role: int


    /**
     * Creates a parser rule associated with an operator definition.
     *
     * <p>The rule priority is initialized from {@code operation.priority}.
     * Operator associativity and fixity are also obtained from the supplied
     * operation.
     *
     * <p>The default post-processing function is {@code identAfter}.
     *
     * @param pattern           a pointer to the token pattern matched by this rule
     * @param resultConstructor the function used to construct the rule result
     * @param role              the parser role of this rule
     * @param operation         a pointer to the operator metadata associated with this rule
     */
    constructor(pattern: pointer<PatternList>, resultConstructor: (pointer<ArrayList>) -> pointer<*>, role: int, operation: pointer<Operation>)
    {
        this.pattern = pattern
        this.resultConstructor = resultConstructor
        this.operation = operation
        this.priority = operation.priority
        this.role = role
        this.afterFun = identAfter
    }


    /**
     * Creates a parser rule with an explicit parsing priority.
     *
     * <p>No operator metadata is associated with the rule. As a result,
     * default associativity and fixity values are returned when requested.
     *
     * <p>The default post-processing function is {@code identAfter}.
     *
     * @param pattern           a pointer to the token pattern matched by this rule
     * @param resultConstructor the function used to construct the rule result
     * @param role              the parser role of this rule
     * @param priority          the parsing priority assigned to this rule
     */
    constructor(pattern: pointer<PatternList>, resultConstructor: (pointer<ArrayList>) -> pointer<*>, role: int, priority: int)
    {
        this.pattern = pattern
        this.resultConstructor = resultConstructor
        this.operation = null
        this.priority = priority
        this.role = role
        this.afterFun = identAfter
    }


    /**
     * Sets the post-processing function invoked after this rule is processed.
     *
     * @param funPtr            the function to invoke after processing the rule
     *
     * @return                  this {@code Rule} instance
     */
    fun setAfterFun(funPtr: (pointer<TokenList>, int) -> pointer<TokenList>) -> pointer<Rule>
    {
        this.afterFun = funPtr
        return this
    }


    /**
     * Constructs the semantic result of this rule.
     *
     * <p>The supplied parsed results are passed directly to the configured
     * result constructor.
     *
     * @param results a pointer to the list of parsed pattern results
     *
     * @return                  the object produced by the result constructor
     */
    fun constructResult(results: pointer<ArrayList>) -> pointer<*>
        = this.resultConstructor(results)


    /**
     * Returns the associativity associated with this rule.
     *
     * <p>If no {@code Operation} is associated with the rule,
     * {@code Operation.LEFT_ASSOC} is returned.
     *
     * @return                  the associativity of this rule
     */
    fun getAssociativity() -> int = if this.operation == null:
            Operation.LEFT_ASSOC
        else:
            this.operation.associativity


    /**
     * Returns the fixity associated with this rule.
     *
     * <p>If no {@code Operation} is associated with the rule,
     * {@code Operation.INFIX_TYPE} is returned.
     *
     * @return                  the fixity of this rule
     */
    fun getFixity() -> int = if this.operation == null:
            Operation.INFIX_TYPE
        else:
            this.operation.fixity


    /**
     * Returns the token pattern matched by this rule.
     *
     * @return                  a pointer to the rule pattern
     */
    fun getPattern() -> pointer<PatternList> = this.pattern


    /**
     * Checks whether this rule contains an empty pattern.
     *
     * @return                  {@code true} if the pattern contains no elements;
     *                          {@code false} otherwise
     */
    fun isEmpty() -> bool = this.pattern.length() == 0
}
