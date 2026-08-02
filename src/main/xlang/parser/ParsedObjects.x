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

@file.class("ParsedObjects")
package xlang.parser

import xlang.lexer.PatternList
import xlang.lexer.TokenList
import xlang.util.ArrayList


/**
 * Repeatedly parses objects using one shared ParsedObject parser.
 *
 * ParsedObjects applies the same parser-rule set multiple times to the
 * beginning of an input TokenList. Each successful iteration consumes one
 * matching token prefix and constructs one result object.
 *
 * Parsing continues until the input becomes empty or the internal parser
 * cannot consume a positive number of tokens.
 *
 * A failed single-object parse is treated as the normal end of the repeated
 * sequence. Therefore, this structure implements zero-or-more parsing rather
 * than requiring at least one successful match.
 *
 * Produced objects are stored as pointer<*> values. Their concrete types and
 * ownership rules are determined by the supplied result constructor.
 *
 * PatternList rules and result objects are referenced by pointer and are not
 * deeply copied.
 */
struct ParsedObjects
{
    /**
     * Stores result objects produced by the most recent parse operation.
     *
     * Each ArrayList element contains one pointer<*> value returned by the
     * internal ParsedObject result constructor.
     *
     * The pointed objects are not copied by ParsedObjects.
     */
    private var results: pointer<ArrayList>

    /**
     * Stores the parser used to consume one object per iteration.
     *
     * All pattern rules and the result constructor are delegated to this
     * ParsedObject.
     */
    private var parser: pointer<ParsedObject>


    /**
     * Initializes an empty repeated-object parser.
     *
     * A new result list and internal ParsedObject are allocated. The supplied
     * result constructor is stored by the internal parser and is invoked after
     * each successfully consumed token prefix.
     *
     * The result constructor may create an expression, statement, declaration
     * or any other parser-owned object.
     *
     * The caller must provide a valid result constructor and keep it callable
     * while this ParsedObjects instance is used.
     *
     * @warning                     A null or invalid result constructor causes undefined behavior
     *                              when getResult is invoked by the internal parser.
     */
    fun __init__(singleParser: pointer<ParsedObject>):
        this.parser = singleParser


    /**
     * Appends a token-pattern rule to the internal parser.
     *
     * Rules are tested in insertion order by ParsedObject. The first complete
     * rule match determines the token prefix consumed during an iteration.
     *
     * The PatternList pointer is stored directly. The rule and its contained
     * pattern atoms are not copied.
     *
     * The caller must keep a non-null rule valid while this parser uses it.
     *
     * @param                   rule the token-pattern rule to append.
     * @return                  this ParsedObjects instance for chained calls.
     *
     * @note                    A null rule may be stored, but ParsedObject ignores null rule
     *                          entries while matching.
     */
    fun addRule(rule: pointer<PatternList>) -> pointer<ParsedObjects>
    {
        this.parser.addRule(rule)
        return this
    }


    /**
     * Returns the number of rule entries stored by the internal parser.
     *
     * The count includes null rule entries because it reflects the size of
     * the internal rule list.
     *
     * @return                  the number of stored parser-rule entries.
     */
    private inline fun ruleLength() -> int = this.parser.ruleLength()


     /**
     * Appends one parser result to the internal result list.
     *
     * The result pointer is copied into an ArrayList element. The object
     * referenced by the pointer is not copied.
     *
     * A null result may be stored if the result constructor returns null.
     *
     * The caller or result constructor remains responsible for the lifetime
     * and ownership of the pointed result object.
     *
     * @param result            the parser result pointer to append.
     * @return                  this ParsedObjects instance for chained calls.
     */
    private fun pushResult(result: pointer<*>) -> pointer<ParsedObjects>
    {
        val resultSpace: blob[sizeof(pointer<*>)]
        val resultSlot: pointer<pointer<*>> = resultSpace as pointer<pointer<*>>

        resultSlot.deref = result
        this.results.push(resultSlot)

        return this
    }


    /**
     * Repeatedly parses result objects from the beginning of an input list.
     *
     * The existing result list is discarded and replaced with an empty list
     * before parsing begins.
     *
     * During each iteration, the internal ParsedObject attempts to consume one
     * matching token prefix. When a positive number of tokens is consumed,
     * the corresponding result object is constructed and appended to results.
     *
     * Successfully consumed tokens are removed from input by
     * ParsedObject.doParse.
     *
     * Parsing stops when:
     *
     * - the input TokenList becomes empty;
     * - the internal parser returns zero; or
     * - the internal parser returns a negative value.
     *
     * A zero or negative result is treated as the normal end of the repeated
     * sequence. Any diagnostic stored by the internal ParsedObject is not
     * returned or exposed by this function.
     *
     * The caller must provide a valid, mutable TokenList and manage the
     * lifetime of result objects created by the result constructor.
     *
     * @param input             the source token list to parse and consume.
     *
     * @return                  the total number of tokens consumed by successful iterations.
     *
     * @note                    Returning zero may mean that the input was empty or that no rule
     *                          matched the first token.
     *
     * @note                    This function implements zero-or-more parsing because failure
     *                          before the first result is treated as a valid empty sequence.
     *
     * @warning                 Passing a null or invalid input pointer causes undefined
     *                          behavior because input.length is accessed immediately.
     */
    fun doParse(input: pointer<TokenList>) -> int
    {
        var consumed: int = 0
        this.results = new ArrayList(sizeof(pointer<*>))

        while input.length() > 0:
        {
            val index: int = this.parser.doParse(input)

            if index <= 0:
                break

            this.pushResult(this.parser.getResult())
            consumed += index
        }

        return consumed
    }


    /**
     * Returns the number of parsed result objects.
     *
     * The count describes the results produced by the most recent call to
     * doParse. It is reset to zero when doParse replaces the result list.
     *
     * @return                    the number of stored parser-result pointers.
     */
    fun length() -> int = this.results.length


    /**
     * Returns the internal parser-result list.
     *
     * The returned pointer refers to the same mutable ArrayList stored by this
     * ParsedObjects instance. Neither the list nor its result objects are
     * copied.
     *
     * Changes made through the returned ArrayList directly affect this
     * ParsedObjects instance.
     *
     * A later call to doParse replaces the internal result list. A pointer
     * returned before that call continues to refer to the old list rather than
     * the newly produced results.
     *
     * The caller must not release the returned ArrayList while this object is
     * still using it.
     *
     * @return                  the internal mutable ArrayList containing pointer<*> result
     *                          slots.
     *
     * @note                    Each element is a pointer slot, not the parser result object
     *                          stored directly.
     *
     * @warning                 Holding the returned pointer across a later doParse call may
     *                          leave the caller referring to an outdated result list.
     */
    fun getResult() -> pointer<ArrayList> = this.results
}
