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

@file.class("ParsedObject")
package xlang.parser

import xlang.Diagnostic
import xlang.SourceLocation
import xlang.lexer.PatternList
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.util.ArrayList


/**
 * Stores the result and intermediate state of one parser operation.
 *
 * A ParsedObject contains an ordered set of token-pattern rules. During
 * parsing, those rules are tested against the beginning of an input TokenList.
 * The first complete match determines the token prefix consumed by this
 * object.
 *
 * If no rule completely matches, the longest partial match is used to locate
 * the first unexpected token and produce a diagnostic.
 *
 * The consumed TokenList, parser rules and diagnostic objects are stored by
 * pointer. Token and PatternList objects are not deeply copied by this
 * structure.
 *
 * The caller must keep referenced rules and callback functions valid while
 * this ParsedObject uses them.
 */
struct ParsedObject
{
    /**
     * Stores the tokens consumed by this parsed object.
     *
     * The list is initially empty and is replaced by the matched token prefix
     * after a successful call to doParse.
     */
    private var tokens: pointer<TokenList>

    /**
     * Stores the diagnostic produced by the most recent failed parse.
     *
     * A null value means that no diagnostic has been recorded. A successful
     * parse does not explicitly clear an error stored by an earlier call.
     */
    private var errorResult: pointer<Diagnostic>

    /**
     * Stores the function that converts consumed tokens into a parser result.
     *
     * The callback receives the consumed TokenList and returns a pointer to
     * an arbitrary parser-owned object, such as an expression, statement or
     * declaration node.
     *
     * This callback is stored directly and is not invoked by doParse in the
     * current implementation.
     */
    var resultConstructor: (pointer<TokenList>) -> pointer<*>

    /**
     * Stores the token-pattern rules tested by eat.
     *
     * The ArrayList contains pointer<PatternList> values in insertion order.
     * The PatternList objects themselves are referenced directly and are not
     * copied.
     */
    private var rules: pointer<ArrayList>


    /**
     * Initializes an empty parsed object with a result callback.
     *
     * A new empty TokenList and rule list are allocated. No diagnostic is
     * initially stored.
     *
     * The callback pointer is stored directly without validation.
     *
     * The caller must provide a valid callback and keep it available while
     * this ParsedObject may invoke it.
     *
     * @param                   result the function that converts consumed tokens into a parser
     *                          result object.
     *
     * @warning                 A null or invalid callback causes undefined behavior if it is
     *                          invoked.
     */
    fun __init__(resultConstructor: (pointer<TokenList>) -> pointer<*>)
    {
        this.tokens = new TokenList()
        this.errorResult = null
        this.resultConstructor = resultConstructor
        this.rules = new ArrayList(sizeof(pointer<PatternList>))
    }


    /**
     * Appends a token to this object's consumed-token list.
     *
     * The token pointer is passed directly to TokenList.push. The Token object
     * itself is not duplicated by this function.
     *
     * The caller must provide a valid token pointer and keep the referenced
     * token alive while the TokenList uses it.
     *
     * @param                   token the token to append.
     * @return                  this ParsedObject for chained calls.
     *
     * @note                    This helper is not used by the current parsing path.
     */
    private fun pushToken(token: pointer<Token>) -> pointer<ParsedObject>
    {
        this.tokens.push(token)
        return this
    }


    /**
     * Appends a token-pattern rule.
     *
     * The PatternList pointer is copied into the internal ArrayList. The
     * PatternList object and its contained pattern atoms are not duplicated.
     *
     * Rules are later tested in the same order in which they are added.
     *
     * A null rule pointer may be stored, but eat will ignore that entry.
     *
     * The caller must keep a non-null PatternList valid while this
     * ParsedObject uses it.
     *
     * @param rule              the token-pattern rule to append.
     * @return this             ParsedObject for chained calls.
     */
    fun addRule(rule: pointer<PatternList>) -> pointer<ParsedObject>
    {
        val ruleSpace: blob[sizeof(pointer<PatternList>)]
        val ruleSlot: pointer<pointer<PatternList>> = ruleSpace as pointer<pointer<PatternList>>

        ruleSlot.deref = rule
        this.rules.push(ruleSlot)

        return this
    }


    /**
     * Returns the number of stored parser-rule entries.
     *
     * Null rule entries are included in the returned count.
     *
     * @return                  the number of entries in the internal rule list.
     */
    fun ruleLength() -> int = this.rules.length


    /**
     * Matches and extracts one token prefix from the input.
     *
     * Rules are tested in insertion order. For each valid rule,
     * input.maxMatchLength determines how many leading tokens match the
     * rule's pattern sequence.
     *
     * When a rule completely matches, the matching prefix is immediately
     * returned through input.subToken. Later rules are not examined.
     *
     * If no rule completely matches, the greatest partial-match length is
     * used as the index of the unexpected token. A diagnostic is stored in
     * errorResult and null is returned.
     *
     * A null input or an empty input is treated as an internal parser error.
     * Null rule entries are ignored.
     *
     * This function does not remove tokens from the input. Removal is
     * performed later by doParse.
     *
     * The exact ownership and copy behavior of the returned TokenList depend
     * on the implementation of TokenList.subToken.
     *
     * @param input             the source token list whose prefix should be matched.

     * @return                  the matched token prefix, or null if parsing fails.
     *
     * @note                    The first complete rule match wins, even if a later rule could
     *                          match a longer prefix.
     *
     * @warning                 When an unexpected token is available, that token and its
     *                          TokenPosition must be valid and non-null.
     */
    private fun eat(input: pointer<TokenList>) -> pointer<TokenList>
    {
        if input == null:
        {
            this.errorResult = Diagnostic.makeInternalError(
                Diagnostic.NULL_INPUT,
                new ArrayList(sizeof(SourceLocation)),
                Diagnostic.NULL_INPUT_MSG)
            return null
        }

        if input.length() <= 0:
        {
            this.errorResult = Diagnostic.makeInternalError(
                Diagnostic.EMPTY_INPUT,
                new ArrayList(sizeof(SourceLocation)),
                Diagnostic.EMPTY_INPUT_MSG)
            return null
        }

        var maxEat: int = 0

        for (var i = 0; i < this.rules.length; i++):
        {
            val ruleSlot: pointer<pointer<PatternList>> = this.rules.get(i) as pointer<pointer<PatternList>>
            val rule: pointer<PatternList> = ruleSlot.deref
            val eaten: int = input.maxMatchLength(rule)

            if eaten == rule.length():
                return input.subToken(0, rule.length())

            if eaten > maxEat:
                maxEat = eaten
        }

        if maxEat >= input.length():
        {
            this.errorResult = Diagnostic.makeError(
                Diagnostic.UNEXPECTED_TOKEN,
                new ArrayList(sizeof(SourceLocation)),
                Diagnostic.UNEXPECTED_TOKEN_MSG)
            return null
        }

        val token: pointer<Token> = input.get(maxEat)
        val location: pointer<SourceLocation> = new SourceLocation(
            input.filePath,
            token.pos.offset,
            token.pos.line,
            token.pos.column,
            token.pos.length)
        val locations: pointer<ArrayList> = new ArrayList(sizeof(SourceLocation))
        locations.push(location)

        this.errorResult = Diagnostic.makeError(
            Diagnostic.UNEXPECTED_TOKEN,
            locations,
            Diagnostic.UNEXPECTED_TOKEN_MSG)

        return null
    }


    /**
     * Matches and extracts one token prefix from the input.
     *
     * Rules are tested in insertion order. For each valid rule,
     * input.maxMatchLength determines how many leading tokens match the
     * rule's pattern sequence.
     *
     * When a rule completely matches, the matching prefix is immediately
     * returned through input.subToken. Later rules are not examined.
     *
     * If no rule completely matches, the greatest partial-match length is
     * used as the index of the unexpected token. A diagnostic is stored in
     * errorResult and null is returned.
     *
     * A null input or an empty input is treated as an internal parser error.
     * Null rule entries are ignored.
     *
     * This function does not remove tokens from the input. Removal is
     * performed later by doParse.
     *
     * The exact ownership and copy behavior of the returned TokenList depend
     * on the implementation of TokenList.subToken.
     *
     * @param input             the source token list whose prefix should be matched.
     *
     * @return                  the matched token prefix, or null if parsing fails.
     *
     * @note                    The first complete rule match wins, even if a later rule could
     *                          match a longer prefix.
     *
     * @warning                 When an unexpected token is available, that token and its
     *                          TokenPosition must be valid and non-null.
     */
    fun doParse(input: pointer<TokenList>) -> int
    {
        val tokens: pointer<TokenList> = this.eat(input)

        if tokens == null:
            return -1

        val index: int = tokens.length()
        this.tokens = tokens
        input.remove(0, index)

        return index
    }


    /**
     * Returns the number of tokens consumed by this parsed object.
     *
     * Before a successful parse, this normally returns zero because the
     * object begins with an empty TokenList.
     *
     * @return                  the number of tokens in the stored parsed prefix.
     */
    fun length() -> int = this.tokens.length()


    /**
     * Returns the diagnostic produced by the most recent parse operation.
     *
     * A null result means that no parsing error is currently recorded.
     * The returned pointer refers to the same Diagnostic object stored by this
     * ParsedObject and is not copied.
     *
     * A successful parse must explicitly clear errorResult if diagnostics from
     * earlier failed parse attempts should no longer be visible.
     *
     * The caller must not release or modify the returned Diagnostic unless it
     * owns the diagnostic or such modification is explicitly intended.
     *
     * @return                  the currently stored diagnostic, or null if no diagnostic exists.
     *
     * @note                    This function does not create, copy or clear the diagnostic.
    */
    fun getError() -> pointer<Diagnostic> = this.errorResult


    /**
     * Constructs and returns the parser result from the consumed tokens.
     *
     * The stored resultConstructor callback is invoked with this ParsedObject's
     * current TokenList. The callback determines the concrete type and ownership
     * of the returned object.
     *
     * This function may be called before parsing has succeeded. In that case,
     * resultConstructor receives the currently stored token list, which is
     * normally empty for a newly initialized ParsedObject.
     *
     * Calling this function multiple times invokes resultConstructor each time.
     * Depending on the callback implementation, each call may allocate and return
     * a new result object.
     *
     * The caller must ensure that resultConstructor is valid and that the stored
     * tokens satisfy the callback's requirements.
     *
     * @return                  the parser result created by resultConstructor.
     *
     * @note                    The returned pointer may refer to any parser result type.
     *
     * @warning                 A null or invalid resultConstructor causes undefined behavior.
    */
    fun getResult() -> pointer<*> = this.resultConstructor(this.tokens)
}
