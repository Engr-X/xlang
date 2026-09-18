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

import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.parser.ParseContainer
import xlang.util.ArrayList


/**
 * Represents a repeated parser reference that parses zero or more consecutive
 * elements using the same underlying parser.
 *
 * <p>A {@code ParserRefs} repeatedly invokes a {@link ParserRef} and collects
 * the resulting parse nodes into an array-like parse result.
 *
 * <p>Elements may optionally be separated by a token pattern. When a separator
 * is configured, matched separator tokens are stored separately in
 * {@code extraTokens}.
 *
 * <p>A trailing separator may optionally be accepted depending on the value of
 * {@code allowTrailing}.
 */
struct ParserRefs
{
    /**
     * The parser used to parse each repeated element.
     */
    private var parser: pointer<ParserRef>

    /**
     * The optional pattern used to separate consecutive parsed elements.
     *
     * <p>If this value is {@code null}, elements are parsed consecutively
     * without requiring a separator.
     */
    private var splitBy: pointer<PatternList>


    /**
     * Indicates whether a trailing separator is accepted after the final
     * successfully parsed element.
     */
    private var allowTrailing: bool


    /**
     * The separator tokens consumed during the most recent parse operation.
     *
     * <p>This list contains tokens matched by {@code splitBy}, including an
     * accepted trailing separator when trailing separators are enabled.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * The parse results produced during the most recent parse operation.
     *
     * <p>Each element contains the referenced result object returned by the
     * underlying parser.
     */
    private var results: pointer<ArrayList>


    /**
     * Creates a repeated parser without a separator pattern.
     *
     * <p>Elements are parsed consecutively using the supplied parser until the
     * next parse attempt fails or consumes no tokens.
     *
     * @param parser            a pointer to the parser used for each repeated element
     */
    constructor(parser: pointer<ParserRef>)
    {
        this.parser = parser
        this.splitBy = null
        this.allowTrailing = false
        this.extraTokens = new ArrayList(sizeof(Token))
        this.results = new ArrayList(sizeof(pointer<*>))
    }


    /**
     * Creates a repeated parser using a single pattern atom as the separator.
     *
     * <p>If {@code splitBy} is not {@code null}, it is wrapped in a
     * {@code PatternList}. Trailing separators are accepted by default.
     *
     * @param parser            a pointer to the parser used for each repeated element
     * @param splitBy           a pointer to the separator pattern atom, or {@code null}
     *                          to disable separator matching
     */
    constructor(parser: pointer<ParserRef>, splitBy: pointer<PatternAtom>)
    {
        this.parser = parser
        this.splitBy = if splitBy == null:
                null
            else:
                new PatternList(splitBy)

        this.allowTrailing = true
        this.extraTokens = new ArrayList(sizeof(Token))
        this.results = new ArrayList(sizeof(pointer<*>))
    }


    /**
     * Creates a repeated parser using the specified separator pattern.
     *
     * <p>Trailing separators are accepted by default.
     *
     * @param parser            a pointer to the parser used for each repeated element
     * @param splitBy           a pointer to the separator pattern list, or {@code null}
     *                          to disable separator matching
     */
    constructor(parser: pointer<ParserRef>, splitBy: pointer<PatternList>)
    {
        this.parser = parser
        this.splitBy = splitBy
        this.allowTrailing = true
        this.extraTokens = new ArrayList(sizeof(Token))
        this.results = new ArrayList(sizeof(pointer<*>))
    }


    /**
     * Creates a repeated parser using a single separator pattern atom.
     *
     * <p>If {@code splitBy} is not {@code null}, it is wrapped in a
     * {@code PatternList}.
     *
     * @param parser            a pointer to the parser used for each repeated element
     * @param splitBy           a pointer to the separator pattern atom, or {@code null}
     *                          to disable separator matching
     * @param allowTrailing     {@code true} to allow a trailing separator after the
     *                          final element; {@code false} otherwise
     */
    constructor(parser: pointer<ParserRef>, splitBy: pointer<PatternAtom>, allowTrailing: bool)
    {
        this.parser = parser
        this.splitBy = if splitBy == null:
                null
            else:
                new PatternList(splitBy)

        this.allowTrailing = allowTrailing
        this.extraTokens = new ArrayList(sizeof(Token))
        this.results = new ArrayList(sizeof(pointer<*>))
    }


    /**
     * Creates a repeated parser using the specified separator pattern.
     *
     * @param parser            a pointer to the parser used for each repeated element
     * @param splitBy           a pointer to the separator pattern list, or {@code null}
     *                          to disable separator matching 
     * @param allowTrailing     {@code true} to allow a trailing separator after the
     *                          final element; {@code false} otherwise
     */
    constructor(parser: pointer<ParserRef>, splitBy: pointer<PatternList>, allowTrailing: bool)
    {
        this.parser = parser
        this.splitBy = splitBy
        this.allowTrailing = allowTrailing
        this.extraTokens = new ArrayList(sizeof(Token))
        this.results = new ArrayList(sizeof(pointer<*>))
    }


    /**
     * Parses the first element of a repeated sequence.
     *
     * <p>The underlying parser is invoked at {@code index}. If parsing fails,
     * produces an error, or consumes no tokens, this method returns {@code 0}.
     *
     * <p>On success, the parsed result reference is appended to
     * {@code results}.
     *
     * @param tokens            a pointer to the token list being parsed
     * @param index             the index at which parsing begins
     *
     * @return                  the number of tokens consumed by the first element, or
     *                          {@code 0} if the first element cannot be parsed
     */
    private fun parseFirst(tokens: pointer<TokenList>, index: int) -> int
    {
        val consumed: int = this.parser.parse(tokens, index)

        if this.parser.haveError(consumed) || consumed <= 0:
            return 0

        val result: pointer<ParseContainer> = this.parser.getResult()

        this.results.push(result.ref)
        return consumed
    }


    /**
     * Continues parsing repeated elements without separator tokens.
     *
     * <p>The parser is repeatedly invoked immediately after the previously
     * consumed element. Parsing stops when the parser reports an error,
     * consumes no tokens, or the end of the token list is reached.
     *
     * <p>Each successfully parsed result is appended to {@code results}.
     *
     * @param tokens            a pointer to the token list being parsed
     * @param index             the index at which the repeated sequence begins
     * @param firstConsumed     the number of tokens already consumed by the first
     *                          successfully parsed element
     *
     * @return                  the total number of tokens consumed by the repeated sequence
     */
    private fun parseWithoutSplit(tokens: pointer<TokenList>, index: int, firstConsumed: int) -> int
    {
        var consumed: int = firstConsumed

        while index + consumed < tokens.length():
        {
            val innerConsumed: int = this.parser.parse(tokens, index + consumed)

            if this.parser.haveError(innerConsumed) || innerConsumed <= 0:
                break

            val result: pointer<ParseContainer> = this.parser.getResult()

            this.results.push(result.ref)
            consumed += innerConsumed
        }

        return consumed
    }


    /**
     * Continues parsing repeated elements separated by the configured pattern.
     *
     * <p>After each parsed element, {@code splitBy} must match before another
     * element is parsed. If the separator does not match, parsing stops.
     *
     * <p>A separator is counted as consumed only when it is followed by another
     * successfully parsed element, unless {@code allowTrailing} is enabled and
     * the separator is accepted as the trailing separator.
     *
     * <p>All consumed separator tokens are appended to {@code extraTokens},
     * while parsed element results are appended to {@code results}.
     *
     * @param tokens            a pointer to the token list being parsed
     * @param index             the index at which the repeated sequence begins
     * @param firstConsumed     the number of tokens already consumed by the first
     *                          successfully parsed element
     *
     * @return                  the total number of tokens consumed by the repeated sequence,
     *                          including accepted separators
     */
    private fun parseWithSplit(tokens: pointer<TokenList>, index: int, firstConsumed: int) -> int
    {
        var consumed: int = firstConsumed

        if this.splitBy.length() <= 0:
            return consumed

        while index + consumed < tokens.length():
        {
            if !this.splitBy.regMatch(tokens, index + consumed):
                break

            val splitStart: int = index + consumed
            val splitLength: int = this.splitBy.length()
            val nextIndex: int = index + consumed + splitLength

            if nextIndex >= tokens.length():
                break

            val innerConsumed: int = this.parser.parse(tokens, nextIndex)

            if this.parser.haveError(innerConsumed) || innerConsumed <= 0:
                break

            val result: pointer<ParseContainer> = this.parser.getResult()

            this.results.push(result.ref)

            for (var i = 0; i < splitLength; i++):
                this.extraTokens.push(tokens.get(splitStart + i))

            consumed += splitLength + innerConsumed
        }

        if this.allowTrailing && this.splitBy.regMatch(tokens, index + consumed):
        {
            val splitStart: int = index + consumed

            for (var i = 0; i < this.splitBy.length(); i++):
                this.extraTokens.push(tokens.get(splitStart + i))

            consumed += this.splitBy.length()
        }

        return consumed
    }


    /**
     * Parses a repeated sequence of elements beginning at the specified index.
     *
     * <p>The previously stored results and separator tokens are cleared before
     * each parse operation.
     *
     * <p>The first element must be parsed successfully. If no first element can
     * be parsed, this method returns {@code 0}.
     *
     * <p>If no separator pattern is configured, elements are parsed
     * consecutively. Otherwise, additional elements must be separated by the
     * configured pattern.
     *
     * @param tokens            a pointer to the token list to parse
     * @param index             the token index at which parsing begins
     *
     * @return                  the total number of tokens consumed, or {@code 0} if parsing
     *                          cannot begin
     */
    fun parse(tokens: pointer<TokenList>, index: int) -> int
    {
        this.results = new ArrayList(sizeof(pointer<*>))
        this.extraTokens = new ArrayList(sizeof(Token))

        if this.parser == null || tokens == null || index < 0 || index >= tokens.length():
            return 0

        val firstConsumed: int = this.parseFirst(tokens, index)

        if firstConsumed <= 0:
            return 0

        return if this.splitBy == null:
                this.parseWithoutSplit(tokens, index, firstConsumed)
            else:
                this.parseWithSplit(tokens, index, firstConsumed)
    }


    /**
     * Returns the results produced by the most recent parse operation.
     *
     * <p>The individual result references are wrapped in a
     * {@code ParseContainer} of kind {@code ARRAY_LIST_KIND}.
     *
     * @return                  a parse container containing all successfully parsed element results
     */
    fun getResult() -> pointer<ParseContainer> =
        new ParseContainer(ParseContainer.ARRAY_LIST_KIND, this.results)


    /**
     * Returns the separator tokens consumed during the most recent parse
     * operation.
     *
     * @return                  a pointer to the list of consumed separator tokens
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens


    /**
     * Creates a copy of this repeated parser configuration.
     *
     * <p>The underlying {@code ParserRef} is cloned. The separator pattern and
     * trailing-separator configuration are preserved.
     *
     * <p>Parse results and consumed separator tokens from the current instance
     * are not copied into the new instance.
     *
     * @return                  a pointer to the cloned {@code ParserRefs} instance
     */
    fun clone() -> pointer<ParserRefs> =
        if this.splitBy == null:
            new ParserRefs(this.parser.clone())
        else:
            new ParserRefs(this.parser.clone(), this.splitBy, this.allowTrailing)
}
