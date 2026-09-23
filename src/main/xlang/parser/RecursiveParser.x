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

import xlang.Diagnostic
import xlang.SourceLocation
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.parser.util.ParserRef
import xlang.parser.util.ParserRefs
import xlang.parser.util.PatternAtom
import xlang.parser.util.PatternList
import xlang.parser.util.Rule
import xlang.util.ArrayList


/**
 * Implements a rule-based recursive parser.
 *
 * <p>A {@code RecursiveParser} contains an ordered collection of parsing rules.
 * During parsing, each rule is tested against the input token stream beginning
 * at the specified cursor. The first rule that matches completely determines
 * the parse result.
 *
 * <p>If no rule matches successfully, the parser tracks the longest partial
 * match and uses that position to generate an unexpected-token diagnostic.
 *
 * <p>Rules may contain regular token patterns, references to other parsers, and
 * repeated parser references. Nested parser references are cloned before use so
 * that their transient parsing state does not interfere with other operations.
 *
 * <p>The parser stores the most recent {@code ParseContainer} result and the
 * most recent diagnostic produced during parsing.
 */
struct RecursiveParser
{
    /**
     * The identifier assigned to parse results produced by this parser.
     *
     * <p>The identifier is stored as the {@code kind} of each resulting
     * {@code ParseContainer}.
     */
    private var id: int

    /**
     * The diagnostic produced by the current parse operation.
     *
     * <p>This value is {@code null} when no parsing error has been recorded.
     */
    private var error: pointer<Diagnostic>

    /**
     * The most recent parse result produced by this parser.
     *
     * <p>This value is {@code null} when no successful parse result is
     * currently available.
     */
    private var result: pointer<ParseContainer>

    /**
     * The collection of parsing rules evaluated by this parser.
     *
     * <p>Rules are stored in insertion order and are tested in that order.
     * The rule objects are referenced directly and are not deeply copied.
     */
    private var rules: pointer<ArrayList>


    /**
     * Creates an empty recursive parser with the specified result identifier.
     *
     * <p>No diagnostic or parse result is initially available. A new empty rule
     * collection is allocated for this parser.
     *
     * @param id                the identifier assigned to parse results produced by this parser
     */
    constructor(id: int)
    {
        this.id = id
        this.error = null
        this.result = null
        this.rules = new ArrayList(sizeof(Rule))
    }


    /**
     * Creates a recursive parser using an existing rule collection.
     *
     * <p>The supplied rule list is stored by reference and is not copied.
     * No diagnostic or parse result is initially available.
     *
     * @param id                the identifier assigned to parse results produced by this parser
     * @param rules             a pointer to the rule collection used by this parser
     */
    constructor(id: int, rules: pointer<ArrayList>)
    {
        this.id = id
        this.error = null
        this.result = null
        this.rules = rules
    }


    /**
     * Returns the identifier of this parser.
     *
     * @return                  the parser identifier
     */
    fun getId() -> int = this.id


    /**
     * Returns the rule at the specified index.
     *
     * @param index             the index of the rule to retrieve
     *
     * @return                  a pointer to the rule at the specified index
     */
    fun getRule(index: int) -> pointer<Rule> = this.rules.get(index) as pointer<Rule>


    /**
     * Returns the number of rules registered with this parser.
     *
     * @return                  the number of parser rules
     */
    fun ruleLength() -> int = this.rules.length


    /**
     * Adds a parsing rule to this parser.
     *
     * <p>The rule is appended to the end of the rule collection and is stored
     * by reference.
     *
     * @param rule              a pointer to the rule to add
     *
     * @return                  this {@code RecursiveParser} instance
     */
    fun addRule(rule: pointer<Rule>) -> pointer<RecursiveParser>
    {
        this.rules.push(rule)
        return this
    }


    /**
     * Creates and stores an internal parser diagnostic.
     *
     * <p>The generated diagnostic initially contains an empty source-location
     * list.
     *
     * @param code              the diagnostic code
     * @param message           a pointer to the diagnostic message
     *
     * @return                  this {@code RecursiveParser} instance
     */
    private fun pushInternalError(code: int, message: pointer<char>) -> pointer<RecursiveParser>
    {
        this.error = Diagnostic.makeInternalError(
            code,
            new ArrayList(sizeof(SourceLocation)),
            message)
        return this
    }


    /**
     * Returns the diagnostic produced by the current parse operation.
     *
     * @return a pointer to the current diagnostic, or {@code null} if no error
     *         has been recorded
     */
    fun getError() -> pointer<Diagnostic> = this.error


    /**
     * Checks whether a parse operation should be considered failed.
     *
     * <p>A parse is considered erroneous if a diagnostic has been recorded or
     * if the reported number of consumed tokens is negative.
     *
     * @param eaten             the number of tokens reported as consumed
     *
     * @return                  {@code true} if the parse failed; {@code false} otherwise
     */
    fun haveError(eaten: int) -> bool = this.error != null || eaten < 0


    /**
     * Resets the transient state of this parser.
     *
     * <p>The current diagnostic and parse result are cleared. The parser
     * identifier and registered rules are preserved.
     *
     * @return                  this {@code RecursiveParser} instance
     */
    fun reset() -> pointer<RecursiveParser>
    {
        this.error = null
        this.result = null
        return this
    }


    /**
     * Attempts to match one of the supplied rules against the token stream.
     *
     * <p>Rules are tested in list order. The first rule that matches completely
     * is accepted and its consumed length is returned.
     *
     * <p>If a rule produces an internal diagnostic, matching stops immediately.
     *
     * <p>If no rule matches completely, the longest partial match is used to
     * determine the position of the unexpected token. An
     * {@code Diagnostic.UNEXPECTED_TOKEN} diagnostic is then generated for that
     * source location.
     *
     * @param token             a pointer to the token list to parse
     * @param cursor            the token index at which parsing begins
     * @param rules             a pointer to the collection of rules to test
     *
     * @return                  the number of tokens consumed by the successful rule, or the
     *                          length of the longest partial match if no rule succeeds
     */
    private fun tryParse(token: pointer<TokenList>, cursor: int, rules: pointer<ArrayList>) -> int
    {
        var maxMatchLength: int = 0

        for (var i = 0; i < rules.length; i++):
        {
            val rule: pointer<Rule> = rules.get(i) as pointer<Rule>
            var matchLength: int = 0

            if !rule.mayStartWith(token, cursor):
                continue

            if this.tryParse(token, cursor, rule, 0, matchLength.ref) || this.getError() != null:
                return matchLength

            if matchLength > maxMatchLength:
                maxMatchLength = matchLength
        }

        var errorIndex: int = cursor + maxMatchLength

        if errorIndex >= token.length():
            errorIndex = token.length() - 1

        val errorToken: pointer<Token> = token.get(errorIndex)
        val locations: pointer<ArrayList> = new ArrayList(sizeof(SourceLocation))
        val location: pointer<SourceLocation> = new SourceLocation(
            null,
            errorToken.pos.offset,
            errorToken.pos.line,
            errorToken.pos.column,
            errorToken.pos.length)

        locations.push(location)
        this.error = Diagnostic.makeError(
            Diagnostic.UNEXPECTED_TOKEN,
            locations,
            Diagnostic.UNEXPECTED_TOKEN_MSG)

        return maxMatchLength
    }


    /**
     * Attempts to match a specific parser rule against the token stream.
     *
     * <p>The rule pattern is processed sequentially beginning at
     * {@code patternStart}. Regular pattern atoms match individual tokens,
     * parser-reference atoms invoke another parser, and repeated-parser atoms
     * invoke their associated {@code ParserRefs}.
     *
     * <p>Matched tokens are cloned before being added to the intermediate result
     * list. Results produced by referenced parsers are added by reference.
     *
     * <p>If the rule has an empty pattern, its result constructor is invoked
     * immediately with an empty result list and no tokens are consumed.
     *
     * <p>After all pattern atoms have matched, the collected values are passed
     * to the rule's result constructor. The constructed value is wrapped in a
     * {@code ParseContainer} using this parser's identifier.
     *
     * <p>If result construction fails, a
     * {@code Diagnostic.CANNOT_CONSTRUCT_AST} internal diagnostic is recorded.
     *
     * <p>The rule's post-processing function is invoked after successful result
     * construction.
     *
     * @param token             a pointer to the token list to parse
     * @param cursor            the token index at which matching begins
     * @param rule              a pointer to the rule being tested
     * @param patternStart      the index of the first pattern atom to process
     * @param matchLength       a pointer that receives the number of matched tokens
     *
     * @return                  {@code true} if the complete rule matches successfully;
     *                          {@code false} otherwise
     */
    private fun tryParse(
        token: pointer<TokenList>, cursor: int,
        rule: pointer<Rule>, patternStart: int,
        matchLength: pointer<int>) -> bool
    {
        // empty  rule
        if rule.isEmpty():
        {
            val results: pointer<ArrayList> = new ArrayList(sizeof(pointer<*>))
            val constructedResult: pointer<*> = rule.constructResult(results)

            this.result = new ParseContainer(this.id, constructedResult)

            // call after
            rule.afterFun(token, cursor)

            matchLength.deref = 0
            return true
        }

        var consumed: int = 0
        val results: pointer<ArrayList> = new ArrayList(sizeof(pointer<*>))
        val pattern: pointer<PatternList> = rule.getPattern()

        matchLength.deref = 0

        for (var i = patternStart; i < pattern.length(); i++):
        {
            val atom: pointer<PatternAtom> = pattern.get(i)

            if atom.isRegex():
            {
                val length: int = atom.matchRegex(token, cursor + consumed)

                if length < 0:
                {
                    matchLength.deref = consumed
                    return false
                }

                // add to result
                val matchedToken: pointer<Token> = token.get(cursor + consumed)
                val resultToken: pointer<Token> = matchedToken.clone()
                val resultItem: pointer<*> = resultToken as pointer<*>

                results.push(resultItem.ref)
                consumed += length
            }
            elif atom.isRef():
            {
                val sourceParser: pointer<ParserRef> = atom.getRefParser()

                if !sourceParser.mayStartWith(token, cursor + consumed):
                {
                    matchLength.deref = consumed
                    return false
                }

                val refParser: pointer<ParserRef> = sourceParser.clone()
                val innerConsumed: int = refParser.parse(token, cursor + consumed)

                if !refParser.haveError(innerConsumed):
                {
                    consumed += innerConsumed

                    // add to result
                    val innerResult: pointer<ParseContainer> = refParser.getResult()
                    results.push(innerResult.ref)
                }
                else:
                {
                    matchLength.deref = if innerConsumed > 0:
                                            consumed + innerConsumed
                                        else:
                                            consumed
                    return false
                }
            }
            elif atom.isRefs():
            {
                val sourceParser: pointer<ParserRefs> = atom.getRefsParser()

                if !sourceParser.mayStartWith(token, cursor + consumed):
                {
                    val emptyResults: pointer<ArrayList> = new ArrayList(sizeof(pointer<*>))
                    val emptyContainer: pointer<ParseContainer> =
                        new ParseContainer(ParseContainer.ARRAY_LIST_KIND, emptyResults)

                    results.push(emptyContainer.ref)
                    continue
                }

                val refsParser: pointer<ParserRefs> = sourceParser.clone()
                val innerConsumed: int = refsParser.parse(token, cursor + consumed)

                if innerConsumed < 0:
                {
                    matchLength.deref = consumed
                    return false
                }

                consumed += innerConsumed

                // add to result
                val innerResults: pointer<ParseContainer> = refsParser.getResult()
                results.push(innerResults.ref)
            }
            else:
            {
                matchLength.deref = consumed
                return false
            }
        }

        val constructedResult: pointer<*> = rule.constructResult(results)

        if constructedResult == null:
        {
            matchLength.deref = consumed
            this.pushInternalError(
                Diagnostic.CANNOT_CONSTRUCT_AST,
                Diagnostic.CANNOT_CONSTRUCT_AST_MSG)
            return false
        }

        this.result = new ParseContainer(this.id, constructedResult)

        // call after
        rule.afterFun(token, cursor + consumed)

        matchLength.deref = consumed

        return true
    }


    /**
     * Parses the token stream beginning at the specified cursor.
     *
     * <p>The parser state is reset before parsing begins.
     *
     * <p>If {@code token} is {@code null}, a {@code Diagnostic.NULL_INPUT}
     * internal diagnostic is generated. If the token list is empty or
     * {@code cursor} is outside the valid token range, a
     * {@code Diagnostic.EMPTY_INPUT} diagnostic is generated.
     *
     * <p>Otherwise, the registered rule collection is tested against the token
     * stream.
     *
     * @param token             a pointer to the token list to parse
     * @param cursor            the token index at which parsing begins
     *
     * @return                  the number of matched tokens, or {@code -1} if the input is invalid
     */
    fun parse(token: pointer<TokenList>, cursor: int) -> int
    {
        this.reset()

        if token == null:
        {
            this.pushInternalError(Diagnostic.NULL_INPUT, Diagnostic.NULL_INPUT_MSG)
            return -1
        }

        if token.length() <= 0 || cursor < 0 || cursor >= token.length():
        {
            this.pushInternalError(Diagnostic.EMPTY_INPUT, Diagnostic.EMPTY_INPUT_MSG)
            return -1
        }

        return this.tryParse(token, cursor, this.rules)
    }


    fun canBeEmpty() -> bool
    {
        for (var i = 0; i < this.rules.length; i++):
        {
            val rule: pointer<Rule> = this.rules.get(i) as pointer<Rule>

            if rule != null && rule.canBeEmpty():
                return true
        }

        return false
    }


    fun mayStartWith(tokens: pointer<TokenList>, index: int) -> bool
    {
        if tokens == null || index < 0 || index >= tokens.length():
            return false

        for (var i = 0; i < this.rules.length; i++):
        {
            val rule: pointer<Rule> = this.rules.get(i) as pointer<Rule>

            if rule != null && rule.mayStartWith(tokens, index):
                return true
        }

        return false
    }


    /**
     * Parses from the beginning of the supplied token list and removes the
     * successfully consumed tokens.
     *
     * <p>If parsing produces an error, this method returns {@code -1} and does
     * not remove tokens from the input.
     *
     * <p>If the parser reports consuming more tokens than are available, an
     * internal diagnostic is generated and the operation fails.
     *
     * <p>On success, the consumed prefix is removed from {@code input}.
     *
     * @param input             a pointer to the token list to parse and consume
     *
     * @return                  the number of consumed and removed tokens, or {@code -1} if
     *                          parsing fails
     */
    fun doParse(input: pointer<TokenList>) -> int
    {
        val consumed: int = this.parse(input, 0)

        if this.haveError(consumed):
            return -1

        if consumed > input.length():
        {
            this.pushInternalError(0, "internal error: parser consumed more tokens than input length")
            return -1
        }

        input.remove(0, consumed)
        return consumed
    }


    /**
     * Returns the result produced by the most recent successful parse.
     *
     * @return                  a pointer to the current parse result, or {@code null} if no
     *                          successful result is available
     */
    fun getResult() -> pointer<ParseContainer> = this.result


    /**
     * Creates a new recursive parser with the same parser identifier and rule
     * collection.
     *
     * <p>The rule collection is shared by reference. Transient state such as
     * the current parse result and diagnostic is not copied.
     *
     * @return                  a pointer to the cloned {@code RecursiveParser}
     */
    fun clone() -> pointer<RecursiveParser> = new RecursiveParser(this.id, this.rules)
}
