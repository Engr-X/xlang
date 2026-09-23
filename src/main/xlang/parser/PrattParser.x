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
 * Implements a Pratt parser for expressions and other precedence-based
 * grammatical constructs.
 *
 * <p>The parser separates grammar rules into starter rules and continuation
 * rules. Starter rules are used to begin an expression, while continuation
 * rules extend an already parsed left-hand expression.
 *
 * <p>Rules are evaluated according to their priority. When continuation rules
 * recursively parse a right-hand expression, the minimum accepted priority is
 * adjusted according to the associativity of the matched operation.
 *
 * <p>The parser stores the most recent parse result and any internal diagnostics
 * generated during parsing.
 */
struct PrattParser
{
    /**
     * The minimum possible parsing priority.
     *
     * <p>This value is used when parsing an expression without imposing a lower
     * priority bound.
     */
    static val MIN_PRIORITY: int = -2147483647 - 1

    /**
     * The identifier assigned to parse results produced by this parser.
     *
     * <p>The identifier is stored as the {@code kind} of each resulting
     * {@code ParseContainer}.
     */
    private var id: int

    /**
     * The list of diagnostics generated during the current parse operation.
     *
     * <p>This collection is reset before each top-level call to {@code parse}.
     */
    private var errors: pointer<ArrayList>

    /**
     * The most recent parse result produced by this parser.
     *
     * <p>This value is {@code null} when no successful result is currently
     * available.
     */
    private var result: pointer<*>

    /**
     * The collection of rules that may begin an expression.
     */
    private var starterRules: pointer<ArrayList>

    /**
     * The collection of rules that may continue an already parsed expression.
     */
    private var continuationRules: pointer<ArrayList>

    /**
     * Indicates whether the parser rule collections are currently sorted.
     *
     * <p>A value of {@code true} means that both starter and continuation rules
     * have been arranged in the priority order required by the parser.
     *
     * <p>This flag should be cleared whenever the rule collections are modified
     * and set to {@code true} after {@code sortRule()} completes.
     */
    private var sorted: bool


    /**
     * Creates an empty Pratt parser.
     *
     * <p>The parser initially uses {@code ParseContainer.ARRAY_LIST_KIND} as its
     * result identifier and contains no starter or continuation rules.
     */
    constructor()
    {
        this.id = ParseContainer.ARRAY_LIST_KIND
        this.errors = new ArrayList(sizeof(Diagnostic))
        this.result = null
        this.starterRules = new ArrayList(sizeof(Rule))
        this.continuationRules = new ArrayList(sizeof(Rule))
        this.sorted = false
    }


    /**
     * Creates a Pratt parser using the supplied starter and continuation rules.
     *
     * <p>The supplied rule collections are stored by reference and are not copied.
     * The parser initially uses {@code ParseContainer.ARRAY_LIST_KIND} as its
     * result identifier.
     *
     * @param starterRules      a pointer to the starter-rule collection
     * @param continuationRules a pointer to the continuation-rule collection
     */
    constructor(starterRules: pointer<ArrayList>, continuationRules: pointer<ArrayList>)
    {
        this.id = ParseContainer.ARRAY_LIST_KIND
        this.errors = new ArrayList(sizeof(Diagnostic))
        this.result = null
        this.starterRules = starterRules
        this.continuationRules = continuationRules
        this.sorted = false
    }


    /**
     * Sorts the starter and continuation rules by priority.
     *
     * <p>Both rule collections are passed to {@code Rule.sortRules}, which returns
     * the rules in the parser-defined priority order.
     *
     * <p>After both collections have been sorted successfully, the {@code sorted}
     * flag is set to {@code true} to indicate that the current rule ordering is
     * valid for parsing.
     */
    private fun sortRules()
    {
        if !this.sorted:
        {
            this.starterRules = Rule.sortRules(this.starterRules)
            this.continuationRules = Rule.sortRules(this.continuationRules)
            this.sorted = true
        }
    }


    /**
     * Sets the identifier assigned to results produced by this parser.
     *
     * @param id                the result identifier to assign
     * @return                  this {@code PrattParser} instance
     */
    fun setId(id: int) -> pointer<PrattParser>
    {
        this.id = id
        return this
    }


    /**
     * Returns the most recently generated diagnostic.
     *
     * @return                  a pointer to the most recent diagnostic, or {@code null} if no
     *                          diagnostic is available
     */
    fun getLastError() -> pointer<Diagnostic> =
        this.errors.peek() as pointer<Diagnostic>


    /**
     * Checks whether the current parse operation should be considered failed.
     *
     * <p>A parse is considered erroneous when at least one diagnostic is
     * available or when the number of consumed tokens is not positive.
     *
     * @param eaten             the number of tokens reported as consumed
     *
     * @return                  {@code true} if the parse failed; {@code false} otherwise
     */
    fun haveError(eaten: int) -> bool =
        this.getLastError() != null || eaten <= 0


    /**
     * Resets the transient state of this parser.
     *
     * <p>The diagnostic list and current result are cleared. Parser rules and
     * the parser identifier are preserved.
     *
     * @return                  this {@code PrattParser} instance
     */
    fun reset() -> pointer<PrattParser>
    {
        this.errors = new ArrayList(sizeof(Diagnostic))
        this.result = null
        return this
    }


    /**
     * Creates and stores an internal parser diagnostic.
     *
     * <p>The generated diagnostic does not contain a source location unless one
     * is added later by another stage of the compiler.
     *
     * @param code              the diagnostic code
     * @param message           a pointer to the diagnostic message
     *
     * @return                  this {@code PrattParser} instance
     */
    private fun pushInternalError(code: int, message: pointer<char>) -> pointer<PrattParser>
    {
        this.errors.push(Diagnostic.makeInternalError(
            code,
            new ArrayList(sizeof(SourceLocation)),
            message))
        return this
    }


    /**
     * Returns the most recent result produced by this parser.
     *
     * @return                  a pointer to the current parse result, or {@code null} if no
     *                          successful result is available
     */
    fun getResult() -> pointer<*> = this.result


    /**
     * Adds a starter rule to this parser.
     *
     * <p>Starter rules are used to begin new Pratt expressions.
     *
     * @param rule              a pointer to the starter rule to add
     *
     * @return                  this {@code PrattParser} instance
     */
    fun addStarterRule(rule: pointer<Rule>) -> pointer<PrattParser>
    {
        this.starterRules.push(rule)
        this.sorted = false
        return this
    }


    /**
     * Adds a continuation rule to this parser.
     *
     * <p>Continuation rules extend an already parsed left-hand expression.
     *
     * @param rule              a pointer to the continuation rule to add
     *
     * @return                  this {@code PrattParser} instance
     */
    fun addContinuationRule(rule: pointer<Rule>) -> pointer<PrattParser>
    {
        this.continuationRules.push(rule)
        this.sorted = false
        return this
    }


    /**
     * Attempts to parse a Pratt expression beginning at the specified cursor.
     *
     * <p>This overload performs parsing without an explicit minimum priority and
     * therefore delegates to the priority-aware overload using
     * {@code MIN_PRIORITY}.
     *
     * @param token             a pointer to the token list to parse
     * @param cursor            the token index at which parsing begins
     * @param matchLength       a pointer that receives the number of consumed tokens
     *
     * @return                  a pointer to the parsed result, or {@code null} if parsing fails
     */
    fun tryParse(
        token: pointer<TokenList>, cursor: int,
        matchLength: pointer<int>) -> pointer<ParseContainer> =
        this.tryParse(token, cursor, MIN_PRIORITY, matchLength)


    /**
     * Attempts to parse a Pratt expression using the specified minimum priority.
     *
     * <p>The parser first matches a starter rule to produce the initial
     * left-hand result. It then repeatedly searches for continuation rules that
     * can extend that result.
     *
     * <p>A continuation rule is applied only when its priority is greater than
     * or equal to {@code minPriority}. If the continuation requires a right-hand
     * expression, that expression is recursively parsed using a priority limit
     * derived from the continuation rule and its associativity.
     *
     * <p>For right-associative operations, the right-hand side may use the same
     * priority as the current rule. For other associativities, the right-hand
     * side must have a strictly greater priority.
     *
     * <p>After a continuation rule is successfully constructed, its configured
     * post-processing function is invoked.
     *
     * @param token             a pointer to the token list to parse
     * @param cursor            the token index at which parsing begins
     * @param minPriority       the minimum continuation-rule priority that may be used
     * @param matchLength       a pointer that receives the total number of consumed tokens
     *
     * @return                  a pointer to the resulting parse container, or {@code null} if
     *                          parsing fails
     */    
    fun tryParse(
        token: pointer<TokenList>, cursor: int,
        minPriority: int,
        matchLength: pointer<int>) -> pointer<ParseContainer>
    {
        /*
         * Parses one Pratt expression from cursor.
         *
         * Pseudocode:
         *
         *     left = parse starter rule at cursor
         *
         *     if left failed:
         *         return null
         *
         *     consumed = starter length
         *
         *     while true:
         *         continuation = match continuation rule at cursor + consumed
         *
         *         if no continuation matched:
         *             break
         *
         *         if continuation.priority < minPriority:
         *             break
         *
         *         right = parse expression after continuation
         *             with minPriority = continuation.priority + 1
         *
         *         if right failed:
         *             return null
         *
         *         left = continuation.constructResult(left, continuation parts, right)
         *         consumed += continuation length + right length
         *
         *     matchLength = consumed
         *     result = left
         *     return left
         */

        this.sortRules()

        var consumed: int = 0
        var starterLength: int = 0
        var left: pointer<ParseContainer> = this.tryParseStarter(token, cursor, starterLength.ref)

        matchLength.deref = 0

        if left == null:
            return null

        consumed += starterLength

        while true:
        {
            var continuationLength: int = 0
            var continuationResults: pointer<ArrayList> = null
            val rule: pointer<Rule> = this.tryParseContinuationHead(
                token,
                cursor + consumed,
                left,
                continuationResults.ref,
                continuationLength.ref)

            if rule == null:
                break

            if rule.priority < minPriority:
                break

            val pattern: pointer<PatternList> = rule.getPattern()
            val lastAtom: pointer<PatternAtom> = pattern.get(pattern.length() - 1)
            val hasRight: bool = lastAtom.isRef() &&
                lastAtom.getRefParser().getId() == left.getKind()
            var rightLength: int = 0
            var right: pointer<ParseContainer> = null

            if hasRight:
            {
                val rightMinPriority: int =
                    if rule.getAssociativity() == Operation.RIGHT_ASSOC:
                        rule.priority
                    else:
                        rule.priority + 1

                right = this.tryParse(
                    token,
                    cursor + consumed + continuationLength,
                    rightMinPriority,
                    rightLength.ref)

                if right == null:
                    return null
            }

            val results: pointer<ArrayList> = new ArrayList(sizeof(pointer<*>))
            val leftItem: pointer<*> = left as pointer<*>

            results.push(leftItem.ref)

            for (var i = 0; i < continuationResults.length; i++):
            {
                val slot: pointer<pointer<*>> = continuationResults.get(i) as pointer<pointer<*>>

                if slot != null:
                    results.push(slot)
            }

            if hasRight:
            {
                val rightItem: pointer<*> = right as pointer<*>
                results.push(rightItem.ref)
            }

            val constructedResult: pointer<*> = rule.constructResult(results)

            if constructedResult == null:
            {
                this.pushInternalError(
                    Diagnostic.CANNOT_CONSTRUCT_AST,
                    Diagnostic.CANNOT_CONSTRUCT_AST_MSG)
                return null
            }

            left = new ParseContainer(this.id, constructedResult)

            consumed += continuationLength + rightLength

            // call after
            rule.afterFun(token, cursor + consumed)
        }

        this.result = left
        matchLength.deref = consumed

        return left
    }


    /**
     * Parses a Pratt expression beginning at the specified token index.
     *
     * <p>The parser state is reset before parsing begins.
     *
     * <p>If {@code token} is {@code null}, a {@code Diagnostic.NULL_INPUT}
     * diagnostic is generated. If the token list is empty or {@code cursor}
     * lies outside the valid range, a {@code Diagnostic.EMPTY_INPUT}
     * diagnostic is generated.
     *
     * @param token             a pointer to the token list to parse
     * @param cursor            the token index at which parsing begins
     *
     * @return                  the number of consumed tokens, or {@code -1} if parsing fails
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

        var matchLength: int = 0
        val result: pointer<ParseContainer> = this.tryParse(token, cursor, matchLength.ref)

        if result == null:
            return -1

        return matchLength
    }


    fun canBeEmpty() -> bool = false


    fun mayStartWith(tokens: pointer<TokenList>, index: int) -> bool
    {
        if tokens == null || index < 0 || index >= tokens.length():
            return false

        this.sortRules()

        for (var i = 0; i < this.starterRules.length; i++):
        {
            val rule: pointer<Rule> = this.starterRules.get(i) as pointer<Rule>

            if rule != null && rule.mayStartWith(tokens, index):
                return true
        }

        return false
    }


    /**
     * Parses an expression from the beginning of the supplied token list and
     * removes the successfully consumed tokens.
     *
     * <p>If parsing fails or produces a diagnostic, this method returns
     * {@code -1} and leaves the input unchanged.
     *
     * <p>An internal diagnostic is generated if the parser reports consuming
     * more tokens than are present in the input.
     *
     * @param input             a pointer to the token list to parse
     *
     * @return                  the number of tokens consumed and removed from {@code input},
     *                          or {@code -1} if parsing fails
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
     * Attempts to parse the starter portion of a Pratt expression.
     *
     * <p>Starter rules are considered by priority, beginning with the highest
     * available priority. All rules at the same priority are tested before
     * lower-priority rules are considered.
     *
     * <p>If exactly one rule at a priority level matches, its result is
     * returned. If multiple rules at the same priority match successfully, an
     * {@code Diagnostic.AMBIGUOUS_PARSER_RULE} diagnostic is generated.
     *
     * <p>If no rule at the current priority matches, parsing continues with the
     * next lower priority.
     *
     * @param token             a pointer to the token list to parse
     * @param cursor            the token index at which starter parsing begins
     * @param matchLength       a pointer that receives the number of consumed tokens
     *
     * @return                  a pointer to the matched starter result, or {@code null} if no
     *                          starter rule matches
     */
    private fun tryParseStarter(
        token: pointer<TokenList>, cursor: int,
        matchLength: pointer<int>
    ) -> pointer<ParseContainer>
    {
        matchLength.deref = 0

        var index: int = 0

        while index < this.starterRules.length:
        {
            val firstRule: pointer<Rule> =
                this.starterRules.get(index) as pointer<Rule>

            val currentPriority: int = firstRule.priority

            var matchedRuleCount: int = 0
            var matchedResult: pointer<ParseContainer> = null
            var matchedLength: int = 0

            // Check all rules with the same priority.
            while index < this.starterRules.length:
            {
                val rule: pointer<Rule> =
                    this.starterRules.get(index) as pointer<Rule>

                if rule.priority != currentPriority:
                    break

                index++

                if !rule.mayStartWith(token, cursor):
                    continue

                var currentMatchLength: int = 0
                val currentResult: pointer<ParseContainer> =
                    this.tryParseStarterRule(
                        token, cursor, rule, 0,
                        currentMatchLength.ref)

                if currentResult == null:
                    continue

                matchedRuleCount++
                matchedResult = currentResult
                matchedLength = currentMatchLength
            }

            // More than one rule with the same priority matched.
            if matchedRuleCount >= 2:
            {
                this.pushInternalError(
                    Diagnostic.AMBIGUOUS_PARSER_RULE,
                    Diagnostic.AMBIGUOUS_PARSER_RULE_MSG)

                return null
            }

            // Exactly one rule matched at the current priority.
            if matchedRuleCount == 1:
            {
                this.result = matchedResult
                matchLength.deref = matchedLength
                return matchedResult
            }

            // No rule matched at this priority.
            // Continue with the next lower priority group.
        }

        return null
    }
    // private fun tryParseStarter(
    //     token: pointer<TokenList>, cursor: int,
    //     matchLength: pointer<int>) -> pointer<ParseContainer>
    // {
    //     var havePriorityLimit: bool = false
    //     var priorityLimit: int = 0

    //     matchLength.deref = 0

    //     while true:
    //     {
    //         var foundPriority: bool = false
    //         var currentPriority: int = MIN_PRIORITY

    //         for (var i = 0; i < this.starterRules.length; i++):
    //         {
    //             val rule: pointer<Rule> = this.starterRules.get(i) as pointer<Rule>

    //             if havePriorityLimit && rule.priority >= priorityLimit:
    //                 continue

    //             if !foundPriority || rule.priority > currentPriority:
    //             {
    //                 foundPriority = true
    //                 currentPriority = rule.priority
    //             }
    //         }

    //         if !foundPriority:
    //             return null

    //         var matchedRuleCount: int = 0
    //         var bestResult: pointer<ParseContainer> = null
    //         var bestMatchLength: int = 0

    //         for (var i = 0; i < this.starterRules.length; i++):
    //         {
    //             val rule: pointer<Rule> = this.starterRules.get(i) as pointer<Rule>

    //             if rule.priority != currentPriority:
    //                 continue

    //             val pattern: pointer<PatternList> = rule.getPattern()

    //             if pattern.length() > 0:
    //             {
    //                 val first: pointer<PatternAtom> = pattern.get(0)

    //                 if first.isRegex() && first.matchRegex(token, cursor) < 0:
    //                     continue
    //             }

    //             var currentMatchLength: int = 0
    //             val currentResult: pointer<ParseContainer> =
    //                 this.tryParseStarterRule(token, cursor, rule, 0, currentMatchLength.ref)

    //             if currentResult != null:
    //             {
    //                 matchedRuleCount++
    //                 bestResult = currentResult
    //                 bestMatchLength = currentMatchLength
    //             }
    //         }

    //         if matchedRuleCount >= 2:
    //         {
    //             this.pushInternalError(
    //                 Diagnostic.AMBIGUOUS_PARSER_RULE,
    //                 Diagnostic.AMBIGUOUS_PARSER_RULE_MSG)
    //             return null
    //         }

    //         if matchedRuleCount == 1:
    //         {
    //             this.result = bestResult
    //             matchLength.deref = bestMatchLength
    //             return bestResult
    //         }

    //         havePriorityLimit = true
    //         priorityLimit = currentPriority
    //     }

    //     return null
    // }


    /**
     * Attempts to match a specific starter rule.
     *
     * <p>The rule pattern is processed sequentially beginning at
     * {@code patternStart}. Regular pattern atoms match and clone tokens,
     * parser-reference atoms invoke the referenced parser, and repeated-parser
     * atoms invoke their associated {@code ParserRefs}.
     *
     * <p>When a prefix rule references this Pratt parser itself, recursive Pratt
     * parsing is performed using the priority of the current rule. This allows
     * prefix operators to participate correctly in precedence handling.
     *
     * <p>Successfully parsed pattern results are collected and passed to the
     * rule's result constructor. If construction succeeds, the resulting object
     * is wrapped in a {@code ParseContainer} using this parser's identifier.
     *
     * <p>The rule's post-processing function is invoked after successful result
     * construction.
     *
     * @param token             a pointer to the token list to parse
     * @param cursor            the token index at which matching begins
     * @param rule              a pointer to the starter rule being tested
     * @param patternStart      the first pattern atom to process
     * @param matchLength       a pointer that receives the number of consumed tokens
     *
     * @return                  a pointer to the constructed parse result, or {@code null} if the
     *                          rule cannot be matched
     */
    private fun tryParseStarterRule(
        token: pointer<TokenList>, cursor: int,
        rule: pointer<Rule>, patternStart: int,
        matchLength: pointer<int>) -> pointer<ParseContainer>
    {
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
                    return null
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
                    return null
                }

                val refParser: pointer<ParserRef> = sourceParser.clone()
                var innerConsumed: int = 0
                var innerResult: pointer<ParseContainer> = null
                var hasError: bool = false

                if rule.getFixity() == Operation.PREFIX_TYPE && refParser.getId() == this.id:
                {
                    innerResult = this.tryParse(
                        token,
                        cursor + consumed,
                        rule.priority,
                        innerConsumed.ref)
                    hasError = innerResult == null
                }
                else:
                {
                    innerConsumed = refParser.parse(token, cursor + consumed)
                    hasError = refParser.haveError(innerConsumed)

                    if !hasError:
                        innerResult = refParser.getResult()
                }

                if !hasError:
                {
                    consumed += innerConsumed

                    // add to result
                    results.push(innerResult.ref)
                }
                else:
                {
                    matchLength.deref = if innerConsumed > 0:
                                            consumed + innerConsumed
                                        else:
                                            consumed
                    return null
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
                    return null
                }

                consumed += innerConsumed

                // add to result
                val innerResults: pointer<ParseContainer> = refsParser.getResult()
                results.push(innerResults.ref)
            }
            else:
            {
                matchLength.deref = consumed
                return null
            }
        }

        val constructedResult: pointer<*> = rule.constructResult(results)

        if constructedResult == null:
            return null

        // call after
        rule.afterFun(token, cursor + consumed)

        matchLength.deref = consumed

        return new ParseContainer(this.id, constructedResult)
    }


    /**
     * Attempts to match the head of a continuation rule for an existing
     * left-hand expression.
     *
     * <p>A valid continuation rule must begin with a parser reference whose
     * identifier matches the kind of {@code left}. This first self-reference
     * represents the already parsed left-hand expression and is not parsed
     * again.
     *
     * <p>Continuation rules are considered by priority. At each priority level,
     * every compatible rule is tested. If more than one rule matches at the
     * same priority, an {@code Diagnostic.AMBIGUOUS_PARSER_RULE} diagnostic is
     * generated.
     *
     * <p>Pattern elements following the initial left reference are parsed until
     * another reference to the same parser kind is reached. Such a reference is
     * interpreted as the right-hand expression and is parsed later by the main
     * Pratt loop.
     *
     * @param token             a pointer to the token list to parse
     * @param cursor            the token index immediately following the left-hand expression
     * @param left              a pointer to the already parsed left-hand result
     * @param results           a pointer that receives the intermediate continuation results
     * @param matchLength       a pointer that receives the number of consumed continuation
     *                          tokens before the right-hand expression
     *
     * @return                  a pointer to the matched continuation rule, or {@code null} if no
     *                          compatible rule matches
     */
    private fun tryParseContinuationHead(
        token: pointer<TokenList>, cursor: int,
        left: pointer<ParseContainer>,
        results: pointer<pointer<ArrayList>>,
        matchLength: pointer<int>) -> pointer<Rule>
    {
        results.deref = null
        matchLength.deref = 0

        if token == null || cursor < 0 || left == null:
            return null

        var index: int = 0

        while index < this.continuationRules.length:
        {
            val firstRule: pointer<Rule> =
                this.continuationRules.get(index) as pointer<Rule>

            val currentPriority: int = firstRule.priority

            var matchedRuleCount: int = 0
            var matchedRule: pointer<Rule> = null
            var matchedResults: pointer<ArrayList> = null
            var matchedLength: int = 0

            // Check all continuation rules with the same priority.
            while index < this.continuationRules.length:
            {
                val rule: pointer<Rule> =
                    this.continuationRules.get(index) as pointer<Rule>

                if rule.priority != currentPriority:
                    break

                index++

                val pattern: pointer<PatternList> = rule.getPattern()

                if pattern == null || pattern.length() <= 0:
                    continue

                val first: pointer<PatternAtom> = pattern.get(0)

                // A continuation rule must begin with a reference to
                // the parser that produced the left-hand result.
                if first == null || !first.isRef():
                    continue

                if first.getRefParser().getId() != left.getKind():
                    continue

                if !pattern.mayStartWithFrom(1, token, cursor):
                    continue

                var currentMatchLength: int = 0
                val currentResults: pointer<ArrayList> =
                    this.tryParseUntilSelfRef(
                        token,
                        cursor,
                        rule,
                        1,
                        left,
                        currentMatchLength.ref)

                if currentResults == null:
                    continue

                matchedRuleCount++
                matchedRule = rule
                matchedResults = currentResults
                matchedLength = currentMatchLength
            }

            if matchedRuleCount >= 2:
            {
                this.pushInternalError(
                    Diagnostic.AMBIGUOUS_PARSER_RULE,
                    Diagnostic.AMBIGUOUS_PARSER_RULE_MSG)

                return null
            }

            if matchedRuleCount == 1:
            {
                results.deref = matchedResults
                matchLength.deref = matchedLength
                return matchedRule
            }

            // No continuation rule at this priority matched.
            // Continue with the next lower-priority group.
        }

        return null
    }
    // private fun tryParseContinuationHead(
    //     token: pointer<TokenList>, cursor: int,
    //     left: pointer<ParseContainer>,
    //     results: pointer<pointer<ArrayList>>,
    //     matchLength: pointer<int>) -> pointer<Rule>
    // {
    //     results.deref = null
    //     matchLength.deref = 0

    //     if token == null || cursor < 0 || left == null:
    //         return null

    //     var havePriorityLimit: bool = false
    //     var priorityLimit: int = 0

    //     while true:
    //     {
    //         var foundPriority: bool = false
    //         var currentPriority: int = MIN_PRIORITY

    //         for (var i = 0; i < this.continuationRules.length; i++):
    //         {
    //             val rule: pointer<Rule> = this.continuationRules.get(i) as pointer<Rule>
    //             val pattern: pointer<PatternList> = rule.getPattern()
    //             val first: pointer<PatternAtom> = pattern.get(0)

    //             if first == null || !first.isRef():
    //                 continue

    //             if first.getRefParser().getId() != left.getKind():
    //                 continue

    //             if havePriorityLimit && rule.priority >= priorityLimit:
    //                 continue

    //             if !foundPriority || rule.priority > currentPriority:
    //             {
    //                 foundPriority = true
    //                 currentPriority = rule.priority
    //             }
    //         }

    //         if !foundPriority:
    //             return null

    //         var matchedRuleCount: int = 0
    //         var bestRule: pointer<Rule> = null
    //         var bestResults: pointer<ArrayList> = null
    //         var bestMatchLength: int = 0

    //         for (var i = 0; i < this.continuationRules.length; i++):
    //         {
    //             val rule: pointer<Rule> = this.continuationRules.get(i) as pointer<Rule>

    //             if rule.priority != currentPriority:
    //                 continue

    //             val pattern: pointer<PatternList> = rule.getPattern()
    //             val first: pointer<PatternAtom> = pattern.get(0)

    //             if first == null || !first.isRef():
    //                 continue

    //             if first.getRefParser().getId() != left.getKind():
    //                 continue

    //             if pattern.length() > 1:
    //             {
    //                 val head: pointer<PatternAtom> = pattern.get(1)

    //                 if head.isRegex() && head.matchRegex(token, cursor) < 0:
    //                     continue
    //             }

    //             var currentMatchLength: int = 0
    //             val currentResults: pointer<ArrayList> = this.tryParseUntilSelfRef(
    //                 token, cursor,
    //                 rule, 1,
    //                 left,
    //                 currentMatchLength.ref)

    //             if currentResults != null:
    //             {
    //                 matchedRuleCount++
    //                 bestRule = rule
    //                 bestResults = currentResults
    //                 bestMatchLength = currentMatchLength
    //             }
    //         }

    //         if matchedRuleCount >= 2:
    //         {
    //             this.pushInternalError(
    //                 Diagnostic.AMBIGUOUS_PARSER_RULE,
    //                 Diagnostic.AMBIGUOUS_PARSER_RULE_MSG)
    //             return null
    //         }

    //         if matchedRuleCount == 1:
    //         {
    //             results.deref = bestResults
    //             matchLength.deref = bestMatchLength
    //             return bestRule
    //         }

    //         havePriorityLimit = true
    //         priorityLimit = currentPriority
    //     }

    //     return null
    // }


    /**
     * Parses a continuation-rule pattern until a self-reference is encountered.
     *
     * <p>The pattern is processed starting at {@code patternStart}. Regular token
     * atoms, external parser references, and repeated parser references are
     * parsed normally.
     *
     * <p>If a parser-reference atom has the same identifier as the kind of
     * {@code left}, parsing stops before that atom. The matching self-reference
     * is treated as the position of the right-hand expression and is therefore
     * handled by the outer Pratt parsing routine.
     *
     * <p>All successfully parsed intermediate values are appended to the
     * returned result list.
     *
     * @param token             a pointer to the token list to parse
     * @param cursor            the token index at which parsing begins
     * @param rule              a pointer to the continuation rule being processed
     * @param patternStart      the first pattern atom to process
     * @param left              a pointer to the previously parsed left-hand result
     * @param matchLength       a pointer that receives the number of consumed tokens
     *
     * @return                  a list containing the successfully parsed continuation components,
     *                          or {@code null} if matching fails
     */
    private fun tryParseUntilSelfRef(
        token: pointer<TokenList>, cursor: int,
        rule: pointer<Rule>, patternStart: int,
        left: pointer<ParseContainer>,
        matchLength: pointer<int>) -> pointer<ArrayList>
    {
        var consumed: int = 0
        val results: pointer<ArrayList> = new ArrayList(sizeof(pointer<*>))
        val pattern: pointer<PatternList> = rule.getPattern()
        val leftId: int = left.getKind()

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
                    return null
                }

                val matchedToken: pointer<Token> = token.get(cursor + consumed)
                val resultToken: pointer<Token> = matchedToken.clone()
                val resultItem: pointer<*> = resultToken as pointer<*>

                results.push(resultItem.ref)
                consumed += length
            }
            elif atom.isRef():
            {
                val sourceParser: pointer<ParserRef> = atom.getRefParser()

                if sourceParser.getId() == leftId:
                {
                    matchLength.deref = consumed
                    return results
                }

                if !sourceParser.mayStartWith(token, cursor + consumed):
                {
                    matchLength.deref = consumed
                    return null
                }

                val refParser: pointer<ParserRef> = sourceParser.clone()
                val innerConsumed: int = refParser.parse(token, cursor + consumed)

                if !refParser.haveError(innerConsumed):
                {
                    consumed += innerConsumed

                    val innerResult: pointer<ParseContainer> = refParser.getResult()
                    results.push(innerResult.ref)
                }
                else:
                {
                    matchLength.deref = if innerConsumed > 0:
                                            consumed + innerConsumed
                                        else:
                                            consumed
                    return null
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
                    return null
                }

                consumed += innerConsumed

                val innerResults: pointer<ParseContainer> = refsParser.getResult()
                results.push(innerResults.ref)
            }
            else:
            {
                matchLength.deref = consumed
                return null
            }
        }

        matchLength.deref = consumed
        return results
    }


    /**
     * Creates a new Pratt parser with the same configuration as this parser.
     *
     * <p>The parser identifier is copied, while the starter and continuation
     * rule collections are shared by reference. Runtime state such as the
     * current result and diagnostic list is not copied.
     *
     * @return                  a pointer to the cloned {@code PrattParser}
     */
    fun clone() -> pointer<PrattParser>
    {
        val result: pointer<PrattParser> = new PrattParser(this.starterRules, this.continuationRules)
        result.id = this.id
        return result
    }
}
