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


struct PrattParser
{
    static val MIN_PRIORITY: int = -2147483647 - 1

    private static fun compareRulePriority(left: pointer<*>, right: pointer<*>) -> int
    {
        val leftRule: pointer<Rule> = left as pointer<Rule>
        val rightRule: pointer<Rule> = right as pointer<Rule>

        if leftRule.priority == rightRule.priority:
            return 0

        return if leftRule.priority > rightRule.priority:
            -1
        else:
            1
    }

    private var id: int

    private var errors: pointer<ArrayList>

    private var result: pointer<*>

    private var starterRules: pointer<ArrayList>

    private var continuationRules: pointer<ArrayList>


    constructor()
    {
        this.id = ParseContainer.ARRAY_LIST_KIND
        this.errors = new ArrayList(sizeof(Diagnostic))
        this.result = null
        this.starterRules = new ArrayList(sizeof(Rule))
        this.continuationRules = new ArrayList(sizeof(Rule))
    }


    constructor(starterRules: pointer<ArrayList>, continuationRules: pointer<ArrayList>)
    {
        this.id = ParseContainer.ARRAY_LIST_KIND
        this.errors = new ArrayList(sizeof(Diagnostic))
        this.result = null
        this.starterRules = starterRules
        this.continuationRules = continuationRules
    }


    fun setId(id: int) -> pointer<PrattParser>
    {
        this.id = id
        return this
    }


    fun getLastError() -> pointer<Diagnostic> =
        this.errors.peek() as pointer<Diagnostic>


    fun haveError(eaten: int) -> bool =
        this.getLastError() != null || eaten <= 0


    fun reset() -> pointer<PrattParser>
    {
        this.errors = new ArrayList(sizeof(Diagnostic))
        this.result = null
        return this
    }


    private fun pushInternalError(code: int, message: pointer<char>) -> pointer<PrattParser>
    {
        this.errors.push(Diagnostic.makeInternalError(
            code,
            new ArrayList(sizeof(SourceLocation)),
            message))
        return this
    }


    fun getResult() -> pointer<*> = this.result


    fun addStarterRule(rule: pointer<Rule>) -> pointer<PrattParser>
    {
        this.starterRules.push(rule)
        return this
    }


    fun addContinuationRule(rule: pointer<Rule>) -> pointer<PrattParser>
    {
        this.continuationRules.push(rule)
        return this
    }


    fun sortByPriority() -> pointer<PrattParser>
    {
        this.starterRules.setComparator(compareRulePriority)
        this.starterRules.sort()
        this.continuationRules.setComparator(compareRulePriority)
        this.continuationRules.sort()
        return this
    }


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
    fun tryParse(
        token: pointer<TokenList>, cursor: int,
        matchLength: pointer<int>) -> pointer<ParseContainer> =
        this.tryParse(token, cursor, MIN_PRIORITY, matchLength)
        

    fun tryParse(
        token: pointer<TokenList>, cursor: int,
        minPriority: int,
        matchLength: pointer<int>) -> pointer<ParseContainer>
    {
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

    private fun tryParseStarter(
        token: pointer<TokenList>, cursor: int,
        matchLength: pointer<int>) -> pointer<ParseContainer>
    {
        var havePriorityLimit: bool = false
        var priorityLimit: int = 0

        matchLength.deref = 0

        while true:
        {
            var foundPriority: bool = false
            var currentPriority: int = MIN_PRIORITY

            for (var i = 0; i < this.starterRules.length; i++):
            {
                val rule: pointer<Rule> = this.starterRules.get(i) as pointer<Rule>

                if havePriorityLimit && rule.priority >= priorityLimit:
                    continue

                if !foundPriority || rule.priority > currentPriority:
                {
                    foundPriority = true
                    currentPriority = rule.priority
                }
            }

            if !foundPriority:
                return null

            var matchedRuleCount: int = 0
            var bestResult: pointer<ParseContainer> = null
            var bestMatchLength: int = 0

            for (var i = 0; i < this.starterRules.length; i++):
            {
                val rule: pointer<Rule> = this.starterRules.get(i) as pointer<Rule>

                if rule.priority != currentPriority:
                    continue

                val pattern: pointer<PatternList> = rule.getPattern()

                if pattern.length() > 0:
                {
                    val first: pointer<PatternAtom> = pattern.get(0)

                    if first.isRegex() && first.matchRegex(token, cursor) < 0:
                        continue
                }

                var currentMatchLength: int = 0
                val currentResult: pointer<ParseContainer> =
                    this.tryParseStarterRule(token, cursor, rule, 0, currentMatchLength.ref)

                if currentResult != null:
                {
                    matchedRuleCount++
                    bestResult = currentResult
                    bestMatchLength = currentMatchLength
                }
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
                this.result = bestResult
                matchLength.deref = bestMatchLength
                return bestResult
            }

            havePriorityLimit = true
            priorityLimit = currentPriority
        }

        return null
    }


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
                val refParser: pointer<ParserRef> = atom.getRefParser().clone()
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
                val refsParser: pointer<ParserRefs> = atom.getRefsParser().clone()
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

        var havePriorityLimit: bool = false
        var priorityLimit: int = 0

        while true:
        {
            var foundPriority: bool = false
            var currentPriority: int = MIN_PRIORITY

            for (var i = 0; i < this.continuationRules.length; i++):
            {
                val rule: pointer<Rule> = this.continuationRules.get(i) as pointer<Rule>
                val pattern: pointer<PatternList> = rule.getPattern()
                val first: pointer<PatternAtom> = pattern.get(0)

                if first == null || !first.isRef():
                    continue

                if first.getRefParser().getId() != left.getKind():
                    continue

                if havePriorityLimit && rule.priority >= priorityLimit:
                    continue

                if !foundPriority || rule.priority > currentPriority:
                {
                    foundPriority = true
                    currentPriority = rule.priority
                }
            }

            if !foundPriority:
                return null

            var matchedRuleCount: int = 0
            var bestRule: pointer<Rule> = null
            var bestResults: pointer<ArrayList> = null
            var bestMatchLength: int = 0

            for (var i = 0; i < this.continuationRules.length; i++):
            {
                val rule: pointer<Rule> = this.continuationRules.get(i) as pointer<Rule>

                if rule.priority != currentPriority:
                    continue

                val pattern: pointer<PatternList> = rule.getPattern()
                val first: pointer<PatternAtom> = pattern.get(0)

                if first == null || !first.isRef():
                    continue

                if first.getRefParser().getId() != left.getKind():
                    continue

                if pattern.length() > 1:
                {
                    val head: pointer<PatternAtom> = pattern.get(1)

                    if head.isRegex() && head.matchRegex(token, cursor) < 0:
                        continue
                }

                var currentMatchLength: int = 0
                val currentResults: pointer<ArrayList> = this.tryParseUntilSelfRef(
                    token, cursor,
                    rule, 1,
                    left,
                    currentMatchLength.ref)

                if currentResults != null:
                {
                    matchedRuleCount++
                    bestRule = rule
                    bestResults = currentResults
                    bestMatchLength = currentMatchLength
                }
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
                results.deref = bestResults
                matchLength.deref = bestMatchLength
                return bestRule
            }

            havePriorityLimit = true
            priorityLimit = currentPriority
        }

        return null
    }


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
                val refParser: pointer<ParserRef> = atom.getRefParser().clone()

                if refParser.getId() == leftId:
                {
                    matchLength.deref = consumed
                    return results
                }

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
                val refsParser: pointer<ParserRefs> = atom.getRefsParser().clone()
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


    fun clone() -> pointer<PrattParser>
    {
        val result: pointer<PrattParser> = new PrattParser(this.starterRules, this.continuationRules)
        result.id = this.id
        return result
    }
}
