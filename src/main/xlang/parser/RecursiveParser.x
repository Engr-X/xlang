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

@file.class("RecursiveParser")
package xlang.parser

import xlang.Diagnostic
import xlang.SourceLocation
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.parser.util.Parser
import xlang.parser.util.Parsers
import xlang.parser.util.PatternAtom
import xlang.parser.util.PatternList
import xlang.parser.util.Rule
import xlang.util.ArrayList


/**
 * Stores the result and intermediate state of one parser operation.
 *
 * A RecursiveParser contains an ordered set of token-pattern rules. During
 * parsing, those rules are tested against the beginning of an input TokenList.
 * The first complete match determines the token prefix consumed by this
 * object.
 *push
 * If no rule completely matches, the longest partial match is used to locate
 * the first unexpected token and produce a diagnostic.
 *
 * The parsed result list, parser rules and diagnostic objects are stored by
 * pointer. Token and PatternList objects are not deeply copied by this
 * structure.
 *
 * The caller must keep referenced rules and callback functions valid while
 * this RecursiveParser uses them.
 */
struct RecursiveParser
{
    private var id: int

    private var error: pointer<Diagnostic>


    private var result: pointer<ParseContainer>


    /**
     * Stores the token-pattern rules tested by eat.
     *
     * The ArrayList contains Rule values in insertion order. Each Rule keeps
     * a direct reference to its PatternList.
     */
    private var rules: pointer<ArrayList>


    /**
     * Initializes an empty parsed object with a result callback.
     *
     * A new empty result list and rule list are allocated. No diagnostic is
     * initially stored.
     *
     * The callback pointer is stored directly without validation.
     *
     * The caller must provide a valid callback and keep it available while
     * this RecursiveParser may invoke it.
     *
     * @param                   result the function that converts result items into a parser
     *                          result object.
     *
     * @warning                 A null or invalid callback causes undefined behavior if it is
     *                          invoked.
     */
    fun __init__(id: int)
    {
        this.id = id
        this.error = null
        this.result = null
        this.rules = new ArrayList(sizeof(Rule))
    }


    fun getId() -> int = this.id


    fun getRule(index: int) -> pointer<Rule> = this.rules.get(index) as pointer<Rule>


    fun ruleLength() -> int = this.rules.length


    fun addRule(rule: pointer<Rule>) -> pointer<RecursiveParser>
    {
        this.rules.push(rule)
        return this
    }


    private fun pushInternalError(code: int, message: pointer<char>) -> pointer<RecursiveParser>
    {
        this.error = Diagnostic.makeInternalError(
            code,
            new ArrayList(sizeof(SourceLocation)),
            message)
        return this
    }


    fun getError() -> pointer<Diagnostic> = this.error


    fun haveError(eaten: int) -> bool = this.error != null || eaten <= 0


    fun reset() -> pointer<RecursiveParser>
    {
        this.error = null
        this.result = null
        return this
    }


    private fun tryParse(token: pointer<TokenList>, cursor: int, rules: pointer<ArrayList>) -> int
    {
        var maxMatchLength: int = 0

        for (var i = 0; i < rules.length; i++):
        {
            val rule: pointer<Rule> = rules.get(i) as pointer<Rule>
            var matchLength: int = 0

            if this.tryParse(token, cursor, rule, 0, matchLength.ref):
                return matchLength

            if this.getError() != null:
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
            token.filePath,
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


    private fun tryParse(
        token: pointer<TokenList>, cursor: int,
        rule: pointer<Rule>, patternStart: int,
        matchLength: pointer<int>) -> bool
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
                    return false
                }

                // add to result
                val matchedToken: pointer<Token> = token.get(cursor + consumed)
                val resultToken: pointer<Token> = matchedToken.copy()
                val resultItem: pointer<*> = resultToken as pointer<*>

                results.push(resultItem.ref)
                consumed += length
            }
            elif atom.isRef():
            {
                val refParser: pointer<Parser> = atom.getRefParser()
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
                val refsParser: pointer<Parsers> = atom.getRefsParser()
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
        matchLength.deref = consumed

        return true
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

        return this.tryParse(token, cursor, this.rules)
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


    fun getResult() -> pointer<ParseContainer> = this.result
}
