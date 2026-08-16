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
import xlang.lexer.PatternAtom
import xlang.lexer.PatternList
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.util.ArrayList


/**
 * Stores the result and intermediate state of one parser operation.
 *
 * A RecursiveParser contains an ordered set of token-pattern rules. During
 * parsing, those rules are tested against the beginning of an input TokenList.
 * The first complete match determines the token prefix consumed by this
 * object.
 *
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
    private var errors: pointer<ArrayList>


    private var results: pointer<ArrayList>


    private var result: pointer<*>


    private var consumedLength: int


    // maybe Token maybe Atom, Statement, ArrayList<Expression>, etc. very dangerous !!!
    var resultConstructor: (pointer<ArrayList>) -> pointer<*>

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
    fun __init__(resultConstructor: (pointer<ArrayList>) -> pointer<*>)
    {
        this.errors = new ArrayList(sizeof(Diagnostic))
        this.results = new ArrayList(sizeof(pointer<*>))
        this.result = null
        this.consumedLength = 0
        this.resultConstructor = resultConstructor
        this.rules = new ArrayList(sizeof(PatternList))
    }


    fun getRule(index: int) -> pointer<PatternList> = this.rules.get(index) as pointer<PatternList>


    fun ruleLength() -> int = this.rules.length


    fun addRule(rule: pointer<PatternList>) -> pointer<RecursiveParser>
    {
        this.rules.push(rule)
        return this
    }


    private fun pushResult(results: pointer<ArrayList>, item: pointer<*>) -> pointer<RecursiveParser>
    {
        results.push(item.ref)
        return this
    }


    private fun pushInternalError(code: int, message: pointer<char>) -> pointer<RecursiveParser>
    {
        this.errors.push(Diagnostic.makeInternalError(
            code,
            new ArrayList(sizeof(SourceLocation)),
            message))
        return this
    }


    fun getError(index: int) -> pointer<Diagnostic> =
        this.errors.get(index) as pointer<Diagnostic>


    fun getLastError() -> pointer<Diagnostic> =
        this.errors.peek() as pointer<Diagnostic>


    fun lastTrySuccess() -> bool
    {
        val last: pointer<Diagnostic> = this.getLastError()

        return last != null && last.level == Diagnostic.NORMAL_LEVEL
    }


    private fun tryParse(token: pointer<TokenList>, index: int, rules: pointer<ArrayList>) -> int
    {
        var maxConsumed: int = -1
        var maxConsumedIndex: int = -1

        for (var i = 0; i < rules.length; i++):
        {
            val rule: pointer<PatternList> = rules.get(i) as pointer<PatternList>
            val consumed: int = this.tryParse(token, index, rule)

            if this.lastTrySuccess():
                return consumed

            if consumed > maxConsumed:
            {
                maxConsumed = consumed
                maxConsumedIndex = i
            }
        }

        if maxConsumedIndex >= 0:
        {
            val error: pointer<Diagnostic> = this.getError(maxConsumedIndex)
            this.errors.push(error)
        }

        return maxConsumed
    }


    private fun tryParse(token: pointer<TokenList>, index: int, rule: pointer<PatternList>) -> int
    {
        var consumed: int = 0
        var success: bool = true
        val results: pointer<ArrayList> = new ArrayList(sizeof(pointer<*>))

        for (var i = 0; i < rule.length(); i++):
        {
            val atom: pointer<PatternAtom> = rule.get(i)

            if atom.isRegex():
            {
                val length: int = atom.matchRegex(token, index + consumed)

                if length < 0:
                {
                    var errorIndex: int = index + consumed

                    if errorIndex >= token.length():
                        errorIndex = token.length() - 1

                    val locations: pointer<ArrayList> = new ArrayList(sizeof(SourceLocation))
                    val errorToken: pointer<Token> = token.get(errorIndex)
                    val location: pointer<SourceLocation> = new SourceLocation(
                        token.filePath,
                        errorToken.pos.offset,
                        errorToken.pos.line,
                        errorToken.pos.column,
                        errorToken.pos.length)

                    locations.push(location)

                    this.errors.push(Diagnostic.makeError(
                        Diagnostic.UNEXPECTED_TOKEN,
                        locations,
                        Diagnostic.UNEXPECTED_TOKEN_MSG))
                    success = false
                    break
                }

                // add to result
                val matchedToken: pointer<Token> = token.get(index + consumed)
                val resultToken: pointer<Token> = matchedToken.copy()

                this.pushResult(results, resultToken as pointer<*>)
                consumed += length
            }
            elif atom.isRef():
            {
                val refParser: pointer<Parser> = atom.getRefParser()
                val innerConsumed: int = refParser.parse(token, index + consumed)

                if refParser.lastTrySuccess():
                {
                    consumed += innerConsumed

                    // add to result
                    this.pushResult(results, refParser.getResult())
                }
                else:
                {
                    val error: pointer<Diagnostic> = refParser.getLastError()
                    this.errors.push(error)
                    success = false
                    break
                }
            }
            elif atom.isRefs():
            {
                val refsParser: pointer<Parsers> = atom.getRefsParser()
                val innerConsumed: int = refsParser.parse(token, index + consumed)

                consumed += innerConsumed

                // add to result
                this.pushResult(results, refsParser.getResult())
            }
            else:
            {
                this.pushInternalError(
                    Diagnostic.INVALID_PATTERN_ATOM,
                    Diagnostic.INVALID_PATTERN_ATOM_MSG)
                success = false
                break
            }
        }

        if success:
        {
            this.result = this.resultConstructor(results)

            if this.result == null:
                this.pushInternalError(
                    Diagnostic.CANNOT_CONSTRUCT_AST,
                    Diagnostic.CANNOT_CONSTRUCT_AST_MSG)
            else:
            {
                this.results = results
                this.consumedLength = consumed
                this.errors.push(Diagnostic.makeNormal())
            }
        }
            
        return consumed
    }


    fun parse(token: pointer<TokenList>, index: int) -> int
    {
        this.errors = new ArrayList(sizeof(Diagnostic))
        this.results = new ArrayList(sizeof(pointer<*>))
        this.result = null
        this.consumedLength = 0

        if token == null:
        {
            this.pushInternalError(Diagnostic.NULL_INPUT, Diagnostic.NULL_INPUT_MSG)
            return -1
        }

        if token.length() <= 0 || index < 0 || index >= token.length():
        {
            this.pushInternalError(Diagnostic.EMPTY_INPUT, Diagnostic.EMPTY_INPUT_MSG)
            return -1
        }

        return this.tryParse(token, index, this.rules)
    }


    fun doParse(input: pointer<TokenList>) -> int
    {
        val index: int = this.parse(input, 0)

        if index <= 0 || !this.lastTrySuccess():
            return -1

        if index > input.length():
        {
            this.pushInternalError(0, "internal error: parser consumed more tokens than input length")
            return -1
        }

        input.remove(0, index)
        return index
    }


    fun getResult() -> pointer<*> = this.result
}
