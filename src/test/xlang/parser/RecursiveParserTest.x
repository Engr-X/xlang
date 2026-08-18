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

@file.class("RecursiveParserTest")
package xlang.parser

import xlang.Diagnostic
import xlang.compiler.lexer.Tokenizer
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.parser.util.PatternList
import xlang.parser.util.Rule
import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion
import xlang.util.ArrayList
import xlang.util.string.String


val TEST_GROUP: pointer<TestGroup> = genTest()


fun genTest() -> pointer<TestGroup>
{
    val result: pointer<TestGroup> = new TestGroup("xlang.parser.RecursiveParser")
    val resultLifetimeTC: pointer<TestCase> = new TestCase("resultLifetime", resultLifetimeTest)
    val resultLifetimeUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, resultLifetimeTC, null)
    val constructFailureTC: pointer<TestCase> = new TestCase("constructFailure", constructFailureTest)
    val constructFailureUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, constructFailureTC, null)

    result.addTestUnion(resultLifetimeUnion)
    result.addTestUnion(constructFailureUnion)

    return result
}


private fun makeTokenResult(results: pointer<ArrayList>) -> pointer<*>
{
    val slot: pointer<pointer<*>> = results.get(0) as pointer<pointer<*>>
    return slot.deref
}


private fun makeNullResult(results: pointer<ArrayList>) -> pointer<*> = null


private fun resultLifetimeTest() -> int
{
    val pattern: pointer<PatternList> = new PatternList().pushRegex(Tokenizer.KW_TRUE)
    val rule: pointer<Rule> = new Rule(pattern, makeTokenResult, Rule.STARTER_ROLE, 0)
    val recursiveParser: pointer<RecursiveParser> = new RecursiveParser(1).addRule(rule)
    val tokens: pointer<TokenList> = Tokenizer.tokenize("true")

    if recursiveParser.doParse(tokens) != 1:
        return 1

    val result: pointer<ParseContainer> = recursiveParser.getResult()

    if result == null || !result.isKind(1):
        return 2

    val token: pointer<Token> = result.getValue() as pointer<Token>

    if token == null:
        return 3

    if token.kind != Tokenizer.KW_TRUE:
        return 4

    if !String.streq(token.text, "true"):
        return 5

    return 0
}


private fun constructFailureTest() -> int
{
    val pattern: pointer<PatternList> = new PatternList().pushRegex(Tokenizer.KW_TRUE)
    val rule: pointer<Rule> = new Rule(pattern, makeNullResult, Rule.STARTER_ROLE, 0)
    val recursiveParser: pointer<RecursiveParser> = new RecursiveParser(1).addRule(rule)
    val tokens: pointer<TokenList> = Tokenizer.tokenize("true")
    val originalLength: int = tokens.length()

    if recursiveParser.doParse(tokens) != -1:
        return 1

    if tokens.length() != originalLength:
        return 2

    if recursiveParser.getError() == null:
        return 3

    if recursiveParser.getResult() != null:
        return 4

    val error: pointer<Diagnostic> = recursiveParser.getError()

    if error == null || !error.isInternalError():
        return 5

    if error.code != Diagnostic.CANNOT_CONSTRUCT_AST:
        return 6

    if !String.streq(error.message, Diagnostic.CANNOT_CONSTRUCT_AST_MSG):
        return 7

    return 0
}
