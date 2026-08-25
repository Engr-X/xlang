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
 */

@file.class("PrattParserTest")
package xlang.parser

import xlang.compiler.lexer.Tokenizer
import xlang.parser.util.ParserRef
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.parser.util.PatternList
import xlang.parser.util.Rule
import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion
import xlang.util.ArrayList


val TEST_GROUP: pointer<TestGroup> = genTest()

private val EXPR_ID: int = 1


fun genTest() -> pointer<TestGroup>
{
    val result: pointer<TestGroup> = new TestGroup("xlang.parser.PrattParser")

    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("tryParse", tryParseTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("parse", parseTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("doParse", doParseEntryTest), null))

    return result
}


private fun parseTest() -> int
{
    val parser: pointer<PrattParser> = parserWithBinary()
    val tokens: pointer<TokenList> = Tokenizer.tokenize("1 + 2")
    val originalLength: int = tokens.length()
    val consumed: int = parser.parse(tokens, 0)

    if consumed != 3:
        return 1

    if tokens.length() != originalLength:
        return 2

    if parser.getResult() == null:
        return 3

    return 0
}


private fun doParseEntryTest() -> int
{
    val parser: pointer<PrattParser> = parserWithBinary()
    val tokens: pointer<TokenList> = Tokenizer.tokenize("1 + 2")
    val originalLength: int = tokens.length()
    val consumed: int = parser.doParse(tokens)

    if consumed != 3:
        return 1

    if tokens.length() != originalLength - consumed:
        return 2

    if parser.getResult() == null:
        return 3

    return 0
}

private fun makeExpr(results: pointer<ArrayList>) -> pointer<*> =
    results.clone() as pointer<*>


private fun atomRule() -> pointer<Rule> =
    new Rule(new PatternList().pushRegex(Tokenizer.TK_INTEGER), makeExpr, Rule.STARTER_ROLE, 0)


private fun atomParserRef() -> pointer<ParserRef> =
    ParserRef.fromRecursiveDown(EXPR_ID).addRule(atomRule())


private fun plusRule(priority: int) -> pointer<Rule> =
    new Rule(
        new PatternList()
            .pushRef(atomParserRef())
            .pushRegex(Tokenizer.PLUS)
            .pushRef(atomParserRef()),
        makeExpr, Rule.CONTINUATION_ROLE, priority)


private fun starRule(priority: int) -> pointer<Rule> =
    new Rule(
        new PatternList()
            .pushRef(atomParserRef())
            .pushRegex(Tokenizer.STAR)
            .pushRef(atomParserRef()),
        makeExpr, Rule.CONTINUATION_ROLE, priority)


private fun parserWithAtom() -> pointer<PrattParser> =
    new PrattParser().setId(EXPR_ID).addStarterRule(atomRule())


private fun parserWithBinary() -> pointer<PrattParser> =
    parserWithAtom().addContinuationRule(plusRule(10)).addContinuationRule(starRule(20))


private fun valueList(container: pointer<ParseContainer>) -> pointer<ArrayList> =
    container.getValue() as pointer<ArrayList>


private fun tokenAt(container: pointer<ParseContainer>, index: int) -> pointer<Token>
{
    val list: pointer<ArrayList> = valueList(container)
    val slot: pointer<pointer<*>> = list.get(index) as pointer<pointer<*>>
    return slot.deref as pointer<Token>
}


private fun containerAt(container: pointer<ParseContainer>, index: int) -> pointer<ParseContainer>
{
    val list: pointer<ArrayList> = valueList(container)
    val slot: pointer<pointer<*>> = list.get(index) as pointer<pointer<*>>
    return slot.deref as pointer<ParseContainer>
}


private fun tryParseTest() -> int
{
    var code: int = tryParseAtomTest()

    if code != 0:
        return code

    code = tryParsePlusTest()

    if code != 0:
        return 10 + code

    code = tryParsePriorityTest()

    if code != 0:
        return 20 + code

    code = tryParseMinPriorityTest()

    if code != 0:
        return 30 + code

    return 0
}


private fun tryParseAtomTest() -> int
{
    val parser: pointer<PrattParser> = parserWithAtom()
    val tokens: pointer<TokenList> = Tokenizer.tokenize("1")
    var length: int = 0
    val result: pointer<ParseContainer> = parser.tryParse(tokens, 0, length.ref)

    if result == null || length != 1:
        return 1

    if result.getKind() != EXPR_ID:
        return 2

    val token0: pointer<Token> = tokenAt(result, 0)

    if token0.kind != Tokenizer.TK_INTEGER:
        return 3

    return 0
}


private fun tryParsePlusTest() -> int
{
    val parser: pointer<PrattParser> = parserWithBinary()
    val tokens: pointer<TokenList> = Tokenizer.tokenize("1 + 2")
    var length: int = 0
    val result: pointer<ParseContainer> = parser.tryParse(tokens, 0, length.ref)

    if result == null || length != 3:
        return 1

    val resultList: pointer<ArrayList> = valueList(result)

    if resultList.length != 3:
        return 2

    if containerAt(result, 0).getKind() != EXPR_ID:
        return 3

    val plusToken: pointer<Token> = tokenAt(result, 1)

    if plusToken.kind != Tokenizer.PLUS:
        return 4

    if containerAt(result, 2).getKind() != EXPR_ID:
        return 5

    return 0
}


private fun tryParsePriorityTest() -> int
{
    val parser: pointer<PrattParser> = parserWithBinary()
    val tokens: pointer<TokenList> = Tokenizer.tokenize("1 + 2 * 3")
    var length: int = 0
    val result: pointer<ParseContainer> = parser.tryParse(tokens, 0, length.ref)

    if result == null || length != 5:
        return 1

    val plusToken: pointer<Token> = tokenAt(result, 1)

    if plusToken.kind != Tokenizer.PLUS:
        return 2

    val right: pointer<ParseContainer> = containerAt(result, 2)
    val starToken: pointer<Token> = tokenAt(right, 1)

    if starToken.kind != Tokenizer.STAR:
        return 3

    return 0
}


private fun tryParseMinPriorityTest() -> int
{
    val parser: pointer<PrattParser> = parserWithBinary()
    val tokens: pointer<TokenList> = Tokenizer.tokenize("1 + 2")
    var length: int = 0
    val result: pointer<ParseContainer> = parser.tryParse(tokens, 0, 11, length.ref)

    if result == null:
        return 1

    if length != 1:
        return 2

    val resultList: pointer<ArrayList> = valueList(result)

    if resultList.length != 1:
        return 3

    return 0
}
