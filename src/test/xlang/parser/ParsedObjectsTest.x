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

@file.class("ParsedObjectsTest")
package xlang.parser

import xlang.lexer.PatternList
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.lexer.TokenPosition
import xlang.parser.ParsedObject
import xlang.parser.ParsedObjects
import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion
import xlang.util.ArrayList
import xlang.util.string.String


private val TOKEN_KIND_A: int = 300
private val TOKEN_KIND_B: int = 301
private val TOKEN_KIND_C: int = 302

val TEST_GROUP: pointer<TestGroup> = genTest()


fun genTest() -> pointer<TestGroup>
{
    val result: pointer<TestGroup> = new TestGroup("xlang.parser.ParsedObjects")
    val doParseTC: pointer<TestCase> = new TestCase("doParse", doParseTest)
    val doParseUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, doParseTC, null)
    val getResultTC: pointer<TestCase> = new TestCase("getResult", getResultTest)
    val getResultUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, getResultTC, null)

    result.addTestUnion(doParseUnion)
    result.addTestUnion(getResultUnion)

    return result
}


private fun resultConstructor(tokens: pointer<TokenList>) -> pointer<*> =
    tokens as pointer<*>


private fun newTestToken(kind: int, text: pointer<char>, offset: int) -> pointer<Token> =
    new Token(kind, new TokenPosition(offset, 1, offset + 1, String.strlen(text)), text)


private fun pushTestToken(list: pointer<TokenList>, kind: int, text: pointer<char>, offset: int)
{
    val token: pointer<Token> = newTestToken(kind, text, offset)
    list.push(token)
}


private fun makeABCList() -> pointer<TokenList>
{
    val list: pointer<TokenList> = new TokenList("ParsedObjectsTest.x")

    pushTestToken(list, TOKEN_KIND_A, "a", 0)
    pushTestToken(list, TOKEN_KIND_B, "b", 1)
    pushTestToken(list, TOKEN_KIND_C, "c", 2)

    return list
}


private fun addABRules(parser: pointer<ParsedObjects>)
{
    val ruleA: pointer<PatternList> = new PatternList()
    val ruleB: pointer<PatternList> = new PatternList()

    ruleA.push(TOKEN_KIND_A, "a")
    ruleB.push(TOKEN_KIND_B, "b")

    parser.addRule(ruleA)
    parser.addRule(ruleB)
}


private fun resultAt(results: pointer<ArrayList>, index: int) -> pointer<TokenList>
{
    val slot: pointer<pointer<*>> = results.get(index) as pointer<pointer<*>>

    if slot == null:
        return null

    return slot.deref as pointer<TokenList>
}


private fun doParseTest() -> int
{
    val input: pointer<TokenList> = makeABCList()
    val objectParser: pointer<ParsedObject> = new ParsedObject(resultConstructor)
    val parser: pointer<ParsedObjects> = new ParsedObjects(objectParser)

    addABRules(parser)

    if parser.doParse(input) != 2:
        return 1

    if parser.length() != 2:
        return 2

    if input.length() != 1:
        return 3

    val token: pointer<Token> = input.get(0)

    if token.kind != TOKEN_KIND_C:
        return 4

    return 0
}


private fun getResultTest() -> int
{
    val input: pointer<TokenList> = makeABCList()
    val objectParser: pointer<ParsedObject> = new ParsedObject(resultConstructor)
    val parser: pointer<ParsedObjects> = new ParsedObjects(objectParser)

    addABRules(parser)

    if parser.doParse(input) != 2:
        return 1

    val results: pointer<ArrayList> = parser.getResult()

    if results == null:
        return 2

    if results.length != 2:
        return 3

    val resultA: pointer<TokenList> = resultAt(results, 0)
    val resultB: pointer<TokenList> = resultAt(results, 1)

    if resultA == null || resultB == null:
        return 4

    if resultA.length() != 1:
        return 5

    if resultB.length() != 1:
        return 6

    val tokenA: pointer<Token> = resultA.get(0)
    val tokenB: pointer<Token> = resultB.get(0)

    if tokenA.kind != TOKEN_KIND_A:
        return 7

    if tokenB.kind != TOKEN_KIND_B:
        return 8

    return 0
}
