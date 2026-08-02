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

@file.class("ParsedObjectTest")
package xlang.parser

import xlang.lexer.PatternList
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.lexer.TokenPosition
import xlang.parser.ParsedObject
import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion
import xlang.util.string.String


private val TOKEN_KIND_A: int = 200
private val TOKEN_KIND_B: int = 201
private val TOKEN_KIND_C: int = 202

val TEST_GROUP: pointer<TestGroup> = genTest()


fun genTest() -> pointer<TestGroup>
{
    val result: pointer<TestGroup> = new TestGroup("xlang.parser.ParsedObject")
    val eatTC: pointer<TestCase> = new TestCase("eat", eatTest)
    val eatUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, eatTC, null)
    val getResultTC: pointer<TestCase> = new TestCase("getResult", getResultTest)
    val getResultUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, getResultTC, null)

    result.addTestUnion(eatUnion)
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
    val list: pointer<TokenList> = new TokenList("ParsedObjectTest.x")

    pushTestToken(list, TOKEN_KIND_A, "a", 0)
    pushTestToken(list, TOKEN_KIND_B, "b", 1)
    pushTestToken(list, TOKEN_KIND_C, "c", 2)

    return list
}


private fun fullMatchEatTest() -> int
{
    val input: pointer<TokenList> = makeABCList()
    val rule: pointer<PatternList> = new PatternList()
    val parser: pointer<ParsedObject> = new ParsedObject(resultConstructor)

    rule.push(TOKEN_KIND_A, "a")
    rule.push(TOKEN_KIND_B, "b")
    parser.addRule(rule)

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


private fun partialMatchEatTest() -> int
{
    val input: pointer<TokenList> = makeABCList()
    val rule: pointer<PatternList> = new PatternList()
    val parser: pointer<ParsedObject> = new ParsedObject(resultConstructor)

    rule.push(TOKEN_KIND_A, "a")
    rule.push(TOKEN_KIND_B, "x")
    parser.addRule(rule)

    if parser.doParse(input) != -1:
        return 1

    if parser.length() != 0:
        return 2

    if input.length() != 3:
        return 3

    return 0
}


private fun eatTest() -> int
{
    var result: int = fullMatchEatTest()

    if result != 0:
        return 100 + result

    result = partialMatchEatTest()

    if result != 0:
        return 200 + result

    return 0
}


private fun getResultTest() -> int
{
    val input: pointer<TokenList> = makeABCList()
    val rule: pointer<PatternList> = new PatternList()
    val parser: pointer<ParsedObject> = new ParsedObject(resultConstructor)

    rule.push(TOKEN_KIND_A, "a")
    rule.push(TOKEN_KIND_B, "b")
    parser.addRule(rule)

    if parser.doParse(input) != 2:
        return 1

    val result: pointer<TokenList> = parser.getResult() as pointer<TokenList>

    if result == null:
        return 2

    if result.length() != 2:
        return 3

    val tokenA: pointer<Token> = result.get(0)
    val tokenB: pointer<Token> = result.get(1)

    if tokenA.kind != TOKEN_KIND_A:
        return 4

    if tokenB.kind != TOKEN_KIND_B:
        return 5

    return 0
}
