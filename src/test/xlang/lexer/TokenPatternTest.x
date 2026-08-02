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

@file.class("TokenPatternTest")
package xlang.lexer

import xlang.lexer.PatternList
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.lexer.TokenPosition
import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion
import xlang.util.string.String


private val TOKEN_KIND_A: int = 100
private val TOKEN_KIND_B: int = 101
private val TOKEN_KIND_C: int = 102
private val TOKEN_KIND_D: int = 103

val TEST_GROUP: pointer<TestGroup> = genTest()


fun genTest() -> pointer<TestGroup>
{
    val result: pointer<TestGroup> = new TestGroup("xlang.lexer.PatternList")
    val maxMatchLengthTC: pointer<TestCase> = new TestCase("maxMatchLength", maxMatchLengthTest)
    val maxMatchLengthUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, maxMatchLengthTC, null)

    result.addTestUnion(maxMatchLengthUnion)

    return result
}


private fun newTestToken(kind: int, text: pointer<char>, offset: int) -> pointer<Token> =
    new Token(kind, new TokenPosition(offset, 1, offset + 1, String.strlen(text)), text)


private fun pushTestToken(list: pointer<TokenList>, kind: int, text: pointer<char>, offset: int)
{
    val token: pointer<Token> = newTestToken(kind, text, offset)
    list.push(token)
}


private fun makeABCList() -> pointer<TokenList>
{
    val list: pointer<TokenList> = new TokenList()

    pushTestToken(list, TOKEN_KIND_A, "a", 0)
    pushTestToken(list, TOKEN_KIND_B, "b", 1)
    pushTestToken(list, TOKEN_KIND_C, "c", 2)

    return list
}


private fun fullMatchTest(list: pointer<TokenList>) -> int
{
    val pattern: pointer<PatternList> = new PatternList()

    pattern.push(TOKEN_KIND_A, "a")
    pattern.push(TOKEN_KIND_B, "b")
    pattern.push(TOKEN_KIND_C, "c")

    if pattern.maxMatchLength(list, 0) != 3:
        return 1

    if pattern.maxMatchLength(list, 1) != 0:
        return 2

    if list.maxMatchLength(0, pattern) != 3:
        return 3

    if list.maxMatchLength(pattern) != 3:
        return 4

    if !list.canMatch(0, pattern):
        return 5

    if !list.canMatch(pattern):
        return 6

    if list.canMatch(1, pattern):
        return 7

    return 0
}


private fun partialMismatchTest(list: pointer<TokenList>) -> int
{
    val pattern: pointer<PatternList> = new PatternList()

    pattern.push(TOKEN_KIND_A, "a")
    pattern.push(TOKEN_KIND_B, "x")
    pattern.push(TOKEN_KIND_C, "c")

    if pattern.maxMatchLength(list, 0) != 1:
        return 1

    return 0
}


private fun inputEndTest(list: pointer<TokenList>) -> int
{
    val pattern: pointer<PatternList> = new PatternList()

    pattern.push(TOKEN_KIND_B, "b")
    pattern.push(TOKEN_KIND_C, "c")
    pattern.push(TOKEN_KIND_D, "d")

    if pattern.maxMatchLength(list, 1) != 2:
        return 1

    if pattern.maxMatchLength(list, list.length()) != 0:
        return 2

    return 0
}


private fun regexOnlyTest(list: pointer<TokenList>) -> int
{
    val pattern: pointer<PatternList> = new PatternList()

    pattern.push("[a-c]")

    if pattern.maxMatchLength(list, 2) != 1:
        return 1

    if pattern.maxMatchLength(list, -1) != 0:
        return 2

    return 0
}


private fun kindMismatchRegexMatchTest(list: pointer<TokenList>) -> int
{
    val exactPattern: pointer<PatternList> = new PatternList()
    val anyPattern: pointer<PatternList> = new PatternList()

    exactPattern.push(TOKEN_KIND_A, "b")

    if exactPattern.maxMatchLength(list, 1) != 0:
        return 1

    if list.canMatch(1, exactPattern):
        return 2

    anyPattern.push(Token.AnyKind, "b")

    if anyPattern.maxMatchLength(list, 1) != 1:
        return 3

    if !list.canMatch(1, anyPattern):
        return 4

    if anyPattern.maxMatchLength(list, 0) != 0:
        return 5

    return 0
}


private fun maxMatchLengthTest() -> int
{
    val list: pointer<TokenList> = makeABCList()
    var result: int = fullMatchTest(list)

    if result != 0:
        return 100 + result

    result = partialMismatchTest(list)

    if result != 0:
        return 200 + result

    result = inputEndTest(list)

    if result != 0:
        return 300 + result

    result = regexOnlyTest(list)

    if result != 0:
        return 400 + result

    result = kindMismatchRegexMatchTest(list)

    if result != 0:
        return 500 + result

    return 0
}
