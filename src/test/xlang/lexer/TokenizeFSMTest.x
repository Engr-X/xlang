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
 *
 *
 */

@file.class("TokenizeFSMTest")
package xlang.lexer

import xlang.lexer.LexInput
import xlang.lexer.LexPosition
import xlang.lexer.TokenizeFSM
import xlang.lexer.TokenizeRule
import xlang.lexer.Token
import xlang.test.TestGroup
import xlang.test.TestCase
import xlang.test.TestUnion
import xlang.util.string.String


private val TOKEN_KIND_A: int = 100
private val TOKEN_KIND_B: int = 101
private val OTHER_STATE: int = 7

val TEST_GROUP: pointer<TestGroup> = genTest()


fun genTest() -> pointer<TestGroup>
{
    val result: pointer<TestGroup> = new TestGroup("xlang.lexer.TokenizeFSM")
    val applyTC: pointer<TestCase> = new TestCase("apply", applyTest)
    val applyUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, applyTC, null)

    result.addTestUnion(applyUnion)

    return result
}


private fun applyTest() -> int
{
    val eofResult: int = eofDefaultTest()

    if eofResult != 0:
        return 100 + eofResult

    val noMatchResult: int = noMatchTest()

    if noMatchResult != 0:
        return 200 + noMatchResult

    val stateFilterResult: int = stateFilterTest()

    if stateFilterResult != 0:
        return 300 + stateFilterResult

    val firstRuleResult: int = firstRuleTest()

    if firstRuleResult != 0:
        return 400 + firstRuleResult

    return 0
}


private fun eatA(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token>
{
    dest.updateCursor(input.textLength)
    return new Token(TOKEN_KIND_A, input.pos.toTokenPosition(input.textLength), input.text)
}


private fun eatB(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token>
{
    dest.updateCursor(input.textLength)
    return new Token(TOKEN_KIND_B, input.pos.toTokenPosition(input.textLength), input.text)
}


private fun eatEOF(input: pointer<LexInput>, dest: pointer<TokenizeFSM>) -> pointer<Token> =
    new Token(Token.EOF_KIND, input.pos.toTokenPosition(input.textLength), "")


private fun eofDefaultTest() -> int
{
    val state: pointer<TokenizeFSM> = new TokenizeFSM("")
    val rulesSpace: blob[sizeof(pointer<TokenizeRule>) * 1]
    val rules: pointer<pointer<TokenizeRule>> = rulesSpace as pointer<pointer<TokenizeRule>>
    val rule0: pointer<TokenizeRule> = new TokenizeRule(0, TokenizeFSM.DEFAULT, "\\0", eatEOF)

    rules[0] = rule0

    val token: pointer<Token> = state.apply(rules, 1)

    if token == null:
        return 1

    if !token.isEOF():
        return 2

    if token.kind != Token.EOF_KIND:
        return 3

    if token.pos.offset != 0:
        return 4

    if token.pos.line != 1:
        return 5

    if token.pos.column != 1:
        return 6

    if token.pos.length != 0:
        return 7

    return 0
}


private fun noMatchTest() -> int
{
    val state: pointer<TokenizeFSM> = new TokenizeFSM("abc")
    val rulesSpace: blob[sizeof(pointer<TokenizeRule>) * 1]
    val rules: pointer<pointer<TokenizeRule>> = rulesSpace as pointer<pointer<TokenizeRule>>
    val rule0: pointer<TokenizeRule> = new TokenizeRule(0, TokenizeFSM.DEFAULT, "z", eatA)

    rules[0] = rule0

    val token: pointer<Token> = state.apply(rules, 1)

    if token != null:
        return 1

    val cursor: pointer<LexPosition> = state.getCursorPtr()

    if cursor.offset != 0:
        return 2

    if cursor.column != 1:
        return 3

    return 0
}


private fun stateFilterTest() -> int
{
    val state: pointer<TokenizeFSM> = new TokenizeFSM("abc")
    val rulesSpace: blob[sizeof(pointer<TokenizeRule>) * 2]
    val rules: pointer<pointer<TokenizeRule>> = rulesSpace as pointer<pointer<TokenizeRule>>
    val rule0: pointer<TokenizeRule> = new TokenizeRule(0, TokenizeFSM.DEFAULT, "a", eatA)
    val rule1: pointer<TokenizeRule> = new TokenizeRule(1, OTHER_STATE, "a", eatB)

    rules[0] = rule0
    rules[1] = rule1
    state.setState(OTHER_STATE)

    val token: pointer<Token> = state.apply(rules, 2)

    if token == null:
        return 1

    if token.kind != TOKEN_KIND_B:
        return 2

    if !String.streq(token.text, "a"):
        return 3

    val cursor: pointer<LexPosition> = state.getCursorPtr()

    if cursor.offset != 1:
        return 4

    return 0
}


private fun firstRuleTest() -> int
{
    val state: pointer<TokenizeFSM> = new TokenizeFSM("abc")
    val rulesSpace: blob[sizeof(pointer<TokenizeRule>) * 2]
    val rules: pointer<pointer<TokenizeRule>> = rulesSpace as pointer<pointer<TokenizeRule>>
    val rule0: pointer<TokenizeRule> = new TokenizeRule(0, TokenizeFSM.DEFAULT, "a", eatA)
    val rule1: pointer<TokenizeRule> = new TokenizeRule(1, TokenizeFSM.DEFAULT, "ab", eatB)

    rules[0] = rule0
    rules[1] = rule1

    val token: pointer<Token> = state.apply(rules, 2)

    if token == null:
        return 1

    if token.kind != TOKEN_KIND_A:
        return 2

    if !String.streq(token.text, "a"):
        return 3

    val cursor: pointer<LexPosition> = state.getCursorPtr()

    if cursor.offset != 1:
        return 4

    if cursor.column != 2:
        return 5

    return 0
}
