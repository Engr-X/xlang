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

@file.class("LexTest")
package xlang.lexer

import xlang.lexer.LexInput
import xlang.lexer.LexPosition
import xlang.lexer.LexState
import xlang.lexer.Rule
import xlang.lexer.Token
import xlang.test.TestGroup
import xlang.test.TestCase
import xlang.test.TestUnion
import xlang.util.string.String


private val TOKEN_KIND_A: int = 100
private val TOKEN_KIND_B: int = 101
private val OTHER_STATE: int = 7

val TEST_GROUP: TestGroup = genTest()


fun genTest() -> TestGroup
{
    val result: TestGroup = new TestGroup("xlang.lexer.LexState")
    val applyTC: pointer<TestCase> = new TestCase("apply", applyTest)
    val applyUnion: TestUnion = new TestUnion(TestCase.TYPE, applyTC, null)

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


private fun eatA(input: LexInput, dest: LexState) -> Token
{
    dest.updateCursor(input.textLength)
    return new Token(TOKEN_KIND_A, input.pos.toTokenPosition(input.textLength), input.text)
}


private fun eatB(input: LexInput, dest: LexState) -> Token
{
    dest.updateCursor(input.textLength)
    return new Token(TOKEN_KIND_B, input.pos.toTokenPosition(input.textLength), input.text)
}


private fun eofDefaultTest() -> int
{
    val state: LexState = LexState("")
    val token: Token = state.apply(null, 0)

    if token == null:
        return 1

    if !token.isEOF():
        return 2

    if token.kind != Token.EOF_KIND:
        return 3

    return 0
}


private fun noMatchTest() -> int
{
    val state: LexState = LexState("abc")
    val rulesSpace: blob[sizeof(pointer<Rule>) * 1]
    val rules: pointer<pointer<Rule>> = rulesSpace as pointer<pointer<Rule>>
    val rule0: Rule = Rule(0, LexState.DEFAULT, "z", eatA)

    rules[0] = rule0

    val token: Token = state.apply(rules, 1)

    if token != null:
        return 1

    val cursor: LexPosition = state.getCursorPtr()

    if cursor.offset != 0:
        return 2

    if cursor.column != 1:
        return 3

    return 0
}


private fun stateFilterTest() -> int
{
    val state: LexState = LexState("abc")
    val rulesSpace: blob[sizeof(pointer<Rule>) * 2]
    val rules: pointer<pointer<Rule>> = rulesSpace as pointer<pointer<Rule>>
    val rule0: Rule = Rule(0, LexState.DEFAULT, "a", eatA)
    val rule1: Rule = Rule(1, OTHER_STATE, "a", eatB)

    rules[0] = rule0
    rules[1] = rule1
    state.setState(OTHER_STATE)

    val token: Token = state.apply(rules, 2)

    if token == null:
        return 1

    if token.kind != TOKEN_KIND_B:
        return 2

    if !String.streq(token.text, "a"):
        return 3

    val cursor: LexPosition = state.getCursorPtr()

    if cursor.offset != 1:
        return 4

    return 0
}


private fun firstRuleTest() -> int
{
    val state: LexState = LexState("abc")
    val rulesSpace: blob[sizeof(pointer<Rule>) * 2]
    val rules: pointer<pointer<Rule>> = rulesSpace as pointer<pointer<Rule>>
    val rule0: Rule = Rule(0, LexState.DEFAULT, "a", eatA)
    val rule1: Rule = Rule(1, LexState.DEFAULT, "ab", eatB)

    rules[0] = rule0
    rules[1] = rule1

    val token: Token = state.apply(rules, 2)

    if token == null:
        return 1

    if token.kind != TOKEN_KIND_A:
        return 2

    if !String.streq(token.text, "a"):
        return 3

    val cursor: LexPosition = state.getCursorPtr()

    if cursor.offset != 1:
        return 4

    if cursor.column != 2:
        return 5

    return 0
}
