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

@file.class("NormalizeFSMTest")
package xlang.lexer

import xlang.lexer.NormalizeReceiver
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.lexer.TokenPosition
import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion
import xlang.util.ArrayList
import xlang.util.string.String


private val TOKEN_KIND_A: int = 100
private val TOKEN_KIND_B: int = 101
private val TOKEN_KIND_C: int = 102
private val TOKEN_KIND_D: int = 103
private val TOKEN_KIND_E: int = 104

private var ACTION_INDEX: int = -1

val TEST_GROUP: pointer<TestGroup> = genTest()


fun genTest() -> pointer<TestGroup>
{
    val result: pointer<TestGroup> = new TestGroup("xlang.lexer.NormalizeFSM")
    val receiverApplyTC: pointer<TestCase> = new TestCase("receiverApply", receiverApplyTest)
    val ruleWindowTC: pointer<TestCase> = new TestCase("ruleWindow", ruleWindowTest)
    val anyKindTC: pointer<TestCase> = new TestCase("anyKind", anyKindTest)
    val currentIndexTC: pointer<TestCase> = new TestCase("currentIndex", currentIndexTest)
    val actionFallthroughTC: pointer<TestCase> = new TestCase("actionFallthrough", actionFallthroughTest)
    val receiverApplyUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, receiverApplyTC, null)
    val ruleWindowUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, ruleWindowTC, null)
    val anyKindUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, anyKindTC, null)
    val currentIndexUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, currentIndexTC, null)
    val actionFallthroughUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, actionFallthroughTC, null)

    result.addTestUnion(receiverApplyUnion)
    result.addTestUnion(ruleWindowUnion)
    result.addTestUnion(anyKindUnion)
    result.addTestUnion(currentIndexUnion)
    result.addTestUnion(actionFallthroughUnion)

    return result
}


private fun receiverApplyTest() -> int
{
    var result: int = lengthMismatchTest()

    if result != 0:
        return 100 + result

    result = noChangeTest()

    if result != 0:
        return 200 + result

    result = editChangeTest()

    if result != 0:
        return 300 + result

    return 0
}


private fun newTestToken(kind: int, text: pointer<char>, offset: int) -> pointer<Token> =
    new Token(kind, new TokenPosition(offset, 1, offset + 1, String.strlen(text)), text)


private fun testNormalizeAction(fsm: pointer<NormalizeFSM>, tokens: pointer<ArrayList>) -> bool =
    false


private fun recordCurrentIndexAction(fsm: pointer<NormalizeFSM>, tokens: pointer<ArrayList>) -> bool
{
    ACTION_INDEX = fsm.getCurrentIndex()
    return true
}


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


private fun checkToken(list: pointer<TokenList>, index: int, kind: int, text: pointer<char>) -> int
{
    val token: pointer<Token> = list.get(index)

    if token == null:
        return 1

    if token.kind != kind:
        return 2

    if !String.streq(token.text, text):
        return 3

    return 0
}


private fun currentIndexTest() -> int
{
    ACTION_INDEX = -1

    val list: pointer<TokenList> = makeABCList()
    val fsm: pointer<NormalizeFSM> = new NormalizeFSM(list)
    val rulesSpace: blob[sizeof(pointer<NormalizeRule>) * 1]
    val rules: pointer<pointer<NormalizeRule>> = rulesSpace as pointer<pointer<NormalizeRule>>
    val rule: pointer<NormalizeRule> = new NormalizeRule(
        1,
        NormalizeFSM.DEFAULT,
        recordCurrentIndexAction)

    rule.addPattern(TOKEN_KIND_A, "a")
    rule.addPattern(TOKEN_KIND_B, "b")
    rule.addPattern(TOKEN_KIND_C, "c")
    rule.setPivot(1)

    rules[0] = rule

    val result: pointer<TokenList> = fsm.apply(rules, 1)

    if result == null:
        return 1

    if ACTION_INDEX != 1:
        return 2

    if fsm.getPtr() != 1:
        return 3

    return 0
}


private fun actionFallthroughTest() -> int
{
    ACTION_INDEX = -1

    val list: pointer<TokenList> = makeABCList()
    val fsm: pointer<NormalizeFSM> = new NormalizeFSM(list)
    val rulesSpace: blob[sizeof(pointer<NormalizeRule>) * 2]
    val rules: pointer<pointer<NormalizeRule>> = rulesSpace as pointer<pointer<NormalizeRule>>
    val falseRule: pointer<NormalizeRule> = new NormalizeRule(
        1,
        NormalizeFSM.DEFAULT,
        testNormalizeAction)
    val trueRule: pointer<NormalizeRule> = new NormalizeRule(
        2,
        NormalizeFSM.DEFAULT,
        recordCurrentIndexAction)

    falseRule.addPattern(TOKEN_KIND_B, "b")
    falseRule.setPivot(0)

    trueRule.addPattern(TOKEN_KIND_B, "b")
    trueRule.setPivot(0)

    rules[0] = falseRule
    rules[1] = trueRule

    val result: pointer<TokenList> = fsm.apply(rules, 2)

    if result == null:
        return 1

    if ACTION_INDEX != 1:
        return 2

    if fsm.getPtr() != 1:
        return 3

    return 0
}


private fun checkArrayToken(list: pointer<ArrayList>, index: int, kind: int, text: pointer<char>) -> int
{
    val token: pointer<Token> = list.get(index) as pointer<Token>

    if token == null:
        return 1

    if token.kind != kind:
        return 2

    if !String.streq(token.text, text):
        return 3

    return 0
}


private fun lengthMismatchTest() -> int
{
    val list: pointer<TokenList> = makeABCList()
    val receiver: pointer<NormalizeReceiver> = new NormalizeReceiver(2)
    val result: pointer<TokenList> = receiver.apply(list)

    if result != null:
        return 1

    return 0
}


private fun noChangeTest() -> int
{
    val list: pointer<TokenList> = makeABCList()
    val receiver: pointer<NormalizeReceiver> = new NormalizeReceiver(list.length())
    val result: pointer<TokenList> = receiver.apply(list)

    if result == null:
        return 1

    if result.length() != 3:
        return 2

    if checkToken(result, 0, TOKEN_KIND_A, "a") != 0:
        return 3

    if checkToken(result, 1, TOKEN_KIND_B, "b") != 0:
        return 4

    if checkToken(result, 2, TOKEN_KIND_C, "c") != 0:
        return 5

    return 0
}


private fun editChangeTest() -> int
{
    val list: pointer<TokenList> = makeABCList()
    val receiver: pointer<NormalizeReceiver> = new NormalizeReceiver(list.length())
    val inserted: pointer<Token> = newTestToken(TOKEN_KIND_D, "d", 3)
    val replacement: pointer<Token> = newTestToken(TOKEN_KIND_E, "e", 4)

    receiver.insertAt(0, inserted)
    receiver.deleteAt(1)
    receiver.insertAt(2, replacement)
    receiver.deleteAt(2)

    val result: pointer<TokenList> = receiver.apply(list)

    if result == null:
        return 1

    if result.length() != 3:
        return 2

    if checkToken(result, 0, TOKEN_KIND_D, "d") != 0:
        return 3

    if checkToken(result, 1, TOKEN_KIND_A, "a") != 0:
        return 4

    if checkToken(result, 2, TOKEN_KIND_E, "e") != 0:
        return 5

    return 0
}


private fun ruleWindowTest() -> int
{
    val list: pointer<TokenList> = makeABCList()
    val tokenArray: pointer<ArrayList> = list.toArray()
    val rule: pointer<NormalizeRule> = new NormalizeRule(
        1,
        NormalizeFSM.DEFAULT,
        testNormalizeAction)

    rule.addPattern(TOKEN_KIND_A, "a")
    rule.addPattern(TOKEN_KIND_B, "b")
    rule.addPattern(TOKEN_KIND_C, "c")
    rule.setPivot(1)

    if !rule.match(tokenArray, 1):
        return 1

    if rule.match(tokenArray, 2):
        return 2

    val window: pointer<ArrayList> = NormalizeFSM.window(rule, tokenArray, 1)

    if window == null:
        return 3

    if window.length != 3:
        return 4

    if checkArrayToken(window, 0, TOKEN_KIND_A, "a") != 0:
        return 5

    if checkArrayToken(window, 1, TOKEN_KIND_B, "b") != 0:
        return 6

    if checkArrayToken(window, 2, TOKEN_KIND_C, "c") != 0:
        return 7

    return 0
}


private fun anyKindTest() -> int
{
    val list: pointer<TokenList> = makeABCList()
    val tokenArray: pointer<ArrayList> = list.toArray()
    val exactRule: pointer<NormalizeRule> = new NormalizeRule(
        1,
        NormalizeFSM.DEFAULT,
        testNormalizeAction)
    val anyRule: pointer<NormalizeRule> = new NormalizeRule(
        2,
        NormalizeFSM.DEFAULT,
        testNormalizeAction)

    exactRule.addPattern(TOKEN_KIND_A, "b")
    exactRule.setPivot(0)

    if exactRule.match(tokenArray, 1):
        return 1

    anyRule.addPattern(Token.AnyKind, "b")
    anyRule.setPivot(0)

    if !anyRule.match(tokenArray, 1):
        return 2

    if anyRule.match(tokenArray, 0):
        return 3

    return 0
}
