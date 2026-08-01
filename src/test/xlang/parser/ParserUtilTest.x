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

@file.class("ParserUtilTest")
package xlang.parser

import xlang.Diagnostic
import xlang.compiler.lexer.Tokenizer
import xlang.lexer.TokenList
import xlang.parser.ParserUtil
import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion


val TEST_GROUP: pointer<TestGroup> = genTest()


fun genTest() -> pointer<TestGroup>
{
    val result: pointer<TestGroup> = new TestGroup("xlang.parser.ParserUtil")
    val checkBracketsTC: pointer<TestCase> = new TestCase("checkBrackets", checkBracketsTest)
    val checkBracketsUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, checkBracketsTC, null)

    result.addTestUnion(checkBracketsUnion)

    return result
}


private fun checkInvalidBrackets(input: pointer<char>) -> int
{
    val tokens: pointer<TokenList> = Tokenizer.tokenize(input)
    val diagnostic: pointer<Diagnostic> = ParserUtil.checkBrackets(tokens)

    if diagnostic == null:
        return 1

    return 0
}


private fun checkBracketsTest() -> int
{
    val validTokens: pointer<TokenList> = Tokenizer.tokenize("{ a(b[c]) }")
    val validDiagnostic: pointer<Diagnostic> = ParserUtil.checkBrackets(validTokens)

    if validDiagnostic != null:
        return 1

    if checkInvalidBrackets(")") != 0:
        return 2

    if checkInvalidBrackets("(]") != 0:
        return 3

    if checkInvalidBrackets("{") != 0:
        return 4

    return 0
}
