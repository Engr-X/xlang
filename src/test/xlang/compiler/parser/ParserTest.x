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

@file.class("ParserTest")
package xlang.compiler.parser

import xlang.compiler.lexer.Tokenizer
import xlang.lexer.TokenList
import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion


val TEST_GROUP: pointer<TestGroup> = genTest()


fun genTest() -> pointer<TestGroup>
{
    val result: pointer<TestGroup> = new TestGroup("xlang.compiler.parser.Parser")
    val atomParserTC: pointer<TestCase> = new TestCase("atomParser", atomParserTest)
    val atomParserUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, atomParserTC, null)

    result.addTestUnion(atomParserUnion)

    return result
}


private fun atomParserTest() -> int
{
    val tokens: pointer<TokenList> = Tokenizer.tokenize("true")
    val originalLength: int = tokens.length()
    val consumed: int = Parser.atomParserParse(tokens, 0)
    val success: bool = Parser.atomParserLastTrySuccess()

    if consumed != 1:
        return 1

    if success == false:
        return 2

    if Parser.atomParserGetResult() == null:
        return 3

    if tokens.length() != originalLength:
        return 4

    if Parser.atomParserDoParse(tokens) != 1:
        return 5

    if tokens.length() != originalLength - 1:
        return 6

    if Parser.atomParserGetResult() == null:
        return 7

    return 0
}
