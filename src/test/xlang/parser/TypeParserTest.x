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

@file.class("TypeParserTest")
package xlang.parser

import xlang.compiler.Type
import xlang.compiler.lexer.Tokenizer
import xlang.lexer.TokenList
import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion


val TEST_GROUP: pointer<TestGroup> = genTest()


fun genTest() -> pointer<TestGroup>
{
    val result: pointer<TestGroup> = new TestGroup("xlang.parser.TypeParser")
    val pointerVoidTC: pointer<TestCase> = new TestCase("pointerVoid", pointerVoidTest)
    val topLevelStarTC: pointer<TestCase> = new TestCase("topLevelStar", topLevelStarTest)
    val pointerVoidUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, pointerVoidTC, null)
    val topLevelStarUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, topLevelStarTC, null)

    result.addTestUnion(pointerVoidUnion)
    result.addTestUnion(topLevelStarUnion)

    return result
}


private fun pointerVoidTest() -> int
{
    val tokens: pointer<TokenList> = Tokenizer.tokenize("pointer<*>")
    val parser: pointer<TypeParser> = new TypeParser(1)
    val consumed: int = parser.parse(tokens, 0)

    if parser.haveError(consumed):
        return 1

    if consumed != 4:
        return 2

    val container: pointer<ParseContainer> = parser.getResult()

    if container == null || !container.isKind(1):
        return 3

    val parsedType: pointer<Type> = container.getValue() as pointer<Type>

    if parsedType == null:
        return 4

    if parsedType.length != 1:
        return 5

    val typeArgument: pointer<Type> = parsedType.getTypeArgument(0)

    if typeArgument == null:
        return 6

    if !typeArgument.equals(Type.voidType()):
        return 7

    return 0
}


private fun topLevelStarTest() -> int
{
    val tokens: pointer<TokenList> = Tokenizer.tokenize("*")
    val parser: pointer<TypeParser> = new TypeParser(1)
    val consumed: int = parser.parse(tokens, 0)

    if consumed >= 0:
        return 1

    if parser.getError() == null:
        return 2

    if parser.getResult() != null:
        return 3

    return 0
}
