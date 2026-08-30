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
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.util.ArrayList
import xlang.util.string.String
import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion


val TEST_GROUP: pointer<TestGroup> = genTest()


fun genTest() -> pointer<TestGroup>
{
    val result: pointer<TestGroup> = new TestGroup("xlang.parser.TypeParser")
    val pointerVoidTC: pointer<TestCase> = new TestCase("pointerVoid", pointerVoidTest)
    val topLevelStarTC: pointer<TestCase> = new TestCase("topLevelStar", topLevelStarTest)
    val normalParseTC: pointer<TestCase> = new TestCase("normalParse", normalParseTest)
    val emptyFunctionTC: pointer<TestCase> = new TestCase("emptyFunction", emptyFunctionTest)
    val functionParametersTC: pointer<TestCase> = new TestCase("functionParameters", functionParametersTest)
    val nestedFunctionTC: pointer<TestCase> = new TestCase("nestedFunction", nestedFunctionTest)
    val mixedFunctionTC: pointer<TestCase> = new TestCase("mixedFunction", mixedFunctionTest)
    val pointerVoidUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, pointerVoidTC, null)
    val topLevelStarUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, topLevelStarTC, null)
    val normalParseUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, normalParseTC, null)
    val emptyFunctionUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, emptyFunctionTC, null)
    val functionParametersUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, functionParametersTC, null)
    val nestedFunctionUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, nestedFunctionTC, null)
    val mixedFunctionUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, mixedFunctionTC, null)

    result.addTestUnion(pointerVoidUnion)
    result.addTestUnion(topLevelStarUnion)
    result.addTestUnion(normalParseUnion)
    result.addTestUnion(emptyFunctionUnion)
    result.addTestUnion(functionParametersUnion)
    result.addTestUnion(nestedFunctionUnion)
    result.addTestUnion(mixedFunctionUnion)

    return result
}


private fun tokenTextAt(tokens: pointer<ArrayList>, index: int, text: pointer<char>) -> bool
{
    if tokens == null || index < 0 || index >= tokens.length:
        return false

    val token: pointer<Token> = tokens.get(index) as pointer<Token>

    return token != null && String.streq(token.text, text)
}


private fun parseTypeTokens(input: pointer<char>, expectedConsumed: int) -> pointer<ArrayList>
{
    val tokens: pointer<TokenList> = Tokenizer.tokenize(input)
    val parser: pointer<TypeParser> = new TypeParser(1)
    val consumed: int = parser.parse(tokens, 0)

    if parser.haveError(consumed) || consumed != expectedConsumed:
        return null

    val container: pointer<ParseContainer> = parser.getResult()

    if container == null || !container.isKind(1):
        return null

    val parsedType: pointer<Type> = container.getValue() as pointer<Type>

    if parsedType == null:
        return null

    return parsedType.getAllTokens()
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

    val typeTokens: pointer<ArrayList> = parsedType.getAllTokens()

    if typeTokens == null || typeTokens.length != 4:
        return 5

    if !tokenTextAt(typeTokens, 0, "pointer") || !tokenTextAt(typeTokens, 1, "<"):
        return 6

    if !tokenTextAt(typeTokens, 2, "*") || !tokenTextAt(typeTokens, 3, ">"):
        return 7

    return 0
}


private fun normalParseTest() -> int
{
    val typeTokens: pointer<ArrayList> = parseTypeTokens("int", 1)

    if typeTokens == null || typeTokens.length != 1:
        return 1

    if !tokenTextAt(typeTokens, 0, "int"):
        return 2

    return 0
}


private fun emptyFunctionTest() -> int
{
    val typeTokens: pointer<ArrayList> = parseTypeTokens("() -> int", 4)

    if typeTokens == null || typeTokens.length != 4:
        return 1

    if !tokenTextAt(typeTokens, 0, "(") || !tokenTextAt(typeTokens, 1, ")"):
        return 2

    if !tokenTextAt(typeTokens, 2, "->") || !tokenTextAt(typeTokens, 3, "int"):
        return 3

    return 0
}


private fun functionParametersTest() -> int
{
    val typeTokens: pointer<ArrayList> = parseTypeTokens("(int, bool) -> void", 7)

    if typeTokens == null || typeTokens.length != 7:
        return 1

    if !tokenTextAt(typeTokens, 0, "(") || !tokenTextAt(typeTokens, 1, "int"):
        return 2

    if !tokenTextAt(typeTokens, 2, ",") || !tokenTextAt(typeTokens, 3, "bool"):
        return 3

    if !tokenTextAt(typeTokens, 4, ")") || !tokenTextAt(typeTokens, 5, "->"):
        return 4

    if !tokenTextAt(typeTokens, 6, "void"):
        return 5

    return 0
}


private fun nestedFunctionTest() -> int
{
    val typeTokens: pointer<ArrayList> = parseTypeTokens("((int) -> bool, pointer<*>) -> () -> void", 17)

    if typeTokens == null || typeTokens.length != 17:
        return 1

    if !tokenTextAt(typeTokens, 0, "(") || !tokenTextAt(typeTokens, 1, "("):
        return 2

    if !tokenTextAt(typeTokens, 2, "int") || !tokenTextAt(typeTokens, 3, ")"):
        return 3

    if !tokenTextAt(typeTokens, 4, "->") || !tokenTextAt(typeTokens, 5, "bool"):
        return 4

    if !tokenTextAt(typeTokens, 6, ",") || !tokenTextAt(typeTokens, 7, "pointer"):
        return 5

    if !tokenTextAt(typeTokens, 8, "<") || !tokenTextAt(typeTokens, 9, "*"):
        return 6

    if !tokenTextAt(typeTokens, 10, ">") || !tokenTextAt(typeTokens, 11, ")"):
        return 7

    if !tokenTextAt(typeTokens, 12, "->") || !tokenTextAt(typeTokens, 13, "("):
        return 8

    if !tokenTextAt(typeTokens, 14, ")") || !tokenTextAt(typeTokens, 15, "->"):
        return 9

    if !tokenTextAt(typeTokens, 16, "void"):
        return 10

    return 0
}


private fun mixedFunctionTest() -> int
{
    val typeTokens: pointer<ArrayList> = parseTypeTokens("(blob[64], () -> pointer<*>) -> pointer<blob[8]>", 22)

    if typeTokens == null || typeTokens.length != 22:
        return 1

    if !tokenTextAt(typeTokens, 0, "(") || !tokenTextAt(typeTokens, 1, "blob"):
        return 2

    if !tokenTextAt(typeTokens, 2, "[") || !tokenTextAt(typeTokens, 3, "64"):
        return 3

    if !tokenTextAt(typeTokens, 4, "]") || !tokenTextAt(typeTokens, 5, ","):
        return 4

    if !tokenTextAt(typeTokens, 6, "(") || !tokenTextAt(typeTokens, 7, ")"):
        return 5

    if !tokenTextAt(typeTokens, 8, "->") || !tokenTextAt(typeTokens, 9, "pointer"):
        return 6

    if !tokenTextAt(typeTokens, 10, "<") || !tokenTextAt(typeTokens, 11, "*"):
        return 7

    if !tokenTextAt(typeTokens, 12, ">") || !tokenTextAt(typeTokens, 13, ")"):
        return 8

    if !tokenTextAt(typeTokens, 14, "->") || !tokenTextAt(typeTokens, 15, "pointer"):
        return 9

    if !tokenTextAt(typeTokens, 16, "<") || !tokenTextAt(typeTokens, 17, "blob"):
        return 10

    if !tokenTextAt(typeTokens, 18, "[") || !tokenTextAt(typeTokens, 19, "8"):
        return 11

    if !tokenTextAt(typeTokens, 20, "]") || !tokenTextAt(typeTokens, 21, ">"):
        return 12

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
