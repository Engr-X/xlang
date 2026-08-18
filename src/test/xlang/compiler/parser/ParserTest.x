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
import xlang.util.string.String
import xlang.util.string.StringBuilder
import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion


val TEST_GROUP: pointer<TestGroup> = genTest()


fun genTest() -> pointer<TestGroup>
{
    val result: pointer<TestGroup> = new TestGroup("xlang.compiler.parser.Parser")
    val atomParserTC: pointer<TestCase> = new TestCase("atomParser", atomParserTest)
    val expressionParserTC: pointer<TestCase> = new TestCase("expressionParser", expressionParserTest)
    val leftAssociativeExpressionTC: pointer<TestCase> = new TestCase("leftAssociativeExpression", leftAssociativeExpressionTest)
    val parenthesizedExpressionTC: pointer<TestCase> = new TestCase("parenthesizedExpression", parenthesizedExpressionTest)
    val atomParserUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, atomParserTC, null)
    val expressionParserUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, expressionParserTC, null)
    val leftAssociativeExpressionUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, leftAssociativeExpressionTC, null)
    val parenthesizedExpressionUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, parenthesizedExpressionTC, null)

    result.addTestUnion(atomParserUnion)
    result.addTestUnion(expressionParserUnion)
    result.addTestUnion(leftAssociativeExpressionUnion)
    result.addTestUnion(parenthesizedExpressionUnion)

    return result
}


private fun atomParserTest() -> int
{
    val tokens: pointer<TokenList> = Tokenizer.tokenize("true")
    val originalLength: int = tokens.length()
    val atom: pointer<Atom> = Parser.parseAtom(tokens)

    if atom == null:
        return 1

    if tokens.length() != originalLength - 1:
        return 2

    return 0
}

private fun expressionParserTest() -> int
{
    val tokens: pointer<TokenList> = Tokenizer.tokenize("1 + 2 * 3")
    val originalLength: int = tokens.length()
    val expression: pointer<Expression> = Parser.parseExpression(tokens)

    if expression == null:
        return 1

    if tokens.length() != originalLength - 5:
        return 2

    return 0
}

private fun leftAssociativeExpressionTest() -> int
{
    val tokens: pointer<TokenList> = Tokenizer.tokenize("1 + 2 + 3")
    val expression: pointer<Expression> = Parser.parseExpression(tokens)

    if expression == null:
        return 1

    if expression.getKind() != Expression.METHOD_CALL_KIND:
        return 2

    val rootCall: pointer<MethodCall> = expression.getRoot() as pointer<MethodCall>

    if rootCall == null:
        return 3

    if String.streq(rootCall.getCallName(), "plus") == false:
        return 4

    if rootCall.getHost() != null:
        return 5

    if rootCall.argumentsCount() != 2:
        return 6

    val left: pointer<Expression> = rootCall.getArgument(0)

    if left == null || left.getKind() != Expression.METHOD_CALL_KIND:
        return 7

    val leftCall: pointer<MethodCall> = left.getRoot() as pointer<MethodCall>

    if leftCall == null:
        return 8

    if String.streq(leftCall.getCallName(), "plus") == false:
        return 9

    val right: pointer<Expression> = rootCall.getArgument(1)

    if right == null || right.getKind() != Expression.ATOM_KIND:
        return 10

    return 0
}
private fun parenthesizedExpressionTest() -> int
{
    val tokens: pointer<TokenList> = Tokenizer.tokenize("(1 + 2) * (3 * 4)")
    val expression: pointer<Expression> = Parser.parseExpression(tokens)

    if expression == null || expression.getKind() != Expression.METHOD_CALL_KIND:
        return 1

    val rootCall: pointer<MethodCall> = expression.getRoot() as pointer<MethodCall>

    if rootCall == null || String.streq(rootCall.getCallName(), "times") == false:
        return 2

    if rootCall.argumentsCount() != 2:
        return 3

    val left: pointer<Expression> = rootCall.getArgument(0)
    val right: pointer<Expression> = rootCall.getArgument(1)

    if left == null || left.getKind() != Expression.METHOD_CALL_KIND:
        return 4

    if right == null || right.getKind() != Expression.METHOD_CALL_KIND:
        return 5

    val leftCall: pointer<MethodCall> = left.getRoot() as pointer<MethodCall>
    val rightCall: pointer<MethodCall> = right.getRoot() as pointer<MethodCall>

    if leftCall == null || String.streq(leftCall.getCallName(), "plus") == false:
        return 6

    if rightCall == null || String.streq(rightCall.getCallName(), "times") == false:
        return 7

    return 0
}
