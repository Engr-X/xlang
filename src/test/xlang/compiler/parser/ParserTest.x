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

import xlang.compiler.Type
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
    val indexAccessExpressionTC: pointer<TestCase> = new TestCase("indexAccessExpression", indexAccessExpressionTest)
    val typeCastExpressionTC: pointer<TestCase> = new TestCase("typeCastExpression", typeCastExpressionTest)
    val blobTypeCastTC: pointer<TestCase> = new TestCase("blobTypeCast", blobTypeCastTest)
    val atomParserUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, atomParserTC, null)
    val expressionParserUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, expressionParserTC, null)
    val leftAssociativeExpressionUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, leftAssociativeExpressionTC, null)
    val parenthesizedExpressionUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, parenthesizedExpressionTC, null)
    val indexAccessExpressionUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, indexAccessExpressionTC, null)
    val typeCastExpressionUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, typeCastExpressionTC, null)
    val blobTypeCastUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, blobTypeCastTC, null)

    result.addTestUnion(atomParserUnion)
    result.addTestUnion(expressionParserUnion)
    result.addTestUnion(leftAssociativeExpressionUnion)
    result.addTestUnion(parenthesizedExpressionUnion)
    result.addTestUnion(indexAccessExpressionUnion)
    result.addTestUnion(typeCastExpressionUnion)
    result.addTestUnion(blobTypeCastUnion)

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

private fun indexAccessExpressionTest() -> int
{
    val tokens: pointer<TokenList> = Tokenizer.tokenize("arr[1, 2 + 3]")
    val expression: pointer<Expression> = Parser.parseExpression(tokens)

    if expression == null || expression.getKind() != Expression.INDEX_ACCESS_KIND:
        return 1

    val access: pointer<IndexAccess> = expression.getRoot() as pointer<IndexAccess>

    if access == null || access.getHost() == null:
        return 2

    if access.indicesCount() != 2:
        return 3

    val second: pointer<Expression> = access.getIndex(1)

    if second == null || second.getKind() != Expression.METHOD_CALL_KIND:
        return 4

    val functionTokens: pointer<TokenList> = Tokenizer.tokenize("function()[a + b]")
    val functionExpression: pointer<Expression> = Parser.parseExpression(functionTokens)

    if functionExpression == null || functionExpression.getKind() != Expression.INDEX_ACCESS_KIND:
        return 5

    val functionAccess: pointer<IndexAccess> = functionExpression.getRoot() as pointer<IndexAccess>

    if functionAccess == null || functionAccess.indicesCount() != 1:
        return 6

    val functionHost: pointer<Expression> = functionAccess.getHost()

    if functionHost == null || functionHost.getKind() != Expression.METHOD_CALL_KIND:
        return 7

    val functionIndex: pointer<Expression> = functionAccess.getIndex(0)

    if functionIndex == null || functionIndex.getKind() != Expression.METHOD_CALL_KIND:
        return 8

    val chainTokens: pointer<TokenList> = Tokenizer.tokenize("a.b.c[f.a][f.b]")
    val chainExpression: pointer<Expression> = Parser.parseExpression(chainTokens)

    if chainExpression == null || chainExpression.getKind() != Expression.INDEX_ACCESS_KIND:
        return 9

    val outerAccess: pointer<IndexAccess> = chainExpression.getRoot() as pointer<IndexAccess>

    if outerAccess == null || outerAccess.indicesCount() != 1:
        return 10

    val innerExpression: pointer<Expression> = outerAccess.getHost()

    if innerExpression == null || innerExpression.getKind() != Expression.INDEX_ACCESS_KIND:
        return 11

    val innerAccess: pointer<IndexAccess> = innerExpression.getRoot() as pointer<IndexAccess>

    if innerAccess == null || innerAccess.indicesCount() != 1:
        return 12

    if innerAccess.getHost() == null || innerAccess.getHost().getKind() != Expression.FIELD_ACCESS_KIND:
        return 13

    if innerAccess.getIndex(0) == null || innerAccess.getIndex(0).getKind() != Expression.FIELD_ACCESS_KIND:
        return 14

    if outerAccess.getIndex(0) == null || outerAccess.getIndex(0).getKind() != Expression.FIELD_ACCESS_KIND:
        return 15

    return 0
}

private fun typeCastExpressionTest() -> int
{
    val tokens: pointer<TokenList> = Tokenizer.tokenize("1 + 1 as double")
    val expression: pointer<Expression> = Parser.parseExpression(tokens)

    if expression == null || expression.getKind() != Expression.METHOD_CALL_KIND:
        return 1

    val call: pointer<MethodCall> = expression.getRoot() as pointer<MethodCall>

    if call == null || call.argumentsCount() != 2:
        return 2

    val castExpression: pointer<Expression> = call.getArgument(1)

    if castExpression == null || castExpression.getKind() != Expression.TYPE_CAST_KIND:
        return 3

    val cast: pointer<TypeCast> = castExpression.getRoot() as pointer<TypeCast>

    if cast == null || cast.getExpression() == null:
        return 4

    val targetType: pointer<Type> = cast.getTargetType()

    if targetType == null || !String.streq(targetType.getTypeName(), "double"):
        return 5

    return 0
}

private fun blobTypeCastTest() -> int
{
    val tokens: pointer<TokenList> = Tokenizer.tokenize("1 as blob[64]")
    val expression: pointer<Expression> = Parser.parseExpression(tokens)

    if expression == null || expression.getKind() != Expression.TYPE_CAST_KIND:
        return 1

    val cast: pointer<TypeCast> = expression.getRoot() as pointer<TypeCast>

    if cast == null || cast.getExpression() == null:
        return 2

    val targetType: pointer<Type> = cast.getTargetType()

    if targetType == null || !String.streq(targetType.getTypeName(), "blob"):
        return 3

    if targetType.getMemSize() != 64:
        return 4

    return 0
}
