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
 */

@file.class("ParserTest")
package xlang.compiler.parser

import xlang.compiler.Type
import xlang.compiler.lexer.Tokenizer
import xlang.lexer.TokenList
import xlang.util.string.String
import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion


val TEST_GROUP: pointer<TestGroup> = genTest()


fun genTest() -> pointer<TestGroup>
{
    val result: pointer<TestGroup> = new TestGroup("xlang.compiler.parser.Parser")
    val atomParserTC: pointer<TestCase> = new TestCase("atomParser", atomParserTest)
    val functionCallExpressionTC: pointer<TestCase> = new TestCase("functionCallExpression", functionCallExpressionTest)
    val atomExpressionTC: pointer<TestCase> = new TestCase("atomExpression", atomExpressionTest)
    val parenthesizedExpressionTC: pointer<TestCase> = new TestCase("parenthesizedExpression", parenthesizedExpressionTest)
    val prefixExpressionTC: pointer<TestCase> = new TestCase("prefixExpression", prefixExpressionTest)
    val indexAccessExpressionTC: pointer<TestCase> = new TestCase("indexAccessExpression", indexAccessExpressionTest)
    val methodCallExpressionTC: pointer<TestCase> = new TestCase("methodCallExpression", methodCallExpressionTest)
    val fieldAccessExpressionTC: pointer<TestCase> = new TestCase("fieldAccessExpression", fieldAccessExpressionTest)
    val postfixExpressionTC: pointer<TestCase> = new TestCase("postfixExpression", postfixExpressionTest)
    val typeCastExpressionTC: pointer<TestCase> = new TestCase("typeCastExpression", typeCastExpressionTest)
    val infixExpressionTC: pointer<TestCase> = new TestCase("infixExpression", infixExpressionTest)
    val mixedExpressionTC: pointer<TestCase> = new TestCase("mixedExpression", mixedExpressionTest)
    val atomParserUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, atomParserTC, null)
    val functionCallExpressionUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, functionCallExpressionTC, null)
    val atomExpressionUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, atomExpressionTC, null)
    val parenthesizedExpressionUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, parenthesizedExpressionTC, null)
    val prefixExpressionUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, prefixExpressionTC, null)
    val indexAccessExpressionUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, indexAccessExpressionTC, null)
    val methodCallExpressionUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, methodCallExpressionTC, null)
    val fieldAccessExpressionUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, fieldAccessExpressionTC, null)
    val postfixExpressionUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, postfixExpressionTC, null)
    val typeCastExpressionUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, typeCastExpressionTC, null)
    val infixExpressionUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, infixExpressionTC, null)
    val mixedExpressionUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, mixedExpressionTC, null)

    result.addTestUnion(atomParserUnion)
    result.addTestUnion(functionCallExpressionUnion)
    result.addTestUnion(atomExpressionUnion)
    result.addTestUnion(parenthesizedExpressionUnion)
    result.addTestUnion(prefixExpressionUnion)
    result.addTestUnion(indexAccessExpressionUnion)
    result.addTestUnion(methodCallExpressionUnion)
    result.addTestUnion(fieldAccessExpressionUnion)
    result.addTestUnion(postfixExpressionUnion)
    result.addTestUnion(typeCastExpressionUnion)
    result.addTestUnion(infixExpressionUnion)
    result.addTestUnion(mixedExpressionUnion)

    return result
}


private fun parseExpressionText(text: pointer<char>) -> pointer<Expression>
{
    val tokens: pointer<TokenList> = Tokenizer.tokenize(text)
    return Parser.parseExpression(tokens)
}


private fun getRootMethodCall(expression: pointer<Expression>) -> pointer<MethodCall>
{
    if expression == null || expression.getKind() != Expression.METHOD_CALL_KIND:
        return null

    return expression.getRoot() as pointer<MethodCall>
}


private fun hasRootMethodCall(input: pointer<char>, callName: pointer<char>) -> bool
{
    val call: pointer<MethodCall> = getRootMethodCall(parseExpressionText(input))

    return call != null && String.streq(call.getCallName(), callName)
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


private fun functionCallExpressionTest() -> int
{
    val expression: pointer<Expression> = parseExpressionText("f(1, 2 + 3)")
    val call: pointer<MethodCall> = getRootMethodCall(expression)

    if call == null:
        return 1

    if call.getHost() != null:
        return 2

    if String.streq(call.getCallName(), "f") == false:
        return 3

    if call.argumentsCount() != 2:
        return 4

    val second: pointer<Expression> = call.getArgument(1)

    if second == null || second.getKind() != Expression.METHOD_CALL_KIND:
        return 5

    return 0
}


private fun atomExpressionTest() -> int
{
    val expression: pointer<Expression> = parseExpressionText("x")

    if expression == null || expression.getKind() != Expression.ATOM_KIND:
        return 1

    return 0
}


private fun parenthesizedExpressionTest() -> int
{
    val expression: pointer<Expression> = parseExpressionText("(1 + 2) * (3 * 4)")
    val rootCall: pointer<MethodCall> = getRootMethodCall(expression)

    if rootCall == null || String.streq(rootCall.getCallName(), "times") == false:
        return 1

    if rootCall.argumentsCount() != 2:
        return 2

    val left: pointer<Expression> = rootCall.getArgument(0)
    val right: pointer<Expression> = rootCall.getArgument(1)

    if left == null || left.getKind() != Expression.METHOD_CALL_KIND:
        return 3

    if right == null || right.getKind() != Expression.METHOD_CALL_KIND:
        return 4

    val leftCall: pointer<MethodCall> = left.getRoot() as pointer<MethodCall>
    val rightCall: pointer<MethodCall> = right.getRoot() as pointer<MethodCall>

    if leftCall == null || String.streq(leftCall.getCallName(), "plus") == false:
        return 5

    if rightCall == null || String.streq(rightCall.getCallName(), "times") == false:
        return 6

    return 0
}


private fun prefixExpressionTest() -> int
{
    if !hasRootMethodCall("+x", "pos"):
        return 1

    if !hasRootMethodCall("-x", "neg"):
        return 2

    val incExpression: pointer<Expression> = parseExpressionText("++x + y")
    val incRoot: pointer<MethodCall> = getRootMethodCall(incExpression)

    if incRoot == null || String.streq(incRoot.getCallName(), "plus") == false:
        return 3

    val incLeft: pointer<Expression> = incRoot.getArgument(0)
    val incLeftCall: pointer<MethodCall> = getRootMethodCall(incLeft)

    if incLeftCall == null || String.streq(incLeftCall.getCallName(), "inc") == false:
        return 4

    val decExpression: pointer<Expression> = parseExpressionText("--x + y")
    val decRoot: pointer<MethodCall> = getRootMethodCall(decExpression)

    if decRoot == null || String.streq(decRoot.getCallName(), "plus") == false:
        return 5

    val decLeft: pointer<Expression> = decRoot.getArgument(0)
    val decLeftCall: pointer<MethodCall> = getRootMethodCall(decLeft)

    if decLeftCall == null || String.streq(decLeftCall.getCallName(), "dec") == false:
        return 6

    return 0
}


private fun indexAccessExpressionTest() -> int
{
    val expression: pointer<Expression> = parseExpressionText("arr[1, 2 + 3]")

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

    val nested: pointer<Expression> = parseExpressionText("function()[a + b]")

    if nested == null || nested.getKind() != Expression.INDEX_ACCESS_KIND:
        return 5

    return 0
}


private fun methodCallExpressionTest() -> int
{
    val expression: pointer<Expression> = parseExpressionText("a.func(1, 2 + 3)")
    val call: pointer<MethodCall> = getRootMethodCall(expression)

    if call == null:
        return 1

    if call.getHost() == null:
        return 2

    if String.streq(call.getCallName(), "func") == false:
        return 3

    if call.argumentsCount() != 2:
        return 4

    val second: pointer<Expression> = call.getArgument(1)

    if second == null || second.getKind() != Expression.METHOD_CALL_KIND:
        return 5

    return 0
}


private fun fieldAccessExpressionTest() -> int
{
    val expression: pointer<Expression> = parseExpressionText("a.b.c")

    if expression == null || expression.getKind() != Expression.FIELD_ACCESS_KIND:
        return 1

    val access: pointer<FieldAccess> = expression.getRoot() as pointer<FieldAccess>

    if access == null:
        return 2

    return 0
}


private fun postfixExpressionTest() -> int
{
    if !hasRootMethodCall("x++", "succ"):
        return 1

    if !hasRootMethodCall("x--", "pred"):
        return 2

    return 0
}


private fun typeCastExpressionTest() -> int
{
    val expression: pointer<Expression> = parseExpressionText("1 + 1 as double")
    val call: pointer<MethodCall> = getRootMethodCall(expression)

    if call == null || call.argumentsCount() != 2:
        return 1

    val castExpression: pointer<Expression> = call.getArgument(1)

    if castExpression == null || castExpression.getKind() != Expression.TYPE_CAST_KIND:
        return 2

    val cast: pointer<TypeCast> = castExpression.getRoot() as pointer<TypeCast>

    if cast == null || cast.getExpression() == null:
        return 3

    val targetType: pointer<Type> = cast.getTargetType()

    if targetType == null || !String.streq(targetType.getTypeName(), "double"):
        return 4

    val blobExpression: pointer<Expression> = parseExpressionText("1 as blob[64]")

    if blobExpression == null || blobExpression.getKind() != Expression.TYPE_CAST_KIND:
        return 5

    val blobCast: pointer<TypeCast> = blobExpression.getRoot() as pointer<TypeCast>

    if blobCast == null:
        return 6

    val blobType: pointer<Type> = blobCast.getTargetType()

    if blobType == null || !String.streq(blobType.getTypeName(), "blob"):
        return 7

    if blobType.getMemSize() != 64:
        return 8

    return 0
}


private fun infixExpressionTest() -> int
{
    if !hasRootMethodCall("1 ** 2", "pow"):
        return 1

    if !hasRootMethodCall("1 * 2", "times"):
        return 2

    if !hasRootMethodCall("1 / 2", "div"):
        return 3

    if !hasRootMethodCall("1 % 2", "rem"):
        return 4

    if !hasRootMethodCall("1 + 2", "plus"):
        return 5

    if !hasRootMethodCall("1 - 2", "minus"):
        return 6

    val priorityExpression: pointer<Expression> = parseExpressionText("1 + 2 * 3")
    val priorityCall: pointer<MethodCall> = getRootMethodCall(priorityExpression)

    if priorityCall == null || String.streq(priorityCall.getCallName(), "plus") == false:
        return 7

    val priorityRight: pointer<Expression> = priorityCall.getArgument(1)

    if priorityRight == null || priorityRight.getKind() != Expression.METHOD_CALL_KIND:
        return 8

    val priorityRightCall: pointer<MethodCall> = priorityRight.getRoot() as pointer<MethodCall>

    if priorityRightCall == null || String.streq(priorityRightCall.getCallName(), "times") == false:
        return 9

    val leftExpression: pointer<Expression> = parseExpressionText("1 + 2 + 3")
    val leftCall: pointer<MethodCall> = getRootMethodCall(leftExpression)

    if leftCall == null || String.streq(leftCall.getCallName(), "plus") == false:
        return 10

    val leftArgument: pointer<Expression> = leftCall.getArgument(0)

    if leftArgument == null || leftArgument.getKind() != Expression.METHOD_CALL_KIND:
        return 11

    val nestedLeftCall: pointer<MethodCall> = leftArgument.getRoot() as pointer<MethodCall>

    if nestedLeftCall == null || String.streq(nestedLeftCall.getCallName(), "plus") == false:
        return 12

    return 0
}
private fun mixedExpressionTest() -> int
{
    val expression: pointer<Expression> = parseExpressionText("f(1 + 2 * 3, a.b(4)[5 + -x]++, y as double) - z--")
    val rootCall: pointer<MethodCall> = getRootMethodCall(expression)

    if rootCall == null || String.streq(rootCall.getCallName(), "minus") == false:
        return 1

    if rootCall.argumentsCount() != 2:
        return 2

    val left: pointer<Expression> = rootCall.getArgument(0)
    val right: pointer<Expression> = rootCall.getArgument(1)

    if left == null || left.getKind() != Expression.METHOD_CALL_KIND:
        return 3

    if right == null || right.getKind() != Expression.METHOD_CALL_KIND:
        return 4

    val functionCall: pointer<MethodCall> = left.getRoot() as pointer<MethodCall>
    val postfixRight: pointer<MethodCall> = right.getRoot() as pointer<MethodCall>

    if functionCall == null || String.streq(functionCall.getCallName(), "f") == false:
        return 5

    if functionCall.argumentsCount() != 3:
        return 6

    if postfixRight == null || String.streq(postfixRight.getCallName(), "pred") == false:
        return 7

    val firstArg: pointer<Expression> = functionCall.getArgument(0)
    val secondArg: pointer<Expression> = functionCall.getArgument(1)
    val thirdArg: pointer<Expression> = functionCall.getArgument(2)

    if firstArg == null || firstArg.getKind() != Expression.METHOD_CALL_KIND:
        return 8

    val firstCall: pointer<MethodCall> = firstArg.getRoot() as pointer<MethodCall>

    if firstCall == null || String.streq(firstCall.getCallName(), "plus") == false:
        return 9

    if secondArg == null || secondArg.getKind() != Expression.METHOD_CALL_KIND:
        return 10

    val secondCall: pointer<MethodCall> = secondArg.getRoot() as pointer<MethodCall>

    if secondCall == null || String.streq(secondCall.getCallName(), "succ") == false:
        return 11

    val indexedArg: pointer<Expression> = secondCall.getArgument(0)

    if indexedArg == null || indexedArg.getKind() != Expression.INDEX_ACCESS_KIND:
        return 12

    if thirdArg == null || thirdArg.getKind() != Expression.TYPE_CAST_KIND:
        return 13

    return 0
}