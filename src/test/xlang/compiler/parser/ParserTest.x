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
import xlang.System
import xlang.compiler.lexer.Tokenizer
import xlang.compiler.parser.expression.Atom
import xlang.compiler.parser.expression.Expression
import xlang.compiler.parser.expression.FieldAccess
import xlang.compiler.parser.expression.IndexAccess
import xlang.compiler.parser.expression.MethodCall
import xlang.compiler.parser.expression.TypeCast
import xlang.compiler.parser.statement.ExprListStatement
import xlang.compiler.parser.statement.ExprStatement
import xlang.compiler.parser.statement.ReturnStatement
import xlang.compiler.parser.statement.Statement
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.util.ArrayList
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
    val compoundOperatorExpressionTC: pointer<TestCase> = new TestCase("compoundOperatorExpression", compoundOperatorExpressionTest)
    val mixedExpressionTC: pointer<TestCase> = new TestCase("mixedExpression", mixedExpressionTest)
    val statementTC: pointer<TestCase> = new TestCase("statement", statementTest)
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
    val compoundOperatorExpressionUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, compoundOperatorExpressionTC, null)
    val mixedExpressionUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, mixedExpressionTC, null)
    val statementUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, statementTC, null)

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
    result.addTestUnion(compoundOperatorExpressionUnion)
    result.addTestUnion(mixedExpressionUnion)
    result.addTestUnion(statementUnion)

    return result
}


private fun parseExpressionText(text: pointer<char>) -> pointer<Expression>
{
    val tokens: pointer<TokenList> = Tokenizer.tokenize(text)
    return Parser.parseExpression(tokens)
}


private fun parseStatementText(text: pointer<char>) -> pointer<Statement>
{
    val tokens: pointer<TokenList> = Tokenizer.fullTokenize(text)
    return Parser.parseStatement(tokens)
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


private fun expressionTextEquals(input: pointer<char>, expected: pointer<char>) -> bool
{
    val expression: pointer<Expression> = parseExpressionText(input)

    if expression == null:
        return false

    val builder: pointer<StringBuilder> = expression.toString()
    val actual: pointer<char> = System.allocMemory((builder.length + 1) * sizeof(char)) as pointer<char>

    builder.toString(actual)
    return String.streq(actual, expected)
}


private fun tokenTextAt(tokens: pointer<ArrayList>, index: int, text: pointer<char>) -> bool
{
    if tokens == null || index < 0 || index >= tokens.length:
        return false

    val token: pointer<Token> = tokens.get(index) as pointer<Token>

    return token != null && String.streq(token.text, text)
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



private fun statementTest() -> int
{
    val exprStatement: pointer<Statement> = parseStatementText("x + 1;")

    if exprStatement == null || exprStatement.getKind() != Statement.EXPRESSION_TYPE:
        return 1

    val exprRoot: pointer<ExprStatement> = exprStatement.getRoot() as pointer<ExprStatement>

    if exprRoot == null || exprRoot.getExpression() == null:
        return 2

    val exprTokens: pointer<ArrayList> = exprStatement.getAllTokens()

    if exprTokens == null || exprTokens.length != 3:
        return 3

    if !tokenTextAt(exprTokens, 0, "x") || !tokenTextAt(exprTokens, 1, "+") || !tokenTextAt(exprTokens, 2, "1"):
        return 4

    val exprListStatement: pointer<Statement> = parseStatementText("x, y + 1;")

    if exprListStatement == null || exprListStatement.getKind() != Statement.EXPRESSION_LIST_TYPE:
        return 5

    val exprListRoot: pointer<ExprListStatement> = exprListStatement.getRoot() as pointer<ExprListStatement>
    var expressions: pointer<ArrayList> = null

    if exprListRoot != null:
        expressions = exprListRoot.getExpressions()

    if expressions == null || expressions.length != 2:
        return 6

    val exprListTokens: pointer<ArrayList> = exprListStatement.getAllTokens()

    if exprListTokens == null || exprListTokens.length != 5:
        return 7

    if !tokenTextAt(exprListTokens, 0, "x") || !tokenTextAt(exprListTokens, 1, ",") || !tokenTextAt(exprListTokens, 2, "y") || !tokenTextAt(exprListTokens, 3, "+") || !tokenTextAt(exprListTokens, 4, "1"):
        return 8

    val returnStatement: pointer<Statement> = parseStatementText("return y + 1;")

    if returnStatement == null || returnStatement.getKind() != Statement.RETURN_TYPE:
        return 9

    val returnRoot: pointer<ReturnStatement> = returnStatement.getRoot() as pointer<ReturnStatement>

    if returnRoot == null || !returnRoot.haveReturnValue():
        return 10

    val returnTokens: pointer<ArrayList> = returnStatement.getAllTokens()

    if returnTokens == null || returnTokens.length != 4:
        return 11

    if !tokenTextAt(returnTokens, 0, "return") || !tokenTextAt(returnTokens, 1, "y") || !tokenTextAt(returnTokens, 2, "+") || !tokenTextAt(returnTokens, 3, "1"):
        return 12

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

    val tokens: pointer<ArrayList> = expression.getAllTokens()

    if tokens == null || tokens.length != 8:
        return 6

    val token0: pointer<Token> = tokens.get(0) as pointer<Token>
    val token1: pointer<Token> = tokens.get(1) as pointer<Token>
    val token2: pointer<Token> = tokens.get(2) as pointer<Token>
    val token3: pointer<Token> = tokens.get(3) as pointer<Token>
    val token4: pointer<Token> = tokens.get(4) as pointer<Token>
    val token5: pointer<Token> = tokens.get(5) as pointer<Token>
    val token6: pointer<Token> = tokens.get(6) as pointer<Token>
    val token7: pointer<Token> = tokens.get(7) as pointer<Token>

    if token0 == null || String.streq(token0.text, "f") == false:
        return 7

    if token1 == null || String.streq(token1.text, "(") == false:
        return 8

    if token2 == null || String.streq(token2.text, "1") == false:
        return 9

    if token3 == null || String.streq(token3.text, ",") == false:
        return 10

    if token4 == null || String.streq(token4.text, "2") == false:
        return 11

    if token5 == null || String.streq(token5.text, "+") == false:
        return 12

    if token6 == null || String.streq(token6.text, "3") == false:
        return 13

    if token7 == null || String.streq(token7.text, ")") == false:
        return 14

    return 0
}


private fun atomExpressionTest() -> int
{
    val expression: pointer<Expression> = parseExpressionText("x")

    if expression == null || expression.getKind() != Expression.ATOM_KIND:
        return 1

    val tokens: pointer<ArrayList> = expression.getAllTokens()

    if tokens == null || tokens.length != 1:
        return 2

    if !tokenTextAt(tokens, 0, "x"):
        return 3

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

    val tokens: pointer<ArrayList> = expression.getAllTokens()

    if tokens == null || tokens.length != 11:
        return 7

    if !tokenTextAt(tokens, 0, "(") || !tokenTextAt(tokens, 4, ")") || !tokenTextAt(tokens, 5, "*") || !tokenTextAt(tokens, 6, "(") || !tokenTextAt(tokens, 10, ")"):
        return 8

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

    val tokens: pointer<ArrayList> = incExpression.getAllTokens()

    if tokens == null || tokens.length != 4:
        return 7

    if !tokenTextAt(tokens, 0, "++") || !tokenTextAt(tokens, 1, "x") || !tokenTextAt(tokens, 2, "+") || !tokenTextAt(tokens, 3, "y"):
        return 8

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

    val tokens: pointer<ArrayList> = expression.getAllTokens()

    if tokens == null || tokens.length != 8:
        return 6

    if !tokenTextAt(tokens, 0, "arr") || !tokenTextAt(tokens, 1, "[") || !tokenTextAt(tokens, 3, ",") || !tokenTextAt(tokens, 7, "]"):
        return 7

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

    val tokens: pointer<ArrayList> = expression.getAllTokens()

    if tokens == null || tokens.length != 10:
        return 6

    if !tokenTextAt(tokens, 0, "a"):
        return 7

    if !tokenTextAt(tokens, 1, "."):
        return 8

    if !tokenTextAt(tokens, 2, "func"):
        return 9

    if !tokenTextAt(tokens, 3, "("):
        return 10

    if !tokenTextAt(tokens, 4, "1"):
        return 11

    if !tokenTextAt(tokens, 5, ","):
        return 12

    if !tokenTextAt(tokens, 6, "2"):
        return 13

    if !tokenTextAt(tokens, 7, "+"):
        return 14

    if !tokenTextAt(tokens, 8, "3"):
        return 15

    if !tokenTextAt(tokens, 9, ")"):
        return 16

    return 0}


private fun fieldAccessExpressionTest() -> int
{
    val expression: pointer<Expression> = parseExpressionText("a.b.c")

    if expression == null || expression.getKind() != Expression.FIELD_ACCESS_KIND:
        return 1

    val access: pointer<FieldAccess> = expression.getRoot() as pointer<FieldAccess>

    if access == null:
        return 2

    val tokens: pointer<ArrayList> = expression.getAllTokens()

    if tokens == null || tokens.length != 5:
        return 3

    if !tokenTextAt(tokens, 0, "a") || !tokenTextAt(tokens, 1, ".") || !tokenTextAt(tokens, 2, "b") || !tokenTextAt(tokens, 3, ".") || !tokenTextAt(tokens, 4, "c"):
        return 4

    return 0
}


private fun postfixExpressionTest() -> int
{
    if !hasRootMethodCall("x++", "succ"):
        return 1

    if !hasRootMethodCall("x--", "pred"):
        return 2

    val expression: pointer<Expression> = parseExpressionText("x++")
    val tokens: pointer<ArrayList> = expression.getAllTokens()

    if tokens == null || tokens.length != 2:
        return 3

    if !tokenTextAt(tokens, 0, "x") || !tokenTextAt(tokens, 1, "++"):
        return 4

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

    val tokens: pointer<ArrayList> = expression.getAllTokens()

    if tokens == null || tokens.length != 5:
        return 9

    if !tokenTextAt(tokens, 0, "1") || !tokenTextAt(tokens, 1, "+") || !tokenTextAt(tokens, 2, "1") || !tokenTextAt(tokens, 3, "as") || !tokenTextAt(tokens, 4, "double"):
        return 10

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

    val tokens: pointer<ArrayList> = priorityExpression.getAllTokens()

    if tokens == null || tokens.length != 5:
        return 13

    if !tokenTextAt(tokens, 0, "1") || !tokenTextAt(tokens, 1, "+") || !tokenTextAt(tokens, 2, "2") || !tokenTextAt(tokens, 3, "*") || !tokenTextAt(tokens, 4, "3"):
        return 14

    return 0
}
private fun compoundOperatorExpressionTest() -> int
{
    if !expressionTextEquals("inv x", "inv(x)"):
        return 1

    if !expressionTextEquals("!x", "not(x)"):
        return 2

    if !expressionTextEquals("x xor y", "bitwiseOr(bitwiseAnd(x, inv(y)), bitwiseAnd(inv(x), y))"):
        return 3

    if !expressionTextEquals("x xnor y", "bitwiseOr(bitwiseAnd(x, y), bitwiseAnd(inv(x), inv(y)))"):
        return 4

    if !expressionTextEquals("x implies y", "bitwiseOr(inv(x), y)"):
        return 5

    if !expressionTextEquals("x nimplies y", "bitwiseAnd(x, inv(y))"):
        return 6

    if !expressionTextEquals("x ^ y", "logicalOr(logicalAnd(x, not(y)), logicalAnd(not(x), y))"):
        return 7

    if !expressionTextEquals("x !^ y", "logicalOr(logicalAnd(x, y), logicalAnd(not(x), not(y)))"):
        return 8

    if !expressionTextEquals("x -> y", "logicalOr(not(x), y)"):
        return 9

    if !expressionTextEquals("x !-> y", "logicalAnd(x, not(y))"):
        return 10

    if !expressionTextEquals("x <-> y", "logicalOr(logicalAnd(x, y), logicalAnd(not(x), not(y)))"):
        return 11

    if !expressionTextEquals("x !<-> y", "logicalOr(logicalAnd(x, not(y)), logicalAnd(not(x), y))"):
        return 12

    if !expressionTextEquals("x !&& y", "not(logicalAnd(x, y))"):
        return 13

    if !expressionTextEquals("x !|| y", "not(logicalOr(x, y))"):
        return 14

    if !expressionTextEquals("x **= y", "(x = pow(x, y))"):
        return 15

    if !expressionTextEquals("x *= y", "(x = times(x, y))"):
        return 16

    if !expressionTextEquals("x /= y", "(x = div(x, y))"):
        return 17

    if !expressionTextEquals("x %= y", "(x = rem(x, y))"):
        return 18

    if !expressionTextEquals("x += y", "(x = plus(x, y))"):
        return 19

    if !expressionTextEquals("x -= y", "(x = minus(x, y))"):
        return 20

    if !expressionTextEquals("x <<= y", "(x = shl(x, y))"):
        return 21

    if !expressionTextEquals("x >>= y", "(x = shr(x, y))"):
        return 22

    if !expressionTextEquals("x <<<= y", "(x = shl(x, y))"):
        return 23

    if !expressionTextEquals("x >>>= y", "(x = ushr(x, y))"):
        return 24

    if !expressionTextEquals("x &= y", "(x = bitwiseAnd(x, y))"):
        return 25

    if !expressionTextEquals("x !&= y", "(x = inv(bitwiseAnd(x, y)))"):
        return 26

    if !expressionTextEquals("x |= y", "(x = bitwiseOr(x, y))"):
        return 27

    if !expressionTextEquals("x !|= y", "(x = inv(bitwiseOr(x, y)))"):
        return 28

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

    val tokens: pointer<ArrayList> = expression.getAllTokens()

    if tokens == null || tokens.length != 29:
        return 14

    if !tokenTextAt(tokens, 0, "f") || !tokenTextAt(tokens, 1, "(") || !tokenTextAt(tokens, 21, ",") || !tokenTextAt(tokens, 26, "-") || !tokenTextAt(tokens, 28, "--"):
        return 15

    return 0
}
