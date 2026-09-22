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
 */

#file.outerClass("ParserRegressionTest")
package xlang.compiler.parser

import xlang.System
import xlang.compiler.lexer.Tokenizer
import xlang.compiler.parser.expression.Expression
import xlang.compiler.parser.expression.ExpressionTuple
import xlang.compiler.parser.program.Function
import xlang.compiler.parser.statement.Statement
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.util.string.StringBuilder
import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion


val TEST_GROUP: pointer<TestGroup> = genTest()


fun genTest() -> pointer<TestGroup>
{
    val result: pointer<TestGroup> = new TestGroup("xlang.compiler.parser.ParserRegression")

    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("genericReturnExpressionFunction", genericReturnExpressionFunctionTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("genericReturnInlineBlockFunction", genericReturnInlineBlockFunctionTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("genericReturnMultilineBlockFunction", genericReturnMultilineBlockFunctionTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("ifReturnStatement", ifReturnStatementTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("blockIfReturnFunction", blockIfReturnFunctionTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("typedValMethodCallStatement", typedValMethodCallStatementTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("subTokenWithoutIfFunction", subTokenWithoutIfFunctionTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("emptyExpressionTuple", emptyExpressionTupleTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("oneArgExpressionTuple", oneArgExpressionTupleTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("twoArgExpressionTuple", twoArgExpressionTupleTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("functionCallOneArgStatement", functionCallOneArgStatementTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("functionCallTwoArgsStatement", functionCallTwoArgsStatementTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("methodCallOneArgStatement", methodCallOneArgStatementTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("methodCallTwoArgsStatement", methodCallTwoArgsStatementTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("thisFieldAccessExpression", thisFieldAccessExpressionTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("thisMethodCallTwoArgsStatement", thisMethodCallTwoArgsStatementTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("systemMethodCallTwoArgsStatement", systemMethodCallTwoArgsStatementTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("functionCallFieldArgStatement", functionCallFieldArgStatementTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("functionCallThisFieldArgStatement", functionCallThisFieldArgStatementTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("functionCallInfixFieldArgStatement", functionCallInfixFieldArgStatementTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("singleLineMethodCallStatement", singleLineMethodCallStatementTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("multilinePlainFunctionCallStatement", multilinePlainFunctionCallStatementTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("emptyCallExpression", emptyCallExpressionTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("valNewEmptyCallStatement", valNewEmptyCallStatementTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("tokenListSubTokenFunction", tokenListSubTokenFunctionTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("multilineMethodCallStatement", multilineMethodCallStatementTest), null))
    result.addTestUnion(new TestUnion(TestCase.TYPE, new TestCase("arrayListSublistFunction", arrayListSublistFunctionTest), null))

    return result
}


private fun genericReturnExpressionFunctionTest() -> int
{
    val function: pointer<Function> = parseFunctionText("fun f() -> pointer<TokenList>: result\n")

    if function == null:
        return 1

    if function.getReturnType() == null || function.getBodyExpr() == null:
        return 2

    return 0
}


private fun genericReturnInlineBlockFunctionTest() -> int
{
    val function: pointer<Function> = parseFunctionText("fun f() -> pointer<TokenList> {return result}\n")

    if function == null:
        return 1

    if function.getReturnType() == null || function.getBodyExpr() == null:
        return 2

    if function.getBodyExpr().getKind() != Expression.BLOCK_EXPR_KIND:
        return 3

    return 0
}


private fun genericReturnMultilineBlockFunctionTest() -> int
{
    val source: pointer<StringBuilder> = new StringBuilder()
    source.append("fun f() -> pointer<TokenList>\n")
    source.append("{\n")
    source.append("    return result\n")
    source.append("}\n")

    val function: pointer<Function> = parseFunctionText(builderToString(source))

    if function == null:
        return 1

    if function.getReturnType() == null || function.getBodyExpr() == null:
        return 2

    if function.getBodyExpr().getKind() != Expression.BLOCK_EXPR_KIND:
        return 3

    return 0
}


private fun ifReturnStatementTest() -> int
{
    val source: pointer<StringBuilder> = new StringBuilder()
    source.append("if copiedTokens == null:\n")
    source.append("    return null\n")

    val statement: pointer<Statement> = parseStatementText(builderToString(source))

    if statement == null:
        return 1

    return 0
}


private fun blockIfReturnFunctionTest() -> int
{
    val source: pointer<StringBuilder> = new StringBuilder()
    source.append("fun f() -> pointer<TokenList>\n")
    source.append("{\n")
    source.append("    if copiedTokens == null:\n")
    source.append("        return null\n")
    source.append("    return result\n")
    source.append("}\n")

    val function: pointer<Function> = parseFunctionText(builderToString(source))

    if function == null:
        return 1

    return 0
}


private fun typedValMethodCallStatementTest() -> int
{
    val statement: pointer<Statement> =
        parseStatementText("val copiedTokens: pointer<ArrayList> = this.tokens.sublist(fromIndex, toIndex)\n")

    if statement == null:
        return 1

    return 0
}


private fun subTokenWithoutIfFunctionTest() -> int
{
    val source: pointer<StringBuilder> = new StringBuilder()
    source.append("fun subToken(fromIndex: int, toIndex: int) -> pointer<TokenList>\n")
    source.append("{\n")
    source.append("    val copiedTokens: pointer<ArrayList> = this.tokens.sublist(fromIndex, toIndex)\n")
    source.append("    val result: pointer<TokenList> = new TokenList()\n")
    source.append("    result.tokens = copiedTokens\n")
    source.append("    return result\n")
    source.append("}\n")

    val function: pointer<Function> = parseFunctionText(builderToString(source))

    if function == null:
        return 1

    return 0
}


private fun emptyExpressionTupleTest() -> int =
    if parseExpressionTupleText("()") != null:
        0
    else:
        1


private fun oneArgExpressionTupleTest() -> int =
    if parseExpressionTupleText("(1)") != null:
        0
    else:
        1


private fun twoArgExpressionTupleTest() -> int =
    if parseExpressionTupleText("(1, 2)") != null:
        0
    else:
        1


private fun functionCallOneArgStatementTest() -> int =
    if parseStatementText("f(1)\n") != null:
        0
    else:
        1


private fun functionCallTwoArgsStatementTest() -> int =
    if parseStatementText("f(1, 2)\n") != null:
        0
    else:
        1


private fun methodCallOneArgStatementTest() -> int =
    if parseStatementText("a.b(1)\n") != null:
        0
    else:
        1


private fun methodCallTwoArgsStatementTest() -> int =
    if parseStatementText("a.b(1, 2)\n") != null:
        0
    else:
        1


private fun thisFieldAccessExpressionTest() -> int =
    if parseExpressionText("this.tokens") != null:
        0
    else:
        1


private fun thisMethodCallTwoArgsStatementTest() -> int =
    if parseStatementText("this.tokens.sublist(fromIndex, toIndex)\n") != null:
        0
    else:
        1


private fun systemMethodCallTwoArgsStatementTest() -> int =
    if parseStatementText("System.memcopy(1, 2)\n") != null:
        0
    else:
        1


private fun functionCallFieldArgStatementTest() -> int =
    if parseStatementText("f(sublist.data)\n") != null:
        0
    else:
        1


private fun functionCallThisFieldArgStatementTest() -> int =
    if parseStatementText("f(this.data)\n") != null:
        0
    else:
        1


private fun functionCallInfixFieldArgStatementTest() -> int =
    if parseStatementText("f(this.data + fromIndex * this.tsize)\n") != null:
        0
    else:
        1


private fun singleLineMethodCallStatementTest() -> int =
    if parseStatementText("System.memcopy(sublist.data, this.data + fromIndex * this.tsize, size * this.tsize)\n") != null:
        0
    else:
        1


private fun multilinePlainFunctionCallStatementTest() -> int
{
    val source: pointer<StringBuilder> = new StringBuilder()
    source.append("memcopy(\n")
    source.append("    sublist.data,\n")
    source.append("    this.data + fromIndex * this.tsize,\n")
    source.append("    size * this.tsize)\n")

    if parseStatementText(builderToString(source)) == null:
        return 1

    return 0
}


private fun emptyCallExpressionTest() -> int
{
    val newCall: pointer<Expression> = parseExpressionText("new TokenList()")

    if newCall == null:
        return 1

    val methodCall: pointer<Expression> = parseExpressionText("this.tokens.clone()")

    if methodCall == null:
        return 2

    return 0
}


private fun valNewEmptyCallStatementTest() -> int =
    if parseStatementText("val result: pointer<TokenList> = new TokenList()") != null:
        0
    else:
        1


private fun tokenListSubTokenFunctionTest() -> int
{
    val source: pointer<StringBuilder> = new StringBuilder()
    source.append("fun subToken(fromIndex: int, toIndex: int) -> pointer<TokenList>\n")
    source.append("{\n")
    source.append("    val copiedTokens: pointer<ArrayList> = this.tokens.sublist(fromIndex, toIndex)\n")
    source.append("    if copiedTokens == null:\n")
    source.append("        return null\n")
    source.append("    val result: pointer<TokenList> = new TokenList()\n")
    source.append("    result.tokens = copiedTokens\n")
    source.append("    return result\n")
    source.append("}\n")

    val function: pointer<Function> = parseFunctionText(builderToString(source))

    if function == null:
        return 1

    if function.getReturnType() == null || function.getBodyExpr() == null:
        return 2

    if function.getBodyExpr().getKind() != Expression.BLOCK_EXPR_KIND:
        return 3

    return 0
}


private fun multilineMethodCallStatementTest() -> int
{
    val source: pointer<StringBuilder> = new StringBuilder()
    source.append("System.memcopy(\n")
    source.append("    sublist.data,\n")
    source.append("    this.data + fromIndex * this.tsize,\n")
    source.append("    size * this.tsize)\n")

    if parseStatementText(builderToString(source)) == null:
        return 1

    return 0
}


private fun arrayListSublistFunctionTest() -> int
{
    val source: pointer<StringBuilder> = new StringBuilder()
    source.append("fun sublist(fromIndex: int, toIndex: int) -> pointer<ArrayList>\n")
    source.append("{\n")
    source.append("    if fromIndex < 0 || toIndex < fromIndex || toIndex > this.length:\n")
    source.append("        return null\n")
    source.append("\n")
    source.append("    val size: int = toIndex - fromIndex\n")
    source.append("    val sublist: pointer<ArrayList> = new ArrayList(this.tsize, size + 1, this.loadFactor, this.cmp)\n")
    source.append("\n")
    source.append("    if size > 0:\n")
    source.append("    {\n")
    source.append("        System.memcopy(\n")
    source.append("            sublist.data,\n")
    source.append("            this.data + fromIndex * this.tsize,\n")
    source.append("            size * this.tsize)\n")
    source.append("        sublist.length = size\n")
    source.append("    }\n")
    source.append("\n")
    source.append("    return sublist\n")
    source.append("}\n")

    val function: pointer<Function> = parseFunctionText(builderToString(source))

    if function == null:
        return 1

    if function.getReturnType() == null || function.getBodyExpr() == null:
        return 2

    if function.getBodyExpr().getKind() != Expression.BLOCK_EXPR_KIND:
        return 3

    return 0
}


private fun parseExpressionText(text: pointer<char>) -> pointer<Expression>
{
    val tokens: pointer<TokenList> = Tokenizer.tokenize(text)
    return Parser.parseExpression(tokens)
}


private fun parseExpressionTupleText(text: pointer<char>) -> pointer<ExpressionTuple>
{
    val tokens: pointer<TokenList> = Tokenizer.tokenize(text)
    val tuple: pointer<ExpressionTuple> = Parser.parseExpressionTuple(tokens)

    if tuple == null:
        return null

    val rest: pointer<Token> = tokens.get(0)

    if tokens.length() > 0 && rest != null && !rest.isEOF():
        return null

    return tuple
}


private fun parseStatementText(text: pointer<char>) -> pointer<Statement>
{
    val tokens: pointer<TokenList> = Tokenizer.fullTokenize(text)
    return Parser.parseStatement(tokens)
}


private fun parseFunctionText(text: pointer<char>) -> pointer<Function>
{
    val tokens: pointer<TokenList> = Tokenizer.fullTokenize(text)
    return Parser.parseFunction(tokens)
}


private fun builderToString(builder: pointer<StringBuilder>) -> pointer<char>
{
    if builder == null:
        return null

    val buffer: pointer<char> = System.allocMemory((builder.length + 1) * sizeof(char)) as pointer<char>
    builder.toString(buffer)
    return buffer
}
