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

#file.outerClass("TypeParserTest")
package xlang.parser

import xlang.compiler.type.BlobType
import xlang.compiler.type.NormalType
import xlang.compiler.type.Type
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
    val parenthesizedTypeTC: pointer<TestCase> = new TestCase("parenthesizedType", parenthesizedTypeTest)
    val parenthesizedFunctionTC: pointer<TestCase> = new TestCase("parenthesizedFunction", parenthesizedFunctionTest)
    val blobExpressionTC: pointer<TestCase> = new TestCase("blobExpression", blobExpressionTest)
    val voidPrimaryTypeTC: pointer<TestCase> = new TestCase("voidPrimaryType", voidPrimaryTypeTest)
    val boolPrimaryTypeTC: pointer<TestCase> = new TestCase("boolPrimaryType", boolPrimaryTypeTest)
    val charPrimaryTypeTC: pointer<TestCase> = new TestCase("charPrimaryType", charPrimaryTypeTest)
    val bytePrimaryTypeTC: pointer<TestCase> = new TestCase("bytePrimaryType", bytePrimaryTypeTest)
    val shortPrimaryTypeTC: pointer<TestCase> = new TestCase("shortPrimaryType", shortPrimaryTypeTest)
    val intPrimaryTypeTC: pointer<TestCase> = new TestCase("intPrimaryType", intPrimaryTypeTest)
    val longPrimaryTypeTC: pointer<TestCase> = new TestCase("longPrimaryType", longPrimaryTypeTest)
    val floatPrimaryTypeTC: pointer<TestCase> = new TestCase("floatPrimaryType", floatPrimaryTypeTest)
    val doublePrimaryTypeTC: pointer<TestCase> = new TestCase("doublePrimaryType", doublePrimaryTypeTest)
    val pointerPrimaryTypeTC: pointer<TestCase> = new TestCase("pointerPrimaryType", pointerPrimaryTypeTest)
    val pointerBlobTypeTC: pointer<TestCase> = new TestCase("pointerBlobType", pointerBlobTypeTest)
    val nestedPointerBlobTypeTC: pointer<TestCase> = new TestCase("nestedPointerBlobType", nestedPointerBlobTypeTest)
    val emptyFunctionTC: pointer<TestCase> = new TestCase("emptyFunction", emptyFunctionTest)
    val functionParametersTC: pointer<TestCase> = new TestCase("functionParameters", functionParametersTest)
    val nestedFunctionTC: pointer<TestCase> = new TestCase("nestedFunction", nestedFunctionTest)
    val mixedFunctionTC: pointer<TestCase> = new TestCase("mixedFunction", mixedFunctionTest)
    val pointerVoidUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, pointerVoidTC, null)
    val topLevelStarUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, topLevelStarTC, null)
    val normalParseUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, normalParseTC, null)
    val parenthesizedTypeUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, parenthesizedTypeTC, null)
    val parenthesizedFunctionUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, parenthesizedFunctionTC, null)
    val blobExpressionUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, blobExpressionTC, null)
    val voidPrimaryTypeUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, voidPrimaryTypeTC, null)
    val boolPrimaryTypeUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, boolPrimaryTypeTC, null)
    val charPrimaryTypeUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, charPrimaryTypeTC, null)
    val bytePrimaryTypeUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, bytePrimaryTypeTC, null)
    val shortPrimaryTypeUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, shortPrimaryTypeTC, null)
    val intPrimaryTypeUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, intPrimaryTypeTC, null)
    val longPrimaryTypeUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, longPrimaryTypeTC, null)
    val floatPrimaryTypeUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, floatPrimaryTypeTC, null)
    val doublePrimaryTypeUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, doublePrimaryTypeTC, null)
    val pointerPrimaryTypeUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, pointerPrimaryTypeTC, null)
    val pointerBlobTypeUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, pointerBlobTypeTC, null)
    val nestedPointerBlobTypeUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, nestedPointerBlobTypeTC, null)
    val emptyFunctionUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, emptyFunctionTC, null)
    val functionParametersUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, functionParametersTC, null)
    val nestedFunctionUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, nestedFunctionTC, null)
    val mixedFunctionUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, mixedFunctionTC, null)

    result.addTestUnion(pointerVoidUnion)
    result.addTestUnion(topLevelStarUnion)
    result.addTestUnion(normalParseUnion)
    result.addTestUnion(parenthesizedTypeUnion)
    result.addTestUnion(parenthesizedFunctionUnion)
    result.addTestUnion(blobExpressionUnion)
    result.addTestUnion(voidPrimaryTypeUnion)
    result.addTestUnion(boolPrimaryTypeUnion)
    result.addTestUnion(charPrimaryTypeUnion)
    result.addTestUnion(bytePrimaryTypeUnion)
    result.addTestUnion(shortPrimaryTypeUnion)
    result.addTestUnion(intPrimaryTypeUnion)
    result.addTestUnion(longPrimaryTypeUnion)
    result.addTestUnion(floatPrimaryTypeUnion)
    result.addTestUnion(doublePrimaryTypeUnion)
    result.addTestUnion(pointerPrimaryTypeUnion)
    result.addTestUnion(pointerBlobTypeUnion)
    result.addTestUnion(nestedPointerBlobTypeUnion)
    result.addTestUnion(emptyFunctionUnion)
    result.addTestUnion(functionParametersUnion)
    result.addTestUnion(nestedFunctionUnion)
    result.addTestUnion(mixedFunctionUnion)

    return result
}


private fun parseTypeValue(input: pointer<char>, expectedConsumed: int) -> pointer<Type>
{
    val tokens: pointer<TokenList> = Tokenizer.tokenize(input)
    val parser: pointer<TypeParser> = new TypeParser(1)
    val consumed: int = parser.parse(tokens, 0)

    if parser.haveError(consumed) || consumed != expectedConsumed:
        return null

    val container: pointer<ParseContainer> = parser.getResult()

    if container == null || !container.isKind(1):
        return null

    return container.getValue() as pointer<Type>
}


private fun checkPrimaryType(input: pointer<char>, typeName: pointer<char>, memSize: int) -> int
{
    val parsedType: pointer<Type> = parseTypeValue(input, 1)

    if parsedType == null:
        return 1

    if parsedType.getKind() != Type.NORMAL_KIND:
        return 2

    val normalType: pointer<NormalType> = parsedType.getHost() as pointer<NormalType>

    if normalType == null:
        return 3

    if !String.streq(normalType.getPackageName(), "xlang.primary"):
        return 4

    if !String.streq(normalType.getTypeName(), typeName):
        return 5

    if normalType.getMemSize() != memSize:
        return 6

    if normalType.length != 0:
        return 7

    return 0
}


private fun voidPrimaryTypeTest() -> int = checkPrimaryType("void", "void", 0)


private fun boolPrimaryTypeTest() -> int = checkPrimaryType("bool", "bool", 1)


private fun charPrimaryTypeTest() -> int = checkPrimaryType("char", "char", 8)


private fun bytePrimaryTypeTest() -> int = checkPrimaryType("byte", "byte", 1)


private fun shortPrimaryTypeTest() -> int = checkPrimaryType("short", "short", 2)


private fun intPrimaryTypeTest() -> int = checkPrimaryType("int", "int", 4)


private fun longPrimaryTypeTest() -> int = checkPrimaryType("long", "long", 8)


private fun floatPrimaryTypeTest() -> int = checkPrimaryType("float", "float", 4)


private fun doublePrimaryTypeTest() -> int = checkPrimaryType("double", "double", 8)


private fun pointerPrimaryTypeTest() -> int = checkPrimaryType("pointer", "pointer", 8)


private fun pointerBlobTypeTest() -> int
{
    val parsedType: pointer<Type> = parseTypeValue("pointer<blob[100]>", 7)

    if parsedType == null || parsedType.getKind() != Type.NORMAL_KIND:
        return 1

    val pointerType: pointer<NormalType> = parsedType.getHost() as pointer<NormalType>

    if pointerType == null:
        return 2

    if !String.streq(pointerType.getPackageName(), "xlang.primary") ||
        !String.streq(pointerType.getTypeName(), "pointer") ||
        pointerType.getMemSize() != 8:
        return 3

    if pointerType.length != 1:
        return 4

    val argument: pointer<Type> = pointerType.getTypeArgument(0)

    if argument == null || argument.getKind() != Type.BLOB_KIND:
        return 5

    val blobType: pointer<BlobType> = argument.getHost() as pointer<BlobType>

    if blobType == null || blobType.getBlobSize() == null || blobType.getMemSize() != 0:
        return 6

    return 0
}


private fun nestedPointerBlobTypeTest() -> int
{
    val parsedType: pointer<Type> = parseTypeValue("pointer<pointer<blob[1000]>>", 10)

    if parsedType == null || parsedType.getKind() != Type.NORMAL_KIND:
        return 1

    val outerPointer: pointer<NormalType> = parsedType.getHost() as pointer<NormalType>

    if outerPointer == null ||
        !String.streq(outerPointer.getPackageName(), "xlang.primary") ||
        !String.streq(outerPointer.getTypeName(), "pointer") ||
        outerPointer.length != 1:
        return 2

    val innerPointerType: pointer<Type> = outerPointer.getTypeArgument(0)

    if innerPointerType == null || innerPointerType.getKind() != Type.NORMAL_KIND:
        return 3

    val innerPointer: pointer<NormalType> = innerPointerType.getHost() as pointer<NormalType>

    if innerPointer == null ||
        !String.streq(innerPointer.getPackageName(), "xlang.primary") ||
        !String.streq(innerPointer.getTypeName(), "pointer") ||
        innerPointer.length != 1:
        return 4

    val blobArgument: pointer<Type> = innerPointer.getTypeArgument(0)

    if blobArgument == null || blobArgument.getKind() != Type.BLOB_KIND:
        return 5

    val blobType: pointer<BlobType> = blobArgument.getHost() as pointer<BlobType>

    if blobType == null || blobType.getBlobSize() == null || blobType.getMemSize() != 0:
        return 6

    return 0
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


private fun blobExpressionTest() -> int
{
    val typeTokens: pointer<ArrayList> = parseTypeTokens("blob[1 + 2 * 3]", 8)

    if typeTokens == null || typeTokens.length != 8:
        return 1

    if !tokenTextAt(typeTokens, 0, "blob") || !tokenTextAt(typeTokens, 1, "["):
        return 2

    if !tokenTextAt(typeTokens, 2, "1") || !tokenTextAt(typeTokens, 3, "+"):
        return 3

    if !tokenTextAt(typeTokens, 4, "2") || !tokenTextAt(typeTokens, 5, "*"):
        return 4

    if !tokenTextAt(typeTokens, 6, "3") || !tokenTextAt(typeTokens, 7, "]"):
        return 5

    return 0
}


private fun parenthesizedTypeTest() -> int
{
    val typeTokens: pointer<ArrayList> = parseTypeTokens("((int))", 5)

    if typeTokens == null || typeTokens.length != 1:
        return 1

    if !tokenTextAt(typeTokens, 0, "int"):
        return 2

    return 0
}


private fun parenthesizedFunctionTest() -> int
{
    val typeTokens: pointer<ArrayList> = parseTypeTokens("((int), double) -> (() -> void)", 14)

    if typeTokens == null || typeTokens.length != 10:
        return 1

    if !tokenTextAt(typeTokens, 0, "(") || !tokenTextAt(typeTokens, 1, "int"):
        return 2

    if !tokenTextAt(typeTokens, 2, ",") || !tokenTextAt(typeTokens, 3, "double"):
        return 3

    if !tokenTextAt(typeTokens, 4, ")") || !tokenTextAt(typeTokens, 5, "->"):
        return 4

    if !tokenTextAt(typeTokens, 6, "(") || !tokenTextAt(typeTokens, 7, ")"):
        return 5

    if !tokenTextAt(typeTokens, 8, "->") || !tokenTextAt(typeTokens, 9, "void"):
        return 6

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
