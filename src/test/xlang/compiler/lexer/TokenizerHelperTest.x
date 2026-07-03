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
 *
 */

@file.class("TokenizerHelperTest")
package xlang.compiler.lexer

import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.lexer.LexState
import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion
import xlang.util.string.String


val TEST_GROUP: TestGroup = genTest()


fun genTest() -> TestGroup
{
    val result: TestGroup = new TestGroup("xlang.compiler.lexer.Tokenizer")
    val tokenizeTC: pointer<TestCase> = new TestCase("tokenize", tokenizeTest)
    val tokenizeUnion: TestUnion = new TestUnion(TestCase.TYPE, tokenizeTC, null)

    result.addTestUnion(tokenizeUnion)

    return result
}


private fun tokenizeTest() -> int
{
    var result: int = charEscapeTest()

    if result != 0:
        return 1

    result = stringEscapeTest()

    if result != 0:
        return 2

    result = forLoopTest()

    if result != 0:
        return 3

    result = numbersTest()

    if result != 0:
        return 4

    result = symbolsTest()

    if result != 0:
        return 5

    result = dotIncDecTest()

    if result != 0:
        return 6

    result = commentsTest()

    if result != 0:
        return 7

    result = invalidIdentTest()

    if result != 0:
        return 8

    result = arrayExpressionTest()

    if result != 0:
        return 9

    result = unclosedBlockCommentTest()

    if result != 0:
        return 10

    result = unclosedStringTest()

    if result != 0:
        return 11

    result = unclosedCharTest()

    if result != 0:
        return 12

    result = positionTest()

    if result != 0:
        return 13

    return 0
}


private fun checkTokens(input: pointer<char>, kinds: pointer<int>, texts: pointer<pointer<char>>, length: int) -> int
{
    val tokens: TokenList = Tokenizer.tokenize(input)

    if tokens.length() != length:
        return 1

    for (var i = 0; i < length; i++):
    {
        val token: pointer<Token> = tokens.get(i)

        if token.kind != kinds[i]:
            return 1

        if !String.streq(token.text, texts[i]):
            return 1
    }

    return 0
}


private fun checkTokenPosition(tokens: TokenList, index: int, offset: int, line: int, column: int, length: int) -> int
{
    if tokens.length() <= index:
        return 1

    val token: pointer<Token> = tokens.get(index)

    if token.pos.offset != offset:
        return 1

    if token.pos.line != line:
        return 1

    if token.pos.column != column:
        return 1

    if token.pos.length != length:
        return 1

    return 0
}


private fun commentsTest() -> int
{
    val kindsSpace: blob[sizeof(int) * 10]
    val textsSpace: blob[sizeof(pointer<char>) * 10]
    val kinds: pointer<int> = kindsSpace as pointer<int>
    val texts: pointer<pointer<char>> = textsSpace as pointer<pointer<char>>

    kinds[0] = Tokenizer.TK_IDENTITY
    texts[0] = "a"
    kinds[1] = Tokenizer.EQUAL
    texts[1] = "="
    kinds[2] = Tokenizer.TK_INTEGER
    texts[2] = "1"
    kinds[3] = Tokenizer.TK_IDENTITY
    texts[3] = "b"
    kinds[4] = Tokenizer.EQUAL
    texts[4] = "="
    kinds[5] = Tokenizer.TK_INTEGER
    texts[5] = "2"
    kinds[6] = Tokenizer.TK_IDENTITY
    texts[6] = "c"
    kinds[7] = Tokenizer.EQUAL
    texts[7] = "="
    kinds[8] = Tokenizer.TK_INTEGER
    texts[8] = "3"
    kinds[9] = Token.EOF_KIND
    texts[9] = Token.EOF_STRING

    return checkTokens("a=1/*x*/b=2//y\nc=3", kinds, texts, 10)
}


private fun charEscapeTest() -> int
{
    val kindsSpace: blob[sizeof(int) * 13]
    val textsSpace: blob[sizeof(pointer<char>) * 13]
    val kinds: pointer<int> = kindsSpace as pointer<int>
    val texts: pointer<pointer<char>> = textsSpace as pointer<pointer<char>>

    kinds[0] = Tokenizer.TK_IDENTITY
    texts[0] = "a"
    kinds[1] = Tokenizer.EQUAL
    texts[1] = "="
    kinds[2] = Tokenizer.TK_CHAR
    texts[2] = "\\n"
    kinds[3] = Tokenizer.SEMICOLON
    texts[3] = ";"
    kinds[4] = Tokenizer.TK_IDENTITY
    texts[4] = "b"
    kinds[5] = Tokenizer.EQUAL
    texts[5] = "="
    kinds[6] = Tokenizer.TK_CHAR
    texts[6] = "\\'"
    kinds[7] = Tokenizer.SEMICOLON
    texts[7] = ";"
    kinds[8] = Tokenizer.TK_IDENTITY
    texts[8] = "c"
    kinds[9] = Tokenizer.EQUAL
    texts[9] = "="
    kinds[10] = Tokenizer.TK_CHAR
    texts[10] = "\\\\"
    kinds[11] = Tokenizer.SEMICOLON
    texts[11] = ";"
    kinds[12] = Token.EOF_KIND
    texts[12] = Token.EOF_STRING

    return checkTokens("a='\\n'; b='\\''; c='\\\\';", kinds, texts, 13)
}


private fun stringEscapeTest() -> int
{
    val kindsSpace: blob[sizeof(int) * 20]
    val textsSpace: blob[sizeof(pointer<char>) * 20]
    val kinds: pointer<int> = kindsSpace as pointer<int>
    val texts: pointer<pointer<char>> = textsSpace as pointer<pointer<char>>

    kinds[0] = Tokenizer.TK_IDENTITY
    texts[0] = "str1"
    kinds[1] = Tokenizer.EQUAL
    texts[1] = "="
    kinds[2] = Tokenizer.TK_STRING
    texts[2] = "hello, this is my own program language!!!"
    kinds[3] = Tokenizer.SEMICOLON
    texts[3] = ";"
    kinds[4] = Tokenizer.TK_IDENTITY
    texts[4] = "str2"
    kinds[5] = Tokenizer.EQUAL
    texts[5] = "="
    kinds[6] = Tokenizer.TK_STRING
    texts[6] = "hello, world"
    kinds[7] = Tokenizer.SEMICOLON
    texts[7] = ";"
    kinds[8] = Tokenizer.TK_IDENTITY
    texts[8] = "str3"
    kinds[9] = Tokenizer.EQUAL
    texts[9] = "="
    kinds[10] = Tokenizer.TK_STRING
    texts[10] = "\\0"
    kinds[11] = Tokenizer.SEMICOLON
    texts[11] = ";"
    kinds[12] = Tokenizer.TK_IDENTITY
    texts[12] = "str4"
    kinds[13] = Tokenizer.EQUAL
    texts[13] = "="
    kinds[14] = Tokenizer.KW_NULL
    texts[14] = "null"
    kinds[15] = Tokenizer.SEMICOLON
    texts[15] = ";"
    kinds[16] = Tokenizer.TK_IDENTITY
    texts[16] = "q"
    kinds[17] = Tokenizer.EQUAL
    texts[17] = "="
    kinds[18] = Tokenizer.TK_STRING
    texts[18] = "a\\\"b"
    kinds[19] = Token.EOF_KIND
    texts[19] = Token.EOF_STRING

    return checkTokens("str1=\"hello, this is my own program language!!!\"; str2=\"hello, world\"; /* this is a test */ str3=\"\\0\"; /* /* perfect */ str4=null; q=\"a\\\"b\"", kinds, texts, 20)
}


private fun numbersTest() -> int
{
    val kindsSpace: blob[sizeof(int) * 7]
    val textsSpace: blob[sizeof(pointer<char>) * 7]
    val kinds: pointer<int> = kindsSpace as pointer<int>
    val texts: pointer<pointer<char>> = textsSpace as pointer<pointer<char>>

    kinds[0] = Tokenizer.TK_INTEGER
    texts[0] = "0x1f"
    kinds[1] = Tokenizer.PLUS
    texts[1] = "+"
    kinds[2] = Tokenizer.MINUS
    texts[2] = "-"
    kinds[3] = Tokenizer.TK_LONG
    texts[3] = "42L"
    kinds[4] = Tokenizer.STAR
    texts[4] = "*"
    kinds[5] = Tokenizer.TK_FLOAT
    texts[5] = "3.14e-2f"
    kinds[6] = Token.EOF_KIND
    texts[6] = Token.EOF_STRING

    return checkTokens("0x1f + -42L * 3.14e-2f", kinds, texts, 7)
}


private fun forLoopTest() -> int
{
    val kindsSpace: blob[sizeof(int) * 23]
    val textsSpace: blob[sizeof(pointer<char>) * 23]
    val kinds: pointer<int> = kindsSpace as pointer<int>
    val texts: pointer<pointer<char>> = textsSpace as pointer<pointer<char>>

    kinds[0] = Tokenizer.KW_FOR
    texts[0] = "for"
    kinds[1] = Tokenizer.LEFT_PAREN
    texts[1] = "("
    kinds[2] = Tokenizer.TK_IDENTITY
    texts[2] = "x"
    kinds[3] = Tokenizer.EQUAL
    texts[3] = "="
    kinds[4] = Tokenizer.TK_IDENTITY
    texts[4] = "a_address"
    kinds[5] = Tokenizer.DOT
    texts[5] = "."
    kinds[6] = Tokenizer.TK_IDENTITY
    texts[6] = "getSize"
    kinds[7] = Tokenizer.LEFT_PAREN
    texts[7] = "("
    kinds[8] = Tokenizer.RIGHT_PAREN
    texts[8] = ")"
    kinds[9] = Tokenizer.SEMICOLON
    texts[9] = ";"
    kinds[10] = Tokenizer.TK_IDENTITY
    texts[10] = "x"
    kinds[11] = Tokenizer.LESS_EQUAL
    texts[11] = "<="
    kinds[12] = Tokenizer.TK_INTEGER
    texts[12] = "10"
    kinds[13] = Tokenizer.SEMICOLON
    texts[13] = ";"
    kinds[14] = Tokenizer.TK_IDENTITY
    texts[14] = "x"
    kinds[15] = Tokenizer.STAR_EQUAL
    texts[15] = "*="
    kinds[16] = Tokenizer.TK_INTEGER
    texts[16] = "2"
    kinds[17] = Tokenizer.RIGHT_PAREN
    texts[17] = ")"
    kinds[18] = Tokenizer.TK_IDENTITY
    texts[18] = "print"
    kinds[19] = Tokenizer.LEFT_PAREN
    texts[19] = "("
    kinds[20] = Tokenizer.TK_IDENTITY
    texts[20] = "x"
    kinds[21] = Tokenizer.RIGHT_PAREN
    texts[21] = ")"
    kinds[22] = Token.EOF_KIND
    texts[22] = Token.EOF_STRING

    return checkTokens("for (x = a_address.getSize(); x <= 10; x *= 2) /* iterate the list */ print(x) // this is a comment too", kinds, texts, 23)
}


private fun symbolsTest() -> int
{
    val kindsSpace: blob[sizeof(int) * 20]
    val textsSpace: blob[sizeof(pointer<char>) * 20]
    val kinds: pointer<int> = kindsSpace as pointer<int>
    val texts: pointer<pointer<char>> = textsSpace as pointer<pointer<char>>

    kinds[0] = Tokenizer.TK_IDENTITY
    texts[0] = "a"
    kinds[1] = Tokenizer.DOUBLE_LESS_EQUAL
    texts[1] = "<<="
    kinds[2] = Tokenizer.TK_INTEGER
    texts[2] = "1"
    kinds[3] = Tokenizer.SEMICOLON
    texts[3] = ";"
    kinds[4] = Tokenizer.TK_IDENTITY
    texts[4] = "b"
    kinds[5] = Tokenizer.DOUBLE_GREATER_EQUAL
    texts[5] = ">>="
    kinds[6] = Tokenizer.TK_INTEGER
    texts[6] = "2"
    kinds[7] = Tokenizer.SEMICOLON
    texts[7] = ";"
    kinds[8] = Tokenizer.TK_IDENTITY
    texts[8] = "c"
    kinds[9] = Tokenizer.BANG_CARET_EQUAL
    texts[9] = "!^="
    kinds[10] = Tokenizer.TK_INTEGER
    texts[10] = "3"
    kinds[11] = Tokenizer.SEMICOLON
    texts[11] = ";"
    kinds[12] = Tokenizer.TK_IDENTITY
    texts[12] = "d"
    kinds[13] = Tokenizer.DOUBLE_STAR_EQUAL
    texts[13] = "**="
    kinds[14] = Tokenizer.TK_INTEGER
    texts[14] = "4"
    kinds[15] = Tokenizer.SEMICOLON
    texts[15] = ";"
    kinds[16] = Tokenizer.TK_IDENTITY
    texts[16] = "e"
    kinds[17] = Tokenizer.QUESTION_ARROW
    texts[17] = "?->"
    kinds[18] = Tokenizer.TK_IDENTITY
    texts[18] = "f"
    kinds[19] = Token.EOF_KIND
    texts[19] = Token.EOF_STRING

    return checkTokens("a<<=1; b>>=2; c!^=3; d**=4; e?->f", kinds, texts, 20)
}


private fun invalidIdentTest() -> int
{
    val kindsSpace: blob[sizeof(int) * 1]
    val textsSpace: blob[sizeof(pointer<char>) * 1]
    val kinds: pointer<int> = kindsSpace as pointer<int>
    val texts: pointer<pointer<char>> = textsSpace as pointer<pointer<char>>

    kinds[0] = -LexState.DEFAULT
    texts[0] = "123abc"

    if checkTokens("123abc", kinds, texts, 1) != 0:
        return 1

    val tokens: TokenList = Tokenizer.tokenize("123abc")
    val token: pointer<Token> = tokens.get(0)

    if !String.streq(token.errorInfo, "invalid identity name: 123abc"):
        return 1

    return 0
}


private fun dotIncDecTest() -> int
{
    val kindsSpace: blob[sizeof(int) * 10]
    val textsSpace: blob[sizeof(pointer<char>) * 10]
    val kinds: pointer<int> = kindsSpace as pointer<int>
    val texts: pointer<pointer<char>> = textsSpace as pointer<pointer<char>>

    kinds[0] = Tokenizer.TK_IDENTITY
    texts[0] = "a"
    kinds[1] = Tokenizer.DOUBLE_DOT
    texts[1] = ".."
    kinds[2] = Tokenizer.TK_IDENTITY
    texts[2] = "b"
    kinds[3] = Tokenizer.DOT
    texts[3] = "."
    kinds[4] = Tokenizer.TK_IDENTITY
    texts[4] = "c"
    kinds[5] = Tokenizer.DOUBLE_PLUS
    texts[5] = "++"
    kinds[6] = Tokenizer.TK_IDENTITY
    texts[6] = "d"
    kinds[7] = Tokenizer.DOUBLE_MINUS
    texts[7] = "--"
    kinds[8] = Tokenizer.TK_IDENTITY
    texts[8] = "e"
    kinds[9] = Token.EOF_KIND
    texts[9] = Token.EOF_STRING

    return checkTokens("a..b . c ++d -- e", kinds, texts, 10)
}


private fun arrayExpressionTest() -> int
{
    val kindsSpace: blob[sizeof(int) * 18]
    val textsSpace: blob[sizeof(pointer<char>) * 18]
    val kinds: pointer<int> = kindsSpace as pointer<int>
    val texts: pointer<pointer<char>> = textsSpace as pointer<pointer<char>>

    kinds[0] = Tokenizer.TK_IDENTITY
    texts[0] = "arr"
    kinds[1] = Tokenizer.LEFT_BRACKET
    texts[1] = "["
    kinds[2] = Tokenizer.TK_IDENTITY
    texts[2] = "i"
    kinds[3] = Tokenizer.DOUBLE_PLUS
    texts[3] = "++"
    kinds[4] = Tokenizer.RIGHT_BRACKET
    texts[4] = "]"
    kinds[5] = Tokenizer.PLUS_EQUAL
    texts[5] = "+="
    kinds[6] = Tokenizer.LEFT_PAREN
    texts[6] = "("
    kinds[7] = Tokenizer.TK_IDENTITY
    texts[7] = "x"
    kinds[8] = Tokenizer.DOUBLE_LESS
    texts[8] = "<<"
    kinds[9] = Tokenizer.TK_INTEGER
    texts[9] = "2"
    kinds[10] = Tokenizer.RIGHT_PAREN
    texts[10] = ")"
    kinds[11] = Tokenizer.DOUBLE_STAR
    texts[11] = "**"
    kinds[12] = Tokenizer.TK_INTEGER
    texts[12] = "3"
    kinds[13] = Tokenizer.MINUS
    texts[13] = "-"
    kinds[14] = Tokenizer.TK_IDENTITY
    texts[14] = "y"
    kinds[15] = Tokenizer.DOUBLE_MINUS
    texts[15] = "--"
    kinds[16] = Tokenizer.SEMICOLON
    texts[16] = ";"
    kinds[17] = Token.EOF_KIND
    texts[17] = Token.EOF_STRING

    return checkTokens("arr[i++] += (x<<2) ** 3 - y--;", kinds, texts, 18)
}


private fun unclosedBlockCommentTest() -> int
{
    val kindsSpace: blob[sizeof(int) * 5]
    val textsSpace: blob[sizeof(pointer<char>) * 5]
    val kinds: pointer<int> = kindsSpace as pointer<int>
    val texts: pointer<pointer<char>> = textsSpace as pointer<pointer<char>>

    kinds[0] = Tokenizer.TK_IDENTITY
    texts[0] = "a"
    kinds[1] = Tokenizer.EQUAL
    texts[1] = "="
    kinds[2] = Tokenizer.TK_INTEGER
    texts[2] = "1"
    kinds[3] = Tokenizer.SEMICOLON
    texts[3] = ";"
    kinds[4] = -Tokenizer.BLOCK_COMMENT_STATE
    texts[4] = ""

    return checkTokens("a=1; /* block\nstill", kinds, texts, 5)
}


private fun unclosedStringTest() -> int
{
    val kindsSpace: blob[sizeof(int) * 3]
    val textsSpace: blob[sizeof(pointer<char>) * 3]
    val kinds: pointer<int> = kindsSpace as pointer<int>
    val texts: pointer<pointer<char>> = textsSpace as pointer<pointer<char>>

    kinds[0] = Tokenizer.TK_IDENTITY
    texts[0] = "a"
    kinds[1] = Tokenizer.EQUAL
    texts[1] = "="
    kinds[2] = -Tokenizer.STRING_STATE
    texts[2] = "\n"

    return checkTokens("a = \"this is unclosed\n", kinds, texts, 3)
}


private fun unclosedCharTest() -> int
{
    val kindsSpace: blob[sizeof(int) * 3]
    val textsSpace: blob[sizeof(pointer<char>) * 3]
    val kinds: pointer<int> = kindsSpace as pointer<int>
    val texts: pointer<pointer<char>> = textsSpace as pointer<pointer<char>>

    kinds[0] = Tokenizer.TK_IDENTITY
    texts[0] = "c"
    kinds[1] = Tokenizer.EQUAL
    texts[1] = "="
    kinds[2] = -Tokenizer.CHAR_STATE
    texts[2] = "\n"

    return checkTokens("c = '\\t\n", kinds, texts, 3)
}


private fun positionTest() -> int
{
    val tokens: TokenList = Tokenizer.tokenize("foo = 42;")

    if checkTokenPosition(tokens, 0, 0, 1, 1, 3) != 0:
        return 1

    if checkTokenPosition(tokens, 1, 4, 1, 5, 1) != 0:
        return 1

    if checkTokenPosition(tokens, 2, 6, 1, 7, 2) != 0:
        return 1

    if checkTokenPosition(tokens, 3, 8, 1, 9, 1) != 0:
        return 1

    return 0
}
