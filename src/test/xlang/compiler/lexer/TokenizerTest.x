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

@file.class("TokenizerTest")
package xlang.compiler.lexer

import xlang.compiler.lexer.Tokenizer
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.lexer.TokenizeFSM
import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion
import xlang.util.string.String


val TEST_GROUP: pointer<TestGroup> = genTest()


fun genTest() -> pointer<TestGroup>
{
    val result: pointer<TestGroup> = new TestGroup("xlang.compiler.Tokenizer")
    val tokenizeTC: pointer<TestCase> = new TestCase("tokenize", tokenizeTest)
    val fullTokenizeTC: pointer<TestCase> = new TestCase("fullTokenize", fullTokenizeTest)
    val tokenizeUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, tokenizeTC, null)
    val fullTokenizeUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, fullTokenizeTC, null)

    result.addTestUnion(tokenizeUnion)
    result.addTestUnion(fullTokenizeUnion)

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

    result = compoundSymbolsTest()

    if result != 0:
        return 14

    return 0
}


private fun fullTokenizeTest() -> int
{
    var result: int = fullTokenizeSymbolLineTerminatorTest()

    if result != 0:
        return 1

    result = fullTokenizeRightBraceLineTerminatorTest()

    if result != 0:
        return 2

    return 0
}


private fun checkTokenList(tokens: pointer<TokenList>, kinds: pointer<int>, texts: pointer<pointer<char>>, length: int) -> int
{
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


private fun checkTokens(input: pointer<char>, kinds: pointer<int>, texts: pointer<pointer<char>>, length: int) -> int
{
    val tokens: pointer<TokenList> = Tokenizer.tokenize(input)

    return checkTokenList(tokens, kinds, texts, length)
}


private fun checkTokenAt(tokens: pointer<TokenList>, index: int, kind: int, text: pointer<char>) -> int
{
    if tokens == null || index < 0 || index >= tokens.length():
        return 1

    val token: pointer<Token> = tokens.get(index)

    if token == null || token.kind != kind:
        return 1

    if !String.streq(token.text, text):
        return 1

    return 0
}


private fun checkFullTokens(input: pointer<char>, kinds: pointer<int>, texts: pointer<pointer<char>>, length: int) -> int
{
    val tokens: pointer<TokenList> = Tokenizer.fullTokenize(input)

    return checkTokenList(tokens, kinds, texts, length)
}


private fun checkTokenPosition(tokens: pointer<TokenList>, index: int, offset: int, line: int, column: int, length: int) -> int
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


private fun fullTokenizeSymbolLineTerminatorTest() -> int
{
    val kindsSpace: blob[sizeof(int) * 5]
    val textsSpace: blob[sizeof(pointer<char>) * 5]
    val kinds: pointer<int> = kindsSpace as pointer<int>
    val texts: pointer<pointer<char>> = textsSpace as pointer<pointer<char>>

    kinds[0] = Tokenizer.TK_IDENTIFIER
    texts[0] = "a"
    kinds[1] = Tokenizer.PLUS
    texts[1] = "+"
    kinds[2] = Tokenizer.TK_IDENTIFIER
    texts[2] = "b"
    kinds[3] = Tokenizer.TK_LINE_TERMINATOR
    texts[3] = "\n"
    kinds[4] = Token.EOF_KIND
    texts[4] = Token.EOF_STRING

    return checkFullTokens("a\n+\nb", kinds, texts, 5)
}


private fun fullTokenizeRightBraceLineTerminatorTest() -> int
{
    val kindsSpace: blob[sizeof(int) * 7]
    val textsSpace: blob[sizeof(pointer<char>) * 7]
    val kinds: pointer<int> = kindsSpace as pointer<int>
    val texts: pointer<pointer<char>> = textsSpace as pointer<pointer<char>>

    kinds[0] = Tokenizer.TK_IDENTIFIER
    texts[0] = "a"
    kinds[1] = Tokenizer.TK_LINE_TERMINATOR
    texts[1] = null
    kinds[2] = Tokenizer.RIGHT_BRACE
    texts[2] = "}"
    kinds[3] = Tokenizer.TK_LINE_TERMINATOR
    texts[3] = null
    kinds[4] = Tokenizer.TK_IDENTIFIER
    texts[4] = "b"
    kinds[5] = Tokenizer.TK_LINE_TERMINATOR
    texts[5] = "\n"
    kinds[6] = Token.EOF_KIND
    texts[6] = Token.EOF_STRING

    return checkFullTokens("a}b", kinds, texts, 7)
}


private fun commentsTest() -> int
{
    val kindsSpace: blob[sizeof(int) * 12]
    val textsSpace: blob[sizeof(pointer<char>) * 12]
    val kinds: pointer<int> = kindsSpace as pointer<int>
    val texts: pointer<pointer<char>> = textsSpace as pointer<pointer<char>>

    kinds[0] = Tokenizer.TK_IDENTIFIER
    texts[0] = "a"
    kinds[1] = Tokenizer.EQUAL
    texts[1] = "="
    kinds[2] = Tokenizer.TK_INTEGER
    texts[2] = "1"
    kinds[3] = Tokenizer.TK_LINE_TERMINATOR
    texts[3] = null
    kinds[4] = Tokenizer.TK_IDENTIFIER
    texts[4] = "b"
    kinds[5] = Tokenizer.EQUAL
    texts[5] = "="
    kinds[6] = Tokenizer.TK_INTEGER
    texts[6] = "2"
    kinds[7] = Tokenizer.TK_LINE_TERMINATOR
    texts[7] = null
    kinds[8] = Tokenizer.TK_IDENTIFIER
    texts[8] = "c"
    kinds[9] = Tokenizer.EQUAL
    texts[9] = "="
    kinds[10] = Tokenizer.TK_INTEGER
    texts[10] = "3"
    kinds[11] = Token.EOF_KIND
    texts[11] = Token.EOF_STRING

    return checkTokens("a=1/*x*/b=2//y\nc=3", kinds, texts, 12)
}


private fun charEscapeTest() -> int
{
    val kindsSpace: blob[sizeof(int) * 13]
    val textsSpace: blob[sizeof(pointer<char>) * 13]
    val kinds: pointer<int> = kindsSpace as pointer<int>
    val texts: pointer<pointer<char>> = textsSpace as pointer<pointer<char>>

    kinds[0] = Tokenizer.TK_IDENTIFIER
    texts[0] = "a"
    kinds[1] = Tokenizer.EQUAL
    texts[1] = "="
    kinds[2] = Tokenizer.TK_CHAR
    texts[2] = "\\n"
    kinds[3] = Tokenizer.SEMICOLON
    texts[3] = ";"
    kinds[4] = Tokenizer.TK_IDENTIFIER
    texts[4] = "b"
    kinds[5] = Tokenizer.EQUAL
    texts[5] = "="
    kinds[6] = Tokenizer.TK_CHAR
    texts[6] = "\\'"
    kinds[7] = Tokenizer.SEMICOLON
    texts[7] = ";"
    kinds[8] = Tokenizer.TK_IDENTIFIER
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
    val kindsSpace: blob[sizeof(int) * 22]
    val textsSpace: blob[sizeof(pointer<char>) * 22]
    val kinds: pointer<int> = kindsSpace as pointer<int>
    val texts: pointer<pointer<char>> = textsSpace as pointer<pointer<char>>

    kinds[0] = Tokenizer.TK_IDENTIFIER
    texts[0] = "str1"
    kinds[1] = Tokenizer.EQUAL
    texts[1] = "="
    kinds[2] = Tokenizer.TK_STRING
    texts[2] = "hello, this is my own program language!!!"
    kinds[3] = Tokenizer.SEMICOLON
    texts[3] = ";"
    kinds[4] = Tokenizer.TK_IDENTIFIER
    texts[4] = "str2"
    kinds[5] = Tokenizer.EQUAL
    texts[5] = "="
    kinds[6] = Tokenizer.TK_STRING
    texts[6] = "hello, world"
    kinds[7] = Tokenizer.SEMICOLON
    texts[7] = ";"
    kinds[8] = Tokenizer.TK_LINE_TERMINATOR
    texts[8] = null
    kinds[9] = Tokenizer.TK_IDENTIFIER
    texts[9] = "str3"
    kinds[10] = Tokenizer.EQUAL
    texts[10] = "="
    kinds[11] = Tokenizer.TK_STRING
    texts[11] = "\\0"
    kinds[12] = Tokenizer.SEMICOLON
    texts[12] = ";"
    kinds[13] = Tokenizer.TK_LINE_TERMINATOR
    texts[13] = null
    kinds[14] = Tokenizer.TK_IDENTIFIER
    texts[14] = "str4"
    kinds[15] = Tokenizer.EQUAL
    texts[15] = "="
    kinds[16] = Tokenizer.KW_NULL
    texts[16] = "null"
    kinds[17] = Tokenizer.SEMICOLON
    texts[17] = ";"
    kinds[18] = Tokenizer.TK_IDENTIFIER
    texts[18] = "q"
    kinds[19] = Tokenizer.EQUAL
    texts[19] = "="
    kinds[20] = Tokenizer.TK_STRING
    texts[20] = "a\\\"b"
    kinds[21] = Token.EOF_KIND
    texts[21] = Token.EOF_STRING

    return checkTokens("str1=\"hello, this is my own program language!!!\"; str2=\"hello, world\"; /* this is a test */ str3=\"\\0\"; /* /* perfect */ str4=null; q=\"a\\\"b\"", kinds, texts, 22)
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
    val kindsSpace: blob[sizeof(int) * 24]
    val textsSpace: blob[sizeof(pointer<char>) * 24]
    val kinds: pointer<int> = kindsSpace as pointer<int>
    val texts: pointer<pointer<char>> = textsSpace as pointer<pointer<char>>

    kinds[0] = Tokenizer.KW_FOR
    texts[0] = "for"
    kinds[1] = Tokenizer.LEFT_PAREN
    texts[1] = "("
    kinds[2] = Tokenizer.TK_IDENTIFIER
    texts[2] = "x"
    kinds[3] = Tokenizer.EQUAL
    texts[3] = "="
    kinds[4] = Tokenizer.TK_IDENTIFIER
    texts[4] = "a_address"
    kinds[5] = Tokenizer.DOT
    texts[5] = "."
    kinds[6] = Tokenizer.TK_IDENTIFIER
    texts[6] = "getSize"
    kinds[7] = Tokenizer.LEFT_PAREN
    texts[7] = "("
    kinds[8] = Tokenizer.RIGHT_PAREN
    texts[8] = ")"
    kinds[9] = Tokenizer.SEMICOLON
    texts[9] = ";"
    kinds[10] = Tokenizer.TK_IDENTIFIER
    texts[10] = "x"
    kinds[11] = Tokenizer.LESS_EQUAL
    texts[11] = "<="
    kinds[12] = Tokenizer.TK_INTEGER
    texts[12] = "10"
    kinds[13] = Tokenizer.SEMICOLON
    texts[13] = ";"
    kinds[14] = Tokenizer.TK_IDENTIFIER
    texts[14] = "x"
    kinds[15] = Tokenizer.STAR_EQUAL
    texts[15] = "*="
    kinds[16] = Tokenizer.TK_INTEGER
    texts[16] = "2"
    kinds[17] = Tokenizer.RIGHT_PAREN
    texts[17] = ")"
    kinds[18] = Tokenizer.TK_LINE_TERMINATOR
    texts[18] = null
    kinds[19] = Tokenizer.TK_IDENTIFIER
    texts[19] = "print"
    kinds[20] = Tokenizer.LEFT_PAREN
    texts[20] = "("
    kinds[21] = Tokenizer.TK_IDENTIFIER
    texts[21] = "x"
    kinds[22] = Tokenizer.RIGHT_PAREN
    texts[22] = ")"
    kinds[23] = Token.EOF_KIND
    texts[23] = Token.EOF_STRING

    return checkTokens("for (x = a_address.getSize(); x <= 10; x *= 2) /* iterate the list */ print(x) // this is a comment too", kinds, texts, 24)
}


private fun symbolsTest() -> int
{
    val kindsSpace: blob[sizeof(int) * 28]
    val textsSpace: blob[sizeof(pointer<char>) * 28]
    val kinds: pointer<int> = kindsSpace as pointer<int>
    val texts: pointer<pointer<char>> = textsSpace as pointer<pointer<char>>

    kinds[0] = Tokenizer.TK_IDENTIFIER
    texts[0] = "a"
    kinds[1] = Tokenizer.DOUBLE_LESS_EQUAL
    texts[1] = "<<="
    kinds[2] = Tokenizer.TK_INTEGER
    texts[2] = "1"
    kinds[3] = Tokenizer.SEMICOLON
    texts[3] = ";"
    kinds[4] = Tokenizer.TK_IDENTIFIER
    texts[4] = "b"
    kinds[5] = Tokenizer.DOUBLE_GREATER_EQUAL
    texts[5] = ">>="
    kinds[6] = Tokenizer.TK_INTEGER
    texts[6] = "2"
    kinds[7] = Tokenizer.SEMICOLON
    texts[7] = ";"
    kinds[8] = Tokenizer.TK_IDENTIFIER
    texts[8] = "c"
    kinds[9] = Tokenizer.BANG_CARET_EQUAL
    texts[9] = "!^="
    kinds[10] = Tokenizer.TK_INTEGER
    texts[10] = "3"
    kinds[11] = Tokenizer.SEMICOLON
    texts[11] = ";"
    kinds[12] = Tokenizer.TK_IDENTIFIER
    texts[12] = "d"
    kinds[13] = Tokenizer.DOUBLE_STAR_EQUAL
    texts[13] = "**="
    kinds[14] = Tokenizer.TK_INTEGER
    texts[14] = "4"
    kinds[15] = Tokenizer.SEMICOLON
    texts[15] = ";"
    kinds[16] = Tokenizer.TK_IDENTIFIER
    texts[16] = "e"
    kinds[17] = Tokenizer.ARROW
    texts[17] = "->"
    kinds[18] = Tokenizer.TK_IDENTIFIER
    texts[18] = "f"
    kinds[19] = Tokenizer.SEMICOLON
    texts[19] = ";"
    kinds[20] = Tokenizer.TK_IDENTIFIER
    texts[20] = "g"
    kinds[21] = Tokenizer.TRIPLE_EQUAL
    texts[21] = "==="
    kinds[22] = Tokenizer.TK_IDENTIFIER
    texts[22] = "h"
    kinds[23] = Tokenizer.SEMICOLON
    texts[23] = ";"
    kinds[24] = Tokenizer.TK_IDENTIFIER
    texts[24] = "i"
    kinds[25] = Tokenizer.BANG_DOUBLE_EQUAL
    texts[25] = "!=="
    kinds[26] = Tokenizer.TK_IDENTIFIER
    texts[26] = "j"
    kinds[27] = Token.EOF_KIND
    texts[27] = Token.EOF_STRING

    return checkTokens("a<<=1; b>>=2; c!^=3; d**=4; e->f; g===h; i!==j", kinds, texts, 28)
}


private fun compoundSymbolsTest() -> int
{
    val tokens: pointer<TokenList> = Tokenizer.tokenize("inv !x; a xor b; a xnor b; a implies b; a nimplies b; a !&& b; a !|| b; a !^ b; a -> b; a !-> b; a <-> b; a !<-> b; a ~= b")

    if tokens == null || tokens.length() != 52:
        return 1

    if checkTokenAt(tokens, 0, Tokenizer.KW_INV, "inv") != 0:
        return 2

    if checkTokenAt(tokens, 1, Tokenizer.BANG, "!") != 0:
        return 3

    if checkTokenAt(tokens, 5, Tokenizer.KW_XOR, "xor") != 0:
        return 4

    if checkTokenAt(tokens, 9, Tokenizer.KW_XNOR, "xnor") != 0:
        return 5

    if checkTokenAt(tokens, 13, Tokenizer.KW_IMPLIES, "implies") != 0:
        return 6

    if checkTokenAt(tokens, 17, Tokenizer.KW_NIMPLIES, "nimplies") != 0:
        return 7

    if checkTokenAt(tokens, 21, Tokenizer.BANG_DOUBLE_AMPERSAND, "!&&") != 0:
        return 8

    if checkTokenAt(tokens, 25, Tokenizer.BANG_DOUBLE_PIPE, "!||") != 0:
        return 9

    if checkTokenAt(tokens, 29, Tokenizer.BANG_CARET, "!^") != 0:
        return 10

    if checkTokenAt(tokens, 33, Tokenizer.ARROW, "->") != 0:
        return 11

    if checkTokenAt(tokens, 37, Tokenizer.NOT_ARROW, "!->") != 0:
        return 12

    if checkTokenAt(tokens, 41, Tokenizer.DOUBLE_ARROW, "<->") != 0:
        return 13

    if checkTokenAt(tokens, 45, Tokenizer.BANG_DOUBLE_ARROW, "!<->") != 0:
        return 14

    if checkTokenAt(tokens, 49, Tokenizer.TILDE_EQUAL, "~=") != 0:
        return 15

    return 0
}


private fun invalidIdentTest() -> int
{
    val kindsSpace: blob[sizeof(int) * 1]
    val textsSpace: blob[sizeof(pointer<char>) * 1]
    val kinds: pointer<int> = kindsSpace as pointer<int>
    val texts: pointer<pointer<char>> = textsSpace as pointer<pointer<char>>

    kinds[0] = -TokenizeFSM.DEFAULT
    texts[0] = "123abc"

    if checkTokens("123abc", kinds, texts, 1) != 0:
        return 1

    val tokens: pointer<TokenList> = Tokenizer.tokenize("123abc")
    val token: pointer<Token> = tokens.get(0)

    if !String.streq(token.errorInfo, "invalid identifier name: 123abc"):
        return 1

    return 0
}


private fun dotIncDecTest() -> int
{
    val kindsSpace: blob[sizeof(int) * 10]
    val textsSpace: blob[sizeof(pointer<char>) * 10]
    val kinds: pointer<int> = kindsSpace as pointer<int>
    val texts: pointer<pointer<char>> = textsSpace as pointer<pointer<char>>

    kinds[0] = Tokenizer.TK_IDENTIFIER
    texts[0] = "a"
    kinds[1] = Tokenizer.DOT
    texts[1] = "."
    kinds[2] = Tokenizer.TK_IDENTIFIER
    texts[2] = "b"
    kinds[3] = Tokenizer.DOT
    texts[3] = "."
    kinds[4] = Tokenizer.TK_IDENTIFIER
    texts[4] = "c"
    kinds[5] = Tokenizer.DOUBLE_PLUS
    texts[5] = "++"
    kinds[6] = Tokenizer.TK_IDENTIFIER
    texts[6] = "d"
    kinds[7] = Tokenizer.DOUBLE_MINUS
    texts[7] = "--"
    kinds[8] = Tokenizer.TK_IDENTIFIER
    texts[8] = "e"
    kinds[9] = Token.EOF_KIND
    texts[9] = Token.EOF_STRING

    return checkTokens("a.b . c ++d -- e", kinds, texts, 10)
}


private fun arrayExpressionTest() -> int
{
    val kindsSpace: blob[sizeof(int) * 18]
    val textsSpace: blob[sizeof(pointer<char>) * 18]
    val kinds: pointer<int> = kindsSpace as pointer<int>
    val texts: pointer<pointer<char>> = textsSpace as pointer<pointer<char>>

    kinds[0] = Tokenizer.TK_IDENTIFIER
    texts[0] = "arr"
    kinds[1] = Tokenizer.LEFT_BRACKET
    texts[1] = "["
    kinds[2] = Tokenizer.TK_IDENTIFIER
    texts[2] = "i"
    kinds[3] = Tokenizer.DOUBLE_PLUS
    texts[3] = "++"
    kinds[4] = Tokenizer.RIGHT_BRACKET
    texts[4] = "]"
    kinds[5] = Tokenizer.PLUS_EQUAL
    texts[5] = "+="
    kinds[6] = Tokenizer.LEFT_PAREN
    texts[6] = "("
    kinds[7] = Tokenizer.TK_IDENTIFIER
    texts[7] = "x"
    kinds[8] = Tokenizer.LESS
    texts[8] = "<"
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
    kinds[14] = Tokenizer.TK_IDENTIFIER
    texts[14] = "y"
    kinds[15] = Tokenizer.DOUBLE_MINUS
    texts[15] = "--"
    kinds[16] = Tokenizer.SEMICOLON
    texts[16] = ";"
    kinds[17] = Token.EOF_KIND
    texts[17] = Token.EOF_STRING

    return checkTokens("arr[i++] += (x<2) ** 3 - y--;", kinds, texts, 18)
}


private fun unclosedBlockCommentTest() -> int
{
    val kindsSpace: blob[sizeof(int) * 5]
    val textsSpace: blob[sizeof(pointer<char>) * 5]
    val kinds: pointer<int> = kindsSpace as pointer<int>
    val texts: pointer<pointer<char>> = textsSpace as pointer<pointer<char>>

    kinds[0] = Tokenizer.TK_IDENTIFIER
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

    kinds[0] = Tokenizer.TK_IDENTIFIER
    texts[0] = "a"
    kinds[1] = Tokenizer.EQUAL
    texts[1] = "="
    kinds[2] = -Tokenizer.STRING_STATE
    texts[2] = "\n"

    if checkTokens("a = \"this is unclosed\n", kinds, texts, 3) != 0:
        return 1

    val tokens: pointer<TokenList> = Tokenizer.tokenize("a = \"this is unclosed\n")
    val token: pointer<Token> = tokens.get(2)

    if !String.streq(token.errorInfo, "unterminated string literal"):
        return 1

    return 0
}


private fun unclosedCharTest() -> int
{
    val kindsSpace: blob[sizeof(int) * 3]
    val textsSpace: blob[sizeof(pointer<char>) * 3]
    val kinds: pointer<int> = kindsSpace as pointer<int>
    val texts: pointer<pointer<char>> = textsSpace as pointer<pointer<char>>

    kinds[0] = Tokenizer.TK_IDENTIFIER
    texts[0] = "c"
    kinds[1] = Tokenizer.EQUAL
    texts[1] = "="
    kinds[2] = -Tokenizer.CHAR_STATE
    texts[2] = "\n"

    if checkTokens("c = '\\t\n", kinds, texts, 3) != 0:
        return 1

    val tokens: pointer<TokenList> = Tokenizer.tokenize("c = '\\t\n")
    val token: pointer<Token> = tokens.get(2)

    if !String.streq(token.errorInfo, "unterminated char literal"):
        return 1

    return 0
}


private fun positionTest() -> int
{
    val tokens: pointer<TokenList> = Tokenizer.tokenize("foo = 42;")

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
