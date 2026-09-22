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

#file.outerClass("TokenNormalizerTest")
package xlang.compiler.lexer

import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.test.TestCase
import xlang.test.TestGroup
import xlang.test.TestUnion
import xlang.util.string.String


val TEST_GROUP: pointer<TestGroup> = genTest()


fun genTest() -> pointer<TestGroup>
{
    val result: pointer<TestGroup> = new TestGroup("xlang.compiler.lexer.TokenNormalizer")
    val canonicalizeTC: pointer<TestCase> = new TestCase("canonicalize", canonicalizeTest)
    val normalizeTC: pointer<TestCase> = new TestCase("normalize", normalizeTest)
    val canonicalizeUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, canonicalizeTC, null)
    val normalizeUnion: pointer<TestUnion> = new TestUnion(TestCase.TYPE, normalizeTC, null)

    result.addTestUnion(canonicalizeUnion)
    result.addTestUnion(normalizeUnion)

    return result
}


private fun canonicalizeTest() -> int
{
    var result: int = canonicalizeLeadingLineTerminatorTest()

    if result != 0:
        return 1

    result = canonicalizeStatementSeparatorTest()

    if result != 0:
        return 2

    result = canonicalizeParenthesisSemicolonTest()

    if result != 0:
        return 3

    result = canonicalizeBracketSemicolonTest()

    if result != 0:
        return 4

    result = canonicalizeFilePathTest()

    if result != 0:
        return 5

    return 0
}


private fun canonicalizeLeadingLineTerminatorTest() -> int
{
    val raw: pointer<TokenList> = Tokenizer.tokenize("\n\nfoo")
    val tokens: pointer<TokenList> = TokenNormalizer.canonicalize(raw)

    if tokens.length() != 3:
        return 1

    val ident: pointer<Token> = tokens.get(0)

    if ident.kind != Tokenizer.TK_IDENTIFIER:
        return 2

    if !String.streq(ident.text, "foo"):
        return 3

    val terminator: pointer<Token> = tokens.get(1)

    if terminator.kind != Tokenizer.TK_LINE_TERMINATOR:
        return 4

    val eof: pointer<Token> = tokens.get(2)

    if !eof.isEOF():
        return 5

    return 0
}


private fun canonicalizeStatementSeparatorTest() -> int
{
    val raw: pointer<TokenList> = Tokenizer.tokenize("foo\n\n;bar;\n;baz;;qux;;")
    val tokens: pointer<TokenList> = TokenNormalizer.canonicalize(raw)

    if tokens.length() != 9:
        return 1

    val foo: pointer<Token> = tokens.get(0)

    if foo.kind != Tokenizer.TK_IDENTIFIER || !String.streq(foo.text, "foo"):
        return 2

    val sep1: pointer<Token> = tokens.get(1)

    if sep1.kind != Tokenizer.TK_LINE_TERMINATOR:
        return 3

    val bar: pointer<Token> = tokens.get(2)

    if bar.kind != Tokenizer.TK_IDENTIFIER || !String.streq(bar.text, "bar"):
        return 4

    val sep2: pointer<Token> = tokens.get(3)

    if sep2.kind != Tokenizer.TK_LINE_TERMINATOR:
        return 5

    val baz: pointer<Token> = tokens.get(4)

    if baz.kind != Tokenizer.TK_IDENTIFIER || !String.streq(baz.text, "baz"):
        return 6

    val sep3: pointer<Token> = tokens.get(5)

    if sep3.kind != Tokenizer.TK_LINE_TERMINATOR:
        return 7

    val qux: pointer<Token> = tokens.get(6)

    if qux.kind != Tokenizer.TK_IDENTIFIER || !String.streq(qux.text, "qux"):
        return 8

    val sep4: pointer<Token> = tokens.get(7)

    if sep4.kind != Tokenizer.TK_LINE_TERMINATOR:
        return 9

    val eof: pointer<Token> = tokens.get(8)

    if !eof.isEOF():
        return 10

    return 0
}


private fun checkPairSemicolon(input: pointer<char>, leftKind: int, rightKind: int) -> int
{
    val raw: pointer<TokenList> = Tokenizer.tokenize(input)
    val tokens: pointer<TokenList> = TokenNormalizer.canonicalize(raw)

    if tokens.length() != 9:
        return 1

    val left: pointer<Token> = tokens.get(0)

    if left.kind != Tokenizer.TK_IDENTIFIER || !String.streq(left.text, "a"):
        return 2

    val open: pointer<Token> = tokens.get(1)

    if open.kind != leftKind:
        return 3

    val first: pointer<Token> = tokens.get(2)

    if first.kind != Tokenizer.TK_IDENTIFIER || !String.streq(first.text, "b"):
        return 4

    val semicolon: pointer<Token> = tokens.get(3)

    if semicolon.kind != Tokenizer.SEMICOLON:
        return 5

    val second: pointer<Token> = tokens.get(4)

    if second.kind != Tokenizer.TK_IDENTIFIER || !String.streq(second.text, "c"):
        return 6

    val close: pointer<Token> = tokens.get(5)

    if close.kind != rightKind:
        return 7

    val right: pointer<Token> = tokens.get(6)

    if right.kind != Tokenizer.TK_IDENTIFIER || !String.streq(right.text, "d"):
        return 8

    val terminator: pointer<Token> = tokens.get(7)

    if terminator.kind != Tokenizer.TK_LINE_TERMINATOR:
        return 9

    val eof: pointer<Token> = tokens.get(8)

    if !eof.isEOF():
        return 10

    return 0
}


private fun canonicalizeParenthesisSemicolonTest() -> int =
    checkPairSemicolon("a(b;c)d", Tokenizer.LEFT_PAREN, Tokenizer.RIGHT_PAREN)


private fun canonicalizeBracketSemicolonTest() -> int =
    checkPairSemicolon("a[b;c]d", Tokenizer.LEFT_BRACKET, Tokenizer.RIGHT_BRACKET)


private fun canonicalizeFilePathTest() -> int
{
    return 0
}


private fun normalizeTest() -> int
{
    var result: int = normalizeRightBraceInsertLineTerminatorTest()

    if result != 0:
        return 1

    result = normalizeRightBraceKeepLineTerminatorTest()

    if result != 0:
        return 2

    result = normalizeRightBraceKeepTrailingLineTerminatorTest()

    if result != 0:
        return 3

    result = normalizeFilePathTest()

    if result != 0:
        return 4

    result = normalizeGenericTypeBeforeVarLineTerminatorTest()

    if result != 0:
        return 5

    result = normalizeGenericTypeBeforeValLineTerminatorTest()

    if result != 0:
        return 6

    result = normalizeRightParenBeforeStaticFunLineTerminatorTest()

    if result != 0:
        return 7

    result = normalizeRightParenBeforeRightBraceLineTerminatorTest()

    if result != 0:
        return 8

    result = normalizeRightBracketKeepsLineTerminatorTest()

    if result != 0:
        return 9

    result = normalizePostfixIncrementKeepsLineTerminatorTest()

    if result != 0:
        return 10

    result = normalizePostfixDecrementKeepsLineTerminatorTest()

    if result != 0:
        return 11

    result = normalizeDotStarKeepsLineTerminatorTest()

    if result != 0:
        return 12

    return 0
}


private fun checkRightBraceLineTerminator(input: pointer<char>) -> int
{
    val raw: pointer<TokenList> = Tokenizer.tokenize(input)
    val tokens: pointer<TokenList> = TokenNormalizer.normalize(raw)

    if tokens.length() != 7:
        return 1

    val left: pointer<Token> = tokens.get(0)

    if left.kind != Tokenizer.TK_IDENTIFIER || !String.streq(left.text, "a"):
        return 2

    val terminator: pointer<Token> = tokens.get(1)

    if terminator.kind != Tokenizer.TK_LINE_TERMINATOR:
        return 3

    val rightBrace: pointer<Token> = tokens.get(2)

    if rightBrace.kind != Tokenizer.RIGHT_BRACE:
        return 4

    val afterBrace: pointer<Token> = tokens.get(3)

    if afterBrace.kind != Tokenizer.TK_LINE_TERMINATOR:
        return 5

    val right: pointer<Token> = tokens.get(4)

    if right.kind != Tokenizer.TK_IDENTIFIER || !String.streq(right.text, "b"):
        return 6

    val finalTerminator: pointer<Token> = tokens.get(5)

    if finalTerminator.kind != Tokenizer.TK_LINE_TERMINATOR:
        return 7

    val eof: pointer<Token> = tokens.get(6)

    if !eof.isEOF():
        return 8

    return 0
}


private fun normalizeRightBraceInsertLineTerminatorTest() -> int =
    checkRightBraceLineTerminator("a}b")


private fun normalizeRightBraceKeepLineTerminatorTest() -> int =
    checkRightBraceLineTerminator("a\n}b")


private fun normalizeRightBraceKeepTrailingLineTerminatorTest() -> int =
    checkRightBraceLineTerminator("a}\nb")


private fun checkGenericTypeMemberLineTerminator(input: pointer<char>, nextKind: int) -> int
{
    val raw: pointer<TokenList> = Tokenizer.tokenize(input)
    val tokens: pointer<TokenList> = TokenNormalizer.normalize(raw)

    if tokens.length() != 14:
        return 1

    val varToken: pointer<Token> = tokens.get(0)

    if varToken.kind != Tokenizer.KW_VAR:
        return 2

    val fieldName: pointer<Token> = tokens.get(1)

    if !String.streq(fieldName.text, "filePath"):
        return 3

    val colon: pointer<Token> = tokens.get(2)

    if colon.kind != Tokenizer.COLON:
        return 4

    val typeName: pointer<Token> = tokens.get(3)

    if !String.streq(typeName.text, "pointer"):
        return 5

    val less: pointer<Token> = tokens.get(4)

    if less.kind != Tokenizer.LESS:
        return 6

    val typeArg: pointer<Token> = tokens.get(5)

    if !String.streq(typeArg.text, "char"):
        return 7

    val greater: pointer<Token> = tokens.get(6)

    if greater.kind != Tokenizer.GREATER:
        return 8

    val terminator: pointer<Token> = tokens.get(7)

    if terminator.kind != Tokenizer.TK_LINE_TERMINATOR:
        return 9

    val next: pointer<Token> = tokens.get(8)

    if next.kind != nextKind:
        return 10

    val nextName: pointer<Token> = tokens.get(9)

    if !String.streq(nextName.text, "offset"):
        return 11

    val finalTerminator: pointer<Token> = tokens.get(12)

    if finalTerminator.kind != Tokenizer.TK_LINE_TERMINATOR:
        return 12

    val eof: pointer<Token> = tokens.get(13)

    if !eof.isEOF():
        return 13

    return 0
}


private fun normalizeGenericTypeBeforeVarLineTerminatorTest() -> int =
    checkGenericTypeMemberLineTerminator("var filePath: pointer<char>\nvar offset: int", Tokenizer.KW_VAR)


private fun normalizeGenericTypeBeforeValLineTerminatorTest() -> int =
    checkGenericTypeMemberLineTerminator("var filePath: pointer<char>\nval offset: int", Tokenizer.KW_VAL)


private fun normalizeRightParenBeforeStaticFunLineTerminatorTest() -> int
{
    val raw: pointer<TokenList> = Tokenizer.tokenize("value = make()\nstatic fun next()")
    val tokens: pointer<TokenList> = TokenNormalizer.normalize(raw)

    if tokens.length() < 8:
        return 1

    val rightParen: pointer<Token> = tokens.get(4)

    if rightParen.kind != Tokenizer.RIGHT_PAREN:
        return 2

    val terminator: pointer<Token> = tokens.get(5)

    if terminator.kind != Tokenizer.TK_LINE_TERMINATOR:
        return 3

    val staticToken: pointer<Token> = tokens.get(6)

    if staticToken.kind != Tokenizer.KW_STATIC:
        return 4

    val funToken: pointer<Token> = tokens.get(7)

    if funToken.kind != Tokenizer.KW_FUN:
        return 5

    return 0
}


private fun normalizeRightParenBeforeRightBraceLineTerminatorTest() -> int
{
    val raw: pointer<TokenList> = Tokenizer.tokenize("value = make()\n}")
    val tokens: pointer<TokenList> = TokenNormalizer.normalize(raw)

    if tokens.length() < 7:
        return 1

    val rightParen: pointer<Token> = tokens.get(4)

    if rightParen.kind != Tokenizer.RIGHT_PAREN:
        return 2

    val terminator: pointer<Token> = tokens.get(5)

    if terminator.kind != Tokenizer.TK_LINE_TERMINATOR:
        return 3

    val rightBrace: pointer<Token> = tokens.get(6)

    if rightBrace.kind != Tokenizer.RIGHT_BRACE:
        return 4

    return 0
}


private fun normalizeRightBracketKeepsLineTerminatorTest() -> int
{
    val raw: pointer<TokenList> = Tokenizer.tokenize("val first: blob[4]\nval second: int")
    val tokens: pointer<TokenList> = TokenNormalizer.normalize(raw)

    if tokens.length() < 9:
        return 1

    val rightBracket: pointer<Token> = tokens.get(6)

    if rightBracket.kind != Tokenizer.RIGHT_BRACKET:
        return 2

    val terminator: pointer<Token> = tokens.get(7)

    if terminator.kind != Tokenizer.TK_LINE_TERMINATOR:
        return 3

    val nextVal: pointer<Token> = tokens.get(8)

    if nextVal.kind != Tokenizer.KW_VAL:
        return 4

    return 0
}


private fun normalizePostfixIncrementKeepsLineTerminatorTest() -> int
{
    val raw: pointer<TokenList> = Tokenizer.tokenize("cursor.line++\ncursor.column = 0")
    val tokens: pointer<TokenList> = TokenNormalizer.normalize(raw)

    if tokens.length() < 6:
        return 1

    val increment: pointer<Token> = tokens.get(3)

    if increment.kind != Tokenizer.DOUBLE_PLUS:
        return 2

    val terminator: pointer<Token> = tokens.get(4)

    if terminator.kind != Tokenizer.TK_LINE_TERMINATOR:
        return 3

    val cursor: pointer<Token> = tokens.get(5)

    if cursor.kind != Tokenizer.TK_IDENTIFIER:
        return 4

    return 0
}


private fun normalizePostfixDecrementKeepsLineTerminatorTest() -> int
{
    val raw: pointer<TokenList> = Tokenizer.tokenize("i--\nitems.removeAt(i)")
    val tokens: pointer<TokenList> = TokenNormalizer.normalize(raw)

    if tokens.length() < 4:
        return 1

    val decrement: pointer<Token> = tokens.get(1)

    if decrement.kind != Tokenizer.DOUBLE_MINUS:
        return 2

    val terminator: pointer<Token> = tokens.get(2)

    if terminator.kind != Tokenizer.TK_LINE_TERMINATOR:
        return 3

    val items: pointer<Token> = tokens.get(3)

    if items.kind != Tokenizer.TK_IDENTIFIER:
        return 4

    return 0
}


private fun normalizeDotStarKeepsLineTerminatorTest() -> int
{
    val raw: pointer<TokenList> = Tokenizer.tokenize("Math.*\nnext")
    val tokens: pointer<TokenList> = TokenNormalizer.normalize(raw)

    if tokens.length() < 5:
        return 1

    val dot: pointer<Token> = tokens.get(1)

    if dot.kind != Tokenizer.DOT:
        return 2

    val star: pointer<Token> = tokens.get(2)

    if star.kind != Tokenizer.STAR:
        return 3

    val terminator: pointer<Token> = tokens.get(3)

    if terminator.kind != Tokenizer.TK_LINE_TERMINATOR:
        return 4

    val next: pointer<Token> = tokens.get(4)

    if next.kind != Tokenizer.TK_IDENTIFIER || !String.streq(next.text, "next"):
        return 5

    return 0
}


private fun normalizeFilePathTest() -> int
{
    return 0
}
