module Util.BasicTest where

import IC.TestHelper

import qualified Util.Basic as UB


isIdentCharTests :: [TestCase]
isIdentCharTests = map (\(c, e) -> UB.isIdentChar c --> e) [
    ('a', True), ('z', True), ('A', True), ('Z', True), ('0', True),
    ('9', True), ('_', True), ('#', False), ('{', False), ('.', False)]

isIntLiteralTests :: [TestCase]
isIntLiteralTests = map (\(c, e) -> UB.isIntLiteral c --> e) [
    ("0", True), ("+0", True), ("-123", True), ("2323", True), ("+012232",  True),
    ("", False), ("+", False), ("--1", False), ("1.0", False),
    ("001", True), ("-0", True), ("+123", True), ("2147483647", True), ("-2147483648", True),
    ("123a", False), ("a123", False), ("++1", False), (" 1", False), ("1 ", False)]


isLongLiteralTests :: [TestCase]
isLongLiteralTests = map (\(c, e) -> UB.isLongLiteral c --> e) [
    ("0L", True), ("123l", True), ("+456L", True), ("-0123l", True), ("-0123121211l", True),
    ("0x1AL", False), ("0", False), ("123", False), ("1.0L", False), ("1e3L", False), ("123LL", False)]


isHexIntLiteralTests :: [TestCase]
isHexIntLiteralTests = map (\(c, e) -> UB.isHexIntLiteral c --> e) [
    ("0x0", True), ("0X1A", True), ("0xabc", True), ("0XFF", True), ("0x1234", True),
    ("0x", False), ("0x1G", False), ("123", False), ("0x1AL", False), ("0x1.0", False)]

isHexLongLiteralTests :: [TestCase]
isHexLongLiteralTests = map (\(c, e) -> UB.isHexLongLiteral c --> e) [
    ("0x1AL", True), ("0XFFl", True), ("0xabcL", True), ("0X1234l", True), ("0x0L", True),
    ("0x1A", False), ("123L", False), ("0x1.0L", False), ("1.0L", False), ("0x123LL", False)]

isFloatLiteralTests :: [TestCase]
isFloatLiteralTests = map (\(c, e) -> UB.isFloatLiteral c --> e) [
    ("1.0f", True), ("2.f", True), (".5f", True), ("+0.f", True), ("+.0f", True),
    ("-1.23F", True), ("3.0e2f", True), (".1e-3F", True), ("0.0f", True), ("123.f", True),
    ("1e3f", True), ("1E+3f", True), ("1E-3F", True), (".5e2f", True),

    ("1.0", False), ("1e3", False), ("1.0L", False), ("123", False), ("123L", False),
    ("0x1AL", False), ("0x1.0p0", False), ("+1.0", False),
    ("-0.5", False), ("1.0ll", False), ("1e", False), ("1e+", False), (".e1", False)]


isDoubleLiteralTests :: [TestCase]
isDoubleLiteralTests = map (\(c, e) -> UB.isDoubleLiteral c --> e) [
    ("1.0", True), ("2.", True), (".5", True), ("+0.0", True), ("-0.1", True),
    ("123.456", True), ("0.0", True), ("1e3", True), ("1E-2", True), (".1e+3", True),
    ("1E+3", True), ("1e-3", True), (".5e2", True), ("+.5", True), ("-.5", True),
    ("+1.", True), ("-1.", True), ("0.0001", True), ("123e10", True), ("-123E+10", True),
    ("+123e-10", True), ("1.e3", True), (".0e0", True), ("1e+0", True), ("1e-0", True),

    ("1.0f", False), ("2.f", False), ("1.0L", False), ("123", False), ("123L", False),
    ("0x1A", False), ("0x1.0p0", False), ("+.0f", False), ("-0.5F", False), ("123LL", False),
    ("1e", False), ("e3", False), (".e3", False), ("1.e", False), ("e", False), ("", False),
    ("+.", False), ("-.", False), ("++1.0", False), ("1..0", False), ("1.0e++3", False), ("1.0e-+", False)]

isLongDoubleLiteralTests :: [TestCase]
isLongDoubleLiteralTests = map (\(c, e) -> UB.isLongDoubleLiteral c --> e) [
    ("1.0l", True), ("2.L", True), ("1e3l", True), (".5L", True),
    ("123", False), ("123L", False), ("1.0", False), ("1e3", False)]

isNumberLiteralTest :: [TestCase]
isNumberLiteralTest =  map (\(c, e) -> UB.isNumber c --> e) [("+1e+10", True), ("123", True), ("0xFFFFFGF", False), ("123lf", False)]

isStringLiteralTests :: [TestCase]
isStringLiteralTests = map (\(c, e) -> UB.isStringLiteral c --> e) [
    ("\"hello\"", True),
    ("\"\"", True),
    ("\"\\n\"", True),
    ("\"\\t\\\"\"", True),
    ("hello", False),
    ("'a'", False),
    ("\"unterminated", False),
    ("\"z\"", True)]

isCharLiteralTests :: [TestCase]
isCharLiteralTests = map (\(c, e) -> UB.isCharLiteral c --> e) [
    ("'a'", True),
    ("'\\n'", True),
    ("'\\r'", True),
    ("'\\t'", True),
    ("'\\''", True),
    ("'\\\\'", True),
    ("'ab'", False),
    ("a", False),
    ("''", False)]

isBoolLiteralTests :: [TestCase]
isBoolLiteralTests = map (\(c, e) -> UB.isBoolLiteral c --> e) [
    ("true", True),
    ("false", True),
    ("True", True),
    ("False", True),
    ("TRUE", False),
    ("FALSE", False),
    ("truex", False),
    ("fals", False),
    ("", False)]

startRegexTest :: [TestCase]
startRegexTest = map (\(c, e) -> UB.start c --> e) [("[1-9]", "^([1-9])"), ("", "^()")]

fullRegexTest :: [TestCase]
fullRegexTest = map (\(c, e) -> UB.full c --> e) [("[1-9]", "^([1-9])$"), ("", "^()$")]

-- Match

matchTests :: [TestCase]
matchTests = map (\(pat, x, e) -> UB.match pat x --> e) [("a?", "", True), ("[0-9]*", "123a", True)]

matchFullTests :: [TestCase]
matchFullTests = map (\(pat, x, e) -> UB.matchFull pat x --> e) [("[0-9]*", "123", True), ("[0-9]*", "123abc", False)]


matchLengthTests :: [TestCase]
matchLengthTests = map (\(pat, s, e) -> UB.matchLength pat s --> e) [
    ("[0-9]+", "12345678abc", 8),
    ("[+-]?[0-9]+", "+123abc", 4),
    ("[0-9]+", "", 0),
    ("[0-9]+", "abc123", 0),
    ("[0-9]+(\\.[0-9]+)?", "12.34rest", 5),
    ("[0-9]+([eE][+-]?[0-9]+)?", "1e3x", 3)]

-- sk-2f1d95f8f63645d0ad44a8cbc9f2b66f

findMatchesTests :: [TestCase]
findMatchesTests = map (\(pat, s, e) -> UB.findMatches pat s --> e) [
    ("[0-9]+", "a 123 b 45", [("123", 2, 3), ("45", 8, 2)]),
    ("aa", "aaaa", [("aa", 0, 2),("aa", 2, 2)]),
    ("test", "test test", [("test", 0, 4),("test", 5, 4)]),
    ("test", "test test", [("test", 0, 4),("test", 5, 4)]),
    ("a.b", "axb ayb a b", [("axb", 0, 3),("ayb", 4, 3),("a b", 8, 3)]),
    ("ab+", "abbbab", [("abbb", 0, 4),("ab", 4, 2)]),
    ("[0-9]{2}", "12 345 67", [("12", 0, 2),("34", 3, 2),("67", 7, 2)]),
    ("cat|dog", "dog cat dogcat", [("dog", 0, 3),("cat", 4, 3),("dog", 8, 3),("cat", 11, 3)]),
    ("", "abc", []),
    ("aba", "ababa", [("aba", 0, 3)]),

    ("[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}",
     "Contact: alice@example.com, bob.smith+test@sub.domain.co.uk; invalid@com, c@d.io",
     [("alice@example.com", 9, 17),("bob.smith+test@sub.domain.co.uk", 28, 31),("c@d.io", 74, 6)]),

    ("你好", "说 你好 世界 你好", [("你好", 2, 2), ("你好", 8, 2)]),
    ("\\.", "a.b.c", [(".", 1, 1), (".", 3, 1)]),
    ("[A-Z]+", "ABC def G", [("ABC", 0, 3), ("G", 8, 1)]),
    ("aa", "aaa", [("aa", 0, 2)]),
    ("^", "abc", [])]
    

tests :: [TestGroup]
tests = [
    testGroup "Util.Basic.isIdentChar" isIdentCharTests,
    testGroup "Util.Basic.isIntLiteral" isIntLiteralTests,
    testGroup "Util.Basic.isLongLiteral" isLongLiteralTests,
    testGroup "Util.Basic.isHexIntLiteral" isHexIntLiteralTests,
    testGroup "Util.Basic.isHexLongLiteral" isHexLongLiteralTests,
    testGroup "Util.Basic.isFloatLiteral" isFloatLiteralTests,
    testGroup "Util.Basic.isDoubleLiteral" isDoubleLiteralTests,
    testGroup "Util.Basic.isLongDoubleLiteral" isLongDoubleLiteralTests,
    testGroup "Util.Basic.isNumber" isNumberLiteralTest,
    testGroup "Util.Basic.isStringLiteral" isStringLiteralTests,
    testGroup "Util.Basic.isCharLiteral" isCharLiteralTests,
    testGroup "Util.Basic.isBoolLiteral" isBoolLiteralTests,

    testGroup "Util.Basic.full" fullRegexTest,
    testGroup "Util.Basic.start" startRegexTest,

    testGroup "Util.Basic.match" matchTests,
    testGroup "Util.Basic.matchFull" matchFullTests,
    testGroup "Util.Basic.matchLength" matchLengthTests,

    testGroup "Util.Basic.findMatches" findMatchesTests]
