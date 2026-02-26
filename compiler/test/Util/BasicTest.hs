module Util.BasicTest where

import Util.Basic
import Test.Tasty
import Test.Tasty.HUnit
import Text.Regex.TDFA (makeRegex)


fullTests :: TestTree
fullTests = testGroup "Util.Basic.full" [
    testCase "0" $ full "" @?= "^()$",
    testCase "1" $ full "[0-9]" @?= "^([0-9])$",
    testCase "2" $ full "a" @?= "^(a)$",
    testCase "3" $ full "+/-" @?= "^(+/-)$"]


matchTests :: TestTree
matchTests = testGroup "Util.Basic.match" [
    testCase "0" $ match (makeRegex "[0-9]") "123" @?= True,
    testCase "1" $ match (makeRegex "[a-z]") "abcdef" @?= True,
    testCase "2" $ match (makeRegex "[+-]") "123" @?= False,
    testCase "3" $ match (makeRegex "[0-9]") "abc" @?= False]


isIntTests :: TestTree
isIntTests = testGroup "Util.Basic.isInt" $ map (\(name, c, e) -> testCase name $ isInt c @?= e) [
    ("0", "0", True), ("1", "+0", True), ("2", "-123", True),
    ("3", "2323", True), ("4", "+012232", True), ("5", "", False),
    ("6", "+", False), ("7", "--1", False), ("8", "1.0", False),
    ("9", "001", True), ("10", "-0", True), ("11", "+123", True),
    ("12", "2147483647", True), ("13", "-2147483648", True),
    ("14", "123a", False), ("15", "a123", False), ("16", "++1", False),
    ("17", " 1", False), ("18", "1 ", False),
    
    -- hex int
    ("19", "0x0", True), ("20", "0X1A", True), ("21", "0xabc", True), ("22", "0XFF", True),
    ("23", "0x1234", True), ("24", "0x", False), ("25", "0x1G", False),
    ("26", "123", True), ("27", "0x1AL", False), ("28", "0x1.0", False)]


isLongTests :: TestTree
isLongTests = testGroup "Util.Basic.isLong" $ map (\(name, c, e) -> testCase name $ isLong c @?= e) [
    ("0", "0L", True), ("1", "123l", True), ("2", "+456L", True), ("3", "-0123l", True),
    ("4", "-0123121211l", True), ("5", "0x1AL", True), ("6", "0", False),
    ("7", "123", False), ("8", "1.0L", False), ("9", "1e3L", False), ("10", "123LL", False),
    
    -- hex long
    ("11", "0XFFl", True), ("12", "0xabcL", True), ("13", "0X1234l", True),
    ("14", "0x0L", True), ("15", "0x1A", False), ("16", "123L", True),
    ("17", "0x1.0L", False), ("18", "1.0L", False), ("18", "0x123LL", False)]


isFloatTests :: TestTree
isFloatTests = testGroup "Util.Basic.isFloat" $ map (\(name, c, e) -> testCase name $ isFloat c @?= e) [
    ("0", "1.0f", True), ("1", "2.f", True), ("2", ".5f", True), ("3", "+0.f", True),
    ("4", "+.0f", True), ("5", "-1.23F", True), ("6", "3.0e2f", True),
    ("7", ".1e-3F", True), ("8", "0.0f", True), ("9", "123.f", True),
    ("10", "1e3f", True), ("12", "1E+3f", True), ("13", "1E-3F", True), ("14", ".5e2f", True),

    ("15", "1.0", False), ("16", "1e3", False), ("17", "1.0L", False),
    ("18", "123", False), ("19", "123L", False), ("20", "0x1AL", False),
    ("21", "0x1.0p0", False), ("22", "+1.0", False), ("23", "-0.5", False),
    ("24", "1.0ll", False), ("25", "1e", False), ("25", "1e+", False), ("26", ".e1", False)]


isDoubleTests :: TestTree
isDoubleTests = testGroup "Util.Basic.isDouble" $ map (\(name, c, e) -> testCase name $ isDouble c @?= e) [
    ("0", "1.0", True), ("1", "2.", True), ("2", ".5", True), ("3", "+0.0", True),
    ("4", "-0.1", True), ("5", "123.456", True), ("6", "0.0", True), ("7", "1e3", True),
    ("8", "1E-2", True), ("9", ".1e+3", True), ("10", "1E+3", True),
    ("11", "1e-3", True), ("12", ".5e2", True), ("13", "+.5", True), ("14", "-.5", True),
    ("15", "+1.", True), ("16", "-1.", True), ("17", "0.0001", True), ("18", "123e10", True),
    ("19", "-123E+10", True), ("20", "+123e-10", True), ("21", "1.e3", True), ("Util.Basic.isDoubl22", ".0e0", True),
    ("23", "1e+0", True), ("24", "1e-0", True), ("25", "1.0f", False), ("26", "2.f", False),
    ("27", "1.0L", False), ("28", "123", False), ("29", "123L", False),
    ("30", "0x1A", False), ("31", "0x1.0p0", False), ("32", "+.0f", False), ("33", "-0.5F", False),
    ("34", "123LL", False), ("35", "1e", False), ("36", "e3", False), ("37", ".e3", False),
    ("38", "1.e", False), ("39", "e", False), ("40", "", False), ("41", "+.", False),
    ("42", "-.", False), ("43", "++1.0", False), ("44", "1..0", False), ("45", "1.0e++3", False),
    ("46", "1.0e-+", False)]


isLongDoubleTests :: TestTree
isLongDoubleTests = testGroup "Util.Basic.isLongDouble" $ map (\(name, c, e) -> testCase name $ isLongDouble c @?= e) [
    ("0", "1.0l", True), ("1", "2.L", True), ("2", "1e3l", True), ("3", ".5L", True),
    ("4", "123", False), ("5", "123L", False), ("6", "1.0", False), ("7", "1e3", False)]


insertSpaceTests :: TestTree
insertSpaceTests = testGroup "Utils.Basic.insertSpace" $ map
    (\(name, n, expected) -> testCase name $ insertSpace n @=? expected) [
        ("0", 0, ""), ("1", 1, " "), ("3", 3, "   "), ("10", 10, "          ")]


insertTabTests :: TestTree
insertTabTests = testGroup "Utils.Basic.insertTabTests" $ map
    (\(name, n, expected) -> testCase name $ insertTab n @=? expected) [
        ("0", 0, ""), ("1", 1, "    "), ("2", 2, "        "), ("5", 5, "                    ")]


tests :: TestTree
tests = testGroup "Util.Basic" [fullTests, matchTests, isIntTests, isLongTests, isFloatTests, isDoubleTests, isLongDoubleTests, insertSpaceTests, insertTabTests]
