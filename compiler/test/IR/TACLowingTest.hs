module IR.TACLowingTest where

import IR.TAC (IRAtom(..), IRFunction(..), IRMemberType(..))
import IR.TACLowing
import Parse.ParserBasic (AccessModified(..))
import Parse.SyntaxTree (Class(..))
import Semantic.TypeEnv (FunSig(..))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map.Strict as Map
import qualified IR.TAC as TAC


stripIntSuffixTests :: TestTree
stripIntSuffixTests = testGroup "IR.TACLowing.stripIntSuffix" $ map (uncurry testCase) [
    ("0", stripIntSuffix "10L" @?= "10"),
    ("1", stripIntSuffix "10l" @?= "10"),
    ("2", stripIntSuffix "10" @?= "10"),
    ("3", stripIntSuffix "0xFFL" @?= "0xFF")
    ]


stripFloatSuffixTests :: TestTree
stripFloatSuffixTests = testGroup "IR.TACLowing.stripFloatSuffix" $ map (uncurry testCase) [
    ("0", stripFloatSuffix "1.0f" @?= "1.0"),
    ("1", stripFloatSuffix "2.0F" @?= "2.0"),
    ("2", stripFloatSuffix "3.0L" @?= "3.0"),
    ("3", stripFloatSuffix "4.0" @?= "4.0")
    ]


readIntegerLiteralTests :: TestTree
readIntegerLiteralTests = testGroup "IR.TACLowing.readIntegerLiteral" $ map (uncurry testCase) [
    ("0", readIntegerLiteral "123" @?= Just 123),
    ("1", readIntegerLiteral "0x10" @?= Just 16),
    ("2", readIntegerLiteral "0XffL" @?= Just 255),
    ("3", readIntegerLiteral "0x1g" @?= Nothing)
    ]


wrapIntTests :: TestTree
wrapIntTests = testGroup "IR.TACLowing.wrapInt" $ map (uncurry testCase) [
    ("0", wrapInt (-128, 127) 130 @?= (-126)),
    ("1", wrapInt (-128, 127) (-129) @?= 127),
    ("2", wrapInt (0, 10) 12 @?= 1),
    ("3", wrapInt (0, 10) 5 @?= 5)
    ]


lookupParamIndexTests :: TestTree
lookupParamIndexTests = testGroup "IR.TACLowing.lookupParamIndex" $ map (uncurry testCase) [
    ("0", lookupParamIndex (Int32C 1) Map.empty @?= Nothing),
    ("1", do
        let mp = Map.fromList [(0, Int32C 1), (1, Int32C 2)]
        lookupParamIndex (Int32C 2) mp @?= Just 1),
    ("2", do
        let mp = Map.fromList [(1, Int32C 1), (2, Int32C 1)]
        lookupParamIndex (Int32C 1) mp @?= Just 1),
    ("3", do
        let mp = Map.fromList [(0, Int32C 1)]
        lookupParamIndex (Int32C 3) mp @?= Nothing)
    ]


defaultAtomForClassTests :: TestTree
defaultAtomForClassTests = testGroup "IR.TACLowing.defaultAtomForClass" $ map (uncurry testCase) [
    ("0", defaultAtomForClass Int32T @?= Just (Int32C 0)),
    ("1", defaultAtomForClass Bool @?= Just (BoolC False)),
    ("2", defaultAtomForClass (Class ["java", "lang", "String"] []) @?= Just (StringC "")),
    ("3", defaultAtomForClass (Class ["My", "Type"] []) @?= Nothing)
    ]


detectMainKindTests :: TestTree
detectMainKindTests = testGroup "IR.TACLowing.detectMainKind" $ map (uncurry testCase) [
    ("0", do
        let out = detectMainKind ["pkg", "MainX"] [mkFun "notMain" [] Int32T]
        out @?= TAC.NoMain),
    ("1", do
        let out = detectMainKind ["pkg", "MainX"] [mkFun "main" [] Int32T]
        out @?= TAC.MainInt ["pkg", "MainX", "main"]),
    ("2", do
        let out = detectMainKind ["pkg", "MainX"] [mkFun "main" [] Int32T, mkFun "main" [] Void]
        out @?= TAC.MainVoid ["pkg", "MainX", "main"]),
    ("3", do
        let out = detectMainKind ["pkg", "MainX"] [mkFun "main" [Array (Class ["java", "lang", "String"] []) 1] Void]
        out @?= TAC.MainVoidArgs ["pkg", "MainX", "main"])
    ]
    where
        mkFun :: String -> [Class] -> Class -> IRFunction
        mkFun name params ret =
            IRFunction (Public, []) name (FunSig params ret) Map.empty [] MemberClass


tests :: TestTree
tests = testGroup "IR.TACLowing" [
    stripIntSuffixTests,
    stripFloatSuffixTests,
    readIntegerLiteralTests,
    wrapIntTests,
    lookupParamIndexTests,
    defaultAtomForClassTests,
    detectMainKindTests
    ]
