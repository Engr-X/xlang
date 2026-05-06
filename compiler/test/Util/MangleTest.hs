module Util.MangleTest where

import Test.Tasty
import Test.Tasty.HUnit
import Parse.SyntaxTree (Class(..))
import Util.Mangle


mangleTests :: TestTree
mangleTests = testGroup "Util.Mangle.mangle" [
    testCase "mangle function no args -> v signature" $
        mangleName ["com", "wangdi"] ["Math", "main"] [] True
            @?= "_XN3com6wangdi4Math4mainEv",
    testCase "mangle field with type signature" $
        mangleName ["com", "wangdi"] ["Math", "a"] [Int32T] False
            @?= "_XN3com6wangdi4Math1aEi",
    testCase "mangle with nested class param (Class exported)" $
        mangleName ["com"] ["Fn", "f"] [Class ["pkg", "Pair"] []] True
            @?= "_XN3com2Fn1fEN3pkg4PairE",
    testCase "mangle pointer params PiPi" $
        mangleName [] ["f"] [Pointer Int32T, Pointer Int32T] True
            @?= "_X1fPiPi",
    testCase "mangle pointer param PPi" $
        mangleName [] ["f"] [Pointer (Pointer Int32T)] True
            @?= "_X1fPPi"
    ]


demangleTests :: TestTree
demangleTests = testGroup "Util.Mangle.demangle" [
    testCase "demangle from plain symbol" $
        demangleName "_XN5TestX3fibEii" @?= Right (["TestX", "fib"], [Int32T, Int32T]),
    testCase "demangle after class-info suffix trim" $
        demangleNameFromMetaSymbol "_XN4demo4Math6secretEi_info"
            @?= Right (["demo", "Math", "secret"], [Int32T]),
    testCase "trimMetaSuffix keeps plain symbol unchanged" $
        trimMetaSuffix "_XN4demo4Math6secretEi" @?= "_XN4demo4Math6secretEi",
    testCase "demangle pointer params PiPi" $
        demangleName "_X1fPiPi" @?= Right (["f"], [Pointer Int32T, Pointer Int32T]),
    testCase "demangle pointer param PPi" $
        demangleName "_X1fPPi" @?= Right (["f"], [Pointer (Pointer Int32T)])
    ]


tests :: TestTree
tests = testGroup "Util.Mangle" [
    mangleTests,
    demangleTests
    ]
