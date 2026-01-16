module Lex.TokenTest where

import Test.Tasty
import Test.Tasty.HUnit
import Lex.Token
import Util.Type


isErrTokenTests :: TestTree
isErrTokenTests = testGroup "Lex.Token.isErrToken" [
    testCase "Lex.Token.isErrToken0" $ isErrToken (Error "fuck" $ makePosition 0 0 0) @=? True,
    testCase "Lex.Token.isErrToken1" $ isErrToken (Error "fuck" $ makePosition 1 1 0) @=? True,
    testCase "Lex.Token.isErrToken2" $ isErrToken (Ident "x" $ makePosition 0 0 0) @=? False,
    testCase "Lex.Token.isErrToken3" $ isErrToken (NumberConst "123" $ makePosition 0 0 0) @=? False]


tokPosTests :: TestTree
tokPosTests = testGroup "Lex.Token.tokPos" [
    testCase "Lex.Token.tokPos0" $ tokPos (Error "fuck" $ makePosition 1 2 3) @=? makePosition 1 2 3,
    testCase "Lex.Token.tokPos1" $ tokPos (StrConst "fuck" $ makePosition 1 1 0) @=? makePosition 1 1 0,
    testCase "Lex.Token.tokPos2" $ tokPos (Ident "x" $ makePosition 0 1 0) @=? makePosition 0 1 0,
    testCase "Lex.Token.tokPos3" $ tokPos (NumberConst "123" $ makePosition 0 0 0) @=? makePosition 0 0 0]


tests :: TestTree
tests = testGroup "Lex.Token" [isErrTokenTests, tokPosTests]
