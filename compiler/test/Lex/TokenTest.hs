module Lex.TokenTest where

import Test.Tasty
import Test.Tasty.HUnit
import Lex.Token
import Util.Type


isErrTokenTests :: TestTree
isErrTokenTests = testGroup "Lex.Token.isErrToken" [
    testCase "0" $ isErrToken (Error "fuck" $ makePosition 0 0 0) @=? True,
    testCase "1" $ isErrToken (Error "fuck" $ makePosition 1 1 0) @=? True,
    testCase "2" $ isErrToken (Ident "x" $ makePosition 0 0 0) @=? False,
    testCase "3" $ isErrToken (NumberConst "123" $ makePosition 0 0 0) @=? False]


isLBracketTokenTest :: TestTree
isLBracketTokenTest = testGroup "Lex.Token.isLBracketToken" [
    testCase "0" $ isLBracketToken (Symbol LParen $ makePosition 0 0 0) @=? True,
    testCase "0" $ isLBracketToken (Symbol RParen $ makePosition 0 1 0) @=? False,
    testCase "0" $ isLBracketToken (Symbol LBracket $ makePosition 3 0 0) @=? True,
    testCase "0" $ isLBracketToken (Error "fuck" $ makePosition 1 2 3) @=? False]


isRBracketTokenTest :: TestTree
isRBracketTokenTest = testGroup "Lex.Token.isRBracketToken" [
    testCase "0" $ isRBracketToken (Symbol LParen $ makePosition 0 0 0) @=? False,
    testCase "0" $ isRBracketToken (Symbol RParen $ makePosition 0 1 0) @=? True,
    testCase "0" $ isRBracketToken (Symbol LBracket $ makePosition 3 0 0) @=? False,
    testCase "0" $ isRBracketToken (Error "fuck" $ makePosition 1 2 3) @=? False]


isBracketTokenTest :: TestTree
isBracketTokenTest = testGroup "Lex.Token.isBracketToken" [
    testCase "0" $ isBracketToken (Symbol LParen $ makePosition 0 0 0) @=? True,
    testCase "0" $ isBracketToken (Symbol RParen $ makePosition 0 1 0) @=? True,
    testCase "0" $ isBracketToken (Symbol LBracket $ makePosition 3 0 0) @=? True,
    testCase "0" $ isBracketToken (Error "fuck" $ makePosition 1 2 3) @=? False]


tokenPosTests :: TestTree
tokenPosTests = testGroup "Lex.Token.tokenPos" [
    testCase "0" $ tokenPos (Error "fuck" $ makePosition 1 2 3) @=? makePosition 1 2 3,
    testCase "1" $ tokenPos (StrConst "fuck" $ makePosition 1 1 0) @=? makePosition 1 1 0,
    testCase "2" $ tokenPos (Ident "x" $ makePosition 0 1 0) @=? makePosition 0 1 0,
    testCase "3" $ tokenPos (NumberConst "123" $ makePosition 0 0 0) @=? makePosition 0 0 0]


tests :: TestTree
tests = testGroup "Lex.Token" [isErrTokenTests, isLBracketTokenTest, isRBracketTokenTest, isBracketTokenTest, tokenPosTests]
