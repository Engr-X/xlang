module Parse.SyntaxTreeTest where

import Test.Tasty
import Test.Tasty.HUnit
import Parse.SyntaxTree
import Util.Type

import qualified Lex.Token as Lex


pos :: Position
pos = makePosition 0 0 0


isErrExprTest :: TestTree
isErrExprTest = testGroup "Parse.SyntaxTree.isErrExpr" [
    testCase "0" $ isErrExpr (Error (Lex.Ident "x" pos) "why") @=? True,
    testCase "0" $ isErrExpr (IntConst "12") @=? False,
    testCase "0" $ isErrExpr (FloatConst "12.3f") @=? False,
    testCase "0" $ isErrExpr Empty @=? False]


tests :: TestTree
tests = testGroup "Parse.SyntaxTree" [isErrExprTest]
