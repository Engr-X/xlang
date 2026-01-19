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


toClassTests :: TestTree
toClassTests = testGroup "Parse.SyntaxTree.toClass" [
    testCase "0" $ toClass "bool" @=? Bool,
    testCase "1" $ toClass "int" @=? Int32T,
    testCase "2" $ toClass "int32" @=? Int32T,
    testCase "3" $ toClass "int64" @=? Int64T,
    testCase "4" $ toClass "float" @=? Float32T,
    testCase "5" $ toClass "double" @=? Float64T,
    testCase "6" $ toClass "float64" @=? Float64T,

    -- aliases
    testCase "7" $ toClass "byte" @=? Int8T,
    testCase "8" $ toClass "short" @=? Int16T,

    -- user-defined / qualified class names
    testCase "9"  $ toClass "MyClass" @=? Class "MyClass",
    testCase "10" $ toClass "com.wangdi.Math" @=? Class "com.wangdi.Math",
    testCase "11" $ toClass "java.lang.String" @=? Class "java.lang.String"]



tests :: TestTree
tests = testGroup "Parse.SyntaxTree" [isErrExprTest, toClassTests]
