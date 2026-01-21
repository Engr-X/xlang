module Parse.SyntaxTreeTest where

import Test.Tasty
import Test.Tasty.HUnit
import Parse.SyntaxTree
import Util.Type
import Lex.Token (Token)

import qualified Lex.Token as Lex


flattenBlockTests :: TestTree
flattenBlockTests = testGroup "Parse.SyntaxTree.flattenBlock" $ map (\(i, inp, out) -> testCase i $ flattenBlock inp @=? out) [
    ("0", Nothing, []),
    ("1", Just (Multiple [Expr (IntConst "1"), Expr (IntConst "2")]), [IntConst "1", IntConst "2"]),
    ("2", Just (Multiple [Expr (IntConst "1"), Expr (BoolConst True), Expr (Variable "x")]), [IntConst "1", BoolConst True, Variable "x"]),
    ("3", Just (Multiple [Expr (IntConst "1"), Expr (Binary Mul (IntConst "2") (IntConst "3"))]), [IntConst "1", Binary Mul (IntConst "2") (IntConst "3")])]


flattenCaseTests :: TestTree
flattenCaseTests = testGroup "Parse.SyntaxTree.flattenCase" $ map (\(i, inp, out) -> testCase i $ flattenCase inp @=? out) [
    ("0", Nothing, []),
    ("1", Just (Case (IntConst "1") (Multiple [Expr (IntConst "2")])), [IntConst "1", IntConst "2"]),
    ("2", Just (Case (IntConst "1") (Multiple [Expr (IntConst "2"), Expr (IntConst "3")])), [IntConst "1", IntConst "2", IntConst "3"]),
    ("3", Just (Case (Binary Add (IntConst "1") (IntConst "2")) (Multiple [Expr (IntConst "3")])), [Binary Add (IntConst "1") (IntConst "2"), IntConst "3"]),
    ("4", Just (Default (Multiple [Expr (IntConst "1")])), [IntConst "1"]), 
    ("5", Just (Default (Multiple [Expr (IntConst "1"), Expr (BoolConst True)])), [IntConst "1", BoolConst True]),
    ("6", Just (Case (IntConst "1") (Multiple [])), [IntConst "1"]),
    ("7", Just (Default (Multiple [])), [])]


flattenStatementTests :: TestTree
flattenStatementTests = testGroup "Parse.SyntaxTree.flattenStatement" $ map (\(i, inp, out) -> testCase i $ flattenStatement inp @=? out) [
    ("0", Nothing, []),
    ("1", Just (Expr (IntConst "1")), [IntConst "1"]),
    ("2", Just (If (BoolConst True) (Just (Multiple [Expr (IntConst "1")])) (Just (Multiple [Expr (IntConst "0")]))), [BoolConst True, IntConst "1", IntConst "0"]),
    ("3", Just (If (BoolConst True) (Just (Multiple [Expr (IntConst "1")])) Nothing), [BoolConst True, IntConst "1"]),
    ("4", Just (For (Just (IntConst "1"), Just (IntConst "2"), Just (IntConst "3")) (Just (Multiple [Expr (IntConst "4")]))), [IntConst "1", IntConst "2", IntConst "3", IntConst "4"]),
    ("5", Just (For (Nothing, Just (IntConst "2"), Nothing) Nothing), [IntConst "2"]),
    ("6", Just (While (Variable "cond") (Just (Multiple [Expr (IntConst "1")]))), [Variable "cond", IntConst "1"]),
    ("7", Just (Switch (Variable "x") [Case (IntConst "1") (Multiple [Expr (IntConst "10")]), Default (Multiple [Expr (IntConst "0")])]), [Variable "x", IntConst "1", IntConst "10", IntConst "0"])]


flattenProgramTests :: TestTree
flattenProgramTests = testGroup "Parse.SyntaxTree.flattenProgram" $ map (\(i, inp, out) -> testCase i $ flattenProgram inp @=? out) [
    ("0", ([], []), []),
    ("1", ([], [Expr (IntConst "1")]), [IntConst "1"]),
    ("2", ([], [Expr (IntConst "1"), Expr (BoolConst True)]), [IntConst "1", BoolConst True]),
    ("3", ([], [If (BoolConst True) (Just (Multiple [Expr (IntConst "1")])) Nothing]), [BoolConst True, IntConst "1"])]
getErrorProgramTests :: TestTree
getErrorProgramTests = testGroup "Parse.SyntaxTree.getErrorProgram" $ map (\(i, inp, out) -> testCase i $ getErrorProgram inp @=? out) [
    ("0", ([], []), []),
    ("1", ([], [Expr (IntConst "1")]), []),
    ("2", ([], [Expr (makeError "err")]), [makeError "err"]),
    ("3", ([], [If (BoolConst True) (Just (Multiple [Expr (makeError "err1")])) (Just (Multiple [Expr (makeError "err2")]))]), [makeError "err1", makeError "err2"])]
        where
            dummyTok :: String -> Token
            dummyTok str = Lex.Ident str (makePosition 1 1 (length str))

            makeError :: String -> Expression
            makeError s = Error (dummyTok s) "why?"


toClassTests :: TestTree
toClassTests = testGroup "Parse.SyntaxTree.toClass" $ map (\(i, inp, out) -> testCase i $ toClass inp @=? out) [
    ("0", "bool", Bool), ("1",  "int", Int32T), ("2", "int32", Int32T), ("3", "int64", Int64T), ("4", "float", Float32T),
    ("5", "double", Float64T), ("6", "float64", Float64T), ("7", "byte", Int8T), ("8", "short", Int16T),
    ("9", "MyClass", Class "MyClass"), ("10", "com.wangdi.Math", Class "com.wangdi.Math"), ("11", "java.lang.String", Class "java.lang.String")]


tests :: TestTree
tests = testGroup "Parse.SyntaxTree" [flattenBlockTests, flattenCaseTests, flattenStatementTests, flattenProgramTests, getErrorProgramTests, toClassTests]
