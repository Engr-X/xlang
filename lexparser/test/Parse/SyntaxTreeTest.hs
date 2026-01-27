module Parse.SyntaxTreeTest where

import Test.Tasty
import Test.Tasty.HUnit
import Parse.SyntaxTree
import Util.Type
import Lex.Token (Token, Symbol)

import qualified Lex.Token as Lex


mkNumD :: String -> Token
mkNumD s = Lex.NumberConst s $ makePosition 0 0 0

mkIdD :: String -> Token
mkIdD s = Lex.Ident s $ makePosition 0 0 0

mkSymD :: Symbol -> Token
mkSymD s = Lex.Symbol s $ makePosition 0 0 0


flattenBlockTests :: TestTree
flattenBlockTests = testGroup "Parse.SyntaxTree.flattenBlock" $ map (\(i, inp, out) -> testCase i $ flattenBlock inp @=? out) [
    ("0", Nothing, []),
    ("1", Just (Multiple
        [Expr (IntConst "1" $ mkNumD "1"), Expr (IntConst "2" $ mkNumD "2")]),
        [IntConst "1" (mkNumD "1"), IntConst "2" (mkNumD "2")]),

    ("2", Just (Multiple
        [Expr (IntConst "1" $ mkNumD "1"), Expr (BoolConst True $ mkIdD "true"), Expr (Variable "x" $ mkIdD "x")]),
        [IntConst "1" $ mkNumD "1", BoolConst True $ mkIdD "true", Variable "x" $ mkIdD "x"]),

    ("3", Just (Multiple
        [Expr (IntConst "1" $ mkNumD "1"), Expr (Binary Mul (IntConst "2"  $ mkNumD "2") (IntConst "3"  $ mkNumD "3") (mkSymD Lex.Multiply))]),
        [IntConst "1" $ mkNumD "1", Binary Mul (IntConst "2" $ mkNumD "2") (IntConst "3" $ mkNumD "3") (mkSymD Lex.Multiply)])]


flattenCaseTests :: TestTree
flattenCaseTests = testGroup "Parse.SyntaxTree.flattenCase" $ map (\(i, inp, out) -> testCase i $ flattenCase inp @=? out) [
    ("0", Nothing, []),
    ("1", Just (Case (IntConst "1" $ mkNumD "1") (Multiple [Expr (IntConst "2" $ mkNumD "2")])),
        [IntConst "1" $ mkNumD "1", IntConst "2" $ mkNumD "2"]),

    ("2", Just (Case (IntConst "1" $ mkNumD "1") (Multiple [Expr (IntConst "2" $ mkNumD "2"), Expr (IntConst "3" $ mkNumD "3")])),
        [IntConst "1" $ mkNumD "1", IntConst "2" $ mkNumD "2", IntConst "3" $ mkNumD "3"]),

    ("3", Just (Case (Binary Add (IntConst "1" $ mkNumD "1") (IntConst "2" $ mkNumD "2") (mkSymD Lex.Plus)) (Multiple [Expr (IntConst "3" $ mkNumD "3")])),
        [Binary Add (IntConst "1" $ mkNumD "1") (IntConst "2" $ mkNumD "2") (mkSymD Lex.Plus), IntConst "3" $ mkNumD "3"]),

    ("4", Just (Default (Multiple [Expr (IntConst "1" $ mkNumD "1")])), [IntConst "1" $ mkNumD "1"]),

    ("5", Just (Default (Multiple [Expr (IntConst "1" $ mkNumD "1"), Expr (BoolConst True $ mkIdD "true")])),
        [IntConst "1" $ mkNumD "1", BoolConst True $ mkIdD "true"]),

    ("6", Just (Case (IntConst "1" $ mkNumD "1") (Multiple [])), [IntConst "1" $ mkNumD "1"]),

    ("7", Just (Default (Multiple [])), [])]


flattenStatementTests :: TestTree
flattenStatementTests = testGroup "Parse.SyntaxTree.flattenStatement" $ map (\(i, inp, out) -> testCase i $ flattenStatement inp @=? out) [
    ("0", Nothing, []),
    ("1", Just (Expr (IntConst "1" $ mkNumD "1")), [IntConst "1" $ mkNumD "1"]),

    ("2", Just (BlockStmt (Multiple [
            If (BoolConst True $ mkIdD "true") (Just (Multiple [Expr (IntConst "1" $ mkNumD "1")])),
            Else (Just (Multiple [Expr (IntConst "0" $ mkNumD "0")]))])), [
                
            BoolConst True $ mkIdD "true",
            IntConst "1" $ mkNumD "1", IntConst "0" $ mkNumD "0"]),
            
    ("3", Just (
        If (BoolConst True $ mkIdD "true") (Just (Multiple [Expr (IntConst "1" $ mkNumD "1")]))), [
        BoolConst True $ mkIdD "true", IntConst "1" $ mkNumD "1"]),

    ("4", Just (For (Just (IntConst "1" $ mkNumD "1"), Just (IntConst "2" $ mkNumD "2"), Just (IntConst "3" $ mkNumD "3")) (Just (Multiple [Expr (IntConst "4" $ mkNumD "4")]))),
        [IntConst "1" $ mkNumD "1", IntConst "2" $ mkNumD "2", IntConst "3" $ mkNumD "3", IntConst "4" $ mkNumD "4"]),

    ("5", Just (For (Nothing, Just (IntConst "2" $ mkNumD "2"), Nothing) Nothing), [IntConst "2" $ mkNumD "2"]),

    ("6", Just (While (Variable "cond" $ mkIdD "cond") (Just (Multiple [Expr (IntConst "1" $ mkNumD "1")]))),
        [Variable "cond" $ mkIdD "cond", IntConst "1" $ mkNumD "1"]),

    ("7", Just (Switch (Variable "x" $ mkIdD "x") [Case (IntConst "1" $ mkNumD "1") (Multiple [Expr (IntConst "10" $ mkNumD "10")]), Default (Multiple [Expr (IntConst "0" $ mkNumD "0")])]),
        [Variable "x" $ mkIdD "x", IntConst "1" $ mkNumD "1", IntConst "10" $ mkNumD "10", IntConst "0" $ mkNumD "0"])]


flattenProgramTests :: TestTree
flattenProgramTests = testGroup "Parse.SyntaxTree.flattenProgram" $ map (\(i, inp, out) -> testCase i $ flattenProgram inp @=? out) [
    ("0", ([], []), []),
    ("1", ([], [Expr (IntConst "1" $ mkNumD "1")]), [IntConst "1" $ mkNumD "1"]),
    ("2", ([], [Expr (IntConst "1" $ mkNumD "1"), Expr (BoolConst True $ mkIdD "true")]), [IntConst "1" $ mkNumD "1", BoolConst True $ mkIdD "true"]),
    ("3", ([], [If (BoolConst True $ mkIdD "true") (Just (Multiple [Expr (IntConst "1" $ mkNumD "1")]))]), [BoolConst True $ mkIdD "true", IntConst "1" $ mkNumD "1"])]


getErrorProgramTests :: TestTree
getErrorProgramTests = testGroup "Parse.SyntaxTree.getErrorProgram" $ map (\(i, inp, out) -> testCase i $ getErrorProgram inp @=? out) [
    ("0", ([], []), []),
    ("1", ([], [Expr (IntConst "1" $ mkNumD "1")]), []),
    ("2", ([], [Expr (makeError "err")]), [makeError "err"]),
    ("3", ([], [ If (BoolConst True $ mkIdD "true") (Just (Multiple [Expr (makeError "err1")])), Else (Just (Multiple [Expr (makeError "err2")]))]),
        [makeError "err1", makeError "err2"])]
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


isVariableTests :: TestTree
isVariableTests = testGroup "Parse.SyntaxTree.isVariable" $ map (\(i, inp, out) -> testCase i $ isVariable inp @=? out) [
    ("0", Variable "a" dummyTok, True),
    ("1 Qualified", Qualified ["java","lang","Math"] [dummyTok], True),
    ("2 IntConst", IntConst "1" dummyTok, False),
    ("3 BinaryExpr", Binary Add (Variable "a" dummyTok) (IntConst "1" dummyTok) dummyTok, False)]
        where
            dummyTok :: Token
            dummyTok = Lex.Ident "<test>" (makePosition 0 0 0)


identTextTests :: TestTree
identTextTests = testGroup "Parse.ParserBasic.identText" $ map (\(i, inp, out) -> testCase i $ identText inp @=? out) [
    ("0", Lex.Ident "a" (makePosition 1 1 1), "a"),
    ("1", Lex.Ident "abc" (makePosition 1 1 3), "abc"),
    ("2", Lex.Ident "my_var" (makePosition 1 1 6), "my_var"),
    ("3", Lex.Ident "String" (makePosition 1 1 6), "String")]


numTextTests :: TestTree
numTextTests = testGroup "Parse.ParserBasic.numText" $ map (\(i, inp, out) -> testCase i $ numText inp @=? out) [
    ("0", Lex.NumberConst "1" (makePosition 1 1 1), "1"),
    ("1", Lex.NumberConst "123" (makePosition 1 1 3), "123"),
    ("2", Lex.NumberConst "-42" (makePosition 1 1 3), "-42"),
    ("3", Lex.NumberConst "3.14" (makePosition 1 1 4), "3.14")]


charValTests :: TestTree
charValTests = testGroup "Parse.ParserBasic.charVal" $ map (\(i, inp, out) -> testCase i $ charVal inp @=? out) [
    ("0", Lex.CharConst 'a' (makePosition 1 1 1), 'a'),
    ("1", Lex.CharConst '7' (makePosition 1 1 1), '7'),
    ("2", Lex.CharConst '_' (makePosition 1 1 1), '_'),
    ("3", Lex.CharConst ' ' (makePosition 1 1 1), ' ')]


strValTests :: TestTree
strValTests = testGroup "Parse.ParserBasic.strVal" $ map (\(i, inp, out) -> testCase i $ strVal inp @=? out) [
    ("0", Lex.StrConst "" (makePosition 1 1 0), ""),
    ("1", Lex.StrConst "hello" (makePosition 1 1 5), "hello"),
    ("2", Lex.StrConst "hello world" (makePosition 1 1 11), "hello world"),
    ("3", Lex.StrConst "a+b*c" (makePosition 1 1 5), "a+b*c")]


exprTokTests :: TestTree
exprTokTests = testGroup "Parse.SyntaxTree.exprTok" $ map (\(n, e, t) -> testCase n $ exprTok e @=? t) [
    ("0", Variable "x" tokX, tokX),
    ("1", Qualified ["java","lang","String"] [tokJava, tokLang, tokString], tokString),
    ("2", Unary UnaryMinus (IntConst "1" tok1) tokMinus, tokMinus),
    ("3", Binary Add (Variable "a" tokA) (Variable "b" tokB) tokPlus, tokPlus)]
    where
        tokX, tokJava, tokLang, tokString, tok1, tokMinus, tokA, tokB, tokPlus :: Token
        tokX      = Lex.Ident "x"      (makePosition 1 1 1)
        tokJava   = Lex.Ident "java"   (makePosition 1 1 4)
        tokLang   = Lex.Ident "lang"   (makePosition 1 6 4)
        tokString = Lex.Ident "String" (makePosition 1 11 6)

        tok1      = Lex.NumberConst "1" (makePosition 1 1 1)
        tokMinus  = Lex.Symbol Lex.Minus (makePosition 1 1 1)

        tokA      = Lex.Ident "a" (makePosition 1 1 1)
        tokB      = Lex.Ident "b" (makePosition 1 5 1)
        tokPlus   = Lex.Symbol Lex.Plus (makePosition 1 3 1)


tests :: TestTree
tests = testGroup "Parse.SyntaxTree" [
    flattenBlockTests, flattenCaseTests, flattenStatementTests, flattenProgramTests, 
    getErrorProgramTests, toClassTests, isVariableTests, identTextTests, numTextTests, charValTests, strValTests,
    exprTokTests]
