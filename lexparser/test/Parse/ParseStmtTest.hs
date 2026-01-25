module Parse.ParseStmtTest where

import Test.Tasty
import Test.Tasty.HUnit
import Parse.SyntaxTree
import Parse.ParseStmt
import Util.Type
import Lex.Token (Token, Symbol)
import Util.Exception

import qualified Lex.Token as Lex


mkSym :: Symbol -> Int -> Int -> Int -> Token
mkSym s a b c = Lex.Symbol s $ Position a b c

mkNum :: String -> Int -> Int -> Int -> Token
mkNum s a b c = Lex.NumberConst s $ Position a b c

mkId :: String -> Int -> Int -> Int -> Token
mkId s a b c = Lex.Ident s $ makePosition a b c


normalTests :: TestTree
normalTests = testGroup "Parse.ParseStmt.lexparseStmt" $ map (\(n, src, expected) ->
    testCase n $ replLexparseStmt src @=? expected) [
        ("0", "1 + 2 * 3;", Right $ Expr (
            Binary Add
                (IntConst "1" $ mkNum "1" 1 1 1)
                (Binary Mul
                    (IntConst "2" $ mkNum "2" 1 5 1)
                    (IntConst "3" $ mkNum "3" 1 9 1)
                    (mkSym Lex.Multiply 1 7 1))
                (mkSym Lex.Plus 1 3 1))),

        ("1", "{ 1 + 2; 3 * 4; }", Right $ BlockStmt (
            Multiple
            [Expr (Binary Add
                (IntConst "1" $ mkNum "1" 1 3 1)
                (IntConst "2" $ mkNum "2" 1 7 1)
                (mkSym Lex.Plus 1 5 1))
            , Expr (Binary Mul
                (IntConst "3" $ mkNum "3" 1 10 1)
                (IntConst "4" $ mkNum "4" 1 14 1)
                (mkSym Lex.Multiply 1 12 1))])),
                
        ("2", "{ { 1; } 2; }", Right $ BlockStmt (
            Multiple
            [BlockStmt (Multiple
                [Expr (IntConst "1" $ mkNum "1" 1 5 1)]),
                Expr (IntConst "2" $ mkNum "2" 1 10 1)])),
                
        ("3", "{ 1 + 2\n  3 + 4\n}", Right $ BlockStmt (
            Multiple
            [Expr (Binary Add
                (IntConst "1" $ mkNum "1" 1 3 1)
                (IntConst "2" $ mkNum "2" 1 7 1)
                (mkSym Lex.Plus 1 5 1)),
            
            Expr (Binary Add
                (IntConst "3" $ mkNum "3" 2 3 1)
                (IntConst "4" $ mkNum "4" 2 7 1)
                (mkSym Lex.Plus 2 5 1))]))]


whileTests :: TestTree
whileTests = testGroup "Parse.ParseStmt.while" $ map (\(n, src, expected) ->
    testCase n $ replLexparseStmt src @=? expected) [
        ("0", unlines [
            "while true {",
            "    x = x + 1",
            "}"],
        
            Right $ While
                (BoolConst True (mkId "true" 1 7 4))
                (Just (Multiple [
                    Expr (
                        Binary Assign
                            (Variable "x" (mkId "x" 2 5 1))
                            (Binary Add
                                (Variable "x" (mkId "x" 2 9 1))
                                (IntConst "1" (mkNum "1" 2 13 1))
                                (mkSym Lex.Plus 2 11 1))
                            (mkSym Lex.Assign 2 7 1))
                ]))),

        ("1",
            unlines [
                "while true",
                "    while false",
                "        aa = aa + 2"
            ],
            Right $ While
                (BoolConst True (mkId "true" 1 7 4))
                (Just (Multiple [
                    While
                        (BoolConst False (mkId "false" 2 11 5))
                        (Just (Multiple [
                            Expr (
                                Binary Assign
                                    (Variable "aa" (mkId "aa" 3 9 2))
                                    (Binary Add
                                        (Variable "aa" (mkId "aa" 3 14 2))
                                        (IntConst "2" (mkNum "2" 3 19 1))
                                        (mkSym Lex.Plus 3 17 1))
                                    (mkSym Lex.Assign 3 12 1))
                        ]))
                ]))
        ),

        -- while true
        --   x = x + 1
        --   x = x + 2
        ("2",
            unlines [
                "while true",
                "    x = x + 1",
                "    x = x + 2"
            ],
            Left [Parsing $ makeError 
                "stdin" (makePosition 3 5 1) "Parse error near: Ident \"x\" (3,5,1)"
            ]),


        -- while x + 2 { a }
        ("3",
            unlines [
                "while x + 2",
                "{",
                "    a",
                "}"
            ],
            Right $ While
                (Binary Add
                    (Variable "x" (mkId "x" 1 7 1))
                    (IntConst "2" (mkNum "2" 1 11 1))
                    (mkSym Lex.Plus 1 9 1))
                (Just (Multiple [
                    Expr (Variable "a" (mkId "a" 3 5 1))
                ])))]


tests :: TestTree
tests = testGroup "Parse.ParseStmt" [normalTests, whileTests]
