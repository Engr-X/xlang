module Parse.ParseProgmTest where

import Test.Tasty
import Test.Tasty.HUnit
import Parse.SyntaxTree
import Parse.ParseProgm
import Util.Type
import Lex.Token (Token)

import qualified Lex.Token as Lex


mkSym :: Lex.Symbol -> Int -> Int -> Int -> Token
mkSym s a b c = Lex.Symbol s $ Position a b c

mkNum :: String -> Int -> Int -> Int -> Token
mkNum s a b c = Lex.NumberConst s $ Position a b c

mkId :: String -> Int -> Int -> Int -> Token
mkId s a b c = Lex.Ident s $ Position a b c


lexparseProgmTests :: TestTree
lexparseProgmTests = testGroup "Parse.ParseProgm.lexparseProgm" $ map (\(n, src, expected) ->
    testCase n $ replLexparseProgm src @=? Right expected) [
    ("0", unlines [
        "a=-1+2*3"],
        
        ([], [
            Expr (Binary Assign
                (Variable "a" (mkId "a" 1 1 1))
                (Binary Add
                    (Unary UnaryMinus (IntConst "1" (mkNum "1" 1 4 1)) (mkSym Lex.Minus 1 3 1))
                    (Binary Mul
                        (IntConst "2" (mkNum "2" 1 6 1))
                        (IntConst "3" (mkNum "3" 1 8 1))
                        (mkSym Lex.Multiply 1 7 1))
                    (mkSym Lex.Plus 1 5 1))
                (mkSym Lex.Assign 1 2 1))])),
                
    ("1", unlines [
        "b=+(1+2)*(3-4)/-5"],
        
        ([], [
            Expr (Binary Assign
                (Variable "b" (mkId "b" 1 1 1))
                (Binary Div
                    (Binary Mul
                        (Unary UnaryPlus
                            (Binary Add
                                (IntConst "1" (mkNum "1" 1 5 1))
                                (IntConst "2" (mkNum "2" 1 7 1))
                                (mkSym Lex.Plus 1 6 1))
                            (mkSym Lex.Plus 1 3 1))
                        (Binary Sub
                            (IntConst "3" (mkNum "3" 1 11 1))
                            (IntConst "4" (mkNum "4" 1 13 1))
                            (mkSym Lex.Minus 1 12 1))
                        (mkSym Lex.Multiply 1 9 1))
                    (Unary UnaryMinus
                        (IntConst "5" (mkNum "5" 1 17 1))
                        (mkSym Lex.Minus 1 16 1))
                    (mkSym Lex.Divide 1 15 1))
                (mkSym Lex.Assign 1 2 1))])),

    ("2", unlines [
        "{",
        "c=1",
        "d=(1+2)/(3+4)",
        "}",
        "e=-(1)"],
        
        ([], [BlockStmt (Multiple [ 
            Expr (Binary Assign
                (Variable "c" (mkId "c" 2 1 1))
                (IntConst "1" (mkNum "1" 2 3 1))
                (mkSym Lex.Assign 2 2 1)),
                    
            Expr (Binary Assign
                (Variable "d" (mkId "d" 3 1 1))
                (Binary Div
                    (Binary Add
                        (IntConst "1" (mkNum "1" 3 4 1))
                        (IntConst "2" (mkNum "2" 3 6 1))
                        (mkSym Lex.Plus 3 5 1))
                    (Binary Add
                        (IntConst "3" (mkNum "3" 3 10 1))
                        (IntConst "4" (mkNum "4" 3 12 1))
                        (mkSym Lex.Plus 3 11 1))
                    (mkSym Lex.Divide 3 8 1))
                (mkSym Lex.Assign 3 2 1))]),
            
            Expr (Binary Assign
                (Variable "e" (mkId "e" 5 1 1))
                (Unary UnaryMinus
                (IntConst "1" (mkNum "1" 5 5 1))
                (mkSym Lex.Minus 5 3 1))
                (mkSym Lex.Assign 5 2 1))])),
                
    ("3", unlines [
        "{",
        "{",
        "f=-(-1)",
        "}",
        "g=(1+2)*((3))",
        "}"],
        
    ([], [BlockStmt (Multiple [
        BlockStmt (Multiple [
            Expr (Binary Assign
                (Variable "f" (mkId "f" 3 1 1))
                (Unary UnaryMinus
                    (Unary UnaryMinus
                        (IntConst "1" (mkNum "1" 3 6 1))
                        (mkSym Lex.Minus 3 5 1))
                    (mkSym Lex.Minus 3 3 1))
                (mkSym Lex.Assign 3 2 1))]),
                
            Expr (Binary Assign
                (Variable "g" (mkId "g" 5 1 1))
                (Binary Mul
                    (Binary Add
                        (IntConst "1" (mkNum "1" 5 4 1))
                        (IntConst "2" (mkNum "2" 5 6 1))
                        (mkSym Lex.Plus 5 5 1))
                    (IntConst "3" (mkNum "3" 5 11 1))
                    (mkSym Lex.Multiply 5 8 1))
                (mkSym Lex.Assign 5 2 1))])])),
                
                
        ("4", unlines [
            "while true",
            "   x + a",
            "   while false",
            "       y + a"],

        ([], [
            While
                (BoolConst True (mkId "true" 1 7 4))
                (Just (Multiple [
                    Expr (Binary Add
                        (Variable "x" (mkId "x" 2 4 1))
                        (Variable "a" (mkId "a" 2 8 1))
                        (mkSym Lex.Plus 2 6 1))
                ])),

            While
                (BoolConst False (mkId "false" 3 10 5))
                (Just (Multiple [
                    Expr (Binary Add
                        (Variable "y" (mkId "y" 4 8 1))
                        (Variable "a" (mkId "a" 4 12 1))
                        (mkSym Lex.Plus 4 10 1))
                ]))
        ])),


        ("5", unlines [
            "while true",
            "while x > 1",
            "x = x + 1",
            "y = y + 1"],

            ([], [
                While
                    (BoolConst True (mkId "true" 1 7 4))
                    (Just (Multiple [
                        While
                            (Binary GreaterThan
                                (Variable "x" (mkId "x" 2 7 1))
                                (IntConst "1" (mkNum "1" 2 11 1))
                                (mkSym Lex.GreaterThan 2 9 1))
                            (Just (Multiple [
                                Expr (Binary Assign
                                    (Variable "x" (mkId "x" 3 1 1))
                                    (Binary Add
                                        (Variable "x" (mkId "x" 3 5 1))
                                        (IntConst "1" (mkNum "1" 3 9 1))
                                        (mkSym Lex.Plus 3 7 1))
                                    (mkSym Lex.Assign 3 3 1))
                            ]))
                    ])),

                Expr (Binary Assign
                    (Variable "y" (mkId "y" 4 1 1))
                    (Binary Add
                        (Variable "y" (mkId "y" 4 5 1))
                        (IntConst "1" (mkNum "1" 4 9 1))
                        (mkSym Lex.Plus 4 7 1))
                    (mkSym Lex.Assign 4 3 1))
            ])),

        ("6a", unlines [
            "while true;"],

            ([], [
                -- 语法：KW_WHILE Expr ';'  => While cond Nothing
                While
                    (BoolConst True (mkId "true" 1 7 4))
                    Nothing
            ])),

        ("6b", unlines [
            "while true {}"],

            ([], [
                While
                    (BoolConst True (mkId "true" 1 7 4))
                    Nothing
            ])),

        ("7", unlines [
            "a = a + 1",
            "while a",
            "{",
            "    while (p)",
            "        p + I",
            "    while (l)",
            "        l + p - I",
            "}"],

        ([], [
            -- a = a + 1
            Expr (Binary Assign
                (Variable "a" (mkId "a" 1 1 1))
                (Binary Add
                    (Variable "a" (mkId "a" 1 5 1))
                    (IntConst "1" (mkNum "1" 1 9 1))
                    (mkSym Lex.Plus 1 7 1))
                (mkSym Lex.Assign 1 3 1)),

            -- while a { ... }
            While
                (Variable "a" (mkId "a" 2 7 1))
                (Just (Multiple [
                    While
                        (Variable "p" (mkId "p" 4 12 1))
                        (Just (Multiple [
                            Expr (Binary Add
                                (Variable "p" (mkId "p" 5 9 1))
                                (Variable "I" (mkId "I" 5 13 1))
                                (mkSym Lex.Plus 5 11 1))
                        ])),

                    While
                        (Variable "l" (mkId "l" 6 12 1))
                        (Just (Multiple [
                            Expr (Binary Sub
                                (Binary Add
                                    (Variable "l" (mkId "l" 7 9 1))
                                    (Variable "p" (mkId "p" 7 13 1))
                                    (mkSym Lex.Plus 7 11 1))
                                (Variable "I" (mkId "I" 7 17 1))
                                (mkSym Lex.Minus 7 15 1))
                        ]))
                ]))
        ]))]


tests :: TestTree
tests = testGroup "Parse.ParseProgm" [lexparseProgmTests]
