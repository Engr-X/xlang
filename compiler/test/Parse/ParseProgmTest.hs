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
            "while true:",
            "   x + a",
            "   while false:",
            "       y + a"],

        ([], [
            While
                (BoolConst True (mkId "true" 1 7 4))
                (Just (Multiple [
                    Expr (Binary Add
                        (Variable "x" (mkId "x" 2 4 1))
                        (Variable "a" (mkId "a" 2 8 1))
                        (mkSym Lex.Plus 2 6 1))
                ]))
                Nothing
                (mkId "while" 1 1 5, Nothing),

                While
                    (BoolConst False (mkId "false" 3 10 5))
                    (Just (Multiple [
                        Expr (Binary Add
                            (Variable "y" (mkId "y" 4 8 1))
                            (Variable "a" (mkId "a" 4 12 1))
                            (mkSym Lex.Plus 4 10 1))

                    ]))
                    Nothing
                    (mkId "while" 3 4 5, Nothing)])),

        ("5", unlines [
            "while true:",
            "while x > 1:",
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
                            Nothing
                            (mkId "while" 2 1 5, Nothing)
                    ]))
                Nothing
                (mkId "while" 1 1 5, Nothing),

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
                While
                    (BoolConst True (mkId "true" 1 7 4))
                    Nothing
                    Nothing
                    (mkId "while" 1 1 5, Nothing)
            ])),

        ("6b", unlines [
            "while true {}"],

            ([], [
                While
                    (BoolConst True (mkId "true" 1 7 4))
                    (Just (Multiple []))
                    Nothing
                    (mkId "while" 1 1 5, Nothing)
            ])),
            
        ("7", unlines [
            "a = a + 1",
            "while a",
            "{",
            "    while p:",
            "        p + I",
            "}",
            "else",
            "{",
            "    while l:",
            "        l + p - I",
            "}"
        ],

        ([], [
            -- a = a + 1
            Expr (Binary Assign
                (Variable "a" (mkId "a" 1 1 1))
                (Binary Add
                    (Variable "a" (mkId "a" 1 5 1))
                    (IntConst "1" (mkNum "1" 1 9 1))
                    (mkSym Lex.Plus 1 7 1))
                (mkSym Lex.Assign 1 3 1)),

            -- while a { ... } else { ... }
            While
                (Variable "a" (mkId "a" 2 7 1))
                (Just (Multiple [
                    While
                        (Variable "p" (mkId "p" 4 11 1))
                        (Just (Multiple [
                            Expr (Binary Add
                                (Variable "p" (mkId "p" 5 9 1))
                                (Variable "I" (mkId "I" 5 13 1))
                                (mkSym Lex.Plus 5 11 1))
                        ]))
                        Nothing
                        (mkId "while" 4 5 5, Nothing)
                ]))
                (Just (Multiple [
                    While
                        (Variable "l" (mkId "l" 9 11 1))
                        (Just (Multiple [
                            Expr (Binary Sub
                                (Binary Add
                                    (Variable "l" (mkId "l" 10 9 1))
                                    (Variable "p" (mkId "p" 10 13 1))
                                    (mkSym Lex.Plus 10 11 1))
                                (Variable "I" (mkId "I" 10 17 1))
                                (mkSym Lex.Minus 10 15 1))
                        ]))
                        Nothing
                        (mkId "while" 9 5 5, Nothing)
                ]))
                (mkId "while" 2 1 5, Just (mkId "else" 7 1 4))
        ])),
        
        ("8", unlines [
            "while true:",
            "    a",
            "else:",
            "    while false:",
            "        println(true)"
        ],

        ([], [
            While
                (BoolConst True (mkId "true" 1 7 4))
                (Just (Multiple [
                    Expr (Variable "a" (mkId "a" 2 5 1))
                ]))
                (Just (Multiple [
                    While
                        (BoolConst False (mkId "false" 4 11 5))
                        (Just (Multiple [
                            Expr (Call
                                (Variable "println" (mkId "println" 5 9 7))
                                [BoolConst True (mkId "true" 5 17 4)]
                            )

                        ]))
                        Nothing
                        (mkId "while" 4 5 5, Nothing)
                ]))
                (mkId "while" 1 1 5, Just (mkId "else" 3 1 4))
        ])),
        
        ("9", unlines [
            "while true",
            "{",
            "    a = a + 1",
            "}",
            "else:",
            "    if false: print(a)",
            "    else: print(b)"
        ],

        ([], [
            While
                (BoolConst True (mkId "true" 1 7 4))
                (Just (Multiple [
                    Expr (Binary Assign
                        (Variable "a" (mkId "a" 3 5 1))
                        (Binary Add
                            (Variable "a" (mkId "a" 3 9 1))
                            (IntConst "1" (mkNum "1" 3 13 1))
                            (mkSym Lex.Plus 3 11 1))
                        (mkSym Lex.Assign 3 7 1))
                ]))
                (Just (Multiple [
                    If
                        (BoolConst False (mkId "false" 6 8 5))
                        (Just (Multiple [
                            Expr (Call
                                (Variable "print" (mkId "print" 6 15 5))
                                [Variable "a" (mkId "a" 6 21 1)]
                            )
                        ]))
                        (Just (Multiple [
                            Expr (Call
                                (Variable "print" (mkId "print" 7 11 5))
                                [Variable "b" (mkId "b" 7 17 1)]
                            )
                        ]))
                        (mkId "if" 6 5 2, Just (mkId "else" 7 5 4))
                ]))
                (mkId "while" 1 1 5, Just (mkId "else" 5 1 4))
        ])),
        
        ("10", unlines [
        "void addToMap::<T>(map::<T> m, T item) {",
        "    m.add(item)",
        "}"],
        ([], [
            FunctionT
                (Void, [mkId "void" 1 1 4])
                (Variable "addToMap" (mkId "addToMap" 1 6 8))
                [ (Class ["T"] [], [mkId "T" 1 17 1]) ] [
                    (Class ["map"] [Class ["T"] []],
                        "m", [
                            mkId "m" 1 29 1,
                            mkId "map" 1 20 3,
                            mkSym Lex.DoubleColon 1 23 2,
                            mkSym Lex.LessThan 1 25 1,
                            mkId "T" 1 26 1,
                            mkSym Lex.GreaterThan 1 27 1])
                ,
                    ( Class ["T"] []
                    , "item"
                    , [mkId "item" 1 34 4, mkId "T" 1 32 1]
                    )
                ]
                (Multiple [
                    Expr (Call
                        (Qualified ["m","add"] [mkId "m" 2 5 1, mkId "add" 2 7 3])
                        [Variable "item" (mkId "item" 2 11 4)]
                    )
                ])
        ])),
        
        ("11", unlines [
        "void pushAll::<T>(stack::<T> s, list::<T> items) {",
        "    s.push(items)",
        "}"], ([], [
            FunctionT
                (Void, [mkId "void" 1 1 4])
                (Variable "pushAll" (mkId "pushAll" 1 6 7))
                [ (Class ["T"] [], [mkId "T" 1 16 1]) ] [
                    (Class ["stack"] [Class ["T"] []],
                        "s", [
                            mkId "s" 1 30 1,
                            mkId "stack" 1 19 5,
                            mkSym Lex.DoubleColon 1 24 2,
                            mkSym Lex.LessThan 1 26 1,
                            mkId "T" 1 27 1,
                            mkSym Lex.GreaterThan 1 28 1]),
                    (Class ["list"] [Class ["T"] []],
                    "items", [
                        mkId "items" 1 43 5,
                        mkId "list" 1 33 4,
                        mkSym Lex.DoubleColon 1 37 2,
                        mkSym Lex.LessThan 1 39 1,
                        mkId "T" 1 40 1,
                        mkSym Lex.GreaterThan 1 41 1]
                    )
                ]
                (Multiple [
                    Expr (Call
                        (Qualified ["s","push"] [mkId "s" 2 5 1, mkId "push" 2 7 4])
                        [Variable "items" (mkId "items" 2 12 5)]
                    )
                ])
        ])),

        ("12", unlines [
        "void flatten::<T>(list::<list::<T>> ll, list::<T> out) {",
        "    out.addAll(ll)",
        "}"],
        ([], [
            FunctionT
                (Void, [mkId "void" 1 1 4])
                (Variable "flatten" (mkId "flatten" 1 6 7))
                [ (Class ["T"] [], [mkId "T" 1 16 1]) ] [
                    (Class ["list"] [Class ["list"] [Class ["T"] []]],
                        "ll", [
                            mkId "ll" 1 37 2,
                            mkId "list" 1 19 4,
                            mkSym Lex.DoubleColon 1 23 2,
                            mkSym Lex.LessThan 1 25 1,
                            mkId "list" 1 26 4,
                            mkSym Lex.DoubleColon 1 30 2,
                            mkSym Lex.LessThan 1 32 1,
                            mkId "T" 1 33 1,
                            mkSym Lex.GreaterThan 1 34 1,
                            mkSym Lex.GreaterThan 1 35 1]),
                    (Class ["list"] [Class ["T"] []], "out", [
                        mkId "out" 1 51 3,
                        mkId "list" 1 41 4,
                        mkSym Lex.DoubleColon 1 45 2,
                        mkSym Lex.LessThan 1 47 1,
                        mkId "T" 1 48 1,
                        mkSym Lex.GreaterThan 1 49 1]
                    )
                ]
                (Multiple [
                    Expr (Call
                            (Qualified ["out","addAll"] [mkId "out" 2 5 3, mkId "addAll" 2 9 6])
                            [Variable "ll" (mkId "ll" 2 16 2)]
                    )
                ])
        ]))]
        

tests :: TestTree
tests = testGroup "Parse.ParseProgm" [lexparseProgmTests]
