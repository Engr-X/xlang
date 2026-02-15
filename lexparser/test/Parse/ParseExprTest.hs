module Parse.ParseExprTest where

import Test.Tasty
import Test.Tasty.HUnit
import Parse.SyntaxTree
import Parse.ParseExpr
import Util.Type
import Lex.Token (Token, Symbol)

import qualified Lex.Token as Lex


mkSym :: Symbol -> Int -> Int -> Int -> Token
mkSym s a b c = Lex.Symbol s $ Position a b c

mkNum :: String -> Int -> Int -> Int -> Token
mkNum s a b c = Lex.NumberConst s $ Position a b c

mkId :: String -> Int -> Int -> Int -> Token
mkId s a b c = Lex.Ident s $ makePosition a b c


lexparseExprTests :: TestTree
lexparseExprTests = testGroup "Parse.ParseExpr.lexparseExpr" $
    map (\(n, src, expected) -> testCase n $ replLexparseExpr src @=? expected) [
    ("0", "1 + 2 - 3", Right $
        Binary Sub
            (Binary Add
                (IntConst "1" $ mkNum "1" 1 1 1)
                (IntConst "2" $ mkNum "2" 1 5 1) $ mkSym Lex.Plus 1 3 1)
            (IntConst "3" $ mkNum "3" 1 9 1) $ mkSym Lex.Minus 1 7 1),

    ("1", "(1 + 2) * (3 - 4) / 5", Right $
        Binary Div
            (Binary Mul
                (Binary Add
                    (IntConst "1" $ mkNum "1" 1 2 1)
                    (IntConst "2" $ mkNum "2" 1 6 1)
                    (mkSym Lex.Plus 1 4 1))
                (Binary Sub
                    (IntConst "3" $ mkNum "3" 1 12 1)
                    (IntConst "4" $ mkNum "4" 1 16 1)
                    (mkSym Lex.Minus 1 14 1))
                (mkSym Lex.Multiply 1 9 1))
            (IntConst "5" $ mkNum "5" 1 21 1)
            (mkSym Lex.Divide 1 19 1)),

    ("2", "-1 + +2 * -3", Right $ Binary Add
        (Unary UnaryMinus
            (IntConst "1" $ mkNum "1" 1 2 1)
            (mkSym Lex.Minus 1 1 1))
        (Binary Mul
            (Unary UnaryPlus
                (IntConst "2" $ mkNum "2" 1 7 1)
                (mkSym Lex.Plus 1 6 1))
            (Unary UnaryMinus
                (IntConst "3" $ mkNum "3" 1 12 1)
                (mkSym Lex.Minus 1 11 1))
            (mkSym Lex.Multiply 1 9 1))
        (mkSym Lex.Plus 1 4 1)),
        
    ("3", "a = b = c + 1", Right $
        Binary Assign
            (Variable "a" $ mkId "a" 1 1 1)
            (Binary Assign
                (Variable "b" $ mkId "b" 1 5 1)
                (Binary Add
                    (Variable "c" $ mkId "c" 1 9 1)
                    (IntConst "1" $ mkNum "1" 1 13 1)
                    (mkSym Lex.Plus 1 11 1))
                (mkSym Lex.Assign 1 7 1))
            (mkSym Lex.Assign 1 3 1)),
            
    ("4", "a + 1 > b * 2 + (c - 3)", Right $
        Binary GreaterThan
            (Binary Add
                (Variable "a" $ mkId "a" 1 1 1)
                (IntConst "1" $ mkNum "1" 1 5 1)
                (mkSym Lex.Plus 1 3 1))
            (Binary Add
                (Binary Mul
                    (Variable "b" $ mkId "b" 1 9 1)
                    (IntConst "2" $ mkNum "2" 1 13 1)
                    (mkSym Lex.Multiply 1 11 1))
                (Binary Sub
                    (Variable "c" $ mkId "c" 1 18 1)
                    (IntConst "3" $ mkNum "3" 1 22 1)
                    (mkSym Lex.Minus 1 20 1))
                (mkSym Lex.Plus 1 15 1))
            (mkSym Lex.GreaterThan 1 7 1)),
            
    ("5", "(x * 2 + 3) < y - 4 / 2", Right $
        Binary LessThan
            (Binary Add
                (Binary Mul
                    (Variable "x" $ mkId "x" 1 2 1)
                    (IntConst "2" $ mkNum "2" 1 6 1)
                    (mkSym Lex.Multiply 1 4 1))
                (IntConst "3" $ mkNum "3" 1 10 1)
                (mkSym Lex.Plus 1 8 1))
            (Binary Sub
                (Variable "y" $ mkId "y" 1 15 1)
                (Binary Div
                    (IntConst "4" $ mkNum "4" 1 19 1)
                    (IntConst "2" $ mkNum "2" 1 23 1)
                    (mkSym Lex.Divide 1 21 1))
                (mkSym Lex.Minus 1 17 1))
            (mkSym Lex.LessThan 1 13 1)),
            
    ("6", "a + b == c * (d - e) + 1", Right $
        Binary Equal
            (Binary Add
                (Variable "a" $ mkId "a" 1 1 1)
                (Variable "b" $ mkId "b" 1 5 1)
                (mkSym Lex.Plus 1 3 1))
            (Binary Add
                (Binary Mul
                    (Variable "c" $ mkId "c" 1 10 1)
                    (Binary Sub
                        (Variable "d" $ mkId "d" 1 15 1)
                        (Variable "e" $ mkId "e" 1 19 1)
                        (mkSym Lex.Minus 1 17 1))
                    (mkSym Lex.Multiply 1 12 1))
                (IntConst "1" $ mkNum "1" 1 24 1)
                (mkSym Lex.Plus 1 22 1))
            (mkSym Lex.Equal 1 7 2)),
            
    ("7", "(m + 1) != n / 2 - (p + q)", Right $
        Binary NotEqual
            (Binary Add
                (Variable "m" $ mkId "m" 1 2 1)
                (IntConst "1" $ mkNum "1" 1 6 1)
                (mkSym Lex.Plus 1 4 1))
            (Binary Sub
                (Binary Div
                    (Variable "n" $ mkId "n" 1 12 1)
                    (IntConst "2" $ mkNum "2" 1 16 1)
                    (mkSym Lex.Divide 1 14 1))
                (Binary Add
                    (Variable "p" $ mkId "p" 1 21 1)
                    (Variable "q" $ mkId "q" 1 25 1)
                    (mkSym Lex.Plus 1 23 1))
                (mkSym Lex.Minus 1 18 1))
            (mkSym Lex.NotEqual 1 9 2)),
            
    ("8", "!a > b", Right $
        Binary GreaterThan
            (Unary BitNot
                (Variable "a" $ mkId "a" 1 2 1)
                (mkSym Lex.BitNot 1 1 1))
            (Variable "b" $ mkId "b" 1 6 1)
            (mkSym Lex.GreaterThan 1 4 1)),

    ("9", "!a == b", Right $
        Binary Equal
            (Unary BitNot
                (Variable "a" $ mkId "a" 1 2 1)
                (mkSym Lex.BitNot 1 1 1))
            (Variable "b" $ mkId "b" 1 7 1)
            (mkSym Lex.Equal 1 4 2)),

    ("10", "!!a != !b", Right $
        Binary NotEqual
            (Unary BitNot
                (Unary BitNot
                    (Variable "a" $ mkId "a" 1 3 1)
                    (mkSym Lex.BitNot 1 2 1))
                (mkSym Lex.BitNot 1 1 1))
            (Unary BitNot
                (Variable "b" $ mkId "b" 1 9 1)
                (mkSym Lex.BitNot 1 8 1))
            (mkSym Lex.NotEqual 1 5 2)),

    ("11", "!(a < b) == !c", Right $
        Binary Equal
            (Unary BitNot
                (Binary LessThan
                    (Variable "a" $ mkId "a" 1 3 1)
                    (Variable "b" $ mkId "b" 1 7 1)
                    (mkSym Lex.LessThan 1 5 1))
                (mkSym Lex.BitNot 1 1 1))
            (Unary BitNot
                (Variable "c" $ mkId "c" 1 14 1)
                (mkSym Lex.BitNot 1 13 1))
            (mkSym Lex.Equal 1 10 2)),
            
    ("12", "addToMap::<int>(m, 1)", Right $
            CallT
                (Variable "addToMap" (mkId "addToMap" 1 1 8))
                [(Class ["int"] [], [mkId "int" 1 12 3])]
                [Variable "m" (mkId "m" 1 17 1), IntConst "1" (mkNum "1" 1 20 1)]
        ),
        
    ("13", "makePair::<int, double>(x, 2)", Right $
        CallT
            (Variable "makePair" (mkId "makePair" 1 1 8))
            [
                (Class ["int"] [],    [mkId "int"    1 12 3]),
                (Class ["double"] [], [mkId "double" 1 17 6])
            ] [
                Variable "x" (mkId "x" 1 25 1),
                IntConst "2" (mkNum "2" 1 28 1)]
        ),

    ("14", "foo::<HashMap::<List::<Int>>>(h)", Right $
            CallT
                (Variable "foo" (mkId "foo" 1 1 3))
                [(
                    Class ["HashMap"] [Class ["List"] [Class ["Int"] []]], [
                    mkId "HashMap" 1 7 7,
                    mkSym Lex.DoubleColon 1 14  2,
                    mkSym Lex.LessThan    1 16  1,
                    mkId  "List"          1 17  4,
                    mkSym Lex.DoubleColon 1 21  2,
                    mkSym Lex.LessThan    1 23  1,
                    mkId  "Int"           1 24  3,
                    mkSym Lex.GreaterThan 1 27  1,
                    mkSym Lex.GreaterThan 1 28  1])
                ]
                [Variable "h" (mkId "h" 1 31 1)]
        ),

    ("15", "bar::<List::<int>, HashMap::<int>>(a, b)", Right $
        CallT
            (Variable "bar" (mkId "bar" 1 1 3))
                [(
                    Class ["List"] [Class ["int"] []], [
                    mkId "List" 1 7 4,
                    mkSym Lex.DoubleColon 1 11 2,
                    mkSym Lex.LessThan 1 13 1,
                    mkId "int" 1 14 3,
                    mkSym Lex.GreaterThan 1 17 1]), (
                        
                        Class ["HashMap"] [Class ["int"] []], [
                            mkId "HashMap" 1 20  7,
                            mkSym Lex.DoubleColon 1 27 2,
                            mkSym Lex.LessThan 1 29 1,
                            mkId  "int" 1 30 3,
                            mkSym Lex.GreaterThan 1 33 1])
                    ] [
                        Variable "a" (mkId "a" 1 36 1),
                        Variable "b" (mkId "b" 1 39 1)]
        ),

    ("16", "size::<HashMap::<List::<Int>>>(h) + 1", Right $
        Binary Add
            (CallT
                (Variable "size" (mkId "size" 1 1 4))
                [(
                    Class ["HashMap"] [Class ["List"] [Class ["Int"] []]], [
                        mkId  "HashMap" 1 8 7,
                        mkSym Lex.DoubleColon 1 15 2,
                        mkSym Lex.LessThan 1 17 1,
                        mkId  "List" 1 18 4,
                        mkSym Lex.DoubleColon 1 22 2,
                        mkSym Lex.LessThan 1 24 1,
                        mkId  "Int" 1 25 3,
                        mkSym Lex.GreaterThan 1 28 1,
                        mkSym Lex.GreaterThan 1 29 1])
                    ]
                    [Variable "h" (mkId "h" 1 32 1)]
                )
                (IntConst "1" (mkNum "1" 1 37 1))
                (mkSym Lex.Plus 1 35 1)
    )]

tests :: TestTree
tests = testGroup "Parse.ParseExpr" [lexparseExprTests]
