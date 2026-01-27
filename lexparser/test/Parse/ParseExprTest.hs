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
lexparseExprTests = testGroup "Parse.ParseExpr.lexparseExpr" [
    testCase "0" $ replLexparseExpr "1 + 2 - 3" @=? Right
        (Binary Sub
            (Binary Add
                (IntConst "1" $ mkNum "1" 1 1 1)
                (IntConst "2" $ mkNum "2" 1 5 1) $ mkSym Lex.Plus 1 3 1)
            (IntConst "3" $ mkNum "3" 1 9 1) $ mkSym Lex.Minus 1 7 1),

    testCase "1" $ replLexparseExpr "(1 + 2) * (3 - 4) / 5" @=? Right
        (Binary Div
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

    testCase "2" $ replLexparseExpr "-1 + +2 * -3" @=? Right (Binary Add
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
        
    testCase "3" $ replLexparseExpr "a = b = c + 1" @=? Right
      (Binary Assign
            (Variable "a" $ mkId "a" 1 1 1)
            (Binary Assign
                (Variable "b" $ mkId "b" 1 5 1)
                (Binary Add
                    (Variable "c" $ mkId "c" 1 9 1)
                    (IntConst "1" $ mkNum "1" 1 13 1)
                    (mkSym Lex.Plus 1 11 1))
                (mkSym Lex.Assign 1 7 1))
            (mkSym Lex.Assign 1 3 1)),
            
    testCase "4" $ replLexparseExpr "a + 1 > b * 2 + (c - 3)" @=? Right
        (Binary GreaterThan
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
            
    testCase "5" $ replLexparseExpr "(x * 2 + 3) < y - 4 / 2" @=? Right
        (Binary LessThan
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
            
    testCase "6" $ replLexparseExpr "a + b == c * (d - e) + 1" @=? Right
        (Binary Equal
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
            
    testCase "7" $ replLexparseExpr "(m + 1) != n / 2 - (p + q)" @=? Right
        (Binary NotEqual
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
            
    testCase "8" $ replLexparseExpr "!a > b" @=? Right
        (Binary GreaterThan
            (Unary BitNot
                (Variable "a" $ mkId "a" 1 2 1)
                (mkSym Lex.BitNot 1 1 1))
            (Variable "b" $ mkId "b" 1 6 1)
            (mkSym Lex.GreaterThan 1 4 1)),

    testCase "9" $ replLexparseExpr "!a == b" @=? Right
        (Binary Equal
            (Unary BitNot
                (Variable "a" $ mkId "a" 1 2 1)
                (mkSym Lex.BitNot 1 1 1))
            (Variable "b" $ mkId "b" 1 7 1)
            (mkSym Lex.Equal 1 4 2)),

    testCase "10" $ replLexparseExpr "!!a != !b" @=? Right
        (Binary NotEqual
            (Unary BitNot
                (Unary BitNot
                    (Variable "a" $ mkId "a" 1 3 1)
                    (mkSym Lex.BitNot 1 2 1))
                (mkSym Lex.BitNot 1 1 1))
            (Unary BitNot
                (Variable "b" $ mkId "b" 1 9 1)
                (mkSym Lex.BitNot 1 8 1))
            (mkSym Lex.NotEqual 1 5 2)),

    testCase "11" $ replLexparseExpr "!(a < b) == !c" @=? Right
        (Binary Equal
            (Unary BitNot
                (Binary LessThan
                    (Variable "a" $ mkId "a" 1 3 1)
                    (Variable "b" $ mkId "b" 1 7 1)
                    (mkSym Lex.LessThan 1 5 1))
                (mkSym Lex.BitNot 1 1 1))
            (Unary BitNot
                (Variable "c" $ mkId "c" 1 14 1)
                (mkSym Lex.BitNot 1 13 1))
            (mkSym Lex.Equal 1 10 2))]

tests :: TestTree
tests = testGroup "Parse.ParseExpr" [lexparseExprTests]
