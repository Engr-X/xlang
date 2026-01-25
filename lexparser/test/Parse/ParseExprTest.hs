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

    testCase "2" $ replLexparseExpr "(1 + 2) * (3 - 4) / 5" @=? Right
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

    testCase "3" $ replLexparseExpr "-1 + +2 * -3" @=? Right (Binary Add
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
        
    testCase "4" $ replLexparseExpr "a = b = c + 1" @=? Right
      (Binary Assign
            (Variable "a" $ mkId "a" 1 1 1)
            (Binary Assign
                (Variable "b" $ mkId "b" 1 5 1)
                (Binary Add
                    (Variable "c" $ mkId "c" 1 9 1)
                    (IntConst "1" $ mkNum "1" 1 13 1)
                    (mkSym Lex.Plus 1 11 1))
                (mkSym Lex.Assign 1 7 1))
            (mkSym Lex.Assign 1 3 1))]

tests :: TestTree
tests = testGroup "Parse.ParseExpr" [lexparseExprTests]
