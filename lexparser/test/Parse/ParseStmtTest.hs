module Parse.ParseStmtTest where

import Test.Tasty
import Test.Tasty.HUnit
import Parse.SyntaxTree
import Parse.ParseStmt
import Util.Type
import Lex.Token (Token, Symbol)

import qualified Lex.Token as Lex


mkSym :: Symbol -> Int -> Int -> Int -> Token
mkSym s a b c = Lex.Symbol s $ Position a b c

mkNum :: String -> Int -> Int -> Int -> Token
mkNum s a b c = Lex.NumberConst s $ Position a b c

mkId :: String -> Int -> Int -> Int -> Token
mkId s a b c = Lex.Ident s $ makePosition a b c


lexparseStmtTests :: TestTree
lexparseStmtTests = testGroup "Parse.ParseStmt.lexparseStmt" $ map (\(n, src, expected) ->
    testCase n $ replLexparseStmt src @=? Right expected) [
        ("0", "1 + 2 * 3;", Expr (
            Binary Add
                (IntConst "1" $ mkNum "1" 1 1 1)
                (Binary Mul
                    (IntConst "2" $ mkNum "2" 1 5 1)
                    (IntConst "3" $ mkNum "3" 1 9 1)
                    (mkSym Lex.Multiply 1 7 1))
                (mkSym Lex.Plus 1 3 1))),

        ("1", "{ 1 + 2; 3 * 4; }", BlockStmt (
            Multiple
            [Expr (Binary Add
                (IntConst "1" $ mkNum "1" 1 3 1)
                (IntConst "2" $ mkNum "2" 1 7 1)
                (mkSym Lex.Plus 1 5 1))
            , Expr (Binary Mul
                (IntConst "3" $ mkNum "3" 1 10 1)
                (IntConst "4" $ mkNum "4" 1 14 1)
                (mkSym Lex.Multiply 1 12 1))])),
                
        ("2", "{ { 1; } 2; }", BlockStmt (
            Multiple
            [BlockStmt (Multiple
                [Expr (IntConst "1" $ mkNum "1" 1 5 1)]),
                Expr (IntConst "2" $ mkNum "2" 1 10 1)])),
                
        ("3", "{ 1 + 2\n  3 + 4\n}", BlockStmt (
            Multiple
            [Expr (Binary Add
                (IntConst "1" $ mkNum "1" 1 3 1)
                (IntConst "2" $ mkNum "2" 1 7 1)
                (mkSym Lex.Plus 1 5 1))
            , Expr (Binary Add
                (IntConst "3" $ mkNum "3" 2 3 1)
                (IntConst "4" $ mkNum "4" 2 7 1)
                (mkSym Lex.Plus 2 5 1))]))]
  where
    mkSym :: Symbol -> Int -> Int -> Int -> Token
    mkSym s a b c = Lex.Symbol s $ Position a b c

    mkNum :: String -> Int -> Int -> Int -> Token
    mkNum s a b c = Lex.NumberConst s $ Position a b c



tests :: TestTree
tests = testGroup "Parse.ParseStmt" [lexparseStmtTests]
