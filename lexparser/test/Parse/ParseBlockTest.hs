module Parse.ParseBlockTest where

import Test.Tasty
import Test.Tasty.HUnit
import Parse.SyntaxTree
import Parse.ParseBlock
import Util.Type
import Lex.Token (Token)

import qualified Lex.Token as Lex


lexparseBlockTests :: TestTree
lexparseBlockTests = testGroup "Parse.ParseBlock.lexparseBlock" $ map (\(n, src, expected) ->
    testCase n $ replLexparseBlock src @=? Right expected) [
    ("0", unlines [
        "{",
        "}"],
        Multiple []),
        
    ("1", unlines [
        "{",
        "1",
        "}"],
        Multiple [Expr (IntConst "1" $ mkNum "1" 2 1 1)]),
        
    ("2", unlines [
        "{",
        "1",
        "2",
        "}"],
        
        Multiple [Expr (IntConst "1" $ mkNum "1" 2 1 1), Expr (IntConst "2" $ mkNum "2" 3 1 1)]),

    ("3", unlines [
        "{",
        "{",
        "1",
        "}",
        "2",
        "}"],
        
        Multiple [
            BlockStmt (Multiple
            [Expr (IntConst "1" $ mkNum "1" 3 1 1)]),
            Expr (IntConst "2" $ mkNum "2" 5 1 1)])]
    where
        mkNum :: String -> Int -> Int -> Int -> Token
        mkNum s a b c = Lex.NumberConst s $ Position a b c


tests :: TestTree
tests = testGroup "Parse.ParseBlock" [lexparseBlockTests]
