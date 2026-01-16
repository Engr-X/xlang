module Parse.ParserTest where


import Test.Tasty
import Test.Tasty.HUnit
import Lex.Token
import Parse.Parser


{-checkBracketTests :: TestTree
checkBracketTests = testGroup "Parse.Parser.checkBracket" [
    testCase "empty input" $ checkBracket [] @=? True,
    testCase "single pair " $ checkBracket [Symbol LParen, Symbol RParen] @=? True, -- 
    testCase "single pair []" $ checkBracket [Symbol LBracket, Symbol RBracket] @=? True, -- []
    testCase "single pair {}" $ checkBracket [Symbol LBrace, Symbol RBrace] @=? True, -- {}
    testCase "nested brackets" $ checkBracket [Symbol LParen, Symbol LBracket, Symbol LBrace, Symbol RBrace, Symbol RBracket, Symbol RParen] @=? True,
    testCase "sequential brackets" $ checkBracket [Symbol LParen, Symbol RParen, Symbol LBracket, Symbol RBracket, Symbol LBrace, Symbol RBrace] @=? True,
    testCase "crossed brackets (wrong order)" $ checkBracket [Symbol LParen, Symbol LBracket, Symbol RParen, Symbol RBracket] @=? False,
    testCase "extra left bracket" $ checkBracket [Symbol LParen, Symbol LBracket, Symbol RBracket] @=? False,
    testCase "extra right bracket" $ checkBracket [Symbol LParen, Symbol RParen, Symbol RParen] @=? False,
    testCase "ignore non-bracket tokens" $ checkBracket [Symbol LParen, Ident "x", Symbol LBracket, IntLit 123, Symbol RBracket, Symbol RParen] @=? True] -}


tests :: TestTree
tests = testGroup "Parse.Parser" []
