module Main (main) where


import Lexer.TokenizerTest

import Test.Tasty


main :: IO ()
main = defaultMain $ testGroup "All tests" [
    Lexer.TokenizerTest.tests]
