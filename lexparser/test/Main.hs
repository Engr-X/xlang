module Main (main) where


import Lex.TokenTest
import Lex.TokenizerTest

import Parse.ParserTest
import Parse.SyntaxTreeTest

import Util.BasicTest
import Util.ExceptionTest
import Util.FileHelperTest
import Util.TypeTest

import Test.Tasty


main :: IO ()
main = defaultMain $ testGroup "All tests" [
    Util.BasicTest.tests, Util.TypeTest.tests, Util.ExceptionTest.tests, Util.FileHelperTest.tests,
    Lex.TokenTest.tests, Lex.TokenizerTest.tests,
    Parse.SyntaxTreeTest.tests, Parse.ParserTest.tests]
