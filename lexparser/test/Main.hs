module Main (main) where

import Lex.TokenPassTest
import Lex.TokenTest
import Lex.TokenizerTest

import Parse.ParserBasicTest
import Parse.SyntaxTreeTest
import Parse.ParseExprTest
import Parse.ParseBlockTest
import Parse.ParseStmtTest
import Parse.ParseProgmTest

import Semantic.NameEnvTest
import Semantic.ImportLoaderTest

import Util.BasicTest
import Util.ExceptionTest
import Util.FileHelperTest
import Util.TypeTest

import Test.Tasty


main :: IO ()
main = defaultMain $ testGroup "xlang test" [
    Util.BasicTest.tests, Util.TypeTest.tests, Util.ExceptionTest.tests, Util.FileHelperTest.tests,
    Lex.TokenTest.tests, Lex.TokenizerTest.tests, Lex.TokenPassTest.tests,
    
    Parse.ParserBasicTest.tests, Parse.SyntaxTreeTest.tests,
    Parse.ParseExprTest.tests, Parse.ParseBlockTest.tests, Parse.ParseStmtTest.tests, Parse.ParseProgmTest.tests,
    
    Semantic.NameEnvTest.tests, Semantic.ImportLoaderTest.tests]
