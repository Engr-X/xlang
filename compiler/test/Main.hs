module Main (main) where

import Lex.TokenPassTest
import Lex.TokenizerTest

import Parse.ParserBasicTest
import Parse.SyntaxTreeTest
import Parse.ParseExprTest
import Parse.ParseBlockTest
import Parse.ParseStmtTest
import Parse.ParseProgmTest

import IR.LowingTest
import IR.OptimizeTest
import IR.TACLowingTest
import IR.TACTest

import Lowing.JVMTest
import Lowing.JVMJsonTest
import Lowing.JVMLowingTest

import Semantic.NameEnvTest
import Semantic.ContextCheckTest
import Semantic.CheckProgramTest
import Semantic.OpInferTest
import Semantic.TypeCheckTest
import Semantic.TypeEnvTest
import Semantic.ReturnCheckTest
import Semantic.SemanticDebugTest
import Semantic.NativeLibLoaderTest

import Util.BasicTest
import Util.CompileJavaPolicyTest
import Util.ExceptionTest
import Util.TypeTest

import Test.Tasty


main :: IO ()
main = defaultMain $ testGroup "xlang test" [
    Util.BasicTest.tests, Util.TypeTest.tests, Util.ExceptionTest.tests, Util.CompileJavaPolicyTest.tests,
    Lex.TokenizerTest.tests, Lex.TokenPassTest.tests,

    Parse.ParserBasicTest.tests, Parse.SyntaxTreeTest.tests,
    Parse.ParseExprTest.tests, Parse.ParseBlockTest.tests, Parse.ParseStmtTest.tests, Parse.ParseProgmTest.tests,

    IR.LowingTest.tests, IR.TACTest.tests, IR.TACLowingTest.tests, IR.OptimizeTest.tests,
    Lowing.JVMTest.tests, Lowing.JVMJsonTest.tests, Lowing.JVMLowingTest.tests,

    Semantic.NameEnvTest.tests, Semantic.ContextCheckTest.tests,
    Semantic.CheckProgramTest.tests,
    Semantic.OpInferTest.tests, Semantic.TypeEnvTest.tests, Semantic.TypeCheckTest.tests,
    Semantic.ReturnCheckTest.tests, Semantic.SemanticDebugTest.tests, Semantic.NativeLibLoaderTest.tests
    ]
