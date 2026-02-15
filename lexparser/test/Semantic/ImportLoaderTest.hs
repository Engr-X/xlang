module Semantic.ImportLoaderTest where

import Test.Tasty
import Test.Tasty.HUnit
import Lex.Token (Token, tokenPos)
import Semantic.ImportLoader (loadSrcI)
import Semantic.NameEnv (ImportEnv(..))
import Util.Type (Position, makePosition)

import qualified Data.Map.Strict as Map
import qualified Lex.Token as Lex
import qualified Parse.SyntaxTree as AST
import qualified Util.Exception as UE


loadSrcITests :: TestTree
loadSrcITests = testGroup "Semantic.ImportLoader.loadSrcI" [
    testCase "0" $ loadSrcI "stdin" program0 @?= Right expected0,
    testCase "1" $ loadSrcI "stdin" program1 @?= Left [errDup],
    testCase "2" $ loadSrcI "stdin" program2 @?= Left [errQualifiedVar],
    testCase "3" $ loadSrcI "stdin" program3 @?= Left [errQualifiedFun]]
    where
        posPkg, posX1, posX2, posF, posA, posB, posAssign, posNum :: Position
        posPkg = makePosition 1 1 3
        posX1 = makePosition 2 1 1
        posX2 = makePosition 3 1 1
        posF = makePosition 4 1 1
        posA = makePosition 5 1 1
        posB = makePosition 5 3 1
        posAssign = makePosition 2 3 1
        posNum = makePosition 2 5 1

        tokPkg, tokX1, tokX2, tokF, tokA, tokB, tokAssign, tokNum :: Token
        tokPkg = Lex.Ident "pkg" posPkg
        tokX1 = Lex.Ident "x" posX1
        tokX2 = Lex.Ident "x" posX2
        tokF = Lex.Ident "f" posF
        tokA = Lex.Ident "a" posA
        tokB = Lex.Ident "b" posB
        tokAssign = Lex.Symbol Lex.Assign posAssign
        tokNum = Lex.NumberConst "1" posNum

        rhsConst :: AST.Expression
        rhsConst = AST.IntConst "1" tokNum

        assignX1, assignX2, assignQualified :: AST.Statement
        assignX1 = AST.Expr (AST.Binary AST.Assign (AST.Variable "x" tokX1) rhsConst tokAssign)
        assignX2 = AST.Expr (AST.Binary AST.Assign (AST.Variable "x" tokX2) rhsConst tokAssign)
        assignQualified =
            AST.Expr (AST.Binary AST.Assign (AST.Qualified ["a", "b"] [tokA, tokB]) rhsConst tokAssign)

        funStmt, funQualified :: AST.Statement
        funStmt = AST.Function (AST.Int32T, []) (AST.Variable "f" tokF) [] (AST.Multiple [])
        funQualified =
            AST.Function (AST.Int32T, []) (AST.Qualified ["a", "b"] [tokA, tokB]) [] (AST.Multiple [])

        program0, program1, program2, program3 :: AST.Program
        program0 = ([AST.Package ["pkg"] [tokPkg]], [assignX1, funStmt])
        program1 = ([], [assignX1, assignX2])
        program2 = ([], [assignQualified])
        program3 = ([], [funQualified])

        expected0 :: ImportEnv
        expected0 = IEnv {
            file = "stdin",
            iVars = Map.fromList [(["pkg", "x"], [posX1])],
            iFuncs = Map.fromList [(["pkg", "f"], [posF])]
        }

        errDup, errQualifiedVar, errQualifiedFun :: UE.ErrorKind
        errDup = UE.Syntax $ UE.makeError "stdin" [tokenPos tokX1] (UE.multipleVariableDefMsg "x")
        errQualifiedVar = UE.Syntax $ UE.makeError "stdin" (map tokenPos [tokA, tokB]) UE.assignErrorMsg
        errQualifiedFun = UE.Syntax $ UE.makeError "stdin" (map tokenPos [tokA, tokB]) UE.unsupportedErrorMsg


tests :: TestTree
tests = testGroup "Semantic.ImportLoader" [loadSrcITests]
