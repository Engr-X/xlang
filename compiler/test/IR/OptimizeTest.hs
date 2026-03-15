module IR.OptimizeTest where

import IR.Optimize
import IR.TAC (IRAtom(..), IRBlock(..), IRClass(..), IRFunction(..), IRInstr(..), IRMemberType(..), IRProgm(..), IRStmt(..), MainKind(..), StaticInit(..))
import Parse.ParserBasic (AccessModified(..))
import Parse.SyntaxTree (Class(..))
import Semantic.TypeEnv (FunSig(..))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map.Strict as Map


decl0 :: (AccessModified, [a])
decl0 = (Public, [])

phiAtom :: IRAtom
phiAtom = Phi [(1, Int32C 1), (2, Int32C 2)]

lhsAtom :: IRAtom
lhsAtom = Var ("x", 0, 0)

keepStmt :: IRStmt
keepStmt = IRInstr (IAssign lhsAtom (Int32C 7))

phiStmt :: IRStmt
phiStmt = IRInstr (IAssign lhsAtom phiAtom)

mkFun :: [IRStmt] -> IRFunction
mkFun stmts =
    IRFunction (Public, []) "f" (FunSig [] Void) Map.empty stmts MemberClass

mkClass :: [IRStmt] -> [IRFunction] -> IRClass
mkClass staticStmts funs =
    IRClass (Public, []) "C" [] (StaticInit staticStmts) Map.empty funs NoMain


stripPhiStmtTests :: TestTree
stripPhiStmtTests = testGroup "IR.Optimize.stripPhiStmt" $ map (uncurry testCase) [
    ("0", stripPhiStmt phiStmt @?= []),
    ("1", stripPhiStmt keepStmt @?= [keepStmt]),
    ("2", do
        let stmt = IRBlockStmt (IRBlock (1, [phiStmt, keepStmt]))
            out = IRBlockStmt (IRBlock (1, [keepStmt]))
        stripPhiStmt stmt @?= [out]),
    ("3", do
        let nested = IRBlockStmt (IRBlock (1, [IRBlockStmt (IRBlock (2, [phiStmt, keepStmt]))]))
            out = IRBlockStmt (IRBlock (1, [IRBlockStmt (IRBlock (2, [keepStmt]))]))
        stripPhiStmt nested @?= [out])]


stripPhiStmtsTests :: TestTree
stripPhiStmtsTests = testGroup "IR.Optimize.stripPhiStmts" $ map (uncurry testCase) [
    ("0", stripPhiStmts [] @?= []),
    ("1", stripPhiStmts [phiStmt, keepStmt] @?= [keepStmt]),
    ("2", do
        let inp = [IRBlockStmt (IRBlock (1, [phiStmt, keepStmt]))]
            out = [IRBlockStmt (IRBlock (1, [keepStmt]))]
        stripPhiStmts inp @?= out),
    ("3", stripPhiStmts [phiStmt, phiStmt] @?= [])]


stripPhiProgmTests :: TestTree
stripPhiProgmTests = testGroup "IR.Optimize.stripPhiProgm" $ map (uncurry testCase) [
    ("0", do
        let p = IRProgm [] []
        stripPhiProgm p @?= p),
    ("1", do
        let p = IRProgm [] [mkClass [phiStmt, keepStmt] []]
            out = IRProgm [] [mkClass [keepStmt] []]
        stripPhiProgm p @?= out),
    ("2", do
        let p = IRProgm [] [mkClass [] [mkFun [phiStmt, keepStmt]]]
            out = IRProgm [] [mkClass [] [mkFun [keepStmt]]]
        stripPhiProgm p @?= out),
    ("3", do
        let nested = IRBlockStmt (IRBlock (1, [phiStmt, keepStmt]))
            p = IRProgm [] [mkClass [] [mkFun [nested]]]
            outNested = IRBlockStmt (IRBlock (1, [keepStmt]))
            out = IRProgm [] [mkClass [] [mkFun [outNested]]]
        stripPhiProgm p @?= out)]


tests :: TestTree
tests = testGroup "IR.Optimize" [
    stripPhiStmtTests,
    stripPhiStmtsTests,
    stripPhiProgmTests]
