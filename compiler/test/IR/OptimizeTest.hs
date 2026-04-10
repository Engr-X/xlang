module IR.OptimizeTest where

import IR.Optimize
import IR.TAC (IRAtom(..), IRBlock(..), IRClass(..), IRFunction(..), IRInstr(..), IRMemberType(..), IRProgm(..), MainKind(..), StaticInit(..))
import Parse.ParserBasic (AccessModified(..))
import Parse.SyntaxTree (Class(..))
import Semantic.TypeEnv (FunSig(..))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map.Strict as Map


phiAtom :: IRAtom
phiAtom = Phi [(1, Int32C 1), (2, Int32C 2)]

lhsAtom :: IRAtom
lhsAtom = Var ("x", 0, 0)

keepInstr :: IRInstr
keepInstr = IAssign lhsAtom (Int32C 7)

phiInstr :: IRInstr
phiInstr = IAssign lhsAtom phiAtom

mkFun :: [IRInstr] -> IRFunction
mkFun instrs =
    IRFunction (Public, []) "f" (FunSig [] Void) Map.empty ([IRBlock (0, instrs)], 0) MemberClass

mkClass :: [IRInstr] -> [IRFunction] -> IRClass
mkClass staticInstrs funs =
    IRClass (Public, []) "C" [] (StaticInit ([IRBlock (0, staticInstrs)], 0)) Map.empty funs NoMain


stripPhiStmtTests :: TestTree
stripPhiStmtTests = testGroup "IR.Optimize.stripPhiStmt" $ map (uncurry testCase) [
    ("0", stripPhiStmt phiInstr @?= []),
    ("1", stripPhiStmt keepInstr @?= [keepInstr])]


stripPhiStmtsTests :: TestTree
stripPhiStmtsTests = testGroup "IR.Optimize.stripPhiStmts" $ map (uncurry testCase) [
    ("0", stripPhiStmts [] @?= []),
    ("1", stripPhiStmts [phiInstr, keepInstr] @?= [keepInstr]),
    ("2", stripPhiStmts [phiInstr, phiInstr] @?= [])]


stripPhiProgmTests :: TestTree
stripPhiProgmTests = testGroup "IR.Optimize.stripPhiProgm" $ map (uncurry testCase) [
    ("0", do
        let p = IRProgm [] []
        stripPhiProgm p @?= p),
    ("1", do
        let p = IRProgm [] [mkClass [phiInstr, keepInstr] []]
            out = IRProgm [] [mkClass [keepInstr] []]
        stripPhiProgm p @?= out),
    ("2", do
        let p = IRProgm [] [mkClass [] [mkFun [phiInstr, keepInstr]]]
            out = IRProgm [] [mkClass [] [mkFun [keepInstr]]]
        stripPhiProgm p @?= out)]


tests :: TestTree
tests = testGroup "IR.Optimize" [
    stripPhiStmtTests,
    stripPhiStmtsTests,
    stripPhiProgmTests]
