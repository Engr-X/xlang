module IR.TACLowingTest where

import Test.Tasty (TestTree, testGroup)
import Data.List (isSuffixOf)
import Test.Tasty.HUnit (testCase, assertBool, assertFailure)

import IR.Lowing (codeToIR)
import IR.TAC (
    IRProgm(..),
    IRClass(..),
    IRFunction(..),
    IRStmt(..),
    IRBlock(..),
    IRInstr(..),
    StaticInit(..),
    MainKind(..))


collectInstrs :: [IRStmt] -> [IRInstr]
collectInstrs = concatMap collectStmt
    where
        collectStmt :: IRStmt -> [IRInstr]
        collectStmt (IRInstr i) = [i]
        collectStmt (IRBlockStmt (IRBlock (_, ss))) = concatMap collectStmt ss


collectInstrsProgm :: IRProgm -> [IRInstr]
collectInstrsProgm (IRProgm _ classes) = concatMap collectClass classes
    where
        collectClass :: IRClass -> [IRInstr]
        collectClass (IRClass _ _ _ (StaticInit stmts) _ funs _) =
            collectInstrs stmts ++ concatMap collectFun funs

        collectFun :: IRFunction -> [IRInstr]
        collectFun (IRFunction _ _ _ _ stmts) = collectInstrs stmts


assertHasInstr :: (IRInstr -> Bool) -> String -> String -> IO ()
assertHasInstr pred' src msg = case codeToIR "<test>" src of
    Left errs -> assertFailure (msg ++ " (codeToIR failed: " ++ show errs ++ ")")
    Right (ir, _) ->
        assertBool msg (any pred' (collectInstrsProgm ir))


compareEqLoweringTest :: TestTree
compareEqLoweringTest = testCase "compare eq uses Ifeq" $
    assertHasInstr isIfeq
        (unlines [
            "b = 2;",
            "a = 1 == b;"
        ])
        "expected Ifeq in IR"
    where
        isIfeq :: IRInstr -> Bool
        isIfeq instr = case instr of
            Ifeq {} -> True
            _ -> False


compareLtLoweringTest :: TestTree
compareLtLoweringTest = testCase "compare lt uses Iflt" $
    assertHasInstr isIflt
        (unlines [
            "b = 2;",
            "a = 1 < b;"
        ])
        "expected Iflt in IR"
    where
        isIflt :: IRInstr -> Bool
        isIflt instr = case instr of
            Iflt {} -> True
            _ -> False


assertMainKind :: (MainKind -> Bool) -> String -> String -> IO ()
assertMainKind pred' src msg = case codeToIR "<test>" src of
    Left errs -> assertFailure ("codeToIR failed: " ++ show errs)
    Right (IRProgm _ [IRClass _ _ _ _ _ _ kind], _) -> assertBool msg (pred' kind)
    Right (IRProgm _ classes, _) ->
        assertFailure ("expected one class, got " ++ show (length classes))


mainIntDetectTest :: TestTree
mainIntDetectTest = testCase "detect int main()" $
    assertMainKind isMainInt (unlines [
        "int main() {",
        "    return 0;",
        "}"
    ]) "expected MainInt with qname ending in main"
    where
        isMainInt :: MainKind -> Bool
        isMainInt mk = case mk of
            MainInt qn -> ["main"] `isSuffixOf` qn
            _ -> False


mainVoidDetectTest :: TestTree
mainVoidDetectTest = testCase "detect void main()" $
    assertMainKind isMainVoid (unlines [
        "void main() {",
        "    return;",
        "}"
    ]) "expected MainVoid with qname ending in main"
    where
        isMainVoid :: MainKind -> Bool
        isMainVoid mk = case mk of
            MainVoid qn -> ["main"] `isSuffixOf` qn
            _ -> False


tests :: TestTree
tests = testGroup "IR.TACLowing" [
    compareEqLoweringTest,
    compareLtLoweringTest,
    mainIntDetectTest,
    mainVoidDetectTest]
