module IR.TACLowingTest where

import Test.Tasty (TestTree, testGroup)
import Data.List (isSuffixOf, nub)
import Test.Tasty.HUnit (testCase, assertBool, assertFailure)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import IR.Lowing (codeToIR)
import IR.TAC (
    IRProgm(..),
    IRClass(..),
    IRFunction(..),
    IRStmt(..),
    IRBlock(..),
    IRInstr(..),
    IRAtom(..),
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


functionBlocks :: String -> IRProgm -> [[IRInstr]]
functionBlocks fname (IRProgm _ classes) = concatMap classBlocks classes
    where
        classBlocks :: IRClass -> [[IRInstr]]
        classBlocks (IRClass _ _ _ _ _ funs _) = concatMap funBlocks funs

        funBlocks :: IRFunction -> [[IRInstr]]
        funBlocks (IRFunction _ name _ _ stmts)
            | name == fname = map blockInstrs [blk | IRBlockStmt blk <- stmts]
            | otherwise = []

        blockInstrs :: IRBlock -> [IRInstr]
        blockInstrs (IRBlock (_, ss)) = [instr | IRInstr instr <- ss]


functionBlockEntries :: String -> IRProgm -> [(Int, [IRInstr])]
functionBlockEntries fname (IRProgm _ classes) = concatMap classBlocks classes
    where
        classBlocks :: IRClass -> [(Int, [IRInstr])]
        classBlocks (IRClass _ _ _ _ _ funs _) = concatMap funBlocks funs

        funBlocks :: IRFunction -> [(Int, [IRInstr])]
        funBlocks (IRFunction _ name _ _ stmts)
            | name == fname = map blockEntry [blk | IRBlockStmt blk <- stmts]
            | otherwise = []

        blockEntry :: IRBlock -> (Int, [IRInstr])
        blockEntry (IRBlock (bid, ss)) = (bid, [instr | IRInstr instr <- ss])


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


whileCompareBranchOrderTest :: TestTree
whileCompareBranchOrderTest = testCase "while compare keeps two-step branch in separate blocks" $
    case codeToIR "<test>" (unlines [
        "int sum(int from, int to) {",
        "    sum = 0;",
        "    while (from < to) {",
        "        sum = sum + from;",
        "        from = from + 1;",
        "    }",
        "    return sum;",
        "}"
        ]) of
        Left errs -> assertFailure ("codeToIR failed: " ++ show errs)
        Right (ir, _) -> do
            let blocks = functionBlocks "sum" ir
                isIflt instr = case instr of
                    Iflt {} -> True
                    _ -> False
                isIfeqBool instr = case instr of
                    Ifeq _ (Int32C 1) _ -> True
                    _ -> False
                headerBlocks = filter (any isIflt) blocks
                hasIfeqInHeader = any (any isIfeqBool) headerBlocks
                hasIfeqElsewhere = any (any isIfeqBool) blocks

            assertBool "expected while header compare block" (not (null headerBlocks))
            assertBool "expected no second-step ifeq in compare header block" (not hasIfeqInHeader)
            assertBool "expected second-step ifeq in a later block" hasIfeqElsewhere


whileIfBodyNoDoubleGotoTest :: TestTree
whileIfBodyNoDoubleGotoTest = testCase "while body if should not emit consecutive gotos" $
    case codeToIR "<test>" (unlines [
        "bool isPrime(int i) {",
        "    test = 2;",
        "    while (test < i) {",
        "        if (i % test == 0):",
        "            return false;",
        "    }",
        "    return true;",
        "}"
        ]) of
        Left errs -> assertFailure ("codeToIR failed: " ++ show errs)
        Right (ir, _) -> do
            let blocks = functionBlocks "isPrime" ir
                isJump instr = case instr of
                    Jump {} -> True
                    _ -> False
                hasAdjacentJumps [] = False
                hasAdjacentJumps [_] = False
                hasAdjacentJumps (a:b:rest)
                    | isJump a && isJump b = True
                    | otherwise = hasAdjacentJumps (b:rest)
                bad = any hasAdjacentJumps blocks
            assertBool "expected no consecutive gotos in any block" (not bad)


whileIfBodyNoDanglingLabelTest :: TestTree
whileIfBodyNoDanglingLabelTest = testCase "while body if should not reference missing labels" $
    case codeToIR "<test>" (unlines [
        "bool isPrime(int i) {",
        "    test = 2;",
        "    while (test < i) {",
        "        if (i % test == 0):",
        "            return false;",
        "    }",
        "    return true;",
        "}"
        ]) of
        Left errs -> assertFailure ("codeToIR failed: " ++ show errs)
        Right (ir, _) -> do
            let entries = functionBlockEntries "isPrime" ir
                blockIds = map fst entries
                hasBlock bid = bid `elem` blockIds
                targets instr = case instr of
                    Jump bid -> [bid]
                    Ifeq _ _ bid -> [bid]
                    Ifne _ _ bid -> [bid]
                    Iflt _ _ bid -> [bid]
                    Ifle _ _ bid -> [bid]
                    Ifgt _ _ bid -> [bid]
                    Ifge _ _ bid -> [bid]
                    _ -> []
                allTargets = concatMap (concatMap targets . snd) entries
                dangling = filter (not . hasBlock) allTargets
            assertBool ("dangling labels: " ++ show dangling) (null dangling)


findPrimeNestedElseReachabilityTest :: TestTree
findPrimeNestedElseReachabilityTest = testCase "if-else with nested while keeps else body reachable" $
    case codeToIR "<test>" (unlines [
        "void findPrime(int from, int to) {",
        "    while (from < to) {",
        "        if (from == 2):",
        "            putln(from);",
        "        else {",
        "            i = 2;",
        "            isPrime = true;",
        "            while (i < from) {",
        "                if (from % i == 0):",
        "                    isPrime = false;",
        "                i = i + 1;",
        "            }",
        "            if (isPrime):",
        "                putln(from);",
        "        }",
        "        from = from + 1;",
        "    }",
        "}",
        "void main() {",
        "    findPrime(2, 1000);",
        "}"
        ]) of
        Left errs -> assertFailure ("codeToIR failed: " ++ show errs)
        Right (ir, _) -> do
            let entries = functionBlockEntries "findPrime" ir
                blockIds = map fst entries
                blockMap = Map.fromList entries
                orderedNext = Map.fromList (zip blockIds (map Just (drop 1 blockIds) ++ [Nothing]))

                targets instr = case instr of
                    Jump bid -> [bid]
                    Ifeq _ _ bid -> [bid]
                    Ifne _ _ bid -> [bid]
                    Iflt _ _ bid -> [bid]
                    Ifle _ _ bid -> [bid]
                    Ifgt _ _ bid -> [bid]
                    Ifge _ _ bid -> [bid]
                    _ -> []

                isTerminator instrs = case reverse instrs of
                    (Jump _ : _) -> True
                    (IReturn : _) -> True
                    (Return : _) -> True
                    _ -> False

                succs bid instrs =
                    let explicit = concatMap targets instrs
                        fallthrough = case Map.lookup bid orderedNext of
                            Just (Just nextBid)
                                | not (isTerminator instrs) -> [nextBid]
                            _ -> []
                    in nub (explicit ++ fallthrough)

                visit seen [] = seen
                visit seen (bid:queue)
                    | Set.member bid seen = visit seen queue
                    | otherwise =
                        let instrs = Map.findWithDefault [] bid blockMap
                            nextBids = succs bid instrs
                        in visit (Set.insert bid seen) (queue ++ nextBids)

                reachable = case blockIds of
                    [] -> Set.empty
                    (entry:_) -> visit Set.empty [entry]
                unreachable = filter (`Set.notMember` reachable) blockIds

            assertBool ("unreachable blocks in findPrime: " ++ show unreachable) (null unreachable)


tests :: TestTree
tests = testGroup "IR.TACLowing" [
    compareEqLoweringTest,
    compareLtLoweringTest,
    mainIntDetectTest,
    mainVoidDetectTest,
    whileCompareBranchOrderTest,
    whileIfBodyNoDoubleGotoTest,
    whileIfBodyNoDanglingLabelTest,
    findPrimeNestedElseReachabilityTest]
