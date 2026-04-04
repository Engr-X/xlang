module IR.Liveness where

import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.HashSet (HashSet)
import IR.TAC (IRAtom(..), IRInstr(..), IRBlock, getInstrSuccs, isConstAtom)

import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import qualified IR.TAC as TAC

-- this one should be run for registers allocated and run after span phi


blockSucc :: [IRBlock] -> Map Int (HashSet Int)
blockSucc blocks = Map.fromList (map blockEntry indexedBlocks)
    where
        indexedBlocks :: [(Int, IRBlock)]
        indexedBlocks = zip [0..] blocks

        blockEntry :: (Int, IRBlock) -> (Int, HashSet Int)
        blockEntry (idx, blk@(TAC.IRBlock (bid, stmts'))) =
            let innerSucc = blockInnerSucc blk
                nextSucc = maybe HashSet.empty HashSet.singleton (fallthroughSucc idx stmts')
            in (bid, HashSet.union innerSucc nextSucc)

        blockInnerSucc :: IRBlock -> HashSet Int
        blockInnerSucc (TAC.IRBlock (_, instrs)) =
            HashSet.fromList (concatMap getInstrSuccs instrs)

        fallthroughSucc :: Int -> [IRInstr] -> Maybe Int
        fallthroughSucc idx instrs
            | hasFallthrough instrs = nextBlockLabel idx
            | otherwise = Nothing

        nextBlockLabel :: Int -> Maybe Int
        nextBlockLabel idx = case drop (idx + 1) blocks of
            (TAC.IRBlock (nextBid, _) : _) -> Just nextBid
            _ -> Nothing

        hasFallthrough :: [IRInstr] -> Bool
        hasFallthrough [] = True
        hasFallthrough xs = not (isTerminator (last xs))

        isTerminator :: IRInstr -> Bool
        isTerminator (Jump _) = True
        isTerminator (Ifeq _ _ _) = True
        isTerminator (Ifne _ _ _) = True
        isTerminator (Iflt _ _ _) = True
        isTerminator (Ifle _ _ _) = True
        isTerminator (Ifgt _ _ _) = True
        isTerminator (Ifge _ _ _) = True
        isTerminator Return = True
        isTerminator VReturn = True
        isTerminator _ = False


-- Require and define (as lists; callers can deduplicate with Set/HashSet as needed).
instrRD :: IRInstr -> Maybe ([IRAtom], [IRAtom])
instrRD (TAC.Jump _) = Nothing
instrRD (TAC.Ifeq a b _) = mkRD [a, b] []
instrRD (TAC.Ifne a b _) = mkRD [a, b] []
instrRD (TAC.Iflt a b _) = mkRD [a, b] []
instrRD (TAC.Ifge a b _) = mkRD [a, b] []
instrRD (TAC.Ifgt a b _) = mkRD [a, b] []
instrRD (TAC.Ifle a b _) = mkRD [a, b] []
instrRD (TAC.SetRet a) = mkRD [a] []
instrRD TAC.Return = Nothing
instrRD TAC.VReturn = Nothing
instrRD (TAC.IAssign dst src) = mkRD [src] [dst]
instrRD (TAC.IUnary dst _ x) = mkRD [x] [dst]
instrRD (TAC.IBinary dst _ x y) = mkRD [x, y] [dst]
instrRD (TAC.ICast dst _ x) = mkRD [x] [dst]
instrRD (TAC.ICall dst _ args) = mkRD args [dst]
instrRD (TAC.ICallStatic dst _ args) = mkRD args [dst]
instrRD (TAC.IGetField dst obj _) = mkRD [obj] [dst]
instrRD (TAC.IPutField obj _ v) = mkRD [obj, v] []
instrRD (TAC.IGetStatic dst _) = mkRD [] [dst]
instrRD (TAC.IPutStatic _ v) = mkRD [v] []

mkRD :: [IRAtom] -> [IRAtom] -> Maybe ([IRAtom], [IRAtom])
mkRD req def = Just (normalizeAtoms req, normalizeAtoms def)

normalizeAtoms :: [IRAtom] -> [IRAtom]
normalizeAtoms = concatMap (filter (not . isConstAtom) . atomLeaves)

atomLeaves :: IRAtom -> [IRAtom]
atomLeaves (Phi pairs) = concatMap (atomLeaves . snd) pairs
atomLeaves atom = [atom]



instrsRD :: [IRInstr] -> (HashSet IRAtom, HashSet IRAtom)
instrsRD = foldl' step (HashSet.empty, HashSet.empty)
    where
        step :: (HashSet IRAtom, HashSet IRAtom) -> IRInstr -> (HashSet IRAtom, HashSet IRAtom)
        step (reqAcc, defAcc) instr = case instrRD instr of
            Nothing -> (reqAcc, defAcc)
            Just (reqs, defs) ->
                let reqSet = HashSet.fromList reqs
                    defSet = HashSet.fromList defs
                    reqAcc' = HashSet.union reqAcc (HashSet.difference reqSet defAcc)
                    defAcc' = HashSet.union defAcc defSet
                in (reqAcc', defAcc')


-- | Per-block (use, def), keyed by block label.
blockRD :: [IRBlock] -> Map Int (HashSet IRAtom, HashSet IRAtom)
blockRD = Map.fromList . map blockEntry
    where
        blockEntry :: IRBlock -> (Int, (HashSet IRAtom, HashSet IRAtom))
        blockEntry (TAC.IRBlock (bid, instrs)) = (bid, instrsRD instrs)


-- | Block-level liveness fixed-point:
--   out[b] = U in[s], s in succ[b]
--   in[b]  = use[b] U (out[b] - def[b])
--   Returns (inMap, outMap), keyed by block label.
livenessBlocks :: [IRBlock] -> (Map Int (HashSet IRAtom), Map Int (HashSet IRAtom))
livenessBlocks blocks = go initIn initOut
    where
        succMap :: Map Int (HashSet Int)
        succMap = blockSucc blocks

        rdMap :: Map Int (HashSet IRAtom, HashSet IRAtom)
        rdMap = blockRD blocks

        blockIds :: [Int]
        blockIds = reverse [bid | TAC.IRBlock (bid, _) <- blocks]

        initIn :: Map Int (HashSet IRAtom)
        initIn = Map.fromList [(bid, HashSet.empty) | bid <- blockIds]

        initOut :: Map Int (HashSet IRAtom)
        initOut = Map.fromList [(bid, HashSet.empty) | bid <- blockIds]

        go :: Map Int (HashSet IRAtom) -> Map Int (HashSet IRAtom) -> (Map Int (HashSet IRAtom), Map Int (HashSet IRAtom))
        go inMap outMap =
            let outMap' = Map.fromList [(bid, outOf bid inMap) | bid <- blockIds]
                inMap' = Map.fromList [(bid, inOf bid outMap') | bid <- blockIds]
            in if inMap' == inMap && outMap' == outMap
                then (inMap', outMap')
                else go inMap' outMap'

        outOf :: Int -> Map Int (HashSet IRAtom) -> HashSet IRAtom
        outOf bid inMap =
            let succs = Map.findWithDefault HashSet.empty bid succMap
            in HashSet.foldl'
                (\acc sid -> HashSet.union acc (Map.findWithDefault HashSet.empty sid inMap))
                HashSet.empty
                succs

        inOf :: Int -> Map Int (HashSet IRAtom) -> HashSet IRAtom
        inOf bid outMap =
            let (useSet, defSet) = Map.findWithDefault (HashSet.empty, HashSet.empty) bid rdMap
                outSet = Map.findWithDefault HashSet.empty bid outMap
            in HashSet.union useSet (HashSet.difference outSet defSet)


-- | Per-instruction liveness for each block, in source order.
--   Each tuple is (liveIn, liveOut) for that instruction.
livenessInstrs :: [IRBlock] -> Map Int [(HashSet IRAtom, HashSet IRAtom)]
livenessInstrs blocks = Map.fromList (map oneBlock blocks)
    where
        (_, outMap) = livenessBlocks blocks

        oneBlock :: IRBlock -> (Int, [(HashSet IRAtom, HashSet IRAtom)])
        oneBlock (TAC.IRBlock (bid, instrs)) =
            let out0 = Map.findWithDefault HashSet.empty bid outMap
            in (bid, instrsLivenessFromOut out0 instrs)


-- | Per-instruction liveness with instruction payload.
livenessInstrsWithCode :: [IRBlock] -> Map Int [(IRInstr, HashSet IRAtom, HashSet IRAtom)]
livenessInstrsWithCode blocks = Map.fromList (map oneBlock blocks)
    where
        inOutMap = livenessInstrs blocks

        oneBlock :: IRBlock -> (Int, [(IRInstr, HashSet IRAtom, HashSet IRAtom)])
        oneBlock (TAC.IRBlock (bid, instrs)) =
            let ios = Map.findWithDefault [] bid inOutMap
            in (bid, zipWith (\ins (insIn, insOut) -> (ins, insIn, insOut)) instrs ios)


-- | Compute instruction liveness inside one block.
--   Input out set is the block-level liveOut at block end.
instrsLivenessFromOut :: HashSet IRAtom -> [IRInstr] -> [(HashSet IRAtom, HashSet IRAtom)]
instrsLivenessFromOut outAtEnd instrs =
    snd $ foldl' step (outAtEnd, []) (reverse instrs)
    where
        step ::
            (HashSet IRAtom, [(HashSet IRAtom, HashSet IRAtom)]) ->
            IRInstr ->
            (HashSet IRAtom, [(HashSet IRAtom, HashSet IRAtom)])
        step (outSet, acc) instr =
            let (useSet, defSet) = instrRDSet instr
                inSet = HashSet.union useSet (HashSet.difference outSet defSet)
            in (inSet, (inSet, outSet) : acc)

        instrRDSet :: IRInstr -> (HashSet IRAtom, HashSet IRAtom)
        instrRDSet instr = case instrRD instr of
            Nothing -> (HashSet.empty, HashSet.empty)
            Just (reqs, defs) -> (HashSet.fromList reqs, HashSet.fromList defs)
