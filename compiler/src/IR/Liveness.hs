module IR.Liveness where

import Data.Map.Strict (Map)
import Data.HashSet (HashSet)
import Data.Maybe (mapMaybe)
import IR.TAC (IRInstr(..), IRBlock, getInstrSucc)

import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import qualified IR.TAC as TAC



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
            HashSet.fromList (mapMaybe getInstrSucc instrs)

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
        isTerminator IReturn = True
        isTerminator Return = True
        isTerminator _ = False
