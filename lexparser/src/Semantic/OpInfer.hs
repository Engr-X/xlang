{-# LANGUAGE TupleSections #-}

module Semantic.OpInfer where

import Control.Applicative (liftA3)
import Data.Map.Strict (Map)
import Parse.SyntaxTree (Class(..), Operator(..))

import qualified Data.Map.Strict as Map


-- | Conversion rank for basic types (smaller -> narrower).
--   Used by promotion to choose the wider of two operands.
--   'Void' is included as a sentinel with rank -1.
basicTypeRank :: Map Class Int
basicTypeRank = Map.fromList [
    (Bool, 0), (Char, 1), (Int8T, 2), (Int16T, 3), (Int32T, 4),
    (Int64T, 5), (Float32T, 6), (Float64T, 7), (Float128T, 8),(Void, -1)]


-- | Reverse lookup for basic type ranks.
--   Converts a rank back into a 'Class' after promotion.
--   Assumes ranks in 'basicTypeRank' are unique.
basicTypeRankRev :: Map Int Class
basicTypeRankRev = Map.fromList $ map (\(k, v) -> (v, k)) (Map.toList basicTypeRank)


-- | Check whether a class is a basic type tracked in 'basicTypeRank'.
--   This excludes arrays and user-defined classes.
--   Useful for guarding numeric-only rules.
isBasicType :: Class -> Bool
isBasicType c = Map.member c basicTypeRank


-- | Promote two basic types to a common result type (at least Int32).
--   Picks the max rank of the two operands and enforces minimum Int32.
--   Errors if either operand is not in 'basicTypeRank'.
promoteBasicType :: Class -> Class -> Class
promoteBasicType a b =
    let ra = Map.findWithDefault (-2) a basicTypeRank
        rb = Map.findWithDefault (-2) b basicTypeRank
        rInt = Map.findWithDefault (-2) Int32T basicTypeRank
        r = max rInt (max ra rb)
    in Map.findWithDefault (error "cannot find the type") r basicTypeRankRev


-- | Inference table for binary operators over basic types.
--   Built by merging the per-operator maps defined below.
--   Assignment operators are handled elsewhere.
binOpInfer :: Map (Operator, Class, Class) Class
binOpInfer = foldr Map.union Map.empty [loadCompare, loadShift, loadBitOp, arithmeticOp, modOp]
    where
        types :: [Class]
        types = [Bool, Char, Int8T, Int16T, Int32T, Int64T, Float32T, Float64T, Float128T]

        narrowIntegerTypes :: [Class]
        narrowIntegerTypes = [Char, Int8T, Int16T, Int32T, Int64T]

        generalIntegerTypes :: [Class]
        generalIntegerTypes = [Bool, Char, Int8T, Int16T, Int32T, Int64T]

        -- | Build rules for ==, !=, >, <, >=, <=.
        --   Any pair of 'types' is permitted.
        --   The result is always Bool.
        loadCompare :: Map (Operator, Class, Class) Class
        loadCompare =
            let op = [Equal, NotEqual, GreaterThan, LessThan, GreaterEqual, LessEqual]
                res = liftA3 (,,) op types types
            in Map.fromList $ map (, Bool) res

        -- | Build rules for <<, >>.
        --   Operates over general integer types only.
        --   Result type follows 'promoteBasicType'.
        loadShift :: Map (Operator, Class, Class) Class
        loadShift =
            let op = [BitRShift, BitLShift]
                res = liftA3 (,,) op generalIntegerTypes generalIntegerTypes
            in Map.fromList $ map (\x@(_, a, b) -> (x, promoteBasicType a b)) res

        -- | Build rules for |, ^, !^, &.
        --   Integer operands promote to a common integer result.
        --   Bool/Bool is explicitly mapped to Bool.
        loadBitOp :: Map (Operator, Class, Class) Class
        loadBitOp =
            let op = [BitOr, BitXor, BitXnor, BitAnd]
                res = liftA3 (,,) op narrowIntegerTypes narrowIntegerTypes
            in Map.fromList $ map (\x@(_, a, b) -> (x, promoteBasicType a b)) res ++ 
                map (\x -> ((x, Bool, Bool), Bool)) op

        -- | Build rules for +, -, *, **, /.
        --   + - * / use 'promoteBasicType' across all 'types'.
        --   Pow returns Float64T, except Float128T ** Float128T.
        arithmeticOp :: Map (Operator, Class, Class) Class
        arithmeticOp =
            let types2 = [Bool, Char, Int8T, Int16T, Int32T, Int64T, Float32T, Float64T]
                op = [Add, Sub, Mul, Div]
                res1 = liftA3 (,,) op types types
                res2 = liftA3 (,,) [Pow] types2 types2
            in Map.fromList $ concat [
                map (\x@(_, a, b) -> (x, promoteBasicType a b)) res1,
                map (, Float64T) res2,
                pure ((Pow, Float128T, Float128T), Float128T)]

        -- | Build rules for %.
        --   Restricted to general integer types.
        --   Result type follows 'promoteBasicType'.
        modOp :: Map (Operator, Class, Class) Class
        modOp = let res = liftA3 (,,) [Mod] generalIntegerTypes generalIntegerTypes
            in Map.fromList $ map (\x@(_, a, b) -> (x, promoteBasicType a b)) res
