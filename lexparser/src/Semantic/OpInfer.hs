{-# LANGUAGE TupleSections #-}

module Semantic.OpInfer where

import Control.Applicative (liftA2, liftA3)
import Data.List (elemIndex, intercalate)
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import Parse.SyntaxTree (Class(..), Operator(..), prettyClass)
import Semantic.TypeEnv (FunSig(..))
import Util.Exception (ErrorKind, Warning(..))
import Util.Type (Path, Position)

import qualified Data.Map.Strict as Map
import qualified Util.Exception as UE


-- | Conversion rank for basic types (smaller -> narrower).
--   Used by promotion to choose the wider of two operands.
--   'Void' is included as a sentinel with rank -1.
basicTypeRank :: Map Class Int
basicTypeRank = Map.fromList [
    (Bool, 0), (Char, 1), (Int8T, 2), (Int16T, 3), (Int32T, 4),
    (Int64T, 5), (Float32T, 6), (Float64T, 7), (Float128T, 8),(Void, -1)]


-- | Project-defined numeric range rank (smaller -> narrower).
--   Note: this ordering treats Float32 as narrower than Int64.
numericRangeRank :: Map Class Int
numericRangeRank = Map.fromList [
    (Bool, 0),
    (Char, 1), (Int8T, 1),
    (Int16T, 2),
    (Int32T, 3),
    (Float32T, 4),
    (Int64T, 5),
    (Float64T, 6),
    (Float128T, 7),
    (Void, -1)]


widenedClass :: Class -> [Class]
widenedClass Bool = [Bool]
widenedClass Char = [Char, Int8T, Int16T, Int32T, Float32T, Int64T, Float64T, Float128T]
widenedClass Int8T = [Int8T, Int16T, Int32T, Float32T, Int64T, Float64T, Float128T]
widenedClass Int16T = [Int16T, Int32T, Float32T, Int64T, Float64T, Float128T]
widenedClass Int32T = [Int32T, Float32T, Int64T, Float64T, Float128T]
widenedClass Int64T = [Int64T, Float64T, Float128T]
widenedClass Float32T = [Float32T, Float64T, Float128T]
widenedClass Float64T = [Float64T, Float128T]
widenedClass Float128T = [Float128T]
widenedClass Void = [Void]
widenedClass (Array c n)
    | isBasicType c = [Array c n]
    | otherwise = map (`Array` n) (widenedClass c)
widenedClass (Class _ _) = error "TODO: class widen is not supported yet"
widenedClass ErrorClass = error "ErrorClass cannot be widened"


widenedArgs :: Path -> [Position] -> [(Class, [Position])] -> [FunSig] -> Either ErrorKind (FunSig, [Warning])
widenedArgs path callPos argInfos sigs =
    let (argTs, argPos) = unzip argInfos
        dist argT paramT = elemIndex paramT (widenedClass argT)

        buildCandidate sig =
            let ps = funParams sig
            in if length ps /= length argTs then Nothing
               else case traverse (uncurry dist) (zip argTs ps) of
                    Nothing -> Nothing
                    Just dists ->
                        let warns = concat [mkWarnings a p pos
                                           | ((a, p), pos, d) <- zip3 (zip argTs ps) argPos dists,
                                           d > 0]
                        in Just (sig, dists, warns)

        candidates = mapMaybe buildCandidate sigs

        dominates (_, d1, _) (_, d2, _) =
            and (zipWith (<=) d1 d2) && or (zipWith (<) d1 d2)

        best = [c | c <- candidates, not (any (`dominates` c) candidates)]

        expectedS = intercalate " | " [
                concat ["(", intercalate ", " (map prettyClass (funParams s)), ")"]
                | s <- sigs]
        actualS = concat ["(", intercalate ", " (map prettyClass argTs), ")"]

    in case candidates of
        [] -> Left $ UE.Syntax $ UE.makeError path callPos (UE.typeMismatchMsg expectedS actualS)
        _ -> case best of
            [(sig, _, warns)] -> Right (sig, warns)
            _ -> Left $ UE.Syntax $ UE.makeError path callPos UE.ambiguousCallMsg
    where
        -- | Build implicit-cast related warnings with source positions.
        mkWarnings :: Class -> Class -> [Position] -> [Warning]
        mkWarnings fromT toT pos = iCast path pos fromT toT


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


-- | Check whether a class is an integer-like type (excluding float).
isIntegerType :: Class -> Bool
isIntegerType c = c `elem` [Bool, Char, Int8T, Int16T, Int32T, Int64T]


-- | Check whether a class is a floating-point type.
isFloatType :: Class -> Bool
isFloatType c = c `elem` [Float32T, Float64T, Float128T]


-- | Build implicit-cast related warnings for a type conversion.
--   If types are equal, returns [].
--   If types differ, returns an ImplicitCast warning.
--   If narrowing (e.g. int -> short), also returns an OverflowWarning.
iCast :: Path -> [Position] -> Class -> Class -> [Warning]
iCast p pos fromT toT
    | fromT == toT = []
    | otherwise = implicitW : overflowW
    where
        fromS = prettyClass fromT
        toS = prettyClass toT
        implicitW = ImplicitCast (UE.makeError p pos (UE.implicitCastMsg fromS toS))
        overflowW = case (Map.lookup fromT numericRangeRank, Map.lookup toT numericRangeRank) of
            (Just rf, Just rt) | rf > rt ->
                [OverflowWarning (UE.makeError p pos (UE.overflowCastMsg fromS toS))]
            _ -> []


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


unaryOpInfer :: Map (Operator, Class) Class
unaryOpInfer = foldr Map.union Map.empty [unaryArithmetic, unaryIncDec, unaryBitNot]
    where
        types :: [Class]
        types = [Bool, Char, Int8T, Int16T, Int32T, Int64T, Float32T, Float64T, Float128T]

        unaryArithmetic :: Map (Operator, Class) Class
        unaryArithmetic =
            let op = [UnaryPlus, UnaryMinus]
                res = liftA2 (,) op types
            in Map.fromList $ map (\x@(_, t) -> (x, incType t)) res

        unaryIncDec :: Map (Operator, Class) Class
        unaryIncDec =
            let op = [IncSelf, DecSelf, SelfInc, SelfDec]
                res = liftA2 (,) op types
            in Map.fromList $ map (\x@(_, t) -> (x, t)) res

        unaryBitNot :: Map (Operator, Class) Class
        unaryBitNot = Map.fromList $ map (\t -> ((BitNot, t), if t == Bool then Bool else incType t)) types

        incType :: Class -> Class
        incType cls =
            case (Map.lookup cls basicTypeRank, Map.lookup Int32T basicTypeRank) of
                (Just rCls, Just rInt) | rCls < rInt -> Int32T
                _ -> cls


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
