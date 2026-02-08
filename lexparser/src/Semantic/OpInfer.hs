module Semantic.OpInfer where

import Control.Applicative (liftA3)
import Data.Map.Strict (Map)
import Parse.SyntaxTree (Class(..), Operator(..))

import qualified Data.Map.Strict as Map


-- | Conversion rank for basic types (smaller -> narrower).
basicTypeRank :: Map Class Int
basicTypeRank = Map.fromList [
    (Bool, 0), (Char, 1), (Int8T, 2), (Int16T, 3), (Int32T, 4),
    (Int64T, 5), (Float32T, 6), (Float64T, 7), (Float128T, 8),(Void, -1)]

-- | Reverse lookup for basic type ranks.
basicTypeRankRev :: Map Int Class
basicTypeRankRev = Map.fromList $ map (\(k, v) -> (v, k)) (Map.toList basicTypeRank)


isBasicType :: Class -> Bool
isBasicType c = Map.member c basicTypeRank


binOpInfer :: Map (Op, Class, Class) Class
binOpInfer = Map.empty
    where
        loadCompare :: Map (Op, Class, Class) Class
        loadCompare = 
            let types = [Char, Int8T, Int16T, Int32T, Int64T, Float32T, Float64T, Float128T]
                op = [Equal, NotEqual, GreaterThan, LessThan, GreaterEqual, LessEqual]
                res = liftA3 (,,) op types types
            in Map.fromList $ map (, Bool) res





