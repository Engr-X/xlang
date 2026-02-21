{-# LANGUAGE HexFloatLiterals #-}


module IR.TACLowing where

import Data.Bits ((.&.))
import Data.Map.Strict (Map)
import Text.Read (readMaybe)
import Data.Maybe (listToMaybe)
import Numeric (readHex)
import Lex.Token (tokenPos)
import Parse.SyntaxTree (Program, Expression)
import Semantic.TypeEnv (FullVarTable, FullFunctionTable)
import Util.Exception (Warning(..))
import Util.Type (Path, Position)

import IR.TAC (IRInstr, IRAtom, TACM(..))

import qualified Parse.SyntaxTree as AST
import qualified IR.TAC as TAC
import qualified Util.Exception as UE


-- | Path placeholder for IR-stage warnings.
tacWarnPath :: Path
tacWarnPath = "<tac>"

-- | Explicit numeric ranges (IEEE 754 for floats).
int32Range :: (Integer, Integer)
int32Range = (0x80000000,  0x7fffffff)

int64Range :: (Integer, Integer)
int64Range = (0x8000000000000000, 0x7fffffffffffffff)

float32Range :: (Double, Double)
float32Range = (0x0.000002P-126, 0x1.fffffeP+127)

float64Range :: (Double, Double)
float64Range = (0x0.0000000000001P-1022, 0x1.fffffffffffffP+1023)

float128Range :: (Rational, Rational)
float128Range = (0x0.0000000000000000000000000001P-16382, 0x1.ffffffffffffffffffffffffffffP+16383)


-- | Strip integer literal suffixes (only 'L' / 'l').
stripIntSuffix :: String -> String
stripIntSuffix s = case reverse s of
    (c:cs) | c `elem` "lL" -> reverse cs
    _ -> s

-- | Strip floating literal suffixes ('F'/'f' or 'L'/'l').
stripFloatSuffix :: String -> String
stripFloatSuffix s = case reverse s of
    (c:cs) | c `elem` "fFlL" -> reverse cs
    _ -> s

-- | Read integer literals supporting hex (0x/0X) and optional suffix.
readIntegerLiteral :: String -> Maybe Integer
readIntegerLiteral raw =
    let s = stripIntSuffix raw
    in case s of
        ('0':'x':rest) -> readHexExact rest
        ('0':'X':rest) -> readHexExact rest
        _ -> readMaybe s
    where
        readHexExact :: String -> Maybe Integer
        readHexExact xs = do
            (n, tailS) <- listToMaybe (readHex xs)
            if null tailS then Just n else Nothing


-- | Wrap an integer into a target range; prefers bitmask for two's-complement.
wrapInt :: (Integer, Integer) -> Integer -> Integer
wrapInt (minI, maxI) n =
    let range = maxI - minI + 1
        isPow2 x = x > 0 && (x .&. (x - 1)) == 0
    in if isPow2 range && minI == negate (range `div` 2) && maxI == (range `div` 2) - 1
        then
            let mask = range - 1
                u = n .&. mask
            in if u > maxI then u - range else u
        else ((n - minI) `mod` range) + minI


-- | Safe read for integer literals with overflow warning and wrap-around.
safeInteger :: Position -> (Integer, Integer) -> String -> TACM Integer
safeInteger pos (minI, maxI) raw = do
    let range = maxI - minI + 1
        wrap = wrapInt (minI, maxI)
        warn msg = TAC.addWarn $ OverflowWarning (UE.makeError tacWarnPath [pos] msg)
    case readIntegerLiteral raw of
        Nothing -> error "this string must be valid any how"
        Just n ->
            if n < minI || n > maxI
                then do
                    warn $ UE.overflowCastMsg "number" "int"
                    pure (wrap n)
                else pure n


-- | Safe read for rational literals; out of range -> clamp to min/max.
safeRational :: Position -> (Rational, Rational) -> String -> TACM Rational
safeRational pos (minR, maxR) raw = do
    let warn msg = TAC.addWarn $ OverflowWarning (UE.makeError tacWarnPath [pos] msg)
    case readMaybe (stripFloatSuffix raw) :: Maybe Double of
        Nothing -> error "this string must be valid any how"
        Just n
            | isNaN n -> do
                warn $ UE.overflowCastMsg "number" "float128"
                pure 0
            | isInfinite n -> do
                warn $ UE.overflowCastMsg "number" "float128"
                pure (if n > 0 then maxR else minR)
            | otherwise ->
                let r = toRational n
                in if r > maxR
                    then warn (UE.overflowCastMsg "number" "float128") >> pure maxR
                    else if r < minR
                        then warn (UE.overflowCastMsg "number" "float128") >> pure minR
                        else pure r


-- | Safe read for floating literals; out of range -> +/-Infinity.
safeDouble :: Position -> (Double, Double) -> String -> TACM Double
safeDouble pos (minD, maxD) raw = do
    let warn msg = TAC.addWarn $ OverflowWarning (UE.makeError tacWarnPath [pos] msg)
    case readMaybe (stripFloatSuffix raw) :: Maybe Double of
        Nothing -> error "this string must be valid any how"
        Just n
            | n > maxD -> do
                warn $ UE.overflowCastMsg "number" "double"
                pure (1.0 / 0.0)
            | n < minD -> do
                warn $ UE.overflowCastMsg "number" "double"
                pure $ -(1.0 / 0.0)
            | otherwise -> pure n


-- | Lower a single expression into a TAC atom.
--   Use lookupVarUse / lookupFunUse when you need resolved ids/types.
exprLowing :: Expression -> TACM IRAtom
exprLowing (AST.Error _ _) = error "this error is catching in semantic already"
exprLowing (AST.IntConst value tok) = do
    v <- safeInteger (tokenPos tok) int32Range value
    pure $ TAC.Int32C (fromInteger v)
exprLowing (AST.LongConst value tok) = do
    v <- safeInteger (tokenPos tok) int64Range value
    pure $ TAC.Int64C (fromInteger v)
exprLowing (AST.FloatConst value tok) = do
    v <- safeDouble (tokenPos tok) float32Range value
    pure $ TAC.Float32C v
exprLowing (AST.DoubleConst value tok) = do
    v <- safeDouble (tokenPos tok) float64Range value
    pure $ TAC.Float64C v
exprLowing (AST.LongDoubleConst value tok) = do
    v <- safeRational (tokenPos tok) float128Range value
    pure $ TAC.Float128C v
    
exprLowing (AST.CharConst value _) = pure $ TAC.CharC value
exprLowing (AST.BoolConst value _) = pure $ TAC.BoolC value
exprLowing (AST.StringConst _ _) = error "string literal is not supported in TAC"

--exprLowing


tacGen :: Path -> Program -> Map Position FullVarTable -> Map Position FullFunctionTable -> IRInstr
tacGen = error "TODO"
