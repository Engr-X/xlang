{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Replace case with maybe" -}
module X64Lowing.Lowing where

import Control.Monad.State.Strict (State, evalState, get, gets, modify', put)
import Data.Aeson (Value, encode, object, (.=))
import Data.Bits ((.|.), shiftL)
import Data.Char (ord)
import Data.List (foldl', intercalate)
import Data.Maybe (mapMaybe)
import Data.Map.Strict (Map)
import Numeric (showHex)
import IR.TAC (IRAtom, IRFunction, IRInstr)
import Parse.SyntaxTree (Class)
import X64Lowing.ASM (
    CallConv64(..),
    Register,
    SignExt64(..),
    asByteDst64,
    asDwordDst64,
    bitsByClass64,    castIntWidth64,
    classBits64,
    intAssignBits64,
    intRet32Classes64,
    intRet64Classes64,
    isCastIntClass64,
    isRefClass64,
    sizeByClass64,
    staticInitFlagName,
    staticInitName)

import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified IR.TAC as IR
import qualified Parse.ParserBasic as Parse
import qualified Parse.SyntaxTree as AST
import qualified Semantic.TypeEnv as TEnv
import qualified Util.Basic as Basic
import qualified X64Lowing.ASM as X64


genIntCast64 ::
    X64.Atom ->
    Class ->
    Class ->
    X64.Atom ->
    X64LowerM [X64.Instruction]
genIntCast64 dstAtom fromC toC srcAtom = do
    tmp <- getTmpIRegM64
    let t64 = X64.Reg tmp X64.B64
        t32 = X64.Reg tmp X64.B32
        t16 = X64.Reg tmp X64.B16
        t8 = X64.Reg tmp X64.B8L

        loadIns = if fromC == AST.Int64T
            then X64.Mov t64 srcAtom X64.B64
            else X64.Mov t32 srcAtom X64.B32

        widen = castIntWidth64 toC > castIntWidth64 fromC

        widenIns
            | not widen = []
            | fromC == AST.Char = [X64.Movz t32 t8 X64.B32]
            | fromC == AST.Int8T = [X64.Movs t32 t8 X64.B32]
            | fromC == AST.Int16T = [X64.Movs t32 t16 X64.B32]
            | otherwise = []

        outBits = if toC == AST.Int64T then X64.B64 else classBits64 toC
        outSrc = case outBits of
            X64.B64 -> t64
            X64.B32 -> t32
            X64.B16 -> t16
            X64.B8L -> t8
            X64.B8H -> t8
            X64.NN -> error "genIntCast64: NN output bits is invalid"
    return $ [loadIns] ++ widenIns ++ [X64.Mov dstAtom outSrc outBits]


genIntToBoolCast64 ::
    X64.Atom ->
    Class ->
    X64.Atom ->
    X64LowerM [X64.Instruction]
genIntToBoolCast64 dstAtom fromC srcAtom = do
    tmp <- getTmpIRegM64
    let srcBits = classBits64 fromC
        srcReg = X64.Reg tmp srcBits
        dstByte = asByteDst64 dstAtom
    return [
        X64.Mov srcReg srcAtom srcBits,
        X64.Test srcReg srcReg srcBits,
        X64.Setne dstByte X64.B8L]


genBoolToIntCast64 ::
    X64.Atom ->
    Class ->
    X64.Atom ->
    X64LowerM [X64.Instruction]
genBoolToIntCast64 dstAtom toC srcAtom = do
    iRet <- getIRetRegM64
    let src8 = X64.Reg iRet X64.B8L
        dst32 = X64.Reg iRet X64.B32
        dst64 = X64.Reg iRet X64.B64
        pref = [X64.Mov src8 srcAtom X64.B8L, X64.Movz dst32 src8 X64.B32]
    case toC of
        AST.Int64T -> case dstAtom of
            X64.Reg r _ -> return $ pref ++ [X64.Mov (X64.Reg r X64.B32) dst32 X64.B32]
            _ -> return $ pref ++ [X64.Mov dstAtom dst64 X64.B64]
        _ | isCastIntClass64 toC -> return $ pref ++ [X64.Mov (asDwordDst64 dstAtom) dst32 X64.B32]
        _ -> error $ "genBoolToIntCast64: unsupported target class: " ++ show toC


genBoolToFloatCast64 ::
    X64.Atom ->
    Class ->
    X64.Atom ->
    X64LowerM [X64.Instruction]
genBoolToFloatCast64 dstAtom toC srcAtom = do
    iRet <- getIRetRegM64
    fTmp <- getTmpFRegM64
    let src8 = X64.Reg iRet X64.B8L
        src32 = X64.Reg iRet X64.B32
        xDst = X64.Reg fTmp X64.NN
        pref = [X64.Mov src8 srcAtom X64.B8L, X64.Movz src32 src8 X64.B32]
    case toC of
        AST.Float32T ->
            return $ pref ++ [
                X64.Xorps xDst xDst X64.B32,
                X64.Cvtsi2ss xDst src32 X64.B32,
                X64.Movss dstAtom xDst X64.B32]
        AST.Float64T ->
            return $ pref ++ [
                X64.Xorpd xDst xDst X64.B64,
                X64.Cvtsi2sd xDst src32 X64.B64,
                X64.Movsd dstAtom xDst X64.B64]
        _ ->
            error $ "genBoolToFloatCast64: unsupported target class: " ++ show toC


genFloatToBoolCast64 ::
    X64.Atom ->
    Class ->
    X64.Atom ->
    X64LowerM [X64.Instruction]
genFloatToBoolCast64 dstAtom fromC srcAtom = do
    tmpF <- getTmpFRegM64
    fRet <- getFRetRegM64
    tmpI <- getTmpIRegM64
    iRet <- getIRetRegM64
    let xSrc = X64.Reg tmpF X64.NN
        xZero = X64.Reg fRet X64.NN
        one32 = X64.Reg tmpI X64.B32
        out32 = X64.Reg iRet X64.B32
        out8 = X64.Reg iRet X64.B8L
        dst8 = asByteDst64 dstAtom

        (loadF, clearF, cmpF) = case fromC of
            AST.Float32T -> (X64.Movss xSrc srcAtom X64.B32, X64.Xorps xZero xZero X64.B32, X64.Ucomiss xSrc xZero X64.B32)
            AST.Float64T -> (X64.Movsd xSrc srcAtom X64.B64, X64.Xorpd xZero xZero X64.B64, X64.Ucomisd xSrc xZero X64.B64)
            _ -> error $ "genFloatToBoolCast64: unsupported source class: " ++ show fromC
    return [
        loadF,
        clearF,
        X64.Mov out32 (X64.Imm 0) X64.B32,
        X64.Mov one32 (X64.Imm 1) X64.B32,
        cmpF,
        X64.Setp out8 X64.B8L,
        X64.Cmovne out32 one32 X64.B32,
        X64.Mov dst8 out8 X64.B8L]


genFloatToIntCast64 ::
    X64.Atom ->
    Class ->
    Class ->
    X64.Atom ->
    X64LowerM [X64.Instruction]
genFloatToIntCast64 dstAtom fromC toC srcAtom = do
    tmp <- getTmpIRegM64
    let dst32 = X64.Reg tmp X64.B32
        dst64 = X64.Reg tmp X64.B64
    case (fromC, toC) of
        (AST.Float32T, AST.Int64T) ->
            return [X64.Cvttss2si dst64 srcAtom X64.B64, X64.Mov dstAtom dst64 X64.B64]
        (AST.Float64T, AST.Int64T) ->
            return [X64.Cvttsd2si dst64 srcAtom X64.B64, X64.Mov dstAtom dst64 X64.B64]
        (AST.Float32T, toInt)
            | isCastIntClass64 toInt ->
                return [X64.Cvttss2si dst32 srcAtom X64.B32, X64.Mov (asDwordDst64 dstAtom) dst32 X64.B32]
        (AST.Float64T, toInt)
            | isCastIntClass64 toInt ->
                return [X64.Cvttsd2si dst32 srcAtom X64.B64, X64.Mov (asDwordDst64 dstAtom) dst32 X64.B32]
        _ ->
            error $ "genFloatToIntCast64: unsupported cast: " ++ show (fromC, toC)


genFloatToFloatCast64 ::
    X64.Atom ->
    Class ->
    Class ->
    X64.Atom ->
    X64LowerM [X64.Instruction]
genFloatToFloatCast64 dstAtom fromC toC srcAtom = do
    tmpF <- getTmpFRegM64
    let fTmp = X64.Reg tmpF X64.NN
    case (fromC, toC) of
        (AST.Float32T, AST.Float64T) ->
            return [X64.Cvtss2sd fTmp srcAtom X64.B32, X64.Movsd dstAtom fTmp X64.B64]
        (AST.Float64T, AST.Float32T) ->
            return [X64.Cvtsd2ss fTmp srcAtom X64.B64, X64.Movss dstAtom fTmp X64.B32]
        _ ->
            error $ "genFloatToFloatCast64: unsupported cast: " ++ show (fromC, toC)


genIntToFloatCast64 ::
    X64.Atom ->
    Class ->
    Class ->
    X64.Atom ->
    X64LowerM [X64.Instruction]
genIntToFloatCast64 dstAtom fromC toC srcAtom = do
    tmpI <- getTmpIRegM64
    tmpF <- getTmpFRegM64
    let i64 = X64.Reg tmpI X64.B64
        i32 = X64.Reg tmpI X64.B32
        i16 = X64.Reg tmpI X64.B16
        i8 = X64.Reg tmpI X64.B8L
        fTmp = X64.Reg tmpF X64.NN

        (prepInt, intSrc, intBits) = case fromC of
            AST.Int64T -> ([X64.Mov i64 srcAtom X64.B64], i64, X64.B64)
            AST.Int32T -> ([X64.Mov i32 srcAtom X64.B32], i32, X64.B32)
            AST.Int16T -> ([X64.Mov i16 srcAtom X64.B16, X64.Movs i32 i16 X64.B32], i32, X64.B32)
            AST.Int8T -> ([X64.Mov i8 srcAtom X64.B8L, X64.Movs i32 i8 X64.B32], i32, X64.B32)
            AST.Char -> ([X64.Mov i8 srcAtom X64.B8L, X64.Movz i32 i8 X64.B32], i32, X64.B32)
            AST.Bool -> ([X64.Mov i8 srcAtom X64.B8L, X64.Movz i32 i8 X64.B32], i32, X64.B32)
            _ -> error $ "genIntToFloatCast64: unsupported source class: " ++ show fromC
    case toC of
        AST.Float32T ->
            return $ prepInt ++ [
                X64.Xorps fTmp fTmp X64.B32,
                X64.Cvtsi2ss fTmp intSrc intBits,
                X64.Movss dstAtom fTmp X64.B32]
        AST.Float64T ->
            return $ prepInt ++ [
                X64.Xorpd fTmp fTmp X64.B64,
                X64.Cvtsi2sd fTmp intSrc intBits,
                X64.Movsd dstAtom fTmp X64.B64]
        _ ->
            error $ "genIntToFloatCast64: unsupported target class: " ++ show toC


mkStaticCallName64 :: [String] -> [Class] -> ([String], [Class])
mkStaticCallName64 qname argTs =
    if null qname
        then error "mkStaticCallName64: empty qname"
        else (qname, argTs)


mkDirectCallName64 :: [String] -> [Class] -> ([String], [Class])
mkDirectCallName64 qname sigTs = case qname of
    [] -> error "mkDirectCallName64: empty qname"
    [one]
        | '.' `elem` one ->
            let segs = splitByDot64 one
            in if null segs
                then error $ "mkDirectCallName64: invalid dotted call target: " ++ show qname
                else (segs, sigTs)
        | otherwise -> (X64.mkRawQName64 one, sigTs)
    _ -> (qname, sigTs)


splitByDot64 :: String -> [String]
splitByDot64 s = filter (not . null) (go s)
    where
        go :: String -> [String]
        go txt = case break (== '.') txt of
            (a, []) -> [a]
            (a, _ : rest) -> a : go rest


staticOwnerQName64 :: [String] -> [String]
staticOwnerQName64 qname =
    if length qname < 2
        then error $ "staticOwnerQName64: expected static field qname with owner and field, got: " ++ show qname
        else init qname


staticClinitName64 :: [String] -> [String]
staticClinitName64 qname = staticOwnerQName64 qname ++ [staticInitName]


staticClinitCall64 :: [String] -> X64.Instruction
staticClinitCall64 qname = X64.Call (staticClinitName64 qname) [AST.Void]


withStaticClinitM64 :: [String] -> [X64.Instruction] -> X64LowerM [X64.Instruction]
withStaticClinitM64 qname body = do
    inClinit <- gets stInClinit64
    curOwner <- gets stOwnerQName64
    let targetOwner = staticOwnerQName64 qname
        isSelfClinit = inClinit && curOwner == targetOwner
    return $ if isSelfClinit
        then body
        else staticClinitCall64 qname : body


staticFieldAtom64 :: [String] -> Class -> X64LowerM X64.Atom
staticFieldAtom64 qname cls = X64.Bss qname [cls] <$> getRipRegM64


data ArgRegSel64
    = ArgRegI Register
    | ArgRegF Register
    deriving (Eq, Show)


isFloatArgClass64 :: Class -> Bool
isFloatArgClass64 cls = cls == AST.Float32T || cls == AST.Float64T


isWinArgLayout64 :: CallConv64 -> Bool
isWinArgLayout64 cc = ccArgs cc == [X64.C, X64.D, X64.R8, X64.R9]


floatArgRegs64 :: CallConv64 -> [Register]
floatArgRegs64 cc
    | isWinArgLayout64 cc = [X64.Xmm0, X64.Xmm1, X64.Xmm2, X64.Xmm3]
    | otherwise = [X64.Xmm0, X64.Xmm1, X64.Xmm2, X64.Xmm3, X64.Xmm4, X64.Xmm5, X64.Xmm6, X64.Xmm7]


assignArgRegs64 :: CallConv64 -> [Class] -> X64LowerM [ArgRegSel64]
assignArgRegs64 cc argClasses
    | isWinArgLayout64 cc =
        if length argClasses > length iRegs
            then error $ "assignArgRegs64: stack args are not supported yet, got " ++ show (length argClasses) ++ " args"
            else return [
                if isFloatArgClass64 cls
                    then ArgRegF (fRegs !! idx)
                    else ArgRegI (iRegs !! idx)
                | (idx, cls) <- zip [0 ..] argClasses
                ]
    | otherwise = go 0 0 argClasses
    where
        iRegs = ccArgs cc
        fRegs = floatArgRegs64 cc

        go :: Int -> Int -> [Class] -> X64LowerM [ArgRegSel64]
        go _ _ [] = return []
        go iIdx fIdx (cls : rest)
            | isFloatArgClass64 cls =
                if fIdx >= length fRegs
                    then error $ "assignArgRegs64: float stack args are not supported yet, got " ++ show (length argClasses) ++ " args"
                    else do
                        tailRegs <- go iIdx (fIdx + 1) rest
                        return (ArgRegF (fRegs !! fIdx) : tailRegs)
            | otherwise =
                if iIdx >= length iRegs
                    then error $ "assignArgRegs64: integer stack args are not supported yet, got " ++ show (length argClasses) ++ " args"
                    else do
                        tailRegs <- go (iIdx + 1) fIdx rest
                        return (ArgRegI (iRegs !! iIdx) : tailRegs)


moveArgToReg64 :: ArgRegSel64 -> X64.Atom -> Class -> X64LowerM [X64.Instruction]
moveArgToReg64 regSel atom cls = case regSel of
    ArgRegF freg -> case cls of
        AST.Float32T -> return [X64.Movss (X64.Reg freg X64.NN) atom X64.B32]
        AST.Float64T -> return [X64.Movsd (X64.Reg freg X64.NN) atom X64.B64]
        _ -> error $ "moveArgToReg64: non-float class to float arg register: " ++ show cls

    ArgRegI ireg -> case cls of
        AST.Float32T -> error "moveArgToReg64: float32 cannot be moved to integer arg register"
        AST.Float64T -> error "moveArgToReg64: float64 cannot be moved to integer arg register"
        _ | isRefClass64 cls ->
            return [X64.Mov (X64.Reg ireg X64.B64) atom X64.B64]
        _ -> case Map.lookup cls bitsByClass64 of
            Just bits -> return [X64.Mov (X64.Reg ireg bits) atom bits]
            Nothing -> error $ "moveArgToReg64: unsupported arg class: " ++ show cls


currentParamTypes64 :: X64LowerM [Class]
currentParamTypes64 = do
    mSig <- gets stCurFunSig64
    case mSig of
        Just sig -> return (TEnv.funParams sig)
        Nothing -> return []


prepareDirectCallParams64 :: X64LowerM [X64.Instruction]
prepareDirectCallParams64 = do
    cc <- getCCM64
    paramTypes <- currentParamTypes64
    regPlan <- assignArgRegs64 cc paramTypes
    pairs <- mapM (\(idx, cls) -> do
        (atom, _, _) <- tySizeM64 (IR.Param idx)
        return (idx, atom, cls))
        (zip [0 ..] paramTypes)
    concat <$> mapM (\(regSel, (_, atom, cls)) -> moveArgToReg64 regSel atom cls)
        (zip regPlan pairs)


-- 64-bit target byte size for a type.
classBytes64 :: Class -> Int
classBytes64 (AST.Class {}) = 8
classBytes64 AST.Void = 0
classBytes64 cls = case Map.lookup cls sizeByClass64 of
    Just n -> n
    Nothing -> error $ "classBytes64: unknown class: " ++ show cls


classAlign64 :: Class -> Int
classAlign64 cls = case classBytes64 cls of
    n | n <= 1 -> 1
    2 -> 2
    4 -> 4
    8 -> 8
    _ -> 16


alignUpN :: Int -> Int -> Int
alignUpN x a
    | a <= 0 = x
    | otherwise = ((x + a - 1) `div` a) * a


-- Map from param index to positive stack offset.
paramOffsets64 :: IRFunction -> Int -> Map Int Int
paramOffsets64 (IR.IRFunction _ _ funSig _ _ _) baseOff =
    let start = max 0 baseOff
        ents = zip [0 ..] (TEnv.funParams funSig)
        (offMap, _) = foldl' step (Map.empty, start) ents
    in offMap
    where
        step :: (Map Int Int, Int) -> (Int, Class) -> (Map Int Int, Int)
        step (acc, curOff) (idx, cls) =
            let sizeN = classBytes64 cls
                nextOff = alignUpN (curOff + sizeN) (classAlign64 cls)
            in (Map.insert idx nextOff acc, nextOff)


-- Map from local atom to positive stack offset.
localOffsets64 :: IRFunction -> Int -> Map IRAtom Int
localOffsets64 (IR.IRFunction _ _ _ atomTypes _ _) baseOff =
    let start = max 0 baseOff
        locals = filter (isStackAtom . fst) (Map.toAscList atomTypes)
        (offMap, _) = foldl' step (Map.empty, start) locals
    in offMap
    where
        isStackAtom :: IRAtom -> Bool
        isStackAtom (IR.Var _) = True
        isStackAtom (IR.Param _) = True
        isStackAtom _ = False

        step :: (Map IRAtom Int, Int) -> (IRAtom, Class) -> (Map IRAtom Int, Int)
        step (acc, curOff) (a, cls) =
            let sizeN = classBytes64 cls
                nextOff = alignUpN (curOff + sizeN) (classAlign64 cls)
            in (Map.insert a nextOff acc, nextOff)


-- Params first, then locals.
-- Returns:
--   1) atom -> positive stack offset
--   2) final/max offset
stackLayout64 :: IRFunction -> Int -> (Map IRAtom Int, Int)
stackLayout64 (IR.IRFunction _ _ funSig atomTypes _ _) baseOff =
    let start = max 0 baseOff
        paramEnts = [(IR.Param i, cls) | (i, cls) <- zip [0 ..] (TEnv.funParams funSig)]
        localEnts = [(a, cls) | (a, cls) <- Map.toAscList atomTypes, isLocal a]
        ents = paramEnts ++ localEnts
        (layout, endOff) = foldl' step (Map.empty, start) ents
    in (layout, endOff)
    where
        isLocal :: IRAtom -> Bool
        isLocal (IR.Var _) = True
        isLocal _ = False

        step :: (Map IRAtom Int, Int) -> (IRAtom, Class) -> (Map IRAtom Int, Int)
        step (acc, curOff) (a, cls) =
            let sizeN = classBytes64 cls
                nextOff = alignUpN (curOff + sizeN) (classAlign64 cls)
            in (Map.insert a nextOff acc, nextOff)


-- Lowering state for one function.
data StackLayoutState = StackLayoutState {
    stOff64 :: Map IRAtom Int, -- atom -> positive stack offset
    stMaxOff64 :: Int,         -- max/final offset
    stCC64 :: CallConv64,      -- current calling convention
    stFloatConstSeq64 :: Int,  -- next LC index for float/double immediate pool
    stFloatConstLabelMap64 :: Map FloatImmKey64 String, -- float/double immediate -> const label
    stTypes64 :: Map IRAtom Class, -- atom -> type
    stOwnerQName64 :: [String], -- current class owner qname
    stCurFunName64 :: Maybe String, -- current lowering function name
    stCurFunSig64 :: Maybe TEnv.FunSig, -- current lowering function signature
    stInClinit64 :: Bool,      -- currently lowering .clinit body
    stRetBid64 :: Maybe Int,   -- return block id (if available)
    stDivWantRem64 :: Bool      -- False: quotient(/), True: remainder(%)
} deriving (Eq, Show)


atomTypes64 :: IRFunction -> Map IRAtom Class
atomTypes64 (IR.IRFunction _ _ funSig atomTypes _ _) =
    let paramTypes = Map.fromList [(IR.Param i, cls) | (i, cls) <- zip [0 ..] (TEnv.funParams funSig)]
    in Map.union atomTypes paramTypes


mkState64 :: IRFunction -> Int -> CallConv64 -> StackLayoutState
mkState64 fun@(IR.IRFunction _ _ funSig _ (_, retBid) _) baseOff cc =
    let (layout, endOff) = stackLayout64 fun baseOff
        tys = atomTypes64 fun
    in StackLayoutState {
        stOff64 = layout,
        stMaxOff64 = endOff,
        stCC64 = cc,
        stFloatConstSeq64 = 0,
        stFloatConstLabelMap64 = Map.empty,
        stTypes64 = tys,
        stOwnerQName64 = [],
        stCurFunName64 = Nothing,
        stCurFunSig64 = Just funSig,
        stInClinit64 = False,
        stRetBid64 = Just retBid,
        stDivWantRem64 = False
    }


stackLayoutClinit64 :: Map IRAtom Class -> Int -> (Map IRAtom Int, Int)
stackLayoutClinit64 atomTypes baseOff =
    let start = max 0 baseOff
        stackEnts = [(a, cls) | (a, cls) <- Map.toAscList atomTypes, isStackAtom a]
        (layout, endOff) = foldl' step (Map.empty, start) stackEnts
    in (layout, endOff)
    where
        isStackAtom :: IRAtom -> Bool
        isStackAtom (IR.Var _) = True
        isStackAtom (IR.Param _) = True
        isStackAtom _ = False

        step :: (Map IRAtom Int, Int) -> (IRAtom, Class) -> (Map IRAtom Int, Int)
        step (acc, curOff) (a, cls) =
            let sizeN = classBytes64 cls
                nextOff = alignUpN (curOff + sizeN) (classAlign64 cls)
            in (Map.insert a nextOff acc, nextOff)


mkClinitState64 :: IR.StaticInit -> Map IRAtom Class -> Int -> CallConv64 -> StackLayoutState
mkClinitState64 (IR.StaticInit (_, retBid)) staticAtomTypes baseOff cc =
    let (layout, endOff) = stackLayoutClinit64 staticAtomTypes baseOff
    in StackLayoutState {
        stOff64 = layout,
        stMaxOff64 = endOff,
        stCC64 = cc,
        stFloatConstSeq64 = 0,
        stFloatConstLabelMap64 = Map.empty,
        stTypes64 = staticAtomTypes,
        stOwnerQName64 = [],
        stCurFunName64 = Just staticInitName,
        stCurFunSig64 = Just (TEnv.FunSig [] AST.Void),
        stInClinit64 = True,
        stRetBid64 = Just retBid,
        stDivWantRem64 = False
    }


-- X64 lowering monad carrying stack/layout state.
type X64LowerM a = State StackLayoutState a


runX64LowerState64 :: IRFunction -> Int -> CallConv64 -> StackLayoutState
runX64LowerState64 = mkState64


withLowerState64 :: StackLayoutState -> X64LowerM a -> X64LowerM a
withLowerState64 st action = do
    saved <- get
    put st
    out <- action
    put saved
    return out


getOffMapM64 :: X64LowerM (Map IRAtom Int)
getOffMapM64 = gets stOff64


getMaxOffM64 :: X64LowerM Int
getMaxOffM64 = gets stMaxOff64


getCCM64 :: X64LowerM CallConv64
getCCM64 = gets stCC64


getSpRegM64 :: X64LowerM Register
getSpRegM64 = gets (ccSp . stCC64)


getBpRegM64 :: X64LowerM Register
getBpRegM64 = gets (ccBp . stCC64)


getTmpIRegM64 :: X64LowerM Register
getTmpIRegM64 = gets (ccTmpI . stCC64)


getTmpFRegM64 :: X64LowerM Register
getTmpFRegM64 = gets (ccTmpF . stCC64)


getRipRegM64 :: X64LowerM Register
getRipRegM64 = gets (ccRip . stCC64)


getIRetRegM64 :: X64LowerM Register
getIRetRegM64 = gets (ccIRet . stCC64)


getFRetRegM64 :: X64LowerM Register
getFRetRegM64 = gets (ccFRet . stCC64)


getAtomOffM64 :: IRAtom -> X64LowerM Int
getAtomOffM64 a = do
    mOff <- gets (Map.lookup a . stOff64)
    case mOff of
        Just off -> return off
        Nothing -> error $ "getAtomOffM64: stack offset not found for atom: " ++ show a


putStateM64 :: StackLayoutState -> X64LowerM ()
putStateM64 = put


putCCM64 :: CallConv64 -> X64LowerM ()
putCCM64 cc = modify' (\s -> s { stCC64 = cc })


setDivWantRemM64 :: Bool -> X64LowerM ()
setDivWantRemM64 flag = modify' (\s -> s { stDivWantRem64 = flag })


getDivWantRemM64 :: X64LowerM Bool
getDivWantRemM64 = gets stDivWantRem64


blockName :: Int -> String
blockName idx = ".L" ++ show idx


data FloatImmKey64
    = Float32ImmKey64 String
    | Float64ImmKey64 String
    deriving (Eq, Ord, Show)


floatConstLabelPrefix64 :: X64.CallConv64 -> String
floatConstLabelPrefix64 cc = case X64.ccCompiler cc of
    X64.NASM -> "LC"
    _ -> ".LC"


floatConstLabel64 :: X64.CallConv64 -> Int -> String
floatConstLabel64 cc idx = floatConstLabelPrefix64 cc ++ showHex idx ""


floatImmKeyFromAtom64 :: IR.IRAtom -> Maybe FloatImmKey64
floatImmKeyFromAtom64 atom = case atom of
    IR.Float32C f -> Just (Float32ImmKey64 (floatHex32Const64 (realToFrac f :: Float)))
    IR.Float64C d -> Just (Float64ImmKey64 (doubleHex64Const64 d))
    _ -> Nothing


atomsInInstr64 :: IR.IRInstr -> [IR.IRAtom]
atomsInInstr64 instr = case instr of
    IR.Jump _ -> []
    IR.Ifeq a b _ -> [a, b]
    IR.Ifne a b _ -> [a, b]
    IR.Iflt a b _ -> [a, b]
    IR.Ifle a b _ -> [a, b]
    IR.Ifgt a b _ -> [a, b]
    IR.Ifge a b _ -> [a, b]
    IR.SetRet atom -> [atom]
    IR.Return -> []
    IR.VReturn -> []
    IR.IAssign dst src -> [dst, src]
    IR.IUnary dst _ x -> [dst, x]
    IR.IBinary dst _ x y -> [dst, x, y]
    IR.ICast dst _ x -> [dst, x]
    IR.ICallStaticDirect dst _ args -> dst : args
    IR.ICallStatic dst _ args -> dst : args
    IR.ICallVirtual dst _ args -> dst : args
    IR.IGetField dst obj _ -> [dst, obj]
    IR.IPutField obj _ v -> [obj, v]
    IR.IGetStatic dst _ -> [dst]
    IR.IPutStatic _ v -> [v]


floatImmKeysFromBlock64 :: IR.IRBlock -> [FloatImmKey64]
floatImmKeysFromBlock64 (IR.IRBlock (_, instrs)) =
    concatMap (mapMaybe floatImmKeyFromAtom64 . atomsInInstr64) instrs


uniqueInOrder64 :: Ord a => [a] -> [a]
uniqueInOrder64 xs = reverse rev
    where
        (rev, _) = foldl' step ([], Set.empty) xs
        step :: Ord a => ([a], Set.Set a) -> a -> ([a], Set.Set a)
        step (acc, seen) one
            | one `Set.member` seen = (acc, seen)
            | otherwise = (one : acc, Set.insert one seen)


floatImmStaticData64 :: FloatImmKey64 -> String -> X64.StaticData
floatImmStaticData64 key label = case key of
    Float32ImmKey64 hex32 -> X64.StaticData label [X64.Long hex32]
    Float64ImmKey64 hex64 -> X64.StaticData label [X64.Quad hex64]


collectClassFloatConsts64 :: X64.CallConv64 -> Int -> IR.IRClass -> (Map FloatImmKey64 String, [X64.StaticData], Int)
collectClassFloatConsts64 cc seqStart (IR.IRClass _ _ _ (IR.StaticInit (sBlocks, _)) _ funs _) =
    let staticKeys = concatMap floatImmKeysFromBlock64 sBlocks
        funKeys = concatMap funFloatKeys funs
        keys = uniqueInOrder64 (staticKeys ++ funKeys)
        pairs = zip keys [seqStart ..]
        keyToLabel = Map.fromList [(k, floatConstLabel64 cc idx) | (k, idx) <- pairs]
        dataSegs = [floatImmStaticData64 k (floatConstLabel64 cc idx) | (k, idx) <- pairs]
        seqNext = seqStart + length keys
    in (keyToLabel, dataSegs, seqNext)
    where
        funFloatKeys :: IR.IRFunction -> [FloatImmKey64]
        funFloatKeys (IR.IRFunction _ _ _ _ (blocks, _) _) = concatMap floatImmKeysFromBlock64 blocks


lookupFloatConstLabel64 :: FloatImmKey64 -> X64LowerM String
lookupFloatConstLabel64 key = do
    mLbl <- gets (Map.lookup key . stFloatConstLabelMap64)
    case mLbl of
        Just lbl -> return lbl
        Nothing -> error $ "lookupFloatConstLabel64: float immediate label not found for key: " ++ show key


floatConstAtom64 :: Class -> FloatImmKey64 -> X64LowerM X64.Atom
floatConstAtom64 cls key = do
    lbl <- lookupFloatConstLabel64 key
    rip <- getRipRegM64
    return (X64.Bss (X64.mkRawQName64 lbl) [cls] rip)


-- Type query in X64 lowering state.
-- Constants return their fixed type directly.
-- Vars/Params are resolved from stTypes64.
atomTypeM64 :: IRAtom -> X64LowerM Class
atomTypeM64 (IR.BoolC _) = return AST.Bool
atomTypeM64 (IR.CharC _) = return AST.Char
atomTypeM64 (IR.StringC _) = return (AST.Class ["String"] [])
atomTypeM64 (IR.Int8C _) = return AST.Int8T
atomTypeM64 (IR.Int16C _) = return AST.Int16T
atomTypeM64 (IR.Int32C _) = return AST.Int32T
atomTypeM64 (IR.Int64C _) = return AST.Int64T
atomTypeM64 (IR.Float32C _) = return AST.Float32T
atomTypeM64 (IR.Float64C _) = return AST.Float64T
atomTypeM64 (IR.Float128C _) = return AST.Float128T
atomTypeM64 atom@(IR.Var _) = lookupTyM64 atom
atomTypeM64 atom@(IR.Param _) = lookupTyM64 atom
atomTypeM64 (IR.Phi _) = error "getAtomTypeM64: phi should be stripped before x64 lowering"


lookupTyM64 :: IRAtom -> X64LowerM Class
lookupTyM64 atom = do
    mTy <- gets (Map.lookup atom . stTypes64)
    case mTy of
        Just ty -> return ty
        Nothing -> error $ "getAtomTypeM64: type not found for atom: " ++ show atom


getByteSize :: IRAtom -> X64LowerM Int
getByteSize atom = (\(_, _, sz) -> sz) <$> tySizeM64 atom


-- Lower one IR atom to an x64 operand.
-- Vars/params are materialized as stack memory operands: [rbp - offset].
atomAddrM64 :: IRAtom -> X64LowerM X64.Atom
atomAddrM64 atom = case atom of
    IR.Var _ -> stackMem
    IR.Param _ -> stackMem
    IR.BoolC b -> return (X64.Imm (if b then 1 else 0))
    IR.CharC c -> return (X64.Imm (fromEnum c))
    IR.Int8C i -> return (X64.Imm (fromIntegral i))
    IR.Int16C i -> return (X64.Imm (fromIntegral i))
    IR.Int32C i -> return (X64.Imm (fromIntegral i))
    IR.Int64C i -> return (X64.Imm (fromIntegral i))
    IR.Float32C f -> floatConstAtom64 AST.Float32T (Float32ImmKey64 (floatHex32Const64 (realToFrac f :: Float)))
    IR.Float64C d -> floatConstAtom64 AST.Float64T (Float64ImmKey64 (doubleHex64Const64 d))
    IR.Float128C _ -> error "atomAddrM64: float128 immediate is not supported yet"
    IR.StringC _ -> error "atomAddrM64: string immediate is not supported yet"
    IR.Phi _ -> error "atomAddrM64: phi should be stripped before x64 lowering"
    where
        stackMem :: X64LowerM X64.Atom
        stackMem = do
            bp <- getBpRegM64
            off <- getAtomOffM64 atom
            return (X64.Mem (Just bp) Nothing (negate off))


tySizeM64 :: IRAtom -> X64LowerM (X64.Atom, Class, Int)
tySizeM64 atom = do
    x64Atom <- atomAddrM64 atom
    cls <- atomTypeM64 atom
    return (x64Atom, cls, classBytes64 cls)


movMemToReg :: X64.Atom -> Class -> X64LowerM X64.Instruction
movMemToReg memAtom@(X64.Mem {}) cls =
    case cls of
        AST.Int64T -> movInt X64.B64
        AST.Int32T -> movInt X64.B32
        AST.Int16T -> movInt X64.B16
        AST.Int8T -> movInt X64.B8L
        AST.Bool -> movInt X64.B8L
        AST.Char -> movInt X64.B8L
        AST.Float32T -> getTmpFRegM64 >>= \fTmp -> return (X64.Movss (X64.Reg fTmp X64.NN) memAtom X64.B32)
        AST.Float64T -> getTmpFRegM64 >>= \fTmp -> return (X64.Movsd (X64.Reg fTmp X64.NN) memAtom X64.B64)
        _ -> error $ "movMemToReg: unsupported class " ++ show cls
    where
        movInt :: X64.Bits -> X64LowerM X64.Instruction
        movInt bits = getTmpIRegM64 >>= \iTmp -> return (X64.Mov (X64.Reg iTmp bits) memAtom bits)
movMemToReg atom _ =
    error $ "movMemToReg: expected memory atom, got: " ++ show atom


isIntLikeCmpClass64 :: Class -> Bool
isIntLikeCmpClass64 cls =
    cls `elem` [AST.Int8T, AST.Int16T, AST.Int32T, AST.Int64T, AST.Bool, AST.Char]


isBoolIntBridgeClass64 :: Class -> Bool
isBoolIntBridgeClass64 cls =
    cls `elem` [AST.Int8T, AST.Int16T, AST.Int32T, AST.Int64T]


isBoolIntLiteralBridge64 :: Class -> X64.Atom -> Class -> X64.Atom -> Bool
isBoolIntLiteralBridge64 c1 a1 c2 a2 = case (c1, a1, c2, a2) of
    (AST.Bool, _, ci, X64.Imm n) | isBoolIntBridgeClass64 ci -> n == 0 || n == 1
    (ci, X64.Imm n, AST.Bool, _) | isBoolIntBridgeClass64 ci -> n == 0 || n == 1
    _ -> False


loadAtom2ToTempIfMem64 :: X64.Atom -> Class -> X64LowerM ([X64.Instruction], X64.Atom)
loadAtom2ToTempIfMem64 rhs@(X64.Mem {}) cls = do
    movInstr <- movMemToReg rhs cls
    return ([movInstr], moveDst64 movInstr)
loadAtom2ToTempIfMem64 rhs _ = return ([], rhs)


moveDst64 :: X64.Instruction -> X64.Atom
moveDst64 (X64.Mov dst _ _) = dst
moveDst64 (X64.Movss dst _ _) = dst
moveDst64 (X64.Movsd dst _ _) = dst
moveDst64 _ = error "moveDst64: unexpected move instruction"


normalizeFloatCmpOrder64 :: X64.Atom -> X64.Atom -> (X64.Atom, X64.Atom)
normalizeFloatCmpOrder64 lhs@(X64.Reg _ _) rhs = (lhs, rhs)
normalizeFloatCmpOrder64 lhs rhs@(X64.Reg _ _) = (rhs, lhs)
normalizeFloatCmpOrder64 _ _ = error "genIfCmp64: float/double compare requires at least one register operand"


genIfCmp64 ::
    (Int -> X64.Instruction) ->
    (Int -> Int -> [X64.Instruction]) ->
    IRAtom ->
    IRAtom ->
    (Int, Int) ->
    X64LowerM [X64.Instruction]
genIfCmp64 intJump floatJumps atom1 atom2 (thenBlock, elseBlock) = do
    (xAtom1, class1, _) <- tySizeM64 atom1
    (xAtom2, class2, _) <- tySizeM64 atom2
    let boolIntBridge = isBoolIntLiteralBridge64 class1 xAtom1 class2 xAtom2
        cmpClass = if boolIntBridge then AST.Bool else class1
    if class1 /= class2 && not boolIntBridge
        then error $ "genIfCmp64: type mismatch: " ++ show (class1, class2)
        else do
            (pref, rhsReady) <- loadAtom2ToTempIfMem64 xAtom2 cmpClass
            case cmpClass of
                cls | isIntLikeCmpClass64 cls ->
                    let cmpBits = if cls == AST.Int64T then X64.B64 else classBits64 cls
                    in return $ pref ++ [
                        X64.Cmp xAtom1 rhsReady cmpBits,
                        intJump thenBlock,
                        X64.Jump (Right elseBlock)]
                AST.Float32T ->
                    let (lhsF, rhsF) = normalizeFloatCmpOrder64 xAtom1 rhsReady
                    in return $ concat [pref, [X64.Ucomiss lhsF rhsF X64.B32], floatJumps thenBlock elseBlock]
                AST.Float64T ->
                    let (lhsD, rhsD) = normalizeFloatCmpOrder64 xAtom1 rhsReady
                    in return $ concat [pref, [X64.Ucomisd lhsD rhsD X64.B64], floatJumps thenBlock elseBlock]
                _ ->
                    error $ "genIfCmp64: unsupported compare type: " ++ show class1


genIfRel64 :: IRInstr -> X64LowerM [X64.Instruction]
genIfRel64 (IR.Ifeq atom1 atom2 labels) = genIfCmp64 (X64.Je . Right) (\t f -> [X64.Jp (Right f), X64.Je (Right t), X64.Jump (Right f)]) atom1 atom2 labels
genIfRel64 (IR.Ifne atom1 atom2 labels) = genIfCmp64 (X64.Jne . Right) (\t f -> [X64.Jp (Right t), X64.Jne (Right t), X64.Jump (Right f)]) atom1 atom2 labels
genIfRel64 (IR.Iflt atom1 atom2 labels) = genIfCmp64 (X64.Jl . Right) (\t f -> [X64.Jp (Right f), X64.Jb (Right t), X64.Jump (Right f)]) atom1 atom2 labels
genIfRel64 (IR.Ifle atom1 atom2 labels) = genIfCmp64 (X64.Jle . Right) (\t f -> [X64.Jp (Right f), X64.Jbe (Right t), X64.Jump (Right f)]) atom1 atom2 labels
genIfRel64 (IR.Ifgt atom1 atom2 labels) = genIfCmp64 (X64.Jg . Right) (\t f -> [X64.Jp (Right f), X64.Ja (Right t), X64.Jump (Right f)]) atom1 atom2 labels
genIfRel64 (IR.Ifge atom1 atom2 labels) = genIfCmp64 (X64.Jge . Right) (\t f -> [X64.Jp (Right f), X64.Jae (Right t), X64.Jump (Right f)]) atom1 atom2 labels
genIfRel64 _ = error "genIfRel64: expected relational if instruction"


unaryIntBits64 :: Class -> Class -> AST.Operator -> X64LowerM X64.Bits
unaryIntBits64 dstCls srcCls op
    | dstCls /= srcCls =
        error $ "x64StmtLowing64(IUnary " ++ AST.prettyOp op ++ "): type mismatch: " ++ show (dstCls, srcCls)
    | dstCls == AST.Int64T = return X64.B64
    | dstCls == AST.Int32T = return X64.B32
    | otherwise =
        error $ "x64StmtLowing64(IUnary " ++ AST.prettyOp op ++ "): only int/long are supported now, got: " ++ show dstCls


movLike64 :: X64.Bits -> X64.Atom -> X64.Atom -> X64LowerM [X64.Instruction]
movLike64 bits dst src = case (dst, src) of
    (X64.Mem {}, X64.Mem {}) -> do
        tmp <- getTmpIRegM64
        let tmpAtom = X64.Reg tmp bits
        return [X64.Mov tmpAtom src bits, X64.Mov dst tmpAtom bits]
    _ ->
        return [X64.Mov dst src bits]


movLikeF32 :: X64.Atom -> X64.Atom -> X64LowerM [X64.Instruction]
movLikeF32 dst src = case (dst, src) of
    (X64.Mem {}, X64.Mem {}) -> do
        tmp <- getTmpFRegM64
        let tmpAtom = X64.Reg tmp X64.NN
        return [X64.Movss tmpAtom src X64.B32, X64.Movss dst tmpAtom X64.B32]
    _ ->
        return [X64.Movss dst src X64.B32]


movLikeF64 :: X64.Atom -> X64.Atom -> X64LowerM [X64.Instruction]
movLikeF64 dst src = case (dst, src) of
    (X64.Mem {}, X64.Mem {}) -> do
        tmp <- getTmpFRegM64
        let tmpAtom = X64.Reg tmp X64.NN
        return [X64.Movsd tmpAtom src X64.B64, X64.Movsd dst tmpAtom X64.B64]
    _ ->
        return [X64.Movsd dst src X64.B64]


genFloatBin32 ::
    (X64.Atom -> X64.Atom -> X64.Bits -> X64.Instruction) ->
    X64.Atom ->
    X64.Atom ->
    X64.Atom ->
    X64LowerM [X64.Instruction]
genFloatBin32 mkOp dst lhs rhs = do
    rhsReg <- getTmpFRegM64
    accReg <- getFRetRegM64
    let rhsAtom = X64.Reg rhsReg X64.NN
        accAtom = X64.Reg accReg X64.NN
    return [X64.Movss rhsAtom rhs X64.B32, X64.Movss accAtom lhs X64.B32, mkOp accAtom rhsAtom X64.B32, X64.Movss dst accAtom X64.B32]


genFloatBin64 ::
    (X64.Atom -> X64.Atom -> X64.Bits -> X64.Instruction) ->
    X64.Atom ->
    X64.Atom ->
    X64.Atom ->
    X64LowerM [X64.Instruction]
genFloatBin64 mkOp dst lhs rhs = do
    rhsReg <- getTmpFRegM64
    accReg <- getFRetRegM64
    let rhsAtom = X64.Reg rhsReg X64.NN
        accAtom = X64.Reg accReg X64.NN
    return [X64.Movsd rhsAtom rhs X64.B64, X64.Movsd accAtom lhs X64.B64, mkOp accAtom rhsAtom X64.B64, X64.Movsd dst accAtom X64.B64]


powCallName64 :: [String]
powCallName64 = ["xlang", "math", "pow"]


genPowCallF64 :: X64.Atom -> X64.Atom -> X64.Atom -> X64LowerM [X64.Instruction]
genPowCallF64 dst lhs rhs = do
    retReg <- getFRetRegM64
    let arg0 = X64.Reg X64.Xmm0 X64.NN
        arg1 = X64.Reg X64.Xmm1 X64.NN
        retAtom = X64.Reg retReg X64.NN
    return [
        X64.Movsd arg0 lhs X64.B64,
        X64.Movsd arg1 rhs X64.B64,
        X64.Call powCallName64 [AST.Float64T, AST.Float64T, AST.Float64T],
        X64.Movsd dst retAtom X64.B64
        ]


binaryIntBits64 :: Class -> Class -> Class -> AST.Operator -> X64LowerM X64.Bits
binaryIntBits64 dstCls lhsCls rhsCls op
    | dstCls /= lhsCls || lhsCls /= rhsCls =
        error $ concat [
            "x64StmtLowing64(IBinary ", AST.prettyOp op, "): type mismatch: ",
            show (dstCls, lhsCls, rhsCls)]
    | dstCls == AST.Int64T = return X64.B64
    | dstCls == AST.Int32T = return X64.B32
    | dstCls `elem` [AST.Int16T, AST.Int8T, AST.Bool, AST.Char] =
        case op of
            AST.Div -> return X64.B32
            AST.Mod -> return X64.B32
            AST.Pow -> return X64.B32
            _ -> return (classBits64 dstCls)
    | otherwise =
        error $ concat [
            "x64StmtLowing64(IBinary ", AST.prettyOp op,
            "): only int-like types are supported now, got: ", show dstCls]


genIntBinSimple64 ::
    X64.Bits ->
    (X64.Atom -> X64.Atom -> X64.Bits -> X64.Instruction) ->
    X64.Atom ->
    X64.Atom ->
    X64.Atom ->
    X64LowerM [X64.Instruction]
genIntBinSimple64 bits mkOp dst lhs rhs = do
    rhsReg <- getTmpIRegM64
    accReg <- getIRetRegM64
    let rhsAtom = X64.Reg rhsReg bits
        accAtom = X64.Reg accReg bits
    return [X64.Mov rhsAtom rhs bits, X64.Mov accAtom lhs bits, mkOp accAtom rhsAtom bits, X64.Mov dst accAtom bits]


genIntMul64 ::
    X64.Bits ->
    X64.Atom ->
    X64.Atom ->
    X64.Atom ->
    X64LowerM [X64.Instruction]
genIntMul64 bits dst lhs rhs = do
    rhsReg <- getTmpIRegM64
    accReg <- getIRetRegM64
    let rhsAtom = X64.Reg rhsReg bits
        accAtom = X64.Reg accReg bits
    return [X64.Mov rhsAtom rhs bits, X64.Mov accAtom lhs bits, X64.IMul accAtom rhsAtom bits, X64.Mov dst accAtom bits]


genIntShift64 ::
    X64.Bits ->
    (X64.Atom -> X64.Atom -> X64.Bits -> X64.Instruction) ->
    X64.Atom ->
    X64.Atom ->
    X64.Atom ->
    X64LowerM [X64.Instruction]
genIntShift64 bits mkOp dst lhs rhs = do
    accReg <- getIRetRegM64
    let countWide = X64.Reg X64.C bits
        count8 = X64.Reg X64.C X64.B8L
        accAtom = X64.Reg accReg bits
    return [X64.Mov countWide rhs bits, X64.Mov accAtom lhs bits, mkOp accAtom count8 bits, X64.Mov dst accAtom bits]


genIntIDiv64 ::
    X64.Bits ->
    X64.Atom ->
    X64.Atom ->
    X64.Atom ->
    X64LowerM [X64.Instruction]
genIntIDiv64 bits dst lhs rhs = do
    cc <- getCCM64
    wantRem <- getDivWantRemM64
    let lo = X64.Reg (ccDivLo cc) bits
        out = if wantRem
            then X64.Reg (ccDivR cc) bits
            else X64.Reg (ccDivQ cc) bits
        signExt = case bits of
            X64.B32 -> case ccSx32 cc of
                SxCdq -> X64.Cdq
                SxCqo -> error "genIntIDiv64: invalid sign-extend op for 32-bit division"
            X64.B64 -> case ccSx64 cc of
                SxCqo -> X64.Cqo
                SxCdq -> error "genIntIDiv64: invalid sign-extend op for 64-bit division"
            _ -> error "genIntIDiv64: invalid bits"

        -- signed division uses:
        --   32-bit: edx:eax / r/m32
        --   64-bit: rdx:rax / r/m64
        build :: X64.Atom -> [X64.Instruction]
        build divisor = [
            X64.Mov lo lhs bits,
            signExt,
            X64.IDiv lo divisor bits,
            X64.Mov dst out bits]
    rhsReg <- getTmpIRegM64
    let rhsReady = X64.Reg rhsReg bits
    return $ X64.Mov rhsReady rhs bits : build rhsReady


loadBinaryOperands64 ::
    IRAtom ->
    IRAtom ->
    IRAtom ->
    AST.Operator ->
    X64LowerM (X64.Atom, Class, X64.Atom, X64.Atom)
loadBinaryOperands64 dst lhs rhs op = do
    (dstAtom, dstCls, _) <- tySizeM64 dst
    (lhsAtom, lhsCls, _) <- tySizeM64 lhs
    (rhsAtom, rhsCls, _) <- tySizeM64 rhs
    if dstCls /= lhsCls || lhsCls /= rhsCls
        then error $ concat [
            "x64StmtLowing64(IBinary ", AST.prettyOp op, "): type mismatch: ",
            show (dstCls, lhsCls, rhsCls)]
        else return (dstAtom, dstCls, lhsAtom, rhsAtom)


x64StmtCode64 :: IRInstr -> X64LowerM [X64.Instruction]
x64StmtCode64 (IR.Jump label) = return [X64.Jump (Right label)]
x64StmtCode64 instr@(IR.Ifeq {}) = genIfRel64 instr
x64StmtCode64 instr@(IR.Ifne {}) = genIfRel64 instr
x64StmtCode64 instr@(IR.Iflt {}) = genIfRel64 instr
x64StmtCode64 instr@(IR.Ifge {}) = genIfRel64 instr
x64StmtCode64 instr@(IR.Ifgt {}) = genIfRel64 instr
x64StmtCode64 instr@(IR.Ifle {}) = genIfRel64 instr

x64StmtCode64 (IR.SetRet atom) = do
    (srcAtom, cls, _) <- tySizeM64 atom
    iRet <- getIRetRegM64
    fRet <- getFRetRegM64
    if cls `HashSet.member` intRet64Classes64 || isRefClass64 cls
        then return [X64.Mov (X64.Reg iRet X64.B64) srcAtom X64.B64]
        else if cls `HashSet.member` intRet32Classes64
            then case Map.lookup cls bitsByClass64 of
                Just bits -> return [X64.Mov (X64.Reg iRet bits) srcAtom bits]
                Nothing -> error $ "x64StmtLowing64(SetRet): unsupported int-like return class " ++ show cls
            else case cls of
                AST.Float32T -> return [X64.Movss (X64.Reg fRet X64.NN) srcAtom X64.B32]
                AST.Float64T -> return [X64.Movsd (X64.Reg fRet X64.NN) srcAtom X64.B64]
                _ -> error $ "x64StmtLowing64(SetRet): unsupported return class " ++ show cls

x64StmtCode64 IR.Return = return [X64.Leave, X64.Ret]
x64StmtCode64 IR.VReturn = return [X64.Leave, X64.Ret]

x64StmtCode64 (IR.IAssign dst src) = do
    (dstAtom, dstCls, _) <- tySizeM64 dst
    (srcAtom, srcCls, _) <- tySizeM64 src
    if dstCls /= srcCls
        then error $ "x64StmtLowing64(IAssign): type mismatch: " ++ show (dstCls, srcCls)
        else case intAssignBits64 srcCls of
            Just bits -> do
                tmp <- getTmpIRegM64
                let tmpAtom = X64.Reg tmp bits
                return [X64.Mov tmpAtom srcAtom bits, X64.Mov dstAtom tmpAtom bits]
            Nothing -> case srcCls of
                AST.Float32T -> do
                    tmpF <- getTmpFRegM64
                    let tmpAtom = X64.Reg tmpF X64.NN
                    return [X64.Movss tmpAtom srcAtom X64.B32, X64.Movss dstAtom tmpAtom X64.B32]
                AST.Float64T -> do
                    tmpF <- getTmpFRegM64
                    let tmpAtom = X64.Reg tmpF X64.NN
                    return [X64.Movsd tmpAtom srcAtom X64.B64, X64.Movsd dstAtom tmpAtom X64.B64]
                _ ->
                    error $ "x64StmtLowing64(IAssign): unsupported assignment class: " ++ show srcCls

x64StmtCode64 (IR.IUnary dst AST.UnaryPlus src) = do
    (dstAtom, dstCls, _) <- tySizeM64 dst
    (srcAtom, srcCls, _) <- tySizeM64 src
    if dstCls /= srcCls
        then error $ "x64StmtLowing64(IUnary " ++ AST.prettyOp AST.UnaryPlus ++ "): type mismatch: " ++ show (dstCls, srcCls)
        else case dstCls of
            AST.Int32T -> movLike64 X64.B32 dstAtom srcAtom
            AST.Int64T -> movLike64 X64.B64 dstAtom srcAtom
            AST.Float32T -> movLikeF32 dstAtom srcAtom
            AST.Float64T -> movLikeF64 dstAtom srcAtom
            _ ->
                error $ "x64StmtLowing64(IUnary " ++ AST.prettyOp AST.UnaryPlus ++ "): only int/long/float/double are supported now, got: " ++ show dstCls

x64StmtCode64 (IR.IUnary dst AST.UnaryMinus src) = do
    (dstAtom, dstCls, _) <- tySizeM64 dst
    (srcAtom, srcCls, _) <- tySizeM64 src
    if dstCls /= srcCls
        then error $ concat ["x64StmtLowing64(IUnary ", AST.prettyOp AST.UnaryMinus, "): type mismatch: ", show (dstCls, srcCls)]
        else case dstCls of
            AST.Int64T -> do
                tmp <- getTmpIRegM64
                let tmpAtom = X64.Reg tmp X64.B64
                return [X64.Mov tmpAtom srcAtom X64.B64, X64.Neg tmpAtom X64.B64, X64.Mov dstAtom tmpAtom X64.B64]
            AST.Int32T -> do
                tmp <- getTmpIRegM64
                let tmpAtom = X64.Reg tmp X64.B32
                return [X64.Mov tmpAtom srcAtom X64.B32, X64.Neg tmpAtom X64.B32, X64.Mov dstAtom tmpAtom X64.B32]
            AST.Float32T -> do
                tmpI <- getTmpIRegM64
                tmpF <- getTmpFRegM64
                let iTmp = X64.Reg tmpI X64.B32
                    fTmp = X64.Reg tmpF X64.NN
                -- tmp = -1.0f via int->float conversion; dst = src * tmp
                return [
                    X64.Mov iTmp (X64.Imm (-1)) X64.B32,
                    X64.Cvtsi2ss fTmp iTmp X64.B32,
                    X64.Mulss fTmp srcAtom X64.B32,
                    X64.Movss dstAtom fTmp X64.B32]
            AST.Float64T -> do
                tmpI <- getTmpIRegM64
                tmpF <- getTmpFRegM64
                let iTmp = X64.Reg tmpI X64.B64
                    fTmp = X64.Reg tmpF X64.NN
                -- tmp = -1.0 via int->double conversion; dst = src * tmp
                return [
                    X64.Mov iTmp (X64.Imm (-1)) X64.B64,
                    X64.Cvtsi2sd fTmp iTmp X64.B64,
                    X64.Mulsd fTmp srcAtom X64.B64,
                    X64.Movsd dstAtom fTmp X64.B64]
            _ ->
                error $ concat ["x64StmtLowing64(IUnary ", AST.prettyOp AST.UnaryMinus, "): only int/long/float/double are supported now, got: ", show dstCls]

x64StmtCode64 (IR.IUnary dst AST.LogicalNot src) = do
    (dstAtom, dstCls, _) <- tySizeM64 dst
    (srcAtom, srcCls, _) <- tySizeM64 src
    if dstCls /= AST.Bool || srcCls /= AST.Bool
        then error $ "x64StmtLowing64(LogicalNot): only bool is supported, got: " ++ show (dstCls, srcCls)
        else do
            pref <- movLike64 X64.B32 dstAtom srcAtom
            return $ pref ++ [X64.Xor dstAtom (X64.Imm 1) X64.B32]

x64StmtCode64 (IR.IUnary dst AST.BitInv src) = do
    (dstAtom, dstCls, _) <- tySizeM64 dst
    (srcAtom, srcCls, _) <- tySizeM64 src
    bits <- unaryIntBits64 dstCls srcCls AST.BitInv
    tmp <- getTmpIRegM64
    let tmpAtom = X64.Reg tmp bits
    return [X64.Mov tmpAtom srcAtom bits, X64.Not tmpAtom bits, X64.Mov dstAtom tmpAtom bits]

x64StmtCode64 (IR.IUnary _ op _) =
    error $ "x64StmtLowing64(IUnary): unsupported unary operator: " ++ show op


x64StmtCode64 (IR.IBinary dst AST.Add lhs rhs) = do
    (dstAtom, dstCls, lhsAtom, rhsAtom) <- loadBinaryOperands64 dst lhs rhs AST.Add
    case dstCls of
        AST.Float32T -> genFloatBin32 X64.Addss dstAtom lhsAtom rhsAtom
        AST.Float64T -> genFloatBin64 X64.Addsd dstAtom lhsAtom rhsAtom
        _ -> do
            bits <- binaryIntBits64 dstCls dstCls dstCls AST.Add
            genIntBinSimple64 bits X64.Add dstAtom lhsAtom rhsAtom

x64StmtCode64 (IR.IBinary dst AST.Sub lhs rhs) = do
    (dstAtom, dstCls, lhsAtom, rhsAtom) <- loadBinaryOperands64 dst lhs rhs AST.Sub
    case dstCls of
        AST.Float32T -> genFloatBin32 X64.Subss dstAtom lhsAtom rhsAtom
        AST.Float64T -> genFloatBin64 X64.Subsd dstAtom lhsAtom rhsAtom
        _ -> do
            bits <- binaryIntBits64 dstCls dstCls dstCls AST.Sub
            genIntBinSimple64 bits X64.Sub dstAtom lhsAtom rhsAtom

x64StmtCode64 (IR.IBinary dst AST.Mul lhs rhs) = do
    (dstAtom, dstCls, lhsAtom, rhsAtom) <- loadBinaryOperands64 dst lhs rhs AST.Mul
    case dstCls of
        AST.Float32T -> genFloatBin32 X64.Mulss dstAtom lhsAtom rhsAtom
        AST.Float64T -> genFloatBin64 X64.Mulsd dstAtom lhsAtom rhsAtom
        _ -> do
            bits <- binaryIntBits64 dstCls dstCls dstCls AST.Mul
            genIntMul64 bits dstAtom lhsAtom rhsAtom

x64StmtCode64 (IR.IBinary dst AST.Div lhs rhs) = do
    (dstAtom, dstCls, lhsAtom, rhsAtom) <- loadBinaryOperands64 dst lhs rhs AST.Div
    case dstCls of
        AST.Float32T -> genFloatBin32 X64.Divss dstAtom lhsAtom rhsAtom
        AST.Float64T -> genFloatBin64 X64.Divsd dstAtom lhsAtom rhsAtom
        _ -> do
            bits <- binaryIntBits64 dstCls dstCls dstCls AST.Div
            setDivWantRemM64 False
            genIntIDiv64 bits dstAtom lhsAtom rhsAtom

x64StmtCode64 (IR.IBinary dst AST.Pow lhs rhs) = do
    (dstAtom, dstCls, lhsAtom, rhsAtom) <- loadBinaryOperands64 dst lhs rhs AST.Pow
    case dstCls of
        AST.Float64T -> genPowCallF64 dstAtom lhsAtom rhsAtom
        AST.Float32T ->
            error "x64StmtLowing64(IBinary **): float32 pow should be promoted to double before x64 lowering"
        _ ->
            error "x64StmtLowing64(IBinary **): only double pow is supported; non-double pow should be promoted before lowering"

x64StmtCode64 (IR.IBinary dst AST.Mod lhs rhs) = do
    (dstAtom, dstCls, lhsAtom, rhsAtom) <- loadBinaryOperands64 dst lhs rhs AST.Mod
    bits <- binaryIntBits64 dstCls dstCls dstCls AST.Mod
    setDivWantRemM64 True
    genIntIDiv64 bits dstAtom lhsAtom rhsAtom

x64StmtCode64 (IR.IBinary dst AST.BitAnd lhs rhs) = do
    (dstAtom, dstCls, lhsAtom, rhsAtom) <- loadBinaryOperands64 dst lhs rhs AST.BitAnd
    bits <- binaryIntBits64 dstCls dstCls dstCls AST.BitAnd
    genIntBinSimple64 bits X64.And dstAtom lhsAtom rhsAtom

x64StmtCode64 (IR.IBinary dst AST.BitXor lhs rhs) = do
    (dstAtom, dstCls, lhsAtom, rhsAtom) <- loadBinaryOperands64 dst lhs rhs AST.BitXor
    bits <- binaryIntBits64 dstCls dstCls dstCls AST.BitXor
    genIntBinSimple64 bits X64.Xor dstAtom lhsAtom rhsAtom

x64StmtCode64 (IR.IBinary dst AST.BitOr lhs rhs) = do
    (dstAtom, dstCls, lhsAtom, rhsAtom) <- loadBinaryOperands64 dst lhs rhs AST.BitOr
    bits <- binaryIntBits64 dstCls dstCls dstCls AST.BitOr
    genIntBinSimple64 bits X64.Or dstAtom lhsAtom rhsAtom

x64StmtCode64 (IR.IBinary dst AST.BitLShift lhs rhs) = do
    (dstAtom, dstCls, lhsAtom, rhsAtom) <- loadBinaryOperands64 dst lhs rhs AST.BitLShift
    bits <- binaryIntBits64 dstCls dstCls dstCls AST.BitLShift
    genIntShift64 bits X64.Shl dstAtom lhsAtom rhsAtom

x64StmtCode64 (IR.IBinary dst AST.BitRShift lhs rhs) = do
    (dstAtom, dstCls, lhsAtom, rhsAtom) <- loadBinaryOperands64 dst lhs rhs AST.BitRShift
    bits <- binaryIntBits64 dstCls dstCls dstCls AST.BitRShift
    genIntShift64 bits X64.Sar dstAtom lhsAtom rhsAtom

x64StmtCode64 (IR.IBinary dst AST.BitURShift lhs rhs) = do
    (dstAtom, dstCls, lhsAtom, rhsAtom) <- loadBinaryOperands64 dst lhs rhs AST.BitURShift
    bits <- binaryIntBits64 dstCls dstCls dstCls AST.BitURShift
    genIntShift64 bits X64.Shr dstAtom lhsAtom rhsAtom

x64StmtCode64 (IR.IBinary _ op _ _) =
    error $ "x64StmtLowing64(IBinary): unsupported operator: " ++ show op

x64StmtCode64 (IR.ICast dst (fromC, toC) src) = do
    (dstAtom, dstCls, _) <- tySizeM64 dst
    (srcAtom, srcCls, _) <- tySizeM64 src
    if srcCls /= fromC || dstCls /= toC
        then error $ "x64StmtLowing64(ICast): type mismatch between IR annotation and atom types: " ++ show (fromC, toC, srcCls, dstCls)
        else case (fromC, toC) of
            (fromInt, AST.Bool)
                | isCastIntClass64 fromInt ->
                    genIntToBoolCast64 dstAtom fromInt srcAtom
            (AST.Bool, toInt)
                | isCastIntClass64 toInt ->
                    genBoolToIntCast64 dstAtom toInt srcAtom
            (AST.Bool, toF)
                | toF `elem` [AST.Float32T, AST.Float64T] ->
                    genBoolToFloatCast64 dstAtom toF srcAtom
            (fromF, AST.Bool)
                | fromF `elem` [AST.Float32T, AST.Float64T] ->
                    genFloatToBoolCast64 dstAtom fromF srcAtom
            (fromF, toF)
                | fromF `elem` [AST.Float32T, AST.Float64T]
                    && toF `elem` [AST.Float32T, AST.Float64T]
                    && fromF /= toF ->
                    genFloatToFloatCast64 dstAtom fromF toF srcAtom
            (fromF, toInt)
                | fromF `elem` [AST.Float32T, AST.Float64T] && isCastIntClass64 toInt ->
                    genFloatToIntCast64 dstAtom fromF toInt srcAtom
            (fromInt, toF)
                | (isCastIntClass64 fromInt || fromInt == AST.Bool) && toF `elem` [AST.Float32T, AST.Float64T] ->
                    genIntToFloatCast64 dstAtom fromInt toF srcAtom
            (fromInt, toInt)
                | isCastIntClass64 fromInt && isCastIntClass64 toInt ->
                    genIntCast64 dstAtom fromInt toInt srcAtom
            _ ->
                error $ "x64StmtLowing64(ICast): only int-like<->int-like, int-like->float/double, bool<->int, bool<->float/double, float<->double, and float/double->int casts are supported now, got: " ++ show (fromC, toC)

x64StmtCode64 (IR.ICallStaticDirect dst qname args) = do
    (dstAtom, dstCls, _) <- tySizeM64 dst
    argTriples <- mapM tySizeM64 args
    cc <- getCCM64
    iRet <- getIRetRegM64
    fRet <- getFRetRegM64
    let argAtoms = map (\(a, _, _) -> a) argTriples
        argClasses = map (\(_, c, _) -> c) argTriples
        fullSig = argClasses ++ [dstCls]
        (callName, callSig) = mkDirectCallName64 qname fullSig
        retInstrs = case dstCls of
            AST.Void -> []
            AST.Float32T -> [X64.Movss dstAtom (X64.Reg fRet X64.NN) X64.B32]
            AST.Float64T -> [X64.Movsd dstAtom (X64.Reg fRet X64.NN) X64.B64]
            cls | isRefClass64 cls -> [X64.Mov dstAtom (X64.Reg iRet X64.B64) X64.B64]
            cls -> case Map.lookup cls bitsByClass64 of
                Just bits -> [X64.Mov dstAtom (X64.Reg iRet bits) bits]
                Nothing -> error $ "x64StmtLowing64(ICallStaticDirect): unsupported return class: " ++ show cls
    regPlan <- assignArgRegs64 cc argClasses
    argInstrs <- concat <$> mapM (\(regSel, (atom, cls)) -> moveArgToReg64 regSel atom cls)
        (zip regPlan (zip argAtoms argClasses))
    return (argInstrs ++ [X64.Call callName callSig] ++ retInstrs)
x64StmtCode64 (IR.ICallVirtual {}) = error "TODO"
x64StmtCode64 (IR.ICallStatic dst qname args) = do
    (dstAtom, dstCls, _) <- tySizeM64 dst
    argTriples <- mapM tySizeM64 args
    cc <- getCCM64
    iRet <- getIRetRegM64
    fRet <- getFRetRegM64

    let argAtoms = map (\(a, _, _) -> a) argTriples
        argClasses = map (\(_, c, _) -> c) argTriples
        (callName, callSig) = mkStaticCallName64 qname (argClasses ++ [dstCls])

    regPlan <- assignArgRegs64 cc argClasses
    argInstrs <- concat <$> mapM (\(regSel, (atom, cls)) -> moveArgToReg64 regSel atom cls)
            (zip regPlan (zip argAtoms argClasses))

    let retInstrs = case dstCls of
            AST.Void -> []
            AST.Float32T -> [X64.Movss dstAtom (X64.Reg fRet X64.NN) X64.B32]
            AST.Float64T -> [X64.Movsd dstAtom (X64.Reg fRet X64.NN) X64.B64]
            cls | isRefClass64 cls -> [X64.Mov dstAtom (X64.Reg iRet X64.B64) X64.B64]
            cls -> case Map.lookup cls bitsByClass64 of
                Just bits -> [X64.Mov dstAtom (X64.Reg iRet bits) bits]
                Nothing -> error $ "x64StmtLowing64(ICallStatic): unsupported return class: " ++ show cls

    return $ concat [argInstrs, [X64.Call callName callSig], retInstrs]

x64StmtCode64 (IR.IGetField {}) = error "TODO"
x64StmtCode64 (IR.IPutField {}) = error "TODO"

x64StmtCode64 (IR.IGetStatic dst qname) = do
    (dstAtom, dstCls, _) <- tySizeM64 dst
    bssAtom <- staticFieldAtom64 qname dstCls
    case dstCls of
        AST.Float32T -> do
            tmpF <- getTmpFRegM64
            let tmpAtom = X64.Reg tmpF X64.NN
            withStaticClinitM64 qname [X64.Movss tmpAtom bssAtom X64.B32, X64.Movss dstAtom tmpAtom X64.B32]
        AST.Float64T -> do
            tmpF <- getTmpFRegM64
            let tmpAtom = X64.Reg tmpF X64.NN
            withStaticClinitM64 qname [X64.Movsd tmpAtom bssAtom X64.B64, X64.Movsd dstAtom tmpAtom X64.B64]
        cls | cls == AST.Int64T || isRefClass64 cls -> do
            tmpI <- getTmpIRegM64
            let tmpAtom = X64.Reg tmpI X64.B64
            withStaticClinitM64 qname [X64.Mov tmpAtom bssAtom X64.B64, X64.Mov dstAtom tmpAtom X64.B64]
        cls -> case Map.lookup cls bitsByClass64 of
            Just bits -> do
                tmpI <- getTmpIRegM64
                let tmpAtom = X64.Reg tmpI bits
                withStaticClinitM64 qname [X64.Mov tmpAtom bssAtom bits, X64.Mov dstAtom tmpAtom bits]
            Nothing ->
                error $ "x64StmtLowing64(IGetStatic): unsupported class: " ++ show dstCls

x64StmtCode64 (IR.IPutStatic qname src) = do
    (srcAtom, srcCls, _) <- tySizeM64 src
    bssAtom <- staticFieldAtom64 qname srcCls
    case srcCls of
        AST.Float32T -> do
            tmpF <- getTmpFRegM64
            let tmpAtom = X64.Reg tmpF X64.NN
            withStaticClinitM64 qname [X64.Movss tmpAtom srcAtom X64.B32, X64.Movss bssAtom tmpAtom X64.B32]
        AST.Float64T -> do
            tmpF <- getTmpFRegM64
            let tmpAtom = X64.Reg tmpF X64.NN
            withStaticClinitM64 qname [X64.Movsd tmpAtom srcAtom X64.B64, X64.Movsd bssAtom tmpAtom X64.B64]
        cls | cls == AST.Int64T || isRefClass64 cls -> do
            tmpI <- getTmpIRegM64
            let tmpAtom = X64.Reg tmpI X64.B64
            withStaticClinitM64 qname [X64.Mov tmpAtom srcAtom X64.B64, X64.Mov bssAtom tmpAtom X64.B64]
        cls -> case Map.lookup cls bitsByClass64 of
            Just bits -> do
                tmpI <- getTmpIRegM64
                let tmpAtom = X64.Reg tmpI bits
                withStaticClinitM64 qname [X64.Mov tmpAtom srcAtom bits, X64.Mov bssAtom tmpAtom bits]
            Nothing ->
                error $ "x64StmtLowing64(IPutStatic): unsupported class: " ++ show srcCls


type ExternSym = ([String], [Class])
type ExternRef = ([String], ExternSym)

mkExternRef64 :: [String] -> [Class] -> ExternRef
mkExternRef64 [] sigTs = ([], ([], sigTs))
mkExternRef64 fullName sigTs = (init fullName, (fullName, sigTs))


externTargetFromInstr64 :: X64.Instruction -> Maybe ExternSym
externTargetFromInstr64 (X64.Call fullName sigTs) = Just (fullName, sigTs)
externTargetFromInstr64 (X64.Jump (Left fullName)) = Just (fullName, [])
externTargetFromInstr64 (X64.Je (Left fullName)) = Just (fullName, [])
externTargetFromInstr64 (X64.Jne (Left fullName)) = Just (fullName, [])
externTargetFromInstr64 (X64.Jp (Left fullName)) = Just (fullName, [])
externTargetFromInstr64 (X64.Jg (Left fullName)) = Just (fullName, [])
externTargetFromInstr64 (X64.Jge (Left fullName)) = Just (fullName, [])
externTargetFromInstr64 (X64.Jl (Left fullName)) = Just (fullName, [])
externTargetFromInstr64 (X64.Jle (Left fullName)) = Just (fullName, [])
externTargetFromInstr64 (X64.Ja (Left fullName)) = Just (fullName, [])
externTargetFromInstr64 (X64.Jae (Left fullName)) = Just (fullName, [])
externTargetFromInstr64 (X64.Jb (Left fullName)) = Just (fullName, [])
externTargetFromInstr64 (X64.Jbe (Left fullName)) = Just (fullName, [])
externTargetFromInstr64 _ = Nothing


externTargetsFromInstrs64 :: [X64.Instruction] -> [ExternSym]
externTargetsFromInstrs64 = foldr step []
    where
        step :: X64.Instruction -> [ExternSym] -> [ExternSym]
        step instr acc = case externTargetFromInstr64 instr of
            Just sym -> sym : acc
            Nothing -> acc


instrExternRefs64 :: [X64.Instruction] -> [ExternRef]
instrExternRefs64 = foldl' step []
    where
        step :: [ExternRef] -> X64.Instruction -> [ExternRef]
        step acc instr = case externTargetFromInstr64 instr of
            Just (fullName, sigTs) -> mkExternRef64 fullName sigTs : acc
            Nothing -> acc


x64StmtLowing64 :: IRInstr -> X64LowerM ([X64.Instruction], [ExternRef])
x64StmtLowing64 instr = do
    instrs <- x64StmtCode64 instr
    return (instrs, instrExternRefs64 instrs)


-- Convert one static field attribute to its default data segment const.
-- Current policy: default initialize to zero with field-width.
attrToInstCconst64 :: IR.Attribute -> X64.InstrConst
attrToInstCconst64 (_, cls, _, _) = case cls of
    AST.Float32T -> X64.Long (floatHex32Const64 0.0)
    AST.Float64T -> X64.Quad (doubleHex64Const64 0.0)
    _ -> X64.Zero (staticBits64 cls)

attrToInstCconst :: IR.Attribute -> X64.InstrConst
attrToInstCconst = attrToInstCconst64


-- Convert one static field attribute to one-or-more data segment consts.
-- Kept for future extension like split constants:
--   .long low32
--   .long high32
attrToInstCconsts64 :: IR.Attribute -> [X64.InstrConst]
attrToInstCconsts64 attr@(_, cls, _, _) = case cls of
    AST.Float128T -> [X64.Quad "0x0000000000000000", X64.Quad "0x0000000000000000"]
    _ -> [attrToInstCconst64 attr]


floatHex32Const64 :: Float -> String
floatHex32Const64 = Basic.toData


doubleHex64Const64 :: Double -> String
doubleHex64Const64 d =
    let (hi32, lo32) = Basic.toData64 d
    in "0x" ++ stripHexPrefix64 hi32 ++ stripHexPrefix64 lo32


stripHexPrefix64 :: String -> String
stripHexPrefix64 ('0' : 'x' : xs) = xs
stripHexPrefix64 ('0' : 'X' : xs) = xs
stripHexPrefix64 s = s


-- Class printable name:
--   package + classes => "pkg.a.b" + "." + "Outer$Inner"
classToString64 :: [String] -> [String] -> String
classToString64 pkgSegs classSegs =
    let classPart = intercalate "$" classSegs
    in case pkgSegs of
        [] -> classPart
        _ -> intercalate "." pkgSegs ++ "." ++ classPart


-- Parent list printable string:
-- first item should be extends target, rest are interfaces.
-- encoded as comma-separated class names.
classParentsCsv64 :: [([String], [String])] -> String
classParentsCsv64 xs = intercalate "," [classToString64 pkg cls | (pkg, cls) <- xs]


stringBytesData64 :: X64.CallConv64 -> String -> [X64.InstrConst]
stringBytesData64 cc s = case X64.ccCompiler cc of
    X64.NASM -> map (X64.Byte . show . ord) s ++ [X64.Byte "0"]
    _ -> [stringZConst64 cc s]


stringZConst64 :: X64.CallConv64 -> String -> X64.InstrConst
stringZConst64 cc s = case X64.ccCompiler cc of
    X64.NASM -> X64.RawString ("db\t\t" ++ show s ++ ", 0")
    _ -> X64.RawString (".asciz\t\t" ++ show s)


-- Lower class static attributes to assembly static data labels.
-- Example:
--   owner qname: ["com","wangdi","Const"], attr name "a"
--   => label: "com.wangdi.Const.a"
staticLowing64 :: [String] -> [IR.Attribute] -> [X64.StaticData]
staticLowing64 ownerQName attrs = initFlag : map lowerOne (filter isStaticAttr64 attrs)
    where
        initFlag :: X64.StaticData
        initFlag =
            let label = mangleQNameWithSig64 False (ownerQName ++ [staticInitFlagName]) [AST.Int32T]
            in X64.StaticData label [X64.Zero X64.B32]

        lowerOne :: IR.Attribute -> X64.StaticData
        lowerOne attr@(_, cls, fieldName, _) =
            let label = mangleQNameWithSig64 False (ownerQName ++ [fieldName]) [cls]
            in X64.StaticData label (attrToInstCconsts64 attr)


mangleQName64 :: Bool -> [String] -> String
mangleQName64 isFunction qn = case qn of
    [] -> error "mangleQName64: empty qname"
    [name] -> Basic.mangleName [] [name] [] isFunction
    _ -> Basic.mangleName (init qn) [last qn] [] isFunction


mangleQNameWithSig64 :: Bool -> [String] -> [Class] -> String
mangleQNameWithSig64 isFunction qn sigTs = case qn of
    [] -> error "mangleQNameWithSig64: empty qname"
    [name] -> Basic.mangleName [] [name] sigTs isFunction
    _ -> Basic.mangleName (init qn) [last qn] sigTs isFunction


isStaticAttr64 :: IR.Attribute -> Bool
isStaticAttr64 ((_, flags), _, _, _) = Parse.Static `elem` flags


staticBits64 :: Class -> X64.Bits
staticBits64 cls = case cls of
    AST.Bool -> X64.B8L
    AST.Char -> X64.B8L
    AST.Int8T -> X64.B8L
    AST.Int16T -> X64.B16
    AST.Int32T -> X64.B32
    AST.Int64T -> X64.B64
    AST.Float32T -> X64.B32
    AST.Float64T -> X64.B64
    AST.Float128T -> X64.B64
    AST.Class _ _ -> X64.B64
    AST.Void -> error "staticBits64: void cannot be a field storage type"
    AST.ErrorClass -> error "staticBits64: error class cannot be lowered"


-- Lower one IR block to one x64 segment label.
-- Note: lowering needs X64LowerM state for stack-layout/type lookups.
x64LowingIRBlock :: IR.IRBlock -> X64LowerM (X64.X64Segment, [ExternRef])
x64LowingIRBlock (IR.IRBlock (labelId, instrs)) = do
    stmtOuts <- mapM x64StmtLowing64 instrs
    let x64Instrs = concatMap fst stmtOuts
        refs = foldl' (\acc (_, rs) -> rs ++ acc) [] stmtOuts
    return (X64.X64Label labelId x64Instrs, refs)


-- Align positive stack byte size up to 16 for x64 call alignment.
alignUp16 :: Int -> Int
alignUp16 n
    | n <= 0 = 0
    | otherwise = (n + 15) `div` 16 * 16


-- Win64 caller must reserve 32-byte shadow space before each call.
minCallFrame64 :: CallConv64 -> Int
minCallFrame64 cc
    | isWinArgLayout64 cc = 32
    | otherwise = 0


paramSpillBits64 :: Class -> Maybe X64.Bits
paramSpillBits64 cls
    | cls == AST.Int64T = Just X64.B64
    | isRefClass64 cls = Just X64.B64
    | otherwise = Map.lookup cls bitsByClass64


-- Function prologue + param register spills:
--   push rbp
--   mov  rbp, rsp
--   sub  rsp, <frame-bytes-aligned>
--   mov  [rbp-off(param_i)], argReg_i
x64FuncEntry64 :: IR.IRFunction -> X64LowerM [X64.Instruction]
x64FuncEntry64 (IR.IRFunction _ _ funSig _ _ _) = do
    cc <- getCCM64
    bp <- getBpRegM64
    sp <- getSpRegM64
    frameBytes0 <- alignUp16 <$> getMaxOffM64
    let frameBytes = max frameBytes0 (minCallFrame64 cc)
    let paramTypes = TEnv.funParams funSig
        prologue = [X64.Push X64.B64, X64.Mov (X64.Reg bp X64.B64) (X64.Reg sp X64.B64) X64.B64]
        alloc = ([X64.Sub (X64.Reg sp X64.B64) (X64.Imm frameBytes) X64.B64 | frameBytes > 0])

    regPlan <- assignArgRegs64 cc paramTypes
    let regParams = zip3 [0 ..] regPlan paramTypes
    spills <- concat <$> mapM spillOne regParams
    return (prologue ++ alloc ++ spills)
    where
        spillOne :: (Int, ArgRegSel64, Class) -> X64LowerM [X64.Instruction]
        spillOne (paramIdx, regSel, cls) = do
            off <- getAtomOffM64 (IR.Param paramIdx)
            bp <- getBpRegM64
            let dst = X64.Mem (Just bp) Nothing (negate off)
            case regSel of
                ArgRegF fReg -> case cls of
                    AST.Float32T -> return [X64.Movss dst (X64.Reg fReg X64.NN) X64.B32]
                    AST.Float64T -> return [X64.Movsd dst (X64.Reg fReg X64.NN) X64.B64]
                    _ -> error $ "x64FuncEntry64: non-float class spilled from float register: " ++ show cls
                ArgRegI iReg -> case paramSpillBits64 cls of
                    Just bits ->
                        let src = X64.Reg iReg bits
                        in return [X64.Mov dst src bits]
                    Nothing ->
                        error $ "x64FuncEntry64: unsupported param class for register spill: " ++ show cls


x64LowingBlocks64 :: [IR.IRBlock] -> X64LowerM ([X64.X64Segment], [ExternRef])
x64LowingBlocks64 blocks = do
    outs <- mapM x64LowingIRBlock blocks
    let segs = map fst outs
        refs = foldl' (\acc (_, rs) -> rs ++ acc) [] outs
    return (segs, refs)


accessPublicBit64 :: Int
accessPublicBit64 = 0


accessPrivateBit64 :: Int
accessPrivateBit64 = 1


accessProtectedBit64 :: Int
accessProtectedBit64 = 2


accessStaticBit64 :: Int
accessStaticBit64 = 3


accessFinalBit64 :: Int
accessFinalBit64 = 4


accessInlineBit64 :: Int
accessInlineBit64 = 5


ownerTypeClassCode64 :: Int
ownerTypeClassCode64 = 0


ownerTypeTopLevelCode64 :: Int
ownerTypeTopLevelCode64 = 1


bitMask64 :: Int -> Int
bitMask64 b = 1 `shiftL` b


declAccessMask64 :: Parse.Decl -> Int
declAccessMask64 (acc, flags) =
    let vis = case acc of
            Parse.Public -> bitMask64 accessPublicBit64
            Parse.Private -> bitMask64 accessPrivateBit64
            Parse.Protected -> bitMask64 accessProtectedBit64
        stBits = if Parse.Static `elem` flags then bitMask64 accessStaticBit64 else 0
        finBits = if Parse.Final `elem` flags then bitMask64 accessFinalBit64 else 0
        inlBits = if Parse.Inline `elem` flags then bitMask64 accessInlineBit64 else 0
        extras = stBits .|. finBits .|. inlBits
    in vis .|. extras


ownerTypeCode64 :: IR.IRMemberType -> Int
ownerTypeCode64 memberTy = case memberTy of
    IR.MemberClass -> ownerTypeClassCode64
    IR.MemberClassWrapped -> ownerTypeTopLevelCode64


classTypeToken64 :: Class -> String
classTypeToken64 cls = case cls of
    AST.ErrorClass -> "error"
    AST.Class names [] -> intercalate "." names
    AST.Class _ _ -> AST.classMangle cls
    _ -> AST.classMangle cls


mainTypeCode64 :: IR.MainKind -> Int
mainTypeCode64 mainKind = case mainKind of
    IR.NoMain -> -1
    IR.MainVoid _ -> 0
    IR.MainInt _ -> 1
    IR.MainVoidArgs _ -> 2
    IR.MainIntArgs _ -> 3


mainQName64 :: IR.MainKind -> [String]
mainQName64 mainKind = case mainKind of
    IR.NoMain -> []
    IR.MainVoid qn -> qn
    IR.MainInt qn -> qn
    IR.MainVoidArgs qn -> qn
    IR.MainIntArgs qn -> qn


classInfoFieldJson64 :: IR.Attribute -> Value
classInfoFieldJson64 (decl, cls, fieldName, memberTy) = object [
    "access" .= declAccessMask64 decl,
    "attr_name" .= fieldName,
    "attr_type" .= classTypeToken64 cls,
    "owner_type" .= ownerTypeCode64 memberTy
    ]


classInfoMethodJson64 :: IR.IRFunction -> Value
classInfoMethodJson64 (IR.IRFunction decl funName sig _ _ memberTy) = object [
    "access" .= declAccessMask64 decl,
    "name" .= funName,
    "owner_type" .= ownerTypeCode64 memberTy,
    "param_types" .= map classTypeToken64 (TEnv.funParams sig),
    "return" .= classTypeToken64 (TEnv.funReturn sig)
    ]


classInfoJsonValue64 :: Parse.Decl -> [String] -> IR.MainKind -> [IR.Attribute] -> [IR.IRFunction] -> Value
classInfoJsonValue64 classDecl ownerQName mainKind attrs funs = object [
    "access" .= declAccessMask64 classDecl,
    "attributes" .= map classInfoFieldJson64 attrs,
    "class" .= ownerQName,
    "interfaces" .= ([] :: [[String]]),
    "main_qname" .= mainQName64 mainKind,
    "main_type" .= mainTypeCode64 mainKind,
    "methods" .= map classInfoMethodJson64 funs,
    "signature" .= ([] :: [[String]]),
    "super_class" .= ([] :: [String])
    ]


classInfoJsonText64 :: Parse.Decl -> [String] -> IR.MainKind -> [IR.Attribute] -> [IR.IRFunction] -> String
classInfoJsonText64 classDecl ownerQName mainKind attrs funs =
    BL8.unpack (encode (classInfoJsonValue64 classDecl ownerQName mainKind attrs funs))


-- Lower one IR function into segmented output:
--   [ X64Func(qname, signature, entry+firstBlock)
--   , X64Label ...
--   , X64Label ...
--   ]
-- First block is inlined into function body, remaining blocks are emitted as labels.
x64LowingFunc :: [String] -> IR.IRFunction -> X64LowerM ([X64.X64Segment], [ExternRef])
x64LowingFunc ownerQName fun@(IR.IRFunction _ funName funSig _ (first : blocks, _) _) = do
    entryInstrs <- x64FuncEntry64 fun
    (firstBlockInstrs, firstRefs) <- x64FirstBlock64 first
    (segments, blockRefs) <- x64LowingBlocks64 blocks
    let bodyInstrs = entryInstrs ++ firstBlockInstrs
        funQname = ownerQName ++ [funName]
        funSigPair = (TEnv.funReturn funSig, TEnv.funParams funSig)
        funSeg = X64.X64Func funQname funSigPair bodyInstrs
    return (funSeg : segments, blockRefs ++ firstRefs)
x64LowingFunc ownerQName (IR.IRFunction _ funName funSig _ ([], _) _) =
    let funQname = ownerQName ++ [funName]
        funSigPair = (TEnv.funReturn funSig, TEnv.funParams funSig)
        funSeg = X64.X64Func funQname funSigPair []
    in return ([funSeg], [])


-- Lower the entry block directly into function body instructions.
x64FirstBlock64 :: IR.IRBlock -> X64LowerM ([X64.Instruction], [ExternRef])
x64FirstBlock64 (IR.IRBlock (_, instrs)) = do
    stmtOuts <- mapM x64StmtLowing64 instrs
    let x64Instrs = concatMap fst stmtOuts
        refs = foldl' (\acc (_, rs) -> rs ++ acc) [] stmtOuts
    return (x64Instrs, refs)


mkStaticInitFlagAtom64 :: [String] -> X64LowerM X64.Atom
mkStaticInitFlagAtom64 ownerQName = do
    X64.Bss (ownerQName ++ [staticInitFlagName]) [AST.Int32T] <$> getRipRegM64


-- Entry segment for .clinit:
--   push rbp
--   mov  rbp, rsp
--   sub  rsp, frameSize
--   cmp  DWORD PTR owner..isInit[rip], 1
--   je   <ret-block>
--   mov  DWORD PTR owner..isInit[rip], 1
x64ClinitEntry64 :: [String] -> Int -> X64LowerM ([X64.Instruction], [ExternRef])
x64ClinitEntry64 ownerQName retLabel = do
    cc <- getCCM64
    bp <- getBpRegM64
    sp <- getSpRegM64
    frameBytes0 <- alignUp16 <$> getMaxOffM64
    let frameBytes = max frameBytes0 (minCallFrame64 cc)
    flagAtom <- mkStaticInitFlagAtom64 ownerQName
    let prologue = [X64.Push X64.B64, X64.Mov (X64.Reg bp X64.B64) (X64.Reg sp X64.B64) X64.B64]
        alloc = [X64.Sub (X64.Reg sp X64.B64) (X64.Imm frameBytes) X64.B64 | frameBytes > 0]
        guard = [
            X64.Cmp flagAtom (X64.Imm 1) X64.B32,
            X64.Je (Right retLabel),
            X64.Mov flagAtom (X64.Imm 1) X64.B32]
        instrs = concat [prologue, alloc, guard]
    return (instrs, instrExternRefs64 instrs)


lowerFun :: [String] -> Map FloatImmKey64 String -> IR.IRFunction -> X64LowerM ([X64.X64Segment], [ExternRef])
lowerFun ownerQName floatConstLblMap fun = do
    cc <- getCCM64
    let baseOff = 0
    let funName = case fun of
            IR.IRFunction _ name _ _ _ _ -> name
        stFun = (mkState64 fun baseOff cc) {
            stFloatConstLabelMap64 = floatConstLblMap,
            stOwnerQName64 = ownerQName,
            stCurFunName64 = Just funName,
            stInClinit64 = False
            }
    withLowerState64 stFun (x64LowingFunc ownerQName fun)


-- Lower class static initializer into one or more code segments.
-- First segment is always named ".clinit".
x64LowingClinit :: [String] -> IR.StaticInit -> X64LowerM ([X64.X64Segment], [ExternRef])
x64LowingClinit ownerQName (IR.StaticInit (blocks, retBid)) = do
    let retLabel = retBid
    (entryInstrs, entryRefs) <- x64ClinitEntry64 ownerQName retLabel
    case blocks of
        (first@(IR.IRBlock (firstBid, _)) : rest)
            | firstBid == retLabel -> do
                -- Keep return target block materialized as a label so `je .L<ret>` is always valid.
                (blockLabels, blockRefs) <- x64LowingBlocks64 (first : rest)
                let entrySeg = X64.X64Segement (ownerQName ++ [staticInitName]) entryInstrs
                return (entrySeg : blockLabels, blockRefs ++ entryRefs)
            | otherwise -> do
                (firstInstrs, firstRefs) <- x64FirstBlock64 first
                (blockLabels, blockRefs) <- x64LowingBlocks64 rest
                let entrySeg = X64.X64Segement (ownerQName ++ [staticInitName]) (entryInstrs ++ firstInstrs)
                return (entrySeg : blockLabels, blockRefs ++ firstRefs ++ entryRefs)
        [] -> do
            let entrySeg = X64.X64Segement (ownerQName ++ [staticInitName]) (entryInstrs ++ [X64.Leave, X64.Ret])
            return ([entrySeg], entryRefs)


-- Lower one IR class into x64 code segments:
--   1) class .clinit segments
--   2) all method segments
-- Return value keeps static data and text segments separated.
x64LowingClass :: [String] -> IR.IRClass -> X64LowerM (X64.X64Class, [ExternRef])
x64LowingClass pkgSegs (IR.IRClass decl className attrs sInit staticAtomTypes funs mainKind) = do
    cc <- getCCM64
    seqStart <- gets stFloatConstSeq64
    let baseOff = 0
        ownerQName = pkgSegs ++ [className]
        (floatConstLblMap, floatConstData, seqNext) = collectClassFloatConsts64 cc seqStart (IR.IRClass decl className attrs sInit staticAtomTypes funs mainKind)
        stClinit = (mkClinitState64 sInit staticAtomTypes baseOff cc) {
            stFloatConstLabelMap64 = floatConstLblMap,
            stOwnerQName64 = ownerQName,
            stCurFunName64 = Just staticInitName,
            stInClinit64 = True
            }
        classInfoAttrs = filter isStaticAttr64 attrs
        classInfoText = classInfoJsonText64 decl ownerQName mainKind classInfoAttrs funs
        classInfoDataLabel = mangleQName64 False ownerQName ++ "_infoData"
        classInfoData = X64.StaticData classInfoDataLabel (stringBytesData64 cc classInfoText)
        staticData = classInfoData : (staticLowing64 ownerQName attrs ++ floatConstData)
        classInfoSeg = X64.X64ClassInfo ownerQName classInfoDataLabel (length classInfoText)
    modify' (\s -> s { stFloatConstSeq64 = seqNext })
    (clinitSegs, clinitRefs) <- withLowerState64 stClinit (x64LowingClinit ownerQName sInit)
    funOuts <- mapM (lowerFun ownerQName floatConstLblMap) funs
    let funSegs = concatMap fst funOuts
        funRefs = foldl' (\acc (_, rs) -> rs ++ acc) [] funOuts
    return ((staticData, classInfoSeg : clinitSegs ++ funSegs), funRefs ++ clinitRefs)


classGlobalSymbols64 :: [String] -> IR.IRClass -> [ExternSym]
classGlobalSymbols64 pkgSegs (IR.IRClass _ className _ _ _ funs _) =
    let ownerQName = pkgSegs ++ [className]
        clinitSym = (ownerQName ++ [staticInitName], [AST.Void])
        funSyms =
            map
                (\(IR.IRFunction _ funName sig _ _ _) ->
                    (ownerQName ++ [funName], TEnv.funParams sig ++ [TEnv.funReturn sig]))
                funs
    in clinitSym : funSyms


classInfoSymbol64 :: [String] -> IR.IRClass -> [String]
classInfoSymbol64 pkgSegs (IR.IRClass _ className _ _ _ _ _) = pkgSegs ++ [className]


classMethodSymbols64 :: [String] -> IR.IRClass -> [ExternSym]
classMethodSymbols64 pkgSegs (IR.IRClass _ className _ _ _ funs _) =
    let ownerQName = pkgSegs ++ [className]
    in map
        (\(IR.IRFunction _ funName sig _ _ _) ->
            (ownerQName ++ [funName], TEnv.funParams sig ++ [TEnv.funReturn sig]))
        funs


classPrefix64 :: [String] -> IR.IRClass -> [String]
classPrefix64 pkgSegs (IR.IRClass _ className _ _ _ _ _) = pkgSegs ++ [className]


collectExternDeclsFromRefs64 :: [[String]] -> [ExternRef] -> [X64.X64Decl]
collectExternDeclsFromRefs64 classPrefixes refs =
    let prefixSet = Set.fromList classPrefixes
        externSet = Set.fromList [
            sym | (prefix, sym@(fullName, _)) <- refs,
            not (null fullName),
            prefix `Set.notMember` prefixSet]
    in map (uncurry X64.Extern) (Set.toList externSet)


initClassState64 :: X64.CallConv64 -> IR.IRClass -> StackLayoutState
initClassState64 cc (IR.IRClass _ _ _ sInit staticAtomTypes _ _) =
    mkClinitState64 sInit staticAtomTypes 0 cc


-- Lower whole IR program into one x64 program state.
-- Global symbols include each class .clinit and all method names.
-- Extern list is currently emitted as one empty declaration: Extern [].
x64LowingProgm :: IR.IRProgm -> X64LowerM X64.X64Program
x64LowingProgm (IR.IRProgm pkgSegs classes) = do
    let cc = X64.linuxCC64
    let loweredClasses = map (\cls -> evalState (x64LowingClass pkgSegs cls) (initClassState64 cc cls)) classes
    let staticData = concatMap (fst . fst) loweredClasses
    let textSegs = concatMap (snd . fst) loweredClasses
    let classRefs = foldl' (\acc (_, rs) -> rs ++ acc) [] loweredClasses
    let classPrefixes = map (classPrefix64 pkgSegs) classes
    let globalSymbols = concatMap (classGlobalSymbols64 pkgSegs) classes
    let classSymbols = map (classInfoSymbol64 pkgSegs) classes
    let globals =
            map (uncurry X64.Global) globalSymbols ++
            map X64.GlobalClassInfo classSymbols
    let externs = collectExternDeclsFromRefs64 classPrefixes classRefs
    let decls = globals ++ externs
    let prog = X64.X64Progm decls staticData textSegs
    return prog

