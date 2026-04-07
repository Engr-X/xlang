module X86Lowing.Lowing where

import Control.Monad.State.Strict (State, gets, modify', put)
import Data.HashSet (HashSet)
import Data.List (intercalate, scanl')
import Data.Map.Strict (Map)
import IR.TAC (IRAtom, IRFunction, IRInstr)
import Parse.SyntaxTree (Class)
import X86Lowing.ASM (Register)

import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import qualified IR.TAC as IR
import qualified Parse.SyntaxTree as AST
import qualified Semantic.TypeEnv as TEnv
import qualified X86Lowing.ASM as X86


-- Sign-extend op used before signed division.
-- `cdq`: eax -> edx:eax
-- `cqo`: rax -> rdx:rax
data SignExt64
    = SxCdq
    | SxCqo
    deriving (Eq, Show)


-- x86-64 calling/division convention.
data CallConv64 = CallConv64 {
    ccIRet :: Register,   -- integer/pointer return register
    ccFRet :: Register,   -- floating-point return register
    ccArgs :: [Register], -- integer argument registers (in order)
    ccArgN :: Int,        -- count of register-passed args
    ccTmpI :: Register,   -- integer scratch/temp register
    ccTmpF :: Register,   -- floating-point scratch/temp register
    ccSp :: Register,     -- stack pointer register
    ccBp :: Register,     -- frame/base pointer register
    ccSx32 :: SignExt64,  -- sign-extend op before 32-bit signed division
    ccSx64 :: SignExt64,  -- sign-extend op before 64-bit signed division
    ccDivLo :: Register,  -- dividend low register (also quotient output)
    ccDivHi :: Register,  -- dividend high register (also remainder output)
    ccDivQ :: Register,   -- quotient result register
    ccDivR :: Register    -- remainder result register
} deriving (Eq, Show)


mkCC64 :: Register -> Register -> [Register] -> CallConv64
mkCC64 iRetReg fRetReg argRegs =
    CallConv64 {
        ccIRet = iRetReg,
        ccFRet = fRetReg,
        ccArgs = argRegs,
        ccArgN = length argRegs,
        ccTmpI = X86.C,
        ccTmpF = X86.Xmm15,
        ccSp = X86.SP,
        ccBp = X86.BP,
        ccSx32 = SxCdq,
        ccSx64 = SxCqo,
        ccDivLo = X86.A,
        ccDivHi = X86.D,
        ccDivQ = X86.A,
        ccDivR = X86.D
    }


-- Windows x64 ABI
-- return: rax
-- float return: xmm0
-- params: rcx, rdx, r8, r9
winCC64 :: CallConv64
winCC64 = mkCC64 X86.A X86.Xmm0 [X86.C, X86.D, X86.R8, X86.R9]


-- Linux x86-64 SysV ABI
-- return: rax
-- float return: xmm0
-- params: rdi, rsi, rdx, rcx, r8, r9
linuxCC64 :: CallConv64
linuxCC64 = mkCC64 X86.A X86.Xmm0 [X86.DI, X86.SI, X86.D, X86.C, X86.R8, X86.R9]


-- macOS x86-64 ABI (SysV-style for integer args)
-- return: rax
-- float return: xmm0
-- params: rdi, rsi, rdx, rcx, r8, r9
macCC64 :: CallConv64
macCC64 = mkCC64 X86.A X86.Xmm0 [X86.DI, X86.SI, X86.D, X86.C, X86.R8, X86.R9]


sizeByClass64 :: Map Class Int
sizeByClass64 = Map.fromList [
    (AST.Int8T, 4),
    (AST.Int16T, 4),
    (AST.Int32T, 4),
    (AST.Int64T, 8),
    (AST.Float32T, 4),
    (AST.Float64T, 8),
    (AST.Float128T, 16),
    (AST.Bool, 4),
    (AST.Char, 4)]


bitsByClass64 :: Map Class X86.Bits
bitsByClass64 = Map.fromList [
    (AST.Int32T, X86.B32),
    (AST.Int64T, X86.B64),
    (AST.Int16T, X86.B16),
    (AST.Int8T, X86.B8L),
    (AST.Char, X86.B8L),
    (AST.Bool, X86.B8L)]


bitsWidth64 :: X86.Bits -> Int
bitsWidth64 X86.B8L = 8
bitsWidth64 X86.B8H = 8
bitsWidth64 X86.B16 = 16
bitsWidth64 X86.B32 = 32
bitsWidth64 X86.B64 = 64
bitsWidth64 X86.NN = error "bitsWidth64: NN has no integer width"


intRet32Classes64 :: HashSet Class
intRet32Classes64 = HashSet.fromList [AST.Int32T, AST.Int16T, AST.Int8T, AST.Bool, AST.Char]


intRet64Classes64 :: HashSet Class
intRet64Classes64 = HashSet.fromList [AST.Int64T]


isRefClass64 :: Class -> Bool
isRefClass64 (AST.Array _ _) = True
isRefClass64 (AST.Class _ _) = True
isRefClass64 _ = False


intAssignBits64 :: Class -> Maybe X86.Bits
intAssignBits64 AST.Int64T = Just X86.B64
intAssignBits64 cls | cls `HashSet.member` intRet32Classes64 = Just X86.B32
intAssignBits64 _ = Nothing


isCastIntClass64 :: Class -> Bool
isCastIntClass64 cls =
    cls `elem` [AST.Int8T, AST.Int16T, AST.Int32T, AST.Int64T, AST.Char]


castIntWidth64 :: Class -> Int
castIntWidth64 cls = case Map.lookup cls bitsByClass64 of
    Just bits -> bitsWidth64 bits
    Nothing -> error $ "castIntWidth64: unsupported integer cast class: " ++ show cls


classBits64 :: Class -> X86.Bits
classBits64 cls = case Map.lookup cls bitsByClass64 of
    Just bits -> bits
    Nothing -> error $ "classBits64: unsupported class in bits map: " ++ show cls


asByteDst64 :: X86.Atom -> X86.Atom
asByteDst64 (X86.Reg r _) = X86.Reg r X86.B8L
asByteDst64 (X86.Mem b i d) = X86.Mem b i d
asByteDst64 atom = error $ "asByteDst64: unsupported destination atom: " ++ show atom


asDwordDst64 :: X86.Atom -> X86.Atom
asDwordDst64 (X86.Reg r _) = X86.Reg r X86.B32
asDwordDst64 atom = atom


genIntCast64 ::
    X86.Atom ->
    Class ->
    Class ->
    X86.Atom ->
    X86LowerM [X86.Instruction]
genIntCast64 dstAtom fromC toC srcAtom = do
    tmp <- getTmpIRegM64
    let t64 = X86.Reg tmp X86.B64
        t32 = X86.Reg tmp X86.B32
        t16 = X86.Reg tmp X86.B16
        t8 = X86.Reg tmp X86.B8L

        loadIns = if fromC == AST.Int64T
            then X86.Mov t64 srcAtom
            else X86.Mov t32 srcAtom

        widen = castIntWidth64 toC > castIntWidth64 fromC

        widenIns
            | not widen = []
            | fromC == AST.Char = [X86.Movz t32 t8]
            | fromC == AST.Int8T = [X86.Movs t32 t8]
            | fromC == AST.Int16T = [X86.Movs t32 t16]
            | otherwise = []

        outSrc = if toC == AST.Int64T then t64 else t32
    return $ [loadIns] ++ widenIns ++ [X86.Mov dstAtom outSrc]


genIntToBoolCast64 ::
    X86.Atom ->
    Class ->
    X86.Atom ->
    X86LowerM [X86.Instruction]
genIntToBoolCast64 dstAtom fromC srcAtom = do
    tmp <- getTmpIRegM64
    let srcBits = classBits64 fromC
        srcReg = X86.Reg tmp srcBits
        dstByte = asByteDst64 dstAtom
    return [
        X86.Mov srcReg srcAtom,
        X86.Test srcReg srcReg,
        X86.Setne dstByte]


genBoolToIntCast64 ::
    X86.Atom ->
    Class ->
    X86.Atom ->
    X86LowerM [X86.Instruction]
genBoolToIntCast64 dstAtom toC srcAtom = do
    iRet <- getIRetRegM64
    let src8 = X86.Reg iRet X86.B8L
        dst32 = X86.Reg iRet X86.B32
        dst64 = X86.Reg iRet X86.B64
        pref = [X86.Mov src8 srcAtom, X86.Movz dst32 src8]
    case toC of
        AST.Int64T -> case dstAtom of
            X86.Reg r _ -> return $ pref ++ [X86.Mov (X86.Reg r X86.B32) dst32]
            _ -> return $ pref ++ [X86.Mov dstAtom dst64]
        _ | isCastIntClass64 toC -> return $ pref ++ [X86.Mov (asDwordDst64 dstAtom) dst32]
        _ -> error $ "genBoolToIntCast64: unsupported target class: " ++ show toC


genBoolToFloatCast64 ::
    X86.Atom ->
    Class ->
    X86.Atom ->
    X86LowerM [X86.Instruction]
genBoolToFloatCast64 dstAtom toC srcAtom = do
    iRet <- getIRetRegM64
    fTmp <- getTmpFRegM64
    let src8 = X86.Reg iRet X86.B8L
        src32 = X86.Reg iRet X86.B32
        xDst = X86.Reg fTmp X86.NN
        pref = [X86.Mov src8 srcAtom, X86.Movz src32 src8]
    case toC of
        AST.Float32T ->
            return $ pref ++ [
                X86.Xorps xDst xDst,
                X86.Cvtsi2ss xDst src32,
                X86.Movss dstAtom xDst]
        AST.Float64T ->
            return $ pref ++ [
                X86.Xorpd xDst xDst,
                X86.Cvtsi2sd xDst src32,
                X86.Movsd dstAtom xDst]
        _ ->
            error $ "genBoolToFloatCast64: unsupported target class: " ++ show toC


genFloatToBoolCast64 ::
    X86.Atom ->
    Class ->
    X86.Atom ->
    X86LowerM [X86.Instruction]
genFloatToBoolCast64 dstAtom fromC srcAtom = do
    tmpF <- getTmpFRegM64
    fRet <- getFRetRegM64
    tmpI <- getTmpIRegM64
    iRet <- getIRetRegM64
    let xSrc = X86.Reg tmpF X86.NN
        xZero = X86.Reg fRet X86.NN
        one32 = X86.Reg tmpI X86.B32
        out32 = X86.Reg iRet X86.B32
        out8 = X86.Reg iRet X86.B8L
        dst8 = asByteDst64 dstAtom

        (loadF, clearF, cmpF) = case fromC of
            AST.Float32T -> (X86.Movss xSrc srcAtom, X86.Xorps xZero xZero, X86.Ucomiss xSrc xZero)
            AST.Float64T -> (X86.Movsd xSrc srcAtom, X86.Xorpd xZero xZero, X86.Ucomisd xSrc xZero)
            _ -> error $ "genFloatToBoolCast64: unsupported source class: " ++ show fromC
    return [
        loadF,
        clearF,
        X86.Mov out32 (X86.Imm 0),
        X86.Mov one32 (X86.Imm 1),
        cmpF,
        X86.Setp out8,
        X86.Cmovne out32 one32,
        X86.Mov dst8 out8]


genFloatToIntCast64 ::
    X86.Atom ->
    Class ->
    Class ->
    X86.Atom ->
    X86LowerM [X86.Instruction]
genFloatToIntCast64 dstAtom fromC toC srcAtom = do
    tmp <- getTmpIRegM64
    let dst32 = X86.Reg tmp X86.B32
        dst64 = X86.Reg tmp X86.B64
    case (fromC, toC) of
        (AST.Float32T, AST.Int64T) ->
            return [X86.Cvttss2si dst64 srcAtom, X86.Mov dstAtom dst64]
        (AST.Float64T, AST.Int64T) ->
            return [X86.Cvttsd2si dst64 srcAtom, X86.Mov dstAtom dst64]
        (AST.Float32T, toInt)
            | isCastIntClass64 toInt ->
                return [X86.Cvttss2si dst32 srcAtom, X86.Mov (asDwordDst64 dstAtom) dst32]
        (AST.Float64T, toInt)
            | isCastIntClass64 toInt ->
                return [X86.Cvttsd2si dst32 srcAtom, X86.Mov (asDwordDst64 dstAtom) dst32]
        _ ->
            error $ "genFloatToIntCast64: unsupported cast: " ++ show (fromC, toC)


cppCallType64 :: Class -> String
cppCallType64 AST.Bool = "bool"
cppCallType64 AST.Char = "unsigned char"
cppCallType64 AST.Int8T = "char"
cppCallType64 AST.Int16T = "short"
cppCallType64 AST.Int32T = "int"
cppCallType64 AST.Int64T = "long"
cppCallType64 AST.Float32T = "float"
cppCallType64 AST.Float64T = "double"
cppCallType64 cls =
    error $ "cppCallType64: class/string are not supported in x86 static call yet: " ++ show cls


mkStaticCallName64 :: [String] -> [Class] -> String
mkStaticCallName64 qname argTs =
    let funName = intercalate "::" qname
        sig = intercalate ", " (map cppCallType64 argTs)
    in funName ++ "(" ++ sig ++ ")"


moveArgToReg64 :: Register -> X86.Atom -> Class -> X86LowerM [X86.Instruction]
moveArgToReg64 reg atom cls = case cls of
    AST.Float32T -> error "moveArgToReg64: float args are not supported in ICallStatic yet"
    AST.Float64T -> error "moveArgToReg64: double args are not supported in ICallStatic yet"
    _ -> case Map.lookup cls bitsByClass64 of
        Just bits -> return [X86.Mov (X86.Reg reg bits) atom]
        Nothing -> error $ "moveArgToReg64: unsupported arg class: " ++ show cls


-- 64-bit target byte size for a type.
classBytes64 :: Class -> Int
classBytes64 (AST.Array _ _) = 8
classBytes64 (AST.Class {}) = 8
classBytes64 cls = case Map.lookup cls sizeByClass64 of
    Just n -> n
    Nothing -> error $ "classBytes64: unknown class: " ++ show cls


-- Map from param index to positive stack offset.
paramOffsets64 :: IRFunction -> Int -> Map Int Int
paramOffsets64 (IR.IRFunction _ _ funSig _ _ _) baseOff =
    let start = max 0 baseOff
        sizes = map classBytes64 (TEnv.funParams funSig)
        offs = tail $ scanl' (+) start sizes
    in Map.fromList (zip [0 ..] offs)


-- Map from local atom to positive stack offset.
localOffsets64 :: IRFunction -> Int -> Map IRAtom Int
localOffsets64 (IR.IRFunction _ _ _ atomTypes _ _) baseOff =
    let start = max 0 baseOff
        locals = filter (isStackAtom . fst) (Map.toAscList atomTypes)
        atoms = map fst locals
        sizes = map (classBytes64 . snd) locals
        offs = tail $ scanl' (+) start sizes
    in Map.fromList (zip atoms offs)
    where
        isStackAtom :: IRAtom -> Bool
        isStackAtom (IR.Var _) = True
        isStackAtom (IR.Param _) = True
        isStackAtom _ = False


-- Params first, then locals.
-- Returns:
--   1) atom -> positive stack offset
--   2) final/max offset
stackLayout64 :: IRFunction -> Int -> (Map IRAtom Int, Int)
stackLayout64 (IR.IRFunction _ _ funSig atomTypes _ _) baseOff =
    let start = max 0 baseOff
        paramEnts = [(IR.Param i, classBytes64 cls) | (i, cls) <- zip [0 ..] (TEnv.funParams funSig)]
        localEnts = [(a, classBytes64 cls) | (a, cls) <- Map.toAscList atomTypes, isLocal a]
        ents = paramEnts ++ localEnts
        offs = tail $ scanl' (+) start (map snd ents)
        layout = Map.fromList (zip (map fst ents) offs)
        endOff = case offs of
            [] -> start
            _ -> last offs
    in (layout, endOff)
    where
        isLocal :: IRAtom -> Bool
        isLocal (IR.Var _) = True
        isLocal _ = False


-- Lowering state for one function.
data StackLayoutState = StackLayoutState {
    stOff64 :: Map IRAtom Int, -- atom -> positive stack offset
    stMaxOff64 :: Int,         -- max/final offset
    stCC64 :: CallConv64,      -- current calling convention
    stTypes64 :: Map IRAtom Class, -- atom -> type
    stDivWantRem64 :: Bool      -- False: quotient(/), True: remainder(%)
} deriving (Eq, Show)


atomTypes64 :: IRFunction -> Map IRAtom Class
atomTypes64 (IR.IRFunction _ _ funSig atomTypes _ _) =
    let paramTypes = Map.fromList [(IR.Param i, cls) | (i, cls) <- zip [0 ..] (TEnv.funParams funSig)]
    in Map.union atomTypes paramTypes


mkState64 :: IRFunction -> Int -> CallConv64 -> StackLayoutState
mkState64 fun baseOff cc =
    let (layout, endOff) = stackLayout64 fun baseOff
        tys = atomTypes64 fun
    in StackLayoutState {
        stOff64 = layout,
        stMaxOff64 = endOff,
        stCC64 = cc,
        stTypes64 = tys,
        stDivWantRem64 = False
    }


-- X86 lowering monad carrying stack/layout state.
type X86LowerM a = State StackLayoutState a


runX86LowerState64 :: IRFunction -> Int -> CallConv64 -> StackLayoutState
runX86LowerState64 = mkState64


getOffMapM64 :: X86LowerM (Map IRAtom Int)
getOffMapM64 = gets stOff64


getMaxOffM64 :: X86LowerM Int
getMaxOffM64 = gets stMaxOff64


getCCM64 :: X86LowerM CallConv64
getCCM64 = gets stCC64


getSpRegM64 :: X86LowerM Register
getSpRegM64 = gets (ccSp . stCC64)


getBpRegM64 :: X86LowerM Register
getBpRegM64 = gets (ccBp . stCC64)


getTmpIRegM64 :: X86LowerM Register
getTmpIRegM64 = gets (ccTmpI . stCC64)


getTmpFRegM64 :: X86LowerM Register
getTmpFRegM64 = gets (ccTmpF . stCC64)


getIRetRegM64 :: X86LowerM Register
getIRetRegM64 = gets (ccIRet . stCC64)


getFRetRegM64 :: X86LowerM Register
getFRetRegM64 = gets (ccFRet . stCC64)


getAtomOffM64 :: IRAtom -> X86LowerM Int
getAtomOffM64 a = do
    mOff <- gets (Map.lookup a . stOff64)
    case mOff of
        Just off -> return off
        Nothing -> error $ "getAtomOffM64: stack offset not found for atom: " ++ show a


putStateM64 :: StackLayoutState -> X86LowerM ()
putStateM64 = put


putCCM64 :: CallConv64 -> X86LowerM ()
putCCM64 cc = modify' (\s -> s { stCC64 = cc })


setDivWantRemM64 :: Bool -> X86LowerM ()
setDivWantRemM64 flag = modify' (\s -> s { stDivWantRem64 = flag })


getDivWantRemM64 :: X86LowerM Bool
getDivWantRemM64 = gets stDivWantRem64


blockName :: Int -> String
blockName idx = ".L" ++ show idx


-- Type query in X86 lowering state.
-- Constants return their fixed type directly.
-- Vars/Params are resolved from stTypes64.
atomTypeM64 :: IRAtom -> X86LowerM Class
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
atomTypeM64 (IR.Phi _) = error "getAtomTypeM64: phi should be stripped before x86 lowering"


lookupTyM64 :: IRAtom -> X86LowerM Class
lookupTyM64 atom = do
    mTy <- gets (Map.lookup atom . stTypes64)
    case mTy of
        Just ty -> return ty
        Nothing -> error $ "getAtomTypeM64: type not found for atom: " ++ show atom


getByteSize :: IRAtom -> X86LowerM Int
getByteSize atom = (\(_, _, sz) -> sz) <$> tySizeM64 atom


-- Lower one IR atom to an x86 operand.
-- Vars/params are materialized as stack memory operands: [rbp - offset].
atomAddrM64 :: IRAtom -> X86LowerM X86.Atom
atomAddrM64 atom = case atom of
    IR.Var _ -> stackMem
    IR.Param _ -> stackMem
    IR.BoolC b -> return (X86.Imm (if b then 1 else 0))
    IR.CharC c -> return (X86.Imm (fromEnum c))
    IR.Int8C i -> return (X86.Imm (fromIntegral i))
    IR.Int16C i -> return (X86.Imm (fromIntegral i))
    IR.Int32C i -> return (X86.Imm (fromIntegral i))
    IR.Int64C i -> return (X86.Imm (fromIntegral i))
    IR.Float32C _ -> error "atomAddrM64: float32 immediate is not supported yet"
    IR.Float64C _ -> error "atomAddrM64: float64 immediate is not supported yet"
    IR.Float128C _ -> error "atomAddrM64: float128 immediate is not supported yet"
    IR.StringC _ -> error "atomAddrM64: string immediate is not supported yet"
    IR.Phi _ -> error "atomAddrM64: phi should be stripped before x86 lowering"
    where
        stackMem :: X86LowerM X86.Atom
        stackMem = do
            bp <- getBpRegM64
            off <- getAtomOffM64 atom
            return (X86.Mem (Just bp) Nothing (negate off))


tySizeM64 :: IRAtom -> X86LowerM (X86.Atom, Class, Int)
tySizeM64 atom = do
    x86Atom <- atomAddrM64 atom
    cls <- atomTypeM64 atom
    return (x86Atom, cls, classBytes64 cls)


movMemToReg :: X86.Atom -> Class -> X86LowerM X86.Instruction
movMemToReg memAtom@(X86.Mem {}) cls =
    case cls of
        AST.Int64T -> movInt X86.B64
        AST.Int32T -> movInt X86.B32
        AST.Int16T -> movInt X86.B32
        AST.Int8T -> movInt X86.B32
        AST.Bool -> movInt X86.B32
        AST.Char -> movInt X86.B32
        AST.Float32T -> getTmpFRegM64 >>= \fTmp -> return (X86.Movss (X86.Reg fTmp X86.NN) memAtom)
        AST.Float64T -> getTmpFRegM64 >>= \fTmp -> return (X86.Movsd (X86.Reg fTmp X86.NN) memAtom)
        _ -> error $ "movMemToReg: unsupported class " ++ show cls
    where
        movInt :: X86.Bits -> X86LowerM X86.Instruction
        movInt bits = getTmpIRegM64 >>= \iTmp -> return (X86.Mov (X86.Reg iTmp bits) memAtom)
movMemToReg atom _ =
    error $ "movMemToReg: expected memory atom, got: " ++ show atom


isIntLikeCmpClass64 :: Class -> Bool
isIntLikeCmpClass64 cls =
    cls `elem` [AST.Int8T, AST.Int16T, AST.Int32T, AST.Int64T, AST.Bool, AST.Char]


loadAtom2ToTempIfMem64 :: X86.Atom -> Class -> X86LowerM ([X86.Instruction], X86.Atom)
loadAtom2ToTempIfMem64 rhs@(X86.Mem {}) cls = do
    movInstr <- movMemToReg rhs cls
    return ([movInstr], moveDst64 movInstr)
loadAtom2ToTempIfMem64 rhs _ = return ([], rhs)


moveDst64 :: X86.Instruction -> X86.Atom
moveDst64 (X86.Mov dst _) = dst
moveDst64 (X86.Movss dst _) = dst
moveDst64 (X86.Movsd dst _) = dst
moveDst64 _ = error "moveDst64: unexpected move instruction"


normalizeFloatCmpOrder64 :: X86.Atom -> X86.Atom -> (X86.Atom, X86.Atom)
normalizeFloatCmpOrder64 lhs@(X86.Reg _ _) rhs = (lhs, rhs)
normalizeFloatCmpOrder64 lhs rhs@(X86.Reg _ _) = (rhs, lhs)
normalizeFloatCmpOrder64 _ _ = error "genIfCmp64: float/double compare requires at least one register operand"


genIfCmp64 ::
    (String -> X86.Instruction) ->
    (String -> String -> [X86.Instruction]) ->
    IRAtom ->
    IRAtom ->
    (Int, Int) ->
    X86LowerM [X86.Instruction]
genIfCmp64 intJump floatJumps atom1 atom2 (thenBlock, elseBlock) = do
    (xAtom1, class1, _) <- tySizeM64 atom1
    (xAtom2, class2, _) <- tySizeM64 atom2
    if class1 /= class2
        then error $ "genIfCmp64: type mismatch: " ++ show (class1, class2)
        else do
            (pref, rhsReady) <- loadAtom2ToTempIfMem64 xAtom2 class1
            let thenLbl = blockName thenBlock
                elseLbl = blockName elseBlock
            case class1 of
                cls | isIntLikeCmpClass64 cls ->
                    return $ pref ++ [
                        X86.Cmp xAtom1 rhsReady,
                        intJump thenLbl,
                        X86.Jump elseLbl]
                AST.Float32T ->
                    let (lhsF, rhsF) = normalizeFloatCmpOrder64 xAtom1 rhsReady
                    in return $ concat [pref, [X86.Ucomiss lhsF rhsF], floatJumps thenLbl elseLbl]
                AST.Float64T ->
                    let (lhsD, rhsD) = normalizeFloatCmpOrder64 xAtom1 rhsReady
                    in return $ concat [pref, [X86.Ucomisd lhsD rhsD], floatJumps thenLbl elseLbl]
                _ ->
                    error $ "genIfCmp64: unsupported compare type: " ++ show class1


genIfRel64 :: IRInstr -> X86LowerM [X86.Instruction]
genIfRel64 (IR.Ifeq atom1 atom2 labels) = genIfCmp64 X86.Je (\t f -> [X86.Jp f, X86.Je t, X86.Jump f]) atom1 atom2 labels
genIfRel64 (IR.Ifne atom1 atom2 labels) = genIfCmp64 X86.Jne (\t f -> [X86.Jp t, X86.Jne t, X86.Jump f]) atom1 atom2 labels
genIfRel64 (IR.Iflt atom1 atom2 labels) = genIfCmp64 X86.Jl (\t f -> [X86.Jp f, X86.Jb t, X86.Jump f]) atom1 atom2 labels
genIfRel64 (IR.Ifle atom1 atom2 labels) = genIfCmp64 X86.Jle (\t f -> [X86.Jp f, X86.Jbe t, X86.Jump f]) atom1 atom2 labels
genIfRel64 (IR.Ifgt atom1 atom2 labels) = genIfCmp64 X86.Jg (\t f -> [X86.Jp f, X86.Ja t, X86.Jump f]) atom1 atom2 labels
genIfRel64 (IR.Ifge atom1 atom2 labels) = genIfCmp64 X86.Jge (\t f -> [X86.Jp f, X86.Jae t, X86.Jump f]) atom1 atom2 labels
genIfRel64 _ = error "genIfRel64: expected relational if instruction"


unaryIntBits64 :: Class -> Class -> AST.Operator -> X86LowerM X86.Bits
unaryIntBits64 dstCls srcCls op
    | dstCls /= srcCls =
        error $ "x86StmtLowing64(IUnary " ++ AST.prettyOp op ++ "): type mismatch: " ++ show (dstCls, srcCls)
    | dstCls == AST.Int64T = return X86.B64
    | dstCls == AST.Int32T = return X86.B32
    | otherwise =
        error $ "x86StmtLowing64(IUnary " ++ AST.prettyOp op ++ "): only int/long are supported now, got: " ++ show dstCls


movLike64 :: X86.Bits -> X86.Atom -> X86.Atom -> X86LowerM [X86.Instruction]
movLike64 bits dst src = case (dst, src) of
    (X86.Mem {}, X86.Mem {}) -> do
        tmp <- getTmpIRegM64
        let tmpAtom = X86.Reg tmp bits
        return [X86.Mov tmpAtom src, X86.Mov dst tmpAtom]
    _ ->
        return [X86.Mov dst src]


movLikeF32 :: X86.Atom -> X86.Atom -> X86LowerM [X86.Instruction]
movLikeF32 dst src = case (dst, src) of
    (X86.Mem {}, X86.Mem {}) -> do
        tmp <- getTmpFRegM64
        let tmpAtom = X86.Reg tmp X86.NN
        return [X86.Movss tmpAtom src, X86.Movss dst tmpAtom]
    _ ->
        return [X86.Movss dst src]


movLikeF64 :: X86.Atom -> X86.Atom -> X86LowerM [X86.Instruction]
movLikeF64 dst src = case (dst, src) of
    (X86.Mem {}, X86.Mem {}) -> do
        tmp <- getTmpFRegM64
        let tmpAtom = X86.Reg tmp X86.NN
        return [X86.Movsd tmpAtom src, X86.Movsd dst tmpAtom]
    _ ->
        return [X86.Movsd dst src]


genFloatBin32 ::
    (X86.Atom -> X86.Atom -> X86.Instruction) ->
    X86.Atom ->
    X86.Atom ->
    X86.Atom ->
    X86LowerM [X86.Instruction]
genFloatBin32 mkOp dst lhs rhs = do
    rhsReg <- getTmpFRegM64
    accReg <- getFRetRegM64
    let rhsAtom = X86.Reg rhsReg X86.NN
        accAtom = X86.Reg accReg X86.NN
    return [X86.Movss rhsAtom rhs, X86.Movss accAtom lhs, mkOp accAtom rhsAtom, X86.Movss dst accAtom]


genFloatBin64 ::
    (X86.Atom -> X86.Atom -> X86.Instruction) ->
    X86.Atom ->
    X86.Atom ->
    X86.Atom ->
    X86LowerM [X86.Instruction]
genFloatBin64 mkOp dst lhs rhs = do
    rhsReg <- getTmpFRegM64
    accReg <- getFRetRegM64
    let rhsAtom = X86.Reg rhsReg X86.NN
        accAtom = X86.Reg accReg X86.NN
    return [X86.Movsd rhsAtom rhs, X86.Movsd accAtom lhs, mkOp accAtom rhsAtom, X86.Movsd dst accAtom]


powCallName64 :: String
powCallName64 = "xlang::math::pow(double,double)"


genPowCallF64 :: X86.Atom -> X86.Atom -> X86.Atom -> X86LowerM [X86.Instruction]
genPowCallF64 dst lhs rhs = do
    retReg <- getFRetRegM64
    let arg0 = X86.Reg X86.Xmm0 X86.NN
        arg1 = X86.Reg X86.Xmm1 X86.NN
        retAtom = X86.Reg retReg X86.NN
    return [
        X86.Movsd arg0 lhs,
        X86.Movsd arg1 rhs,
        X86.Call powCallName64,
        X86.Movsd dst retAtom
        ]


binaryIntBits64 :: Class -> Class -> Class -> AST.Operator -> X86LowerM X86.Bits
binaryIntBits64 dstCls lhsCls rhsCls op
    | dstCls /= lhsCls || lhsCls /= rhsCls =
        error $ concat [
            "x86StmtLowing64(IBinary ", AST.prettyOp op, "): type mismatch: ",
            show (dstCls, lhsCls, rhsCls)]
    | dstCls == AST.Int64T = return X86.B64
    | dstCls `HashSet.member` intRet32Classes64 = return X86.B32
    | otherwise =
        error $ concat [
            "x86StmtLowing64(IBinary ", AST.prettyOp op,
            "): only int-like types are supported now, got: ", show dstCls]


genIntBinSimple64 ::
    X86.Bits ->
    (X86.Atom -> X86.Atom -> X86.Instruction) ->
    X86.Atom ->
    X86.Atom ->
    X86.Atom ->
    X86LowerM [X86.Instruction]
genIntBinSimple64 bits mkOp dst lhs rhs = do
    rhsReg <- getTmpIRegM64
    accReg <- getIRetRegM64
    let rhsAtom = X86.Reg rhsReg bits
        accAtom = X86.Reg accReg bits
    return [X86.Mov rhsAtom rhs, X86.Mov accAtom lhs, mkOp accAtom rhsAtom, X86.Mov dst accAtom]


genIntMul64 ::
    X86.Bits ->
    X86.Atom ->
    X86.Atom ->
    X86.Atom ->
    X86LowerM [X86.Instruction]
genIntMul64 bits dst lhs rhs = do
    rhsReg <- getTmpIRegM64
    accReg <- getIRetRegM64
    let rhsAtom = X86.Reg rhsReg bits
        accAtom = X86.Reg accReg bits
    return [X86.Mov rhsAtom rhs, X86.Mov accAtom lhs, X86.IMul accAtom rhsAtom, X86.Mov dst accAtom]


genIntShift64 ::
    X86.Bits ->
    (X86.Atom -> X86.Atom -> X86.Instruction) ->
    X86.Atom ->
    X86.Atom ->
    X86.Atom ->
    X86LowerM [X86.Instruction]
genIntShift64 bits mkOp dst lhs rhs = do
    accReg <- getIRetRegM64
    let countWide = X86.Reg X86.C bits
        count8 = X86.Reg X86.C X86.B8L
        accAtom = X86.Reg accReg bits
    return [X86.Mov countWide rhs, X86.Mov accAtom lhs, mkOp accAtom count8, X86.Mov dst accAtom]


genIntIDiv64 ::
    X86.Bits ->
    X86.Atom ->
    X86.Atom ->
    X86.Atom ->
    X86LowerM [X86.Instruction]
genIntIDiv64 bits dst lhs rhs = do
    cc <- getCCM64
    wantRem <- getDivWantRemM64
    let lo = X86.Reg (ccDivLo cc) bits
        out = if wantRem
            then X86.Reg (ccDivR cc) bits
            else X86.Reg (ccDivQ cc) bits
        signExt = case bits of
            X86.B32 -> case ccSx32 cc of
                SxCdq -> X86.Cdq
                SxCqo -> error "genIntIDiv64: invalid sign-extend op for 32-bit division"
            X86.B64 -> case ccSx64 cc of
                SxCqo -> X86.Cqo
                SxCdq -> error "genIntIDiv64: invalid sign-extend op for 64-bit division"
            _ -> error "genIntIDiv64: invalid bits"

        -- signed division uses:
        --   32-bit: edx:eax / r/m32
        --   64-bit: rdx:rax / r/m64
        build :: X86.Atom -> [X86.Instruction]
        build divisor = [
            X86.Mov lo lhs,
            signExt,
            X86.IDiv lo divisor,
            X86.Mov dst out]
    rhsReg <- getTmpIRegM64
    let rhsReady = X86.Reg rhsReg bits
    return $ X86.Mov rhsReady rhs : build rhsReady


loadBinaryOperands64 ::
    IRAtom ->
    IRAtom ->
    IRAtom ->
    AST.Operator ->
    X86LowerM (X86.Atom, Class, X86.Atom, X86.Atom)
loadBinaryOperands64 dst lhs rhs op = do
    (dstAtom, dstCls, _) <- tySizeM64 dst
    (lhsAtom, lhsCls, _) <- tySizeM64 lhs
    (rhsAtom, rhsCls, _) <- tySizeM64 rhs
    if dstCls /= lhsCls || lhsCls /= rhsCls
        then error $ concat [
            "x86StmtLowing64(IBinary ", AST.prettyOp op, "): type mismatch: ",
            show (dstCls, lhsCls, rhsCls)]
        else return (dstAtom, dstCls, lhsAtom, rhsAtom)


x86StmtLowing64 :: IRInstr -> X86LowerM [X86.Instruction]
x86StmtLowing64 (IR.Jump label) = return [X86.Jump $ blockName label]
x86StmtLowing64 instr@(IR.Ifeq {}) = genIfRel64 instr
x86StmtLowing64 instr@(IR.Ifne {}) = genIfRel64 instr
x86StmtLowing64 instr@(IR.Iflt {}) = genIfRel64 instr
x86StmtLowing64 instr@(IR.Ifge {}) = genIfRel64 instr
x86StmtLowing64 instr@(IR.Ifgt {}) = genIfRel64 instr
x86StmtLowing64 instr@(IR.Ifle {}) = genIfRel64 instr

x86StmtLowing64 (IR.SetRet atom) = do
    (srcAtom, cls, _) <- tySizeM64 atom
    iRet <- getIRetRegM64
    fRet <- getFRetRegM64
    if cls `HashSet.member` intRet64Classes64 || isRefClass64 cls
        then return [X86.Mov (X86.Reg iRet X86.B64) srcAtom]
        else if cls `HashSet.member` intRet32Classes64
            then return [X86.Mov (X86.Reg iRet X86.B32) srcAtom]
            else case cls of
                AST.Float32T -> return [X86.Movss (X86.Reg fRet X86.NN) srcAtom]
                AST.Float64T -> return [X86.Movsd (X86.Reg fRet X86.NN) srcAtom]
                _ -> error $ "x86StmtLowing64(SetRet): unsupported return class " ++ show cls

x86StmtLowing64 IR.Return = return [X86.Ret]
x86StmtLowing64 IR.VReturn = return [X86.Ret]

x86StmtLowing64 (IR.IAssign dst src) = do
    (dstAtom, dstCls, _) <- tySizeM64 dst
    (srcAtom, srcCls, _) <- tySizeM64 src
    if dstCls /= srcCls
        then error $ "x86StmtLowing64(IAssign): type mismatch: " ++ show (dstCls, srcCls)
        else case intAssignBits64 srcCls of
            Just bits -> do
                tmp <- getTmpIRegM64
                let tmpAtom = X86.Reg tmp bits
                return [X86.Mov tmpAtom srcAtom, X86.Mov dstAtom tmpAtom]
            Nothing -> case srcCls of
                AST.Float32T -> do
                    tmpF <- getTmpFRegM64
                    let tmpAtom = X86.Reg tmpF X86.NN
                    return [X86.Movss tmpAtom srcAtom, X86.Movss dstAtom tmpAtom]
                AST.Float64T -> do
                    tmpF <- getTmpFRegM64
                    let tmpAtom = X86.Reg tmpF X86.NN
                    return [X86.Movsd tmpAtom srcAtom, X86.Movsd dstAtom tmpAtom]
                _ ->
                    error $ "x86StmtLowing64(IAssign): unsupported assignment class: " ++ show srcCls

x86StmtLowing64 (IR.IUnary dst AST.UnaryPlus src) = do
    (dstAtom, dstCls, _) <- tySizeM64 dst
    (srcAtom, srcCls, _) <- tySizeM64 src
    if dstCls /= srcCls
        then error $ "x86StmtLowing64(IUnary " ++ AST.prettyOp AST.UnaryPlus ++ "): type mismatch: " ++ show (dstCls, srcCls)
        else case dstCls of
            AST.Int32T -> movLike64 X86.B32 dstAtom srcAtom
            AST.Int64T -> movLike64 X86.B64 dstAtom srcAtom
            AST.Float32T -> movLikeF32 dstAtom srcAtom
            AST.Float64T -> movLikeF64 dstAtom srcAtom
            _ ->
                error $ "x86StmtLowing64(IUnary " ++ AST.prettyOp AST.UnaryPlus ++ "): only int/long/float/double are supported now, got: " ++ show dstCls

x86StmtLowing64 (IR.IUnary dst AST.UnaryMinus src) = do
    (dstAtom, dstCls, _) <- tySizeM64 dst
    (srcAtom, srcCls, _) <- tySizeM64 src
    if dstCls /= srcCls
        then error $ concat ["x86StmtLowing64(IUnary ", AST.prettyOp AST.UnaryMinus, "): type mismatch: ", show (dstCls, srcCls)]
        else case dstCls of
            AST.Int64T -> do
                tmp <- getTmpIRegM64
                let tmpAtom = X86.Reg tmp X86.B64
                return [X86.Mov tmpAtom srcAtom, X86.Neg tmpAtom, X86.Mov dstAtom tmpAtom]
            AST.Int32T -> do
                tmp <- getTmpIRegM64
                let tmpAtom = X86.Reg tmp X86.B32
                return [X86.Mov tmpAtom srcAtom, X86.Neg tmpAtom, X86.Mov dstAtom tmpAtom]
            AST.Float32T -> do
                tmpI <- getTmpIRegM64
                tmpF <- getTmpFRegM64
                let iTmp = X86.Reg tmpI X86.B32
                    fTmp = X86.Reg tmpF X86.NN
                -- tmp = -1.0f via int->float conversion; dst = src * tmp
                return [
                    X86.Mov iTmp (X86.Imm (-1)),
                    X86.Cvtsi2ss fTmp iTmp,
                    X86.Mulss fTmp srcAtom,
                    X86.Movss dstAtom fTmp]
            AST.Float64T -> do
                tmpI <- getTmpIRegM64
                tmpF <- getTmpFRegM64
                let iTmp = X86.Reg tmpI X86.B64
                    fTmp = X86.Reg tmpF X86.NN
                -- tmp = -1.0 via int->double conversion; dst = src * tmp
                return [
                    X86.Mov iTmp (X86.Imm (-1)),
                    X86.Cvtsi2sd fTmp iTmp,
                    X86.Mulsd fTmp srcAtom,
                    X86.Movsd dstAtom fTmp]
            _ ->
                error $ concat ["x86StmtLowing64(IUnary ", AST.prettyOp AST.UnaryMinus, "): only int/long/float/double are supported now, got: ", show dstCls]

x86StmtLowing64 (IR.IUnary dst AST.LogicalNot src) = do
    (dstAtom, dstCls, _) <- tySizeM64 dst
    (srcAtom, srcCls, _) <- tySizeM64 src
    if dstCls /= AST.Bool || srcCls /= AST.Bool
        then error $ "x86StmtLowing64(LogicalNot): only bool is supported, got: " ++ show (dstCls, srcCls)
        else do
            pref <- movLike64 X86.B32 dstAtom srcAtom
            return $ pref ++ [X86.Xor dstAtom (X86.Imm 1)]

x86StmtLowing64 (IR.IUnary dst AST.BitInv src) = do
    (dstAtom, dstCls, _) <- tySizeM64 dst
    (srcAtom, srcCls, _) <- tySizeM64 src
    bits <- unaryIntBits64 dstCls srcCls AST.BitInv
    tmp <- getTmpIRegM64
    let tmpAtom = X86.Reg tmp bits
    return [X86.Mov tmpAtom srcAtom, X86.Not tmpAtom, X86.Mov dstAtom tmpAtom]

x86StmtLowing64 (IR.IUnary _ op _) =
    error $ "x86StmtLowing64(IUnary): unsupported unary operator: " ++ show op


x86StmtLowing64 (IR.IBinary dst AST.Add lhs rhs) = do
    (dstAtom, dstCls, lhsAtom, rhsAtom) <- loadBinaryOperands64 dst lhs rhs AST.Add
    case dstCls of
        AST.Float32T -> genFloatBin32 X86.Addss dstAtom lhsAtom rhsAtom
        AST.Float64T -> genFloatBin64 X86.Addsd dstAtom lhsAtom rhsAtom
        _ -> do
            bits <- binaryIntBits64 dstCls dstCls dstCls AST.Add
            genIntBinSimple64 bits X86.Add dstAtom lhsAtom rhsAtom

x86StmtLowing64 (IR.IBinary dst AST.Sub lhs rhs) = do
    (dstAtom, dstCls, lhsAtom, rhsAtom) <- loadBinaryOperands64 dst lhs rhs AST.Sub
    case dstCls of
        AST.Float32T -> genFloatBin32 X86.Subss dstAtom lhsAtom rhsAtom
        AST.Float64T -> genFloatBin64 X86.Subsd dstAtom lhsAtom rhsAtom
        _ -> do
            bits <- binaryIntBits64 dstCls dstCls dstCls AST.Sub
            genIntBinSimple64 bits X86.Sub dstAtom lhsAtom rhsAtom

x86StmtLowing64 (IR.IBinary dst AST.Mul lhs rhs) = do
    (dstAtom, dstCls, lhsAtom, rhsAtom) <- loadBinaryOperands64 dst lhs rhs AST.Mul
    case dstCls of
        AST.Float32T -> genFloatBin32 X86.Mulss dstAtom lhsAtom rhsAtom
        AST.Float64T -> genFloatBin64 X86.Mulsd dstAtom lhsAtom rhsAtom
        _ -> do
            bits <- binaryIntBits64 dstCls dstCls dstCls AST.Mul
            genIntMul64 bits dstAtom lhsAtom rhsAtom

x86StmtLowing64 (IR.IBinary dst AST.Div lhs rhs) = do
    (dstAtom, dstCls, lhsAtom, rhsAtom) <- loadBinaryOperands64 dst lhs rhs AST.Div
    case dstCls of
        AST.Float32T -> genFloatBin32 X86.Divss dstAtom lhsAtom rhsAtom
        AST.Float64T -> genFloatBin64 X86.Divsd dstAtom lhsAtom rhsAtom
        _ -> do
            bits <- binaryIntBits64 dstCls dstCls dstCls AST.Div
            setDivWantRemM64 False
            genIntIDiv64 bits dstAtom lhsAtom rhsAtom

x86StmtLowing64 (IR.IBinary dst AST.Pow lhs rhs) = do
    (dstAtom, dstCls, lhsAtom, rhsAtom) <- loadBinaryOperands64 dst lhs rhs AST.Pow
    case dstCls of
        AST.Float64T -> genPowCallF64 dstAtom lhsAtom rhsAtom
        AST.Float32T ->
            error "x86StmtLowing64(IBinary **): float32 pow should be promoted to double before x86 lowering"
        _ ->
            error "x86StmtLowing64(IBinary **): only double pow is supported; non-double pow should be promoted before lowering"

x86StmtLowing64 (IR.IBinary dst AST.Mod lhs rhs) = do
    (dstAtom, dstCls, lhsAtom, rhsAtom) <- loadBinaryOperands64 dst lhs rhs AST.Mod
    bits <- binaryIntBits64 dstCls dstCls dstCls AST.Mod
    setDivWantRemM64 True
    genIntIDiv64 bits dstAtom lhsAtom rhsAtom

x86StmtLowing64 (IR.IBinary dst AST.BitAnd lhs rhs) = do
    (dstAtom, dstCls, lhsAtom, rhsAtom) <- loadBinaryOperands64 dst lhs rhs AST.BitAnd
    bits <- binaryIntBits64 dstCls dstCls dstCls AST.BitAnd
    genIntBinSimple64 bits X86.And dstAtom lhsAtom rhsAtom

x86StmtLowing64 (IR.IBinary dst AST.BitXor lhs rhs) = do
    (dstAtom, dstCls, lhsAtom, rhsAtom) <- loadBinaryOperands64 dst lhs rhs AST.BitXor
    bits <- binaryIntBits64 dstCls dstCls dstCls AST.BitXor
    genIntBinSimple64 bits X86.Xor dstAtom lhsAtom rhsAtom

x86StmtLowing64 (IR.IBinary dst AST.BitOr lhs rhs) = do
    (dstAtom, dstCls, lhsAtom, rhsAtom) <- loadBinaryOperands64 dst lhs rhs AST.BitOr
    bits <- binaryIntBits64 dstCls dstCls dstCls AST.BitOr
    genIntBinSimple64 bits X86.Or dstAtom lhsAtom rhsAtom

x86StmtLowing64 (IR.IBinary dst AST.BitLShift lhs rhs) = do
    (dstAtom, dstCls, lhsAtom, rhsAtom) <- loadBinaryOperands64 dst lhs rhs AST.BitLShift
    bits <- binaryIntBits64 dstCls dstCls dstCls AST.BitLShift
    genIntShift64 bits X86.Shl dstAtom lhsAtom rhsAtom

x86StmtLowing64 (IR.IBinary dst AST.BitRShift lhs rhs) = do
    (dstAtom, dstCls, lhsAtom, rhsAtom) <- loadBinaryOperands64 dst lhs rhs AST.BitRShift
    bits <- binaryIntBits64 dstCls dstCls dstCls AST.BitRShift
    genIntShift64 bits X86.Sar dstAtom lhsAtom rhsAtom

x86StmtLowing64 (IR.IBinary dst AST.BitURShift lhs rhs) = do
    (dstAtom, dstCls, lhsAtom, rhsAtom) <- loadBinaryOperands64 dst lhs rhs AST.BitURShift
    bits <- binaryIntBits64 dstCls dstCls dstCls AST.BitURShift
    genIntShift64 bits X86.Shr dstAtom lhsAtom rhsAtom

x86StmtLowing64 (IR.IBinary _ op _ _) =
    error $ "x86StmtLowing64(IBinary): unsupported operator: " ++ show op

x86StmtLowing64 (IR.ICast dst (fromC, toC) src) = do
    (dstAtom, dstCls, _) <- tySizeM64 dst
    (srcAtom, srcCls, _) <- tySizeM64 src
    if srcCls /= fromC || dstCls /= toC
        then error $ "x86StmtLowing64(ICast): type mismatch between IR annotation and atom types: " ++ show (fromC, toC, srcCls, dstCls)
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
            (fromF, toInt)
                | fromF `elem` [AST.Float32T, AST.Float64T] && isCastIntClass64 toInt ->
                    genFloatToIntCast64 dstAtom fromF toInt srcAtom
            (fromInt, toInt)
                | isCastIntClass64 fromInt && isCastIntClass64 toInt ->
                    genIntCast64 dstAtom fromInt toInt srcAtom
            _ ->
                error $ "x86StmtLowing64(ICast): only int/char casts, bool<->int, bool<->float/double, and float/double->int casts are supported now, got: " ++ show (fromC, toC)

x86StmtLowing64 (IR.ICall {}) = error "TODO"
x86StmtLowing64 (IR.ICallStatic dst qname args) = do
    (dstAtom, dstCls, _) <- tySizeM64 dst
    argTriples <- mapM tySizeM64 args
    cc <- getCCM64
    iRet <- getIRetRegM64
    fRet <- getFRetRegM64

    let argAtoms = map (\(a, _, _) -> a) argTriples
        argClasses = map (\(_, c, _) -> c) argTriples
        argRegs = ccArgs cc
        callName = mkStaticCallName64 qname argClasses

    if length argAtoms > length argRegs
        then error $ "x86StmtLowing64(ICallStatic): stack args are not supported yet, got " ++ show (length argAtoms) ++ " args"
        else do
            argInstrs <- concat <$> mapM (\(reg, (atom, cls)) -> moveArgToReg64 reg atom cls)
                    (zip argRegs (zip argAtoms argClasses))

            let retInstrs = case dstCls of
                    AST.Void -> []
                    AST.Float32T -> [X86.Movss dstAtom (X86.Reg fRet X86.NN)]
                    AST.Float64T -> [X86.Movsd dstAtom (X86.Reg fRet X86.NN)]
                    cls | isRefClass64 cls -> [X86.Mov dstAtom (X86.Reg iRet X86.B64)]
                    cls -> case Map.lookup cls bitsByClass64 of
                        Just bits -> [X86.Mov dstAtom (X86.Reg iRet bits)]
                        Nothing -> error $ "x86StmtLowing64(ICallStatic): unsupported return class: " ++ show cls

            return $ argInstrs ++ [X86.Call callName] ++ retInstrs

x86StmtLowing64 (IR.IGetField {}) = error "TODO"
x86StmtLowing64 (IR.IPutField {}) = error "TODO"

x86StmtLowing64 (IR.IGetStatic _ _) = error "TODO"
x86StmtLowing64 (IR.IPutStatic _ _) = error "TODO"
