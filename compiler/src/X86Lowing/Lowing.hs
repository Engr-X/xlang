module X86Lowing.Lowing where

import Control.Monad.State.Strict (State, gets, modify', put)
import Data.List (scanl')
import Data.Map.Strict (Map)
import IR.TAC (IRAtom, IRFunction, IRInstr)
import Parse.SyntaxTree (Class)
import X86Lowing.ASM (Register)

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
    stTypes64 :: Map IRAtom Class -- atom -> type
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
        stTypes64 = tys
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

x86StmtLowing64 :: IRInstr -> X86LowerM [X86.Instruction]
x86StmtLowing64 (IR.Jump label) = return [X86.Jump $ blockName label]
x86StmtLowing64 instr@(IR.Ifeq {}) = genIfRel64 instr
x86StmtLowing64 instr@(IR.Ifne {}) = genIfRel64 instr
x86StmtLowing64 instr@(IR.Iflt {}) = genIfRel64 instr
x86StmtLowing64 instr@(IR.Ifge {}) = genIfRel64 instr
x86StmtLowing64 instr@(IR.Ifgt {}) = genIfRel64 instr
x86StmtLowing64 instr@(IR.Ifle {}) = genIfRel64 instr

x86StmtLowing64 (IR.SetRet _) = error "TODO"
x86StmtLowing64 IR.Return = return [X86.Ret]
x86StmtLowing64 IR.VReturn = return [X86.Ret]
x86StmtLowing64 (IR.IAssign _ _) = error "TODO"
x86StmtLowing64 (IR.IUnary {}) = error "TODO"
x86StmtLowing64 (IR.IBinary {}) = error "TODO"

x86StmtLowing64 (IR.ICast {}) = error "TODO"
x86StmtLowing64 (IR.ICall {}) = error "TODO"
x86StmtLowing64 (IR.ICallStatic {}) = error "TODO"

x86StmtLowing64 (IR.IGetField {}) = error "TODO"
x86StmtLowing64 (IR.IPutField {}) = error "TODO"

x86StmtLowing64 (IR.IGetStatic _ _) = error "TODO"
x86StmtLowing64 (IR.IPutStatic _ _) = error "TODO"
