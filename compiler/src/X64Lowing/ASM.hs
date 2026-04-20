module X64Lowing.ASM where

import Data.HashSet (HashSet)
import Data.List (intercalate, partition)
import Parse.SyntaxTree (Class)
import Util.Basic (insertTab, mangleName)

import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import qualified Parse.SyntaxTree as AST


data Register =
    A | B | C | D |
    SI | DI | SP | BP | RIP |
    R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 |
    Xmm0 | Xmm1 | Xmm2 | Xmm3 | Xmm4 | Xmm5 | Xmm6 | Xmm7 |
    Xmm8 | Xmm9 | Xmm10 | Xmm11 | Xmm12 | Xmm13 | Xmm14 | Xmm15
    deriving (Show, Eq)


-- Name for per-class static initialization guard symbol.
-- Final label form: ownerQName ++ [staticInitFlagName]
-- e.g. com.wangdi.Const..isInit
staticInitFlagName :: String
staticInitFlagName = "@isInit"

staticInitName :: String
staticInitName = "@clinit"


-- Sign-extend op used before signed division.
-- `cdq`: eax -> edx:eax
-- `cqo`: rax -> rdx:rax
data SignExt64
    = SxCdq
    | SxCqo
    deriving (Eq, Show)

data Compiler = NASM | GAS_INTEL | GAS_ATT
    deriving (Eq, Show)


-- x64 calling/division convention.
data CallConv64 = CallConv64 {
    ccCompiler :: Compiler, -- target assembler syntax family
    ccIRet :: Register,   -- integer/pointer return register
    ccFRet :: Register,   -- floating-point return register
    ccArgs :: [Register], -- integer argument registers (in order)
    ccArgN :: Int,        -- count of register-passed args
    ccTmpI :: Register,   -- integer scratch/temp register
    ccTmpF :: Register,   -- floating-point scratch/temp register
    ccRip :: Register,    -- instruction pointer register
    ccSp :: Register,     -- stack pointer register
    ccBp :: Register,     -- frame/base pointer register
    ccSx32 :: SignExt64,  -- sign-extend op before 32-bit signed division
    ccSx64 :: SignExt64,  -- sign-extend op before 64-bit signed division
    ccDivLo :: Register,  -- dividend low register (also quotient output)
    ccDivHi :: Register,  -- dividend high register (also remainder output)
    ccDivQ :: Register,   -- quotient result register
    ccDivR :: Register    -- remainder result register
} deriving (Eq, Show)


rawQNameTag64 :: String
rawQNameTag64 = "@raw"


mkRawQName64 :: String -> [String]
mkRawQName64 sym = [rawQNameTag64, sym]


isRawQName64 :: [String] -> Bool
isRawQName64 (tag : _) = tag == rawQNameTag64
isRawQName64 _ = False


rawQNameSymbol64 :: [String] -> String
rawQNameSymbol64 qn = case qn of
    (_ : sym : _) -> sym
    _ -> error "rawQNameSymbol64: invalid raw qname encoding"


mkCC64 :: Compiler -> Register -> Register -> [Register] -> CallConv64
mkCC64 compiler iRetReg fRetReg argRegs =
    CallConv64 {
        ccCompiler = compiler,
        ccIRet = iRetReg,
        ccFRet = fRetReg,
        ccArgs = argRegs,
        ccArgN = length argRegs,
        ccTmpI = C,
        ccTmpF = Xmm15,
        ccRip = RIP,
        ccSp = SP,
        ccBp = BP,
        ccSx32 = SxCdq,
        ccSx64 = SxCqo,
        ccDivLo = A,
        ccDivHi = D,
        ccDivQ = A,
        ccDivR = D
    }


-- Windows x64 ABI
-- return: rax
-- float return: xmm0
-- params: rcx, rdx, r8, r9
winCC64 :: CallConv64
winCC64 = mkCC64 NASM A Xmm0 [C, D, R8, R9]


-- Linux x64 SysV ABI
-- return: rax
-- float return: xmm0
-- params: rdi, rsi, rdx, rcx, r8, r9
linuxCC64 :: CallConv64
linuxCC64 = mkCC64 NASM A Xmm0 [DI, SI, D, C, R8, R9]


-- macOS x64 ABI (SysV-style for integer args)
-- return: rax
-- float return: xmm0
-- params: rdi, rsi, rdx, rcx, r8, r9
macCC64 :: CallConv64
macCC64 = mkCC64 NASM A Xmm0 [DI, SI, D, C, R8, R9]


sizeByClass64 :: Map.Map Class Int
sizeByClass64 = Map.fromList [
    (AST.Int8T, 1),
    (AST.Int16T, 2),
    (AST.Int32T, 4),
    (AST.Int64T, 8),
    (AST.Float32T, 4),
    (AST.Float64T, 8),
    (AST.Float128T, 16),
    (AST.Bool, 1),
    (AST.Char, 1)]


bitsByClass64 :: Map.Map Class Bits
bitsByClass64 = Map.fromList [
    (AST.Int32T, B32),
    (AST.Int64T, B64),
    (AST.Int16T, B16),
    (AST.Int8T, B8L),
    (AST.Char, B8L),
    (AST.Bool, B8L)]


cppCallTypeMap64 :: Map.Map Class String
cppCallTypeMap64 = Map.fromList [
    (AST.Bool, "bool"),
    (AST.Int8T, "char"),
    (AST.Int16T, "short"),
    (AST.Int32T, "int"),
    (AST.Int64T, "long"),
    (AST.Float32T, "float"),
    (AST.Float64T, "double")]


cppCallType64 :: Class -> String
cppCallType64 cls = case Map.lookup cls cppCallTypeMap64 of
    Just cxxTy -> cxxTy
    Nothing -> error $ "cppCallType64: class/string are not supported in x64 static call yet: " ++ show cls


bitsWidth64 :: Bits -> Int
bitsWidth64 B8L = 8
bitsWidth64 B8H = 8
bitsWidth64 B16 = 16
bitsWidth64 B32 = 32
bitsWidth64 B64 = 64
bitsWidth64 NN = error "bitsWidth64: NN has no integer width"


intRet32Classes64 :: HashSet Class
intRet32Classes64 = HashSet.fromList [AST.Int32T, AST.Int16T, AST.Int8T, AST.Bool, AST.Char]


intRet64Classes64 :: HashSet Class
intRet64Classes64 = HashSet.fromList [AST.Int64T]


isRefClass64 :: Class -> Bool
isRefClass64 (AST.Class _ _) = True
isRefClass64 _ = False


intAssignBits64 :: Class -> Maybe Bits
intAssignBits64 cls = Map.lookup cls bitsByClass64


isCastIntClass64 :: Class -> Bool
isCastIntClass64 cls =
    cls `elem` [AST.Int8T, AST.Int16T, AST.Int32T, AST.Int64T, AST.Char]


castIntWidth64 :: Class -> Int
castIntWidth64 cls = case Map.lookup cls bitsByClass64 of
    Just bits -> bitsWidth64 bits
    Nothing -> error $ "castIntWidth64: unsupported integer cast class: " ++ show cls


classBits64 :: Class -> Bits
classBits64 cls = case Map.lookup cls bitsByClass64 of
    Just bits -> bits
    Nothing -> error $ "classBits64: unsupported class in bits map: " ++ show cls


asByteDst64 :: Atom -> Atom
asByteDst64 (Reg r _) = Reg r B8L
asByteDst64 (Mem b i d) = Mem b i d
asByteDst64 atom = error $ "asByteDst64: unsupported destination atom: " ++ show atom


asDwordDst64 :: Atom -> Atom
asDwordDst64 (Reg r _) = Reg r B32
asDwordDst64 atom = atom


data InstrConst =
    Zero Bits | Byte String | Value String | Long String | Quad String | 
    RawString String 
    deriving (Show, Eq)

prettyInstrConstIntel :: CallConv64 -> Int -> InstrConst -> String
prettyInstrConstIntel cc space ic = case ccCompiler cc of
    NASM -> prettyNasm ic
    _ -> prettyGas ic
    where
        prettyNasm :: InstrConst -> String
        prettyNasm (Zero B8L) = insertTab space ++ "db\t\t0\n"
        prettyNasm (Zero B8H) = insertTab space ++ "db\t\t0\n"
        prettyNasm (Zero B16) = insertTab space ++ "dw\t\t0\n"
        prettyNasm (Zero B32) = insertTab space ++ "dd\t\t0\n"
        prettyNasm (Zero B64) = insertTab space ++ "dq\t\t0\n"
        prettyNasm (Zero NN) = error "prettyInstrConstIntel(NASM): Zero NN is invalid"
        prettyNasm (Byte s) = concat [insertTab space, "db\t\t", s, "\n"]
        prettyNasm (Value s) = concat [insertTab space, "dw\t\t", s, "\n"]
        prettyNasm (Long s) = concat [insertTab space, "dd\t\t", s, "\n"]
        prettyNasm (Quad s) = concat [insertTab space, "dq\t\t", s, "\n"]
        prettyNasm (RawString s) = concat [insertTab space, s, "\n"]

        prettyGas :: InstrConst -> String
        prettyGas (Zero bit) = concat [insertTab space, ".zero\t\t", show (bitsBytes bit), "\n"]
        prettyGas (Byte s) = concat [insertTab space, ".byte\t\t", s, "\n"]
        prettyGas (Value s) = concat [insertTab space, ".value\t\t", s, "\n"]
        prettyGas (Long s) = concat [insertTab space, ".long\t\t", s, "\n"]
        prettyGas (Quad s) = concat [insertTab space, ".quad\t\t", s, "\n"]
        prettyGas (RawString s) = concat [insertTab space, s, "\n"]


data StaticData = StaticData String [InstrConst]
    deriving (Show, Eq)


prettyStaticDataIntel :: CallConv64 -> StaticData -> String
prettyStaticDataIntel cc (StaticData label consts) =
    concat [label, ":\n", concatMap (prettyInstrConstIntel cc 1) consts, "\n"]


-- NN means not need for float registers.
data Bits = NN | B8L | B8H | B16 | B32 | B64
    deriving (Show, Eq, Ord)

bitsBytes :: Bits -> Int
bitsBytes bits = case Map.lookup bits bitsBytesMap of
    Just n -> n
    Nothing -> error $ "bitsBytes: no byte width for " ++ show bits


bitsBytesMap :: Map.Map Bits Int
bitsBytesMap = Map.fromList [
    (B8L, 1),
    (B8H, 1),
    (B16, 2),
    (B32, 4),
    (B64, 8)]


prettyRegister :: Register -> Bits -> String
prettyRegister reg bits
    | isXmm reg = xmmName reg
    | reg `elem` [R8, R9, R10, R11, R12, R13, R14, R15] = extName reg bits
    | otherwise = case reg of
        A -> legacyABCD "a"
        B -> legacyABCD "b"
        C -> legacyABCD "c"
        D -> legacyABCD "d"
        SI -> legacySI "si"
        DI -> legacySI "di"
        SP -> legacySI "sp"
        BP -> legacySI "bp"
        RIP -> case bits of
            B64 -> "rip"
            _ -> error $ "prettyRegister: RIP only supports B64, got " ++ show bits
        _ -> error $ "prettyRegister: unsupported register " ++ show reg
    where
        isXmm :: Register -> Bool
        isXmm r = r `elem`
            [Xmm0, Xmm1, Xmm2, Xmm3, Xmm4, Xmm5, Xmm6, Xmm7,
            Xmm8, Xmm9, Xmm10, Xmm11, Xmm12, Xmm13, Xmm14, Xmm15]

        xmmName :: Register -> String
        xmmName r = case r of
            Xmm0 -> "xmm0"
            Xmm1 -> "xmm1"
            Xmm2 -> "xmm2"
            Xmm3 -> "xmm3"
            Xmm4 -> "xmm4"
            Xmm5 -> "xmm5"
            Xmm6 -> "xmm6"
            Xmm7 -> "xmm7"
            Xmm8 -> "xmm8"
            Xmm9 -> "xmm9"
            Xmm10 -> "xmm10"
            Xmm11 -> "xmm11"
            Xmm12 -> "xmm12"
            Xmm13 -> "xmm13"
            Xmm14 -> "xmm14"
            Xmm15 -> "xmm15"
            _ -> error $ "xmmName: expected xmm register, got " ++ show r

        legacyABCD :: String -> String
        legacyABCD pfx = case bits of
            B8L -> pfx ++ "l"
            B8H -> pfx ++ "h"
            B16 -> pfx ++ "x"
            B32 -> "e" ++ pfx ++ "x"
            B64 -> "r" ++ pfx ++ "x"
            NN -> error "prettyRegister: NN is invalid for GPR register"

        legacySI :: String -> String
        legacySI pfx = case bits of
            B8L -> pfx ++ "l"
            B8H -> error $ "prettyRegister: no high 8-bit register for " ++ pfx
            B16 -> pfx
            B32 -> 'e' : pfx
            B64 -> 'r' : pfx
            NN -> error "prettyRegister: NN is invalid for GPR register"

        extName :: Register -> Bits -> String
        extName r b =
            let n = case r of
                    R8 -> "8"
                    R9 -> "9"
                    R10 -> "10"
                    R11 -> "11"
                    R12 -> "12"
                    R13 -> "13"
                    R14 -> "14"
                    R15 -> "15"
                    _ -> error $ "extName: expected R8-R15, got " ++ show r
            in case b of
                B8L -> "r" ++ n ++ "b"
                B8H -> error $ "prettyRegister: no high 8-bit register for r" ++ n
                B16 -> "r" ++ n ++ "w"
                B32 -> "r" ++ n ++ "d"
                B64 -> "r" ++ n
                NN -> error "prettyRegister: NN is invalid for GPR register"


data Atom =
    Reg Register Bits |
    -- Mem base indexScale disp
    -- Address form: [base + scale*index + disp]
    -- base/index are optional so we can represent:
    --   [eax + 4], [eax*4 + 8], [4]
    Mem (Maybe Register) (Maybe (Int, Register)) Int |
    Imm Int |
    Bss [String] [Class] Register
    deriving (Show, Eq)


ptrPrefixIntelMap :: Map.Map Bits String
ptrPrefixIntelMap = Map.fromList [
    (B8L, "BYTE PTR "),
    (B8H, "BYTE PTR "),
    (B16, "WORD PTR "),
    (B32, "DWORD PTR "),
    (B64, "QWORD PTR ")]


ptrPrefixNasmMap :: Map.Map Bits String
ptrPrefixNasmMap = Map.fromList [
    (B8L, "BYTE "),
    (B8H, "BYTE "),
    (B16, "WORD "),
    (B32, "DWORD "),
    (B64, "QWORD ")]


ptrPrefixIntel :: CallConv64 -> Bits -> String
ptrPrefixIntel cc bits = case ccCompiler cc of
    NASM -> case Map.lookup bits ptrPrefixNasmMap of
        Just s -> s
        Nothing -> error $ "ptrPrefixIntel(NASM): invalid pointer width " ++ show bits
    GAS_ATT -> ""
    _ -> case Map.lookup bits ptrPrefixIntelMap of
        Just s -> s
        Nothing -> error $ "ptrPrefixIntel: invalid pointer width " ++ show bits


prettyAtomIntel :: CallConv64 -> Bits -> Atom -> String
prettyAtomIntel _cc _ (Reg reg bits) = prettyRegister reg bits
prettyAtomIntel cc bits (Bss symbol sigTs baseReg) =
    let baseTxt = prettyRegister baseReg B64
        symTxt = mangleQNameWithSig False symbol sigTs
    in case ccCompiler cc of
        NASM -> concat [ptrPrefixIntel cc bits, "[rel ", symTxt, "]"]
        _ -> concat [ptrPrefixIntel cc bits, "[", baseTxt, " + ", symTxt, "]"]
prettyAtomIntel cc bits (Mem mBase mScaledIndex disp) =
    concat [ptrPrefixIntel cc bits, "[", memExpr, "]"]
    where
        reg64 :: Register -> String
        reg64 r = prettyRegister r B64

        baseTerm :: [String]
        baseTerm = case mBase of
            Nothing -> []
            Just b -> [reg64 b]

        idxTerm :: [String]
        idxTerm = case mScaledIndex of
            Nothing -> []
            Just (scale, idx) -> [show scale ++ "*" ++ reg64 idx]

        coreTerms :: [String]
        coreTerms = baseTerm ++ idxTerm

        memExpr :: String
        memExpr = case (coreTerms, disp) of
            ([], 0) -> "0"
            ([], d) -> show d
            (ts, 0) -> intercalate " + " ts
            (ts, d) | d > 0 -> intercalate " + " ts ++ " + " ++ show d
                    | otherwise -> intercalate " + " ts ++ " - " ++ show (abs d)
prettyAtomIntel _cc _ (Imm val) = show val


opWidthWord :: Bits -> String
opWidthWord bits = case bits of
    B8L -> "BYTE "
    B8H -> "BYTE "
    B16 -> "WORD "
    B32 -> "DWORD "
    B64 -> "QWORD "
    NN -> error "opWidthWord: NN has no operand width"


prettyTypedOpAtom :: CallConv64 -> Bits -> Atom -> String
prettyTypedOpAtom cc bits atom = case ccCompiler cc of
    NASM -> case atom of
        -- register width is already encoded by register name (eax/rax/al...)
        Reg _ _ -> prettyAtomIntel cc bits atom
        -- immediate width should be inferred from destination/use-site
        Imm _ -> prettyAtomIntel cc bits atom
        _ -> prettyAtomIntel cc bits atom
    _ -> prettyAtomIntel cc bits atom


data Instruction =
    Mov Atom Atom Bits |
    Movs Atom Atom Bits |
    Movz Atom Atom Bits |
    Movd Atom Atom Bits |
    Movq Atom Atom Bits |
    Cvtsi2ss Atom Atom Bits |
    Cvtsi2sd Atom Atom Bits |
    Cvttss2si Atom Atom Bits |
    Cvttsd2si Atom Atom Bits |
    Cvtss2sd Atom Atom Bits |
    Cvtsd2ss Atom Atom Bits |
    Movss Atom Atom Bits |
    Movsd Atom Atom Bits |

    Lea Atom Atom Bits |
    Push Bits |
    Pop Bits |

    Add Atom Atom Bits |
    Addss Atom Atom Bits |
    Addsd Atom Atom Bits |
    Addps Atom Atom Bits |
    Addpd Atom Atom Bits |
    Sub Atom Atom Bits |
    Subss Atom Atom Bits |
    Subsd Atom Atom Bits |
    Subps Atom Atom Bits |
    Subpd Atom Atom Bits |
    Inc Atom Bits |
    Dec Atom Bits |
    Neg Atom Bits |
    Mul Atom Atom Bits |
    Mulss Atom Atom Bits |
    Mulsd Atom Atom Bits |
    Mulps Atom Atom Bits |
    Mulpd Atom Atom Bits |
    IMul Atom Atom Bits |
    Cdq |
    Cqo |
    Div Atom Atom Bits |
    Divss Atom Atom Bits |
    Divsd Atom Atom Bits |
    Divps Atom Atom Bits |
    Divpd Atom Atom Bits |
    IDiv Atom Atom Bits |

    And Atom Atom Bits |
    Or Atom Atom Bits |
    Xorps Atom Atom Bits |
    Xorpd Atom Atom Bits |
    Xor Atom Atom Bits |
    Not Atom Bits |

    Shl Atom Atom Bits |
    Sal Atom Atom Bits |
    Shr Atom Atom Bits |
    Sar Atom Atom Bits |

    Cmp Atom Atom Bits |
    Ucomiss Atom Atom Bits |
    Ucomisd Atom Atom Bits |
    Test Atom Atom Bits |

    Cmove Atom Atom Bits |
    Cmovz Atom Atom Bits |
    Cmovne Atom Atom Bits |
    Cmovnz Atom Atom Bits |
    Cmovg Atom Atom Bits |
    Cmovge Atom Atom Bits |
    Cmovl Atom Atom Bits |
    Cmovle Atom Atom Bits |
    Cmovs Atom Atom Bits |
    Cmovns Atom Atom Bits |
    Setp Atom Bits |
    Setne Atom Bits |

    Jump (Either [String] Int) |
    Je (Either [String] Int) |
    Jne (Either [String] Int) |
    Jp (Either [String] Int) |
    Jg (Either [String] Int) |
    Jge (Either [String] Int) |
    Jl (Either [String] Int) |
    Jle (Either [String] Int) |
    Ja (Either [String] Int) |
    Jae (Either [String] Int) |
    Jb (Either [String] Int) |
    Jbe (Either [String] Int) |

    Call [String] [Class] |
    Leave |
    Ret
    deriving (Show, Eq)

prettyInstrIntel :: CallConv64 -> Int -> Instruction -> String
prettyInstrIntel _cc tab (Mov target src bits) = concat [
    insertTab tab,
    "mov ",
    prettyTypedOpAtom _cc bits target,
    ", ",
    prettyTypedOpAtom _cc bits src]
prettyInstrIntel _cc tab (Movd target src bits) = concat [insertTab tab, "movd ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Movq target src bits) = concat [insertTab tab, "movq ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Cvtsi2ss target src bits) = concat [insertTab tab, "cvtsi2ss ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Cvtsi2sd target src bits) = concat [insertTab tab, "cvtsi2sd ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Cvttss2si target src bits) = concat [insertTab tab, "cvttss2si ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Cvttsd2si target src bits) = concat [insertTab tab, "cvttsd2si ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Cvtss2sd target src bits) = concat [insertTab tab, "cvtss2sd ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Cvtsd2ss target src bits) = concat [insertTab tab, "cvtsd2ss ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Movss target src bits) = concat [insertTab tab, "movss ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Movsd target src bits) = concat [insertTab tab, "movsd ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Movs target src bits) = concat [
    insertTab tab,
    "movsx ",
    prettyTypedOpAtom _cc bits target,
    ", ",
    case src of
        Reg _ srcBits -> prettyTypedOpAtom _cc srcBits src
        _ -> prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Movz target src bits) = concat [
    insertTab tab,
    "movzx ",
    prettyTypedOpAtom _cc bits target,
    ", ",
    case src of
        Reg _ srcBits -> prettyTypedOpAtom _cc srcBits src
        _ -> prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Lea target src bits) = concat [insertTab tab, "lea ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Push bits) = case ccCompiler _cc of
    NASM -> concat [insertTab tab, "push ", opWidthWord bits, prettyRegister BP B64]
    _ -> concat [insertTab tab, "push ", prettyRegister BP B64]
prettyInstrIntel _cc tab (Pop bits) = case ccCompiler _cc of
    NASM -> concat [insertTab tab, "pop ", opWidthWord bits, prettyRegister BP B64]
    _ -> concat [insertTab tab, "pop ", prettyRegister BP B64]
prettyInstrIntel _cc tab (Add target src bits) = concat [insertTab tab, "add ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Addss target src bits) = concat [insertTab tab, "addss ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Addsd target src bits) = concat [insertTab tab, "addsd ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Addps target src bits) = concat [insertTab tab, "addps ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Addpd target src bits) = concat [insertTab tab, "addpd ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Sub target src bits) = concat [insertTab tab, "sub ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Subss target src bits) = concat [insertTab tab, "subss ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Subsd target src bits) = concat [insertTab tab, "subsd ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Subps target src bits) = concat [insertTab tab, "subps ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Subpd target src bits) = concat [insertTab tab, "subpd ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Inc target bits) = concat [insertTab tab, "inc ", prettyAtomIntel _cc bits target]
prettyInstrIntel _cc tab (Dec target bits) = concat [insertTab tab, "dec ", prettyAtomIntel _cc bits target]
prettyInstrIntel _cc tab (Neg target bits) = concat [insertTab tab, "neg ", prettyAtomIntel _cc bits target]
prettyInstrIntel _cc tab (Mul target src bits) = concat [insertTab tab, "mul ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Mulss target src bits) = concat [insertTab tab, "mulss ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Mulsd target src bits) = concat [insertTab tab, "mulsd ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Mulps target src bits) = concat [insertTab tab, "mulps ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Mulpd target src bits) = concat [insertTab tab, "mulpd ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (IMul target src bits) = concat [insertTab tab, "imul ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab Cdq = insertTab tab ++ "cdq"
prettyInstrIntel _cc tab Cqo = insertTab tab ++ "cqo"
prettyInstrIntel _cc tab (Div _ divisor bits) = concat [insertTab tab, "div ", prettyAtomIntel _cc bits divisor]
prettyInstrIntel _cc tab (Divss target src bits) = concat [insertTab tab, "divss ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Divsd target src bits) = concat [insertTab tab, "divsd ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Divps target src bits) = concat [insertTab tab, "divps ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Divpd target src bits) = concat [insertTab tab, "divpd ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (IDiv _ divisor bits) = concat [insertTab tab, "idiv ", prettyAtomIntel _cc bits divisor]
prettyInstrIntel _cc tab (And target src bits) = concat [insertTab tab, "and ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Or target src bits) = concat [insertTab tab, "or ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Xorps target src bits) = concat [insertTab tab, "xorps ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Xorpd target src bits) = concat [insertTab tab, "xorpd ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Xor target src bits) = concat [insertTab tab, "xor ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Not target bits) = concat [insertTab tab, "not ", prettyAtomIntel _cc bits target]
prettyInstrIntel _cc tab (Shl target src bits) = concat [insertTab tab, "shl ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Sal target src bits) = concat [insertTab tab, "sal ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Shr target src bits) = concat [insertTab tab, "shr ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Sar target src bits) = concat [insertTab tab, "sar ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Cmp target src bits) = concat [insertTab tab, "cmp ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Ucomiss target src bits) = concat [insertTab tab, "ucomiss ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Ucomisd target src bits) = concat [insertTab tab, "ucomisd ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Test target src bits) = concat [insertTab tab, "test ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Cmove target src bits) = concat [insertTab tab, "cmove ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Cmovz target src bits) = concat [insertTab tab, "cmovz ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Cmovne target src bits) = concat [insertTab tab, "cmovne ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Cmovnz target src bits) = concat [insertTab tab, "cmovnz ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Cmovg target src bits) = concat [insertTab tab, "cmovg ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Cmovge target src bits) = concat [insertTab tab, "cmovge ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Cmovl target src bits) = concat [insertTab tab, "cmovl ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Cmovle target src bits) = concat [insertTab tab, "cmovle ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Cmovs target src bits) = concat [insertTab tab, "cmovs ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Cmovns target src bits) = concat [insertTab tab, "cmovns ", prettyAtomIntel _cc bits target, ", ", prettyAtomIntel _cc bits src]
prettyInstrIntel _cc tab (Setp target bits) = concat [insertTab tab, "setp ", prettyAtomIntel _cc bits target]
prettyInstrIntel _cc tab (Setne target bits) = concat [insertTab tab, "setne ", prettyAtomIntel _cc bits target]
prettyInstrIntel _cc tab (Jump target) = concat [insertTab tab, "jmp ", prettyJumpTarget _cc target]
prettyInstrIntel _cc tab (Je target) = concat [insertTab tab, "je ", prettyJumpTarget _cc target]
prettyInstrIntel _cc tab (Jne target) = concat [insertTab tab, "jne ", prettyJumpTarget _cc target]
prettyInstrIntel _cc tab (Jp target) = concat [insertTab tab, "jp ", prettyJumpTarget _cc target]
prettyInstrIntel _cc tab (Jg target) = concat [insertTab tab, "jg ", prettyJumpTarget _cc target]
prettyInstrIntel _cc tab (Jge target) = concat [insertTab tab, "jge ", prettyJumpTarget _cc target]
prettyInstrIntel _cc tab (Jl target) = concat [insertTab tab, "jl ", prettyJumpTarget _cc target]
prettyInstrIntel _cc tab (Jle target) = concat [insertTab tab, "jle ", prettyJumpTarget _cc target]
prettyInstrIntel _cc tab (Ja target) = concat [insertTab tab, "ja ", prettyJumpTarget _cc target]
prettyInstrIntel _cc tab (Jae target) = concat [insertTab tab, "jae ", prettyJumpTarget _cc target]
prettyInstrIntel _cc tab (Jb target) = concat [insertTab tab, "jb ", prettyJumpTarget _cc target]
prettyInstrIntel _cc tab (Jbe target) = concat [insertTab tab, "jbe ", prettyJumpTarget _cc target]
prettyInstrIntel _cc tab (Call func sigTs) = concat [insertTab tab, "call ", mangleQNameWithSig True func sigTs]
prettyInstrIntel _cc tab Leave = insertTab tab ++ "leave"
prettyInstrIntel _cc tab Ret = insertTab tab ++ "ret"


prettyJumpTarget :: CallConv64 -> Either [String] Int -> String
prettyJumpTarget _cc target = case target of
    Left qn -> mangleQName True qn
    Right bid -> ".L" ++ show bid


mangleQNameWithSig :: Bool -> [String] -> [Class] -> String
mangleQNameWithSig _ qn _ | isRawQName64 qn = rawQNameSymbol64 qn
mangleQNameWithSig isFunction qn sigTs = case qn of
    [] -> error "mangleQName: empty qname"
    [name] -> mangleName [] [dropSig name] sigTs isFunction
    _ -> mangleName (init qn) [dropSig (last qn)] sigTs isFunction
    where
        dropSig :: String -> String
        dropSig = takeWhile (/= '(')


mangleQName :: Bool -> [String] -> String
mangleQName isFunction qn = mangleQNameWithSig isFunction qn []

data X64Segment
    = Const String String -- Const in compile time 
    | X64Label Int [Instruction]
    | X64Segement [String] [Instruction]
    | X64Func [String] (Class, [Class]) [Instruction]
    | X64ClassInfo [String] String Int
    deriving (Show, Eq)


prettyInstrsIntel :: CallConv64 -> [Instruction] -> String
prettyInstrsIntel cc = concatMap (\ins -> prettyInstrIntel cc 1 ins ++ "\n")


prettySegmentIntel :: CallConv64 -> X64Segment -> String
prettySegmentIntel _cc (Const name value) = concat [name, " equ ", value, "\n"]
prettySegmentIntel cc (X64Label bid instrs) = concat [".L", show bid, ":\n", prettyInstrsIntel cc instrs]
prettySegmentIntel cc (X64Segement qn instrs) = concat [mangleQName True qn, ":\n", prettyInstrsIntel cc instrs]
prettySegmentIntel cc (X64Func qn (retT, paramTs) instrs) =
    let fullSig = paramTs ++ [retT]
        sym = mangleQNameWithSig True qn fullSig
    in concat [sym, ":\n", prettyInstrsIntel cc instrs]
prettySegmentIntel cc (X64ClassInfo qn dataSym infoLen) =
    let sym = mangleQName False qn
        ptrInsn = case ccCompiler cc of
            NASM -> concat [insertTab 1, "lea rax, [rel ", dataSym, "]\n"]
            _ -> concat [insertTab 1, "lea rax, [rip + ", dataSym, "]\n"]
        lenInsn = concat [insertTab 1, "mov eax, ", show infoLen, "\n"]
        retInsn = insertTab 1 ++ "ret\n"
    in concat [
        sym, "_info:\n",
        ptrInsn, retInsn,
        sym, "_info_len:\n",
        lenInsn, retInsn]


isLabelSeg :: X64Segment -> Bool
isLabelSeg (X64Label _ _) = True
isLabelSeg _ = False


shouldGlueSegs :: X64Segment -> X64Segment -> Bool
shouldGlueSegs _ (X64Label _ _) = True
shouldGlueSegs _ _ = False


-- Keep function/segment header contiguous with following .L blocks (no empty line).
-- Insert one empty line between different top-level segments/functions.
prettySegmentsIntel :: CallConv64 -> [X64Segment] -> String
prettySegmentsIntel _cc [] = ""
prettySegmentsIntel cc [seg] = prettySegmentIntel cc seg
prettySegmentsIntel cc (seg1 : seg2 : rest) =
    let sep = if shouldGlueSegs seg1 seg2 then "" else "\n"
    in prettySegmentIntel cc seg1 ++ sep ++ prettySegmentsIntel cc (seg2 : rest)

-- bss .data and .text
type X64Class = ([StaticData], [X64Segment])

data X64Decl
    = Global [String] [Class]
    | GlobalClassInfo [String]
    | Extern [String] [Class]
    deriving (Show, Eq)


prettyDeclIntel :: CallConv64 -> X64Decl -> String
prettyDeclIntel cc (Global qn sigTs) = concat [declKw "global", " ", mangleQNameWithSig True qn sigTs, "\n"]
  where
    declKw :: String -> String
    declKw kw = case ccCompiler cc of
        NASM -> kw
        _ -> "." ++ kw
prettyDeclIntel cc (GlobalClassInfo qn) =
    let sym = mangleQName False qn
    in concat [declKw "global", " ", sym, "_info\n", declKw "global", " ", sym, "_info_len\n"]
  where
    declKw :: String -> String
    declKw kw = case ccCompiler cc of
        NASM -> kw
        _ -> "." ++ kw
prettyDeclIntel cc (Extern qn sigTs) = concat [declKw "extern", " ", mangleQNameWithSig True qn sigTs, "\n"]
  where
    declKw :: String -> String
    declKw kw = case ccCompiler cc of
        NASM -> kw
        _ -> "." ++ kw


data X64Program = X64Progm [X64Decl] [StaticData] [X64Segment]
    deriving (Show, Eq)


prettyX64ClassIntel :: CallConv64 -> X64Class -> String
prettyX64ClassIntel cc (staticData, segs) = syntaxPreamble cc ++ prettyData staticData ++ prettyText segs
    where
        prettyData :: [StaticData] -> String
        prettyData [] = ""
        prettyData ds = emitSection cc ".data" (concatMap (prettyStaticDataIntel cc) ds)

        prettyText :: [X64Segment] -> String
        prettyText [] = ""
        prettyText ss = emitSection cc ".text" (prettySegmentsIntel cc ss)


prettyX64ProgmIntel :: CallConv64 -> X64Program -> String
prettyX64ProgmIntel cc (X64Progm decls staticData segs) = concat [
    syntaxPreamble cc,
    declPart,
    prettyData staticData,
    prettyText segs]
    where
        declPart :: String
        declPart = case decls of
            [] -> ""
            _ ->
                let (externDecls, otherDecls) = partition isExternDecl decls
                    externPart = concatMap (prettyDeclIntel cc) externDecls
                    otherPart = concatMap (prettyDeclIntel cc) otherDecls
                in case (null externDecls, null otherDecls) of
                    (False, False) -> externPart ++ "\n" ++ otherPart ++ "\n\n"
                    (False, True) -> externPart ++ "\n\n"
                    (True, False) -> otherPart ++ "\n\n"
                    (True, True) -> ""

        prettyData :: [StaticData] -> String
        prettyData [] = ""
        prettyData ds = emitSection cc ".data" (concatMap (prettyStaticDataIntel cc) ds)

        prettyText :: [X64Segment] -> String
        prettyText [] = ""
        prettyText ss = emitSection cc ".text" (prettySegmentsIntel cc ss)


isExternDecl :: X64Decl -> Bool
isExternDecl (Extern _ _) = True
isExternDecl _ = False


syntaxPreamble :: CallConv64 -> String
syntaxPreamble cc = case ccCompiler cc of
    GAS_INTEL -> ".intel_syntax noprefix\n\n"
    GAS_ATT -> ".att_syntax prefix\n\n"
    _ -> ""


emitSection :: CallConv64 -> String -> String -> String
emitSection cc sec body = case ccCompiler cc of
    NASM -> concat ["section ", sec, "\n", body, "\n"]
    _ -> body ++ "\n"


