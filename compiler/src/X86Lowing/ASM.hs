module X86Lowing.ASM where

import Data.List (intercalate)
import Util.Basic (insertTab)


data Register = 
    A | B | C | D |
    SI | DI | SP | BP |
    R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 |
    Xmm0 | Xmm1 | Xmm2 | Xmm3 | Xmm4 | Xmm5 | Xmm6 | Xmm7 |
    Xmm8 | Xmm9 | Xmm10 | Xmm11 | Xmm12 | Xmm13 | Xmm14 | Xmm15
    deriving (Show, Eq)

-- NN means not need for float registers.
data Bits = NN | B8L | B8H | B16 | B32 | B64
    deriving (Show, Eq)


prettyRegister :: Register -> Bits -> String
prettyRegister reg bits =
    case bits of
        B8H -> high8 reg
        _   -> prefix bits ++ base reg ++ suffix bits
    where
        isXmm :: Register -> Bool
        isXmm r = r `elem`
            [ Xmm0, Xmm1, Xmm2, Xmm3, Xmm4, Xmm5, Xmm6, Xmm7
            , Xmm8, Xmm9, Xmm10, Xmm11, Xmm12, Xmm13, Xmm14, Xmm15 ]

        prefix :: Bits -> String
        prefix NN = ""
        prefix B8L = ""
        prefix B8H = ""
        prefix B16 = ""
        prefix B32
            | isXmm reg = ""
            | reg `elem` [R8, R9, R10, R11, R12, R13, R14, R15] = ""
            | otherwise = "e"
        prefix B64
            | isXmm reg = ""
            | otherwise = "r"

        suffix :: Bits -> String
        suffix NN = ""
        suffix B8L
            | isXmm reg = ""
            | reg `elem` [R8, R9, R10, R11, R12, R13, R14, R15] = "b"
            | reg `elem` [SI, DI, SP, BP] = "l"
            | otherwise = "l"
        suffix B16
            | isXmm reg = ""
            | reg `elem` [R8, R9, R10, R11, R12, R13, R14, R15] = "w"
            | otherwise = ""
        suffix B32
            | isXmm reg = ""
            | reg `elem` [R8, R9, R10, R11, R12, R13, R14, R15] = "d"
            | otherwise = ""
        suffix B64
            | isXmm reg = ""
            | otherwise = ""
        suffix B8H = ""

        base :: Register -> String
        base A = "a"
        base B = "b"
        base C = "c"
        base D = "d"
        base SI = "si"
        base DI = "di"
        base SP = "sp"
        base BP = "bp"
        base R8 = "8"
        base R9 = "9"
        base R10 = "10"
        base R11 = "11"
        base R12 = "12"
        base R13 = "13"
        base R14 = "14"
        base R15 = "15"
        base Xmm0 = "xmm0"
        base Xmm1 = "xmm1"
        base Xmm2 = "xmm2"
        base Xmm3 = "xmm3"
        base Xmm4 = "xmm4"
        base Xmm5 = "xmm5"
        base Xmm6 = "xmm6"
        base Xmm7 = "xmm7"
        base Xmm8 = "xmm8"
        base Xmm9 = "xmm9"
        base Xmm10 = "xmm10"
        base Xmm11 = "xmm11"
        base Xmm12 = "xmm12"
        base Xmm13 = "xmm13"
        base Xmm14 = "xmm14"
        base Xmm15 = "xmm15"

        high8 :: Register -> String
        high8 A = "ah"
        high8 B = "bh"
        high8 C = "ch"
        high8 D = "dh"
        high8 r = error $ "no high 8-bit register for " ++ show r


data Atom =
    Reg Register Bits |
    -- Mem base indexScale disp
    -- Address form: [base + scale*index + disp]
    -- base/index are optional so we can represent:
    --   [eax + 4], [eax*4 + 8], [4]
    Mem (Maybe Register) (Maybe (Int, Register)) Int |
    Imm Int
    deriving (Show, Eq)


prettyAtom :: Atom -> String
prettyAtom (Reg reg bits) = prettyRegister reg bits
prettyAtom (Mem mBase mScaledIndex disp) =
    concat ["[", memExpr, "]"]
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
prettyAtom (Imm val) = show val


data Instruction =
    Mov Atom Atom |
    Movs Atom Atom |
    Movz Atom Atom |

    Lea Atom Atom |
    Push |
    Pop |

    Add Atom Atom |
    Addss Atom Atom |
    Addsd Atom Atom |
    Addps Atom Atom |
    Addpd Atom Atom |
    Sub Atom Atom |
    Subss Atom Atom |
    Subsd Atom Atom |
    Subps Atom Atom |
    Subpd Atom Atom |
    Inc Atom |
    Dec Atom |
    Neg Atom |
    Mul Atom Atom |
    Mulss Atom Atom |
    Mulsd Atom Atom |
    Mulps Atom Atom |
    Mulpd Atom Atom |
    IMul Atom Atom |
    Cdq |
    Cqo |
    Div Atom Atom |
    Divss Atom Atom |
    Divsd Atom Atom |
    Divps Atom Atom |
    Divpd Atom Atom |
    IDiv Atom  Atom |

    And Atom Atom |
    Or Atom Atom |
    Xorps Atom Atom |
    Xorpd Atom Atom |
    Xor Atom Atom |
    Not Atom |

    Shl Atom Atom |
    Sal Atom Atom |
    Shr Atom Atom |
    Sar Atom Atom |

    Cmp Atom Atom |
    Ucomiss Atom Atom |
    Ucomisd Atom Atom |
    Test Atom Atom |

    Cmove Atom Atom |
    Cmovz Atom Atom |
    Cmovne Atom Atom |
    Cmovnz Atom Atom |
    Cmovg Atom Atom |
    Cmovge Atom Atom |
    Cmovl Atom Atom |
    Cmovle Atom Atom |
    Cmovs Atom Atom |
    Cmovns Atom Atom |

    Jump String |
    Je String |
    Jne String |
    Jg String |
    Jge String |
    Jl String |
    Jle String |
    Ja String |
    Jae String |
    Jb String |
    Jbe String |

    Call String |
    Leave |
    Ret
    deriving (Show, Eq)


data X86Segment = X86Label Int [Instruction]
    deriving (Show, Eq)

data X86Func = X86Func (String, [String]) [Instruction]
    deriving (Show, Eq)


prettyInstruction :: Int -> Instruction -> String
prettyInstruction tab (Mov target src) = concat [insertTab tab, "mov ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Movs target src) = concat [insertTab tab, "movsx ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Movz target src) = concat [insertTab tab, "movzx ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Lea target src) = concat [insertTab tab, "lea ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab Push = insertTab tab ++ "push"
prettyInstruction tab Pop = insertTab tab ++ "pop"
prettyInstruction tab (Add target src) = concat [insertTab tab, "add ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Addss target src) = concat [insertTab tab, "addss ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Addsd target src) = concat [insertTab tab, "addsd ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Addps target src) = concat [insertTab tab, "addps ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Addpd target src) = concat [insertTab tab, "addpd ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Sub target src) = concat [insertTab tab, "sub ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Subss target src) = concat [insertTab tab, "subss ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Subsd target src) = concat [insertTab tab, "subsd ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Subps target src) = concat [insertTab tab, "subps ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Subpd target src) = concat [insertTab tab, "subpd ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Inc target) = concat [insertTab tab, "inc ", prettyAtom target]
prettyInstruction tab (Dec target) = concat [insertTab tab, "dec ", prettyAtom target]
prettyInstruction tab (Neg target) = concat [insertTab tab, "neg ", prettyAtom target]
prettyInstruction tab (Mul target src) = concat [insertTab tab, "mul ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Mulss target src) = concat [insertTab tab, "mulss ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Mulsd target src) = concat [insertTab tab, "mulsd ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Mulps target src) = concat [insertTab tab, "mulps ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Mulpd target src) = concat [insertTab tab, "mulpd ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (IMul target src) = concat [insertTab tab, "imul ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab Cdq = insertTab tab ++ "cdq"
prettyInstruction tab Cqo = insertTab tab ++ "cqo"
prettyInstruction tab (Div target src) = concat [insertTab tab, "div ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Divss target src) = concat [insertTab tab, "divss ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Divsd target src) = concat [insertTab tab, "divsd ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Divps target src) = concat [insertTab tab, "divps ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Divpd target src) = concat [insertTab tab, "divpd ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (IDiv target src) = concat [insertTab tab, "idiv ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (And target src) = concat [insertTab tab, "and ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Or target src) = concat [insertTab tab, "or ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Xorps target src) = concat [insertTab tab, "xorps ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Xorpd target src) = concat [insertTab tab, "xorpd ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Xor target src) = concat [insertTab tab, "xor ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Not target) = concat [insertTab tab, "not ", prettyAtom target]
prettyInstruction tab (Shl target src) = concat [insertTab tab, "shl ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Sal target src) = concat [insertTab tab, "sal ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Shr target src) = concat [insertTab tab, "shr ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Sar target src) = concat [insertTab tab, "sar ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Cmp target src) = concat [insertTab tab, "cmp ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Ucomiss target src) = concat [insertTab tab, "ucomiss ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Ucomisd target src) = concat [insertTab tab, "ucomisd ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Test target src) = concat [insertTab tab, "test ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Cmove target src) = concat [insertTab tab, "cmove ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Cmovz target src) = concat [insertTab tab, "cmovz ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Cmovne target src) = concat [insertTab tab, "cmovne ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Cmovnz target src) = concat [insertTab tab, "cmovnz ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Cmovg target src) = concat [insertTab tab, "cmovg ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Cmovge target src) = concat [insertTab tab, "cmovge ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Cmovl target src) = concat [insertTab tab, "cmovl ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Cmovle target src) = concat [insertTab tab, "cmovle ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Cmovs target src) = concat [insertTab tab, "cmovs ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Cmovns target src) = concat [insertTab tab, "cmovns ", prettyAtom target, ", ", prettyAtom src]
prettyInstruction tab (Jump label) = concat [insertTab tab, "jmp ", label]
prettyInstruction tab (Je label) = concat [insertTab tab, "je ", label]
prettyInstruction tab (Jne label) = concat [insertTab tab, "jne ", label]
prettyInstruction tab (Jg label) = concat [insertTab tab, "jg ", label]
prettyInstruction tab (Jge label) = concat [insertTab tab, "jge ", label]
prettyInstruction tab (Jl label) = concat [insertTab tab, "jl ", label]
prettyInstruction tab (Jle label) = concat [insertTab tab, "jle ", label]
prettyInstruction tab (Ja label) = concat [insertTab tab, "ja ", label]
prettyInstruction tab (Jae label) = concat [insertTab tab, "jae ", label]
prettyInstruction tab (Jb label) = concat [insertTab tab, "jb ", label]
prettyInstruction tab (Jbe label) = concat [insertTab tab, "jbe ", label]
prettyInstruction tab (Call func) = concat [insertTab tab, "call ", func]
prettyInstruction tab Leave = insertTab tab ++ "leave"
prettyInstruction tab Ret = insertTab tab ++ "ret"


type Instructions = [(String, [Instruction])]


prettyInstructions :: Instructions -> String
prettyInstructions = concatMap (\(label, instrs) -> concat [".", label, ":\n", concatMap (prettyInstruction 1) instrs, "\n"])

