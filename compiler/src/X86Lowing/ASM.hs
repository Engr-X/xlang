module X86Lowing.ASM where

import Util.Basic (insertSpace)


data Register = 
    A | B | C | D |
    SI | DI | SP | BP |
    R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
    deriving (Show, Eq)

data Bits = B8L | B8H | B16 | B32 | B64
    deriving (Show, Eq)


prettyRegister :: Register -> Bits -> String
prettyRegister reg bits =
    case bits of
        B8H -> high8 reg
        _   -> prefix bits ++ base reg ++ suffix bits
    where
        prefix :: Bits -> String
        prefix B8L = ""
        prefix B8H = ""
        prefix B16 = ""
        prefix B32 = if reg `elem` [R8, R9, R10, R11, R12, R13, R14, R15] then "" else "e"
        prefix B64 = "r"

        suffix :: Bits -> String
        suffix B8L
            | reg `elem` [R8, R9, R10, R11, R12, R13, R14, R15] = "b"
            | reg `elem` [SI, DI, SP, BP] = "l"
            | otherwise = "l"
        suffix B16
            | reg `elem` [R8, R9, R10, R11, R12, R13, R14, R15] = "w"
            | otherwise = ""
        suffix B32
            | reg `elem` [R8, R9, R10, R11, R12, R13, R14, R15] = "d"
            | otherwise = ""
        suffix B64 = ""
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

        high8 :: Register -> String
        high8 A = "ah"
        high8 B = "bh"
        high8 C = "ch"
        high8 D = "dh"
        high8 r = error $ "no high 8-bit register for " ++ show r


data TargetAtom = 
    Reg Register Bits |
    Mem Atom
    deriving (Show, Eq)


prettyTarget :: TargetAtom -> String
prettyTarget (Reg reg bits) = prettyRegister reg bits
prettyTarget (Mem atom) = concat ["[", prettyAtom atom, "]"]


data Atom =
    Target TargetAtom |
    Imm Int
    deriving (Show, Eq)


prettyAtom :: Atom -> String
prettyAtom (Target target) = prettyTarget target
prettyAtom (Imm val) = show val


data Instruction =
    Mov TargetAtom Atom |
    Lea TargetAtom Atom |
    Push |
    Pop |

    Add TargetAtom Atom |
    Sub TargetAtom Atom |
    Inc TargetAtom |
    Dec TargetAtom |
    Neg TargetAtom |
    Mul TargetAtom Atom |
    IMul TargetAtom Atom |
    Div TargetAtom Atom |
    IDiv TargetAtom  Atom |

    And TargetAtom Atom |
    Or TargetAtom Atom |
    Xor TargetAtom Atom |
    Not TargetAtom |

    Shl TargetAtom Atom |
    Sal TargetAtom Atom |
    Shr TargetAtom Atom |
    Sar TargetAtom Atom |

    Cmp TargetAtom Atom |
    Test TargetAtom Atom |

    Jump String |
    Je String |
    Jne String |
    Jg String |
    Jge String |
    Jl String |
    Jle String |

    Call String |
    Ret
    deriving (Show, Eq)


prettyInstruction :: Int -> Instruction -> String
prettyInstruction tab (Mov target src) = concat [insertSpace tab, "mov ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Lea target src) = concat [insertSpace tab, "lea ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab Push = insertSpace tab ++ "push"
prettyInstruction tab Pop = insertSpace tab ++ "pop"
prettyInstruction tab (Add target src) = concat [insertSpace tab, "add ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Sub target src) = concat [insertSpace tab, "sub ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Inc target) = concat [insertSpace tab, "inc ", prettyTarget target]
prettyInstruction tab (Dec target) = concat [insertSpace tab, "dec ", prettyTarget target]
prettyInstruction tab (Neg target) = concat [insertSpace tab, "neg ", prettyTarget target]
prettyInstruction tab (Mul target src) = concat [insertSpace tab, "mul ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (IMul target src) = concat [insertSpace tab, "imul ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Div target src) = concat [insertSpace tab, "div ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (IDiv target src) = concat [insertSpace tab, "idiv ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (And target src) = concat [insertSpace tab, "and ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Or target src) = concat [insertSpace tab, "or ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Xor target src) = concat [insertSpace tab, "xor ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Not target) = concat [insertSpace tab, "not ", prettyTarget target]
prettyInstruction tab (Shl target src) = concat [insertSpace tab, "shl ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Sal target src) = concat [insertSpace tab, "sal ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Shr target src) = concat [insertSpace tab, "shr ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Sar target src) = concat [insertSpace tab, "sar ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Cmp target src) = concat [insertSpace tab, "cmp ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Test target src) = concat [insertSpace tab, "test ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Jump label) = concat [insertSpace tab, "jmp ", label]
prettyInstruction tab (Je label) = concat [insertSpace tab, "je ", label]
prettyInstruction tab (Jne label) = concat [insertSpace tab, "jne ", label]
prettyInstruction tab (Jg label) = concat [insertSpace tab, "jg ", label]
prettyInstruction tab (Jge label) = concat [insertSpace tab, "jge ", label]
prettyInstruction tab (Jl label) = concat [insertSpace tab, "jl ", label]
prettyInstruction tab (Jle label) = concat [insertSpace tab, "jle ", label]
prettyInstruction tab (Call func) = concat [insertSpace tab, "call ", func]
prettyInstruction tab Ret = insertSpace tab ++ "ret"


type Instructions = [(String, [Instruction])]


prettyInstructions :: Instructions -> String
prettyInstructions = concatMap (\(label, instrs) -> concat [".", label, ":\n", concatMap (prettyInstruction 4) instrs, "\n"])

