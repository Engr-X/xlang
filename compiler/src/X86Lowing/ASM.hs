module X86Lowing.ASM where

import Util.Basic (insertTab)


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
    Movs TargetAtom Atom |
    Movz TargetAtom Atom |

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
    Leave |
    Ret
    deriving (Show, Eq)


newtype Segement = X86Label Int [Instruction]
    deriving (Show, Eq)

newtype Function = X86Func (String, [String]) [Instruction]
    deriving (Show, Eq)


prettyInstruction :: Int -> Instruction -> String
prettyInstruction tab (Mov target src) = concat [insertTab tab, "mov ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Movs target src) = concat [insertTab tab, "movs ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Movz target src) = concat [insertTab tab, "movz ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Lea target src) = concat [insertTab tab, "lea ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab Push = insertTab tab ++ "push"
prettyInstruction tab Pop = insertTab tab ++ "pop"
prettyInstruction tab (Add target src) = concat [insertTab tab, "add ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Sub target src) = concat [insertTab tab, "sub ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Inc target) = concat [insertTab tab, "inc ", prettyTarget target]
prettyInstruction tab (Dec target) = concat [insertTab tab, "dec ", prettyTarget target]
prettyInstruction tab (Neg target) = concat [insertTab tab, "neg ", prettyTarget target]
prettyInstruction tab (Mul target src) = concat [insertTab tab, "mul ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (IMul target src) = concat [insertTab tab, "imul ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Div target src) = concat [insertTab tab, "div ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (IDiv target src) = concat [insertTab tab, "idiv ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (And target src) = concat [insertTab tab, "and ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Or target src) = concat [insertTab tab, "or ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Xor target src) = concat [insertTab tab, "xor ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Not target) = concat [insertTab tab, "not ", prettyTarget target]
prettyInstruction tab (Shl target src) = concat [insertTab tab, "shl ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Sal target src) = concat [insertTab tab, "sal ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Shr target src) = concat [insertTab tab, "shr ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Sar target src) = concat [insertTab tab, "sar ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Cmp target src) = concat [insertTab tab, "cmp ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Test target src) = concat [insertTab tab, "test ", prettyTarget target, ", ", prettyAtom src]
prettyInstruction tab (Jump label) = concat [insertTab tab, "jmp ", label]
prettyInstruction tab (Je label) = concat [insertTab tab, "je ", label]
prettyInstruction tab (Jne label) = concat [insertTab tab, "jne ", label]
prettyInstruction tab (Jg label) = concat [insertTab tab, "jg ", label]
prettyInstruction tab (Jge label) = concat [insertTab tab, "jge ", label]
prettyInstruction tab (Jl label) = concat [insertTab tab, "jl ", label]
prettyInstruction tab (Jle label) = concat [insertTab tab, "jle ", label]
prettyInstruction tab (Call func) = concat [insertTab tab, "call ", func]
prettyInstruction tab Leave = insertTab tab ++ "leave"
prettyInstruction tab Ret = insertTab tab ++ "ret"


type Instructions = [(String, [Instruction])]


prettyInstructions :: Instructions -> String
prettyInstructions = concatMap (\(label, instrs) -> concat [".", label, ":\n", concatMap (prettyInstruction 1) instrs, "\n"])

