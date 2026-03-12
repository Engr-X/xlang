module Lowing.JVM where

import Data.Int (Int64)
import IR.TAC (IRProgm, MainKind)
import Parse.ParserBasic (Decl)
import Parse.SyntaxTree (Class)
import Semantic.NameEnv (QName)
import Semantic.TypeEnv (FunSig)


data JType = 
    JInt |
    JLong |
    JFloat |
    JDouble
    deriving (Eq, Show)

data JConst =
    JI Int |
    JL Int64 |
    JF Float |
    JD Double |
    JString String
    deriving (Eq, Show)

type JValue = JConst

data JOP =
    Nop |                                   -- stack: ... -> ...
    Pop |                                   -- stack: ..., x -> ...
    Dup |                                   -- stack: ..., x -> ..., x, x
    CPush JValue |                          -- stack: ... -> ..., c
    PushNull |                              -- stack: ... -> ..., null


    Store Class Int |                       -- stack: ..., v -> ... (store local i)
    Load Class Int |                        -- stack: ... -> ..., v (load local i)
    GetStatic Class QName |                 -- stack: ... -> ..., v (load static field)
    PutStatic Class QName |                 -- stack: ..., v -> ... (store static field)
    InvokeStatic QName FunSig |             -- stack: ..., args -> ..., ret (static call)

    -- compare two values and jump
    IfcmpEq Class Int | IfcmpNe Class Int | IfcmpLt Class Int | IfcmpLe Class Int | IfcmpGt Class Int | IfcmpGe Class Int |

    Ifne Int |                              -- stack: ..., v -> ... (jump if v /= 0)
    Goto Int |                              -- stack: ... -> ...
    Return |                                -- stack: ... -> ... (void return)
    ReturnWV Class |                        -- stack: ..., v -> ... (return with value)

    Cast Class Class |                      -- stack: ..., v -> ..., v' (cast)

    Add Class | Sub Class | Mul Class | Div Class | Rem Class |   -- stack: ..., a, b -> ..., (a op b)
    Neg Class |                               -- stack: ..., a -> ..., (-a)
    Not Class |
    And Class | Or Class | Xor Class |        -- stack: ..., a, b -> ..., (a op b)
    Shl Class | Shr Class | UShr Class        -- stack: ..., a, s -> ..., (a << or >> s)
    deriving (Eq, Show)


type JLabel = (Int, [JCommand])


data JCommand = 
    OP JOP |
    Label JLabel
    deriving (Eq, Show)


data JFunction = JFunction
    Decl            -- ^ declaration (access + flags)
    String          -- ^ function name
    FunSig          -- ^ function signature
    String          -- ^ owner type: xlang-class / xlang-top-level

    [JCommand]      -- ^ function body
    deriving (Eq, Show)

newtype JClinit = JClinit [JCommand] -- refer to static
    deriving (Eq, Show)

data JInit = JInit               -- refer to constractor
    Decl
    FunSig
    [JCommand]
    deriving (Eq, Show)


data JField = JField
    Decl
    Class           -- type
    String          -- name
    String          -- owner type: xlang-class / xlang-top-level



data JClass = JClass
    Decl
    QName           -- class Name
    QName           -- extend class
    [QName]         -- interface

    [JField]
    JClinit
    [JInit]
    [JFunction]
    MainKind        -- main entry metadata (NoMain/Main* with full qname)



specialOptimization :: IRProgm -> IRProgm
specialOptimization = id
