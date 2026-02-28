module Lowing.JVM where


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
    JD Double
    -- JString String |
    -- JRef String
    deriving (Eq, Show)

data ByteCodeOP =
    Nop |                                   -- stack: ... -> ...
    Pop |                                   -- stack: ..., x -> ...
    Dup |                                   -- stack: ..., x -> ..., x, x
    CPush JValue |                          -- stack: ... -> ..., c
    PushNull |                              -- stack: ... -> ..., null


    Store Int |                             -- stack: ..., v -> ... (store local i)
    Load Int |                              -- stack: ... -> ..., v (load local i)
    LoadLocal |                             -- stack: ... -> ..., v (load implicit local)
    StoreLocal |                            -- stack: ..., v -> ... (store implicit local)

    Ifne Int |                              -- stack: ..., v -> ... (jump if v /= 0)
    Goto Int |                              -- stack: ... -> ...
    Return |                                -- stack: ... -> ... (void return)
    IReturn |                               -- stack: ..., v -> ... (return v)

    Cast JType JType |                      -- stack: ..., v -> ..., v' (cast)

    Add | Sub | Mul | Div | Rem |           -- stack: ..., a, b -> ..., (a op b)
    Neg |                                   -- stack: ..., a -> ..., (-a)
    And | Or | Xor |                        -- stack: ..., a, b -> ..., (a op b)
    Shl | Shr | UShr                        -- stack: ..., a, s -> ..., (a << or >> s)
    deriving (Eq, Show)


newtype ByteCodeBlock =
    Multiple [ByteCodeOP]
    deriving (Eq, Show)


specialOptimization :: IRProgm -> IRProgm
specialOptimization = id
