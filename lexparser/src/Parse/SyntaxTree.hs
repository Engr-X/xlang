module Parse.SyntaxTree where
    
import Lex.Token (Token)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)

import qualified Data.Map.Strict as Map


data Class = Unknown |
    Int8T | Int16T | Int32T | Int64T |
    Float16T | Float32T | Float64T | Float128T |
    Bool | Char | 
    Array Class Int |
    Class String
    deriving (Eq, Show)


data Command = Continue | Break | Return (Maybe Expression)
    deriving (Eq, Show)


data Operator = 
    -- 0
    Assign | BitLShiftAssign | BitRShiftAssign | BitOrAssign | BitXorAssign | BitXnorAssign |
    PlusAssign | MinusAssign | MultiplyAssign | DivideAssign | ModuloAssign | PowerAssign | 

    -- 1
    Equal | NotEqual |
    
    -- 2
    GreaterThan | LessThan | GreaterEqual | LessEqual |

    -- 3
    BitRShift | BitLShift |

    -- 4
    BitOr |

    -- 5
    BitXor | BitXnor | 
    
    -- 6
    BitAnd | BitReverse | 

    -- 7
    Add | Sub | 
    
    -- 8
    Mul | Div | 

    -- 9
    Mod | 

    -- 10
    Pow | UnaryPlus | UnaryMinus |

    -- 11
    IncSelf | DecSelf |

    -- 12
    SelfInc | SelfDec |

    -- 13
    AddrOf | DeRef
   
    deriving (Eq, Show)


data Expression = 
    Error Token String| Empty | Command Command |


    IntConst String | LongConst String | FloatConst String | DoubleConst String |
    LongDoubleConst String | CharConst Char | StringConst String | BoolConst Bool |

    Variable String |
    Qualified [String] | -- eg: java.lang.math.PI

    Cast Class Expression |
    Unary Operator Expression |
    Binary Operator Expression Expression
    deriving (Eq, Show)


-- To check this expression is error or not
isErrExpr :: Expression -> Bool
isErrExpr (Error _ _) = True
isErrExpr _ = False


data Block =
    Single Statement | Multiple [Statement] |
    Case Int Block | Default Block
    deriving (Eq, Show)


data Statement = 
    Expr Expression |
    If Expression Block Block |
    For (Expression, Expression, Expression) Block |
    While Expression Block |
    Switch Expression [Block]
    deriving (Eq, Show)


type Program = ([String], [Statement])


classesMap :: Map String Class
classesMap = Map.fromList [
    ("bool", Bool),
    ("byte", Int8T), ("int8", Int8T),
    ("short", Int16T), ("int16", Int16T),
    ("int", Int32T), ("int32", Int32T), 
    ("long", Int32T), ("int64", Int64T),
    
    ("float", Float32T), ("float32", Float32T),
    ("double", Float64T), ("float64", Float64T),
    ("float128", Float128T)]


toClass :: String -> Class
toClass k = fromMaybe (Class k) $ Map.lookup k classesMap

