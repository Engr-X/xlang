module Parse.SyntaxTree where
    
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, catMaybes)
import Lex.Token (Token)

import qualified Data.Map.Strict as Map
import qualified Lex.Token as Lex


-- | Language-level type representation.
--   Includes primitive types, arrays, and user-defined classes.
data Class = 
    Int8T | Int16T | Int32T | Int64T |
    Float32T | Float64T | Float128T |
    Bool | Char | 
    Array Class Int |
    Class String
    deriving (Eq, Show)


-- better toString of class instance
prettyClass :: Class -> String
prettyClass Int8T = "byte"
prettyClass Int16T = "short"
prettyClass Int32T = "int"
prettyClass Int64T = "long"
prettyClass Float32T = "float"
prettyClass Float64T = "double"
prettyClass Float128T = "float128"
prettyClass Bool = "bool"
prettyClass Char = "char"
prettyClass (Array c l) = "Array<" ++ prettyClass c ++ ">"
prettyClass (Class s) = s


-- | Control-flow commands that can appear as expressions.
--   Used for statements such as return, break, and continue.
data Command = Continue | Break | Return (Maybe Expression)
    deriving (Eq, Show)


-- | Operators grouped by precedence level.
--   Includes assignment, arithmetic, bitwise, unary, and pointer operators.
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

    -- 10Cast
    Pow | UnaryPlus | UnaryMinus |

    -- 11
    IncSelf | DecSelf |

    -- 12
    SelfInc | SelfDec |

    -- 13
    AddrOf | DeRef
   
    deriving (Eq, Ord, Show)


operatorTextMap :: Map Operator String
operatorTextMap = Map.fromList [
    (Assign, "="), (BitLShiftAssign, "<<="), (BitRShiftAssign, ">>="), (BitOrAssign, "|="), (BitXorAssign, "^="),
    (BitXnorAssign, "!^="), (PlusAssign, "+="), (MinusAssign, "-="), (MultiplyAssign, "*="), (DivideAssign, "/="),
    (ModuloAssign, "%="), (PowerAssign, "**="),
    
    (Equal, "=="), (NotEqual, "!="),
    
    (GreaterThan, ">"), (LessThan, "<"), (GreaterEqual, ">="), (LessEqual, "<="),
    
    (BitRShift, ">>"), (BitLShift, "<<"), (BitOr, "|"),
    
    (BitXor, "^"), (BitXnor, "!^"), (BitAnd, "&"), (BitReverse, "!"),
    
    (Add, "+"), (Sub, "-"), (Mul, "*"), (Div, "/"), (Mod, "%"),
    
    (Pow, "**"), (UnaryPlus, "+"), (UnaryMinus, "-"),
    
    (IncSelf, "++"), (DecSelf, "--"),
    (SelfInc, "++"), (SelfDec, "--"),
    
    (AddrOf, "&"), (DeRef, "*")]


-- better toString for op
prettyOp :: Operator -> String
prettyOp k = fromMaybe (error "cannot find the error") (Map.lookup k operatorTextMap)


-- | Abstract syntax tree for expressions.
--   Covers literals, variables, casts, unary/binary operations, and errors.
data Expression = 
    Error Token String
    | Command Command Token
    
    | IntConst String Token
    | LongConst String Token
    | FloatConst String Token
    | DoubleConst String Token
    | LongDoubleConst String Token
    | CharConst Char Token
    | StringConst String Token
    | BoolConst Bool Token
    
    | Variable String Token
    | Qualified [String] [Token] -- eg: java.lang.math.PI

    | Cast Class Expression Token
    | Unary Operator Expression Token
    | Binary Operator Expression Expression Token
    deriving (Eq, Show)


-- | toString in human version
prettyExpr :: Int -> Maybe Expression -> String
prettyExpr n me = replicate (4 * n) ' ' ++ prettyExpr' me
    where
        prettyExpr' :: Maybe Expression -> String
        prettyExpr' Nothing = ""
        prettyExpr' (Just (Error t why)) = "error at: " ++ show t ++ " " ++ why
        prettyExpr' (Just (IntConst s _)) = s
        prettyExpr' (Just (LongConst s _)) = s
        prettyExpr' (Just (FloatConst s _)) = s
        prettyExpr' (Just (DoubleConst s _)) = s
        prettyExpr' (Just (LongDoubleConst s _)) = s
        prettyExpr' (Just (CharConst s _)) = show s
        prettyExpr' (Just (StringConst s _)) = show s
        prettyExpr' (Just (BoolConst b _)) = if b then "true" else "false"
        prettyExpr' (Just (Variable s _)) = s
        prettyExpr' (Just (Qualified ss _)) = intercalate "." ss
        prettyExpr' (Just (Cast c e _)) = '(': prettyClass c ++ (")(" ++ prettyExpr' (Just e) ++ ")")


-- | Check whether an expression represents a syntax or semantic error.
isErrExpr :: Expression -> Bool
isErrExpr (Error _ _) = True
isErrExpr _ = False


-- | Statement block representation.
--  A block may contain a single statement or multiple statements.
newtype Block = Multiple [Statement]
    deriving (Eq, Show)


-- | Flatten all expressions contained in a block.
--   Useful for traversal, analysis, or validation passes.
flattenBlock :: Maybe Block -> [Expression]
flattenBlock Nothing = []
flattenBlock (Just (Multiple ss)) = concatMap (flattenStatement . Just) ss


-- | Switch-case representation.
--   A case may have an expression or be a default case.
data SwitchCase = Case Expression Block | Default Block
    deriving (Eq, Show)


-- | Extract all expressions from a switch case.
--   Includes the case condition and expressions in its block.
flattenCase :: Maybe SwitchCase -> [Expression]
flattenCase Nothing = []
flattenCase (Just (Case e b)) = e : flattenBlock (Just b)
flattenCase (Just (Default b)) = flattenBlock (Just b)


-- | Abstract syntax tree for statements.
--   Supports control flow constructs and expression statements.
data Statement = 
    Expr Expression |
    BlockStmt Block |
    If Expression (Maybe Block) (Maybe Block) |
    For (Maybe Expression, Maybe Expression, Maybe Expression) (Maybe Block) |
    While Expression (Maybe Block) |
    DoWhile (Maybe Block) Expression |
    Switch Expression [SwitchCase]
    deriving (Eq, Show)


-- | Flatten all expressions contained in a statement.
--   Recursively traverses nested blocks and control structures.
flattenStatement :: Maybe Statement -> [Expression]
flattenStatement Nothing = []
flattenStatement (Just (Expr e)) = [e]
flattenStatement (Just (BlockStmt b)) = flattenBlock (Just b)
flattenStatement (Just (If e b1 b2)) = e : (flattenBlock b1 ++ flattenBlock b2)
flattenStatement (Just (For (e1, e2, e3) b)) = catMaybes [e1, e2, e3] ++ flattenBlock b
flattenStatement (Just (While e b)) = e : flattenBlock b
flattenStatement (Just (DoWhile b e)) = e : flattenBlock b
flattenStatement (Just (Switch e scs)) = e : concatMap (flattenCase . Just) scs


-- | Program representation.
--   Consists of imported modules and a list of top-level statements.
type Program = ([String], [Statement])


-- | Flatten all programs contained in a statement.
--   Recursively traverses nested statements and control structures.
flattenProgram :: Program -> [Expression]
flattenProgram (_, ss) = concatMap (flattenStatement . Just) ss


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


-- | Extract all error expressions from a program.
--   Traverses the entire syntax tree and filters expressions marked as errors.
getErrorProgram :: Program -> [Expression]
getErrorProgram = filter isErrExpr . flattenProgram


-- | Convert a type name into its corresponding Class.
--   Unknown names are treated as user-defined classes.
toClass :: String -> Class
toClass k = fromMaybe (Class k) $ Map.lookup k classesMap


-- Check a expresson is a variable or not
isVariable :: Expression -> Bool
isVariable (Variable _ _)  = True
isVariable (Qualified _ _) = True
isVariable _ = False


-- Extract payload from tokens (used by parser actions). for identity name
identText :: Token -> String
identText (Lex.Ident s _) = s
identText _ = error "identText: expected Ident token"


-- Extract payload from tokens (used by parser actions). for number
numText :: Token -> String
numText (Lex.NumberConst s _) = s
numText _ = error "numText: expected NumberConst token"


-- Extract payload from tokens (used by parser actions). for char
charVal :: Token -> Char
charVal (Lex.CharConst c _) = c
charVal _ = error "charVal: expected CharConst token"


-- Extract payload from tokens (used by parser actions). for str
strVal :: Token -> String
strVal (Lex.StrConst s _) = s
strVal _ = error "strVal: expected StrConst token"


-- Choose an anchor token for diagnostics.
exprTok :: Expression -> Token
exprTok (Error t _) = t
exprTok (Command _ t) = t
exprTok (IntConst _ t) = t
exprTok (LongConst _ t) = t
exprTok (FloatConst _ t) = t
exprTok (DoubleConst _ t) = t
exprTok (LongDoubleConst _ t) = t
exprTok (CharConst _ t) = t
exprTok (StringConst _ t) = t
exprTok (BoolConst _ t) = t
exprTok (Variable _ t) = t
exprTok (Qualified _ ts) = last ts
exprTok (Cast _ _ t) = t
exprTok (Unary _ _ t) = t
exprTok (Binary _ _ _ t) = t


-- Convert a statement into a block.
-- If it's already a block statement, reuse it; otherwise wrap it.
stmtToBlock :: Statement -> Block
stmtToBlock (BlockStmt b) = b
stmtToBlock s = Multiple [s]
