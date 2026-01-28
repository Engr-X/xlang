module Parse.SyntaxTree where

import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, catMaybes, isNothing)
import Lex.Token (Token)
import Util.Basic

import qualified Data.Map.Strict as Map
import qualified Lex.Token as Lex


-- | Language-level type representation.
--   Includes primitive types, arrays, and user-defined classes.
data Class = 
    Int8T | Int16T | Int32T | Int64T |
    Float32T | Float64T | Float128T |
    Bool | Char | 
    Array Class Int |
    Class [String]
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
prettyClass (Array c _) = "Array<" ++ prettyClass c ++ ">"
prettyClass (Class ss) = intercalate "." ss


-- | Control-flow commands that can appear as expressions.
--   Used for statements such as return, break, and continue.
data Command = Continue | Break | Return (Maybe Expression)
    deriving (Eq, Show)


-- pretty toString method for command
prettyCmd :: Int -> Command -> String
prettyCmd n Continue = insertTab n ++  "continue"
prettyCmd n Break = insertTab n ++ "break"
prettyCmd n (Return Nothing) = insertTab n ++ "return;"
prettyCmd n (Return e) = insertTab n ++ "return " ++ prettyExpr 0 e


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
    BitAnd | BitNot | 

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
    
    (BitXor, "^"), (BitXnor, "!^"), (BitAnd, "&"), (BitNot, "!"),
    
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

    -- this. expr must be Qualified of Variable
    | Cast (Class, [Token]) Expression Token
    | Unary Operator Expression Token
    | Binary Operator Expression Expression Token
    | Call [String] [Expression] [Token]
    deriving (Eq, Show)


-- | toString in human version
prettyExpr :: Int -> Maybe Expression -> String
prettyExpr n me = insertTab n ++ prettyExpr' me
    where
        prettyExpr' :: Maybe Expression -> String
        prettyExpr' Nothing = ""
        prettyExpr' (Just (Error t why)) = "error at: " ++ show t ++ " " ++ why
        prettyExpr' (Just (Command c _)) = prettyCmd 0 c
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
        prettyExpr' (Just (Cast (c, _) e _)) = '(': prettyClass c ++ (")(" ++ prettyExpr' (Just e) ++ ")")
        prettyExpr' (Just (Unary o e _)) = prettyOp o ++ prettyExpr' (Just e)
        prettyExpr' (Just (Binary o e1 e2 _)) = prettyExpr' (Just e1) ++ prettyOp o ++ prettyExpr' (Just e2)
        prettyExpr' (Just (Call fname exprs _)) = concat [concat fname, "(", intercalate ", " (map (prettyExpr' . Just) exprs), ")"]


-- | Flatten all expressions contained in a expr.
--   Useful for traversal, analysis, or validation passes.
flattenExpr :: Maybe Expression -> [Expression]
flattenExpr Nothing = []
flattenExpr (Just fatherE@(Cast _ e2 _)) = [fatherE, e2]
flattenExpr (Just fatherE@(Unary _ e _)) = [fatherE, e]
flattenExpr (Just fatherE@(Binary _ e1 e2 _)) = [fatherE, e1, e2]
flattenExpr (Just fatherE@(Call _ es _)) = fatherE : concatMap (flattenExpr . Just) es
flattenExpr (Just e) = [e]


-- | Check whether an expression represents a syntax or semantic error.
isErrExpr :: Expression -> Bool
isErrExpr (Error _ _) = True
isErrExpr _ = False


-- | Statement block representation.
--  A block may contain a single statement or multiple statements.
newtype Block = Multiple [Statement]
    deriving (Eq, Show)


-- better toString method for block
prettyBlock :: Int -> Block -> String
prettyBlock n (Multiple ss) = let space = insertTab n in unlines [space ++ "{", concatMap (prettyStmt (n + 1) . Just) ss, space ++ "}"]

-- toString in console
prettyBlockIO :: Block -> IO String
prettyBlockIO = pure . prettyBlock 0


-- | Flatten all expressions contained in a block.
--   Useful for traversal, analysis, or validation passes.
flattenBlock :: Maybe Block -> [Expression]
flattenBlock Nothing = []
flattenBlock (Just (Multiple ss)) = concatMap (flattenStatement . Just) ss


-- | Switch-case representation.
--   A case may have an expression or be a default case.
data SwitchCase = Case Expression (Maybe Block) | Default Block
    deriving (Eq, Show)


-- | pretty toString version for switch case
prettySwitchCase :: Int -> SwitchCase -> String
prettySwitchCase n (Case e b)
    | isNothing b = concat [space, "case: ", prettyExpr 0 (Just e), ":\n"]
    | otherwise = unlines [concat ["case ", prettyExpr 0 (Just e), ":"],
        space ++ "{", prettyBlock (n + 1) (fromMaybe (error "this is impossible") b), space ++ "}"]
    where
        space :: String
        space = insertTab n
prettySwitchCase n (Default b) = unlines [space ++ "default: ", space ++ "{", prettyBlock (n + 1) b, space ++ "}"]
    where
        space :: String
        space = insertTab n

-- toString in console
prettySwitchCaseIO :: SwitchCase -> IO String
prettySwitchCaseIO = pure . prettySwitchCase 0


-- | Extract all expressions from a switch case.
--   Includes the case condition and expressions in its block.
flattenCase :: Maybe SwitchCase -> [Expression]
flattenCase Nothing = []
flattenCase (Just (Case e Nothing)) = [e]
flattenCase (Just (Case e b)) = e : flattenBlock b
flattenCase (Just (Default b)) = flattenBlock (Just b)


-- | Abstract syntax tree for statements.
--   Supports control flow constructs and expression statements.
data Statement = 
    Expr Expression |
    BlockStmt Block |
    If Expression (Maybe Block) (Maybe Block) | -- if else
    For (Maybe Expression, Maybe Expression, Maybe Expression) (Maybe Block) |
    While Expression (Maybe Block) (Maybe Block) | -- while else
    DoWhile (Maybe Block) Expression (Maybe Block) |
    Switch Expression [SwitchCase]
    deriving (Eq, Show)


-- beter toString for string instance
prettyStmt :: Int -> Maybe Statement -> String
prettyStmt _ Nothing = "\n"
prettyStmt n (Just (Expr e)) = prettyExpr n (Just e) ++ "\n"
prettyStmt n (Just (BlockStmt b)) = prettyBlock n b

-- if 
prettyStmt n (Just (If expr Nothing Nothing)) = concat [insertTab n, "if (", prettyExpr 0 (Just expr), ");\n"]
prettyStmt n (Just (If expr (Just b) Nothing)) = let
    body = prettyBlock n b
    space = insertTab n in
    unlines [concat [space, "if (", prettyExpr 0 (Just expr), ")"], body]
prettyStmt n (Just (If expr Nothing (Just b))) = let
    body = prettyBlock n b
    space = insertTab n in
    unlines [concat [insertTab n, "if (", prettyExpr 0 (Just expr), ");"], space ++ "else", body]
prettyStmt n (Just (If expr (Just a) (Just b))) = let
    (body1, body2) = (prettyBlock n a, prettyBlock n b)
    space = insertTab n in 
    unlines [concat [space, "if (", prettyExpr 0 (Just expr), ")"], body1, space ++ "else", body2]

-- for loop
prettyStmt n (Just (For (s1, s2, s3) Nothing)) = let s = intercalate ";" $ map (prettyExpr 0) [s1, s2, s3] in
    concat [insertTab n, "for(", s, ");\n"]
prettyStmt n (Just (For (s1, s2, s3) (Just b))) = let s = intercalate ";" $ map (prettyExpr 0) [s1, s2, s3] in
    unlines [concat [insertTab n, "for(", s, ")"], prettyBlock n b]

-- while
prettyStmt n (Just (While e Nothing Nothing)) = concat [insertTab n, "while(", prettyExpr 0 (Just e), ");\n"]
prettyStmt n (Just (While e Nothing (Just b))) = let space = insertTab n in
    unlines [concat [insertTab n, "while(", prettyExpr 0 (Just e), ");\n"], space ++ "else", prettyBlock n b]
prettyStmt n (Just (While e (Just b) Nothing)) = unlines [concat [insertTab n, "while(", prettyExpr 0 (Just e), ")"], prettyBlock n b]
prettyStmt n (Just (While e (Just b1) (Just b2))) = let space = insertTab n in
    unlines [concat [insertTab n, "while(", prettyExpr 0 (Just e), ")"], prettyBlock n b1, space ++ "else", prettyBlock n b2]

-- dowhile
prettyStmt n (Just (DoWhile Nothing e Nothing)) = let space = insertTab n in
    unlines [space ++ "do", concat [space, "while(", prettyExpr 0 (Just e), ")"]]
prettyStmt n (Just (DoWhile Nothing e (Just b))) = let space = insertTab n in
    unlines [space ++ "do",  concat [space, prettyBlock n b, "while(", prettyExpr 0 (Just e), ")"], space ++ "else", prettyBlock n b]
prettyStmt n (Just (DoWhile (Just b) e Nothing)) = let space = insertTab n in
    unlines [space ++ "do",  concat [space, prettyBlock n b, "while(", prettyExpr 0 (Just e), ")"]]
prettyStmt n (Just (DoWhile (Just b1) e (Just b2))) = let space = insertTab n in
        unlines [space ++ "do", concat [space, prettyBlock n b1, "while(", prettyExpr 0 (Just e), ")"],
            space ++ "else", prettyBlock n b2]


prettyStmt n (Just (Switch e xs)) = let space = insertTab n in unlines
    [concat [space, "switch(", prettyExpr n (Just e), ")"], space ++ "{", concatMap (prettySwitchCase (n + 1)) xs,space ++ "}" ]

prettyStmtIO :: Maybe Statement -> IO String
prettyStmtIO = pure . prettyStmt 0


-- | Flatten all expressions contained in a statement.
--   Recursively traverses nested blocks and control structures.
flattenStatement :: Maybe Statement -> [Expression]
flattenStatement Nothing = []
flattenStatement (Just (Expr e)) = flattenExpr (Just e)
flattenStatement (Just (BlockStmt b)) = flattenBlock (Just b)
flattenStatement (Just (If e b c)) = e : (flattenBlock b ++ flattenBlock c)
flattenStatement (Just (For (e1, e2, e3) b)) = catMaybes [e1, e2, e3] ++ flattenBlock b
flattenStatement (Just (While e b1 b2)) = e : (flattenBlock b1 ++ flattenBlock b2)
flattenStatement (Just (DoWhile b1 e b2)) = e : (flattenBlock b1 ++ flattenBlock b2)
flattenStatement (Just (Switch e scs)) = e : concatMap (flattenCase . Just) scs


-- | Program representation.
--   Consists of imported modules and a list of top-level statements.
type Program = ([String], [Statement])


-- better toString value for program
prettyProgram :: Program -> IO String
prettyProgram = pure . mergeNewlines . concatMap (prettyStmt 0 . Just) . snd
    where
        mergeNewlines :: String -> String
        mergeNewlines = go False
            where
                go :: Bool -> String -> String
                go _ [] = []
                go prevNL (c:cs)
                    | c == '\n' = if prevNL then go True cs else '\n' : go True cs
                    | otherwise = c : go False cs


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
toClass :: [String] -> Class
toClass [s] = fromMaybe (Class [s]) $ Map.lookup s classesMap
toClass ss = Class ss


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


{--- Choose an anchor token for diagnostics.
exprTok :: Expression -> [Token]
exprTok (Error t _) = [t]
exprTok (Command _ t) = [t]
exprTok (IntConst _ t) = [t]
exprTok (LongConst _ t) = [t]
exprTok (FloatConst _ t) = [t]
exprTok (DoubleConst _ t) = [t]
exprTok (LongDoubleConst _ t) = [t]
exprTok (CharConst _ t) = [t]
exprTok (StringConst _ t) = [t]
exprTok (BoolConst _ t) = [t]
exprTok (Variable _ t) = [t]
exprTok (Qualified _ ts) = ts
exprTok (Cast (_) (_, t)) = [t]
exprTok (Unary _ _ t) = [t]
exprTok (Binary _ _ _ t) = [t]
exprTok (Call _ _ ts) = ts-}
