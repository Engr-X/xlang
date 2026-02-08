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
    Bool | Char | Void |
    Array Class Int |
    Class [String] [Class] -- name + general
    deriving (Eq, Ord, Show)


-- Better toString of class instance
prettyClass :: Class -> String
prettyClass Int8T = "byte"
prettyClass Int16T = "short"
prettyClass Int32T = "int"
prettyClass Int64T = "long"
prettyClass Float32T = "float"
prettyClass Float64T = "double"
prettyClass Float128T = "float128"
prettyClass Void = "void"
prettyClass Bool = "bool"
prettyClass Char = "char"
prettyClass (Array c _) = "Array<" ++ prettyClass c ++ ">"
prettyClass (Class ss args) =
    let base = intercalate "." ss
    in case args of
        [] -> base
        _  -> concat [base, "<", intercalate ", " (map prettyClass args), ">"]


-- | Control-flow commands that can appear as expressions.
--   Used for statements such as return, break, and continue.
data Command = Continue | Break | Return (Maybe Expression)
    deriving (Eq, Show)


-- pretty toString method for command
prettyCmd :: Int -> Command -> String
prettyCmd n Continue = insertTab n ++  "continue"
prettyCmd n Break = insertTab n ++ "break"
prettyCmd n (Return Nothing) = insertTab n ++ "return;"
prettyCmd n (Return e) = concat [insertTab n, "return ", prettyExpr 0 e]


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


-- Better toString for op
prettyOp :: Operator -> String
prettyOp k = fromMaybe (error "cannot find the error") (Map.lookup k operatorTextMap)


-- | Abstract syntax tree for expressions.
--   Covers literals, variables, casts, unary/binary operations, and errors.
data Expression = 
    Error [Token] String
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
    | Call Expression (Maybe [(Class, [Token])]) [Expression]
    deriving (Eq, Show)


-- | toString in human version
prettyExpr :: Int -> Maybe Expression -> String
prettyExpr n me = insertTab n ++ prettyExpr' me
    where
        prettyExpr' :: Maybe Expression -> String
        prettyExpr' Nothing = ""
        prettyExpr' (Just (Error t why)) = "error at: " ++ show t ++ " " ++ why
       -- prettyExpr' (Just (Command c _)) = prettyCmd 0 c
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
        prettyExpr' (Just (Call callee mTypeArgs args)) =
            let calleeS = prettyExpr' (Just callee)
                typeArgsS = case mTypeArgs of
                    Nothing -> ""
                    Just ts ->
                        "<" ++ intercalate ", " (map (prettyClass . fst) ts) ++ ">"
                argsS = intercalate ", " (map (prettyExpr' . Just) args)
            in calleeS ++ typeArgsS ++ "(" ++ argsS ++ ")"


-- | Choose an anchor token for diagnostics.
exprTokens :: Expression -> [Token]
exprTokens (Error ts _) = ts
exprTokens (IntConst _ t) = [t]
exprTokens (LongConst _ t) = [t]
exprTokens (FloatConst _ t) = [t]
exprTokens (DoubleConst _ t) = [t]
exprTokens (LongDoubleConst _ t) = [t]
exprTokens (CharConst _ t) = [t]
exprTokens (StringConst _ t) = [t]
exprTokens (BoolConst _ t) = [t]
exprTokens (Variable _ t) = [t]
exprTokens (Qualified _ ts) = ts
exprTokens (Cast (_, toks) e t) = exprTokens e ++ (t : toks)
exprTokens (Unary _ e t) = t : exprTokens e
exprTokens (Binary _ e1 e2 t) = t : (exprTokens e1 ++ exprTokens e2)
exprTokens (Call e1 Nothing es) = concatMap exprTokens (e1 : es)
exprTokens (Call e1 (Just cts) es) = concatMap snd cts ++ concatMap exprTokens (e1 : es)




-- | Flatten all expressions contained in a expr.
--   Useful for traversal, analysis, or validation passes.
flattenExpr :: Maybe Expression -> [Expression]
flattenExpr Nothing = []
flattenExpr (Just fatherE@(Cast _ e2 _)) = [fatherE, e2]
flattenExpr (Just fatherE@(Unary _ e _)) = [fatherE, e]
flattenExpr (Just fatherE@(Binary _ e1 e2 _)) = [fatherE, e1, e2]
flattenExpr (Just fatherE@(Call callee _ args)) = fatherE : flattenExpr (Just callee) ++ concatMap (flattenExpr . Just) args
flattenExpr (Just e) = [e]


-- | Check whether an expression represents a syntax or semantic error.
isErrExpr :: Expression -> Bool
isErrExpr (Error _ _) = True
isErrExpr _ = False


-- | Statement block representation.
--  A block may contain a single statement or multiple statements.
newtype Block = Multiple [Statement]
    deriving (Eq, Show)


-- Better toString method for block
prettyBlock :: Int -> Block -> String
prettyBlock n (Multiple ss) = let space = insertTab n in unlines [space ++ "{", concatMap (prettyStmt (n + 1) . Just) ss, space ++ "}"]

-- toString in console
prettyBlockIO :: Block -> IO String
prettyBlockIO = pure . prettyBlock 0


-- | Collect all tokens contained in a block (recursively).
--   This traverses each statement in the block and aggregates
--   the tokens found in nested statements/expressions.
blockTokens :: Maybe Block -> [Token]
blockTokens Nothing = []
blockTokens (Just (Multiple ss)) = concatMap stmtTokens ss


-- | Flatten all expressions contained in a block.
--   Useful for traversal, analysis, or validation passes.
flattenBlock :: Maybe Block -> [Expression]
flattenBlock Nothing = []
flattenBlock (Just (Multiple ss)) = concatMap (flattenStatement . Just) ss


-- | Switch-case representation.
--   A case may have an expression or be a default case.
data SwitchCase =
    Case Expression (Maybe Block) Token | -- case keyword
    Default Block Token -- default keyword
    deriving (Eq, Show)


-- | pretty toString version for switch case
prettySwitchCase :: Int -> SwitchCase -> String
prettySwitchCase n (Case e b _)
    | isNothing b = concat [space, "case: ", prettyExpr 0 (Just e), ":\n"]
    | otherwise = unlines [concat ["case ", prettyExpr 0 (Just e), ":"],
        space ++ "{", prettyBlock (n + 1) (fromMaybe (error "this is impossible") b), space ++ "}"]
    where
        space :: String
        space = insertTab n
prettySwitchCase n (Default b _) = unlines [space ++ "default: ", space ++ "{", prettyBlock (n + 1) b, space ++ "}"]
    where
        space :: String
        space = insertTab n

-- toString in console
prettySwitchCaseIO :: SwitchCase -> IO String
prettySwitchCaseIO = pure . prettySwitchCase 0


-- | Collect all tokens contained in a switch case (recursively).
--   Includes the case expression (if any) and tokens from the case block.
--   For 'Default', only the block tokens are returned.
switchCaseTokens :: SwitchCase -> [Token]
switchCaseTokens (Case e mb _) = exprTokens e ++ blockTokens mb
switchCaseTokens (Default b _) = blockTokens (Just b)


-- | Extract all expressions from a switch case.
--   Includes the case condition and expressions in its block.
flattenCase :: Maybe SwitchCase -> [Expression]
flattenCase Nothing = []
flattenCase (Just (Case e Nothing _)) = [e]
flattenCase (Just (Case e b _)) = e : flattenBlock b
flattenCase (Just (Default b _)) = flattenBlock (Just b)


-- | Abstract syntax tree for statements.
--   Supports control flow constructs and expression statements.
data Statement = 
    Command Command Token |
    Expr Expression |
    BlockStmt Block |
    If Expression (Maybe Block) (Maybe Block) (Token, Maybe Token) | -- (if keyword - maybe else)
    For (Maybe Expression, Maybe Expression, Maybe Expression) (Maybe Block) Token | -- (for keyword)
    While Expression (Maybe Block) (Maybe Block) (Token, Maybe Token) | -- while else -- (while keyword - maybe else keyword)
    DoWhile (Maybe Block) Expression (Maybe Block) (Token, Token, Maybe Token) | -- (do keyword, while keyword, maybe else keyword)
    Switch Expression [SwitchCase] Token | -- (switch keyword)
    Function (Class, [Token]) Expression (Maybe [(Class, [Token])]) [(Class, String, [Token])] Block
    -- function: return_type + pos, name, template params + position, params + position, body
    deriving (Eq, Show)


-- beter toString for string instance
prettyStmt :: Int -> Maybe Statement -> String
prettyStmt _ Nothing = "\n"
prettyStmt n (Just (Command c _)) = prettyCmd n c
prettyStmt n (Just (Expr e)) = prettyExpr n (Just e) ++ "\n"
prettyStmt n (Just (BlockStmt b)) = prettyBlock n b

-- if 
prettyStmt n (Just (If expr Nothing Nothing _)) = concat [insertTab n, "if (", prettyExpr 0 (Just expr), ");\n"]
prettyStmt n (Just (If expr (Just b) Nothing _)) = let
    body = prettyBlock n b
    space = insertTab n in
    unlines [concat [space, "if (", prettyExpr 0 (Just expr), ")"], body]
prettyStmt n (Just (If expr Nothing (Just b) _)) = let
    body = prettyBlock n b
    space = insertTab n in
    unlines [concat [insertTab n, "if (", prettyExpr 0 (Just expr), ");"], space ++ "else", body]
prettyStmt n (Just (If expr (Just a) (Just b) _)) = let
    (body1, body2) = (prettyBlock n a, prettyBlock n b)
    space = insertTab n in 
    unlines [concat [space, "if (", prettyExpr 0 (Just expr), ")"], body1, space ++ "else", body2]

-- for loop
prettyStmt n (Just (For (s1, s2, s3) Nothing _)) = let s = intercalate ";" $ map (prettyExpr 0) [s1, s2, s3] in
    concat [insertTab n, "for(", s, ");\n"]
prettyStmt n (Just (For (s1, s2, s3) (Just b) _)) = let s = intercalate ";" $ map (prettyExpr 0) [s1, s2, s3] in
    unlines [concat [insertTab n, "for(", s, ")"], prettyBlock n b]

-- while
prettyStmt n (Just (While e Nothing Nothing _)) = concat [insertTab n, "while(", prettyExpr 0 (Just e), ");\n"]
prettyStmt n (Just (While e Nothing (Just b) _)) = let space = insertTab n in
    unlines [concat [insertTab n, "while(", prettyExpr 0 (Just e), ");\n"], space ++ "else", prettyBlock n b]
prettyStmt n (Just (While e (Just b) Nothing _)) = unlines [concat [insertTab n, "while(", prettyExpr 0 (Just e), ")"], prettyBlock n b]
prettyStmt n (Just (While e (Just b1) (Just b2) _)) = let space = insertTab n in
    unlines [concat [insertTab n, "while(", prettyExpr 0 (Just e), ")"], prettyBlock n b1, space ++ "else", prettyBlock n b2]

-- dowhile
prettyStmt n (Just (DoWhile Nothing e Nothing _)) = let space = insertTab n in
    unlines [space ++ "do", concat [space, "while(", prettyExpr 0 (Just e), ")"]]
prettyStmt n (Just (DoWhile Nothing e (Just b) _)) = let space = insertTab n in
    unlines [space ++ "do",  concat [space, prettyBlock n b, "while(", prettyExpr 0 (Just e), ")"], space ++ "else", prettyBlock n b]
prettyStmt n (Just (DoWhile (Just b) e Nothing _)) = let space = insertTab n in
    unlines [space ++ "do",  concat [space, prettyBlock n b, "while(", prettyExpr 0 (Just e), ")"]]
prettyStmt n (Just (DoWhile (Just b1) e (Just b2) _)) = let space = insertTab n in
        unlines [space ++ "do", concat [space, prettyBlock n b1, "while(", prettyExpr 0 (Just e), ")"],
            space ++ "else", prettyBlock n b2]

prettyStmt n (Just (Switch e xs _)) = let space = insertTab n in unlines
    [concat [space, "switch(", prettyExpr n (Just e), ")"], space ++ "{", concatMap (prettySwitchCase (n + 1)) xs,space ++ "}" ]


prettyStmt n (Just (Function (retC, _) functionName mGenParams params b)) =
    let space = insertTab n
        prettyGen :: Maybe [(Class, [Token])] -> String
        prettyGen Nothing = ""
        prettyGen (Just xs) = "<" ++ intercalate ", " (map (prettyClass . fst) xs) ++ ">"

        prettyOneParam :: (Class, String, [Token]) -> String
        prettyOneParam (c, name, _) = prettyClass c ++ " " ++ name

    in unlines [
        concat [
            space,
            prettyClass retC,
            " ",
            prettyExpr 0 (Just functionName),
            prettyGen mGenParams,
            "(",
            intercalate ", " (map prettyOneParam params),
            ")"],
        prettyBlock n b]

-- | pretty statement in IO version
prettyStmtIO :: Maybe Statement -> IO String
prettyStmtIO = pure . prettyStmt 0


-- | Collect all tokens contained in a statement (recursively).
--   This includes tokens from nested expressions and blocks.
stmtTokens :: Statement -> [Token]
stmtTokens (Command _ t) = [t]
stmtTokens (Expr e) = exprTokens e
stmtTokens (BlockStmt b) = blockTokens (Just b)

stmtTokens (If e b1 b2 (ifTok, elseTok)) = concat [
    [ifTok], exprTokens e, blockTokens b1, maybe [] pure elseTok, blockTokens b2]

stmtTokens (For (e1, e2, e3) b forTok) = concat [
    [forTok], concatMap exprTokens (catMaybes [e1, e2, e3]), blockTokens b]

stmtTokens (While e b1 b2 (whileTok, elseTok)) = concat [
    [whileTok], exprTokens e, blockTokens b1,
    maybe [] pure elseTok, blockTokens b2]

stmtTokens (DoWhile b1 e b2 (doTok, whileTok, elseTok)) = concat [
    [doTok], blockTokens b1, 
    [whileTok], exprTokens e,
    maybe [] pure elseTok, blockTokens b2]

stmtTokens (Switch e scs switchTok) = concat [[switchTok], exprTokens e, concatMap switchCaseTokens scs]

stmtTokens (Function (_, retToks) name mGenParams params b) = concat [
    retToks, exprTokens name, maybe [] (concatMap snd) mGenParams,
    concatMap (\(_, _, toks) -> toks) params, blockTokens (Just b)]


-- | Flatten all expressions contained in a statement.
--   Recursively traverses nested blocks and control structures.
flattenStatement :: Maybe Statement -> [Expression]
flattenStatement Nothing = []
flattenStatement (Just (Command _ _)) = []
flattenStatement (Just (Expr e)) = flattenExpr (Just e)
flattenStatement (Just (BlockStmt b)) = flattenBlock (Just b)
flattenStatement (Just (If e b c _)) = e : (flattenBlock b ++ flattenBlock c)
flattenStatement (Just (For (e1, e2, e3) b _)) = catMaybes [e1, e2, e3] ++ flattenBlock b
flattenStatement (Just (While e b1 b2 _)) = e : (flattenBlock b1 ++ flattenBlock b2)
flattenStatement (Just (DoWhile b1 e b2 _)) = e : (flattenBlock b1 ++ flattenBlock b2)
flattenStatement (Just (Switch e scs _)) = e : concatMap (flattenCase . Just) scs
flattenStatement (Just (Function _ _ _ params b)) = concatMap one params ++ flattenBlock (Just b)
    where
        one :: (Class, String, [Token]) -> [Expression]
        one (_, name, toks) = case toks of
            [] -> []
            (t:_) -> [Variable name t]


-- | The declaration of a program, especially import and module
data Declaration = 
    Package [String] [Token] |
    Import [String] [Token]
    deriving (Eq, Show)


-- | Better toString method for declearation
prettyDeclaration :: Declaration -> String
prettyDeclaration (Package ss _) = "package " ++ intercalate "." ss
prettyDeclaration (Import ss _) = "Import " ++ intercalate "." ss


-- | Extract the path segments from a declaration.
declPath :: Declaration -> [String]
declPath (Package segs _) = segs
declPath (Import  segs _) = segs 


-- | Check whether a declaration is a package declaration.
isPackageDecl :: Declaration -> Bool
isPackageDecl (Package _ _) = True
isPackageDecl _ = False


-- | Check whether a declaration is an import declaration.
isImportDecl :: Declaration -> Bool
isImportDecl (Import _ _) = True
isImportDecl _ = False


-- | Program representation.
--   Consists of imported modules and a list of top-level statements.
type Program = ([Declaration], [Statement])


-- | Better toString value for program
prettyProgram :: Program -> IO String
prettyProgram (header, body) = case map prettyDeclaration header of
    [] -> (pure . mergeNewlines . concatMap (prettyStmt 0 . Just)) body
    ss -> pure (unlines ss ++ (mergeNewlines . concatMap (prettyStmt 0 . Just)) body)
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
    ("float128", Float128T),
    
    ("char", Char),
    ("void", Void)]


-- | Extract all error expressions from a program.
--   Traverses the entire syntax tree and filters expressions marked as errors.
getErrorProgram :: Program -> [Expression]
getErrorProgram = filter isErrExpr . flattenProgram


-- | Convert a type name into its corresponding Class.
--   Unknown names are treated as user-defined classes.
{-toClass :: [String] -> Class
toClass [s] = fromMaybe (Class [s]) $ Map.lookup s classesMap
toClass ss = Class ss-}


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
