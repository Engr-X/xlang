{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Parse.SyntaxTree where

import Data.List (intercalate, isPrefixOf, stripPrefix, nub, sort, foldl')
import Data.Map.Strict (Map)
import Data.Maybe (listToMaybe, fromMaybe, isNothing, isJust, mapMaybe)
import Data.Char (isDigit, toLower, isSpace)
import Data.HashSet (HashSet)
import Data.Set (Set)
import Data.Hashable (Hashable(..))
import GHC.Generics (Generic)
import Lex.Token (Token, tokenPos)

import qualified Data.Map.Strict as Map
import qualified Data.HashSet as HashSet
import qualified Data.Set as Set
import qualified Lex.Token as Lex


insertTab :: Int -> String
insertTab n = replicate (n * 4) ' '


splitLast :: [a] -> ([a], a)
splitLast [] = error "splitLast: empty list"
splitLast (x : xs) = go [] x xs
    where
        go acc lastOne [] = (reverse acc, lastOne)
        go acc lastOne (y : ys) = go (lastOne : acc) y ys


-- | Language-level type representation.
--   Includes primitive and user-defined classes.
data Class =
    Int8T | Int16T | Int32T | Int64T |
    Float32T | Float64T | Float128T |
    Bool | Char | Void |
    Pointer Class |
    FuncPtr Class [Class] |
    Blob Expression |
    Class [String] [Class] | -- name + general
    ErrorClass
    deriving (Eq, Show, Generic)

instance Ord Class where
    compare a b = compare (show a) (show b)

instance Hashable Class where
    hashWithSalt s cls = hashWithSalt s (show cls)


-- Better toString of class instance
prettyClass :: Class -> String
prettyClass ErrorClass = "error"
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
prettyClass (Pointer c) = concat ["pointer<", prettyClass c, ">"]
prettyClass (FuncPtr ret args) =
    concat ["(", intercalate ", " (map prettyClass args), ") -> ", prettyClass ret]
prettyClass (Blob e) = concat ["blob[", prettyExpr 0 (Just e), "]"]
prettyClass (Class ss args) =
    let base = intercalate "." ss
    in case args of
        [] -> base
        _  -> concat [base, "<", intercalate ", " (map prettyClass args), ">"]


classMangle :: Class -> String
classMangle Int8T = "b"
classMangle Int16T = "s"
classMangle Int32T = "i"
classMangle Int64T = "j"
classMangle Float32T = "f"
classMangle Float64T = "d"
classMangle Float128T = "q"
classMangle Bool = "z"
classMangle Char = "c"
classMangle Void = "v"
classMangle (Pointer c) = "P" ++ classMangle c
classMangle (FuncPtr ret args) = concat ["FI", concatMap classMangle args, "E", classMangle ret]
classMangle (Blob _) = "B"
classMangle (Class ss args) =
    case ss of
        [] -> error "classMangle: empty class qname"
        [one] ->
            let templ = templatePart args
            in encodeIdent one ++ templ
        _ ->
            let templ = templatePart args
            in concat ["N", concatMap encodeIdent ss, templ, "E"]
    where
        templatePart :: [Class] -> String
        templatePart [] = ""
        templatePart ts = concat ["I", concatMap classMangle ts, "E"]

        encodeIdent :: String -> String
        encodeIdent s = show (length s) ++ s

classMangle ErrorClass = error "cannot mangle error class"


classDemangleEither :: String -> Either String Class
classDemangleEither raw = case parseMangledType raw of
    Left msg -> Left msg
    Right (cls, "") -> Right cls
    Right (_, rest) -> Left ("trailing chars '" ++ rest ++ "'")
    where
        parseMangledType :: String -> Either String (Class, String)
        parseMangledType [] = Left "empty input"
        parseMangledType (c : cs) = case c of
            'b' -> Right (Int8T, cs)
            's' -> Right (Int16T, cs)
            'i' -> Right (Int32T, cs)
            'j' -> Right (Int64T, cs)
            'f' -> Right (Float32T, cs)
            'd' -> Right (Float64T, cs)
            'q' -> Right (Float128T, cs)
            'z' -> Right (Bool, cs)
            'c' -> Right (Char, cs)
            'v' -> Right (Void, cs)
            'P' -> do
                (inner, rest) <- parseMangledType cs
                Right (Pointer inner, rest)
            'F' -> parseFuncPtrType cs
            'B' -> Right (Blob (IntConst "1" Lex.dummyToken), cs)
            'N' -> parseNestedType cs
            _
                | isDigit c -> parseSingleType (c : cs)
                | otherwise -> Left ("unexpected tag '" ++ [c] ++ "'")

        parseFuncPtrType :: String -> Either String (Class, String)
        parseFuncPtrType s = case s of
            ('I' : rest0) -> do
                (args, rest1) <- parseFuncPtrArgs [] rest0
                (retTy, rest2) <- parseMangledType rest1
                Right (FuncPtr retTy args, rest2)
            _ ->
                Left "function pointer mangle must start with FI"

        parseFuncPtrArgs :: [Class] -> String -> Either String ([Class], String)
        parseFuncPtrArgs acc s = case s of
            [] -> Left "unexpected eof while parsing function pointer args"
            ('E' : rest) -> Right (reverse acc, rest)
            _ -> do
                (argTy, rest) <- parseMangledType s
                parseFuncPtrArgs (argTy : acc) rest

        parseNestedType :: String -> Either String (Class, String)
        parseNestedType s = do
            (scopes, rest0) <- parseScopes [] s
            if null scopes
                then Left "empty nested scope"
                else do
                    (args, rest1) <- parseTemplateMaybe rest0
                    case rest1 of
                        ('E' : rest2) -> Right (Class scopes args, rest2)
                        _ -> Left "missing closing 'E' for nested type"

        parseSingleType :: String -> Either String (Class, String)
        parseSingleType s = do
            (name, rest0) <- parseIdent s
            (args, rest1) <- parseTemplateMaybe rest0
            Right (Class [name] args, rest1)

        parseScopes :: [String] -> String -> Either String ([String], String)
        parseScopes acc s = case s of
            [] -> Left "unexpected eof while parsing nested scope"
            ('I' : _) -> Right (reverse acc, s)
            ('E' : _) -> Right (reverse acc, s)
            _ -> do
                (one, rest) <- parseIdent s
                parseScopes (one : acc) rest

        parseTemplateMaybe :: String -> Either String ([Class], String)
        parseTemplateMaybe ('I' : rest) = parseTemplateArgs [] rest
        parseTemplateMaybe s = Right ([], s)

        parseTemplateArgs :: [Class] -> String -> Either String ([Class], String)
        parseTemplateArgs acc s = case s of
            [] -> Left "unexpected eof while parsing template args"
            ('E' : rest) ->
                if null acc
                    then Left "empty template args"
                    else Right (reverse acc, rest)
            _ -> do
                (arg, rest) <- parseMangledType s
                parseTemplateArgs (arg : acc) rest

        parseIdent :: String -> Either String (String, String)
        parseIdent s = case span isDigit s of
            ("", _) -> Left "missing identifier length"
            (lenTxt, rest0) ->
                let n = read lenTxt :: Int
                in if n <= 0
                    then Left ("invalid identifier length: " ++ lenTxt)
                    else if length rest0 < n
                        then Left "identifier shorter than encoded length"
                    else
                        let (name, rest1) = splitAt n rest0
                        in Right (name, rest1)

classDemangle :: String -> Class
classDemangle raw = case classDemangleEither raw of
    Right cls -> cls
    Left msg -> error ("classDemangle: " ++ msg ++ ", input=" ++ raw)


blobSizeExprMaybe :: Class -> Maybe Expression
blobSizeExprMaybe (Blob e) = Just e
blobSizeExprMaybe _ = Nothing


blobConstSizeMaybe :: Class -> Maybe Int
blobConstSizeMaybe cls = case cls of
    Blob e -> evalInt e
    _ -> Nothing
  where
    evalInt :: Expression -> Maybe Int
    evalInt ex = case ex of
        IntConst s _ -> parseIntLiteral s
        LongConst s _ -> parseIntLiteral s
        Unary UnaryPlus inner _ -> evalInt inner
        Unary UnaryMinus inner _ -> negate <$> evalInt inner
        Binary Add a b _ -> (+) <$> evalInt a <*> evalInt b
        Binary Sub a b _ -> (-) <$> evalInt a <*> evalInt b
        Binary Mul a b _ -> (*) <$> evalInt a <*> evalInt b
        Binary Div a b _ -> do
            lhs <- evalInt a
            rhs <- evalInt b
            if rhs == 0 then Nothing else Just (lhs `div` rhs)
        Binary Mod a b _ -> do
            lhs <- evalInt a
            rhs <- evalInt b
            if rhs == 0 then Nothing else Just (lhs `mod` rhs)
        Cast _ inner _ -> evalInt inner
        _ -> Nothing

    parseIntLiteral :: String -> Maybe Int
    parseIntLiteral raw =
        let s0 = if not (null raw) && (last raw == 'l' || last raw == 'L')
                    then init raw
                    else raw
            (signN, body0) = case s0 of
                ('+':xs) -> (1 :: Integer, xs)
                ('-':xs) -> ((-1) :: Integer, xs)
                _ -> (1 :: Integer, s0)
            body = map toLower body0
            parsed :: Maybe Integer
            parsed
                | "0x" `isPrefixOf` body =
                    case reads ("0x" ++ drop 2 body) :: [(Integer, String)] of
                        [(n, "")] -> Just n
                        _ -> Nothing
                | otherwise =
                    case reads body :: [(Integer, String)] of
                        [(n, "")] -> Just n
                        _ -> Nothing
        in do
            n <- parsed
            let signedN = signN * n
                lo = toInteger (minBound :: Int)
                hi = toInteger (maxBound :: Int)
            if signedN < lo || signedN > hi
                then Nothing
                else Just (fromInteger signedN)





-- | Control-flow commands that can appear as expressions.
--   Used for statements such as return, break, and continue.
data Command = Pass | Continue | Break | Return (Maybe Expression)
    deriving (Eq, Show)


methodView :: Statement -> Maybe ((Class, [Token]), Expression, [(Class, String, [Token])], Block)
methodView (InstanceMethod ret name params body) = Just (ret, name, params, body)
methodView (StaticMethod ret name params body) = Just (ret, name, params, body)
methodView _ = Nothing


methodTView :: Statement -> Maybe ((Class, [Token]), Expression, [(Class, [Token])], [(Class, String, [Token])], Block)
methodTView (InstanceMethodT ret name gens params body) = Just (ret, name, gens, params, body)
methodTView (StaticMethodT ret name gens params body) = Just (ret, name, gens, params, body)
methodTView _ = Nothing


pattern Function :: (Class, [Token]) -> Expression -> [(Class, String, [Token])] -> Block -> Statement
pattern Function ret name params body <- (methodView -> Just (ret, name, params, body))
    where
        Function ret name params body = InstanceMethod ret name params body


pattern FunctionT :: (Class, [Token]) -> Expression -> [(Class, [Token])] -> [(Class, String, [Token])] -> Block -> Statement
pattern FunctionT ret name gens params body <- (methodTView -> Just (ret, name, gens, params, body))
    where
        FunctionT ret name gens params body = InstanceMethodT ret name gens params body


-- pretty toString method for command
prettyCmd :: Int -> Command -> String
prettyCmd n Pass = insertTab n ++ "pass"
prettyCmd n Continue = insertTab n ++ "continue"
prettyCmd n Break = insertTab n ++ "break"
prettyCmd n (Return Nothing) = insertTab n ++ "return;"
prettyCmd n (Return e) = concat [insertTab n, "return ", prettyExpr 0 e]


-- | Operators grouped by precedence level.
--   Includes assignment, arithmetic, bitwise, unary, and pointer operators.
data Operator =
    -- 0: atom/call/cast (no operator)

    -- 1: postfix ++/--
    SelfInc | SelfDec |

    -- 2: prefix ++/-- (and pointer prefix ops)
    IncSelf | DecSelf | AddrOf | DeRef |

    -- 3: unary plus/minus
    UnaryPlus | UnaryMinus |

    -- 4: unary logical/bit not ('!' / 'inv')
    LogicalNot | BitInv |

    -- 5: power
    Pow |

    -- 6: multiplicative
    Mul | Div | Mod |

    -- 7: additive
    Add | Sub |

    -- 8: shift
    BitLShift | BitRShift | BitURShift |

    -- 9: relational
    GreaterThan | LessThan | GreaterEqual | LessEqual |

    -- 10: equality
    Equal | NotEqual |

    -- 11: bit-and
    BitAnd | BitNand |

    -- 12: bit-xor / bit-xnor
    BitXor | BitXnor |

    -- 13: bit-or / bit-nor
    BitOr | BitNor |

    -- 14: bit-implication
    BitImply | BitNimply |

    -- 15: logical-and
    LogicalAnd | LogicalNand |

    -- 16: logical-or
    LogicalOr | LogicalNor |

    -- 17: logical-implication (reserved)
    LogicalImply | LogicalNimply |

    -- 18: assignment
    Assign | PlusAssign | MinusAssign | MultiplyAssign | DivideAssign | ModuloAssign | PowerAssign

    deriving (Eq, Ord, Enum, Show)


instance Hashable Operator where
    hashWithSalt salt = hashWithSalt salt . fromEnum


operatorTextMap :: Map Operator String
operatorTextMap = Map.fromList [
    (Assign, "="), (PlusAssign, "+="), (MinusAssign, "-="), (MultiplyAssign, "*="), (DivideAssign, "/="),
    (ModuloAssign, "%="), (PowerAssign, "**="),

    (BitImply, "implies"), (BitNimply, "nimplies"),
    (Equal, "=="), (NotEqual, "!="),

    (GreaterThan, ">"), (LessThan, "<"), (GreaterEqual, ">="), (LessEqual, "<="),

    (BitRShift, ">>"), (BitLShift, "<<"), (BitURShift, ">>>"),

    (LogicalOr, "|"), (LogicalNor, "!||"), (LogicalAnd, "&"), (LogicalNand, "!&&"),
    (LogicalImply, "->"), (LogicalNimply, "!->"), (LogicalNot, "!"),
    (BitOr, "|"), (BitNor, "nor"), (BitXor, "^"), (BitXnor, "!^"), (BitAnd, "&"), (BitNand, "nand"), (BitInv, "~"),

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
    | Call Expression [Expression]
    | CallT Expression [(Class, [Token])] [Expression]
    | SizeOfExpr Expression Token
    | SizeOfType (Class, [Token]) Token
    | IfExpr Expression Expression Expression (Token, Maybe Token)
    | BlockExpr Block
    deriving (Eq, Show)


-- | toString in human version
prettyExpr :: Int -> Maybe Expression -> String
prettyExpr n me = insertTab n ++ prettyExpr' me
    where
        prettyExpr' :: Maybe Expression -> String
        prettyExpr' Nothing = ""
        prettyExpr' (Just (Error t why)) = concat ["error at: ", show t, " ", why]
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
        prettyExpr' (Just (Cast (c, _) e _)) = concat ["(", prettyClass c, ")(", prettyExpr' (Just e), ")"]
        prettyExpr' (Just (Unary o e _)) = prettyOp o ++ prettyExpr' (Just e)
        prettyExpr' (Just (Binary o e1 e2 _)) = concat [prettyExpr' (Just e1), prettyOp o, prettyExpr' (Just e2)]
        prettyExpr' (Just (Call callee args)) =
            let calleeS = prettyExpr' (Just callee)
                argsS = intercalate ", " (map (prettyExpr' . Just) args)
            in concat [calleeS, "(", argsS, ")"]
        prettyExpr' (Just (CallT callee ts args)) =
            let calleeS = prettyExpr' (Just callee)
                typeArgsS = concat ["<", intercalate ", " (map (prettyClass . fst) ts), ">"]
                argsS = intercalate ", " (map (prettyExpr' . Just) args)
            in concat [calleeS, typeArgsS, "(", argsS, ")"]
        prettyExpr' (Just (SizeOfExpr inner _)) =
            concat ["sizeof(", prettyExpr' (Just inner), ")"]
        prettyExpr' (Just (SizeOfType (cls, _) _)) =
            concat ["sizeof(", prettyClass cls, ")"]
        prettyExpr' (Just (IfExpr cond thenE elseE _)) =
            concat [
                "if ",
                prettyExpr' (Just cond),
                ": ",
                prettyExpr' (Just thenE),
                " else: ",
                prettyExpr' (Just elseE)
            ]
        prettyExpr' (Just (BlockExpr b)) =
            prettyBlock 0 b


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
exprTokens (Call e1 es) = concatMap exprTokens (e1 : es)
exprTokens (CallT e1 cts es) = concatMap snd cts ++ concatMap exprTokens (e1 : es)
exprTokens (SizeOfExpr e t) = t : exprTokens e
exprTokens (SizeOfType (_, toks) t) = t : toks
exprTokens (IfExpr cond thenE elseE (ifTok, elseTok)) =
    [ifTok] ++ exprTokens cond ++ exprTokens thenE ++ maybe [] pure elseTok ++ exprTokens elseE
exprTokens (BlockExpr b) = blockTokens (Just b)


-- | Flatten all expressions contained in a expr.
--   Useful for traversal, analysis, or validation passes.
flattenExpr :: Maybe Expression -> [Expression]
flattenExpr Nothing = []
flattenExpr (Just fatherE@(Cast _ e2 _)) = [fatherE, e2]
flattenExpr (Just fatherE@(Unary _ e _)) = [fatherE, e]
flattenExpr (Just fatherE@(Binary _ e1 e2 _)) = [fatherE, e1, e2]
flattenExpr (Just fatherE@(Call callee args)) =
    concat [[fatherE], flattenExpr (Just callee), concatMap (flattenExpr . Just) args]
flattenExpr (Just fatherE@(CallT callee _ args)) =
    concat [[fatherE], flattenExpr (Just callee), concatMap (flattenExpr . Just) args]
flattenExpr (Just fatherE@(SizeOfExpr inner _)) = fatherE : flattenExpr (Just inner)
flattenExpr (Just fatherE@(SizeOfType _ _)) = [fatherE]
flattenExpr (Just fatherE@(IfExpr cond thenE elseE _)) =
    concat [[fatherE], flattenExpr (Just cond), flattenExpr (Just thenE), flattenExpr (Just elseE)]
flattenExpr (Just fatherE@(BlockExpr b)) =
    fatherE : flattenBlock (Just b)
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
prettyBlock n (Multiple ss) =
    let space = insertTab n
    in unlines [space ++ "{", concatMap (prettyStmt (n + 1) . Just) ss, space ++ "}"]

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

    DefField [String] (Maybe Class) (Maybe Expression) [Token] | -- this must be [a] = 10, a.b = 10 is not alowed -- this is for class
    DefConstField [String] (Maybe Class) (Maybe Expression) [Token] | -- this must be [a] = 10, a.b = 10 is not alowed -- this is for class

    DefVar [String] (Maybe Class) (Maybe Expression) [Token] | -- this is for static var
    DefConstVar [String] (Maybe Class) (Maybe Expression) [Token] | -- this is for static var


    Expr Expression |
    Exprs [Expression] |
    -- | A transparent statement group used for parser desugaring
    --   (for example: `var a = 1, b = 2`). It does not imply a new scope.
    StmtGroup [Statement] |
    BlockStmt Block |
    If Expression (Maybe Block) (Maybe Block) (Token, Maybe Token) | -- (if keyword - maybe else)
    For (Maybe Statement, Maybe Expression, Maybe Statement) (Maybe Block) (Maybe Block) (Token, Maybe Token) | -- (for keyword)
    Loop (Maybe Block) Token |
    Repeat Expression (Maybe Block) (Maybe Block) (Token, Maybe Token) | -- (repeat keyword, maybe else keyword)
    While Expression (Maybe Block) (Maybe Block) (Token, Maybe Token) | -- while else -- (while keyword - maybe else keyword)
    Until Expression (Maybe Block) (Maybe Block) (Token, Maybe Token) | -- until else -- (while keyword - maybe else keyword)

    DoWhile (Maybe Block) Expression (Maybe Block) (Token, Token, Maybe Token) | -- (do keyword, while keyword, maybe else keyword)
    DoUntil (Maybe Block) Expression (Maybe Block) (Token, Token, Maybe Token) | -- (do keyword, until keyword, maybe else keyword)

    Switch Expression [SwitchCase] Token | -- (switch keyword)

    -- Constructor 

    InstanceMethod (Class, [Token]) Expression [(Class, String, [Token])] Block |

    StaticMethod (Class, [Token]) Expression [(Class, String, [Token])] Block |

    NativeMethod (Class, [Token]) Expression [(Class, String, [Token])] String |

    -- function: return_type + pos, name, params + position, body
    InstanceMethodT (Class, [Token]) Expression [(Class, [Token])] [(Class, String, [Token])] Block |

    -- template function:
    --          return_type + pos,  name, template params + position, params + position,    body
    StaticMethodT (Class, [Token]) Expression [(Class, [Token])] [(Class, String, [Token])] Block |

    --      struct kw, struct name \
    --              must be stringconst
    Struct (Token, Expression) [Statement]

    deriving (Eq, Show)

{-# COMPLETE
    Command, DefField, DefConstField, DefVar, DefConstVar, Expr, Exprs, StmtGroup, BlockStmt,
    If, For, Loop, Repeat, While, Until, DoWhile, DoUntil, Switch, Function, NativeMethod, FunctionT #-}

-- beter toString for string instance
prettyStmt :: Int -> Maybe Statement -> String
prettyStmt _ Nothing = "\n"
prettyStmt n (Just (Command c _)) = prettyCmd n c
prettyStmt n (Just (DefField names mTy me _)) =
    let
        tyS = maybe "" (\c -> ": " ++ prettyClass c) mTy
        rhs = case me of
            Just e -> " = " ++ prettyExpr 0 (Just e)
            Nothing -> ""
    in insertTab n ++ "var " ++ intercalate "." names ++ tyS ++ rhs ++ "\n"
prettyStmt n (Just (DefConstField names mTy me _)) =
    let
        tyS = maybe "" (\c -> ": " ++ prettyClass c) mTy
        rhs = case me of
            Just e -> " = " ++ prettyExpr 0 (Just e)
            Nothing -> ""
    in insertTab n ++ "val " ++ intercalate "." names ++ tyS ++ rhs ++ "\n"
prettyStmt n (Just (DefVar names mTy me _)) =
    let
        tyS = maybe "" (\c -> ": " ++ prettyClass c) mTy
        rhs = case me of
            Just e -> " = " ++ prettyExpr 0 (Just e)
            Nothing -> ""
    in insertTab n ++ "var " ++ intercalate "." names ++ tyS ++ rhs ++ "\n"
prettyStmt n (Just (DefConstVar names mTy me _)) =
    let
        tyS = maybe "" (\c -> ": " ++ prettyClass c) mTy
        rhs = case me of
            Just e -> " = " ++ prettyExpr 0 (Just e)
            Nothing -> ""
    in insertTab n ++ "val " ++ intercalate "." names ++ tyS ++ rhs ++ "\n"
prettyStmt n (Just (Expr e)) = prettyExpr n (Just e) ++ "\n"
prettyStmt n (Just (Exprs es)) =
    insertTab n ++ intercalate ", " (map (prettyExpr 0 . Just) es) ++ "\n"
prettyStmt n (Just (StmtGroup ss)) = concatMap (prettyStmt n . Just) ss
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
prettyStmt n (Just (For (s1, s2, s3) Nothing Nothing _)) = let
    s = intercalate ";" [prettyForPart s1, prettyExpr 0 s2, prettyForPart s3] in
    concat [insertTab n, "for(", s, ");\n"]
prettyStmt n (Just (For (s1, s2, s3) Nothing (Just b) _)) = let
    s = intercalate ";" [prettyForPart s1, prettyExpr 0 s2, prettyForPart s3]
    space = insertTab n in
    unlines [concat [space, "for(", s, ");"], space ++ "else", prettyBlock n b]
prettyStmt n (Just (For (s1, s2, s3) (Just b) Nothing _)) = let
    s = intercalate ";" [prettyForPart s1, prettyExpr 0 s2, prettyForPart s3] in
    unlines [concat [insertTab n, "for(", s, ")"], prettyBlock n b]
prettyStmt n (Just (For (s1, s2, s3) (Just b1) (Just b2) _)) = let
    s = intercalate ";" [prettyForPart s1, prettyExpr 0 s2, prettyForPart s3]
    space = insertTab n in
    unlines [concat [space, "for(", s, ")"], prettyBlock n b1, space ++ "else", prettyBlock n b2]

-- loop (while true)
prettyStmt n (Just (Loop Nothing _)) = insertTab n ++ "loop;\n"
prettyStmt n (Just (Loop (Just b) _)) = unlines [insertTab n ++ "loop", prettyBlock n b]

-- repeat (counted loop)
prettyStmt n (Just (Repeat e Nothing Nothing _)) =
    concat [insertTab n, "repeat(", prettyExpr 0 (Just e), ");\n"]
prettyStmt n (Just (Repeat e Nothing (Just b) _)) = let
    space = insertTab n in
    unlines [concat [space, "repeat(", prettyExpr 0 (Just e), ");"], space ++ "else", prettyBlock n b]
prettyStmt n (Just (Repeat e (Just b) Nothing _)) =
    unlines [concat [insertTab n, "repeat(", prettyExpr 0 (Just e), ")"], prettyBlock n b]
prettyStmt n (Just (Repeat e (Just b1) (Just b2) _)) = let
    space = insertTab n in
    unlines [concat [space, "repeat(", prettyExpr 0 (Just e), ")"], prettyBlock n b1, space ++ "else", prettyBlock n b2]

-- while
prettyStmt n (Just (While e Nothing Nothing _)) = concat [insertTab n, "while(", prettyExpr 0 (Just e), ");\n"]
prettyStmt n (Just (While e Nothing (Just b) _)) = let space = insertTab n in
    unlines [concat [insertTab n, "while(", prettyExpr 0 (Just e), ");\n"], space ++ "else", prettyBlock n b]
prettyStmt n (Just (While e (Just b) Nothing _)) = unlines [concat [insertTab n, "while(", prettyExpr 0 (Just e), ")"], prettyBlock n b]
prettyStmt n (Just (While e (Just b1) (Just b2) _)) = let space = insertTab n in
    unlines [concat [insertTab n, "while(", prettyExpr 0 (Just e), ")"], prettyBlock n b1, space ++ "else", prettyBlock n b2]

-- until
prettyStmt n (Just (Until e Nothing Nothing _)) = concat [insertTab n, "until(", prettyExpr 0 (Just e), ");\n"]
prettyStmt n (Just (Until e Nothing (Just b) _)) = let space = insertTab n in
    unlines [concat [insertTab n, "until(", prettyExpr 0 (Just e), ");\n"], space ++ "else", prettyBlock n b]
prettyStmt n (Just (Until e (Just b) Nothing _)) = unlines [concat [insertTab n, "until(", prettyExpr 0 (Just e), ")"], prettyBlock n b]
prettyStmt n (Just (Until e (Just b1) (Just b2) _)) = let space = insertTab n in
    unlines [concat [insertTab n, "until(", prettyExpr 0 (Just e), ")"], prettyBlock n b1, space ++ "else", prettyBlock n b2]

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

-- dountil
prettyStmt n (Just (DoUntil Nothing e Nothing _)) = let space = insertTab n in
    unlines [space ++ "do", concat [space, "until(", prettyExpr 0 (Just e), ")"]]
prettyStmt n (Just (DoUntil Nothing e (Just b) _)) = let space = insertTab n in
    unlines [space ++ "do",  concat [space, prettyBlock n b, "until(", prettyExpr 0 (Just e), ")"], space ++ "else", prettyBlock n b]
prettyStmt n (Just (DoUntil (Just b) e Nothing _)) = let space = insertTab n in
    unlines [space ++ "do",  concat [space, prettyBlock n b, "until(", prettyExpr 0 (Just e), ")"]]
prettyStmt n (Just (DoUntil (Just b1) e (Just b2) _)) = let space = insertTab n in
        unlines [space ++ "do", concat [space, prettyBlock n b1, "until(", prettyExpr 0 (Just e), ")"],
            space ++ "else", prettyBlock n b2]

prettyStmt n (Just (Switch e xs _)) = let space = insertTab n in unlines
    [concat [space, "switch(", prettyExpr n (Just e), ")"], space ++ "{", concatMap (prettySwitchCase (n + 1)) xs, space ++ "}" ]



prettyStmt n (Just (Function (retC, _) functionName params b)) =
    let space = insertTab n
        prettyOneParam :: (Class, String, [Token]) -> String
        prettyOneParam (c, name, _) = concat [prettyClass c, " ", name]
    in unlines [
            concat [
                space,
                prettyClass retC,
                " ",
                prettyExpr 0 (Just functionName),
                "(",
                intercalate ", " (map prettyOneParam params), ")"],
            prettyBlock n b]
prettyStmt n (Just (FunctionT (retC, _) functionName genParams params b)) =
    let space = insertTab n
        prettyGen = concat ["<", intercalate ", " (map (prettyClass . fst) genParams), ">"]
        prettyOneParam :: (Class, String, [Token]) -> String
        prettyOneParam (c, name, _) = concat [prettyClass c, " ", name]
    in unlines [
        concat [space, prettyClass retC, " ", prettyExpr 0 (Just functionName),
            prettyGen, "(", intercalate ", " (map prettyOneParam params),")"],
        prettyBlock n b]
prettyStmt n (Just (NativeMethod (retC, _) functionName params code)) =
    let
        space = insertTab n
        prettyOneParam :: (Class, String, [Token]) -> String
        prettyOneParam (c, name, _) = concat [prettyClass c, " ", name]

        prettyBody :: String -> String
        prettyBody body = concatMap (\lineTxt -> insertTab (n + 1) ++ lineTxt ++ "\n") (lines body)
    in concat [
        space, prettyClass retC, " ",
        prettyExpr 0 (Just functionName),
        "(",
        intercalate ", " (map prettyOneParam params),
        ")\n", space, "{\n",
        prettyBody code, space, "}\n"]

-- | pretty statement in IO version
prettyStmtIO :: Maybe Statement -> IO String
prettyStmtIO = pure . prettyStmt 0


-- | Collect all tokens contained in a statement (recursively).
--   This includes tokens from nested expressions and blocks.
stmtTokens :: Statement -> [Token]
stmtTokens (Command _ t) = [t]
stmtTokens (DefField _ _ me toks) = toks ++ maybe [] exprTokens me
stmtTokens (DefConstField _ _ me toks) = toks ++ maybe [] exprTokens me
stmtTokens (DefVar _ _ me toks) = toks ++ maybe [] exprTokens me
stmtTokens (DefConstVar _ _ me toks) = toks ++ maybe [] exprTokens me
stmtTokens (Expr e) = exprTokens e
stmtTokens (Exprs es) = concatMap exprTokens es
stmtTokens (StmtGroup ss) = concatMap stmtTokens ss
stmtTokens (BlockStmt b) = blockTokens (Just b)

stmtTokens (If e b1 b2 (ifTok, elseTok)) = concat [
    [ifTok], exprTokens e, blockTokens b1, maybe [] pure elseTok, blockTokens b2]

stmtTokens (For (s1, e2, s3) b1 b2 (forTok, elseTok)) = concat [
    [forTok], maybe [] stmtTokens s1, maybe [] exprTokens e2, maybe [] stmtTokens s3,
    blockTokens b1, maybe [] pure elseTok, blockTokens b2]

stmtTokens (Loop b loopTok) = loopTok : blockTokens b
stmtTokens (Repeat e b1 b2 (repeatTok, elseTok)) = concat [
    [repeatTok], exprTokens e, blockTokens b1, maybe [] pure elseTok, blockTokens b2]

stmtTokens (While e b1 b2 (whileTok, elseTok)) = concat [
    [whileTok], exprTokens e, blockTokens b1,
    maybe [] pure elseTok, blockTokens b2]

stmtTokens (Until e b1 b2 (untilTok, elseTok)) = concat [
    [untilTok], exprTokens e, blockTokens b1,
    maybe [] pure elseTok, blockTokens b2]

stmtTokens (DoWhile b1 e b2 (doTok, whileTok, elseTok)) = concat [
    [doTok], blockTokens b1,
    [whileTok], exprTokens e,
    maybe [] pure elseTok, blockTokens b2]

stmtTokens (DoUntil b1 e b2 (doTok, untilTok, elseTok)) = concat [
    [doTok], blockTokens b1,
    [untilTok], exprTokens e,
    maybe [] pure elseTok, blockTokens b2]

stmtTokens (Switch e scs switchTok) = concat [[switchTok], exprTokens e, concatMap switchCaseTokens scs]


stmtTokens (Function (_, retToks) name params b) = concat [
    retToks, exprTokens name,
    concatMap (\(_, _, toks) -> toks) params, blockTokens (Just b)]
stmtTokens (FunctionT (_, retToks) name genParams params b) = concat [
    retToks, exprTokens name, concatMap snd genParams,
    concatMap (\(_, _, toks) -> toks) params, blockTokens (Just b)]
stmtTokens (NativeMethod (_, retToks) name params _) = concat [
    retToks, exprTokens name, concatMap (\(_, _, toks) -> toks) params]


-- | Flatten all expressions contained in a statement.
--   Recursively traverses nested blocks and control structures.
flattenStatement :: Maybe Statement -> [Expression]
flattenStatement Nothing = []
flattenStatement (Just (Command _ _)) = []
flattenStatement (Just (DefField _ _ me _)) = maybe [] (flattenExpr . Just) me
flattenStatement (Just (DefConstField _ _ me _)) = maybe [] (flattenExpr . Just) me
flattenStatement (Just (DefVar _ _ me _)) = maybe [] (flattenExpr . Just) me
flattenStatement (Just (DefConstVar _ _ me _)) = maybe [] (flattenExpr . Just) me
flattenStatement (Just (Expr e)) = flattenExpr (Just e)
flattenStatement (Just (Exprs es)) = concatMap (flattenExpr . Just) es
flattenStatement (Just (StmtGroup ss)) = concatMap (flattenStatement . Just) ss
flattenStatement (Just (BlockStmt b)) = flattenBlock (Just b)
flattenStatement (Just (If e b c _)) = e : (flattenBlock b ++ flattenBlock c)
flattenStatement (Just (For (s1, e2, s3) b1 b2 _)) =
    flattenStatement s1 ++ maybe [] (flattenExpr . Just) e2 ++ flattenStatement s3 ++ flattenBlock b1 ++ flattenBlock b2
flattenStatement (Just (Loop b _)) = flattenBlock b
flattenStatement (Just (Repeat e b1 b2 _)) = e : (flattenBlock b1 ++ flattenBlock b2)
flattenStatement (Just (While e b1 b2 _)) = e : (flattenBlock b1 ++ flattenBlock b2)
flattenStatement (Just (Until e b1 b2 _)) = e : (flattenBlock b1 ++ flattenBlock b2)
flattenStatement (Just (DoWhile b1 e b2 _)) = e : (flattenBlock b1 ++ flattenBlock b2)
flattenStatement (Just (DoUntil b1 e b2 _)) = e : (flattenBlock b1 ++ flattenBlock b2)
flattenStatement (Just (Switch e scs _)) = e : concatMap (flattenCase . Just) scs
flattenStatement (Just (Function _ _ params b)) = paramExprs params ++ flattenBlock (Just b)
    where
        paramExprs :: [(Class, String, [Token])] -> [Expression]
        paramExprs = concatMap one
            where
                one :: (Class, String, [Token]) -> [Expression]
                one (_, _, []) = []
                one (_, name, t:_)  = [Variable name t]

flattenStatement (Just (FunctionT _ _ _ params b)) = paramExprs params ++ flattenBlock (Just b)
    where
        paramExprs :: [(Class, String, [Token])] -> [Expression]
        paramExprs = concatMap one
            where
                one :: (Class, String, [Token]) -> [Expression]
                one (_, _, [])  = []
                one (_, name, t:_)  = [Variable name t]
flattenStatement (Just (NativeMethod _ _ params _)) = paramExprs params
    where
        paramExprs :: [(Class, String, [Token])] -> [Expression]
        paramExprs = concatMap one
            where
                one :: (Class, String, [Token]) -> [Expression]
                one (_, _, [])  = []
                one (_, name, t:_)  = [Variable name t]


-- | The declaration of a program, especially import and module
data Declaration =
    Package [String] [Token] |
    Import [String] [Token] |
    JavaName String Token
    deriving (Eq, Show)


-- | Better toString method for declearation
prettyDeclaration :: Declaration -> String
prettyDeclaration (Package ss _) = "package " ++ intercalate "." ss
prettyDeclaration (Import ss _) = "Import " ++ intercalate "." ss
prettyDeclaration (JavaName s _) = "classname " ++ show s


-- | Check whether a statement is a class declaration.
--   Placeholder: class declarations are not modeled yet, so this is always False.
--   Add cases here when class statements are introduced.
isClassDeclar :: Statement -> Bool
isClassDeclar _ = False


-- | Check whether a statement is a function declaration.
isFunction :: Statement -> Bool
isFunction (Function {}) = True
isFunction (NativeMethod {}) = True
isFunction _ = False


-- | 
isCFunction :: Statement -> Bool
isCFunction (NativeMethod {}) = True
isCFunction _ = False


-- | Check whether a statement is a template function declaration.
isFunctionT :: Statement -> Bool
isFunctionT (FunctionT {}) = True
isFunctionT _ = False


assignOps :: HashSet Operator
assignOps = HashSet.fromList [
    Assign,
    PlusAssign, MinusAssign,
    MultiplyAssign, DivideAssign,
    ModuloAssign, PowerAssign]


-- | Check whether a statement is an assignment expression statement.
isAssignment :: Statement -> Bool
isAssignment (Expr (Binary op _ _ _)) = HashSet.member op assignOps
isAssignment (Exprs es) = any isAssignExpr es
    where
        isAssignExpr (Binary op _ _ _) = HashSet.member op assignOps
        isAssignExpr _ = False
isAssignment (StmtGroup ss) = any isAssignment ss
isAssignment _ = False


prettyForPart :: Maybe Statement -> String
prettyForPart Nothing = ""
prettyForPart (Just (Expr e)) = prettyExpr 0 (Just e)
prettyForPart (Just (Exprs es)) = intercalate ", " (map (prettyExpr 0 . Just) es)
prettyForPart (Just (DefField names mTy me _)) = "var " ++ declItem names mTy me
prettyForPart (Just (DefVar names mTy me _)) = "var " ++ declItem names mTy me
prettyForPart (Just (DefConstField names mTy me _)) = "val " ++ declItem names mTy me
prettyForPart (Just (DefConstVar names mTy me _)) = "val " ++ declItem names mTy me
prettyForPart (Just (StmtGroup ss)) = prettyForPartDecls ss
prettyForPart (Just (BlockStmt (Multiple ss))) = prettyForPartDecls ss
prettyForPart (Just other) = trimNewline $ prettyStmt 0 (Just other)
    where
        trimNewline = filter (/= '\n')


prettyForPartDecls :: [Statement] -> String
prettyForPartDecls ss = case ss of
    [] -> ""
    _ -> let asVarItems = map toVarItem ss in
        case sequence asVarItems of
            Just items -> "var " ++ intercalate ", " items
            Nothing -> case mapM toValItem ss of
                Just items -> "val " ++ intercalate ", " items
                Nothing -> intercalate ", " (map (trimNewline . prettyStmt 0 . Just) ss)
    where
        trimNewline = filter (/= '\n')
        toVarItem st = case st of
            DefField names mTy me _ -> Just (declItem names mTy me)
            DefVar names mTy me _ -> Just (declItem names mTy me)
            _ -> Nothing
        toValItem st = case st of
            DefConstField names mTy me _ -> Just (declItem names mTy me)
            DefConstVar names mTy me _ -> Just (declItem names mTy me)
            _ -> Nothing


declItem :: [String] -> Maybe Class -> Maybe Expression -> String
declItem names mTy me =
    let tyS = maybe "" (\c -> ": " ++ prettyClass c) mTy
        rhs = maybe "" (\e -> " = " ++ prettyExpr 0 (Just e)) me
    in intercalate "." names ++ tyS ++ rhs


-- | Extract the path segments from a declaration.
declPath :: Declaration -> [String]
declPath (Package segs _) = segs
declPath (Import  segs _) = segs
declPath (JavaName _ _) = []


-- | Check whether a declaration is a package declaration.
isPackageDecl :: Declaration -> Bool
isPackageDecl (Package _ _) = True
isPackageDecl _ = False


-- | Check whether a declaration is an import declaration.
isImportDecl :: Declaration -> Bool
isImportDecl (Import _ _) = True
isImportDecl _ = False


-- | Check whether a declaration is a classname declaration.
isJavaNameDecl :: Declaration -> Bool
isJavaNameDecl (JavaName _ _) = True
isJavaNameDecl _ = False


-- | Program representation.
--   Consists of imported modules and a list of top-level statements.
type Program = ([Declaration], [Statement])


getCFun :: Program -> [Statement]
getCFun = filter isCFunction . snd


type ParamDecl = (Class, String, [Token])


data HoistCallSpec = HoistCallSpec {
    hoistCalleeName :: String,
    hoistExtraArgs :: [Expression]
}

data InlineFunSpec = InlineFunSpec {
    inlineParamNames :: [String],
    inlineBodyExpr :: Expression
}


-- | Inline function calls for functions marked with `inline`.
--   This pass only rewrites function calls; variable reads/writes are unchanged.
--   Only single-expression inline functions are expanded:
--     inline fun f(...) -> T { return <expr> }
inlineProgramFunctions :: Program -> Program
inlineProgramFunctions (decls, stmts) =
    let specs = filterNonRecursiveInlineSpecs (collectInlineSpecs stmts)
        stmts' = map (rewriteStmtInline specs []) stmts
    in (decls, stmts')
    where
        collectInlineSpecs :: [Statement] -> Map String InlineFunSpec
        collectInlineSpecs = foldl step Map.empty
            where
                step :: Map String InlineFunSpec -> Statement -> Map String InlineFunSpec
                step acc stmt = case inlineSpecOf stmt of
                    Just (name, spec) -> Map.insert name spec acc
                    Nothing -> acc

        inlineSpecOf :: Statement -> Maybe (String, InlineFunSpec)
        inlineSpecOf stmt = do
            (name, _) <- functionNameVar stmt
            params <- functionParams stmt
            bodyExpr <- inlineReturnExpr stmt
            let retToks = functionRetTokens stmt
            if hasInlineToken retToks
                then Just (name, InlineFunSpec (map (\(_, n, _) -> n) params) bodyExpr)
                else Nothing

        inlineReturnExpr :: Statement -> Maybe Expression
        inlineReturnExpr stmt = do
            (_, body) <- functionParamsAndBody stmt
            case body of
                Multiple [Command (Return (Just e)) _] -> Just e
                _ -> Nothing

        functionRetTokens :: Statement -> [Token]
        functionRetTokens stmt = case stmt of
            InstanceMethod (_, toks) _ _ _ -> toks
            StaticMethod (_, toks) _ _ _ -> toks
            InstanceMethodT (_, toks) _ _ _ _ -> toks
            StaticMethodT (_, toks) _ _ _ _ -> toks
            _ -> []

        hasInlineToken :: [Token] -> Bool
        hasInlineToken = any isInlineToken

        isInlineToken :: Token -> Bool
        isInlineToken (Lex.Ident s _) = map toLower s == "inline"
        isInlineToken _ = False

        filterNonRecursiveInlineSpecs :: Map String InlineFunSpec -> Map String InlineFunSpec
        filterNonRecursiveInlineSpecs specs =
            let recursiveNames = recursiveInlineNames specs
            in HashSet.foldl' (flip Map.delete) specs recursiveNames

        recursiveInlineNames :: Map String InlineFunSpec -> HashSet String
        recursiveInlineNames specs =
            let inlineNames = HashSet.fromList (Map.keys specs)
                callGraph =
                    Map.map
                        (HashSet.filter (`HashSet.member` inlineNames) . exprCallees . inlineBodyExpr)
                        specs
                isRecursiveFrom :: String -> Bool
                isRecursiveFrom start = dfsFrom start HashSet.empty start
                    where
                        dfsFrom :: String -> HashSet String -> String -> Bool
                        dfsFrom root visited node =
                            case Map.lookup node callGraph of
                                Nothing -> False
                                Just nexts ->
                                    HashSet.foldr
                                        (\n acc ->
                                            acc ||
                                            ((n == root) || (not (HashSet.member n visited) && dfsFrom root (HashSet.insert n visited) n))
                                        )
                                        False
                                        nexts
            in HashSet.fromList (filter isRecursiveFrom (Map.keys specs))

        exprCallees :: Expression -> HashSet String
        exprCallees expr = case expr of
            Call callee args ->
                let selfCall = case callee of
                        Variable name _ -> HashSet.singleton name
                        _ -> HashSet.empty
                in HashSet.unions (selfCall : exprCallees callee : map exprCallees args)
            CallT callee _ args ->
                let selfCall = case callee of
                        Variable name _ -> HashSet.singleton name
                        _ -> HashSet.empty
                in HashSet.unions (selfCall : exprCallees callee : map exprCallees args)
            Cast _ e _ -> exprCallees e
            Unary _ e _ -> exprCallees e
            Binary _ e1 e2 _ -> HashSet.union (exprCallees e1) (exprCallees e2)
            _ -> HashSet.empty

        rewriteStmtInline :: Map String InlineFunSpec -> [String] -> Statement -> Statement
        rewriteStmtInline specs stack stmt = case stmt of
            Command (Return me) tok -> Command (Return (fmap (rewriteExprInline specs stack) me)) tok
            Command _ _ -> stmt
            Expr e -> Expr (rewriteExprInline specs stack e)
            Exprs es -> Exprs (map (rewriteExprInline specs stack) es)
            DefField names mTy mRhs toks -> DefField names mTy (fmap (rewriteExprInline specs stack) mRhs) toks
            DefConstField names mTy mRhs toks -> DefConstField names mTy (fmap (rewriteExprInline specs stack) mRhs) toks
            DefVar names mTy mRhs toks -> DefVar names mTy (fmap (rewriteExprInline specs stack) mRhs) toks
            DefConstVar names mTy mRhs toks -> DefConstVar names mTy (fmap (rewriteExprInline specs stack) mRhs) toks
            StmtGroup ss -> StmtGroup (map (rewriteStmtInline specs stack) ss)
            BlockStmt b -> BlockStmt (rewriteBlockInline specs stack b)
            If e b1 b2 toks ->
                If
                    (rewriteExprInline specs stack e)
                    (fmap (rewriteBlockInline specs stack) b1)
                    (fmap (rewriteBlockInline specs stack) b2)
                    toks
            For (s1, e2, s3) b1 b2 toks ->
                For
                    ( fmap (rewriteStmtInline specs stack) s1
                    , fmap (rewriteExprInline specs stack) e2
                    , fmap (rewriteStmtInline specs stack) s3
                    )
                    (fmap (rewriteBlockInline specs stack) b1)
                    (fmap (rewriteBlockInline specs stack) b2)
                    toks
            Loop b tok -> Loop (fmap (rewriteBlockInline specs stack) b) tok
            Repeat e b1 b2 toks ->
                Repeat
                    (rewriteExprInline specs stack e)
                    (fmap (rewriteBlockInline specs stack) b1)
                    (fmap (rewriteBlockInline specs stack) b2)
                    toks
            While e b1 b2 toks ->
                While
                    (rewriteExprInline specs stack e)
                    (fmap (rewriteBlockInline specs stack) b1)
                    (fmap (rewriteBlockInline specs stack) b2)
                    toks
            Until e b1 b2 toks ->
                Until
                    (rewriteExprInline specs stack e)
                    (fmap (rewriteBlockInline specs stack) b1)
                    (fmap (rewriteBlockInline specs stack) b2)
                    toks
            DoWhile b1 e b2 toks ->
                DoWhile
                    (fmap (rewriteBlockInline specs stack) b1)
                    (rewriteExprInline specs stack e)
                    (fmap (rewriteBlockInline specs stack) b2)
                    toks
            DoUntil b1 e b2 toks ->
                DoUntil
                    (fmap (rewriteBlockInline specs stack) b1)
                    (rewriteExprInline specs stack e)
                    (fmap (rewriteBlockInline specs stack) b2)
                    toks
            Switch e scs tok ->
                Switch (rewriteExprInline specs stack e) (map (rewriteSwitchCaseInline specs stack) scs) tok
            _ -> case functionNameVar stmt of
                Just (fname, _) ->
                    case functionParamsAndBody stmt of
                        Just (_, body) -> setFunctionBody (rewriteBlockInline specs (fname : stack) body) stmt
                        Nothing -> stmt
                Nothing -> stmt

        rewriteSwitchCaseInline :: Map String InlineFunSpec -> [String] -> SwitchCase -> SwitchCase
        rewriteSwitchCaseInline specs stack sc = case sc of
            Case e mb tok -> Case (rewriteExprInline specs stack e) (fmap (rewriteBlockInline specs stack) mb) tok
            Default b tok -> Default (rewriteBlockInline specs stack b) tok

        rewriteBlockInline :: Map String InlineFunSpec -> [String] -> Block -> Block
        rewriteBlockInline specs stack (Multiple ss) = Multiple (map (rewriteStmtInline specs stack) ss)

        rewriteExprInline :: Map String InlineFunSpec -> [String] -> Expression -> Expression
        rewriteExprInline specs stack expr = case expr of
            Error _ _ -> expr
            IntConst _ _ -> expr
            LongConst _ _ -> expr
            FloatConst _ _ -> expr
            DoubleConst _ _ -> expr
            LongDoubleConst _ _ -> expr
            CharConst _ _ -> expr
            StringConst _ _ -> expr
            BoolConst _ _ -> expr
            Variable _ _ -> expr
            Qualified _ _ -> expr
            Cast ty e tok -> Cast ty (rewriteExprInline specs stack e) tok
            Unary op e tok -> Unary op (rewriteExprInline specs stack e) tok
            Binary op e1 e2 tok -> Binary op (rewriteExprInline specs stack e1) (rewriteExprInline specs stack e2) tok
            IfExpr cond thenE elseE toks ->
                IfExpr
                    (rewriteExprInline specs stack cond)
                    (rewriteExprInline specs stack thenE)
                    (rewriteExprInline specs stack elseE)
                    toks
            BlockExpr b ->
                BlockExpr (rewriteBlockInline specs stack b)
            Call callee args ->
                inlineCall specs stack (Call callee args)
            CallT callee tys args ->
                inlineCallT specs stack (CallT callee tys args)
            SizeOfExpr inner tok ->
                SizeOfExpr (rewriteExprInline specs stack inner) tok
            SizeOfType _ _ ->
                expr

        inlineCall :: Map String InlineFunSpec -> [String] -> Expression -> Expression
        inlineCall specs stack (Call callee args) =
            let callee' = rewriteExprInline specs stack callee
                args' = map (rewriteExprInline specs stack) args
            in case callee' of
                Variable name _ -> case Map.lookup name specs of
                    Just spec
                        | name `notElem` stack && length args' == length (inlineParamNames spec) ->
                            let subst = Map.fromList (zip (inlineParamNames spec) args')
                                body' = substInlineExpr subst (inlineBodyExpr spec)
                            in rewriteExprInline specs (name : stack) body'
                    _ -> Call callee' args'
                _ -> Call callee' args'
        inlineCall _ _ e = e

        inlineCallT :: Map String InlineFunSpec -> [String] -> Expression -> Expression
        inlineCallT specs stack (CallT callee tys args) =
            let callee' = rewriteExprInline specs stack callee
                args' = map (rewriteExprInline specs stack) args
            in case callee' of
                Variable name _ -> case Map.lookup name specs of
                    Just spec
                        | name `notElem` stack && length args' == length (inlineParamNames spec) ->
                            let subst = Map.fromList (zip (inlineParamNames spec) args')
                                body' = substInlineExpr subst (inlineBodyExpr spec)
                            in rewriteExprInline specs (name : stack) body'
                    _ -> CallT callee' tys args'
                _ -> CallT callee' tys args'
        inlineCallT _ _ e = e

        substInlineExpr :: Map String Expression -> Expression -> Expression
        substInlineExpr subst expr = case expr of
            Variable name tok -> fromMaybe (Variable name tok) (Map.lookup name subst)
            Error _ _ -> expr
            IntConst _ _ -> expr
            LongConst _ _ -> expr
            FloatConst _ _ -> expr
            DoubleConst _ _ -> expr
            LongDoubleConst _ _ -> expr
            CharConst _ _ -> expr
            StringConst _ _ -> expr
            BoolConst _ _ -> expr
            Qualified _ _ -> expr
            Cast ty e tok -> Cast ty (substInlineExpr subst e) tok
            Unary op e tok -> Unary op (substInlineExpr subst e) tok
            Binary op e1 e2 tok -> Binary op (substInlineExpr subst e1) (substInlineExpr subst e2) tok
            IfExpr cond thenE elseE toks ->
                IfExpr
                    (substInlineExpr subst cond)
                    (substInlineExpr subst thenE)
                    (substInlineExpr subst elseE)
                    toks
            BlockExpr b ->
                BlockExpr (substInlineBlock subst b)
            Call callee args -> Call (substInlineExpr subst callee) (map (substInlineExpr subst) args)
            CallT callee tys args -> CallT (substInlineExpr subst callee) tys (map (substInlineExpr subst) args)
            SizeOfExpr inner tok -> SizeOfExpr (substInlineExpr subst inner) tok
            SizeOfType _ _ -> expr

        substInlineBlock :: Map String Expression -> Block -> Block
        substInlineBlock subst (Multiple ss) = Multiple (map (substInlineStmt subst) ss)

        substInlineStmt :: Map String Expression -> Statement -> Statement
        substInlineStmt subst stmt = case stmt of
            Command (Return mExpr) tok -> Command (Return (fmap (substInlineExpr subst) mExpr)) tok
            Expr e -> Expr (substInlineExpr subst e)
            Exprs es -> Exprs (map (substInlineExpr subst) es)
            DefField names mTy mRhs toks -> DefField names mTy (fmap (substInlineExpr subst) mRhs) toks
            DefConstField names mTy mRhs toks -> DefConstField names mTy (fmap (substInlineExpr subst) mRhs) toks
            DefVar names mTy mRhs toks -> DefVar names mTy (fmap (substInlineExpr subst) mRhs) toks
            DefConstVar names mTy mRhs toks -> DefConstVar names mTy (fmap (substInlineExpr subst) mRhs) toks
            StmtGroup ss -> StmtGroup (map (substInlineStmt subst) ss)
            BlockStmt b -> BlockStmt (substInlineBlock subst b)
            If e b1 b2 toks -> If (substInlineExpr subst e) (fmap (substInlineBlock subst) b1) (fmap (substInlineBlock subst) b2) toks
            For (s1, e2, s3) b1 b2 toks ->
                For
                    (fmap (substInlineStmt subst) s1, fmap (substInlineExpr subst) e2, fmap (substInlineStmt subst) s3)
                    (fmap (substInlineBlock subst) b1)
                    (fmap (substInlineBlock subst) b2)
                    toks
            Loop b tok -> Loop (fmap (substInlineBlock subst) b) tok
            Repeat e b1 b2 toks -> Repeat (substInlineExpr subst e) (fmap (substInlineBlock subst) b1) (fmap (substInlineBlock subst) b2) toks
            While e b1 b2 toks -> While (substInlineExpr subst e) (fmap (substInlineBlock subst) b1) (fmap (substInlineBlock subst) b2) toks
            Until e b1 b2 toks -> Until (substInlineExpr subst e) (fmap (substInlineBlock subst) b1) (fmap (substInlineBlock subst) b2) toks
            DoWhile b1 e b2 toks -> DoWhile (fmap (substInlineBlock subst) b1) (substInlineExpr subst e) (fmap (substInlineBlock subst) b2) toks
            DoUntil b1 e b2 toks -> DoUntil (fmap (substInlineBlock subst) b1) (substInlineExpr subst e) (fmap (substInlineBlock subst) b2) toks
            Switch e scs tok -> Switch (substInlineExpr subst e) (map (substInlineCase subst) scs) tok
            _ -> stmt

        substInlineCase :: Map String Expression -> SwitchCase -> SwitchCase
        substInlineCase subst sc = case sc of
            Case e mb tok -> Case (substInlineExpr subst e) (fmap (substInlineBlock subst) mb) tok
            Default b tok -> Default (substInlineBlock subst b) tok


data TemplateDef = TemplateDef {
    tdIsStatic :: Bool,
    tdName :: String,
    tdNameTok :: Token,
    tdRet :: (Class, [Token]),
    tdTypeParams :: [String],
    tdParams :: [ParamDecl],
    tdBody :: Block
}

type TemplateReq = (String, [Class], Int)


collectTemplateDefsFromStmts :: [Statement] -> Map String [TemplateDef]
collectTemplateDefsFromStmts = foldl step Map.empty
  where
    step :: Map String [TemplateDef] -> Statement -> Map String [TemplateDef]
    step acc stmt = case stmt of
        StaticMethodT ret (Variable name tok) gens params body ->
            case mapM (templateParamNameFromClass . fst) gens of
                Just tpNames ->
                    let def = TemplateDef True name tok ret tpNames params body
                    in Map.insertWith (++) name [def] acc
                Nothing -> acc
        InstanceMethodT ret (Variable name tok) gens params body ->
            case mapM templateParamNameFromClass (map fst gens) of
                Just tpNames ->
                    let def = TemplateDef False name tok ret tpNames params body
                    in Map.insertWith (++) name [def] acc
                Nothing -> acc
        _ -> acc

templateParamNameFromClass :: Class -> Maybe String
templateParamNameFromClass cls = case cls of
    Class [n] [] -> Just n
    _ -> Nothing

-- | Instantiate template calls (`CallT`) into concrete generated functions.
--   Instantiated functions keep the original function name and become
--   ordinary overload candidates resolved by normal type checking/mangling.
--   This keeps call checking deterministic and lets implicit-cast warnings
--   reuse the normal function-call checker.
instantiateTemplateCallsProgramWithExternal :: [Statement] -> Program -> Program
instantiateTemplateCallsProgramWithExternal extTemplateStmts =
    instantiateTemplateCallsProgramWithDefs (collectTemplateDefsFromStmts (flattenStmtGroups extTemplateStmts))


instantiateTemplateCallsProgram :: Program -> Program
instantiateTemplateCallsProgram = instantiateTemplateCallsProgramWithDefs Map.empty


instantiateTemplateCallsProgramWithDefs :: Map String [TemplateDef] -> Program -> Program
instantiateTemplateCallsProgramWithDefs externalDefs (decls, stmts0) =
    let stmts = flattenTop stmts0
        defs = Map.unionWith (++) (collectTemplateDefs stmts) externalDefs
        (stmts1, reqs0, _) = rewriteStmts False defs Map.empty stmts
        (_, _, generatedRev) = processReqs defs Set.empty Map.empty [] reqs0
    in (decls, stmts1 ++ reverse generatedRev)
    where
        templateDefsAll :: Map String [TemplateDef]
        templateDefsAll = Map.unionWith (++) (collectTemplateDefs (flattenTop stmts0)) externalDefs

        concreteArities :: Map String [Int]
        concreteArities = collectConcreteCallArities (flattenTop stmts0)

        flattenTop :: [Statement] -> [Statement]
        flattenTop = concatMap go
          where
            go (StmtGroup ss) = flattenTop ss
            go st = [st]

        collectTemplateDefs :: [Statement] -> Map String [TemplateDef]
        collectTemplateDefs = collectTemplateDefsFromStmts

        collectConcreteCallArities :: [Statement] -> Map String [Int]
        collectConcreteCallArities = foldl step Map.empty
          where
            step :: Map String [Int] -> Statement -> Map String [Int]
            step acc stmt = case stmt of
                StaticMethod _ (Variable name _) params _ ->
                    insertArity name (length params) acc
                InstanceMethod _ (Variable name _) params _ ->
                    insertArity name (length params) acc
                NativeMethod _ (Variable name _) params _ ->
                    insertArity name (length params) acc
                Function _ (Variable name _) params _ ->
                    insertArity name (length params) acc
                _ -> acc

            insertArity :: String -> Int -> Map String [Int] -> Map String [Int]
            insertArity name arity =
                Map.insertWith
                    (\new old -> nub (new ++ old))
                    name
                    [arity]

        hasConcreteArity :: String -> Int -> Bool
        hasConcreteArity name arity =
            maybe False (elem arity) (Map.lookup name concreteArities)

        templateCalleeName :: Expression -> Maybe (String, Token)
        templateCalleeName calleeE = case calleeE of
            Variable name tok ->
                Just (name, tok)
            Qualified names toks -> case reverse names of
                (name:_) -> case reverse toks of
                    (tok:_) -> Just (name, tok)
                    [] -> Nothing
                [] -> Nothing
            _ -> Nothing

        hasInferableTemplateArity :: Map String [TemplateDef] -> String -> Int -> Bool
        hasInferableTemplateArity defs0 name arity =
            case Map.lookup name defs0 of
                Nothing -> False
                Just ds ->
                    any
                        (\d -> length (tdParams d) == arity && not (null (tdTypeParams d)))
                        ds

        hasMatchingTemplate :: Map String [TemplateDef] -> String -> Int -> Int -> Bool
        hasMatchingTemplate defs0 name arity tArgCount =
            case Map.lookup name defs0 of
                Nothing -> False
                Just xs ->
                    any (\d -> length (tdParams d) == arity && length (tdTypeParams d) == tArgCount) xs

        templateTypeArgCountMismatch ::
            Map String [TemplateDef] ->
            String ->
            Int ->
            Int ->
            Maybe [Int]
        templateTypeArgCountMismatch defs0 name arity tArgCount =
            case Map.lookup name defs0 of
                Nothing -> Nothing
                Just xs ->
                    let sameArity = filter (\d -> length (tdParams d) == arity) xs
                    in if null sameArity
                        then Nothing
                        else
                            let expected = sort (nub (map (length . tdTypeParams) sameArity))
                            in if tArgCount `elem` expected
                                then Nothing
                                else Just expected

        formatExpectedTypeArgCounts :: [Int] -> String
        formatExpectedTypeArgCounts counts = case counts of
            [] -> "unknown"
            [n] -> show n
            _ -> concat ["one of {", intercalate ", " (map show counts), "}"]

        inferExprTypeHint :: Map String Class -> Expression -> Maybe Class
        inferExprTypeHint hints expr = case expr of
            IntConst {} -> Just Int32T
            LongConst {} -> Just Int64T
            FloatConst {} -> Just Float32T
            DoubleConst {} -> Just Float64T
            LongDoubleConst {} -> Just Float128T
            CharConst {} -> Just Char
            BoolConst {} -> Just Bool
            StringConst {} -> Just (Class ["String"] [])
            Variable n _ -> Map.lookup n hints
            Cast (ty, _) _ _ -> Just (normalizeClass ty)
            Unary AddrOf e _ -> Pointer <$> inferExprTypeHint hints e
            Unary DeRef e _ -> case inferExprTypeHint hints e of
                Just (Pointer inner) -> Just inner
                _ -> Nothing
            Unary _ e _ -> inferExprTypeHint hints e
            Binary Assign _ rhs _ -> inferExprTypeHint hints rhs
            Binary Add e1 e2 _ -> inferAddSub hints e1 e2
            Binary Sub e1 e2 _ -> inferAddSub hints e1 e2
            Binary Mul e1 e2 _ -> inferNumeric hints e1 e2
            Binary Div e1 e2 _ -> inferNumeric hints e1 e2
            Binary Mod e1 e2 _ -> inferNumeric hints e1 e2
            Binary Pow e1 e2 _ -> inferNumeric hints e1 e2
            Binary LessThan _ _ _ -> Just Bool
            Binary LessEqual _ _ _ -> Just Bool
            Binary GreaterThan _ _ _ -> Just Bool
            Binary GreaterEqual _ _ _ -> Just Bool
            Binary Equal _ _ _ -> Just Bool
            Binary NotEqual _ _ _ -> Just Bool
            Binary LogicalAnd _ _ _ -> Just Bool
            Binary LogicalOr _ _ _ -> Just Bool
            Binary LogicalImply _ _ _ -> Just Bool
            Binary LogicalNimply _ _ _ -> Just Bool
            Binary LogicalNand _ _ _ -> Just Bool
            Binary LogicalNor _ _ _ -> Just Bool
            IfExpr _ t1 t2 _ ->
                case (inferExprTypeHint hints t1, inferExprTypeHint hints t2) of
                    (Just a, Just b) | a == b -> Just a
                    _ -> Nothing
            Call callee args ->
                inferCallReturnHint hints callee args
            CallT callee tys args ->
                inferCallTReturnHint hints callee tys args
            SizeOfExpr {} -> Just Int32T
            SizeOfType {} -> Just Int32T
            BlockExpr (Multiple ss) -> inferBlockTail hints ss
            _ -> Nothing
          where
            inferBlockTail :: Map String Class -> [Statement] -> Maybe Class
            inferBlockTail _ [] = Nothing
            inferBlockTail hs xs = case reverse xs of
                (Expr eLast:_) -> inferExprTypeHint hs eLast
                _ -> Nothing

            inferCallReturnHint :: Map String Class -> Expression -> [Expression] -> Maybe Class
            inferCallReturnHint hs callee args = case templateCalleeName callee of
                Just (name, _) ->
                    inferTemplateReturnHint hs name (length args) args
                Nothing -> Nothing

            inferCallTReturnHint ::
                Map String Class ->
                Expression ->
                [(Class, [Token])] ->
                [Expression] ->
                Maybe Class
            inferCallTReturnHint hs callee tys args = case callee of
                Variable name _ ->
                    let arity = length args
                        explicitTypeArgs = map (normalizeClass . fst) tys
                    in
                        if null explicitTypeArgs
                            then inferTemplateReturnHint hs name arity args
                            else inferTemplateReturnHintWithArgs name arity explicitTypeArgs
                _ -> case templateCalleeName callee of
                    Just (name, _) ->
                        let arity = length args
                            explicitTypeArgs = map (normalizeClass . fst) tys
                        in
                            if null explicitTypeArgs
                                then inferTemplateReturnHint hs name arity args
                                else inferTemplateReturnHintWithArgs name arity explicitTypeArgs
                    Nothing -> Nothing

            inferTemplateReturnHint ::
                Map String Class ->
                String ->
                Int ->
                [Expression] ->
                Maybe Class
            inferTemplateReturnHint hs name arity args =
                case inferTemplateTypeArgsFromCall templateDefsAll hs name arity args of
                    Right inferredArgs ->
                        inferTemplateReturnHintWithArgs name arity inferredArgs
                    Left _ -> Nothing

            inferTemplateReturnHintWithArgs ::
                String ->
                Int ->
                [Class] ->
                Maybe Class
            inferTemplateReturnHintWithArgs name arity typeArgs =
                case Map.lookup name templateDefsAll of
                    Nothing -> Nothing
                    Just ds ->
                        let sameArity = filter (\d -> length (tdParams d) == arity) ds
                            exactArityAndTypeCount =
                                filter (\d -> length (tdTypeParams d) == length typeArgs) sameArity
                            retTypes = mapMaybe (retTypeForDef typeArgs) exactArityAndTypeCount
                            uniqRetTypes = nub retTypes
                        in case uniqRetTypes of
                            [one] -> Just one
                            _ -> Nothing

            retTypeForDef :: [Class] -> TemplateDef -> Maybe Class
            retTypeForDef typeArgs def = do
                subst <- buildTypeSubst def typeArgs
                pure (normalizeClass (substituteClass subst (normalizeClass (fst (tdRet def)))))

            inferAddSub :: Map String Class -> Expression -> Expression -> Maybe Class
            inferAddSub hs e1 e2 = do
                t1 <- inferExprTypeHint hs e1
                t2 <- inferExprTypeHint hs e2
                case (t1, t2) of
                    (Pointer inner, t) | isIntegerHintType t -> Just (Pointer inner)
                    (t, Pointer inner) | isIntegerHintType t -> Just (Pointer inner)
                    _ -> numericWiden t1 t2

            inferNumeric :: Map String Class -> Expression -> Expression -> Maybe Class
            inferNumeric hs e1 e2 = do
                t1 <- inferExprTypeHint hs e1
                t2 <- inferExprTypeHint hs e2
                numericWiden t1 t2

            numericWiden :: Class -> Class -> Maybe Class
            numericWiden a b =
                let rank t = case normalizeClass t of
                        Int8T -> Just (1 :: Int)
                        Int16T -> Just 2
                        Int32T -> Just 3
                        Int64T -> Just 4
                        Float32T -> Just 5
                        Float64T -> Just 6
                        Float128T -> Just 7
                        _ -> Nothing
                    fromRank r = case r of
                        1 -> Int8T
                        2 -> Int16T
                        3 -> Int32T
                        4 -> Int64T
                        5 -> Float32T
                        6 -> Float64T
                        7 -> Float128T
                        _ -> Int32T
                in case (rank a, rank b) of
                    (Just ra, Just rb) -> Just (fromRank (max ra rb))
                    _ -> Nothing

            isIntegerHintType :: Class -> Bool
            isIntegerHintType t = case normalizeClass t of
                Int8T -> True
                Int16T -> True
                Int32T -> True
                Int64T -> True
                _ -> False

        insertTypeHints :: Map String Class -> [String] -> Class -> Map String Class
        insertTypeHints hints names ty =
            foldl (\m n -> Map.insert n (normalizeClass ty) m) hints names

        updateHintsFromDecl ::
            Map String Class ->
            [String] ->
            Maybe Class ->
            Maybe Expression ->
            Map String Class
        updateHintsFromDecl hints names mTy mRhs =
            case mTy of
                Just ty -> insertTypeHints hints names ty
                Nothing ->
                    case mRhs >>= inferExprTypeHint hints of
                        Just ty -> insertTypeHints hints names ty
                        Nothing -> hints

        inferTemplateTypeArgsFromCall ::
            Map String [TemplateDef] ->
            Map String Class ->
            String ->
            Int ->
            [Expression] ->
            Either String [Class]
        inferTemplateTypeArgsFromCall defs0 hints name arity argsN =
            case Map.lookup name defs0 of
                Nothing -> Left ("'" ++ name ++ "' is not a template in this context")
                Just ds ->
                    let sameArity = filter (\d -> length (tdParams d) == arity) ds
                        templated = filter (\d -> not (null (tdTypeParams d))) sameArity
                        inferred = mapMaybe (\d -> inferTypeArgsForDef d hints argsN) templated
                        uniques = nub inferred
                    in case templated of
                        [] -> Left ("'" ++ name ++ "' is not a template in this context")
                        _ -> case uniques of
                            [tys] -> Right tys
                            [] -> Left ("cannot infer template type argument(s) for " ++ name)
                            _ -> Left ("ambiguous template type inference for " ++ name)

        inferTemplateTypeArgsFromFuncPtrExpected ::
            Map String [TemplateDef] ->
            String ->
            Class ->
            [Class] ->
            Either String [Class]
        inferTemplateTypeArgsFromFuncPtrExpected defs0 name retTy argTys =
            case Map.lookup name defs0 of
                Nothing -> Left ("'" ++ name ++ "' is not a template in this context")
                Just ds ->
                    let arity = length argTys
                        sameArity = filter (\d -> length (tdParams d) == arity) ds
                        templated = filter (\d -> not (null (tdTypeParams d))) sameArity
                        inferred = mapMaybe (\d -> inferTypeArgsForFuncPtrDef d retTy argTys) templated
                        uniques = nub inferred
                    in case templated of
                        [] -> Left ("'" ++ name ++ "' is not a template in this context")
                        _ -> case uniques of
                            [tys] -> Right tys
                            [] -> Left ("cannot infer template type argument(s) for " ++ name)
                            _ -> Left ("ambiguous template type inference for " ++ name)
          where
            inferTypeArgsForFuncPtrDef :: TemplateDef -> Class -> [Class] -> Maybe [Class]
            inferTypeArgsForFuncPtrDef def expectedRet expectedArgs
                | length (tdParams def) /= length expectedArgs = Nothing
                | null (tdTypeParams def) = Nothing
                | otherwise = do
                    subst0 <- unifyMany tparamsSet Map.empty paramTypes expectedArgs
                    subst1 <- unifyTemplateType tparamsSet subst0 declaredRet (normalizeClass expectedRet)
                    mapM (`Map.lookup` subst1) (tdTypeParams def)
              where
                tparamsSet = Set.fromList (tdTypeParams def)
                paramTypes = map (\(t, _, _) -> normalizeClass t) (tdParams def)
                declaredRet = normalizeClass (fst (tdRet def))

                unifyTemplateType ::
                    Set String ->
                    Map String Class ->
                    Class ->
                    Class ->
                    Maybe (Map String Class)
                unifyTemplateType tset subst0 pTy aTy = case normalizeClass pTy of
                    Class [n] [] | Set.member n tset ->
                        case Map.lookup n subst0 of
                            Nothing -> Just (Map.insert n (normalizeClass aTy) subst0)
                            Just old
                                | normalizeClass old == normalizeClass aTy -> Just subst0
                                | otherwise -> Nothing
                    Pointer pInner -> case normalizeClass aTy of
                        Pointer aInner -> unifyTemplateType tset subst0 pInner aInner
                        _ -> Nothing
                    FuncPtr pRet pArgs -> case normalizeClass aTy of
                        FuncPtr aRet aArgs
                            | length pArgs == length aArgs -> do
                                subst1 <- unifyMany tset subst0 pArgs aArgs
                                unifyTemplateType tset subst1 pRet aRet
                        _ -> Nothing
                    Class pName pArgs -> case normalizeClass aTy of
                        Class aName aArgs
                            | pName == aName && length pArgs == length aArgs ->
                                unifyMany tset subst0 pArgs aArgs
                        _ -> Nothing
                    other ->
                        if other == normalizeClass aTy then Just subst0 else Nothing

                unifyMany ::
                    Set String ->
                    Map String Class ->
                    [Class] ->
                    [Class] ->
                    Maybe (Map String Class)
                unifyMany _ subst0 [] [] = Just subst0
                unifyMany tset subst0 (p:ps) (a:as) = do
                    subst1 <- unifyTemplateType tset subst0 p a
                    unifyMany tset subst1 ps as
                unifyMany _ _ _ _ = Nothing

        inferTypeArgsForDef :: TemplateDef -> Map String Class -> [Expression] -> Maybe [Class]
        inferTypeArgsForDef def hints argsN
            | length (tdParams def) /= length argsN = Nothing
            | null (tdTypeParams def) = Nothing
            | otherwise = do
                subst0 <- inferSubstArgs tparamsSet Map.empty paramTypes argsN
                subst1 <- inferReturnSubst subst0
                mapM (`Map.lookup` subst1) (tdTypeParams def)
          where
            paramTypes = map (\(t, _, _) -> normalizeClass t) (tdParams def)
            tparamsSet = Set.fromList (tdTypeParams def)

            inferSubstArgs ::
                Set String ->
                Map String Class ->
                [Class] ->
                [Expression] ->
                Maybe (Map String Class)
            inferSubstArgs _ subst0 [] [] = Just subst0
            inferSubstArgs tset subst0 (p:ps) (argE:as) =
                case inferExprTypeHint hints argE of
                    Just aTy -> do
                        subst1 <- unifyTemplateType tset subst0 (normalizeClass p) (normalizeClass aTy)
                        inferSubstArgs tset subst1 ps as
                    Nothing ->
                        inferSubstArgs tset subst0 ps as
            inferSubstArgs _ _ _ _ = Nothing

            unifyTemplateType ::
                Set String ->
                Map String Class ->
                Class ->
                Class ->
                Maybe (Map String Class)
            unifyTemplateType tset subst0 pTy aTy = case normalizeClass pTy of
                Class [n] [] | Set.member n tset ->
                    case Map.lookup n subst0 of
                        Nothing -> Just (Map.insert n (normalizeClass aTy) subst0)
                        Just old
                            | normalizeClass old == normalizeClass aTy -> Just subst0
                            | otherwise -> Nothing
                Pointer pInner -> case normalizeClass aTy of
                    Pointer aInner -> unifyTemplateType tset subst0 pInner aInner
                    _ -> Nothing
                FuncPtr pRet pArgs -> case normalizeClass aTy of
                    FuncPtr aRet aArgs
                        | length pArgs == length aArgs -> do
                            subst1 <- unifyMany tset subst0 pArgs aArgs
                            unifyTemplateType tset subst1 pRet aRet
                    _ -> Nothing
                Class pName pArgs -> case normalizeClass aTy of
                    Class aName aArgs
                        | pName == aName && length pArgs == length aArgs ->
                            unifyMany tset subst0 pArgs aArgs
                    _ -> Nothing
                other ->
                    if other == normalizeClass aTy then Just subst0 else Nothing

            unifyMany ::
                Set String ->
                Map String Class ->
                [Class] ->
                [Class] ->
                Maybe (Map String Class)
            unifyMany _ subst0 [] [] = Just subst0
            unifyMany tset subst0 (p:ps) (a:as) = do
                subst1 <- unifyTemplateType tset subst0 p a
                unifyMany tset subst1 ps as
            unifyMany _ _ _ _ = Nothing

            inferReturnSubst :: Map String Class -> Maybe (Map String Class)
            inferReturnSubst subst0 =
                let unresolved = filter (`Map.notMember` subst0) (tdTypeParams def)
                in if null unresolved
                    then Just subst0
                    else do
                        retBodyTy <- inferTemplateBodyReturnType subst0
                        let declaredRetTy = substituteTemplateClass subst0 (normalizeClass (fst (tdRet def)))
                        unifyTemplateType tparamsSet subst0 declaredRetTy (normalizeClass retBodyTy)

            inferTemplateBodyReturnType :: Map String Class -> Maybe Class
            inferTemplateBodyReturnType subst0 =
                let paramHints = foldl addParamHint hints (tdParams def)
                    bodyHints = inferLocalHintsFromBlock paramHints (tdBody def)
                    returns = collectReturnsFromBlock (tdBody def)
                in case returns of
                    [] ->
                        inferTailTypeFromBlock bodyHints (tdBody def)
                    _ ->
                        inferMergedReturnType bodyHints subst0 returns
              where
                addParamHint :: Map String Class -> ParamDecl -> Map String Class
                addParamHint acc (t, n, _) =
                    Map.insert n (substituteTemplateClass subst0 (normalizeClass t)) acc

                inferLocalHintsFromBlock :: Map String Class -> Block -> Map String Class
                inferLocalHintsFromBlock hs (Multiple ss) = foldl' inferLocalHintsFromStmt hs ss

                inferLocalHintsFromStmt :: Map String Class -> Statement -> Map String Class
                inferLocalHintsFromStmt hs st = case st of
                    DefField names mTy mRhs _ ->
                        updateHintsFromDecl hs names (fmap substTy mTy) mRhs
                    DefConstField names mTy mRhs _ ->
                        updateHintsFromDecl hs names (fmap substTy mTy) mRhs
                    DefVar names mTy mRhs _ ->
                        updateHintsFromDecl hs names (fmap substTy mTy) mRhs
                    DefConstVar names mTy mRhs _ ->
                        updateHintsFromDecl hs names (fmap substTy mTy) mRhs
                    Expr e ->
                        inferLocalHintsFromExpr hs e
                    Exprs es ->
                        foldl' inferLocalHintsFromExpr hs es
                    StmtGroup ss ->
                        foldl' inferLocalHintsFromStmt hs ss
                    BlockStmt b ->
                        inferLocalHintsFromBlock hs b
                    _ -> hs

                inferLocalHintsFromExpr :: Map String Class -> Expression -> Map String Class
                inferLocalHintsFromExpr hs expr = case expr of
                    Binary Assign (Variable n _) rhsE _ ->
                        case inferExprTypeHint hs rhsE of
                            Just ty -> Map.insert n (normalizeClass ty) hs
                            Nothing -> hs
                    _ -> hs

                substTy :: Class -> Class
                substTy = substituteTemplateClass subst0 . normalizeClass

            collectReturnsFromBlock :: Block -> [Maybe Expression]
            collectReturnsFromBlock (Multiple ss) = concatMap collectReturnsFromStmt ss

            collectReturnsFromStmt :: Statement -> [Maybe Expression]
            collectReturnsFromStmt st = case st of
                Command (Return me) _ -> [me]
                StmtGroup ss -> concatMap collectReturnsFromStmt ss
                BlockStmt b -> collectReturnsFromBlock b
                If _ b1 b2 _ ->
                    collectFromMaybeBlock b1 ++ collectFromMaybeBlock b2
                For (_, _, _) b1 b2 _ ->
                    collectFromMaybeBlock b1 ++ collectFromMaybeBlock b2
                Loop mb _ ->
                    collectFromMaybeBlock mb
                Repeat _ b1 b2 _ ->
                    collectFromMaybeBlock b1 ++ collectFromMaybeBlock b2
                While _ b1 b2 _ ->
                    collectFromMaybeBlock b1 ++ collectFromMaybeBlock b2
                Until _ b1 b2 _ ->
                    collectFromMaybeBlock b1 ++ collectFromMaybeBlock b2
                DoWhile b1 _ b2 _ ->
                    collectFromMaybeBlock b1 ++ collectFromMaybeBlock b2
                DoUntil b1 _ b2 _ ->
                    collectFromMaybeBlock b1 ++ collectFromMaybeBlock b2
                Switch _ scs _ ->
                    concatMap collectReturnsFromCase scs
                _ -> []

            collectReturnsFromCase :: SwitchCase -> [Maybe Expression]
            collectReturnsFromCase sc = case sc of
                Case _ mb _ -> collectFromMaybeBlock mb
                Default b _ -> collectReturnsFromBlock b

            collectFromMaybeBlock :: Maybe Block -> [Maybe Expression]
            collectFromMaybeBlock Nothing = []
            collectFromMaybeBlock (Just b) = collectReturnsFromBlock b

            inferTailTypeFromBlock :: Map String Class -> Block -> Maybe Class
            inferTailTypeFromBlock hs (Multiple ss) = inferTailTypeFromStmts hs ss

            inferTailTypeFromStmts :: Map String Class -> [Statement] -> Maybe Class
            inferTailTypeFromStmts _ [] = Nothing
            inferTailTypeFromStmts hs xs = case reverse xs of
                (one:_) -> inferTailTypeFromStmt hs one
                [] -> Nothing

            inferTailTypeFromStmt :: Map String Class -> Statement -> Maybe Class
            inferTailTypeFromStmt hs st = case st of
                Expr e -> inferExprTypeHint hs e
                Exprs es -> case reverse es of
                    (e:_) -> inferExprTypeHint hs e
                    [] -> Nothing
                StmtGroup ss -> inferTailTypeFromStmts hs ss
                BlockStmt b -> inferTailTypeFromBlock hs b
                _ -> Nothing

            inferMergedReturnType :: Map String Class -> Map String Class -> [Maybe Expression] -> Maybe Class
            inferMergedReturnType hs substBase retExprs =
                let hasVoid = any isNothing retExprs
                    hasValue = any isJust retExprs
                in case (hasVoid, hasValue) of
                    (True, True) -> Nothing
                    (True, False) -> Just Void
                    (False, True) -> do
                        ts <- mapM (\me -> me >>= inferExprTypeHint hs) retExprs
                        mergeReturnTypes substBase ts
                    (False, False) -> Nothing

            mergeReturnTypes :: Map String Class -> [Class] -> Maybe Class
            mergeReturnTypes _ [] = Nothing
            mergeReturnTypes substBase (t:ts) = foldl' step (Just (normalizeClass t)) (map normalizeClass ts)
              where
                step :: Maybe Class -> Class -> Maybe Class
                step Nothing _ = Nothing
                step (Just acc) one = mergeTwo acc one

                mergeTwo :: Class -> Class -> Maybe Class
                mergeTwo a b
                    | a == b = Just a
                    | otherwise =
                        case unifyTemplateType tparamsSet substBase a b of
                            Just substX -> Just (substituteTemplateClass substX (normalizeClass a))
                            Nothing
                                | bothInt a b -> Just (widerInt a b)
                                | bothFloat a b -> Just (widerFloat a b)
                                | isNumeric a && isNumeric b -> Just (widerNumeric a b)
                                | otherwise -> Nothing

                bothInt :: Class -> Class -> Bool
                bothInt x y = isIntClass x && isIntClass y

                bothFloat :: Class -> Class -> Bool
                bothFloat x y = isFloatClass x && isFloatClass y

                isNumeric :: Class -> Bool
                isNumeric x = isIntClass x || isFloatClass x

                isIntClass :: Class -> Bool
                isIntClass x = case normalizeClass x of
                    Int8T -> True
                    Int16T -> True
                    Int32T -> True
                    Int64T -> True
                    _ -> False

                isFloatClass :: Class -> Bool
                isFloatClass x = case normalizeClass x of
                    Float32T -> True
                    Float64T -> True
                    Float128T -> True
                    _ -> False

                widerInt :: Class -> Class -> Class
                widerInt x y = fromIntRank (max (intRank x) (intRank y))

                intRank :: Class -> Int
                intRank x = case normalizeClass x of
                    Int8T -> 1
                    Int16T -> 2
                    Int32T -> 3
                    Int64T -> 4
                    _ -> 0

                fromIntRank :: Int -> Class
                fromIntRank r = case r of
                    1 -> Int8T
                    2 -> Int16T
                    3 -> Int32T
                    4 -> Int64T
                    _ -> Int32T

                widerFloat :: Class -> Class -> Class
                widerFloat x y = fromFloatRank (max (floatRank x) (floatRank y))

                floatRank :: Class -> Int
                floatRank x = case normalizeClass x of
                    Float32T -> 1
                    Float64T -> 2
                    Float128T -> 3
                    _ -> 0

                fromFloatRank :: Int -> Class
                fromFloatRank r = case r of
                    1 -> Float32T
                    2 -> Float64T
                    3 -> Float128T
                    _ -> Float64T

                widerNumeric :: Class -> Class -> Class
                widerNumeric x y
                    | isFloatClass x && isIntClass y = x
                    | isIntClass x && isFloatClass y = y
                    | otherwise = Float64T

            substituteTemplateClass :: Map String Class -> Class -> Class
            substituteTemplateClass subst0 cls = case normalizeClass cls of
                Class [n] [] -> fromMaybe (Class [n] []) (Map.lookup n subst0)
                Class qn args -> Class qn (map (substituteTemplateClass subst0) args)
                Pointer inner -> Pointer (substituteTemplateClass subst0 inner)
                FuncPtr ret args ->
                    FuncPtr (substituteTemplateClass subst0 ret) (map (substituteTemplateClass subst0) args)
                Blob e -> Blob e
                other -> other

        rewriteStmts ::
            Bool ->
            Map String [TemplateDef] ->
            Map String Class ->
            [Statement] ->
            ([Statement], [TemplateReq], Map String Class)
        rewriteStmts rewriteTemplateBodies defs0 hints0 stmtsN =
            let step (ssAcc, reqAcc, hintsAcc) st =
                    let (st', reqs, hints') = rewriteStmt rewriteTemplateBodies defs0 hintsAcc st
                    in (st' : ssAcc, reqs ++ reqAcc, hints')
                (ssRev, reqRev, hintsOut) = foldl step ([], [], hints0) stmtsN
            in (reverse ssRev, reverse reqRev, hintsOut)

        rewriteStmt ::
            Bool ->
            Map String [TemplateDef] ->
            Map String Class ->
            Statement ->
            (Statement, [TemplateReq], Map String Class)
        rewriteStmt rewriteTemplateBodies defs0 hints stmt = case stmt of
            FunctionT {} | not rewriteTemplateBodies -> (stmt, [], hints)
            Command (Return me) tok ->
                let (me', reqs) = rewriteMaybeExpr defs0 hints me
                in (Command (Return me') tok, reqs, hints)
            Command _ _ -> (stmt, [], hints)
            Expr e ->
                let (e', reqs) = rewriteExpr defs0 hints e
                    hints' = case e' of
                        Binary Assign (Variable n _) rhsE _ ->
                            case inferExprTypeHint hints rhsE of
                                Just ty -> Map.insert n ty hints
                                Nothing -> hints
                        _ -> hints
                in (Expr e', reqs, hints')
            Exprs es ->
                let (es', reqs) = rewriteExprs defs0 hints es
                in (Exprs es', reqs, hints)
            DefField names mTy mRhs toks ->
                let (mRhs', reqs) = rewriteMaybeExpr defs0 hints mRhs
                    hints' = updateHintsFromDecl hints names mTy mRhs'
                in (DefField names mTy mRhs' toks, reqs, hints')
            DefConstField names mTy mRhs toks ->
                let (mRhs', reqs) = rewriteMaybeExpr defs0 hints mRhs
                    hints' = updateHintsFromDecl hints names mTy mRhs'
                in (DefConstField names mTy mRhs' toks, reqs, hints')
            DefVar names mTy mRhs toks ->
                let (mRhs', reqs) = rewriteMaybeExpr defs0 hints mRhs
                    hints' = updateHintsFromDecl hints names mTy mRhs'
                in (DefVar names mTy mRhs' toks, reqs, hints')
            DefConstVar names mTy mRhs toks ->
                let (mRhs', reqs) = rewriteMaybeExpr defs0 hints mRhs
                    hints' = updateHintsFromDecl hints names mTy mRhs'
                in (DefConstVar names mTy mRhs' toks, reqs, hints')
            StmtGroup ss ->
                let (ss', reqs, hints') = rewriteStmts rewriteTemplateBodies defs0 hints ss
                in (StmtGroup ss', reqs, hints')
            BlockStmt b ->
                let (b', reqs, _) = rewriteBlock rewriteTemplateBodies defs0 hints b
                in (BlockStmt b', reqs, hints)
            If e b1 b2 toks ->
                let (e', reqE) = rewriteExpr defs0 hints e
                    (b1', reqB1, _) = rewriteMaybeBlock rewriteTemplateBodies defs0 hints b1
                    (b2', reqB2, _) = rewriteMaybeBlock rewriteTemplateBodies defs0 hints b2
                in (If e' b1' b2' toks, reqE ++ reqB1 ++ reqB2, hints)
            For (s1, e2, s3) b1 b2 toks ->
                let (s1', reqS1, _) = rewriteMaybeStmt rewriteTemplateBodies defs0 hints s1
                    (e2', reqE2) = rewriteMaybeExpr defs0 hints e2
                    (s3', reqS3, _) = rewriteMaybeStmt rewriteTemplateBodies defs0 hints s3
                    (b1', reqB1, _) = rewriteMaybeBlock rewriteTemplateBodies defs0 hints b1
                    (b2', reqB2, _) = rewriteMaybeBlock rewriteTemplateBodies defs0 hints b2
                in (For (s1', e2', s3') b1' b2' toks, reqS1 ++ reqE2 ++ reqS3 ++ reqB1 ++ reqB2, hints)
            Loop b tok ->
                let (b', reqs, _) = rewriteMaybeBlock rewriteTemplateBodies defs0 hints b
                in (Loop b' tok, reqs, hints)
            Repeat e b1 b2 toks ->
                let (e', reqE) = rewriteExpr defs0 hints e
                    (b1', reqB1, _) = rewriteMaybeBlock rewriteTemplateBodies defs0 hints b1
                    (b2', reqB2, _) = rewriteMaybeBlock rewriteTemplateBodies defs0 hints b2
                in (Repeat e' b1' b2' toks, reqE ++ reqB1 ++ reqB2, hints)
            While e b1 b2 toks ->
                let (e', reqE) = rewriteExpr defs0 hints e
                    (b1', reqB1, _) = rewriteMaybeBlock rewriteTemplateBodies defs0 hints b1
                    (b2', reqB2, _) = rewriteMaybeBlock rewriteTemplateBodies defs0 hints b2
                in (While e' b1' b2' toks, reqE ++ reqB1 ++ reqB2, hints)
            Until e b1 b2 toks ->
                let (e', reqE) = rewriteExpr defs0 hints e
                    (b1', reqB1, _) = rewriteMaybeBlock rewriteTemplateBodies defs0 hints b1
                    (b2', reqB2, _) = rewriteMaybeBlock rewriteTemplateBodies defs0 hints b2
                in (Until e' b1' b2' toks, reqE ++ reqB1 ++ reqB2, hints)
            DoWhile b1 e b2 toks ->
                let (b1', reqB1, _) = rewriteMaybeBlock rewriteTemplateBodies defs0 hints b1
                    (e', reqE) = rewriteExpr defs0 hints e
                    (b2', reqB2, _) = rewriteMaybeBlock rewriteTemplateBodies defs0 hints b2
                in (DoWhile b1' e' b2' toks, reqB1 ++ reqE ++ reqB2, hints)
            DoUntil b1 e b2 toks ->
                let (b1', reqB1, _) = rewriteMaybeBlock rewriteTemplateBodies defs0 hints b1
                    (e', reqE) = rewriteExpr defs0 hints e
                    (b2', reqB2, _) = rewriteMaybeBlock rewriteTemplateBodies defs0 hints b2
                in (DoUntil b1' e' b2' toks, reqB1 ++ reqE ++ reqB2, hints)
            Switch e scs tok ->
                let (e', reqE) = rewriteExpr defs0 hints e
                    (scs', reqS, _) = rewriteSwitchCases rewriteTemplateBodies defs0 hints scs
                in (Switch e' scs' tok, reqE ++ reqS, hints)
            Function ret name params body ->
                let paramHints = Map.fromList [(n, normalizeClass t) | (t, n, _) <- params]
                    (body', reqs, _) = rewriteBlock rewriteTemplateBodies defs0 (Map.union paramHints hints) body
                in (Function ret name params body', reqs, hints)
            StaticMethod ret name params body ->
                let paramHints = Map.fromList [(n, normalizeClass t) | (t, n, _) <- params]
                    (body', reqs, _) = rewriteBlock rewriteTemplateBodies defs0 (Map.union paramHints hints) body
                in (StaticMethod ret name params body', reqs, hints)
            InstanceMethod ret name params body ->
                let paramHints = Map.fromList [(n, normalizeClass t) | (t, n, _) <- params]
                    (body', reqs, _) = rewriteBlock rewriteTemplateBodies defs0 (Map.union paramHints hints) body
                in (InstanceMethod ret name params body', reqs, hints)
            NativeMethod {} -> (stmt, [], hints)
            FunctionT ret name gens params body ->
                let paramHints = Map.fromList [(n, normalizeClass t) | (t, n, _) <- params]
                    (body', reqs, _) = rewriteBlock rewriteTemplateBodies defs0 (Map.union paramHints hints) body
                    rebuilt = case stmt of
                        StaticMethodT {} -> StaticMethodT ret name gens params body'
                        InstanceMethodT {} -> InstanceMethodT ret name gens params body'
                        _ -> FunctionT ret name gens params body'
                in (rebuilt, reqs, hints)

        rewriteMaybeStmt ::
            Bool ->
            Map String [TemplateDef] ->
            Map String Class ->
            Maybe Statement ->
            (Maybe Statement, [TemplateReq], Map String Class)
        rewriteMaybeStmt _ _ hints Nothing = (Nothing, [], hints)
        rewriteMaybeStmt rewriteTemplateBodies defs0 hints (Just st) =
            let (st', reqs, hints') = rewriteStmt rewriteTemplateBodies defs0 hints st
            in (Just st', reqs, hints')

        rewriteBlock ::
            Bool ->
            Map String [TemplateDef] ->
            Map String Class ->
            Block ->
            (Block, [TemplateReq], Map String Class)
        rewriteBlock rewriteTemplateBodies defs0 hints (Multiple ss) =
            let (ss', reqs, hints') = rewriteStmts rewriteTemplateBodies defs0 hints ss
            in (Multiple ss', reqs, hints')

        rewriteMaybeBlock ::
            Bool ->
            Map String [TemplateDef] ->
            Map String Class ->
            Maybe Block ->
            (Maybe Block, [TemplateReq], Map String Class)
        rewriteMaybeBlock _ _ hints Nothing = (Nothing, [], hints)
        rewriteMaybeBlock rewriteTemplateBodies defs0 hints (Just b) =
            let (b', reqs, hints') = rewriteBlock rewriteTemplateBodies defs0 hints b
            in (Just b', reqs, hints')

        rewriteSwitchCase ::
            Bool ->
            Map String [TemplateDef] ->
            Map String Class ->
            SwitchCase ->
            (SwitchCase, [TemplateReq], Map String Class)
        rewriteSwitchCase rewriteTemplateBodies defs0 hints sc = case sc of
            Case e mb tok ->
                let (e', reqE) = rewriteExpr defs0 hints e
                    (mb', reqB, _) = rewriteMaybeBlock rewriteTemplateBodies defs0 hints mb
                in (Case e' mb' tok, reqE ++ reqB, hints)
            Default b tok ->
                let (b', reqs, _) = rewriteBlock rewriteTemplateBodies defs0 hints b
                in (Default b' tok, reqs, hints)

        rewriteSwitchCases ::
            Bool ->
            Map String [TemplateDef] ->
            Map String Class ->
            [SwitchCase] ->
            ([SwitchCase], [TemplateReq], Map String Class)
        rewriteSwitchCases rewriteTemplateBodies defs0 hints0 scs =
            let step (accS, accR, hintsAcc) sc =
                    let (sc', reqs, hints') = rewriteSwitchCase rewriteTemplateBodies defs0 hintsAcc sc
                    in (sc' : accS, reqs ++ accR, hints')
                (scRev, reqRev, hintsOut) = foldl step ([], [], hints0) scs
            in (reverse scRev, reverse reqRev, hintsOut)

        rewriteMaybeExpr ::
            Map String [TemplateDef] ->
            Map String Class ->
            Maybe Expression ->
            (Maybe Expression, [TemplateReq])
        rewriteMaybeExpr _ _ Nothing = (Nothing, [])
        rewriteMaybeExpr defs0 hints (Just e) =
            let (e', reqs) = rewriteExpr defs0 hints e
            in (Just e', reqs)

        rewriteExprs ::
            Map String [TemplateDef] ->
            Map String Class ->
            [Expression] ->
            ([Expression], [TemplateReq])
        rewriteExprs defs0 hints = foldr step ([], [])
          where
            step e (accE, accR) =
                let (e', reqs) = rewriteExpr defs0 hints e
                in (e' : accE, reqs ++ accR)

        rewriteExpr ::
            Map String [TemplateDef] ->
            Map String Class ->
            Expression ->
            (Expression, [TemplateReq])
        rewriteExpr defs0 hints expr = case expr of
            Error {} -> (expr, [])
            IntConst {} -> (expr, [])
            LongConst {} -> (expr, [])
            FloatConst {} -> (expr, [])
            DoubleConst {} -> (expr, [])
            LongDoubleConst {} -> (expr, [])
            CharConst {} -> (expr, [])
            StringConst {} -> (expr, [])
            BoolConst {} -> (expr, [])
            Variable {} -> (expr, [])
            Qualified {} -> (expr, [])
            Cast ty e tok ->
                let (e', reqs) = rewriteExpr defs0 hints e
                in (Cast ty e' tok, reqs)
            Unary op e tok ->
                let (e', reqs) = rewriteExpr defs0 hints e
                in (Unary op e' tok, reqs)
            Binary op e1 e2 tok ->
                let (e1', req1) = rewriteExpr defs0 hints e1
                    (e2', req2) = rewriteExpr defs0 hints e2
                in (Binary op e1' e2' tok, req1 ++ req2)
            SizeOfExpr inner tok ->
                let (inner', reqs) = rewriteExpr defs0 hints inner
                in (SizeOfExpr inner' tok, reqs)
            SizeOfType {} ->
                (expr, [])
            Call callee args ->
                let (callee', req0) = rewriteExpr defs0 hints callee
                    (args', reqA) = rewriteExprs defs0 hints args
                    arity = length args'
                    reqBase = req0 ++ reqA
                in case templateCalleeName callee' of
                    Just (name, tok)
                        | not (hasConcreteArity name arity)
                        , hasInferableTemplateArity defs0 name arity ->
                            case inferTemplateTypeArgsFromCall defs0 hints name arity args' of
                                Right inferredArgs ->
                                    let req = (name, inferredArgs, arity)
                                        argsCast = applyTemplateArgCasts defs0 name inferredArgs arity tok args'
                                        extraReqs = fromMaybe [] (collectFuncPtrTemplateReqs defs0 <$> concreteParamTypes defs0 name inferredArgs arity <*> pure args')
                                    in (Call (Variable name tok) argsCast, reqBase ++ [req] ++ extraReqs)
                                Left inferWhy ->
                                    let why = "template type argument inference failed: " ++ inferWhy
                                    in (Error [tok] why, reqBase)
                    _ ->
                        (Call callee' args', reqBase)
            CallT callee tys args ->
                let (callee', req0) = rewriteExpr defs0 hints callee
                    (args', reqA) = rewriteExprs defs0 hints args
                    typeArgs = map (normalizeClass . fst) tys
                    arity = length args'
                    reqBase = req0 ++ reqA
                in case templateCalleeName callee' of
                    Just (name, tok)
                        | null typeArgs ->
                            case Map.lookup name defs0 of
                                Just _ ->
                                    case inferTemplateTypeArgsFromCall defs0 hints name arity args' of
                                        Right inferredArgs ->
                                            let req = (name, inferredArgs, arity)
                                                argsCast = applyTemplateArgCasts defs0 name inferredArgs arity tok args'
                                                extraReqs = fromMaybe [] (collectFuncPtrTemplateReqs defs0 <$> concreteParamTypes defs0 name inferredArgs arity <*> pure args')
                                            in (Call (Variable name tok) argsCast, reqBase ++ [req] ++ extraReqs)
                                        Left inferWhy ->
                                            let why = "template type argument inference failed: " ++ inferWhy
                                            in (Error [tok] why, reqBase)
                                Nothing ->
                                    (Call callee' args', reqBase)
                        | hasMatchingTemplate defs0 name arity (length typeArgs) ->
                            let req = (name, typeArgs, arity)
                                argsCast = applyTemplateArgCasts defs0 name typeArgs arity tok args'
                                extraReqs = fromMaybe [] (collectFuncPtrTemplateReqs defs0 <$> concreteParamTypes defs0 name typeArgs arity <*> pure args')
                            in (Call (Variable name tok) argsCast, reqBase ++ [req] ++ extraReqs)
                        | Just expected <- templateTypeArgCountMismatch defs0 name arity (length typeArgs) ->
                            let typeTok = case concatMap snd tys of
                                    (t:_) -> t
                                    [] -> tok
                                why = "template type argument count mismatch: "
                                    ++ name
                                    ++ " expects "
                                    ++ formatExpectedTypeArgCounts expected
                                    ++ " type argument(s), got "
                                    ++ show (length typeArgs)
                            in (Error [typeTok] why, reqBase)
                    _ ->
                        -- keep execution deterministic: once type args are parsed,
                        -- lower as an ordinary call if no local template definition matches.
                        (Call callee' args', reqBase)
            IfExpr cond thenE elseE toks ->
                let (cond', req0) = rewriteExpr defs0 hints cond
                    (thenE', req1) = rewriteExpr defs0 hints thenE
                    (elseE', req2) = rewriteExpr defs0 hints elseE
                in (IfExpr cond' thenE' elseE' toks, req0 ++ req1 ++ req2)
            BlockExpr b ->
                let (b', reqs, _) = rewriteBlock False defs0 hints b
                in (BlockExpr b', reqs)

        processReqs ::
            Map String [TemplateDef] ->
            Set TemplateReq ->
            Map TemplateReq Statement ->
            [Statement] ->
            [TemplateReq] ->
            (Set TemplateReq, Map TemplateReq Statement, [Statement])
        processReqs _ seen genMap genRev [] = (seen, genMap, genRev)
        processReqs defs0 seen genMap genRev (req:rest)
            | Set.member req seen = processReqs defs0 seen genMap genRev rest
            | otherwise =
                case instantiateReq defs0 req of
                    Nothing ->
                        processReqs defs0 (Set.insert req seen) genMap genRev rest
                    Just rawStmt ->
                        let (stmt', reqsNew, _) = rewriteStmt False defs0 Map.empty rawStmt
                            seen' = Set.insert req seen
                            genMap' = Map.insert req stmt' genMap
                            rest' = rest ++ reqsNew
                        in processReqs defs0 seen' genMap' (stmt' : genRev) rest'

        instantiateReq :: Map String [TemplateDef] -> TemplateReq -> Maybe Statement
        instantiateReq defs0 (name, typeArgs, arity) = do
            def <- selectTemplateDef defs0 name typeArgs arity
            subst <- buildTypeSubst def typeArgs
            let retTy = substituteClass subst (fst (tdRet def))
                retTok = tdRet def
                retToks' = generatedTemplateRetTokens (tdNameTok def) (snd retTok)
                params' = map (\(t, n, toks) -> (substituteClass subst t, n, toks)) (tdParams def)
                body' = substituteBlock subst (tdBody def)
                ret' = (retTy, retToks')
            if tdIsStatic def
                then Just (StaticMethod ret' (Variable name (tdNameTok def)) params' body')
                else Just (InstanceMethod ret' (Variable name (tdNameTok def)) params' body')

        selectTemplateDef :: Map String [TemplateDef] -> String -> [Class] -> Int -> Maybe TemplateDef
        selectTemplateDef defs0 name typeArgs arity = do
            ds <- Map.lookup name defs0
            let matched = filter (\d ->
                    length (tdParams d) == arity &&
                    length (tdTypeParams d) == length typeArgs) ds
            case matched of
                [one] -> Just one
                _ -> Nothing

        buildTypeSubst :: TemplateDef -> [Class] -> Maybe (Map String Class)
        buildTypeSubst def typeArgs
            | length (tdTypeParams def) /= length typeArgs = Nothing
            | otherwise = Just (Map.fromList (zip (tdTypeParams def) typeArgs))

        concreteParamTypes :: Map String [TemplateDef] -> String -> [Class] -> Int -> Maybe [Class]
        concreteParamTypes defs0 name typeArgs arity = do
            def <- selectTemplateDef defs0 name typeArgs arity
            subst <- buildTypeSubst def typeArgs
            pure (map (\(t, _, _) -> substituteClass subst t) (tdParams def))

        applyTemplateArgCasts :: Map String [TemplateDef] -> String -> [Class] -> Int -> Token -> [Expression] -> [Expression]
        applyTemplateArgCasts defs0 name typeArgs arity castTok argsN =
            case concreteParamTypes defs0 name typeArgs arity of
                Just paramTs
                    | length paramTs == length argsN ->
                        zipWith (\toT argE -> Cast (toT, [castTok]) argE castTok) paramTs argsN
                _ -> argsN

        collectFuncPtrTemplateReqs ::
            Map String [TemplateDef] ->
            [Class] ->
            [Expression] ->
            [TemplateReq]
        collectFuncPtrTemplateReqs defs0 paramTs argsN =
            concat (zipWith one paramTs argsN)
          where
            one :: Class -> Expression -> [TemplateReq]
            one pTy argE = case normalizeClass pTy of
                FuncPtr retTy fpArgTys -> case argE of
                    Variable fname _ ->
                        case inferTemplateTypeArgsFromFuncPtrExpected defs0 fname retTy fpArgTys of
                            Right typeArgs -> [(fname, typeArgs, length fpArgTys)]
                            Left _ -> []
                    _ -> []
                _ -> []

        generatedTemplateRetTokens :: Token -> [Token] -> [Token]
        generatedTemplateRetTokens nameTok toks =
            [ Lex.Ident "private" (tokenPos nameTok)
            , Lex.Ident "final" (tokenPos nameTok)
            ] ++ dropWhile isFunctionModifierToken toks

        substituteClass :: Map String Class -> Class -> Class
        substituteClass subst cls = case cls of
            Class [n] [] -> fromMaybe cls (Map.lookup n subst)
            Class qn args -> Class qn (map (substituteClass subst) args)
            Pointer inner -> Pointer (substituteClass subst inner)
            FuncPtr ret args -> FuncPtr (substituteClass subst ret) (map (substituteClass subst) args)
            Blob e -> Blob (substituteExpr subst e)
            _ -> cls

        substituteExpr :: Map String Class -> Expression -> Expression
        substituteExpr subst expr = case expr of
            Cast (ty, toks) e tok -> Cast (substituteClass subst ty, toks) (substituteExpr subst e) tok
            Unary op e tok -> Unary op (substituteExpr subst e) tok
            Binary op e1 e2 tok -> Binary op (substituteExpr subst e1) (substituteExpr subst e2) tok
            Call callee args -> Call (substituteExpr subst callee) (map (substituteExpr subst) args)
            CallT callee tys args ->
                let tys' = map (\(t, toks) -> (substituteClass subst t, toks)) tys
                in CallT (substituteExpr subst callee) tys' (map (substituteExpr subst) args)
            SizeOfExpr inner tok ->
                SizeOfExpr (substituteExpr subst inner) tok
            SizeOfType (ty, toks) tok ->
                SizeOfType (substituteClass subst ty, toks) tok
            IfExpr cond thenE elseE toks ->
                IfExpr
                    (substituteExpr subst cond)
                    (substituteExpr subst thenE)
                    (substituteExpr subst elseE)
                    toks
            BlockExpr b -> BlockExpr (substituteBlock subst b)
            _ -> expr

        substituteMaybeExpr :: Map String Class -> Maybe Expression -> Maybe Expression
        substituteMaybeExpr subst = fmap (substituteExpr subst)

        substituteBlock :: Map String Class -> Block -> Block
        substituteBlock subst (Multiple ss) = Multiple (map (substituteStmt subst) ss)

        substituteStmt :: Map String Class -> Statement -> Statement
        substituteStmt subst stmt = case stmt of
            Command (Return me) tok -> Command (Return (substituteMaybeExpr subst me)) tok
            Command Pass tok -> Command Pass tok
            Command Continue tok -> Command Continue tok
            Command Break tok -> Command Break tok
            Expr e -> Expr (substituteExpr subst e)
            Exprs es -> Exprs (map (substituteExpr subst) es)
            DefField names mTy mRhs toks -> DefField names (fmap (substituteClass subst) mTy) (substituteMaybeExpr subst mRhs) toks
            DefConstField names mTy mRhs toks -> DefConstField names (fmap (substituteClass subst) mTy) (substituteMaybeExpr subst mRhs) toks
            DefVar names mTy mRhs toks -> DefVar names (fmap (substituteClass subst) mTy) (substituteMaybeExpr subst mRhs) toks
            DefConstVar names mTy mRhs toks -> DefConstVar names (fmap (substituteClass subst) mTy) (substituteMaybeExpr subst mRhs) toks
            StmtGroup ss -> StmtGroup (map (substituteStmt subst) ss)
            BlockStmt b -> BlockStmt (substituteBlock subst b)
            If e b1 b2 toks ->
                If
                    (substituteExpr subst e)
                    (fmap (substituteBlock subst) b1)
                    (fmap (substituteBlock subst) b2)
                    toks
            For (s1, e2, s3) b1 b2 toks ->
                For
                    ( fmap (substituteStmt subst) s1
                    , fmap (substituteExpr subst) e2
                    , fmap (substituteStmt subst) s3
                    )
                    (fmap (substituteBlock subst) b1)
                    (fmap (substituteBlock subst) b2)
                    toks
            Loop b tok -> Loop (fmap (substituteBlock subst) b) tok
            Repeat e b1 b2 toks ->
                Repeat
                    (substituteExpr subst e)
                    (fmap (substituteBlock subst) b1)
                    (fmap (substituteBlock subst) b2)
                    toks
            While e b1 b2 toks ->
                While
                    (substituteExpr subst e)
                    (fmap (substituteBlock subst) b1)
                    (fmap (substituteBlock subst) b2)
                    toks
            Until e b1 b2 toks ->
                Until
                    (substituteExpr subst e)
                    (fmap (substituteBlock subst) b1)
                    (fmap (substituteBlock subst) b2)
                    toks
            DoWhile b1 e b2 toks ->
                DoWhile
                    (fmap (substituteBlock subst) b1)
                    (substituteExpr subst e)
                    (fmap (substituteBlock subst) b2)
                    toks
            DoUntil b1 e b2 toks ->
                DoUntil
                    (fmap (substituteBlock subst) b1)
                    (substituteExpr subst e)
                    (fmap (substituteBlock subst) b2)
                    toks
            Switch e scs tok ->
                Switch (substituteExpr subst e) (map (substituteCase subst) scs) tok
            Function (retT, retToks) name params body ->
                Function
                    (substituteClass subst retT, retToks)
                    name
                    (map (\(t, n, toks) -> (substituteClass subst t, n, toks)) params)
                    (substituteBlock subst body)
            StaticMethod (retT, retToks) name params body ->
                StaticMethod
                    (substituteClass subst retT, retToks)
                    name
                    (map (\(t, n, toks) -> (substituteClass subst t, n, toks)) params)
                    (substituteBlock subst body)
            InstanceMethod (retT, retToks) name params body ->
                InstanceMethod
                    (substituteClass subst retT, retToks)
                    name
                    (map (\(t, n, toks) -> (substituteClass subst t, n, toks)) params)
                    (substituteBlock subst body)
            NativeMethod (retT, retToks) name params target ->
                NativeMethod
                    (substituteClass subst retT, retToks)
                    name
                    (map (\(t, n, toks) -> (substituteClass subst t, n, toks)) params)
                    target
            FunctionT (retT, retToks) name gens params body ->
                let body' = substituteBlock subst body
                    ret' = (substituteClass subst retT, retToks)
                    gens' = map (\(t, toks) -> (substituteClass subst t, toks)) gens
                    params' = map (\(t, n, toks) -> (substituteClass subst t, n, toks)) params
                in case stmt of
                    StaticMethodT {} ->
                        StaticMethodT ret' name gens' params' body'
                    InstanceMethodT {} ->
                        InstanceMethodT ret' name gens' params' body'
                    _ ->
                        FunctionT
                            ret'
                            name
                            gens'
                            params'
                            body'

        substituteCase :: Map String Class -> SwitchCase -> SwitchCase
        substituteCase subst sc = case sc of
            Case e mb tok -> Case (substituteExpr subst e) (fmap (substituteBlock subst) mb) tok
            Default b tok -> Default (substituteBlock subst b) tok


-- | Shared frontend normalization before semantic/type/IR phases:
--   1) hoist/promote top-level forms,
--   2) inline marked functions,
--   3) instantiate template calls into generated concrete functions.
normalizeProgramWithExternalTemplates :: [Statement] -> Program -> Program
normalizeProgramWithExternalTemplates externalTemplates =
    instantiateTemplateCallsProgramWithExternal externalTemplates . inlineProgramFunctions . promoteTopLevelFunctions


normalizeProgram :: Program -> Program
normalizeProgram = normalizeProgramWithExternalTemplates []

-- | Lowering-stage normalization:
--   semantic/template checks have already happened; keep template
--   blueprints so native backends can emit template metadata.
normalizeProgramForLowering :: Program -> Program
normalizeProgramForLowering (decls, stmts) =
    normalizeProgram (decls, stmts)


-- | Promote top-level declarations into static form.
--   - top-level instance functions -> static functions
--   - top-level fields/const-fields -> static vars/const-vars
--   Nested declarations in blocks are left unchanged.
promoteTopLevelFunctions :: Program -> Program
promoteTopLevelFunctions (decls, stmts) = (decls, map promote (concatMap hoistTopFunction stmts))
    where
        hoistTopFunction :: Statement -> [Statement]
        hoistTopFunction stmt = case functionNameVar stmt of
            Just (fnName, _) ->
                let (stmt', hoisted) = hoistFunctionBody [fnName] Nothing stmt
                in hoisted ++ [stmt']
            Nothing -> [stmt]

        promote :: Statement -> Statement
        promote (DefField names mTy rhs toks) = DefVar names mTy rhs toks
        promote (DefConstField names mTy rhs toks) = DefConstVar names mTy rhs toks
        promote (InstanceMethod ret name params body) = StaticMethod ret name params body
        promote (InstanceMethodT ret name gens params body) = StaticMethodT ret name gens params body
        promote (StmtGroup ss) = StmtGroup (map promote ss)
        promote stmt = stmt


-- | Hoist nested functions inside a function body into top-level sibling functions.
--   `path` is lexical function path, e.g. ["foo","inner1"].
--   `mSelfAlias` is used for recursive calls after renaming:
--   (old local name, capture params appended to this function).
hoistFunctionBody :: [String] -> Maybe (String, [ParamDecl]) -> Statement -> (Statement, [Statement])
hoistFunctionBody path mSelfAlias stmt =
    case functionParamsAndBody stmt of
        Nothing -> (stmt, [])
        Just (params, body) ->
            let selfCallMap = case mSelfAlias of
                    Nothing -> Map.empty
                    Just (oldName, captures) ->
                        case functionNameVar stmt of
                            Just (newName, fallbackTok) ->
                                let extra = map (paramToVarExpr fallbackTok) captures
                                in Map.singleton oldName (HoistCallSpec newName extra)
                            Nothing -> Map.empty
                (body', hoisted) = hoistBlockInFunction path params selfCallMap body
            in (setFunctionBody body' stmt, hoisted)


hoistBlockInFunction :: [String] -> [ParamDecl] -> Map String HoistCallSpec -> Block -> (Block, [Statement])
hoistBlockInFunction path outerParams baseCallMap (Multiple stmts0) =
    let stmts = flattenStmtGroups stmts0
        directFns = mapMaybe (prepareDirectNestedFunction path outerParams) stmts
        directCallMap = Map.fromList [(oldName, spec) | (oldName, spec, _) <- directFns]
        callMap = Map.union directCallMap baseCallMap
        hoistedFromDirect = concatMap (\(_, _, hs) -> hs) directFns
        step :: ([Statement], [[Statement]]) -> Statement -> ([Statement], [[Statement]])
        step (accStmts, accHoisted) stmt =
            if isHoistableFunctionDef stmt
                then (accStmts, accHoisted)
                else
                    let (stmt', hoisted) = hoistStmtInFunction path outerParams callMap stmt
                    in (stmt' : accStmts, hoisted : accHoisted)
        (stmtsKeptRev, hoistedRev) = foldl step ([], []) stmts
    in (Multiple (reverse stmtsKeptRev), hoistedFromDirect ++ concat (reverse hoistedRev))


prepareDirectNestedFunction :: [String] -> [ParamDecl] -> Statement -> Maybe (String, HoistCallSpec, [Statement])
prepareDirectNestedFunction path outerParams stmt = do
    (oldName, nameTok) <- functionNameVar stmt
    params <- functionParams stmt
    let innerParamNames = map (\(_, n, _) -> n) params
        captures = filter (\(_, n, _) -> n `notElem` innerParamNames) outerParams
        generatedName = intercalate "$" (path ++ [oldName])
        renamed = renameGeneratedFunction generatedName nameTok captures stmt
        (renamed', hoistedChildren) = hoistFunctionBody (path ++ [oldName]) (Just (oldName, captures)) renamed
        callSpec = HoistCallSpec generatedName (map (paramToVarExpr nameTok) captures)
        hoistedAll = hoistedChildren ++ [renamed']
    pure (oldName, callSpec, hoistedAll)


hoistStmtInFunction :: [String] -> [ParamDecl] -> Map String HoistCallSpec -> Statement -> (Statement, [Statement])
hoistStmtInFunction path outerParams callMap stmt = case stmt of
    Command (Return me) tok -> (Command (Return (fmap (rewriteExprCalls callMap) me)) tok, [])
    Command _ _ -> (stmt, [])
    Expr e -> (Expr (rewriteExprCalls callMap e), [])
    Exprs es -> (Exprs (map (rewriteExprCalls callMap) es), [])
    DefField names mTy mRhs toks -> (DefField names mTy (fmap (rewriteExprCalls callMap) mRhs) toks, [])
    DefConstField names mTy mRhs toks -> (DefConstField names mTy (fmap (rewriteExprCalls callMap) mRhs) toks, [])
    DefVar names mTy mRhs toks -> (DefVar names mTy (fmap (rewriteExprCalls callMap) mRhs) toks, [])
    DefConstVar names mTy mRhs toks -> (DefConstVar names mTy (fmap (rewriteExprCalls callMap) mRhs) toks, [])
    StmtGroup ss ->
        let (Multiple ss', hoisted) = hoistBlockInFunction path outerParams callMap (Multiple ss)
        in (StmtGroup ss', hoisted)
    BlockStmt b ->
        let (b', hoisted) = hoistBlockInFunction path outerParams callMap b
        in (BlockStmt b', hoisted)
    If e b1 b2 toks ->
        let (b1', h1) = hoistMaybeBlock path outerParams callMap b1
            (b2', h2) = hoistMaybeBlock path outerParams callMap b2
        in (If (rewriteExprCalls callMap e) b1' b2' toks, h1 ++ h2)
    For (s1, e2, s3) b1 b2 toks ->
        let (s1', h1) = hoistMaybeStmt path outerParams callMap s1
            (s3', h3) = hoistMaybeStmt path outerParams callMap s3
            (b1', hb1) = hoistMaybeBlock path outerParams callMap b1
            (b2', hb2) = hoistMaybeBlock path outerParams callMap b2
        in (For (s1', fmap (rewriteExprCalls callMap) e2, s3') b1' b2' toks, h1 ++ h3 ++ hb1 ++ hb2)
    Loop b tok ->
        let (b', h) = hoistMaybeBlock path outerParams callMap b
        in (Loop b' tok, h)
    Repeat e b1 b2 toks ->
        let (b1', h1) = hoistMaybeBlock path outerParams callMap b1
            (b2', h2) = hoistMaybeBlock path outerParams callMap b2
        in (Repeat (rewriteExprCalls callMap e) b1' b2' toks, h1 ++ h2)
    While e b1 b2 toks ->
        let (b1', h1) = hoistMaybeBlock path outerParams callMap b1
            (b2', h2) = hoistMaybeBlock path outerParams callMap b2
        in (While (rewriteExprCalls callMap e) b1' b2' toks, h1 ++ h2)
    Until e b1 b2 toks ->
        let (b1', h1) = hoistMaybeBlock path outerParams callMap b1
            (b2', h2) = hoistMaybeBlock path outerParams callMap b2
        in (Until (rewriteExprCalls callMap e) b1' b2' toks, h1 ++ h2)
    DoWhile b1 e b2 toks ->
        let (b1', h1) = hoistMaybeBlock path outerParams callMap b1
            (b2', h2) = hoistMaybeBlock path outerParams callMap b2
        in (DoWhile b1' (rewriteExprCalls callMap e) b2' toks, h1 ++ h2)
    DoUntil b1 e b2 toks ->
        let (b1', h1) = hoistMaybeBlock path outerParams callMap b1
            (b2', h2) = hoistMaybeBlock path outerParams callMap b2
        in (DoUntil b1' (rewriteExprCalls callMap e) b2' toks, h1 ++ h2)
    Switch e scs tok ->
        let (scs', hs) = hoistSwitchCases path outerParams callMap scs
        in (Switch (rewriteExprCalls callMap e) scs' tok, hs)
    _ -> (stmt, [])


hoistSwitchCases :: [String] -> [ParamDecl] -> Map String HoistCallSpec -> [SwitchCase] -> ([SwitchCase], [Statement])
hoistSwitchCases path outerParams callMap scs =
    let (scsRev, hsRev) = foldl step ([], []) scs
    in (reverse scsRev, concat (reverse hsRev))
    where
        step :: ([SwitchCase], [[Statement]]) -> SwitchCase -> ([SwitchCase], [[Statement]])
        step (accScs, accHs) sc = case sc of
            Case e mb tok ->
                let (mb', h) = hoistMaybeBlock path outerParams callMap mb
                in (Case (rewriteExprCalls callMap e) mb' tok : accScs, h : accHs)
            Default b tok ->
                let (b', h) = hoistBlockInFunction path outerParams callMap b
                in (Default b' tok : accScs, h : accHs)


hoistMaybeBlock :: [String] -> [ParamDecl] -> Map String HoistCallSpec -> Maybe Block -> (Maybe Block, [Statement])
hoistMaybeBlock _ _ _ Nothing = (Nothing, [])
hoistMaybeBlock path outerParams callMap (Just b) =
    let (b', h) = hoistBlockInFunction path outerParams callMap b
    in (Just b', h)


hoistMaybeStmt :: [String] -> [ParamDecl] -> Map String HoistCallSpec -> Maybe Statement -> (Maybe Statement, [Statement])
hoistMaybeStmt _ _ _ Nothing = (Nothing, [])
hoistMaybeStmt path outerParams callMap (Just st) =
    let (st', h) = hoistStmtInFunction path outerParams callMap st
    in (Just st', h)


rewriteExprCalls :: Map String HoistCallSpec -> Expression -> Expression
rewriteExprCalls callMap expr = case expr of
    Error _ _ -> expr
    IntConst _ _ -> expr
    LongConst _ _ -> expr
    FloatConst _ _ -> expr
    DoubleConst _ _ -> expr
    LongDoubleConst _ _ -> expr
    CharConst _ _ -> expr
    StringConst _ _ -> expr
    BoolConst _ _ -> expr
    Variable _ _ -> expr
    Qualified _ _ -> expr
    Cast ty e tok -> Cast ty (rewriteExprCalls callMap e) tok
    Unary op e tok -> Unary op (rewriteExprCalls callMap e) tok
    Binary op e1 e2 tok -> Binary op (rewriteExprCalls callMap e1) (rewriteExprCalls callMap e2) tok
    IfExpr cond thenE elseE toks ->
        IfExpr
            (rewriteExprCalls callMap cond)
            (rewriteExprCalls callMap thenE)
            (rewriteExprCalls callMap elseE)
            toks
    SizeOfExpr inner tok ->
        SizeOfExpr (rewriteExprCalls callMap inner) tok
    SizeOfType _ _ ->
        expr
    BlockExpr b ->
        BlockExpr (rewriteBlockExprCalls callMap b)
    Call callee args ->
        let callee' = rewriteExprCalls callMap callee
            args' = map (rewriteExprCalls callMap) args
        in case callee' of
            Variable name tok -> case Map.lookup name callMap of
                Just spec ->
                    let newName = hoistCalleeName spec
                        extra = map (rewriteExprCalls callMap) (hoistExtraArgs spec)
                    in Call (Variable newName (renameIdentToken newName tok)) (args' ++ extra)
                Nothing -> Call callee' args'
            _ -> Call callee' args'
    CallT callee tys args ->
        let callee' = rewriteExprCalls callMap callee
            args' = map (rewriteExprCalls callMap) args
        in case callee' of
            Variable name tok -> case Map.lookup name callMap of
                Just spec ->
                    let newName = hoistCalleeName spec
                        extra = map (rewriteExprCalls callMap) (hoistExtraArgs spec)
                    in CallT (Variable newName (renameIdentToken newName tok)) tys (args' ++ extra)
                Nothing -> CallT callee' tys args'
            _ -> CallT callee' tys args'


rewriteBlockExprCalls :: Map String HoistCallSpec -> Block -> Block
rewriteBlockExprCalls callMap (Multiple ss) = Multiple (map (rewriteStmtExprCalls callMap) ss)


rewriteStmtExprCalls :: Map String HoistCallSpec -> Statement -> Statement
rewriteStmtExprCalls callMap stmt = case stmt of
    Command (Return mExpr) tok -> Command (Return (fmap (rewriteExprCalls callMap) mExpr)) tok
    Expr e -> Expr (rewriteExprCalls callMap e)
    Exprs es -> Exprs (map (rewriteExprCalls callMap) es)
    DefField names mTy mRhs toks -> DefField names mTy (fmap (rewriteExprCalls callMap) mRhs) toks
    DefConstField names mTy mRhs toks -> DefConstField names mTy (fmap (rewriteExprCalls callMap) mRhs) toks
    DefVar names mTy mRhs toks -> DefVar names mTy (fmap (rewriteExprCalls callMap) mRhs) toks
    DefConstVar names mTy mRhs toks -> DefConstVar names mTy (fmap (rewriteExprCalls callMap) mRhs) toks
    StmtGroup ss -> StmtGroup (map (rewriteStmtExprCalls callMap) ss)
    BlockStmt b -> BlockStmt (rewriteBlockExprCalls callMap b)
    If e b1 b2 toks ->
        If
            (rewriteExprCalls callMap e)
            (fmap (rewriteBlockExprCalls callMap) b1)
            (fmap (rewriteBlockExprCalls callMap) b2)
            toks
    For (s1, e2, s3) b1 b2 toks ->
        For
            (fmap (rewriteStmtExprCalls callMap) s1, fmap (rewriteExprCalls callMap) e2, fmap (rewriteStmtExprCalls callMap) s3)
            (fmap (rewriteBlockExprCalls callMap) b1)
            (fmap (rewriteBlockExprCalls callMap) b2)
            toks
    Loop b tok -> Loop (fmap (rewriteBlockExprCalls callMap) b) tok
    Repeat e b1 b2 toks ->
        Repeat
            (rewriteExprCalls callMap e)
            (fmap (rewriteBlockExprCalls callMap) b1)
            (fmap (rewriteBlockExprCalls callMap) b2)
            toks
    While e b1 b2 toks ->
        While
            (rewriteExprCalls callMap e)
            (fmap (rewriteBlockExprCalls callMap) b1)
            (fmap (rewriteBlockExprCalls callMap) b2)
            toks
    Until e b1 b2 toks ->
        Until
            (rewriteExprCalls callMap e)
            (fmap (rewriteBlockExprCalls callMap) b1)
            (fmap (rewriteBlockExprCalls callMap) b2)
            toks
    DoWhile b1 e b2 toks ->
        DoWhile
            (fmap (rewriteBlockExprCalls callMap) b1)
            (rewriteExprCalls callMap e)
            (fmap (rewriteBlockExprCalls callMap) b2)
            toks
    DoUntil b1 e b2 toks ->
        DoUntil
            (fmap (rewriteBlockExprCalls callMap) b1)
            (rewriteExprCalls callMap e)
            (fmap (rewriteBlockExprCalls callMap) b2)
            toks
    Switch e scs tok ->
        Switch (rewriteExprCalls callMap e) (map (rewriteSwitchExprCalls callMap) scs) tok
    _ -> stmt


rewriteSwitchExprCalls :: Map String HoistCallSpec -> SwitchCase -> SwitchCase
rewriteSwitchExprCalls callMap sc = case sc of
    Case e mb tok -> Case (rewriteExprCalls callMap e) (fmap (rewriteBlockExprCalls callMap) mb) tok
    Default b tok -> Default (rewriteBlockExprCalls callMap b) tok


isHoistableFunctionDef :: Statement -> Bool
isHoistableFunctionDef = isJust . functionNameVar


functionNameVar :: Statement -> Maybe (String, Token)
functionNameVar stmt = case stmt of
    InstanceMethod _ (Variable name tok) _ _ -> Just (name, tok)
    StaticMethod _ (Variable name tok) _ _ -> Just (name, tok)
    NativeMethod _ (Variable name tok) _ _ -> Just (name, tok)
    InstanceMethodT _ (Variable name tok) _ _ _ -> Just (name, tok)
    StaticMethodT _ (Variable name tok) _ _ _ -> Just (name, tok)
    _ -> Nothing


functionParams :: Statement -> Maybe [ParamDecl]
functionParams stmt = case stmt of
    InstanceMethod _ _ params _ -> Just params
    StaticMethod _ _ params _ -> Just params
    NativeMethod _ _ params _ -> Just params
    InstanceMethodT _ _ _ params _ -> Just params
    StaticMethodT _ _ _ params _ -> Just params
    _ -> Nothing


functionParamsAndBody :: Statement -> Maybe ([ParamDecl], Block)
functionParamsAndBody stmt = case stmt of
    InstanceMethod _ _ params body -> Just (params, body)
    StaticMethod _ _ params body -> Just (params, body)
    InstanceMethodT _ _ _ params body -> Just (params, body)
    StaticMethodT _ _ _ params body -> Just (params, body)
    _ -> Nothing


setFunctionBody :: Block -> Statement -> Statement
setFunctionBody body stmt = case stmt of
    InstanceMethod ret name params _ -> InstanceMethod ret name params body
    StaticMethod ret name params _ -> StaticMethod ret name params body
    InstanceMethodT ret name gens params _ -> InstanceMethodT ret name gens params body
    StaticMethodT ret name gens params _ -> StaticMethodT ret name gens params body
    _ -> stmt


renameGeneratedFunction :: String -> Token -> [ParamDecl] -> Statement -> Statement
renameGeneratedFunction newName fallbackTok captures stmt = case stmt of
    InstanceMethod (retC, retToks) (Variable _ nameTok) params body ->
        InstanceMethod
            (retC, generatedRetTokens nameTok retToks)
            (Variable newName (renameIdentToken newName nameTok))
            (params ++ captures)
            body
    StaticMethod (retC, retToks) (Variable _ nameTok) params body ->
        StaticMethod
            (retC, generatedRetTokens nameTok retToks)
            (Variable newName (renameIdentToken newName nameTok))
            (params ++ captures)
            body
    InstanceMethodT (retC, retToks) (Variable _ nameTok) gens params body ->
        InstanceMethodT
            (retC, generatedRetTokens nameTok retToks)
            (Variable newName (renameIdentToken newName nameTok))
            gens
            (params ++ captures)
            body
    StaticMethodT (retC, retToks) (Variable _ nameTok) gens params body ->
        StaticMethodT
            (retC, generatedRetTokens nameTok retToks)
            (Variable newName (renameIdentToken newName nameTok))
            gens
            (params ++ captures)
            body
    _ ->
        case stmt of
            InstanceMethod (retC, retToks) name params body ->
                InstanceMethod (retC, generatedRetTokens fallbackTok retToks) name (params ++ captures) body
            StaticMethod (retC, retToks) name params body ->
                StaticMethod (retC, generatedRetTokens fallbackTok retToks) name (params ++ captures) body
            InstanceMethodT (retC, retToks) name gens params body ->
                InstanceMethodT (retC, generatedRetTokens fallbackTok retToks) name gens (params ++ captures) body
            StaticMethodT (retC, retToks) name gens params body ->
                StaticMethodT (retC, generatedRetTokens fallbackTok retToks) name gens (params ++ captures) body
            _ -> stmt


generatedRetTokens :: Token -> [Token] -> [Token]
generatedRetTokens nameTok toks =
    Lex.Ident "private" (tokenPos nameTok) : dropWhile isFunctionModifierToken toks


isFunctionModifierToken :: Token -> Bool
isFunctionModifierToken (Lex.Ident s _) =
    let k = map toLower s
    in k == "public" || k == "private" || k == "protected" || k == "static" || k == "final" || k == "const"
isFunctionModifierToken _ = False


renameIdentToken :: String -> Token -> Token
renameIdentToken newName tok = case tok of
    Lex.Ident _ p -> Lex.Ident newName p
    _ -> Lex.Ident newName (tokenPos tok)


paramToVarExpr :: Token -> ParamDecl -> Expression
paramToVarExpr fallbackTok (_, name, toks) = case toks of
    (t:_) -> Variable name (renameIdentToken name t)
    [] -> Variable name (renameIdentToken name fallbackTok)


flattenStmtGroups :: [Statement] -> [Statement]
flattenStmtGroups = concatMap go
    where
        go :: Statement -> [Statement]
        go (StmtGroup ss) = flattenStmtGroups ss
        go st = [st]


-- | Get the declared package path from a program header.
--   Returns [] when no package declaration exists.
--   If there are multiple package declarations, this returns the first one.
--   Duplicate-package errors are handled in semantic checking.
getPackage :: Program -> [String]
getPackage (decls, _) = let package = filter isPackageDecl decls in maybe [] declPath (listToMaybe package)


-- | Get the optional declared classname from a program header.
--   Returns the first declaration if multiple exist; duplicate checking
--   belongs to semantic validation.
getJavaName :: Program -> Maybe String
getJavaName (decls, _) = listToMaybe [name | JavaName name _ <- decls]


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


-- | Normalize a parsed class into canonical forms.
--   Converts built-in names (e.g. "int") into primitive Class constructors,
--   and recursively normalizes generic elements.
normalizeClass :: Class -> Class
normalizeClass cls = case cls of
    Class [name] args ->
        case (normalizeBuiltinClass name, map normalizeClass args) of
            (Just prim, []) -> prim
            (Nothing, [Class ["*"] []]) | map toLower name == "pointer" ->
                Pointer Void
            (Nothing, [one]) | map toLower name == "pointer" ->
                Pointer one
            _ -> Class [name] (map normalizeClass args)
    Class names args -> Class names (map normalizeClass args)
    Pointer c -> Pointer (normalizeClass c)
    FuncPtr ret args -> FuncPtr (normalizeClass ret) (map normalizeClass args)
    Blob n -> Blob n
    other -> other


normalizeBuiltinClass :: String -> Maybe Class
normalizeBuiltinClass name = Map.lookup name builtinClassMap


builtinClassMap :: Map String Class
builtinClassMap = Map.fromList [
    ("bool", Bool),
    ("byte", Int8T), ("int8", Int8T), ("i8", Int8T),
    ("short", Int16T), ("int16", Int16T), ("i16", Int16T),
    ("int", Int32T), ("int32", Int32T), ("i32", Int32T),
    ("long", Int64T), ("int64", Int64T), ("i64", Int64T),
    ("float", Float32T), ("float32", Float32T), ("f32", Float32T),
    ("double", Float64T), ("float64", Float64T), ("f64", Float64T),
    ("float128", Float128T),
    ("char", Char),
    ("String", Class ["String"] []),
    ("void", Void)]


-- | Extract all error expressions from a program.
--   Traverses the entire syntax tree and filters expressions marked as errors.
getErrorProgram :: Program -> [Expression]
getErrorProgram = filter isErrExpr . flattenProgram


-- | Collect all imported classes from a program.
--   Returns a map from package paths to sets of class names.
collectInputProgram :: Program -> Map [String] (HashSet String)
collectInputProgram (decls, _) = let imports = filter isImportDecl decls in
        foldr (insert . declPath) Map.empty imports
    where
        insert :: [String] -> Map [String]  (HashSet String) -> Map [String] (HashSet String)
        insert ss imap = let (package, className) = splitLast ss in case Map.lookup package imap of
            Nothing -> Map.insert package (HashSet.singleton className) imap
            Just classSet -> (if HashSet.member "*" classSet then
                    imap
                else
                    Map.insert package (HashSet.insert className classSet) imap)

-- | Collect all imported classes from a list of programs.
collectInputPrograms :: [Program] -> Map [String] (HashSet String)
collectInputPrograms = foldr (Map.unionWith HashSet.union . collectInputProgram) Map.empty


-- Check a expresson is a variable or not
isVariable :: Expression -> Bool
isVariable (Variable _ _)  = True
isVariable (Qualified _ _) = True
isVariable _ = False


-- Check a expression is an atom or not
isAtom :: Expression -> Bool
isAtom (IntConst _ _) = True
isAtom (LongConst _ _) = True
isAtom (FloatConst _ _) = True
isAtom (DoubleConst _ _) = True
isAtom (LongDoubleConst _ _) = True
isAtom (CharConst _ _) = True
isAtom (StringConst _ _) = True
isAtom (BoolConst _ _) = True
isAtom (Variable _ _) = True
isAtom (Qualified _ _) = True
isAtom _ = False


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


nativeBodyVal :: Token -> String
nativeBodyVal (Lex.NativeBody s _) = s
nativeBodyVal _ = error "nativeBodyVal: expected NativeBody token"


annotationName :: Token -> String
annotationName (Lex.Annotation name _ _) = name
annotationName _ = error "annotationName: expected Annotation token"


annotationArgs :: Token -> [Token]
annotationArgs (Lex.Annotation _ args _) = args
annotationArgs _ = error "annotationArgs: expected Annotation token"


nativeAnnotationTokens :: Token -> Maybe [Token]
nativeAnnotationTokens tok = case tok of
    Lex.Annotation name args _
        | map toLower name /= "native" -> Nothing
        | null args -> Just [tok]
        | otherwise ->
            case args of
                [Lex.StrConst _ _] -> Just [tok]
                _ -> Nothing
    _ -> Nothing


nativeAnnotationTarget :: Token -> Maybe ([Token], String)
nativeAnnotationTarget tok = case tok of
    Lex.Annotation name args _
        | map toLower name /= "native" -> Nothing
        | otherwise -> case args of
            [Lex.StrConst s _] -> Just ([tok], s)
            _ -> Nothing
    _ -> Nothing


data CNativeFun = CNativeFun {
    cNativeFunName :: String,
    cNativeFunParamTypes :: [Class],
    cNativeFunCode :: String
}
    deriving (Eq, Show)


cNativeFunFromStmt :: Statement -> Maybe CNativeFun
cNativeFunFromStmt stmt = case stmt of
    NativeMethod _ nameExpr params codeS ->
        mkNative nameExpr params codeS
    _ -> Nothing
    where
        mkNative :: Expression -> [(Class, String, [Token])] -> String -> Maybe CNativeFun
        mkNative nameExpr params codeS = do
            name <- nativeFunShortName nameExpr
            pure $ CNativeFun name (map fst3 params) codeS

        nativeFunShortName :: Expression -> Maybe String
        nativeFunShortName expr = case expr of
            Variable name _ -> Just name
            Qualified qn _ -> case reverse qn of
                (x:_) -> Just x
                [] -> Nothing
            _ -> Nothing

        fst3 :: (a, b, c) -> a
        fst3 (a, _, _) = a


getReqire :: CNativeFun -> [String]
getReqire fun =
    let codeS = cNativeFunCode fun
        fCalls = extractNativeCallStrings "@xfun(\"" codeS
        sReads = extractNativeCallStrings "@xstatic(\"" codeS
    in fCalls ++ sReads


getRequire :: CNativeFun -> [String]
getRequire = getReqire


cNativeFunKey :: CNativeFun -> String
cNativeFunKey fun =
    cNativeKeyFromSig (cNativeFunName fun) (cNativeFunParamTypes fun)


cNativeKeyFromSig :: String -> [Class] -> String
cNativeKeyFromSig name params = name ++ "#" ++ concatMap classMangle params


cNativeRequiredFunSigs :: CNativeFun -> [(String, [Class])]
cNativeRequiredFunSigs fun =
    mapMaybe parseXfunSig (extractNativeCallStrings "@xfun(\"" (cNativeFunCode fun))


cNativeRequiredFunKeys :: CNativeFun -> [String]
cNativeRequiredFunKeys fun =
    map (uncurry cNativeKeyFromSig) (cNativeRequiredFunSigs fun)


cNativeRequiredStatics :: CNativeFun -> [String]
cNativeRequiredStatics fun =
    mapMaybe parseXstaticRef (extractNativeCallStrings "@xstatic(\"" (cNativeFunCode fun))


collectCNativeFuns :: [Statement] -> [CNativeFun]
collectCNativeFuns = foldr go []
    where
        go :: Statement -> [CNativeFun] -> [CNativeFun]
        go (StmtGroup ss) acc = collectCNativeFuns ss ++ acc
        go st acc = case cNativeFunFromStmt st of
            Just f -> f : acc
            Nothing -> acc


cNativeDepGraph :: [CNativeFun] -> Map String [String]
cNativeDepGraph funs =
    let keys = map cNativeFunKey funs
        keySet = HashSet.fromList keys
    in Map.fromList [
        (k, filter (`HashSet.member` keySet) (dedup (cNativeRequiredFunKeys fun))) | fun <- funs,
        let k = cNativeFunKey fun]
    where
        dedup :: [String] -> [String]
        dedup = HashSet.toList . HashSet.fromList


-- | Topological sort on native C function dependency graph.
--   Right: ordered keys; Left: keys that belong to a cycle.
cNativeTopoSort :: [CNativeFun] -> Either [String] [String]
cNativeTopoSort funs =
    let keysInOrder = map cNativeFunKey funs
        depMap = cNativeDepGraph funs
        indeg0 = Map.fromList [(k, 0 :: Int) | k <- keysInOrder]
        indeg1 = foldl bump indeg0 (Map.toList depMap)
        outgoing = foldl buildOutgoing Map.empty (Map.toList depMap)
        queue0 = [k | k <- keysInOrder, Map.findWithDefault 0 k indeg1 == 0]
        (orderedRev, indegFinal) = kahn queue0 indeg1 outgoing []
        ordered = reverse orderedRev
    in if length ordered == length keysInOrder
        then Right ordered
        else
            let cyc = [k | k <- keysInOrder, Map.findWithDefault 0 k indegFinal > 0]
            in Left cyc
    where
        bump :: Map String Int -> (String, [String]) -> Map String Int
        bump indeg (k, deps) = Map.insert k (length deps) indeg

        buildOutgoing :: Map String [String] -> (String, [String]) -> Map String [String]
        buildOutgoing out (k, deps) = foldl (addOne k) out deps

        addOne :: String -> Map String [String] -> String -> Map String [String]
        addOne toKey out depKey = Map.insertWith (++) depKey [toKey] out

        kahn ::
            [String] ->
            Map String Int ->
            Map String [String] ->
            [String] ->
            ([String], Map String Int)
        kahn [] indeg _ acc = (acc, indeg)
        kahn (k:ks) indeg out acc =
            let nexts = Map.findWithDefault [] k out
                (indeg', newZeros) = decMany indeg nexts []
                queue' = ks ++ newZeros
            in kahn queue' indeg' out (k : acc)

        decMany :: Map String Int -> [String] -> [String] -> (Map String Int, [String])
        decMany indeg [] zs = (indeg, zs)
        decMany indeg (n:ns) zs =
            let oldN = Map.findWithDefault 0 n indeg
                newN = oldN - 1
                indeg' = Map.insert n newN indeg
                zs' = if newN == 0 then zs ++ [n] else zs
            in decMany indeg' ns zs'


extractNativeCallStrings :: String -> String -> [String]
extractNativeCallStrings marker = go
    where
        go :: String -> [String]
        go [] = []
        go s = case stripPrefix marker s of
            Just rest ->
                let (payload, after) = takeQuoted rest
                in payload : go after
            Nothing -> case s of
                (_:xs) -> go xs

        takeQuoted :: String -> (String, String)
        takeQuoted = loop []
            where
                loop acc [] = (reverse acc, [])
                loop acc ('\\':c:xs) = loop (c:'\\':acc) xs
                loop acc ('"':xs) = (reverse acc, xs)
                loop acc (c:xs) = loop (c:acc) xs


parseXfunSig :: String -> Maybe (String, [Class])
parseXfunSig raw =
    let s = trimNativeSpaces raw
        (namePart0, rest0) = break (== '(') s
        namePart = trimNativeSpaces namePart0
    in case rest0 of
        [] -> Nothing
        (_:xs) ->
            let (argsPart, _) = break (== ')') xs
                argTypes = mapMaybe parseNativeTypeName (splitNativeArgs argsPart)
                arityRaw = splitNativeArgs argsPart
            in if length argTypes /= length arityRaw
                then Nothing
                else
                    let shortName = nativeLastQNameSegment namePart
                    in if null shortName then Nothing else Just (shortName, argTypes)


parseXstaticRef :: String -> Maybe String
parseXstaticRef raw =
    let leftPart = case breakArrow raw of
            (a, _) -> a
        qn = trimNativeSpaces leftPart
    in if null qn then Nothing else Just qn
    where
        breakArrow :: String -> (String, String)
        breakArrow s = fromMaybe (s, "") (breakAt "->" s)


splitNativeArgs :: String -> [String]
splitNativeArgs s =
    let parts = go 0 0 0 "" s
        trimmed = map trimNativeSpaces parts
    in filter (not . null) trimmed
    where
        go :: Int -> Int -> Int -> String -> String -> [String]
        go _ _ _ cur [] = [reverse cur]
        go dA dP dB cur (ch:rest) = case ch of
            '<' -> go (dA + 1) dP dB (ch:cur) rest
            '>' -> go (max 0 (dA - 1)) dP dB (ch:cur) rest
            '(' -> go dA (dP + 1) dB (ch:cur) rest
            ')' -> go dA (max 0 (dP - 1)) dB (ch:cur) rest
            '[' -> go dA dP (dB + 1) (ch:cur) rest
            ']' -> go dA dP (max 0 (dB - 1)) (ch:cur) rest
            ',' | dA == 0 && dP == 0 && dB == 0 ->
                reverse cur : go dA dP dB "" rest
            _ -> go dA dP dB (ch:cur) rest


parseNativeTypeName :: String -> Maybe Class
parseNativeTypeName raw =
    let t = map toLower (trimNativeSpaces raw)
    in case t of
        "bool" -> Just Bool
        "char" -> Just Char
        "byte" -> Just Int8T
        "short" -> Just Int16T
        "int" -> Just Int32T
        "long" -> Just Int64T
        "float" -> Just Float32T
        "double" -> Just Float64T
        "float128" -> Just Float128T
        "void" -> Just Void
        _ ->
            let qn = splitNativeClassQName (trimNativeSpaces raw)
            in if null qn then Nothing else Just (Class qn [])


splitNativeClassQName :: String -> [String]
splitNativeClassQName s =
    let normalized = replaceScopeSep s
        parts = splitDot normalized
        cleaned = filter (not . null) (map trimNativeSpaces parts)
    in cleaned
    where
        splitDot :: String -> [String]
        splitDot txt = case break (== '.') txt of
            (a, []) -> [a]
            (a, _:rest) -> a : splitDot rest


nativeLastQNameSegment :: String -> String
nativeLastQNameSegment raw =
    let qn = splitNativeClassQName raw
    in case reverse qn of
        (x:_) -> x
        [] -> ""


replaceScopeSep :: String -> String
replaceScopeSep [] = []
replaceScopeSep (':':':':xs) = '.':replaceScopeSep xs
replaceScopeSep (x:xs) = x : replaceScopeSep xs


trimNativeSpaces :: String -> String
trimNativeSpaces = dropWhile isSpace . reverse . dropWhile isSpace . reverse


breakAt :: String -> String -> Maybe (String, String)
breakAt pat = go ""
    where
        go :: String -> String -> Maybe (String, String)
        go acc rest =
            if pat `isPrefixOf` rest
                then Just (reverse acc, drop (length pat) rest)
                else case rest of
                    [] -> Nothing
                    (c:cs) -> go (c:acc) cs


jvmClassAnnotation :: Token -> Maybe (String, Token)
jvmClassAnnotation tok = case tok of
    Lex.Annotation name [strTok@(Lex.StrConst value _)] _
        | let low = map toLower name
        , low `elem` ["jvmclass", "class", "filename"] -> Just (value, strTok)
        | otherwise -> Nothing
    _ -> Nothing

