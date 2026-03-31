{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Parse.SyntaxTree where

import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Maybe (listToMaybe, fromMaybe, isNothing, mapMaybe)
import Data.Char (toLower)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable(..))
import Lex.Token (Token, tokenPos)
import Util.Basic (insertTab, splitLast)

import qualified Data.Map.Strict as Map
import qualified Data.HashSet as HashSet
import qualified Lex.Token as Lex


-- | Language-level type representation.
--   Includes primitive types, arrays, and user-defined classes.
data Class =
    Int8T | Int16T | Int32T | Int64T |
    Float32T | Float64T | Float128T |
    Bool | Char | Void |
    Array Class Int |
    Class [String] [Class] | -- name + general
    ErrorClass
    deriving (Eq, Ord, Show)


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
prettyClass (Array c _) = concat ["Array<", prettyClass c, ">"]
prettyClass (Class ss args) =
    let base = '_' : intercalate "." ss
    in case args of
        [] -> base
        _  -> concat [base, "<", intercalate ", " (map prettyClass args), ">"]

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

    (BitImply, "imp"), (BitNimply, "nimp"),
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
    | Ternary Expression (Expression, Expression) [Token]
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
        prettyExpr' (Just (Ternary cond (tExpr, fExpr) _)) =
            concat [
                prettyExpr' (Just tExpr), " if ", prettyExpr' (Just cond),
                " else ", prettyExpr' (Just fExpr)]


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
exprTokens (Ternary c (e1, e2) ts) = ts ++ exprTokens c ++ exprTokens e1 ++ exprTokens e2


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
flattenExpr (Just fatherE@(Ternary c (e1, e2) _)) =
    concat [[fatherE], flattenExpr (Just c), flattenExpr (Just e1), flattenExpr (Just e2)]
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

    InstanceMethod (Class, [Token]) Expression [(Class, String, [Token])] Block |
    StaticMethod (Class, [Token]) Expression [(Class, String, [Token])] Block |


    -- function: return_type + pos, name, params + position, body
    InstanceMethodT (Class, [Token]) Expression [(Class, [Token])] [(Class, String, [Token])] Block |
    StaticMethodT (Class, [Token]) Expression [(Class, [Token])] [(Class, String, [Token])] Block
    -- template function: return_type + pos, name, template params + position, params + position, body
    deriving (Eq, Show)

{-# COMPLETE
    Command, DefField, DefConstField, DefVar, DefConstVar, Expr, Exprs, StmtGroup, BlockStmt,
    If, For, Loop, Repeat, While, Until, DoWhile, DoUntil, Switch, Function, FunctionT #-}

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
prettyDeclaration (JavaName s _) = "javaname " ++ show s


-- | Check whether a statement is a class declaration.
--   Placeholder: class declarations are not modeled yet, so this is always False.
--   Add cases here when class statements are introduced.
isClassDeclar :: Statement -> Bool
isClassDeclar _ = False


-- | Check whether a statement is a function declaration.
isFunction :: Statement -> Bool
isFunction (Function {}) = True
isFunction _ = False


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


-- | Check whether a declaration is a javaname declaration.
isJavaNameDecl :: Declaration -> Bool
isJavaNameDecl (JavaName _ _) = True
isJavaNameDecl _ = False


-- | Program representation.
--   Consists of imported modules and a list of top-level statements.
type Program = ([Declaration], [Statement])


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
                        (\spec -> HashSet.filter (`HashSet.member` inlineNames) (exprCallees (inlineBodyExpr spec)))
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
                                            if n == root
                                                then True
                                                else
                                                    if HashSet.member n visited
                                                        then False
                                                        else dfsFrom root (HashSet.insert n visited) n
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
            Ternary c (tExpr, fExpr) _ ->
                HashSet.unions [exprCallees c, exprCallees tExpr, exprCallees fExpr]
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
            Ternary c (tExpr, fExpr) toks ->
                Ternary
                    (rewriteExprInline specs stack c)
                    (rewriteExprInline specs stack tExpr, rewriteExprInline specs stack fExpr)
                    toks
            Call callee args ->
                inlineCall specs stack (Call callee args)
            CallT callee tys args ->
                inlineCallT specs stack (CallT callee tys args)

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
            Ternary c (tExpr, fExpr) toks ->
                Ternary (substInlineExpr subst c) (substInlineExpr subst tExpr, substInlineExpr subst fExpr) toks
            Call callee args -> Call (substInlineExpr subst callee) (map (substInlineExpr subst) args)
            CallT callee tys args -> CallT (substInlineExpr subst callee) tys (map (substInlineExpr subst) args)


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
    in (reverse scsRev, reverse hsRev >>= id)
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
    Ternary c (tExpr, fExpr) toks ->
        Ternary (rewriteExprCalls callMap c) (rewriteExprCalls callMap tExpr, rewriteExprCalls callMap fExpr) toks
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


isHoistableFunctionDef :: Statement -> Bool
isHoistableFunctionDef = maybe False (const True) . functionNameVar


functionNameVar :: Statement -> Maybe (String, Token)
functionNameVar stmt = case stmt of
    InstanceMethod _ (Variable name tok) _ _ -> Just (name, tok)
    StaticMethod _ (Variable name tok) _ _ -> Just (name, tok)
    InstanceMethodT _ (Variable name tok) _ _ _ -> Just (name, tok)
    StaticMethodT _ (Variable name tok) _ _ _ -> Just (name, tok)
    _ -> Nothing


functionParams :: Statement -> Maybe [ParamDecl]
functionParams stmt = case stmt of
    InstanceMethod _ _ params _ -> Just params
    StaticMethod _ _ params _ -> Just params
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


-- | Get the optional declared javaname from a program header.
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
--   and recursively normalizes array/generic elements.
normalizeClass :: Class -> Class
normalizeClass cls = case cls of
    Array c n -> Array (normalizeClass c) n
    Class [name] args ->
        case (normalizeBuiltinClass name, args) of
            (Just prim, []) -> prim
            _ -> Class [name] (map normalizeClass args)
    Class names args -> Class names (map normalizeClass args)
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
    ("String", Class ["java", "lang", "String"] []),
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

