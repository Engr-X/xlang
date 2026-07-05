module Parse.ParserBasic where

import Data.Maybe (listToMaybe, fromMaybe, isNothing, mapMaybe)
import Data.List (find, sort, foldl', dropWhileEnd)
import Data.Char (toLower)
import Numeric (readHex)
import Lex.Token (Token, isLBracketToken, isRBracketToken, tokenPos)
import Parse.SyntaxTree
import Util.Basic (isInt, isLong, isFloat, isDouble, isLongDouble, excelColumnNames)
import Util.Exception (ErrorKind, expectedExpression, assignErrorMsg)
import Util.Type (Path, makePosition)

import qualified Lex.Token as Lex
import qualified Util.Exception as UE


type AccessOp = Maybe AccessModified


accessOpToAccess :: AccessOp -> AccessModified
accessOpToAccess = fromMaybe Public


prettyAccess :: AccessModified -> String
prettyAccess Private = "private"
prettyAccess Protected = "protected"
prettyAccess Public = "public"


prettyDeclFlag :: DeclFlag -> String
prettyDeclFlag Static = "static"
prettyDeclFlag Final = "final"
prettyDeclFlag Inline = "inline"


-- | Render a list of declaration flags as a single string.
prettyDeclFlags :: DeclFlags -> String
prettyDeclFlags = unwords . map prettyDeclFlag . sort

-- | Render a declaration as "access [flags]".
prettyDecl :: Decl -> String
prettyDecl (acc, flags) =
    let accS = prettyAccess acc
        flagsS = prettyDeclFlags flags
        parts = filter (not . null) [accS, flagsS]
    in unwords parts


methodDeclFromTokens :: [Token] -> Decl
methodDeclFromTokens toks = (access, flags)
  where
    access = fromMaybe Public (listToMaybe [a | Lex.Ident s _ <- toks, Just a <- [toAccessMaybe s]])
    flags = uniq [f | Lex.Ident s _ <- toks, f <- toFlag s]

    toAccessMaybe :: String -> Maybe AccessModified
    toAccessMaybe s = case map toLower s of
        "private" -> Just Private
        "protected" -> Just Protected
        "public" -> Just Public
        _ -> Nothing

    toFlag :: String -> [DeclFlag]
    toFlag s = case map toLower s of
        "static" -> [Static]
        "inline" -> [Inline]
        "final" -> [Final]
        _ -> []

    uniq :: Ord a => [a] -> [a]
    uniq = foldl' (\acc x -> if x `elem` acc then acc else acc ++ [x]) []


methodDeclsFromTokens :: [Token] -> [Decl]
methodDeclsFromTokens toks = [methodDeclFromTokens toks]


methodAnnotationsFromTokens :: [Token] -> [Annotation]
methodAnnotationsFromTokens = mapMaybe tokenToAnnotation
  where
    tokenToAnnotation :: Token -> Maybe Annotation
    tokenToAnnotation tok@(Lex.Annotation name rawArgs _) =
        Just (name, tok, parseAnnotationArgExprs rawArgs)
    tokenToAnnotation _ = Nothing


nativeSpecifierAnnotation :: ([Token], String) -> Annotation
nativeSpecifierAnnotation (specToks, target) =
    ("native", annTok, [StringConst target strTok])
  where
    annTok = case specToks of
        [] -> Lex.dummyToken
        (t:_) -> t
    strTok = case reverse specToks of
        (Lex.StrConst _ tPos : _) -> Lex.StrConst target tPos
        _ -> annTok


parseAnnotationArgExprs :: [Token] -> [Expression]
parseAnnotationArgExprs rawArgs
    | null cleaned = []
    | otherwise = map parseOne (splitTopLevelCommas cleaned)
  where
    cleaned = filter (not . isTokenPass) rawArgs

    parseOne :: [Token] -> Expression
    parseOne seg =
        let ts = trimTokenPass seg
        in case parseLiteralExpr ts of
            Just e -> e
            Nothing -> Error ts "invalid annotation argument: literal expected"

    isTokenPass :: Token -> Bool
    isTokenPass tok = case tok of
        Lex.TokenPass _ -> True
        _ -> False

    trimTokenPass :: [Token] -> [Token]
    trimTokenPass = dropWhile isTokenPass . dropWhileEnd isTokenPass

    splitTopLevelCommas :: [Token] -> [[Token]]
    splitTopLevelCommas = go 0 [] []
      where
        go :: Int -> [Token] -> [[Token]] -> [Token] -> [[Token]]
        go _ cur acc [] = reverse (reverse cur : acc)
        go depth cur acc (t:ts) = case t of
            Lex.Symbol Lex.Comma _
                | depth == 0 -> go depth [] (reverse cur : acc) ts
                | otherwise -> go depth (t : cur) acc ts
            _ ->
                let depth'
                        | isLBracketToken t = depth + 1
                        | isRBracketToken t = max 0 (depth - 1)
                        | otherwise = depth
                in go depth' (t : cur) acc ts

    parseLiteralExpr :: [Token] -> Maybe Expression
    parseLiteralExpr toks = case toks of
        [Lex.NumberConst s pos] ->
            classifyNumber s (Lex.NumberConst s pos)
        [Lex.CharConst c pos] ->
            Just (CharConst c (Lex.CharConst c pos))
        [Lex.StrConst s pos] ->
            Just (StringConst s (Lex.StrConst s pos))
        [Lex.Ident s pos]
            | map toLower s == "true" -> Just (BoolConst True (Lex.Ident s pos))
            | map toLower s == "false" -> Just (BoolConst False (Lex.Ident s pos))
            | map toLower s == "null" ->
                let tok = Lex.Ident s pos
                in Just (Cast (Pointer Void, [tok]) (IntConst "0" tok) tok)
            | otherwise -> Nothing
        [Lex.Symbol Lex.Plus pos, x] -> do
            inner <- parseLiteralExpr [x]
            Just (Unary UnaryPlus inner (Lex.Symbol Lex.Plus pos))
        [Lex.Symbol Lex.Minus pos, x] -> do
            inner <- parseLiteralExpr [x]
            Just (Unary UnaryMinus inner (Lex.Symbol Lex.Minus pos))
        (Lex.Symbol Lex.LParen _ : rest) ->
            case reverse rest of
                (Lex.Symbol Lex.RParen _ : innerRev) -> parseLiteralExpr (reverse innerRev)
                _ -> Nothing
        _ -> Nothing


-- Convert a qualified name to an expression.
-- A single segment is treated as a Variable; otherwise Qualified.
qnameToExpr :: ([String], [Token]) -> Expression
qnameToExpr ([x], [t]) = Variable x t
qnameToExpr (xs, ts) = Qualified (reverse xs) (reverse ts)


-- Build sugar function:
--   fun add(a, b) = a + b
-- =>
--   fun add<A, B, C>(a: B, b: C) -> A = a + b
mkUntypedSugarFunction ::
    [Token] ->
    ([String], [Token]) ->
    [(String, [Token])] ->
    Block ->
    Statement
mkUntypedSugarFunction funToks qname bareParams body =
    InstanceMethodT [] [] retDecl (qnameToExpr qname) genParams typedParams body'
  where
    returnsValue = untypedBodyReturnsValue body

    body' = normalizeFunctionBodyByRet (fst retDecl) body

    typeNames
        | returnsValue = take (length bareParams + 1) excelColumnNames
        | otherwise = take (length bareParams) excelColumnNames
    mkType n = Class [n] []

    retDecl :: (Class, [Token])
    retDecl
        | returnsValue =
            case typeNames of
                [] -> (Void, funToks)
                (retName:_) -> (mkType retName, funToks)
        | otherwise = (Void, funToks)

    genParams :: [(Class, [Token])]
    genParams = map (\n -> (mkType n, funToks)) typeNames

    typedParams :: [(Class, String, [Token])]
    typedParams =
        zipWith
            (\(name, toks) tyName -> (mkType tyName, name, toks))
            bareParams
            paramTypeNames

    paramTypeNames :: [String]
    paramTypeNames
        | returnsValue = drop 1 typeNames
        | otherwise = typeNames

-- Infer whether an untyped sugar function should be value-returning.
-- Rule:
--   - has explicit `return expr`, or
--   - has a tail expression in function body.
-- Otherwise it is treated as `void` (implicit `return;`).
untypedBodyReturnsValue :: Block -> Bool
untypedBodyReturnsValue body =
    blockHasValueReturn body || blockTailIsExpr body
  where
    blockHasValueReturn :: Block -> Bool
    blockHasValueReturn (Multiple ss) = any stmtHasValueReturn ss

    stmtHasValueReturn :: Statement -> Bool
    stmtHasValueReturn st = case st of
        Command (Return (Just _)) _ -> True
        StmtGroup ss -> any stmtHasValueReturn ss
        BlockStmt b -> blockHasValueReturn b
        If _ mb1 mb2 _ ->
            maybe False blockHasValueReturn mb1 ||
            maybe False blockHasValueReturn mb2
        For (s1, _, s3) mb1 mb2 _ ->
            maybe False stmtHasValueReturn s1 ||
            maybe False stmtHasValueReturn s3 ||
            maybe False blockHasValueReturn mb1 ||
            maybe False blockHasValueReturn mb2
        Loop mb _ -> maybe False blockHasValueReturn mb
        Repeat _ mb1 mb2 _ ->
            maybe False blockHasValueReturn mb1 ||
            maybe False blockHasValueReturn mb2
        While _ mb1 mb2 _ ->
            maybe False blockHasValueReturn mb1 ||
            maybe False blockHasValueReturn mb2
        Until _ mb1 mb2 _ ->
            maybe False blockHasValueReturn mb1 ||
            maybe False blockHasValueReturn mb2
        DoWhile mb1 _ mb2 _ ->
            maybe False blockHasValueReturn mb1 ||
            maybe False blockHasValueReturn mb2
        DoUntil mb1 _ mb2 _ ->
            maybe False blockHasValueReturn mb1 ||
            maybe False blockHasValueReturn mb2
        Switch _ cases _ -> any switchCaseHasValueReturn cases
        Function {} -> False
        FunctionT {} -> False
        NativeMethod {} -> False
        _ -> False

    switchCaseHasValueReturn :: SwitchCase -> Bool
    switchCaseHasValueReturn one = case one of
        Case _ mb _ -> maybe False blockHasValueReturn mb
        Default b _ -> blockHasValueReturn b

    blockTailIsExpr :: Block -> Bool
    blockTailIsExpr (Multiple ss) = stmtsTailIsExpr ss

    stmtsTailIsExpr :: [Statement] -> Bool
    stmtsTailIsExpr [] = False
    stmtsTailIsExpr xs = case reverse xs of
        (one:_) -> stmtTailIsExpr one
        [] -> False

    stmtTailIsExpr :: Statement -> Bool
    stmtTailIsExpr st = case st of
        Expr _ -> True
        Exprs es -> not (null es)
        StmtGroup ss -> stmtsTailIsExpr ss
        BlockStmt b -> blockTailIsExpr b
        _ -> False

-- Normalize function body with Rust-like tail-expression return.
-- Only applies to non-void functions.
normalizeFunctionBodyByRet :: Class -> Block -> Block
normalizeFunctionBodyByRet retTy body
    | normalizeClass retTy == Void = body
    | otherwise = rewriteTailReturnBlock body

rewriteTailReturnBlock :: Block -> Block
rewriteTailReturnBlock (Multiple ss) =
    Multiple (rewriteTailToReturn ss)
  where
    rewriteTailToReturn :: [Statement] -> [Statement]
    rewriteTailToReturn [] = []
    rewriteTailToReturn xs =
        let (revHead, lastStmt) = case reverse xs of
                [] -> ([], Nothing)
                (y:ys) -> (reverse ys, Just y)
        in case lastStmt of
            Nothing -> xs
            Just one -> revHead ++ [rewriteLastStmt one]

    rewriteLastStmt :: Statement -> Statement
    rewriteLastStmt st = case st of
        Expr e ->
            Command (Return (Just e)) (nearestTok (exprTokens e))
        Exprs es -> case reverse es of
            (e:_) -> Command (Return (Just e)) (nearestTok (exprTokens e))
            [] -> st
        StmtGroup ssInner ->
            StmtGroup (rewriteTailToReturn ssInner)
        BlockStmt b ->
            BlockStmt (rewriteTailReturnBlock b)
        _ -> st


--  Choose a token to anchor diagnostics.
nearestTok :: [Token] -> Token
nearestTok = fromMaybe (Lex.EOF (makePosition 0 0 0)) . listToMaybe


-- Canonical fatal parse error expression.
mkHappyErrorExpr :: [Token] -> Expression
mkHappyErrorExpr ts = let t = nearestTok ts in Error [t] ("invalid syntax: " ++ show t)


-- | Check for mismatched or unbalanced brackets in the token stream.
--   Returns the first offending token, or Nothing if brackets are balanced.
checkBracket :: [Token] -> Maybe Token
checkBracket = go []
    where
        go :: [Token] -> [Token] -> Maybe Token
        go [] [] = Nothing
        go (t:_) [] = Just t

        go [] (t:ts)
            | isLBracketToken t = go [t] ts
            | isRBracketToken t = Just t
            | otherwise = go [] ts

        go s@(t:ts) (t':ts')
            | isLBracketToken t' = go (t':s) ts'
            | isRBracketToken t' = if eq (matchBracket t) t' then go ts ts' else Just t'
            | otherwise = go s ts'

        matchBracket :: Token -> Token
        matchBracket (Lex.Symbol Lex.LParen p) = Lex.Symbol Lex.RParen p
        matchBracket (Lex.Symbol Lex.LBracket p) = Lex.Symbol Lex.RBracket p
        matchBracket (Lex.Symbol Lex.LBrace p) = Lex.Symbol Lex.RBrace p
        matchBracket _ = error "matchBracket: expected left bracket"

        eq :: Token -> Token -> Bool
        eq (Lex.Symbol a _) (Lex.Symbol b _) = a == b
        eq _ _ = error "what did u input?"


-- | Classify a numeric literal string into the first matching numeric type.
--   Returns Nothing if the string does not represent a valid number.
classifyNumber :: String -> Token -> Maybe Expression
classifyNumber s t = let xs = [(IntConst, isInt),
                              (LongConst, isLong),
                              (FloatConst, isFloat),
                              (DoubleConst, isDouble),
                              (LongDoubleConst, isLongDouble)]
                    in (\(ctor, _) -> ctor s t) <$> find (\(_, p) -> p s) xs


-- error from parser to a real catchable error
toException :: Path -> Expression -> ErrorKind
toException p (Error ts why) = UE.Parsing $ UE.makeError p (map tokenPos ts) why
toException _ _  = error "Expected an Error expression."


-- Convert a statement into a block.
-- If it's already a block statement, reuse it; otherwise wrap it.
stmtToBlock :: Statement -> Block
stmtToBlock (BlockStmt b) = b
stmtToBlock s = Multiple [s]


-- Convert a statement into a expression.
stmtToExpr :: Token -> Statement -> Expression
stmtToExpr _ (Expr e) = e
stmtToExpr t _ = Error [t] (expectedExpression 1 $ show t)


exprListToStmt :: [Expression] -> Statement
exprListToStmt [one] = exprToStmt one
exprListToStmt xs = Exprs xs


exprToStmt :: Expression -> Statement
exprToStmt (BlockExpr b) = BlockStmt b
exprToStmt e@(IfExpr _ _ _ _) = ifExprToStmt e
exprToStmt e = Expr e


ifExprToStmt :: Expression -> Statement
ifExprToStmt (IfExpr cond thenE elseE toks@(_, elseTok)) =
    If cond (Just (exprToBlock thenE)) maybeElse toks
  where
    maybeElse :: Maybe Block
    maybeElse
        | isNothing elseTok && isMissingIfElseMarker elseE = Nothing
        | otherwise = Just (exprToBlock elseE)

    exprToBlock :: Expression -> Block
    exprToBlock (BlockExpr b) = b
    exprToBlock e = Multiple [exprToStmt e]
ifExprToStmt e = Expr e


mkIfExprNoElse :: Token -> Expression -> Expression -> Expression
mkIfExprNoElse ifTok cond thenE =
    IfExpr cond thenE (Error [ifTok] missingIfElseMarkerMsg) (ifTok, Nothing)

mkIfExprFromTail :: Token -> Expression -> Expression -> Maybe (Expression, Maybe Token) -> Expression
mkIfExprFromTail ifTok cond thenE mTail = case mTail of
    Nothing ->
        mkIfExprNoElse ifTok cond thenE
    Just (elseExpr, elseTok) ->
        IfExpr cond thenE elseExpr (ifTok, elseTok)

mkIfElifFromTail :: Token -> Expression -> Expression -> Maybe (Expression, Maybe Token) -> (Expression, Maybe Token)
mkIfElifFromTail ifTok cond thenE mTail =
    let e = mkIfExprFromTail ifTok cond thenE mTail
    in case e of
        IfExpr _ _ _ (_, t) -> (e, t)
        _ -> (e, Nothing)


isMissingIfElseMarker :: Expression -> Bool
isMissingIfElseMarker (Error _ why) = why == missingIfElseMarkerMsg
isMissingIfElseMarker _ = False


missingIfElseMarkerMsg :: String
missingIfElseMarkerMsg = "if-expression requires else clause"


-- Build an assignment expression with LHS validity check.
mkAssignExpr :: Expression -> Expression -> Token -> Expression
mkAssignExpr lhs rhs tok =
    if isAssignableLhs lhs
        then
            let rhs' = fillIfExprMissingElse rhs lhs
            in case rhs' of
                BlockExpr b ->
                    fromMaybe (Binary Assign lhs rhs' tok) (rewriteAssignWithBlockExpr lhs tok b)
                _ ->
                    Binary Assign lhs rhs' tok
        else Error [tok] assignErrorMsg


fillIfExprMissingElse :: Expression -> Expression -> Expression
fillIfExprMissingElse expr fallback = case expr of
    IfExpr cond thenE elseE toks@(_, elseTok)
        | isNothing elseTok && isMissingIfElseMarker elseE ->
            IfExpr cond thenE fallback toks
        | otherwise ->
            IfExpr cond thenE (fillIfExprMissingElse elseE fallback) toks
    _ -> expr


-- | Parse pointer-style postfix suffix on an arbitrary expression.
-- Supported:
--   - expr.deref / expr.dref  => Unary DeRef expr
-- Unsupported here:
--   - expr.ref (only variable.ref is supported through qualified-name parsing)
mkPointerSuffixExpr :: Expression -> Token -> Expression
mkPointerSuffixExpr base suffixTok = case suffixTok of
    Lex.Ident raw _ -> case map toLower raw of
        "deref" -> Unary DeRef base suffixTok
        "dref" -> Unary DeRef base suffixTok
        "ref" -> Error [suffixTok] "ref can only be used on variables"
        _ -> Error [suffixTok] ("unsupported postfix suffix: " ++ raw)
    _ -> Error [suffixTok] "unsupported postfix suffix"


-- | Build pointer index sugar:
--   ptr[i]  ==>  (ptr + i).deref
mkPointerIndexExpr :: Expression -> Expression -> Token -> Expression
mkPointerIndexExpr base idx lbrTok =
    Unary DeRef (Binary Add base idx lbrTok) lbrTok


-- | Build pointer index + suffix sugar:
--   ptr[i].ref   ==> ptr + i
--   ptr[i].deref ==> (ptr[i]).deref
--   ptr[i].dref  ==> (ptr[i]).deref
mkPointerIndexSuffixExpr :: Expression -> Expression -> Token -> Token -> Expression
mkPointerIndexSuffixExpr base idx lbrTok suffixTok = case suffixTok of
    Lex.Ident raw _ -> case map toLower raw of
        "ref" -> Binary Add base idx lbrTok
        "deref" -> Unary DeRef (mkPointerIndexExpr base idx lbrTok) suffixTok
        "dref" -> Unary DeRef (mkPointerIndexExpr base idx lbrTok) suffixTok
        _ -> Error [suffixTok] ("unsupported postfix suffix: " ++ raw)
    _ -> Error [suffixTok] "unsupported postfix suffix"


-- | Parser-level assignable LHS:
--   - regular variables/qualified names
--   - unary dereference expression (e.g. (arr + i).deref)
isAssignableLhs :: Expression -> Bool
isAssignableLhs e = case e of
    Unary DeRef _ _ -> True
    _ -> isVariable e


-- Build an augmented assignment expression by desugaring to plain assignment.
mkAugAssignExpr :: Operator -> Expression -> Expression -> Token -> Expression
mkAugAssignExpr baseOp lhs rhs tok =
    mkAssignExpr lhs (Binary baseOp lhs rhs tok) tok


-- Rewrite:
--   a = { s1; ...; e; }
-- into
--   { s1; ...; a = e; }
--
-- This keeps block scope intact while making the block tail perform the assign.
rewriteAssignWithBlockExpr :: Expression -> Token -> Block -> Maybe Expression
rewriteAssignWithBlockExpr lhs tok (Multiple ss) =
    BlockExpr . Multiple <$> rewriteTail ss
    where
        rewriteTail :: [Statement] -> Maybe [Statement]
        rewriteTail [] = Nothing
        rewriteTail [one] = (:[]) <$> rewriteLastStmt one
        rewriteTail (x:xs) = do
            xs' <- rewriteTail xs
            pure (x : xs')

        rewriteLastStmt :: Statement -> Maybe Statement
        rewriteLastStmt (Expr e) =
            Just (Expr (Binary Assign lhs e tok))
        rewriteLastStmt (Exprs es) = case reverse es of
            [] -> Nothing
            (eLast:restRev) ->
                let prefix = reverse restRev
                in Just (Exprs (prefix ++ [Binary Assign lhs eLast tok]))
        rewriteLastStmt (StmtGroup gs) =
            StmtGroup <$> rewriteTail gs
        rewriteLastStmt _ = Nothing


-- | Parse an integer-like literal for blob size expressions.
-- Supports:
--   - decimal: 10, -3, +7
--   - long suffix: 10L / 10l
--   - hex: 0x10 / 0X10 (optionally with sign)
-- Returns 0 when parsing fails.
parseBlobSizeLiteral :: String -> Int
parseBlobSizeLiteral raw =
    let s0 = if not (null raw) && (last raw == 'l' || last raw == 'L')
                then init raw
                else raw
        (signN, body0) = case s0 of
            ('+':xs) -> (1 :: Integer, xs)
            ('-':xs) -> (-1 :: Integer, xs)
            _ -> (1 :: Integer, s0)
        body = map toLower body0
        parsed :: Maybe Integer
        parsed
            | "0x" `prefixOf` body =
                case readHex (drop 2 body) of
                    [(n, "")] -> Just n
                    _ -> Nothing
            | otherwise =
                case reads body :: [(Integer, String)] of
                    [(n, "")] -> Just n
                    _ -> Nothing
    in case parsed of
        Just n ->
            let signedN = signN * n
            in if signedN < toInteger (minBound :: Int) || signedN > toInteger (maxBound :: Int)
                then 0
                else fromInteger signedN
        Nothing -> 0
  where
    prefixOf :: String -> String -> Bool
    prefixOf p s = take (length p) s == p
