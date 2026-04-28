module Semantic.ContextCheck where

import Semantic.NameEnv
import Control.Monad (when, unless)
import Data.List (intercalate, find, foldl')
import Data.Char (toLower)
import Data.Map.Strict (Map)
import Data.Maybe (listToMaybe, fromMaybe, isNothing, mapMaybe)
import Data.Foldable (for_)
import Control.Monad.State.Strict (State, get, put, modify, runState)
import Lex.Token (Token, tokenPos)
import Util.Type (Path, Position)
import Util.Exception (ErrorKind, undefinedIdentity, invalidFunctionName, continueCtrlErrorMsg, breakCtrlErrorMsg, returnCtrlErrorMsg, illegalStatementMsg, cannotAssignMsg, loopCondAssignMsg, invalidExprStmtMsg, nestedFunctionModifierMsg, nestedMainFunctionMsg, nativeCFunctionScopeMsg)
import Parse.SyntaxTree (Expression, Statement, Block, SwitchCase, Program, exprTokens, stmtTokens)

import qualified Data.Map.Strict as Map
import qualified Lex.Token as Lex
import qualified Parse.SyntaxTree as AST
import qualified Util.Exception as UE


-- | Predicate: operator is an assignment-like op.
isAssignOp :: AST.Operator -> Bool
isAssignOp op = op `elem` [
    AST.Assign,
    AST.PlusAssign, AST.MinusAssign, AST.MultiplyAssign, AST.DivideAssign, AST.ModuloAssign, AST.PowerAssign]

-- | Predicate: operator is an inc/dec op.
isIncDecOp :: AST.Operator -> Bool
isIncDecOp op = op `elem` [AST.IncSelf, AST.DecSelf, AST.SelfInc, AST.SelfDec]


data PtrSuffixOp
    = PtrRef
    | PtrDeref
    deriving (Eq, Show)


parsePointerSuffixQualified :: [String] -> [Token] -> Maybe (String, Token, [PtrSuffixOp])
parsePointerSuffixQualified names toks = case (names, toks) of
    (base : suffixes, baseTok : _) -> do
        ops <- traverse parseSuffix suffixes
        if null ops
            then Nothing
            else Just (base, baseTok, ops)
    _ -> Nothing
  where
    parseSuffix :: String -> Maybe PtrSuffixOp
    parseSuffix s = case map toLower s of
        "ref" -> Just PtrRef
        "deref" -> Just PtrDeref
        "dref" -> Just PtrDeref
        _ -> Nothing

-- | determine an expression have asssignment or not
hasAssign :: Expression -> Bool
hasAssign = go
  where
    go (AST.Binary op _ _ _) | isAssignOp op = True
    go (AST.Binary _ a b _) = go a || go b
    go (AST.Unary _ x _) = go x
    go (AST.Cast _ x _) = go x
    go (AST.Call f args) = go f || any go args
    go (AST.CallT f _ args) = go f || any go args
    go (AST.BlockExpr (AST.Multiple ss)) = any stmtHasAssign ss
    -- Leaves
    go (AST.Error _ _) = False
    go (AST.IntConst _ _) = False
    go (AST.LongConst _ _) = False
    go (AST.FloatConst _ _) = False
    go (AST.DoubleConst _ _) = False
    go (AST.LongDoubleConst _ _) = False
    go (AST.CharConst _ _) = False
    go (AST.StringConst _ _) = False
    go (AST.BoolConst _ _) = False
    go (AST.Variable _ _) = False
    go (AST.Qualified _ _) = False

    stmtHasAssign :: Statement -> Bool
    stmtHasAssign stmt0 = case stmt0 of
        AST.Expr e -> go e
        AST.Exprs es -> any go es
        AST.DefField _ _ me _ -> maybe False go me
        AST.DefConstField _ _ me _ -> maybe False go me
        AST.DefVar _ _ me _ -> maybe False go me
        AST.DefConstVar _ _ me _ -> maybe False go me
        AST.StmtGroup ss -> any stmtHasAssign ss
        AST.BlockStmt (AST.Multiple ss) -> any stmtHasAssign ss
        AST.Command (AST.Return (Just e)) _ -> go e
        AST.If e b1 b2 _ -> go e || blockHasAssign b1 || blockHasAssign b2
        AST.For (s1, e2, s3) b1 b2 _ ->
            maybe False stmtHasAssign s1 ||
            maybe False go e2 ||
            maybe False stmtHasAssign s3 ||
            blockHasAssign b1 ||
            blockHasAssign b2
        AST.Loop b _ -> blockHasAssign b
        AST.Repeat e b1 b2 _ -> go e || blockHasAssign b1 || blockHasAssign b2
        AST.While e b1 b2 _ -> go e || blockHasAssign b1 || blockHasAssign b2
        AST.Until e b1 b2 _ -> go e || blockHasAssign b1 || blockHasAssign b2
        AST.DoWhile b1 e b2 _ -> blockHasAssign b1 || go e || blockHasAssign b2
        AST.DoUntil b1 e b2 _ -> blockHasAssign b1 || go e || blockHasAssign b2
        AST.Switch e scs _ -> go e || any caseHasAssign scs
        AST.Function {} -> False
        AST.FunctionT {} -> False
        AST.NativeMethod {} -> False
        AST.Command {} -> False

    blockHasAssign :: Maybe Block -> Bool
    blockHasAssign Nothing = False
    blockHasAssign (Just (AST.Multiple ss)) = any stmtHasAssign ss

    caseHasAssign :: AST.SwitchCase -> Bool
    caseHasAssign sc = case sc of
        AST.Case e mb _ -> go e || blockHasAssign mb
        AST.Default b _ -> blockHasAssign (Just b)

-- | Predicate: expression is a valid statement-expression (Java-style).
--   Only assignments, function calls, or ++/-- are allowed.
isStmtExpr :: Expression -> Bool
isStmtExpr expr = case expr of
    AST.Binary op _ _ _ -> isAssignOp op
    AST.Call _ _ -> True
    AST.CallT {} -> True
    AST.Unary op _ _ -> isIncDecOp op
    _ -> False

-- | True if an expression is a plain assignment (used for top-level declarations).
isTopLevelAssign :: Expression -> Bool
isTopLevelAssign (AST.Binary AST.Assign _ _ _) = True
isTopLevelAssign _ = False


data Ctx = Ctx {
    st :: CheckState,
    errs :: [ErrorKind],
    varUses :: Map Position VarId
}
    deriving (Eq, Show)

type CheckM a = State Ctx a


-- | Join a qualified name into a dotted string.
concatQ :: QName -> String
concatQ = intercalate "."

-- | Determine parent control context for legality checks.
--   At top-level (no control stack), treat it like a block so that
--   static-init statements (loops/ifs) are allowed.
parentCtrlFor :: CheckState -> Maybe CtrlState
parentCtrlFor = listToMaybe . ctrlStack


-- | Append a new error to the context (keeps existing errors).
addErr :: ErrorKind -> CheckM ()
addErr e = modify $ \c -> c { errs = e : errs c }


-- | Read the current checking state.
getState :: CheckM CheckState
getState = st <$> get


-- | Replace the current checking state.
putState :: CheckState -> CheckM ()
putState s = modify $ \c -> c { st = s }


-- | Extract common pieces from function-like statements.
--   Left block  = regular xlang body
--   Right codeS = native raw code body
functionLikeParts :: Statement -> Maybe ([Token], [(AST.Class, String, [Token])], Either Block String)
functionLikeParts stmt = case stmt of
    AST.Function (_, toks) _ params b -> Just (toks, params, Left b)
    AST.FunctionT (_, toks) _ _ params b -> Just (toks, params, Left b)
    AST.NativeMethod (_, toks) _ params codeS -> Just (toks, params, Right codeS)
    _ -> Nothing

-- | Predicate: is this statement a function definition?
isFunction :: Statement -> Bool
isFunction stmt = case functionLikeParts stmt of
    Just _ -> True
    Nothing -> False

-- | Extract common pieces from var/val declarations.
declLikeParts :: Statement -> Maybe (Bool, Bool, [String], Maybe AST.Class, Maybe Expression, [Token])
declLikeParts stmt = case stmt of
    AST.DefField names mTy mRhs toks -> Just (True, False, names, mTy, mRhs, toks)
    AST.DefConstField names mTy mRhs toks -> Just (True, True, names, mTy, mRhs, toks)
    AST.DefVar names mTy mRhs toks -> Just (False, False, names, mTy, mRhs, toks)
    AST.DefConstVar names mTy mRhs toks -> Just (False, True, names, mTy, mRhs, toks)
    _ -> Nothing
isPublicDecl :: [Token] -> Bool
isPublicDecl (Lex.Ident "public" _ : _) = True
isPublicDecl _ = False


validateDeclAccess :: Path -> [Token] -> CheckM ()
validateDeclAccess _ toks
    | not (isPublicDecl toks) = pure ()
validateDeclAccess p toks = do
    c <- get
    let parentCtrl = parentCtrlFor (st c)
        valid = case parentCtrl of
            Nothing -> True
            Just InClass -> True
            _ -> False
    unless valid $ addErr $ UE.Syntax $ UE.makeError p (map tokenPos (take 1 toks)) UE.publicScopeMsg


-- | Push a control-flow context for the duration of an action, then pop it.
withCtrl :: CtrlState -> CheckM a -> CheckM a
withCtrl ctrl action = do
    c <- get
    let cState = st c
    put $ c { st = cState { ctrlStack = ctrl : ctrlStack cState } }
    res <- action
    c2 <- get
    let cState2 = st c2
    put $ c2 { st = cState2 { ctrlStack = tail $ ctrlStack cState2 } }
    pure res


-- | Create a new lexical scope for the duration of an action, then restore depth/scope.
withScope :: CheckM a -> CheckM a
withScope action = do
    c <- get
    let cState = st c
    let depth0 = depth cState
    let newScope = Scope { scopeId = scopeCounter cState, sVars = Map.empty, sFuncs = Map.empty }
    let cState' = cState {
        depth = succ depth0,
        scopeCounter = succ $ scopeCounter cState,
        scope = newScope : scope cState}
    put $ c { st = cState' }
    res <- action
    c2 <- get
    let cState2 = st c2
    let scope' = tail $ scope cState2
    put $ c2 { st = cState2 { depth = depth0, scope = scope' } }
    pure res


-- | Push a control context and a new lexical scope for the duration of an action.
withCtrlScope :: CtrlState -> CheckM a -> CheckM a
withCtrlScope ctrl action = withCtrl ctrl (withScope action)


-- | Minimal expression traversal; no name/type rules yet.
checkExpr :: Path -> QName -> [ImportEnv] -> Expression -> CheckM ()
checkExpr p packages envs expr = case expr of
    AST.Error toks msg ->
        addErr $ UE.Syntax $ UE.makeError p (map tokenPos toks) msg
    AST.IntConst _ _ -> pure ()
    AST.LongConst _ _ -> pure ()
    AST.FloatConst _ _ -> pure ()
    AST.DoubleConst _ _ -> pure ()
    AST.LongDoubleConst _ _ -> pure ()
    AST.CharConst _ _ -> pure ()
    AST.StringConst _ _ -> pure ()
    AST.BoolConst _ _ -> pure ()

    AST.Variable name tok -> do
        c <- get
        let cState = st c
        if name == "this"
            then
                when (null (classScope cState)) $ addErr $ UE.Syntax $ UE.makeError p [tokenPos tok] (undefinedIdentity name)
            else
                if isVarDefine name cState || isVarImport (packages ++ [name]) envs
                    then do
                        case lookupVarId name cState of
                            Just (vid, _) -> do
                                let uses' = Map.insert (tokenPos tok) vid (varUses c)
                                put $ c { varUses = uses' }
                            Nothing -> pure ()
                    else case lookupHiddenVarPos (packages ++ [name]) envs of
                        Just _ -> addErr $ UE.Syntax $ UE.makeError p [tokenPos tok] UE.notVisibleMsg
                        Nothing -> addErr $ UE.Syntax $ UE.makeError p [tokenPos tok] (undefinedIdentity name)

    AST.Qualified names tokens ->
        case parsePointerSuffixQualified names tokens of
            Just (baseName, baseTok, ops) -> do
                checkExpr p packages envs (AST.Variable baseName baseTok)
                case ops of
                    [PtrRef] -> pure ()
                    _ | PtrRef `elem` ops ->
                        addErr $ UE.Syntax $ UE.makeError p (map tokenPos tokens)
                            "ref can only be used as a terminal suffix on a variable"
                    _ -> pure ()
            Nothing -> do
                c <- get
                let cState = st c

                case names of
                    -- this.a / this.a.b ... => check 'a' exists in nearest class scope only.
                    ("this":field:_) -> case classScope cState of
                        [] -> addErr $ UE.Syntax $ UE.makeError p (map tokenPos tokens) (undefinedIdentity field)

                        (clsTop:_) -> if Map.member field (sVars clsTop) then pure ()
                            else addErr $ UE.Syntax $ UE.makeError p (map tokenPos tokens) (undefinedIdentity field)

                    -- non-this qualified name is resolved only through imports.
                    _ -> if isVarImport names envs
                            then pure ()
                            else case lookupHiddenVarPos names envs of
                                Just _ -> addErr $ UE.Syntax $ UE.makeError p (map tokenPos tokens) UE.notVisibleMsg
                                Nothing -> addErr $ UE.Syntax $ UE.makeError p (map tokenPos tokens) (undefinedIdentity $ concatQ names)
    AST.BlockExpr (AST.Multiple ss) ->
        withScope $ checkBlockExprItems ss
                    

    -- recurse into children.
    AST.Cast _ e _ -> checkExpr p packages envs e
    AST.Unary _ e _ -> checkExpr p packages envs e
    AST.Binary AST.Assign e1 e2 tok -> do
        checkExpr p packages envs e2

        case e1 of
            AST.Variable "this" nameTok ->
                addErr $ UE.Syntax $ UE.makeError p [tokenPos nameTok] UE.thisAssignMsg
            AST.Variable {} -> do
                c <- get
                let cState = st c
                case defineLocalVar p (AST.Binary AST.Assign e1 e2 tok) cState of
                    Left err -> addErr err
                    Right cState' -> do
                        case e1 of
                            AST.Variable name nameTok ->
                                case lookupVarId name cState' of
                                    Just (vid, _) -> do
                                        let uses' = Map.insert (tokenPos nameTok) vid (varUses c)
                                        put $ c { st = cState', varUses = uses' }
                                    Nothing -> put $ c { st = cState' }

            AST.Qualified names toks ->
                case parsePointerSuffixQualified names toks of
                    Just _ ->
                        addErr $ UE.Syntax $ UE.makeError p (map tokenPos toks)
                            (cannotAssignMsg (AST.prettyExpr 0 (Just e1)))
                    Nothing -> do
                        c <- get
                        let cState = st c
                        case defineLocalVar p (AST.Binary AST.Assign e1 e2 tok) cState of
                            Left err -> addErr err
                            Right cState' -> put $ c { st = cState' }

            _ -> addErr $ UE.Syntax $ UE.makeError p (map tokenPos $ exprTokens e1) (cannotAssignMsg (AST.prettyExpr 0 (Just e1)))

    AST.Binary _ e1 e2 _ -> do
        checkExpr p packages envs e1
        checkExpr p packages envs e2

    -- ???????????????????????CallT ??????????????
    AST.Call callee args -> checkCall callee args
    AST.CallT callee _ args -> checkCall callee args
    where
        checkBlockExprItems :: [Statement] -> CheckM ()
        checkBlockExprItems [] =
            addErr $ UE.Syntax $ UE.makeError p [] "block expression cannot be empty"
        checkBlockExprItems [lastStmt] =
            checkBlockExprTail lastStmt
        checkBlockExprItems (s:ss) = do
            checkBlockExprStmt s
            checkBlockExprItems ss

        checkBlockExprStmt :: Statement -> CheckM ()
        checkBlockExprStmt stmt = case stmt of
            AST.Expr e -> checkExpr p packages envs e
            AST.Exprs es -> mapM_ (checkExpr p packages envs) es
            AST.StmtGroup ss -> mapM_ checkBlockExprStmt ss
            _ | Just (isFieldDecl, isConstDecl, names, mDeclType, mRhs, toks) <- declLikeParts stmt ->
                    checkDefDecl p packages envs isFieldDecl isConstDecl names mDeclType mRhs toks
              | otherwise ->
                    checkStmt p packages envs stmt

        checkBlockExprTail :: Statement -> CheckM ()
        checkBlockExprTail stmt = case stmt of
            AST.Expr e -> checkExpr p packages envs e
            AST.Exprs es -> case reverse es of
                [] -> addErr $ UE.Syntax $ UE.makeError p [] "block expression cannot be empty"
                _ -> mapM_ (checkExpr p packages envs) es
            AST.StmtGroup ss -> case reverse ss of
                [] -> addErr $ UE.Syntax $ UE.makeError p [] "block expression cannot be empty"
                _ -> checkBlockExprItems ss
            _ -> do
                checkBlockExprStmt stmt
                addErr $ UE.Syntax $ UE.makeError p (map tokenPos $ stmtTokens stmt) "block expression must end with an expression"

        checkCall :: Expression -> [Expression] -> CheckM ()
        checkCall callee args = do
            case callee of
                AST.Variable name tok -> do
                    c <- get
                    let cState = st c
                    if isFuncDefine [name] cState || isFunImport (packages ++ [name]) envs then pure ()
                    else case lookupHiddenFunPos (packages ++ [name]) envs of
                        Just _ -> addErr $ UE.Syntax $ UE.makeError p [tokenPos tok] UE.notVisibleMsg
                        Nothing -> addErr $ UE.Syntax $ UE.makeError p [tokenPos tok] (undefinedIdentity name)

                AST.Qualified names tokens -> do
                    c <- get
                    let cState = st c

                    case names of
                        -- this.f / this.f.g ... => check 'f' exists in nearest class scope only.
                        ("this":field:_) -> case classScope cState of
                            [] -> addErr $ UE.Syntax $ UE.makeError p (map tokenPos tokens) (undefinedIdentity field)

                            (clsTop:_) -> if Map.member [field] (sFuncs clsTop) then pure ()
                                else addErr $ UE.Syntax $ UE.makeError p (map tokenPos tokens) (undefinedIdentity field)

                        -- non-this qualified name is resolved only through imports.
                        _ -> if isFunImport names envs
                                then pure ()
                                else case lookupHiddenFunPos names envs of
                                    Just _ -> addErr $ UE.Syntax $ UE.makeError p (map tokenPos tokens) UE.notVisibleMsg
                                    Nothing -> addErr $ UE.Syntax $ UE.makeError p (map tokenPos tokens) (undefinedIdentity $ concatQ names)

                other -> addErr $ UE.Syntax $ UE.makeError p (map tokenPos $ exprTokens other) invalidFunctionName

            mapM_ (checkExpr p packages envs) args



checkDefDecl :: Path -> QName -> [ImportEnv] -> Bool -> Bool -> [String] -> Maybe AST.Class -> Maybe Expression -> [Token] -> CheckM ()
checkDefDecl p package envs isFieldDecl isConstDecl names _mDeclType mRhs toks = do
    validateDeclAccess p toks
    c0 <- get
    let parentCtrl = parentCtrlFor (st c0)
        shouldBeVar = parentCtrl /= Just InClass

    when (isFieldDecl && shouldBeVar && isNothing mRhs) $
        let nameText = case names of
                [name] -> name
                _ -> intercalate "." names
            pos = case reverse toks of
                (nameTok:_) -> [tokenPos nameTok]
                [] -> map tokenPos toks
            keyword = if isConstDecl then "val" else "var"
        in addErr $ UE.Syntax $ UE.makeError p pos (UE.missingInitializerMsg keyword nameText)

    for_ mRhs (checkExpr p package envs)

    case names of
        [name] -> case reverse toks of
            (nameTok:_) -> do
                c <- get
                let cState = st c
                case defineDeclaredVar p name nameTok cState of
                    Left err -> addErr err
                    Right cState' -> case lookupVarId name cState' of
                        Just (vid, _) -> do
                            let uses' = Map.insert (tokenPos nameTok) vid (varUses c)
                            put $ c { st = cState', varUses = uses' }
                        Nothing -> put $ c { st = cState' }
            [] ->
                let errPos = maybe (map tokenPos toks) (map tokenPos . exprTokens) mRhs
                in addErr $ UE.Syntax $ UE.makeError p errPos UE.internalErrorMsg
        _ -> addErr $ UE.Syntax $ UE.makeError p (map tokenPos toks) UE.unsupportedErrorMsg


-- | Continue is valid only inside loops.
isContinueValid :: [CtrlState] -> Bool
isContinueValid = elem InLoop


-- | Break is valid inside loops or switch cases.
isBreakValid :: [CtrlState] -> Bool
isBreakValid ctrls = elem InLoop ctrls || elem InCase ctrls


-- | Return is valid only inside functions.
isReturnValid :: [CtrlState] -> Bool
isReturnValid = elem InFunction

-- | At top-level (empty control stack), allow command-only flow statements.
isTopLevelCtrlScope :: [CtrlState] -> Bool
isTopLevelCtrlScope = null

-- | Top-level `return` must be bare: `return;`.
topLevelReturnValueErrorMsg :: String
topLevelReturnValueErrorMsg = "top-level `return` must not carry a value"


-- | Check a single statement and all of its nested blocks.
checkStmt :: Path -> QName -> [ImportEnv] -> Statement -> CheckM ()
checkStmt p package envs (AST.Command cmd token) = do
    c <- get
    let cState = st c
    let ctrls = ctrlStack cState

    case cmd of
        AST.Pass ->
            pure ()

        AST.Continue ->
            if isContinueValid ctrls || isTopLevelCtrlScope ctrls then pure ()
            else addErr $ UE.Syntax $ UE.makeError p [tokenPos token] continueCtrlErrorMsg

        AST.Break ->
            if isBreakValid ctrls || isTopLevelCtrlScope ctrls then pure ()
            else addErr $ UE.Syntax $ UE.makeError p [tokenPos token] breakCtrlErrorMsg

        AST.Return mExpr ->
            if isReturnValid ctrls || isTopLevelCtrlScope ctrls
                then case mExpr of
                    Just _ | isTopLevelCtrlScope ctrls ->
                        addErr $ UE.Syntax $ UE.makeError p [tokenPos token] topLevelReturnValueErrorMsg
                    _ ->
                        let checkReturnExpr = maybe (pure ()) (checkExpr p package envs) in checkReturnExpr mExpr
            else addErr $ UE.Syntax $ UE.makeError p [tokenPos token] returnCtrlErrorMsg
checkStmt p package envs stmt
    | Just (isFieldDecl, isConstDecl, names, mDeclType, mRhs, toks) <- declLikeParts stmt =
        checkDefDecl p package envs isFieldDecl isConstDecl names mDeclType mRhs toks
checkStmt p package envs (AST.Expr e) = do
    if not (isStmtExpr e)
        then addErr $ UE.Syntax $ UE.makeError p (map tokenPos $ exprTokens e) invalidExprStmtMsg
        else checkExpr p package envs e
checkStmt p package envs (AST.Exprs es) = do
    let checkOne e = do
            if not (isStmtExpr e)
                then addErr $ UE.Syntax $ UE.makeError p (map tokenPos $ exprTokens e) invalidExprStmtMsg
                else checkExpr p package envs e
    mapM_ checkOne es
checkStmt p package envs (AST.StmtGroup ss) =
    mapM_ (checkStmt p package envs) ss

-- block
checkStmt p package envs stmt@(AST.BlockStmt block) = do
    c <- get
    let cState = st c
    let parentCtrl = parentCtrlFor cState
    when (forbiddenFor parentCtrl InBlock) $ addErr $
        UE.Syntax $ UE.makeError p (map tokenPos $ stmtTokens stmt)
            (illegalStatementMsg (prettyCtrlState InBlock) (prettyCtrlState (fromMaybe InClass parentCtrl)))
    withCtrlScope InBlock $ checkBlock p package envs block

-- if-else
checkStmt p package envs stmt@(AST.If e ifBlock elseBlock _) = do
    c <- get
    let cState = st c
    let parentCtrl = parentCtrlFor cState
    when (forbiddenFor parentCtrl InIf) $ addErr $
        UE.Syntax $ UE.makeError p (map tokenPos $ stmtTokens stmt)
            (illegalStatementMsg (prettyCtrlState InIf) (prettyCtrlState (fromMaybe InClass parentCtrl)))

    checkExpr p package envs e
    for_ ifBlock (withCtrlScope InIf . checkBlock p package envs)
    for_ elseBlock (withCtrlScope InElse . checkBlock p package envs)

-- for
checkStmt p package envs stmt@(AST.For (s1, e2, s3) forBlock elseBlock _) = do
    c <- get
    let cState = st c
    let parentCtrl = parentCtrlFor cState
    when (forbiddenFor parentCtrl InLoop) $ addErr $ UE.Syntax $ UE.makeError p (map tokenPos $ stmtTokens stmt)
        (illegalStatementMsg (prettyCtrlState InLoop) (prettyCtrlState (fromMaybe InClass parentCtrl)))

    withScope $ do
        for_ s1 (checkForInitStmt p package envs)
        
        for_ e2 $ \cond -> do
            when (hasAssign cond) $
                addErr $ UE.Syntax $ UE.makeError p (map tokenPos $ exprTokens cond) loopCondAssignMsg
            checkExpr p package envs cond

        for_ s3 (checkForStepStmt p package envs)
        for_ forBlock (withCtrlScope InLoop . checkBlock p package envs)
        for_ elseBlock (withCtrlScope InElse . checkBlock p package envs)

-- while
checkStmt p package envs stmt@(AST.Loop loopBlock _) = do
    c <- get
    let cState = st c
    let parentCtrl = parentCtrlFor cState
    when (forbiddenFor parentCtrl InLoop) $ addErr $ UE.Syntax $ UE.makeError p (map tokenPos $ stmtTokens stmt)
        (illegalStatementMsg (prettyCtrlState InLoop) (prettyCtrlState (fromMaybe InClass parentCtrl)))

    for_ loopBlock (withCtrlScope InLoop . checkBlock p package envs)

-- repeat(count)
checkStmt p package envs stmt@(AST.Repeat e repeatBlock elseBlock _) = do
    c <- get
    let cState = st c
    let parentCtrl = parentCtrlFor cState
    when (forbiddenFor parentCtrl InLoop) $ addErr $ UE.Syntax $ UE.makeError p (map tokenPos $ stmtTokens stmt)
        (illegalStatementMsg (prettyCtrlState InLoop) (prettyCtrlState (fromMaybe InClass parentCtrl)))

    checkExpr p package envs e
    for_ repeatBlock (withCtrlScope InLoop . checkBlock p package envs)
    for_ elseBlock (withCtrlScope InElse . checkBlock p package envs)

-- while
checkStmt p package envs stmt@(AST.While e whileBlock elseBlock _) = do
    c <- get
    let cState = st c
    let parentCtrl = parentCtrlFor cState
    when (forbiddenFor parentCtrl InLoop) $ addErr $ UE.Syntax $ UE.makeError p (map tokenPos $ stmtTokens stmt)
        (illegalStatementMsg (prettyCtrlState InLoop) (prettyCtrlState (fromMaybe InClass parentCtrl)))

    checkExpr p package envs e
    for_ whileBlock (withCtrlScope InLoop . checkBlock p package envs)
    for_ elseBlock (withCtrlScope InElse . checkBlock p package envs)

-- until
checkStmt p package envs stmt@(AST.Until e untilBlock elseBlock _) = do
    c <- get
    let cState = st c
    let parentCtrl = parentCtrlFor cState
    when (forbiddenFor parentCtrl InLoop) $ addErr $ UE.Syntax $ UE.makeError p (map tokenPos $ stmtTokens stmt)
        (illegalStatementMsg (prettyCtrlState InLoop) (prettyCtrlState (fromMaybe InClass parentCtrl)))

    checkExpr p package envs e
    for_ untilBlock (withCtrlScope InLoop . checkBlock p package envs)
    for_ elseBlock (withCtrlScope InElse . checkBlock p package envs)

-- do while
checkStmt p package envs stmt@(AST.DoWhile whileBlock e elseBlock _) = do
    c <- get
    let cState = st c
    let parentCtrl = parentCtrlFor cState
    when (forbiddenFor parentCtrl InLoop) $ addErr $ UE.Syntax $ UE.makeError p (map tokenPos $ stmtTokens stmt)
        (illegalStatementMsg (prettyCtrlState InLoop) (prettyCtrlState (fromMaybe InClass parentCtrl)))

    for_ whileBlock (withCtrlScope InLoop . checkBlock p package envs)
    checkExpr p package envs e
    for_ elseBlock (withCtrlScope InElse . checkBlock p package envs)

-- do until
checkStmt p package envs stmt@(AST.DoUntil untilBlock e elseBlock _) = do
    c <- get
    let cState = st c
    let parentCtrl = parentCtrlFor cState
    when (forbiddenFor parentCtrl InLoop) $ addErr $ UE.Syntax $ UE.makeError p (map tokenPos $ stmtTokens stmt)
        (illegalStatementMsg (prettyCtrlState InLoop) (prettyCtrlState (fromMaybe InClass parentCtrl)))

    for_ untilBlock (withCtrlScope InLoop . checkBlock p package envs)
    checkExpr p package envs e
    for_ elseBlock (withCtrlScope InElse . checkBlock p package envs)

-- switch
checkStmt p package envs stmt@(AST.Switch e scs _) = do
    c <- get
    let cState = st c
    let parentCtrl = parentCtrlFor cState
    when (forbiddenFor parentCtrl InSwitch) $ addErr $ UE.Syntax $ UE.makeError p (map tokenPos $ stmtTokens stmt)
        (illegalStatementMsg (prettyCtrlState InSwitch) (prettyCtrlState (fromMaybe InClass parentCtrl)))

    checkExpr p package envs e
    withCtrl InSwitch $ for_ scs (checkSwitchCase p package envs)


-- function
checkStmt p package envs stmt = case functionLikeParts stmt of
    Nothing -> pure ()
    Just (declToks, params, bodyLike) -> checkFunctionStmt declToks params bodyLike
    where
        checkFunctionStmt :: [Token] -> [(AST.Class, String, [Token])] -> Either Block String -> CheckM ()
        checkFunctionStmt declToks params bodyLike = do
            validateDeclAccess p declToks
            c <- get
            let cState = st c
            let parentCtrl = parentCtrlFor cState
            when (isNativeCFunction stmt && not (isAllowedNativeCParent parentCtrl)) $
                addErr $ UE.Syntax $ UE.makeError p (functionNamePos stmt declToks) nativeCFunctionScopeMsg
            when (parentCtrl == Just InFunction && hasFunctionModifier declToks) $
                addErr $ UE.Syntax $ UE.makeError p (map tokenPos declToks) nestedFunctionModifierMsg
            when (parentCtrl == Just InFunction && isMainFunction stmt) $
                addErr $ UE.Syntax $ UE.makeError p (functionNamePos stmt declToks) nestedMainFunctionMsg
            when (forbiddenFor parentCtrl InFunction) $ addErr $ UE.Syntax $ UE.makeError p (map tokenPos $ stmtTokens stmt)
                (illegalStatementMsg (prettyCtrlState InFunction) (prettyCtrlState (fromMaybe InClass parentCtrl)))
            case firstVoidParam params of
                Nothing -> pure ()
                Just (_, paramName, toks) ->
                    addErr $ UE.Syntax $ UE.makeError p (map tokenPos toks) (UE.voidParameterMsg paramName)

            cBase <- get
            let cStateBase = st cBase

            let depth0 = depth cStateBase
            let (varCounter', paramVars) = foldl addParam (varCounter cStateBase, Map.empty) params
                addParam (vc, m) (_, name, toks) = case toks of
                    [] -> (vc, m)
                    (t:_) -> (succ vc, Map.insert name (vc, tokenPos t) m)

            let newScope = Scope { scopeId = scopeCounter cStateBase, sVars = paramVars, sFuncs = Map.empty }
            let cState' = cStateBase {
                depth = succ depth0,
                varCounter = varCounter',
                scopeCounter = succ $ scopeCounter cStateBase,
                ctrlStack = InFunction : ctrlStack cStateBase,
                scope = newScope : scope cStateBase }
            put $ cBase { st = cState' }

            case bodyLike of
                Left body -> checkBlock p package envs body
                Right _ -> pure ()

            c2 <- get
            let cState2 = st c2
            let scope' = tail $ scope cState2
            let ctrls' = tail $ ctrlStack cState2
            put $ c2 { st = cState2 { depth = depth0, scope = scope', ctrlStack = ctrls' } }

        firstVoidParam :: [(AST.Class, String, [Token])] -> Maybe (AST.Class, String, [Token])
        firstVoidParam = find (\(clazz, _, _) -> clazz == AST.Void)

        hasFunctionModifier :: [Token] -> Bool
        hasFunctionModifier = any isFunctionModifier

        isFunctionModifier :: Token -> Bool
        isFunctionModifier (Lex.Ident s _) =
            let k = map toLower s
            in k == "public" || k == "private" || k == "protected" || k == "static" || k == "final" || k == "const"
        isFunctionModifier _ = False

        isMainFunction :: Statement -> Bool
        isMainFunction s = case functionName s of
            Just "main" -> True
            _ -> False

        functionName :: Statement -> Maybe String
        functionName (AST.Function _ (AST.Variable name _) _ _) = Just name
        functionName (AST.FunctionT _ (AST.Variable name _) _ _ _) = Just name
        functionName (AST.NativeMethod _ (AST.Variable name _) _ _) = Just name
        functionName _ = Nothing

        functionNamePos :: Statement -> [Token] -> [Position]
        functionNamePos (AST.Function _ (AST.Variable _ tok) _ _) _ = [tokenPos tok]
        functionNamePos (AST.FunctionT _ (AST.Variable _ tok) _ _ _) _ = [tokenPos tok]
        functionNamePos (AST.NativeMethod _ (AST.Variable _ tok) _ _) _ = [tokenPos tok]
        functionNamePos _ declToks = map tokenPos declToks

        isNativeCFunction :: Statement -> Bool
        isNativeCFunction (AST.NativeMethod {}) = True
        isNativeCFunction _ = False

        isAllowedNativeCParent :: Maybe CtrlState -> Bool
        isAllowedNativeCParent Nothing = True
        isAllowedNativeCParent (Just InClass) = True
        isAllowedNativeCParent _ = False


checkForInitStmt :: Path -> QName -> [ImportEnv] -> Statement -> CheckM ()
checkForInitStmt p package envs forInitStmt = case forInitStmt of
    AST.Expr e -> checkExprInit e
    AST.Exprs es -> mapM_ checkExprInit es
    AST.DefField {} -> checkStmt p package envs forInitStmt
    AST.DefConstField {} -> checkStmt p package envs forInitStmt
    AST.DefVar {} -> checkStmt p package envs forInitStmt
    AST.DefConstVar {} -> checkStmt p package envs forInitStmt
    AST.StmtGroup ss -> mapM_ (checkForInitStmt p package envs) ss
    AST.BlockStmt (AST.Multiple ss) -> mapM_ (checkForInitStmt p package envs) ss
    _ -> addErr $ UE.Syntax $ UE.makeError p (map tokenPos $ stmtTokens forInitStmt) invalidExprStmtMsg
    where
        checkExprInit :: Expression -> CheckM ()
        checkExprInit initExpr = do
            case initExpr of
                AST.Binary AST.Assign _ _ _ -> pure ()
                _ -> addErr $ UE.Syntax $ UE.makeError p (map tokenPos $ exprTokens initExpr) UE.forInitAssignMsg
            checkExpr p package envs initExpr


checkForStepStmt :: Path -> QName -> [ImportEnv] -> Statement -> CheckM ()
checkForStepStmt p package envs forStepStmt = case forStepStmt of
    AST.Expr e -> checkExprStep e
    AST.Exprs es -> mapM_ checkExprStep es
    AST.StmtGroup ss -> mapM_ (checkForStepStmt p package envs) ss
    AST.BlockStmt (AST.Multiple ss) -> mapM_ (checkForStepStmt p package envs) ss
    _ -> addErr $ UE.Syntax $ UE.makeError p (map tokenPos $ stmtTokens forStepStmt) invalidExprStmtMsg
    where
        checkExprStep :: Expression -> CheckM ()
        checkExprStep e = do
            if not (isStmtExpr e)
                then addErr $ UE.Syntax $ UE.makeError p (map tokenPos $ exprTokens e) invalidExprStmtMsg
                else checkExpr p package envs e


-- | Check a single switch case (case/default).
checkSwitchCase :: Path -> QName -> [ImportEnv] -> SwitchCase -> CheckM()
checkSwitchCase path q envs (AST.Case e mb _) = do
    checkExpr path q envs e
    for_ mb (withCtrlScope InCase . checkBlock path q envs)
checkSwitchCase path q envs (AST.Default b _) = withCtrlScope InCase (checkBlock path q envs b)


-- | Check a block: predefine functions, then check each statement.
checkBlock :: Path -> QName -> [ImportEnv] -> Block -> CheckM()
checkBlock p q envs (AST.Multiple ss) = checkStmts p q envs ss
    

-- | Check a list of statements, preloading function signatures first.
checkStmts :: Path -> QName -> [ImportEnv] -> [Statement] -> CheckM ()
checkStmts path package envs' stmts = do
    let stmts' = flattenStmtGroups stmts
        funDefs = filter isFunction stmts'
        nativeErrs = checkNativeDependencyShape path stmts'
    mapM_ addErr nativeErrs
    mapM_ defineOne funDefs
    mapM_ (checkStmt path package envs') stmts'
    where
        defineOne :: Statement -> CheckM ()
        defineOne stmt = do
            c <- get
            let cState = st c
            case defineFunc path stmt cState of
                Left err -> addErr err
                Right cState' -> put $ c { st = cState' }

        flattenStmtGroups :: [Statement] -> [Statement]
        flattenStmtGroups = concatMap go
            where
                go :: Statement -> [Statement]
                go (AST.StmtGroup ss) = flattenStmtGroups ss
                go stmt0 = [stmt0]


checkNativeDependencyShape :: Path -> [Statement] -> [ErrorKind]
checkNativeDependencyShape path stmts =
    let funs = AST.collectCNativeFuns stmts
        keyPosMap = Map.fromList (mapMaybe nativeKeyPos stmts)
    in case AST.cNativeTopoSort funs of
        Right _ -> []
        Left cycKeys ->
            let pos = case cycKeys of
                    (k:_) -> Map.findWithDefault [] k keyPosMap
                    [] -> []
                cycleTxt = case cycKeys of
                    [] -> "<unknown>"
                    ks@(x:_) -> intercalate " -> " (ks ++ [x])
                msg = "native C dependency has a cycle: " ++ cycleTxt
            in [UE.Syntax $ UE.makeError path pos msg]
    where
        nativeKeyPos :: Statement -> Maybe (String, [Position])
        nativeKeyPos stmt0 = do
            fun <- AST.cNativeFunFromStmt stmt0
            (_, tok) <- AST.functionNameVar stmt0
            pure (AST.cNativeFunKey fun, [tokenPos tok])


-- | Validate top-level name conflicts among:
--   A = static vars (DefVar / DefConstVar)
--   B = fields (DefField / DefConstField)
--   C = function names
-- Rule required:
--   A ∩ B = ∅ and A ∩ C = ∅
checkTopLevelMemberNameConflicts :: Path -> [Statement] -> [ErrorKind]
checkTopLevelMemberNameConflicts path stmts =
    let topStmts = flattenTopStmtGroups stmts
        staticMap = collectStaticVars topStmts
        fieldMap = collectFields topStmts
        funMap = collectFunctions topStmts
        conflictAB = concatMap (mkConflicts "static field" "attribute" staticMap fieldMap) (Map.keys staticMap)
        conflictAC = concatMap (mkConflicts "static field" "function" staticMap funMap) (Map.keys staticMap)
    in conflictAB ++ conflictAC
    where
        flattenTopStmtGroups :: [Statement] -> [Statement]
        flattenTopStmtGroups = concatMap go
            where
                go :: Statement -> [Statement]
                go (AST.StmtGroup ss) = flattenTopStmtGroups ss
                go s = [s]

        collectStaticVars :: [Statement] -> Map String [Position]
        collectStaticVars = foldl' collect Map.empty
            where
                collect :: Map String [Position] -> Statement -> Map String [Position]
                collect acc stmt = case stmt of
                    AST.DefVar [name] _ _ toks -> insertIfPos acc name (namePos toks)
                    AST.DefConstVar [name] _ _ toks -> insertIfPos acc name (namePos toks)
                    _ -> acc

        collectFields :: [Statement] -> Map String [Position]
        collectFields = foldl' collect Map.empty
            where
                collect :: Map String [Position] -> Statement -> Map String [Position]
                collect acc stmt = case stmt of
                    AST.DefField [name] _ _ toks -> insertIfPos acc name (namePos toks)
                    AST.DefConstField [name] _ _ toks -> insertIfPos acc name (namePos toks)
                    _ -> acc

        collectFunctions :: [Statement] -> Map String [Position]
        collectFunctions = foldl' collect Map.empty
            where
                collect :: Map String [Position] -> Statement -> Map String [Position]
                collect acc stmt = case stmt of
                    AST.Function _ (AST.Variable name tok) _ _ ->
                        Map.insertWith (++) name [tokenPos tok] acc
                    AST.FunctionT _ (AST.Variable name tok) _ _ _ ->
                        Map.insertWith (++) name [tokenPos tok] acc
                    AST.NativeMethod _ (AST.Variable name tok) _ _ ->
                        Map.insertWith (++) name [tokenPos tok] acc
                    _ -> acc

        insertIfPos :: Map String [Position] -> String -> Maybe Position -> Map String [Position]
        insertIfPos acc _ Nothing = acc
        insertIfPos acc name (Just pos) = Map.insertWith (++) name [pos] acc

        namePos :: [Token] -> Maybe Position
        namePos toks = case reverse toks of
            (t:_) -> Just (tokenPos t)
            [] -> Nothing

        mkConflicts ::
            String ->
            String ->
            Map String [Position] ->
            Map String [Position] ->
            String ->
            [ErrorKind]
        mkConflicts leftKind rightKind leftMap rightMap name = case Map.lookup name rightMap of
            Nothing -> []
            Just _ ->
                let poss = fromMaybe [] (Map.lookup name leftMap)
                    msg = "name conflict: " ++ leftKind ++ " '" ++ name ++ "' conflicts with " ++ rightKind ++ " '" ++ name ++ "'"
                in map (\pos -> UE.Syntax $ UE.makeError path [pos] msg) poss
                            
                            
-- | Run context checking for a whole program.
--   Returns the final state + use map on success, or a list of errors otherwise.
checkProgmWithUses :: Path -> Program -> [ImportEnv] -> Either [ErrorKind] (CheckState, Map Position VarId)
checkProgmWithUses p (decls, stmts) envs = case getPackageName p decls of
    Left errors -> Left errors
    Right packageName ->
        let initState = CheckState {
                depth = 0,
                varCounter = 0,
                scopeCounter = 1,
                ctrlStack = [],
                scope = [Scope { scopeId = 0, sVars = Map.empty, sFuncs = Map.empty }],
                classScope = []
            }

            initCtx = Ctx { st = initState, errs = [], varUses = Map.empty }
            (_, finalCtx) = runState (checkStmts p packageName envs stmts) initCtx
            finalErrs = checkTopLevelMemberNameConflicts p stmts ++ reverse (errs finalCtx)
        in if null finalErrs then Right (st finalCtx, varUses finalCtx) else Left finalErrs


-- | Run context checking for a whole program.
--   Returns the final state on success, or a list of errors otherwise.
checkProgm :: Path -> Program -> [ImportEnv] -> Either [ErrorKind] CheckState
checkProgm p prog envs = fmap fst (checkProgmWithUses p prog envs)




