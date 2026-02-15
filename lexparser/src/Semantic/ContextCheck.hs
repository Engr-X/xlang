module Semantic.ContextCheck where

import Semantic.NameEnv
import Control.Monad (when)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Foldable (for_)
import Control.Monad.State.Strict (State, get, put, modify, runState)
import Lex.Token (tokenPos)
import Util.Type (Path, Position)
import Util.Exception (ErrorKind, undefinedIdentity, invalidFunctionName, continueCtrlErrorMsg, breakCtrlErrorMsg, returnCtrlErrorMsg, illegalStatementMsg, cannotAssignMsg)
import Parse.SyntaxTree (Expression, Statement, Block, SwitchCase, Program, exprTokens, stmtTokens)

import qualified Data.Map.Strict as Map
import qualified Parse.SyntaxTree as AST
import qualified Util.Exception as UE


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


-- | Append a new error to the context (keeps existing errors).
addErr :: ErrorKind -> CheckM ()
addErr e = modify $ \c -> c { errs = e : errs c }


-- | Read the current checking state.
getState :: CheckM CheckState
getState = st <$> get


-- | Replace the current checking state.
putState :: CheckState -> CheckM ()
putState s = modify $ \c -> c { st = s }


-- | Predicate: is this statement a function definition?
isFunction :: Statement -> Bool
isFunction AST.Function {} = True
isFunction AST.FunctionT {} = True
isFunction _ = False


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


-- | Convenience wrapper to push both a control context and a new scope.
withCtrlScope :: CtrlState -> CheckM a -> CheckM a
withCtrlScope ctrl action = withCtrl ctrl (withScope action)


-- | Minimal expression traversal; no name/type rules yet.
checkExpr :: Path -> QName -> [ImportEnv] -> Expression -> CheckM ()
checkExpr p packages envs expr = case expr of
    AST.Error _ _ -> error "this error should really filter or catched after parser"
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
                    else addErr $ UE.Syntax $ UE.makeError p [tokenPos tok] (undefinedIdentity name)

    AST.Qualified names tokens -> do
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
                    else addErr $ UE.Syntax $ UE.makeError p (map tokenPos tokens) (undefinedIdentity $ concatQ names)
                    

    -- recurse into children.
    AST.Cast _ e _ -> checkExpr p packages envs e
    AST.Unary _ e _ -> checkExpr p packages envs e

    AST.Binary AST.Assign e1 e2 tok -> do
        checkExpr p packages envs e2

        case e1 of
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

            AST.Qualified {} -> do
                c <- get
                let cState = st c
                case defineLocalVar p (AST.Binary AST.Assign e1 e2 tok) cState of
                    Left err -> addErr err
                    Right cState' -> put $ c { st = cState' }

            _ -> addErr $ UE.Syntax $ UE.makeError p (map tokenPos $ exprTokens e1) (cannotAssignMsg (AST.prettyExpr 0 (Just e1)))

    AST.Binary _ e1 e2 _ -> do
        checkExpr p packages envs e1
        checkExpr p packages envs e2

    -- 调用检查不区分是否为模板调用（CallT 只忽略模板实参）。
    AST.Call callee args -> checkCall callee args
    AST.CallT callee _ args -> checkCall callee args
    where
        checkCall :: Expression -> [Expression] -> CheckM ()
        checkCall callee args = do
            case callee of
                AST.Variable name tok -> do
                    c <- get
                    let cState = st c
                    if isFuncDefine [name] cState || isFunImport (packages ++ [name]) envs then pure ()
                    else addErr $ UE.Syntax $ UE.makeError p [tokenPos tok] (undefinedIdentity name)

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
                        _ -> if isFunImport names envs then pure ()
                             else addErr $ UE.Syntax $ UE.makeError p (map tokenPos tokens) (undefinedIdentity $ concatQ names)

                other -> addErr $ UE.Syntax $ UE.makeError p (map tokenPos $ exprTokens other) invalidFunctionName

            mapM_ (checkExpr p packages envs) args



-- | Continue is valid only inside loops.
isContinueValid :: [CtrlState] -> Bool
isContinueValid = elem InLoop


-- | Break is valid inside loops or switch cases.
isBreakValid :: [CtrlState] -> Bool
isBreakValid ctrls = elem InLoop ctrls || elem InCase ctrls


-- | Return is valid only inside functions.
isReturnValid :: [CtrlState] -> Bool
isReturnValid = elem InFunction


-- | Check a single statement and all of its nested blocks.
checkStmt :: Path -> QName -> [ImportEnv] -> Statement -> CheckM ()
checkStmt p package envs (AST.Command cmd token) = do
    c <- get
    let cState = st c
    let ctrls = ctrlStack cState

    case cmd of
        AST.Continue ->
            if isContinueValid ctrls then pure ()
            else addErr $ UE.Syntax $ UE.makeError p [tokenPos token] continueCtrlErrorMsg

        AST.Break ->
            if isBreakValid ctrls then pure ()
            else addErr $ UE.Syntax $ UE.makeError p [tokenPos token] breakCtrlErrorMsg

        AST.Return mExpr ->
            if isReturnValid ctrls then let checkReturnExpr = maybe (pure ()) (checkExpr p package envs) in checkReturnExpr mExpr
            else addErr $ UE.Syntax $ UE.makeError p [tokenPos token] returnCtrlErrorMsg
checkStmt p package envs (AST.Expr e) = checkExpr p package envs e

-- block
checkStmt p package envs stmt@(AST.BlockStmt block) = do
    c <- get
    let cState = st c
    let parentCtrl = fromMaybe InClass (listToMaybe (ctrlStack cState))
    when (forbiddenFor parentCtrl InBlock) $ addErr $
        UE.Syntax $ UE.makeError p (map tokenPos $ stmtTokens stmt) (illegalStatementMsg (prettyCtrlState InBlock) (prettyCtrlState parentCtrl))
    withCtrlScope InBlock $ checkBlock p package envs block

-- if-else
checkStmt p package envs stmt@(AST.If e ifBlock elseBlock _) = do
    c <- get
    let cState = st c
    let parentCtrl = fromMaybe InClass (listToMaybe (ctrlStack cState))
    when (forbiddenFor parentCtrl InIf) $ addErr $
        UE.Syntax $ UE.makeError p (map tokenPos $ stmtTokens stmt) (illegalStatementMsg (prettyCtrlState InIf) (prettyCtrlState parentCtrl))

    checkExpr p package envs e
    for_ ifBlock (withCtrlScope InIf . checkBlock p package envs)
    for_ elseBlock (withCtrlScope InElse . checkBlock p package envs)

-- for
checkStmt p package envs stmt@(AST.For (e1, e2, e3) forBlock _) = do
    c <- get
    let cState = st c
    let parentCtrl = fromMaybe InClass (listToMaybe (ctrlStack cState))
    when (forbiddenFor parentCtrl InLoop) $ addErr $ UE.Syntax $ UE.makeError p (map tokenPos $ stmtTokens stmt)
        (illegalStatementMsg (prettyCtrlState InLoop) (prettyCtrlState parentCtrl))

    withScope $ do
        for_ e1 (checkExpr p package envs)
        for_ e2 (checkExpr p package envs)
        for_ e3 (checkExpr p package envs)
        for_ forBlock (withCtrlScope InLoop . checkBlock p package envs)

-- while
checkStmt p package envs stmt@(AST.While e whileBlock elseBlock _) = do
    c <- get
    let cState = st c
    let parentCtrl = fromMaybe InClass (listToMaybe (ctrlStack cState))
    when (forbiddenFor parentCtrl InLoop) $ addErr $ UE.Syntax $ UE.makeError p (map tokenPos $ stmtTokens stmt)
        (illegalStatementMsg (prettyCtrlState InLoop) (prettyCtrlState parentCtrl))

    checkExpr p package envs e
    for_ whileBlock (withCtrlScope InLoop . checkBlock p package envs)
    for_ elseBlock (withCtrlScope InElse . checkBlock p package envs)

-- do while
checkStmt p package envs stmt@(AST.DoWhile whileBlock e elseBlock _) = do
    c <- get
    let cState = st c
    let parentCtrl = fromMaybe InClass (listToMaybe (ctrlStack cState))
    when (forbiddenFor parentCtrl InLoop) $ addErr $ UE.Syntax $ UE.makeError p (map tokenPos $ stmtTokens stmt)
        (illegalStatementMsg (prettyCtrlState InLoop) (prettyCtrlState parentCtrl))

    for_ whileBlock (withCtrlScope InLoop . checkBlock p package envs)
    checkExpr p package envs e
    for_ elseBlock (withCtrlScope InElse . checkBlock p package envs)

-- switch
checkStmt p package envs stmt@(AST.Switch e scs _) = do
    c <- get
    let cState = st c
    let parentCtrl = fromMaybe InClass (listToMaybe (ctrlStack cState))
    when (forbiddenFor parentCtrl InSwitch) $ addErr $ UE.Syntax $ UE.makeError p (map tokenPos $ stmtTokens stmt)
        (illegalStatementMsg (prettyCtrlState InSwitch) (prettyCtrlState parentCtrl))

    checkExpr p package envs e
    withCtrl InSwitch $ for_ scs (checkSwitchCase p package envs)


-- function
checkStmt p package envs stmt
    | AST.Function _ _ params body <- stmt =
        checkFunctionStmt stmt params body
    | AST.FunctionT _ _ _ params body <- stmt =
        checkFunctionStmt stmt params body
    where
        -- | Common handler for function-like statements (Function / FunctionT).
        checkFunctionStmt stmt' params body = do
            c <- get
            let cState = st c
            let parentCtrl = fromMaybe InClass (listToMaybe (ctrlStack cState))
            when (forbiddenFor parentCtrl InFunction) $ addErr $ UE.Syntax $ UE.makeError p (map tokenPos $ stmtTokens stmt')
                (illegalStatementMsg (prettyCtrlState InFunction) (prettyCtrlState parentCtrl))

            let depth0 = depth cState
            let (varCounter', paramVars) = foldl addParam (varCounter cState, Map.empty) params
                addParam (vc, m) (_, name, toks) = case toks of
                    [] -> (vc, m)
                    (t:_) -> (succ vc, Map.insert name (vc, tokenPos t) m)

            let newScope = Scope { scopeId = scopeCounter cState, sVars = paramVars, sFuncs = Map.empty }
            let cState' = cState {
                depth = succ depth0,
                varCounter = varCounter',
                scopeCounter = succ $ scopeCounter cState,
                ctrlStack = InFunction : ctrlStack cState,
                scope = newScope : scope cState}
            put $ c { st = cState' }

            checkBlock p package envs body

            c2 <- get
            let cState2 = st c2
            let scope' = tail $ scope cState2
            let ctrls' = tail $ ctrlStack cState2
            put $ c2 { st = cState2 { depth = depth0, scope = scope', ctrlStack = ctrls' } }


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
    let funDefs = filter isFunction stmts
    mapM_ defineOne funDefs
    mapM_ (checkStmt path package envs') stmts
    where
        defineOne :: Statement -> CheckM ()
        defineOne stmt = do
            c <- get
            let cState = st c
            case defineFunc path stmt cState of
                Left err -> addErr err
                Right cState' -> put $ c { st = cState' }
                            
                            
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
            finalErrs = reverse (errs finalCtx)
        in if null finalErrs then Right (st finalCtx, varUses finalCtx) else Left finalErrs


-- | Run context checking for a whole program.
--   Returns the final state on success, or a list of errors otherwise.
checkProgm :: Path -> Program -> [ImportEnv] -> Either [ErrorKind] CheckState
checkProgm p prog envs = fmap fst (checkProgmWithUses p prog envs)
