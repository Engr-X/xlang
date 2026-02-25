{-# LANGUAGE DuplicateRecordFields #-}

module Semantic.TypeCheck where

import Control.Monad (when, void)
import Control.Monad.State.Strict (State, get, modify, put, runState)
import Data.Map.Strict (Map)
import Data.List (find, intercalate)
import Data.Maybe (listToMaybe, mapMaybe, isNothing)
import Data.Foldable (for_)
import Parse.SyntaxTree (Block(..), Class(..), Command(..), Expression(..), Operator(..), Program, Statement(..), SwitchCase(..), exprTokens, prettyClass, prettyExpr)
import Parse.ParserBasic (AccessModified(..), DeclFlags, Decl)
import Semantic.NameEnv (CheckState(..), CtrlState(..), ImportEnv, QName, Scope(..), VarId, defineLocalVar, getPackageName, lookupVarId)
import Semantic.ContextCheck (Ctx)
import Semantic.OpInfer (augAssignOp, iCast, inferBinaryOp, inferUnaryOp, isBasicType, widenedArgs)
import Semantic.TypeEnv (FullVarTable(..), FullFunctionTable(..), FunSig(..), FunTable, TypedImportEnv(..), VarTable, normalizeClass)
import Util.Exception (ErrorKind, Warning(..), staticCastError)
import Util.Type (Path, Position)

import qualified Semantic.ContextCheck as CC
import qualified Data.Map.Strict as Map
import qualified Lex.Token as Lex
import qualified Util.Exception as UE


-- | Class member typing definition (maintained by caller).
data ClassDef = ClassDef {
    className :: String,
    varTypes :: VarTable,
    funTypes :: FunTable
} deriving (Show)


-- | Type checking context (context state + type tables + diagnostics).
--   tcCtx: ContextCheck state + use map for resolving VarId by position.
--   tcVarTypes: mapping from VarId to inferred/declared types + def positions.
--   tcFunScopes: stack of function scopes (top = current).
--   tcClassStack: current class definitions (top = nearest).
--   tcClassTypeStack: current class types (top = nearest).
--   tcCurrentReturn: expected return type of the current function.
--   tcErrors/tcWarnings: accumulated diagnostics.
data TypeCtx = TypeCtx {
    tcCtx :: CC.Ctx,
    tcVarTypes :: VarTable,
    tcVarFlags :: Map VarId DeclFlags,
    tcFunScopes :: [FunTable],
    tcClassStack :: [ClassDef],
    tcClassTypeStack :: [Class],
    tcCurrentReturn :: Maybe Class,
    
    tcErrors :: [ErrorKind],
    tcWarnings :: [Warning],
    tcFullVarUses :: Map [Position] FullVarTable,
    tcFullVarUsesList :: [([Position], FullVarTable)],
    tcFullFunUses :: Map [Position] FullFunctionTable
} deriving (Show)


-- | Type checker state monad.
type TypeM a = State TypeCtx a


-- | Append a type error to the current context.
addErr :: ErrorKind -> TypeM ()
addErr e = modify $ \c -> c { tcErrors = e : tcErrors c }

-- | Append a type error and return ErrorClass.
errClass :: ErrorKind -> TypeM Class
errClass e = addErr e >> pure ErrorClass


-- | Append a warning to the current context.
addWarn :: Warning -> TypeM ()
addWarn w = modify $ \c -> c { tcWarnings = w : tcWarnings c }

-- | Record a resolved variable use.
recordVarUse :: [Position] -> FullVarTable -> TypeM ()
recordVarUse pos entry =
    modify $ \c -> c {
        tcFullVarUses = Map.insert pos entry (tcFullVarUses c),
        tcFullVarUsesList = (pos, entry) : tcFullVarUsesList c
    }

-- | Record a resolved function call.
recordFunUse :: [Position] -> FullFunctionTable -> TypeM ()
recordFunUse pos entry =
    modify $ \c -> c { tcFullFunUses = Map.insert pos entry (tcFullFunUses c) }

-- | Default declaration flags (no keywords parsed yet).
-- TODO: populate from parsed modifiers once supported.
defaultDeclFlags :: DeclFlags
defaultDeclFlags = []

-- | Default declaration info for locals/functions.
-- TODO: use parsed access/flags once supported.
defaultDecl :: Decl
defaultDecl = (Public, defaultDeclFlags)

-- | Compute flags for a newly defined variable at the current depth.
newVarFlags :: CheckState -> DeclFlags
newVarFlags _ =
    -- TODO: apply parsed modifiers (e.g. static/final) when available.
    []

-- | Ensure a VarId has flags; if missing, insert defaults based on current state.
ensureVarFlags :: VarId -> CheckState -> TypeM DeclFlags
ensureVarFlags vid st = do
    c <- get
    case Map.lookup vid (tcVarFlags c) of
        Just flags -> pure flags
        Nothing -> do
            let flags = newVarFlags st
            modify $ \c0 -> c0 { tcVarFlags = Map.insert vid flags (tcVarFlags c0) }
            pure flags

-- | Record a resolved local variable use with flags.
recordVarUseLocal :: Position -> String -> VarId -> TypeM ()
recordVarUseLocal pos name vid = do
    c <- get
    let flags = Map.findWithDefault [] vid (tcVarFlags c)
        decl = (Public, flags) -- TODO: use parsed access/flags
    recordVarUse [pos] (VarLocal decl name vid)


-- | Check type compatibility and emit implicit-cast warnings when allowed.
checkTypeCompat :: Path -> [Position] -> Class -> Class -> TypeM ()
checkTypeCompat path pos expected actual = do
    let expected' = normalizeClass expected
        actual' = normalizeClass actual
    if actual' == ErrorClass || expected' == ErrorClass || expected' == actual' then
        pure ()
    else if expected' == Void || actual' == Void then
        addErr $ UE.Syntax $ UE.makeError path pos (UE.typeMismatchMsg (prettyClass expected') (prettyClass actual'))
    else if isBasicType expected' && isBasicType actual' then
        mapM_ addWarn (iCast path pos actual' expected')
    else if isBasicType expected' /= isBasicType actual' then
        addErr $ UE.Syntax $ UE.makeError path pos (staticCastError (prettyClass actual') (prettyClass expected'))
    else
        addErr $ UE.Syntax $ UE.makeError path pos (UE.typeMismatchMsg (prettyClass expected') (prettyClass actual'))

-- | Check condition expression type (must be bool).
checkCondBool :: Path -> [Position] -> Class -> TypeM ()
checkCondBool path pos actual = do
    let actual' = normalizeClass actual
    if actual' == ErrorClass || actual' == Bool then
        pure ()
    else
        addErr $ UE.Syntax $ UE.makeError path pos (UE.conditionBoolMsg (prettyClass actual'))


-- | Create a new lexical scope for the duration of an action, then restore depth/scope.
withScope :: TypeM a -> TypeM a
withScope action = do
    c <- get
    let cctx = tcCtx c
        cState = CC.st cctx
        depth0 = depth cState
        newScope = Scope { scopeId = scopeCounter cState, sVars = Map.empty, sFuncs = Map.empty }
        cState' = cState {
            depth = succ depth0,
            scopeCounter = succ $ scopeCounter cState,
            scope = newScope : scope cState
        }
    -- Push a new fun-scope frame for shadowing.
    put $ c {
        tcCtx = cctx { CC.st = cState' },
        tcFunScopes = Map.empty : tcFunScopes c}

    res <- action

    c2 <- get
    let cctx2 = tcCtx c2
        cState2 = CC.st cctx2
        scope' = tail $ scope cState2
        funScopes' = case tcFunScopes c2 of
            [] -> error "BUG: tcFunScopes underflow"
            (_:rest) -> rest

    put $ c2 {
        tcCtx = cctx2 { CC.st = cState2 { depth = depth0, scope = scope' } },
        tcFunScopes = funScopes'}
    pure res


-- | Resolve `this` usage by returning the current class type.
inferThis :: Path -> Position -> TypeM Class
inferThis path pos = do
    c <- get
    case tcClassTypeStack c of
        (cls:_) -> pure cls
        [] -> errClass $ UE.Syntax $ UE.makeError path [pos] (UE.undefinedIdentity "this")
                

-- | Resolve `this.field` in the nearest class scope (placeholder types).
inferThisField :: Path -> [Position] -> String -> TypeM Class
inferThisField path pos field = do
    c <- get
    let cState = CC.st (tcCtx c)
    case tcClassStack c of
        [] -> errClass $ UE.Syntax $ UE.makeError path pos (UE.undefinedIdentity "this")
        (ClassDef _ classVars _ : _) ->
            case classScope cState of
                [] -> errClass $ UE.Syntax $ UE.makeError path pos (UE.undefinedIdentity field)
                (clsTop:_) ->
                    let mType = Map.lookup field (sVars clsTop) >>= (\(vid, _) -> Map.lookup vid classVars) in do
                        when (isNothing mType) $
                            addErr $ UE.Syntax $ UE.makeError path pos (UE.undefinedIdentity field)
                        pure $ maybe ErrorClass fst mType


-- | Infer the type of an expression.
--   p: file path for diagnostics.
--   pkg: current package name.
--   envs: typed import environments (can be empty in v1).
--   expr: expression to infer.
inferExpr :: Path -> QName -> [TypedImportEnv] -> Expression -> TypeM Class
inferExpr _ _ _ (Error _ _) = error "this error should be catched in parser state"
inferExpr p _ _ e@(IntConst _ _) = inferLiteral p e
inferExpr p _ _ e@(LongConst _ _) = inferLiteral p e
inferExpr p _ _ e@(FloatConst _ _) = inferLiteral p e
inferExpr p _ _ e@(DoubleConst _ _) = inferLiteral p e
inferExpr p _ _ e@(LongDoubleConst _ _) = inferLiteral p e
inferExpr p _ _ e@(CharConst _ _) = inferLiteral p e
inferExpr p _ _ e@(BoolConst _ _) = inferLiteral p e
inferExpr p _ _ e@(StringConst _ _) = inferLiteral p e

inferExpr path packages envs (Variable str tok) = do
    c <- get
    let pos = Lex.tokenPos tok
    let TypeCtx {tcVarTypes = vts} = c

    -- 变量为 this 时，解析为当前类实例。
    if str == "this" then inferThis path pos
    else do
        let lookupByVid vid = fmap fst (Map.lookup vid vts)
        let lookupByName = do
                (vid, _) <- lookupVarId str (CC.st (tcCtx c))
                cls <- fmap fst (Map.lookup vid vts)
                pure (vid, cls)
        let mVidAtPos = getVarId pos (tcCtx c)

        -- 优先通过 ContextCheck 记录的 VarId 查找本地变量。
        case mVidAtPos >>= lookupByVid of
            Just t -> do
                for_ mVidAtPos (recordVarUseLocal pos str)
                pure t
            Nothing -> do
                -- 兜底：按当前 TypeCheck 作用域用名字解析一次，避免 VarId 不一致导致崩溃。
                case lookupByName of
                    Just (vid, t) -> do
                        recordVarUseLocal pos str vid
                        pure t
                    Nothing -> do
                        let qn = packages ++ [str]

                        -- 若本地变量不存在，再按包/导入的限定名查找。
                        case listToMaybe $ mapMaybe (Map.lookup qn . tVars) envs of
                            -- 在导入环境中找到变量类型。
                            Just (t, _) -> do
                                recordVarUse [pos] (VarImported defaultDeclFlags t ["toDO import"])
                                pure t

                            -- 理论上应在 ContextCheck 阶段报错。
                            Nothing -> error "should already be caught in ContextCheck (undefined variable)."

inferExpr path _ envs (Qualified names toks) = do
    c <- get
    let posAll = map Lex.tokenPos toks
    let posHead = head posAll
    let TypeCtx { tcVarTypes = vts } = c
    case names of
        -- 只有 `this` -> 解析为当前类的类型。
        ["this"] -> inferThis path posHead

        -- `this.field`（或更深）-> 解析为实例成员。
        -- TODO
        ("this":field:_) -> inferThisField path posAll field

        -- 1) 先按导入/包的限定名查找。
        _ -> case getImportedVarType names envs of
            Just t -> do
                recordVarUse posAll (VarImported defaultDeclFlags t ["toDO import"])
                pure t

            -- 2) 否则把头部当变量，再处理成员访问。
            Nothing -> do
                let headName = head names
                case getVarId posHead (tcCtx c) of
                    -- 头部变量不存在 -> 未定义。
                    Nothing -> errClass $ UE.Syntax $ UE.makeError path posAll (UE.undefinedIdentity headName)

                    Just vid -> do
                        recordVarUseLocal posHead headName vid
                        case Map.lookup vid vts of
                            -- VarId 存在但没有类型信息 -> 内部错误。
                            Nothing -> error "this variable must is record in context check part"

                            -- TODO
                            -- 变量的成员访问暂未支持。
                            Just _ -> errClass $ UE.Syntax $ UE.makeError path posAll UE.unsupportedErrorMsg


inferExpr path packages envs e@(Cast (cls, _) innerE _) = do
    -- recursively infer inner expression
    fromT <- inferExpr path packages envs innerE

    let toT = normalizeClass cls
        pos = map Lex.tokenPos (exprTokens e)

    -- one is basic, the other is not
    if isBasicType fromT /= isBasicType toT then do
        addErr $ UE.Syntax $ UE.makeError path pos $ staticCastError (prettyClass fromT) (prettyClass toT)

    -- both are non-basic (class / array / user type)
    else do
        if isBasicType fromT
            then pure ()
            else error "TODO: class cast is not supported yet: "

    -- result type of cast is always target type
    pure toT

inferExpr path packages envs e@(Unary op innerE _) = do
    t0 <- inferExpr path packages envs innerE
    let pos = map Lex.tokenPos (exprTokens e)
    if isBasicType t0 then
        let resT = inferUnaryOp op t0 in do
            mapM_ addWarn (iCast path pos t0 resT)
            pure resT
    else error "TODO: unary on non-basic type"


inferExpr path packages envs e@(Binary Assign lhs rhs _) = do
    tRhs <- inferExpr path packages envs rhs
    let tRhsN = normalizeClass tRhs
    case lhs of
        Variable name tok -> do
            c <- get
            let cctx = tcCtx c
                cState = CC.st cctx
                posTok = Lex.tokenPos tok
            case defineLocalVar path e cState of
                Left err -> addErr err
                Right cState' -> do
                    let varTypes0 = tcVarTypes c
                        mByName = lookupVarId name cState'
                        mByPos = getVarId posTok (tcCtx c)
                        (cState'', mVid, defPos) = case mByName of
                            Just (vid, pos) -> (cState', Just vid, pos)
                            Nothing -> case mByPos of
                                Just vid -> (cState', Just vid, posTok)
                                Nothing -> case scope cState' of
                                    [] -> (cState', Nothing, posTok)
                                    (sc:rest) ->
                                        let vid = varCounter cState'
                                            sc' = sc { sVars = Map.insert name (vid, posTok) (sVars sc) }
                                            cStateNew = cState' { varCounter = succ vid, scope = sc' : rest }
                                        in (cStateNew, Just vid, posTok)
                    case mVid of
                        Just vid -> do
                            _ <- ensureVarFlags vid cState''
                            recordVarUseLocal posTok name vid
                            case Map.lookup vid varTypes0 of
                                Just (tLhs, _) -> do
                                    checkTypeCompat path (map Lex.tokenPos (exprTokens e)) tLhs tRhsN
                                    modify $ \c0 -> c0 { tcCtx = (tcCtx c0) { CC.st = cState'' } }
                                Nothing -> do
                                    let varTypes' = Map.insert vid (tRhsN, defPos) varTypes0
                                    modify $ \c0 -> c0 { tcCtx = cctx { CC.st = cState'' }, tcVarTypes = varTypes' }
                        Nothing ->
                            put $ c { tcCtx = cctx { CC.st = cState'' } }
        Qualified _ toks ->
            addErr $ UE.Syntax $ UE.makeError path (map Lex.tokenPos toks) UE.assignErrorMsg
        _ ->
            addErr $ UE.Syntax $ UE.makeError path (map Lex.tokenPos (exprTokens lhs)) (UE.cannotAssignMsg (prettyExpr 0 (Just lhs)))
    pure tRhsN


inferExpr path packages envs e@(Binary op lhs rhs _)
    | Just baseOp <- augAssignOp op = do
        tRhs <- inferExpr path packages envs rhs
        tLhs <- case lhs of
            Variable {} -> inferExpr path packages envs lhs
            Qualified _ toks -> do
                addErr $ UE.Syntax $ UE.makeError path (map Lex.tokenPos toks) UE.assignErrorMsg
                pure ErrorClass
            _ -> do
                addErr $ UE.Syntax $ UE.makeError path (map Lex.tokenPos (exprTokens lhs)) (UE.cannotAssignMsg (prettyExpr 0 (Just lhs)))
                pure ErrorClass
        let tLhsN = normalizeClass tLhs
            tRhsN = normalizeClass tRhs
            pos = map Lex.tokenPos (exprTokens e)
        if tLhsN == ErrorClass || tRhsN == ErrorClass then
            pure ErrorClass
        else if isBasicType tLhsN && isBasicType tRhsN then
            let resT = inferBinaryOp baseOp tLhsN tRhsN in do
                mapM_ addWarn (iCast path pos tLhsN resT)
                mapM_ addWarn (iCast path pos tRhsN resT)
                checkTypeCompat path pos tLhsN resT
                pure tLhsN
        else
            error "TODO: binary on non-basic type"


inferExpr path packages envs e@(Binary op e1 e2 _) = do
    t1 <- inferExpr path packages envs e1
    t2 <- inferExpr path packages envs e2
    let pos = map Lex.tokenPos (exprTokens e)
    if isBasicType t1 && isBasicType t2 then
        let resT = inferBinaryOp op t1 t2 in do
            mapM_ addWarn (iCast path pos t1 resT)
            mapM_ addWarn (iCast path pos t2 resT)
            pure resT
    else error "TODO: binary on non-basic type"

inferExpr path packages envs e@(Call callee args) = do
    c <- get
    let TypeCtx{tcFunScopes = fScopes} = c
    let callPos = map Lex.tokenPos (exprTokens e)

    -- 推导实参类型与位置。
    argTs0 <- mapM (inferExpr path packages envs) args
    let argInfos = zip argTs0 (map (map Lex.tokenPos . exprTokens) args)

    case callee of
        -- 直接调用函数名 f(...)
        Variable name tok -> do
            let qLocal = [name]
            let qImport = packages ++ [name]
            let importSigs = concatMap (maybe [] fst . Map.lookup qImport . tFuncs) envs
            resolveScopedCall qLocal name fScopes importSigs [Lex.tokenPos tok] callPos argInfos

        -- 调用限定名（可能是 this.xxx 或导入函数）。
        Qualified names toks -> case names of
            -- this.xxx(...) -> 从当前类方法表查找。
            ("this":fname:_) -> case tcClassStack c of
                [] -> errClass $ UE.Syntax $ UE.makeError path callPos (UE.undefinedIdentity "this")
                (ClassDef _ _ classFuns : _) -> do
                    let sigs = Map.findWithDefault (error "this error should be catched in COntextCheck") [fname] classFuns
                    let namePoses = map Lex.tokenPos toks
                    applyMatches callPos namePoses fname argInfos (FunLocal defaultDecl fname) sigs

            -- 其他限定名 -> 只从导入/包中查找。
            _ -> do
                let sigs = concatMap (maybe [] fst . Map.lookup names . tFuncs) envs
                let namePoses = map Lex.tokenPos toks
                applyMatches callPos namePoses (intercalate "." names) argInfos (FunImported defaultDeclFlags ["toDO import"]) sigs

        -- 其他表达式作为函数名 -> 非法调用。
        _ -> errClass $ UE.Syntax $ UE.makeError path callPos UE.invalidFunctionName
    where
        -- 统一处理匹配与报错逻辑（使用 widenedArgs）。
        applyMatches :: [Position] -> [Position] -> String -> [(Class, [Position])] -> (FunSig -> FullFunctionTable) -> [FunSig] -> TypeM Class
        applyMatches callPos _ funName _ _ [] =
            errClass $ UE.Syntax $ UE.makeError path callPos (UE.undefinedIdentity funName)
        applyMatches callPos namePoses _ argInfos mkEntry sigs@(_ : _) = do
            res <- matchInSigs sigs callPos argInfos
            case res of
                Left err -> errClass err
                Right sig -> do
                    recordFunUse namePoses (mkEntry sig)
                    pure $ funReturn sig

        addWarns = mapM_ addWarn

        -- | Resolve a call by searching scopes from inner to outer.
        --   If a scope has the name but no matching signature, fall through.
        --   Ambiguous matches are reported immediately.
        resolveScopedCall ::
            QName ->
            String ->
            [FunTable] ->
            [FunSig] ->
            [Position] ->
            [Position] ->
            [(Class, [Position])] ->
            TypeM Class
        resolveScopedCall qname funName scopes importSigs namePos callPos argInfos =
            go scopes Nothing
            where
                go [] lastErr = case importSigs of
                    [] -> case lastErr of
                        Just err -> errClass err
                        Nothing -> errClass $ UE.Syntax $ UE.makeError path callPos (UE.undefinedIdentity funName)
                    _ -> do
                        res <- matchInSigs importSigs callPos argInfos
                        case res of
                            Right sig -> do
                                recordFunUse namePos (FunImported defaultDeclFlags ["toDO import"] sig)
                                pure $ funReturn sig
                            Left err -> errClass err
                go (m:rest) lastErr = do
                    let sigs = lookupFun qname [m]
                    if null sigs then
                        go rest lastErr
                    else do
                        res <- matchInSigs sigs callPos argInfos
                        case res of
                            Right sig -> do
                                recordFunUse namePos (FunLocal defaultDecl funName sig)
                                pure $ funReturn sig
                            Left err ->
                                if isAmbiguous err then errClass err
                                else go rest (Just err)

        matchInSigs ::
            [FunSig] ->
            [Position] ->
            [(Class, [Position])] ->
            TypeM (Either ErrorKind FunSig)
        matchInSigs sigs callPos argInfos =
            case widenedArgs path callPos argInfos sigs of
                Left err -> pure (Left err)
                Right (sig, warns) -> do
                    addWarns warns
                    pure (Right sig)

        isAmbiguous :: ErrorKind -> Bool
        isAmbiguous (UE.Syntax be) = UE.why be == UE.ambiguousCallMsg
        isAmbiguous _ = False

inferExpr path _ _ e@(CallT {}) =
    errClass $ UE.Syntax $ UE.makeError path (map Lex.tokenPos (exprTokens e)) UE.unsupportedErrorMsg


-- | Infer literal types.
inferLiteral :: Path -> Expression -> TypeM Class
inferLiteral _ e = case e of
    IntConst _ _ -> pure Int32T
    LongConst _ _ -> pure Int64T
    FloatConst _ _ -> pure Float32T
    DoubleConst _ _ -> pure Float64T
    LongDoubleConst _ _ -> pure Float128T
    CharConst _ _ -> pure Char
    BoolConst _ _ -> pure Bool
    StringConst _ _ -> error "string type is not supported"
    _ -> error "inferLiteral: non-literal expression"


-- | Lookup a variable id (and its position) from the current scope stack.
--   The nearest scope wins.
getVarId :: Position -> Ctx -> Maybe VarId
getVarId pos context = Map.lookup pos (CC.varUses context)


-- | Lookup a variable type from imported environments by qualified name.
getImportedVarType :: QName -> [TypedImportEnv] -> Maybe Class
getImportedVarType qname = fmap fst . listToMaybe . mapMaybe (Map.lookup qname . tVars)


-- | Lookup a function overload set with shadowing.
--   The nearest scope containing the name wins.
lookupFun :: QName -> [FunTable] -> [FunSig]
lookupFun qname scopes = case find (Map.member qname) scopes of
    Just m -> Map.findWithDefault [] qname m
    Nothing -> []


-- | Infer types in an optional block, creating a new scope if present.
inferOptBlock :: Path -> QName -> [TypedImportEnv] -> Maybe Block -> TypeM ()
inferOptBlock _ _ _ Nothing = pure ()
inferOptBlock path packages envs (Just b) = withScope $ inferBlock path packages envs b


-- | Infer types in a statement.
inferStmt :: Path -> QName -> [TypedImportEnv] -> Statement -> TypeM ()
inferStmt _ _ _ (Command Continue _) = pure ()
inferStmt _ _ _ (Command Break _) = pure ()
inferStmt path _ _ (Command (Return Nothing) tok) = do
    c <- get
    case tcCurrentReturn c of
        Just Void -> pure ()
        Just expected ->
            addErr $ UE.Syntax $ UE.makeError path [Lex.tokenPos tok]
                (UE.typeMismatchMsg (prettyClass expected) (prettyClass Void))
        Nothing -> pure ()
        
inferStmt path packages envs (Command (Return (Just e)) _) = do
    t <- inferExpr path packages envs e
    c <- get
    let pos = map Lex.tokenPos (exprTokens e)
    case tcCurrentReturn c of
        Nothing -> pure ()
        Just expected -> checkTypeCompat path pos expected t


inferStmt path packages envs (Expr e) = void $ inferExpr path packages envs e
inferStmt path packages envs (BlockStmt block) = withScope $ inferBlock path packages envs block

inferStmt path packages envs (If e ifBlock elseBlock _) = do
    t <- inferExpr path packages envs e
    checkCondBool path (map Lex.tokenPos (exprTokens e)) t
    inferOptBlock path packages envs ifBlock
    inferOptBlock path packages envs elseBlock

inferStmt path packages envs (For (e1, e2, e3) forBlock _) = do
    withScope $ do
        case e1 of
            Nothing -> pure ()
            Just initExpr@(Binary Assign _ _ _) ->
                void (inferExpr path packages envs initExpr)
            Just initExpr -> do
                addErr $ UE.Syntax $ UE.makeError path (map Lex.tokenPos (exprTokens initExpr)) UE.forInitAssignMsg
                void (inferExpr path packages envs initExpr)
        case e2 of
            Nothing -> pure ()
            Just condExpr -> do
                t <- inferExpr path packages envs condExpr
                checkCondBool path (map Lex.tokenPos (exprTokens condExpr)) t
        maybe (pure ()) (void . inferExpr path packages envs) e3
        inferOptBlock path packages envs forBlock

inferStmt path packages envs (While e whileBlock elseBlock _) = do
    t <- inferExpr path packages envs e
    checkCondBool path (map Lex.tokenPos (exprTokens e)) t
    inferOptBlock path packages envs whileBlock
    inferOptBlock path packages envs elseBlock

inferStmt path packages envs (DoWhile whileBlock e elseBlock _) = do
    inferOptBlock path packages envs whileBlock
    t <- inferExpr path packages envs e
    checkCondBool path (map Lex.tokenPos (exprTokens e)) t
    inferOptBlock path packages envs elseBlock

inferStmt path packages envs (Switch e scs _) = do
    void $ inferExpr path packages envs e
    mapM_ (inferSwitchCase path packages envs) scs

inferStmt path packages envs (Function (retT, _) _ params body) = do
    c <- get
    let cctx = tcCtx c
        cState = CC.st cctx
        depth0 = depth cState
        ret0 = tcCurrentReturn c
        (varCounter', paramVars, paramTypes, paramFlags) = foldl addParam (varCounter cState, Map.empty, Map.empty, Map.empty) params

        newScope = Scope { scopeId = scopeCounter cState, sVars = paramVars, sFuncs = Map.empty }
        cState' = cState {
            depth = succ depth0,
            varCounter = varCounter',
            scopeCounter = succ $ scopeCounter cState,
            ctrlStack = InFunction : ctrlStack cState,
            scope = newScope : scope cState
        }

        -- Param bindings shadow outer bindings.
        varTypes' = Map.union paramTypes (tcVarTypes c)
        varFlags' = Map.union paramFlags (tcVarFlags c)
        funScopes' = Map.empty : tcFunScopes c

    put $ c {
        tcCtx = cctx { CC.st = cState' },
        tcVarTypes = varTypes',
        tcVarFlags = varFlags',
        tcFunScopes = funScopes',
        tcCurrentReturn = Just (normalizeClass retT)
    }

    mapM_ (\(name, (vid, pos)) -> recordVarUseLocal pos name vid) (Map.toList paramVars)

    inferBlock path packages envs body

    c2 <- get
    let cctx2 = tcCtx c2
        cState2 = CC.st cctx2
        scope' = tail $ scope cState2
        ctrls' = tail $ ctrlStack cState2
        funScopes2 = case tcFunScopes c2 of
            [] -> []
            (_:rest) -> rest
    put $ c2 {
        tcCtx = cctx2 { CC.st = cState2 { depth = depth0, scope = scope', ctrlStack = ctrls' } },
        tcFunScopes = funScopes2,
        tcCurrentReturn = ret0
    }
    where
        addParam :: (VarId, Map.Map String (VarId, Position), VarTable, Map.Map VarId DeclFlags) ->
            (Class, String, [Lex.Token]) ->
            (VarId, Map.Map String (VarId, Position), VarTable, Map.Map VarId DeclFlags)
        addParam (vc, m, vt, vf) (t, name, toks) = case toks of
            [] -> (vc, m, vt, vf)
            (tok:_) ->
                let vid = vc
                    pos = Lex.tokenPos tok
                    m'  = Map.insert name (vid, pos) m
                    vt' = Map.insert vid (normalizeClass t, pos) vt
                    vf' = Map.insert vid [] vf
                in (succ vc, m', vt', vf')


inferStmt _ _ _ (FunctionT {}) = do
    error "TODO for template"


-- | Infer types in a single switch case.
inferSwitchCase :: Path -> QName -> [TypedImportEnv] -> SwitchCase -> TypeM ()
inferSwitchCase path packages envs (Case e mb _) = do
    void (inferExpr path packages envs e)
    inferOptBlock path packages envs mb
inferSwitchCase path packages envs (Default b _) =
    inferOptBlock path packages envs (Just b)


-- | Infer types in a block.
inferBlock :: Path -> QName -> [TypedImportEnv] -> Block -> TypeM ()
inferBlock path package envs (Multiple ss) = inferStmts path package envs ss


-- | Infer types in a list of statements, preloading function signatures first.
inferStmts :: Path -> QName -> [TypedImportEnv] -> [Statement] -> TypeM ()
inferStmts path package envs stmts = do
    let funDefs = filter isFunctionStmt stmts
    mapM_ preloadFun funDefs
    mapM_ (inferStmt path package envs) stmts
    where
        isFunctionStmt :: Statement -> Bool
        isFunctionStmt Function {} = True
        isFunctionStmt FunctionT {} = True
        isFunctionStmt _ = False

        preloadFun :: Statement -> TypeM ()
        preloadFun stmt = case stmt of
            Function (retT, _) name params _ ->
                addFunSig name params retT Nothing
            FunctionT (retT, _) name _ params _ ->
                addFunSig name params retT Nothing
            _ -> pure ()

        addFunSig :: Expression -> [(Class, String, [Lex.Token])] -> Class -> Maybe [(Class, [Lex.Token])] -> TypeM ()
        addFunSig name params retT mTParams = case name of
            Variable s tok -> do
                let sig = FunSig {
                        funParams = map (\(t, _, _) -> normalizeClass t) params,
                        funReturn = normalizeClass retT
                    }
                let sigText = prettySig s sig params mTParams
                let pos = Lex.tokenPos tok
                insertSig [s] sig sigText pos

            Qualified _ toks ->
                addErr $ UE.Syntax $ UE.makeError path (map Lex.tokenPos toks) UE.unsupportedErrorMsg

            _ -> pure ()

        insertSig :: QName -> FunSig -> String -> Position -> TypeM ()
        insertSig qname sig sigText pos = do
            c <- get
            case tcFunScopes c of
                [] -> error "BUG: tcFunScopes is empty (missing initial frame)"
                (fts:rest) -> do
                    let (fts', errs) = addFun qname sig sigText pos fts
                    put $ c { tcFunScopes = fts' : rest }
                    mapM_ addErr errs


        addFun :: QName -> FunSig -> String -> Position -> FunTable -> (FunTable, [ErrorKind])
        addFun name sig sigText pos fts = case Map.lookup name fts of
            Nothing -> (Map.insert name [sig] fts, [])
            Just sigsOld ->
                if any (sameArgs sig) sigsOld
                    then (fts, [UE.Syntax (UE.makeError path [pos] (UE.duplicateMethodMsg sigText))])
                    else (Map.insert name (sig : sigsOld) fts, [])
            where
                sameArgs :: FunSig -> FunSig -> Bool
                sameArgs a b = funParams a == funParams b

        prettySig :: String -> FunSig -> [(Class, String, [Lex.Token])] -> Maybe [(Class, [Lex.Token])] -> String
        prettySig name sig params mTParams =
            let retS = prettyClass (funReturn sig)
                paramS = intercalate ", " [prettyClass t ++ " " ++ n | (t, n, _) <- params]
                genS = case mTParams of
                    Nothing -> ""
                    Just ts -> "<" ++ intercalate ", " (map (prettyClass . fst) ts) ++ ">"
            in concat [retS, " ", name, genS, "(", paramS, ")"]


-- | Run type checking for a whole program.
--   Requires context checking results and typed import environments.
inferProgm :: Path -> Program -> [ImportEnv] -> [TypedImportEnv] -> Either [ErrorKind] TypeCtx
inferProgm path prog@(decls, stmts) importEnvs typedEnvs = do
    packageName <- getPackageName path decls
    case CC.checkProgmWithUses path prog importEnvs of
        Left errs -> Left errs
        Right (st, uses) -> inferProgmWithCtx path packageName stmts st uses typedEnvs


-- | Run type checking given a prepared context check state.
inferProgmWithCtx ::
    Path ->
    QName ->
    [Statement] ->
    CheckState ->
    Map.Map Position VarId ->
    [TypedImportEnv] ->
    Either [ErrorKind] TypeCtx
inferProgmWithCtx path packageName stmts st uses typedEnvs =
    let initCtx = TypeCtx {
        tcCtx = CC.Ctx { st = st, errs = [], varUses = uses },
        tcVarTypes = Map.empty,
        tcVarFlags = Map.empty,
        tcFunScopes = [Map.empty],
        tcClassStack = [],
        tcClassTypeStack = [],
        tcCurrentReturn = Nothing,
        tcErrors = [],
        tcWarnings = [],
        tcFullVarUses = Map.empty,
        tcFullVarUsesList = [],
        tcFullFunUses = Map.empty}
        (_, finalCtx0) = runState (inferStmts path packageName typedEnvs stmts) initCtx
        finalErrs = reverse (tcErrors finalCtx0)
        finalWarns = reverse (tcWarnings finalCtx0)
        finalCtx = finalCtx0 { tcErrors = finalErrs, tcWarnings = finalWarns }
    in if null finalErrs
        then Right finalCtx
        else Left finalErrs
