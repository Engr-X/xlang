{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}

module Semantic.TypeCheck where

import Control.Monad (when, unless, void)
import Control.Monad.State.Strict (State, get, modify, put, runState)
import Data.Map.Strict (Map)
import Data.List (find, intercalate)
import Data.Maybe (listToMaybe, mapMaybe, isNothing, fromMaybe)
import Data.Foldable (for_)
import Parse.SyntaxTree (Block(..), Class(..), Command(..), Expression(..), Operator(..), Program, Statement(..), pattern Function, pattern FunctionT, SwitchCase(..), exprTokens, prettyClass, prettyExpr, promoteTopLevelFunctions, inlineProgramFunctions)
import Parse.ParserBasic (AccessModified(..), DeclFlag(..), DeclFlags, Decl)
import Semantic.NameEnv (CheckState(..), CtrlState(..), ImportEnv, QName, Scope(..), VarId, defineDeclaredVar, defineLocalVar, getPackageName, lookupVarId)
import Semantic.ContextCheck (Ctx)
import Semantic.OpInfer (augAssignOp, binaryOpCastType, iCast, inferBinaryOp, inferUnaryOp, isBasicType, promoteBasicType, widenedArgs)
import Semantic.TypeEnv (FullVarTable(..), FullFunctionTable(..), FunSig(..), FunTable, TypedImportEnv(..), VarTable, defaultTypedImportEnv)
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

-- | Build declaration metadata from flags.
declFromFlags :: DeclFlags -> Decl
declFromFlags flags = (Public, flags)

-- | Default declaration info for locals/functions.
-- TODO: use parsed access/flags once supported.
defaultDecl :: Decl
defaultDecl = declFromFlags defaultDeclFlags


normalizeTypeAlias :: Class -> Class
normalizeTypeAlias cls = case cls of
    Class ["String"] [] -> Class ["String"] []
    Class ["java", "lang", "String"] [] -> Class ["String"] []
    Class ["xlang", "String"] [] -> Class ["String"] []
    Class ["Any"] [] -> Class ["Any"] []
    Class ["xlang", "Any"] [] -> Class ["Any"] []
    Class ["java", "lang", "Object"] [] -> Class ["Any"] []
    Class qn args -> Class qn (map normalizeTypeAlias args)
    other -> other


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

-- | Set flags for a variable id.
setVarFlags :: VarId -> DeclFlags -> TypeM ()
setVarFlags vid flags =
    modify $ \c -> c { tcVarFlags = Map.insert vid flags (tcVarFlags c) }

-- | True if the variable is immutable (`val`).
isVarFinal :: VarId -> TypeM Bool
isVarFinal vid = do
    c <- get
    pure $ Final `elem` Map.findWithDefault [] vid (tcVarFlags c)

-- | Record a resolved local variable use with flags.
recordVarUseLocal :: Position -> String -> VarId -> TypeM ()
recordVarUseLocal pos name vid = do
    c <- get
    let flags = Map.findWithDefault [] vid (tcVarFlags c)
        decl = declFromFlags flags
    recordVarUse [pos] (VarLocal decl name vid)

-- | Check type compatibility and emit implicit-cast warnings when allowed.
checkTypeCompat :: Path -> [Position] -> Class -> Class -> TypeM ()
checkTypeCompat path pos expected actual = do
    let expectedN = normalizeTypeAlias expected
        actualN = normalizeTypeAlias actual
    if actualN == ErrorClass || expectedN == ErrorClass || expectedN == actualN then
        pure ()
    else if expectedN == Bool || expectedN == Void || actualN == Void then
        addErr $ UE.Syntax $ UE.makeError path pos (UE.typeMismatchMsg (prettyClass expectedN) (prettyClass actualN))
    else if isBasicType expectedN && isBasicType actualN then
        mapM_ addWarn (iCast path pos actualN expectedN)
    else if isBasicType expectedN /= isBasicType actualN then
        addErr $ UE.Syntax $ UE.makeError path pos (staticCastError (prettyClass actualN) (prettyClass expectedN))
    else
        addErr $ UE.Syntax $ UE.makeError path pos (UE.typeMismatchMsg (prettyClass expectedN) (prettyClass actualN))

-- | Check condition expression type (must be bool).
checkCondBool :: Path -> [Position] -> Class -> TypeM ()
checkCondBool path pos actual = do
    if actual == ErrorClass || actual == Bool then
        pure ()
    else
        addErr $ UE.Syntax $ UE.makeError path pos (UE.conditionBoolMsg (prettyClass actual))


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

inferExpr path packages envs e@(Ternary cond (thenE, elseE) _) = do
    tCond <- inferExpr path packages envs cond
    checkCondBool path (map Lex.tokenPos (exprTokens cond)) tCond

    tThen <- inferExpr path packages envs thenE
    tElse <- inferExpr path packages envs elseE
    let pos = map Lex.tokenPos (exprTokens e)

    if tThen == ErrorClass || tElse == ErrorClass then
        pure ErrorClass
    else if tThen == tElse then
        pure tThen
    else if isBasicType tThen && isBasicType tElse then do
        let tRes = promoteBasicType tThen tElse
        mapM_ addWarn (iCast path pos tThen tRes)
        mapM_ addWarn (iCast path pos tElse tRes)
        pure tRes
    else
        errClass $ UE.Syntax $ UE.makeError path pos (UE.typeMismatchMsg (prettyClass tThen) (prettyClass tElse))

inferExpr path packages envs (Variable str tok) = do
    c <- get
    let pos = Lex.tokenPos tok
    let TypeCtx {tcVarTypes = vts} = c

    -- ?????this ?????????????????
    if str == "this" then inferThis path pos
    else do
        let lookupByVid vid = fmap fst (Map.lookup vid vts)
        let lookupByName = do
                (vid, _) <- lookupVarId str (CC.st (tcCtx c))
                cls <- fmap fst (Map.lookup vid vts)
                pure (vid, cls)
        let mVidAtPos = getVarId pos (tcCtx c)

        -- ?????? ContextCheck ?????VarId ???????????
        case mVidAtPos >>= lookupByVid of
            Just t -> do
                for_ mVidAtPos (recordVarUseLocal pos str)
                pure t
            Nothing -> do
                -- ????????? TypeCheck ???????????????????? VarId ?????????????
                case lookupByName of
                    Just (vid, t) -> do
                        recordVarUseLocal pos str vid
                        pure t
                    Nothing -> do
                        let qn = packages ++ [str]

                        -- ??????????????????/??????????????
                        case listToMaybe $ mapMaybe (Map.lookup qn . tVars) envs of
                            -- ????????????????????
                            Just (t, _, full) -> do
                                recordVarUse [pos] (VarImported defaultDeclFlags t qn full)
                                pure t

                            -- ????????ContextCheck ????????
                            Nothing ->
                                errClass $ UE.Syntax $ UE.makeError path [pos] (UE.undefinedIdentity str)

inferExpr path _ envs (Qualified names toks) = do
    c <- get
    let posAll = map Lex.tokenPos toks
    let posHead = head posAll
    let TypeCtx { tcVarTypes = vts } = c
    case names of
        -- ??? `this` -> ????????????????
        ["this"] -> inferThis path posHead

        -- `this.field`????????> ?????????????
        -- TODO
        ("this":field:_) -> inferThisField path posAll field

        -- 1) ??????/?????????????
        _ -> case getImportedVarType names envs of
            Just (t, full) -> do
                recordVarUse posAll (VarImported defaultDeclFlags t names full)
                pure t

            -- 2) ??????????????????????????
            Nothing -> do
                let headName = head names
                case getVarId posHead (tcCtx c) of
                    -- ???????????-> ???????
                    Nothing -> errClass $ UE.Syntax $ UE.makeError path posAll (UE.undefinedIdentity headName)

                    Just vid -> do
                        recordVarUseLocal posHead headName vid
                        case Map.lookup vid vts of
                            -- VarId ??????????????-> ????????
                            Nothing -> error "this variable must is record in context check part"

                            -- TODO
                            -- ???????????????????
                            Just _ -> errClass $ UE.Syntax $ UE.makeError path posAll UE.unsupportedErrorMsg


inferExpr path packages envs e@(Cast (cls, _) innerE _) = do
    -- recursively infer inner expression
    fromT <- inferExpr path packages envs innerE

    let toT = cls
        fromTN = normalizeTypeAlias fromT
        toTN = normalizeTypeAlias toT
        pos = map Lex.tokenPos (exprTokens e)

    -- one is basic, the other is not
    if isBasicType fromTN /= isBasicType toTN then do
        addErr $ UE.Syntax $ UE.makeError path pos $ staticCastError (prettyClass fromTN) (prettyClass toTN)

    -- both are non-basic (class / array / user type)
    else do
        if isBasicType fromTN
            then pure ()
            else error "TODO: class cast is not supported yet: "

    -- result type of cast is always target type
    pure toT

inferExpr path packages envs e@(Unary op innerE _) = do
    if isIncDecOperator op then do
        targetOk <- checkIncDecTarget path innerE
        t0 <- inferExpr path packages envs innerE
        let pos = map Lex.tokenPos (exprTokens e)
        if not targetOk || t0 == ErrorClass then
            pure ErrorClass
        else if isIncDecOperandType t0 then
            let resT = inferUnaryOp op t0 in do
                mapM_ addWarn (iCast path pos t0 resT)
                pure resT
        else do
            addErr $ UE.Syntax $ UE.makeError path pos (UE.incDecOperandTypeMsg (prettyClass t0))
            pure ErrorClass
    else do
        t0 <- inferExpr path packages envs innerE
        let pos = map Lex.tokenPos (exprTokens e)
        if isBasicType t0 then
            let resT = inferUnaryOp op t0 in do
                mapM_ addWarn (iCast path pos t0 resT)
                pure resT
        else error "TODO: unary on non-basic type"


inferExpr path packages envs e@(Binary Assign lhs rhs _) = do
    tRhs <- inferExpr path packages envs rhs
    let tRhsN = tRhs
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
                            flags <- ensureVarFlags vid cState''
                            recordVarUseLocal posTok name vid
                            case Map.lookup vid varTypes0 of
                                Just (tLhs, _) ->
                                    if Final `elem` flags
                                        then do
                                            addErr $ UE.Syntax $ UE.makeError path [posTok] (UE.immutableVariableMsg name)
                                            modify $ \c0 -> c0 { tcCtx = (tcCtx c0) { CC.st = cState'' } }
                                        else do
                                            checkTypeCompat path (map Lex.tokenPos (exprTokens e)) tLhs tRhsN
                                            modify $ \c0 -> c0 { tcCtx = (tcCtx c0) { CC.st = cState'' } }
                                Nothing -> do
                                    let varTypes' = Map.insert vid (tRhsN, defPos) varTypes0
                                    modify $ \c0 -> c0 {
                                        tcCtx = cctx { CC.st = cState'' },
                                        tcVarTypes = varTypes'
                                    }
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

        immutableErr <- case lhs of
            Variable name tok -> do
                c <- get
                let posTok = Lex.tokenPos tok
                case resolveLocalVarId name posTok c of
                    Just vid -> do
                        finalVar <- isVarFinal vid
                        pure $ if finalVar
                            then Just $ UE.Syntax $ UE.makeError path [posTok] (UE.immutableVariableMsg name)
                            else Nothing
                    Nothing -> pure Nothing
            _ -> pure Nothing

        case immutableErr of
            Just err -> addErr err >> pure ErrorClass
            Nothing -> do
                let tLhsN = tLhs
                    tRhsN = tRhs
                    pos = map Lex.tokenPos (exprTokens e)
                if tLhsN == ErrorClass || tRhsN == ErrorClass then
                    pure ErrorClass
                else if isBasicType tLhsN && isBasicType tRhsN then
                    let resT = inferBinaryOp baseOp tLhsN tRhsN
                        castT = binaryOpCastType baseOp tLhsN tRhsN in do
                        mapM_ addWarn (iCast path pos tLhsN castT)
                        mapM_ addWarn (iCast path pos tRhsN castT)
                        checkTypeCompat path pos tLhsN resT
                        pure tLhsN
                else
                    error "TODO: binary on non-basic type"


inferExpr path packages envs e@(Binary op e1 e2 _) = do
    t1 <- inferExpr path packages envs e1
    t2 <- inferExpr path packages envs e2
    let pos = map Lex.tokenPos (exprTokens e)
    if isBasicType t1 && isBasicType t2 then
        let resT = inferBinaryOp op t1 t2
            castT = binaryOpCastType op t1 t2 in do
            mapM_ addWarn (iCast path pos t1 castT)
            mapM_ addWarn (iCast path pos t2 castT)
            pure resT
    else error "TODO: binary on non-basic type"

inferExpr path packages envs e@(Call callee args) = do
    c <- get
    let TypeCtx{tcFunScopes = fScopes} = c
    let callPos = map Lex.tokenPos (exprTokens e)

    argTs0 <- mapM (inferExpr path packages envs) args
    let argInfos = zip argTs0 (map (map Lex.tokenPos . exprTokens) args)

    case callee of
        Variable name tok -> do
            let qLocal = [name]
            let qImport = packages ++ [name]
            let importEntries =
                    concatMap
                        (maybe [] (\(sigs, _, full) -> [(qImport, full, sigs)]) . Map.lookup qImport . tFuncs)
                        envs
            resolveScopedCall qLocal name fScopes importEntries [Lex.tokenPos tok] callPos argInfos

        Qualified names toks -> case names of
            ("this":fname:_) -> case tcClassStack c of
                [] -> errClass $ UE.Syntax $ UE.makeError path callPos (UE.undefinedIdentity "this")
                (ClassDef _ _ classFuns : _) -> do
                    let sigs = Map.findWithDefault (error "this error should be catched in COntextCheck") [fname] classFuns
                    let namePoses = map Lex.tokenPos toks
                    applyMatches callPos namePoses fname argInfos (FunLocal defaultDecl [fname]) sigs

            _ -> do
                let importEntries =
                        concatMap
                            (maybe [] (\(sigs', _, full) -> [(names, full, sigs')]) . Map.lookup names . tFuncs)
                            envs
                let namePoses = map Lex.tokenPos toks
                resolveImportedCall names (intercalate "." names) importEntries namePoses callPos argInfos

        _ -> errClass $ UE.Syntax $ UE.makeError path callPos UE.invalidFunctionName
  where
        applyMatches ::
            [Position] ->
            [Position] ->
            String ->
            [(Class, [Position])] ->
            (FunSig -> FullFunctionTable) ->
            [FunSig] ->
            TypeM Class
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

        resolveImportedCall ::
            QName ->
            String ->
            [(QName, QName, [FunSig])] ->
            [Position] ->
            [Position] ->
            [(Class, [Position])] ->
            TypeM Class
        resolveImportedCall usedName funName importEntries namePos callPos argInfos =
            let importSigs = concatMap third importEntries
                findImportNames sig = fmap (\(used, full, _) -> (used, full)) (find (elem sig . third) importEntries)
            in case importSigs of
                [] -> errClass $ UE.Syntax $ UE.makeError path callPos (UE.undefinedIdentity funName)
                _ -> do
                    res <- matchInSigs importSigs callPos argInfos
                    case res of
                        Right sig -> do
                            let (usedQn, fullQn) = fromMaybe (usedName, usedName) (findImportNames sig)
                            recordFunUse namePos (FunImported defaultDeclFlags usedQn fullQn sig)
                            pure $ funReturn sig
                        Left err -> errClass err

        resolveScopedCall ::
            QName ->
            String ->
            [FunTable] ->
            [(QName, QName, [FunSig])] ->
            [Position] ->
            [Position] ->
            [(Class, [Position])] ->
            TypeM Class
        resolveScopedCall qname funName scopes importEntries namePos callPos argInfos =
            go scopes Nothing
          where
            importSigs = concatMap third importEntries
            findImportNames sig = fmap (\(used, full, _) -> (used, full)) (find (elem sig . third) importEntries)
            go [] lastErr = case importSigs of
                [] -> case lastErr of
                    Just err -> errClass err
                    Nothing -> errClass $ UE.Syntax $ UE.makeError path callPos (UE.undefinedIdentity funName)
                _ -> do
                    res <- matchInSigs importSigs callPos argInfos
                    case res of
                        Right sig -> do
                            let (usedQn, fullQn) = fromMaybe (qname, qname) (findImportNames sig)
                            recordFunUse namePos (FunImported defaultDeclFlags usedQn fullQn sig)
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
                            recordFunUse namePos (FunLocal defaultDecl (qualifyLocal funName) sig)
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

        qualifyLocal :: String -> QName
        qualifyLocal name =
            if null packages
                then [name]
                else packages ++ [name]

        third :: (a, b, c) -> c
        third (_, _, c) = c

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
    StringConst _ _ -> pure (Class ["String"] [])
    _ -> error "inferLiteral: non-literal expression"


-- | Lookup a variable id (and its position) from the current scope stack.
--   The nearest scope wins.
getVarId :: Position -> Ctx -> Maybe VarId
getVarId pos context = Map.lookup pos (CC.varUses context)

-- | Resolve local var id by source position first, then by name lookup in scopes.
resolveLocalVarId :: String -> Position -> TypeCtx -> Maybe VarId
resolveLocalVarId name pos c =
    case getVarId pos (tcCtx c) of
        Just vid -> Just vid
        Nothing -> fst <$> lookupVarId name (CC.st (tcCtx c))


-- | Predicate: ++/-- operators.
isIncDecOperator :: Operator -> Bool
isIncDecOperator op = op `elem` [IncSelf, DecSelf, SelfInc, SelfDec]


-- | Predicate: valid operand type for ++/--.
isIncDecOperandType :: Class -> Bool
isIncDecOperandType cls = isBasicType cls && cls /= Bool


-- | Validate ++/-- target mutability/lvalue constraints.
checkIncDecTarget :: Path -> Expression -> TypeM Bool
checkIncDecTarget path innerE = case innerE of
    Variable "this" tok -> do
        addErr $ UE.Syntax $ UE.makeError path [Lex.tokenPos tok] UE.thisAssignMsg
        pure False
    Variable name tok -> do
        c <- get
        let posTok = Lex.tokenPos tok
        case resolveLocalVarId name posTok c of
            Just vid -> do
                finalVar <- isVarFinal vid
                if finalVar
                    then do
                        addErr $ UE.Syntax $ UE.makeError path [posTok] (UE.immutableVariableMsg name)
                        pure False
                    else pure True
            Nothing ->
                pure True
    Qualified _ toks -> do
        addErr $ UE.Syntax $ UE.makeError path (map Lex.tokenPos toks) UE.assignErrorMsg
        pure False
    _ -> do
        addErr $ UE.Syntax $ UE.makeError path (map Lex.tokenPos (exprTokens innerE))
            (UE.cannotAssignMsg (prettyExpr 0 (Just innerE)))
        pure False


-- | Lookup a variable type from imported environments by qualified name.
getImportedVarType :: QName -> [TypedImportEnv] -> Maybe (Class, QName)
getImportedVarType qname =
    fmap (\(t, _, full) -> (t, full)) . listToMaybe . mapMaybe (Map.lookup qname . tVars)


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


inferForInit :: Path -> QName -> [TypedImportEnv] -> Statement -> TypeM ()
inferForInit path packages envs st = case st of
    Expr initExpr@(Binary Assign _ _ _) ->
        void (inferExpr path packages envs initExpr)
    Expr initExpr -> do
        addErr $ UE.Syntax $ UE.makeError path (map Lex.tokenPos (exprTokens initExpr)) UE.forInitAssignMsg
        void (inferExpr path packages envs initExpr)
    Exprs es -> mapM_ inferInitExpr es
    DefField {} -> inferDefineStmtFromDecl path packages envs st
    DefConstField {} -> inferDefineStmtFromDecl path packages envs st
    DefVar {} -> inferDefineStmtFromDecl path packages envs st
    DefConstVar {} -> inferDefineStmtFromDecl path packages envs st
    StmtGroup ss -> mapM_ (inferForInit path packages envs) ss
    BlockStmt (Multiple ss) -> mapM_ (inferForInit path packages envs) ss
    _ -> addErr $ UE.Syntax $ UE.makeError path [] UE.forInitAssignMsg
    where
        inferInitExpr initExpr@(Binary Assign _ _ _) = void (inferExpr path packages envs initExpr)
        inferInitExpr initExpr = do
            addErr $ UE.Syntax $ UE.makeError path (map Lex.tokenPos (exprTokens initExpr)) UE.forInitAssignMsg
            void (inferExpr path packages envs initExpr)


inferForStep :: Path -> QName -> [TypedImportEnv] -> Statement -> TypeM ()
inferForStep path packages envs st = case st of
    Expr e -> void (inferExpr path packages envs e)
    Exprs es -> mapM_ (void . inferExpr path packages envs) es
    StmtGroup ss -> mapM_ (inferForStep path packages envs) ss
    BlockStmt (Multiple ss) -> mapM_ (inferForStep path packages envs) ss
    _ -> addErr $ UE.Syntax $ UE.makeError path [] UE.invalidExprStmtMsg


isRepeatCountType :: Class -> Bool
isRepeatCountType t = normalizeTypeAlias t `elem` [Int8T, Int16T, Int32T, Int64T]

-- | Extract common pieces from var/val declarations.
defineDeclParts :: Statement -> Maybe (Bool, [String], Maybe Class, Maybe Expression, [Lex.Token])
defineDeclParts stmt = case stmt of
    DefField names mDeclType mRhs toks -> Just (False, names, mDeclType, mRhs, toks)
    DefConstField names mDeclType mRhs toks -> Just (True, names, mDeclType, mRhs, toks)
    DefVar names mDeclType mRhs toks -> Just (False, names, mDeclType, mRhs, toks)
    DefConstVar names mDeclType mRhs toks -> Just (True, names, mDeclType, mRhs, toks)
    _ -> Nothing

inferDefineStmtFromDecl :: Path -> QName -> [TypedImportEnv] -> Statement -> TypeM ()
inferDefineStmtFromDecl path packages envs stmt =
    case defineDeclParts stmt of
        Just (isConst, names, mDeclType, mRhs, toks) ->
            inferDefineStmt path packages envs isConst names mDeclType mRhs toks
        Nothing ->
            error "inferDefineStmtFromDecl: expected var/val declaration"


-- | Infer types in a statement.
inferStmt :: Path -> QName -> [TypedImportEnv] -> Statement -> TypeM ()
inferStmt _ _ _ (Command Pass _) = pure ()
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


inferStmt path packages envs stmt@(DefField {}) =
    inferDefineStmtFromDecl path packages envs stmt

inferStmt path packages envs stmt@(DefConstField {}) =
    inferDefineStmtFromDecl path packages envs stmt

inferStmt path packages envs stmt@(DefVar {}) =
    inferDefineStmtFromDecl path packages envs stmt

inferStmt path packages envs stmt@(DefConstVar {}) =
    inferDefineStmtFromDecl path packages envs stmt

inferStmt path packages envs (Expr e) = void $ inferExpr path packages envs e
inferStmt path packages envs (Exprs es) = mapM_ (void . inferExpr path packages envs) es
inferStmt path packages envs (StmtGroup ss) = mapM_ (inferStmt path packages envs) ss
inferStmt path packages envs (BlockStmt block) = withScope $ inferBlock path packages envs block

inferStmt path packages envs (If e ifBlock elseBlock _) = do
    t <- inferExpr path packages envs e
    checkCondBool path (map Lex.tokenPos (exprTokens e)) t
    inferOptBlock path packages envs ifBlock
    inferOptBlock path packages envs elseBlock

inferStmt path packages envs (For (s1, e2, s3) forBlock elseBlock _) = do
    withScope $ do
        maybe (pure ()) (inferForInit path packages envs) s1
        case e2 of
            Nothing -> pure ()
            Just condExpr -> do
                t <- inferExpr path packages envs condExpr
                checkCondBool path (map Lex.tokenPos (exprTokens condExpr)) t
        maybe (pure ()) (inferForStep path packages envs) s3
        inferOptBlock path packages envs forBlock
        inferOptBlock path packages envs elseBlock

inferStmt path packages envs (Loop loopBlock _) = do
    inferOptBlock path packages envs loopBlock

inferStmt path packages envs (Repeat countExpr repeatBlock elseBlock _) = do
    t <- inferExpr path packages envs countExpr
    unless (t == ErrorClass || isRepeatCountType t) $
        addErr $ UE.Syntax $ UE.makeError path
            (map Lex.tokenPos (exprTokens countExpr))
            (UE.repeatCountTypeMsg (prettyClass t))
    inferOptBlock path packages envs repeatBlock
    inferOptBlock path packages envs elseBlock

inferStmt path packages envs (While e whileBlock elseBlock _) = do
    t <- inferExpr path packages envs e
    checkCondBool path (map Lex.tokenPos (exprTokens e)) t
    inferOptBlock path packages envs whileBlock
    inferOptBlock path packages envs elseBlock

inferStmt path packages envs (Until e untilBlock elseBlock _) = do
    t <- inferExpr path packages envs e
    checkCondBool path (map Lex.tokenPos (exprTokens e)) t
    inferOptBlock path packages envs untilBlock
    inferOptBlock path packages envs elseBlock

inferStmt path packages envs (DoWhile whileBlock e elseBlock _) = do
    inferOptBlock path packages envs whileBlock
    t <- inferExpr path packages envs e
    checkCondBool path (map Lex.tokenPos (exprTokens e)) t
    inferOptBlock path packages envs elseBlock

inferStmt path packages envs (DoUntil untilBlock e elseBlock _) = do
    inferOptBlock path packages envs untilBlock
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
        tcCurrentReturn = Just retT
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
                    isMutParam = any isMutToken toks
                    flags = if isMutParam then [] else [Final]
                    m'  = Map.insert name (vid, pos) m
                    vt' = Map.insert vid (t, pos) vt
                    vf' = Map.insert vid flags vf
                in (succ vc, m', vt', vf')

        isMutToken :: Lex.Token -> Bool
        isMutToken tok = case tok of
            Lex.Ident "mut" _ -> True
            _ -> False


inferStmt _ _ _ (FunctionT {}) = do
    error "TODO for template"


-- | Infer a variable declaration (`var` / `val`) and register type + flags.
inferDefineStmt ::
    Path ->
    QName ->
    [TypedImportEnv] ->
    Bool ->
    [String] ->
    Maybe Class ->
    Maybe Expression ->
    [Lex.Token] ->
    TypeM ()
inferDefineStmt path packages envs isConst names mDeclType mRhs toks = do
    mTRhs <- traverse (inferExpr path packages envs) mRhs
    let mDeclTypeN = normalizeTypeAlias <$> mDeclType
    let declFlags = ([Final | isConst])

    case names of
        [name] -> case reverse toks of
            (nameTok:_) -> do
                c <- get
                let cctx = tcCtx c
                    cState0 = CC.st cctx
                    posTok = Lex.tokenPos nameTok
                    mVidFromCtx = getVarId posTok cctx
                    syncCurrentScope :: CheckState -> VarId -> Position -> CheckState
                    syncCurrentScope st vid posDef = case scope st of
                        [] -> st
                        (sc:rest) ->
                            if Map.member name (sVars sc)
                                then st
                                else
                                    let sc' = sc { sVars = Map.insert name (vid, posDef) (sVars sc) }
                                        nextCounter = max (varCounter st) (vid + 1)
                                    in st { scope = sc' : rest, varCounter = nextCounter }
                    cState1 = case mVidFromCtx of
                        Just vid -> syncCurrentScope cState0 vid posTok
                        Nothing -> cState0

                cState2 <- case mVidFromCtx of
                    Just _ -> pure cState1
                    Nothing -> case defineDeclaredVar path name nameTok cState1 of
                        Left err -> addErr err >> pure cState1
                        Right st' -> pure st'

                let mVidPos = case mVidFromCtx of
                        Just vid -> Just (vid, posTok)
                        Nothing -> lookupVarId name cState2

                case mVidPos of
                    Just (vid, defPos) -> do
                        setVarFlags vid declFlags
                        recordVarUseLocal posTok name vid
                        case Map.lookup vid (tcVarTypes c) of
                            Nothing ->
                                do
                                    let pos = map Lex.tokenPos toks
                                    case (mDeclTypeN, mTRhs) of
                                        (Just declT, Just rhsT) -> checkTypeCompat path pos declT rhsT
                                        _ -> pure ()
                                    let inferredType = case (mDeclTypeN, mTRhs) of
                                            (Just declT, _) -> declT
                                            (Nothing, Just rhsT) -> rhsT
                                            (Nothing, Nothing) -> Int32T
                                    modify $ \c0 -> c0 {
                                        tcCtx = (tcCtx c0) { CC.st = cState2 },
                                        tcVarTypes = Map.insert vid (inferredType, defPos) (tcVarTypes c0) }
                            Just (oldT, _) -> do
                                for_ mDeclTypeN (checkTypeCompat path (map Lex.tokenPos toks) oldT)
                                for_ mTRhs (checkTypeCompat path (map Lex.tokenPos toks) oldT)
                                modify $ \c0 -> c0 { tcCtx = (tcCtx c0) { CC.st = cState2 } }
                    Nothing -> do
                        addErr $ UE.Syntax $ UE.makeError path [posTok] (UE.undefinedIdentity name)
                        modify $ \c0 -> c0 { tcCtx = (tcCtx c0) { CC.st = cState2 } }
            [] ->
                addErr $ UE.Syntax $ UE.makeError path (maybe (map Lex.tokenPos toks) (map Lex.tokenPos . exprTokens) mRhs) UE.internalErrorMsg
        _ ->
            addErr $ UE.Syntax $ UE.makeError path (map Lex.tokenPos toks) UE.unsupportedErrorMsg


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
    let stmts' = flattenStmtGroups stmts
        funDefs = filter isFunctionStmt stmts'
    mapM_ preloadFun funDefs
    mapM_ (inferStmt path package envs) stmts'
    where
        flattenStmtGroups :: [Statement] -> [Statement]
        flattenStmtGroups = concatMap go
            where
                go :: Statement -> [Statement]
                go (StmtGroup ss) = flattenStmtGroups ss
                go st = [st]

        functionSigParts :: Statement -> Maybe (Expression, [(Class, String, [Lex.Token])], Class, Maybe [(Class, [Lex.Token])])
        functionSigParts stmt = case stmt of
            Function (retT, _) name params _ -> Just (name, params, retT, Nothing)
            FunctionT (retT, _) name tParams params _ -> Just (name, params, retT, Just tParams)
            _ -> Nothing

        isFunctionStmt :: Statement -> Bool
        isFunctionStmt stmt = case functionSigParts stmt of
            Just _ -> True
            Nothing -> False

        preloadFun :: Statement -> TypeM ()
        preloadFun stmt = case functionSigParts stmt of
            Just (name, params, retT, mTParams) ->
                addFunSig name params retT mTParams
            Nothing -> pure ()
        addFunSig :: Expression -> [(Class, String, [Lex.Token])] -> Class -> Maybe [(Class, [Lex.Token])] -> TypeM ()
        addFunSig name params retT mTParams = case name of
            Variable s tok -> do
                let sig = FunSig {
                        funParams = map (\(t, _, _) -> t) params,
                        funReturn = retT
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
inferProgm path prog0 importEnvs typedEnvs = do
    let prog@(decls, stmts) = inlineProgramFunctions (promoteTopLevelFunctions prog0)
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
    let defaultEnv = defaultTypedImportEnv path
        typedEnvs0 = defaultEnv : typedEnvs
        initCtx = TypeCtx {
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
        (_, finalCtx0) = runState (inferStmts path packageName typedEnvs0 stmts) initCtx
        finalErrs = reverse (tcErrors finalCtx0)
        finalWarns = reverse (tcWarnings finalCtx0)
        finalCtx = finalCtx0 { tcErrors = finalErrs, tcWarnings = finalWarns }
    in if null finalErrs
        then Right finalCtx
        else Left finalErrs







