{-# LANGUAGE DuplicateRecordFields #-}

module Semantic.TypeCheck where

import Control.Monad (when, void)
import Control.Monad.State.Strict (State, get, modify, put)
import Data.List (intercalate)
import Data.Maybe (listToMaybe, mapMaybe, isNothing)
import Parse.SyntaxTree (Block(..), Class(..), Command(..), Expression(..), Statement(..), SwitchCase(..), exprTokens, prettyClass)
import Semantic.NameEnv (CheckState(..), QName, Scope(..), VarId)
import Semantic.ContextCheck (Ctx)
import Semantic.OpInfer (binOpInfer, iCast, isBasicType, unaryOpInfer, widenedArgs)
import Semantic.ImportLoader (loadSrcTypedI)
import Semantic.TypeEnv (FunSig(..), FunTable, TypedImportEnv(..), VarTable, normalizeClass)
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


defToClass :: QName -> ClassDef -> Class
defToClass package def = Class (package ++ [className def]) []


-- | Type checking context (context state + type tables + diagnostics).
--   tcCtx: ContextCheck state + use map for resolving VarId by position.
--   tcVarTypes: mapping from VarId to inferred/declared types + def positions.
--   tcFunTypes: mapping from function name to overload set.
--   tcClassStack: current class definitions (top = nearest).
--   tcClassTypeStack: current class types (top = nearest).
--   tcCurrentReturn: expected return type of the current function.
--   tcErrors/tcWarnings: accumulated diagnostics.
data TypeCtx = TypeCtx {
    tcCtx :: CC.Ctx,
    tcVarTypes :: VarTable,
    tcFunTypes :: FunTable,
    tcClassStack :: [ClassDef],
    tcClassTypeStack :: [Class],
    tcCurrentReturn :: Maybe Class,
    tcErrors :: [ErrorKind],
    tcWarnings :: [Warning]
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
    put $ c { tcCtx = cctx { CC.st = cState' } }
    res <- action
    c2 <- get
    let cctx2 = tcCtx c2
        cState2 = CC.st cctx2
        scope' = tail $ scope cState2
    put $ c2 { tcCtx = cctx2 { CC.st = cState2 { depth = depth0, scope = scope' } } }
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
    -- 优先通过 ContextCheck 记录的 VarId 查找本地变量。
        case getVarId pos (tcCtx c) of
            Just vid ->
                -- 中文: 根据 VarId 在类型表中查找类型。
                case Map.lookup vid vts of
                    Just (t, _) -> pure t
                    -- VarId 存在但类型表缺失，属于内部错误。
                    Nothing -> error "what? find this variable in ContextCheck but not in typeCheck"

            Nothing -> do
                let qn = packages ++ [str]

                -- 若本地变量不存在，再按包/导入的限定名查找。
                case listToMaybe $ mapMaybe (Map.lookup qn . tVars) envs of
                    -- 在导入环境中找到变量类型。
                    Just (t, _) -> pure t

                    -- 理论上应在 ContextCheck 阶段报错。
                    Nothing -> error "should already be caught in ContextCheck (undefined variable)."

inferExpr path _ envs (Qualified names toks) = do
    c <- get
    let posAll = map Lex.tokenPos toks
    let posHead = head posAll
    let TypeCtx{tcVarTypes = vts} = c
    case names of
        -- 只有 `this` -> 解析为当前类的类型。
        ["this"] -> inferThis path posHead

        -- `this.field`（或更深）-> 解析为实例成员。
        -- TODO
        ("this":field:_) -> inferThisField path posAll field

        -- 1) 先按导入/包的限定名查找。
        _ -> case getImportedVarType names envs of
            Just t -> do
                pure t

            -- 2) 否则把头部当变量，再处理成员访问。
            Nothing -> do
                let headName = head names
                case getVarId posHead (tcCtx c) of
                    -- 头部变量不存在 -> 未定义。
                    Nothing -> errClass $ UE.Syntax $ UE.makeError path posAll (UE.undefinedIdentity headName)

                    Just vid -> do
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
    if isBasicType fromT && isBasicType toT then do
        addErr $ UE.Syntax $ UE.makeError path pos $ staticCastError (prettyClass fromT) (prettyClass toT)

    -- both are non-basic (class / array / user type)
    else do
        error "TODO: class cast is not supported yet: "

    -- result type of cast is always target type
    pure toT

inferExpr path packages envs e@(Unary op innerE _) = do
    t0 <- inferExpr path packages envs innerE
    let pos = map Lex.tokenPos (exprTokens e)
    if isBasicType t0 then
        case Map.lookup (op, t0) unaryOpInfer of
            Just resT -> do
                mapM_ addWarn (iCast path pos t0 resT)
                pure resT
            Nothing -> error "TODO: unary operator not supported for this type"
    else error "TODO: unary on non-basic type"


inferExpr path packages envs e@(Binary op e1 e2 _) = do
    t1 <- inferExpr path packages envs e1
    t2 <- inferExpr path packages envs e2
    let pos = map Lex.tokenPos (exprTokens e)
    if isBasicType t1 && isBasicType t2 then
        case Map.lookup (op, t1, t2) binOpInfer of
            Just resT -> do
                mapM_ addWarn (iCast path pos t1 resT)
                mapM_ addWarn (iCast path pos t2 resT)
                pure resT
            Nothing -> error "TODO: binary operator not supported for this type"
    else error "TODO: binary on non-basic type"

inferExpr path packages envs e@(Call callee args) = do
    c <- get
    let TypeCtx{tcFunTypes = fts} = c
    let callPos = map Lex.tokenPos (exprTokens e)

    -- 推导实参类型与位置。
    argTs0 <- mapM (inferExpr path packages envs) args
    let argInfos = zip argTs0 (map (map Lex.tokenPos . exprTokens) args)

    case callee of
        -- 直接调用函数名 f(...)
        Variable name _ -> do
            let qLocal = [name]
            let qImport = packages ++ [name]
            let localSigs = Map.findWithDefault [] qLocal fts
            let importSigs = concatMap (maybe [] fst . Map.lookup qImport . tFuncs) envs
            applyMatches callPos argInfos name (localSigs ++ importSigs)

        -- 调用限定名（可能是 this.xxx 或导入函数）。
        Qualified names _ -> case names of
            -- this.xxx(...) -> 从当前类方法表查找。
            ("this":fname:_) -> case tcClassStack c of
                [] -> errClass $ UE.Syntax $ UE.makeError path callPos (UE.undefinedIdentity "this")
                (ClassDef _ _ classFuns : _) -> do
                    let sigs = Map.findWithDefault (error "this error should be catched in COntextCheck") [fname] classFuns
                    applyMatches callPos argInfos fname sigs

            -- 其他限定名 -> 只从导入/包中查找。
            _ -> do
                let sigs = concatMap (maybe [] fst . Map.lookup names . tFuncs) envs
                applyMatches callPos argInfos (intercalate "." names) sigs

        -- 其他表达式作为函数名 -> 非法调用。
        _ -> errClass $ UE.Syntax $ UE.makeError path callPos UE.invalidFunctionName
    where
        -- 统一处理匹配与报错逻辑（使用 widenedArgs）。
        applyMatches :: [Position] -> [(Class, [Position])] -> String -> [FunSig] -> TypeM Class
        applyMatches callPos _ funName [] =
            errClass $ UE.Syntax $ UE.makeError path callPos (UE.undefinedIdentity funName)
        applyMatches callPos argInfos _ sigs@(_ : _) =
            case widenedArgs path callPos argInfos sigs of
                Left err -> errClass err
                Right (sig, warns) -> do
                    addWarns warns
                    pure $ funReturn sig

        addWarns = mapM_ addWarn

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


getImportedVarType :: QName -> [TypedImportEnv] -> Maybe Class
getImportedVarType qname = fmap fst . listToMaybe . mapMaybe (Map.lookup qname . tVars)


-- | Infer types in a statement.
inferStmt :: Path -> QName -> [TypedImportEnv] -> Statement -> TypeM ()
inferStmt _ _ _ (Command Continue _) = pure ()
inferStmt _ _ _ (Command Break _) = pure ()
inferStmt _ _ _ (Command (Return Nothing) _) = pure ()
inferStmt path packages envs (Command (Return (Just e)) _) = void (inferExpr path packages envs e)

inferStmt path packages envs (Expr e) = void (inferExpr path packages envs e)
inferStmt path packages envs (BlockStmt block) =
    withScope (inferBlock path packages envs block)
inferStmt path packages envs (If e ifBlock elseBlock _) = do
    void (inferExpr path packages envs e)
    case ifBlock of
        Nothing -> pure ()
        Just b -> withScope (inferBlock path packages envs b)
    case elseBlock of
        Nothing -> pure ()
        Just b -> withScope (inferBlock path packages envs b)

-- | Infer types in a block (preload function signatures first).
inferBlock :: Path -> QName -> [TypedImportEnv] -> Block -> TypeM ()
inferBlock path packages envs (Multiple stmts) = do
    c0 <- get
    let oldFts = tcFunTypes c0
    
    case loadSrcTypedI path ([], stmts) of
        Left errs -> mapM_ addErr errs
        Right tenv -> do
            let newFuns = Map.map fst (tFuncs tenv)
                fts' = Map.unionWith (++) oldFts newFuns
            put $ c0 { tcFunTypes = fts' }

    mapM_ (inferStmt path packages envs) stmts

    c1 <- get
    put $ c1 { tcFunTypes = oldFts }

