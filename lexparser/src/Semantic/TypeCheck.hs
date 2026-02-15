{-# LANGUAGE DuplicateRecordFields #-}

module Semantic.TypeCheck where

import Control.Monad (when)
import Control.Monad.State.Strict (State, get, modify, put)
import Data.List (intercalate)
import Data.Maybe (listToMaybe, mapMaybe, isNothing)
import Parse.SyntaxTree (Class(..), Expression(..), exprTokens, prettyClass)
import Semantic.NameEnv (CheckState(..), QName, Scope(..), VarId)
import Semantic.ContextCheck (Ctx)
import Semantic.OpInfer (binOpInfer, iCast, isBasicType, unaryOpInfer, widenedArgs)
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
--   ctx: ContextCheck state + use map for resolving VarId by position.
--   varTypes: mapping from VarId to inferred/declared types + def positions.
--   funTypes: mapping from function name to overload set.
--   classStack: current class definitions (top = nearest).
--   classTypeStack: current class types (top = nearest).
--   currentReturn: expected return type of the current function.
--   errors/warnings: accumulated diagnostics.
data TypeCtx = TypeCtx {
    ctx :: CC.Ctx,
    varTypes :: VarTable,
    funTypes :: FunTable,
    classStack :: [ClassDef],
    classTypeStack :: [Class],
    currentReturn :: Maybe Class,
    errors :: [ErrorKind],
    warnings :: [Warning]
} deriving (Show)


-- | Type checker state monad.
type TypeM a = State TypeCtx a


-- | Append a type error to the current context.
addErr :: ErrorKind -> TypeM ()
addErr e = modify $ \c -> c { errors = e : errors c }

-- | Append a type error and return ErrorClass.
errClass :: ErrorKind -> TypeM Class
errClass e = addErr e >> pure ErrorClass


-- | Append a warning to the current context.
addWarn :: Warning -> TypeM ()
addWarn w = modify $ \c -> c { warnings = w : warnings c }


-- | Create a new lexical scope for the duration of an action, then restore depth/scope.
withScope :: TypeM a -> TypeM a
withScope action = do
    c <- get
    let cctx = ctx c
        cState = CC.st cctx
        depth0 = depth cState
        newScope = Scope { scopeId = scopeCounter cState, sVars = Map.empty, sFuncs = Map.empty }
        cState' = cState {
            depth = succ depth0,
            scopeCounter = succ $ scopeCounter cState,
            scope = newScope : scope cState
        }
    put $ c { ctx = cctx { CC.st = cState' } }
    res <- action
    c2 <- get
    let cctx2 = ctx c2
        cState2 = CC.st cctx2
        scope' = tail $ scope cState2
    put $ c2 { ctx = cctx2 { CC.st = cState2 { depth = depth0, scope = scope' } } }
    pure res


-- | Resolve `this` usage by returning the current class type.
inferThis :: Path -> Position -> TypeM Class
inferThis path pos = do
    c <- get
    case classTypeStack c of
        (cls:_) -> pure $ normalizeClass cls
        [] -> errClass $ UE.Syntax $ UE.makeError path [pos] (UE.undefinedIdentity "this")
                

-- | Resolve `this.field` in the nearest class scope (placeholder types).
inferThisField :: Path -> [Position] -> String -> TypeM Class
inferThisField path pos field = do
    c <- get
    let cState = CC.st (ctx c)
    case classStack c of
        [] -> errClass $ UE.Syntax $ UE.makeError path pos (UE.undefinedIdentity "this")
        (ClassDef _ classVars _ : _) ->
            case classScope cState of
                [] -> errClass $ UE.Syntax $ UE.makeError path pos (UE.undefinedIdentity field)
                (clsTop:_) ->
                    let mType = Map.lookup field (sVars clsTop) >>= (\(vid, _) -> Map.lookup vid classVars) in do
                        when (isNothing mType) $
                            addErr $ UE.Syntax $ UE.makeError path pos (UE.undefinedIdentity field)
                        pure $ maybe ErrorClass (normalizeClass . fst) mType

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
    let TypeCtx {varTypes = vts} = c

    -- 变量为 this 时，解析为当前类实例。
    if str == "this" then inferThis path pos
    else do
    -- 优先通过 ContextCheck 记录的 VarId 查找本地变量。
        case getVarId pos (ctx c) of
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
    let TypeCtx{varTypes = vts} = c
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
                case getVarId posHead (ctx c) of
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
    let TypeCtx{funTypes = fts} = c
    let callPos = map Lex.tokenPos (exprTokens e)

    -- 推导实参类型与位置。
    argTs0 <- mapM (inferExpr path packages envs) args
    let argInfos = zip (map normalizeClass argTs0) (map (map Lex.tokenPos . exprTokens) args)

    -- 统一处理匹配与报错逻辑（使用 widenedArgs）。
    let applyMatches funName sigs =
            if null sigs then
                errClass $ UE.Syntax $ UE.makeError path callPos (UE.undefinedIdentity funName)
            else
                case widenedArgs path callPos argInfos sigs of
                    Left err -> errClass err
                    Right (sig, warns) -> do
                        mapM_ addWarn warns
                        pure $ normalizeClass (funReturn sig)

    case callee of
        -- 直接调用函数名 f(...)
        Variable name _ -> do
            let qLocal = [name]
            let qImport = packages ++ [name]
            let localSigs = Map.findWithDefault [] qLocal fts
            let importSigs = concatMap (maybe [] fst . Map.lookup qImport . tFuncs) envs
            applyMatches name (localSigs ++ importSigs)

        -- 调用限定名（可能是 this.xxx 或导入函数）。
        Qualified names _ -> case names of
            -- this.xxx(...) -> 从当前类方法表查找。
            ("this":fname:_) -> case classStack c of
                [] -> errClass $ UE.Syntax $ UE.makeError path callPos (UE.undefinedIdentity "this")
                (ClassDef _ _ classFuns : _) -> do
                    let sigs = Map.findWithDefault [] [fname] classFuns
                    applyMatches fname sigs

            -- 其他限定名 -> 只从导入/包中查找。
            _ -> do
                let sigs = concatMap (maybe [] fst . Map.lookup names . tFuncs) envs
                applyMatches (intercalate "." names) sigs

        -- 其他表达式作为函数名 -> 非法调用。
        _ -> errClass $ UE.Syntax $ UE.makeError path callPos UE.invalidFunctionName

inferExpr path _ _ e@(CallT _ _ _) =
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
