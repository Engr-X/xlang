module Semantic.TypeCheck where

import Control.Monad.State.Strict (State, get, modify, put, runState)
import Data.Foldable (for_)
import Data.List (intercalate)
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Parse.SyntaxTree (Block(..), Class(..), Command(..), Expression(..),
    Operator(..), Program, Statement(..), SwitchCase(..), exprTokens, prettyClass, prettyExpr)
import Semantic.NameEnv (CheckState(..), QName, Scope(..), VarId, defineLocalVar, getPackageName)
import qualified Semantic.ContextCheck as CC
import Semantic.OpInfer (binOpInfer, iCast, isBasicType)
import Semantic.TypeEnv (FunTable, TypedImportEnv(..), VarTable, normalizeClass)
import Util.Exception (ErrorKind, Warning(..))
import Util.Type (Path, Position)

import qualified Data.Map.Strict as Map
import qualified Lex.Token as Lex
import qualified Util.Exception as UE


-- | Type checking context (context state + type tables + diagnostics).
--   ctx: ContextCheck state + use map for resolving VarId by position.
--   varTypes: mapping from VarId to inferred/declared types + def positions.
--   funTypes: mapping from function name to overload set.
--   currentReturn: expected return type of the current function.
--   errors/warnings: accumulated diagnostics.
data TypeCtx = TypeCtx {
    ctx :: CC.Ctx,
    varTypes :: VarTable,
    funTypes :: FunTable,
    currentReturn :: Maybe Class,
    errors :: [ErrorKind],
    warnings :: [Warning]
} deriving (Show)


-- | Type checker state monad.
type TypeM a = State TypeCtx a


-- | Append a type error to the current context.
addErr :: ErrorKind -> TypeM ()
addErr e = modify $ \c -> c { errors = e : errors c }


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


-- | Check that an actual type matches the expected type.
--   p: file path for diagnostics.
--   expr: expression used to extract token positions.
--   expected: required type in this context.
--   actual: inferred type of the expression.
--   Returns the expected type (or emits a mismatch error).
checkExpect :: Path -> Expression -> Class -> Class -> TypeM Class
checkExpect p expr expected actual = do
    let expT = normalizeClass expected
        actT = normalizeClass actual
        pos = map Lex.tokenPos (exprTokens expr)
        expS = prettyClass expT
        actS = prettyClass actT
    if actT == expT
        then pure expT
        else if isBasicType actT && isBasicType expT
            then do
                mapM_ addWarn (iCast p pos actT expT)
                pure expT
            else do
                addErr $ UE.Syntax $ UE.makeError p pos (UE.typeMismatchMsg expS actS)
                pure expT


-- | Infer the type of an expression.
--   p: file path for diagnostics.
--   pkg: current package name.
--   envs: typed import environments (can be empty in v1).
--   mExpected: optional expected type for contextual checking.
--   expr: expression to infer.
inferExpr :: Path -> QName -> [TypedImportEnv] -> Maybe Class -> Expression -> TypeM Class
inferExpr _ _ _ _ (Error _ _) = error "this error should be catched in parser state"
inferExpr p _ _ mExpected e
    | isLiteral e = inferLiteral p mExpected e
inferExpr _ _ _ _ _ = error "inferExpr: not implemented"


-- | Infer literal types with optional expected type checking.
inferLiteral :: Path -> Maybe Class -> Expression -> TypeM Class
inferLiteral p mExpected e = case e of
    IntConst _ _ -> checkOrPure Int32T
    LongConst _ _ -> checkOrPure Int64T
    FloatConst _ _ -> checkOrPure Float32T
    DoubleConst _ _ -> checkOrPure Float64T
    LongDoubleConst _ _ -> checkOrPure Float128T
    CharConst _ _ -> checkOrPure Char
    BoolConst _ _ -> checkOrPure Bool
    StringConst _ _ -> error "string type is not supported"
    _ -> error "inferLiteral: non-literal expression"
    where
        checkOrPure :: Class -> TypeM Class
        checkOrPure litT = case mExpected of
            Nothing -> pure litT
            Just t -> checkExpect p e t litT


-- | Check whether an expression is a literal.
isLiteral :: Expression -> Bool
isLiteral e = case e of
    IntConst {} -> True
    LongConst {} -> True
    FloatConst {} -> True
    DoubleConst {} -> True
    LongDoubleConst {} -> True
    CharConst {} -> True
    BoolConst {} -> True
    StringConst {} -> True
    _ -> False


-- | Lookup a variable id (and its position) from the current scope stack.
--   The nearest scope wins.
lookupVarId :: String -> CheckState -> Maybe (VarId, Position)
lookupVarId name st = listToMaybe $ mapMaybe (Map.lookup name . sVars) (scope st)
