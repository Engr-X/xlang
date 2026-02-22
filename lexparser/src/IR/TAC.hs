{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IR.TAC where

import Control.Monad.State.Strict (State, get, put, modify, MonadState)
import Data.Map.Strict (Map)
import Parse.SyntaxTree (Operator, Expression, Statement, Block, Class)
import Parse.ParserBasic (AccessModified(..))
import Semantic.TypeEnv (FullVarTable, FullFunctionTable, FunSig)
import Util.Type (Position)
import Util.Exception (Warning)

import qualified Semantic.TypeEnv as TEnv
import qualified Parse.SyntaxTree as AST
import qualified Data.Map.Strict as Map


-- Expand an expression into: (prefix statements, residual expression).
-- English note: This lifts chained assignments out of expressions so that
-- `if (a = b = true)` becomes `b = true; a = b; if (a) ...`.
expandExpr :: Expression -> ([Statement], Expression)

-- Leaf nodes (return the same node via as-pattern)
expandExpr (AST.Error _ _) = error "what this error for parser occur in Ir part?"
expandExpr e@(AST.IntConst _ _) = ([], e)
expandExpr e@(AST.LongConst _ _) = ([], e)
expandExpr e@(AST.FloatConst _ _) = ([], e)
expandExpr e@(AST.DoubleConst _ _) = ([], e)
expandExpr e@(AST.LongDoubleConst _ _) = ([], e)
expandExpr e@(AST.CharConst _ _) = ([], e)
expandExpr e@(AST.StringConst _ _) = ([], e)
expandExpr e@(AST.BoolConst _ _) = ([], e)

expandExpr e@(AST.Variable _ _) = ([], e)
expandExpr e@(AST.Qualified _ _) = ([], e)

-- Cast
expandExpr (AST.Cast ct x tok) = let (ss, x') = expandExpr x in (ss, AST.Cast ct x' tok)

-- Unary
expandExpr (AST.Unary op x tok) = let (ss, x') = expandExpr x in (ss, AST.Unary op x' tok)

-- Assignment lifting: a = (b = rhs)  ==>  b = rhs; a = b; and residual becomes lhs
-- Replace `Assign` with your actual "=" operator constructor.
expandExpr (AST.Binary AST.Assign lhs rhs tok) =
    let (ssR, rhs') = expandExpr rhs
        assignE = AST.Binary AST.Assign lhs rhs' tok
        assignS = AST.Expr assignE
        residual = lhs
    in (ssR ++ [assignS], residual)

-- Other binary ops
expandExpr (AST.Binary op a b tok) =
    let (ss1, a') = expandExpr a
        (ss2, b') = expandExpr b
    in (ss1 ++ ss2, AST.Binary op a' b' tok)

-- Calls
expandExpr (AST.Call f args) =
    let (ssf, f') = expandExpr f
        (ssa, args') = expandExprList args
    in (ssf ++ ssa, AST.Call f' args')

expandExpr (AST.CallT f tys args) =
    let (ssf, f') = expandExpr f
        (ssa, args') = expandExprList args
    in (ssf ++ ssa, AST.CallT f' tys args')


expandExprList :: [Expression] -> ([Statement], [Expression])
expandExprList [] = ([], [])
expandExprList (x:xs) =
    let (ss1, x')  = expandExpr x
        (ss2, xs') = expandExprList xs
    in (ss1 ++ ss2, x' : xs')


-- Expand a command statement.
-- English note: `return <expr>` may contain assignment-exprs that must be lifted
-- before the return, e.g. `return a = 10` -> `a = 10; return a`.
expandStmt :: Statement -> [Statement]
expandStmt e@(AST.Command AST.Continue _) = [e]
expandStmt e@(AST.Command AST.Break _) = [e]
expandStmt e@(AST.Command (AST.Return Nothing) _) = [e]
expandStmt (AST.Command (AST.Return (Just e)) tok) = let (ss, e') = expandExpr e in ss ++ [AST.Command (AST.Return (Just e')) tok]

expandStmt (AST.Expr e) = let (ss, e') = expandExpr e in ss ++ [AST.Expr e']
expandStmt (AST.BlockStmt b) = [AST.BlockStmt $ expandBlock b]
expandStmt (AST.If cond th el toks) =
    let (ssC, cond') = expandExpr cond
        th' = fmap expandBlock th
        el' = fmap expandBlock el
    in ssC ++ [AST.If cond' th' el' toks]

expandStmt (AST.For (mi, mc, ms) body tokFor) =
    let
        (ssi, mi') = expandMaybeExpr mi
        (_, mc') = expandMaybeExpr mc
        (sss, ms') = expandMaybeExpr ms
        (AST.Multiple body') = maybe (AST.Multiple []) expandBlock body
        bodyFinal = Just $ AST.Multiple (body' ++ sss)

    in
        -- Execute lifted init statements once before the loop
        ssi ++ [AST.For (mi', mc', ms') bodyFinal tokFor]
    where
        expandMaybeExpr :: Maybe Expression -> ([Statement], Maybe Expression)
        expandMaybeExpr Nothing  = ([], Nothing)
        expandMaybeExpr (Just e) =
            let (ss, e') = expandExpr e
            in (ss, Just e')

-- Fallback: leave other statements unchanged for now.
expandStmt s = [s]


expandBlock :: Block -> Block
expandBlock (AST.Multiple ss) = AST.Multiple $ concatMap expandStmt ss


-- | TAC variable versions for a source-level variable.
--   Key: (name, varId). Each assignment can create a new version like a0$0, a0$1...
--   Value: stack of (Class, versionIndex). Stack is used because newer versions
--   shadow older ones, and we can pop when leaving a scope.
type VarKey = (String, Int)
type VarStack = [(Class, Int)]
type VarStackMap = Map VarKey VarStack

data TACState = TACState {
    tacVarStacks :: VarStackMap,
    tacVarUses :: Map [Position] FullVarTable,
    tacFunUses :: Map [Position] FullFunctionTable,
    tacWarnings :: [Warning],

    tacCurrentVar :: Maybe VarKey,
    tacCurrentFun :: Maybe FunSig
} deriving (Eq, Show)


newtype TACM a = TACM { runTACM :: State TACState a }
    deriving (Functor, Applicative, Monad, MonadState TACState)


mkTACState :: Map [Position] FullVarTable -> Map [Position] FullFunctionTable -> TACState
mkTACState vUses fUses = TACState {
    tacVarStacks = Map.empty,
    tacVarUses = vUses,
    tacFunUses = fUses,
    tacWarnings = [],
    tacCurrentVar = Nothing,
    tacCurrentFun = Nothing
}


-- | Append a warning emitted during TAC lowering.
addWarn :: Warning -> TACM ()
addWarn w = TACM $ modify $ \st -> st { tacWarnings = w : tacWarnings st }


-- | Set current variable context (if any).
setCurrentVar :: Maybe VarKey -> TACM ()
setCurrentVar mv = TACM $ modify $ \st -> st { tacCurrentVar = mv }


-- | Get current variable context (if any).
getCurrentVar :: TACM (Maybe VarKey)
getCurrentVar = tacCurrentVar <$> get


-- | Set current function context (if any).
setCurrentFun :: Maybe FunSig -> TACM ()
setCurrentFun mf = TACM $ modify $ \st -> st { tacCurrentFun = mf }


-- | Get current function context (if any).
getCurrentFun :: TACM (Maybe FunSig)
getCurrentFun = tacCurrentFun <$> get



-- | Get variable usage info by source position (must exist).
getVar :: [Position] -> TACM FullVarTable
getVar pos = TACM $ do
    st <- get
    case Map.lookup pos (tacVarUses st) of
        Just v -> pure v
        Nothing -> error ("getVar: no var info at " ++ show pos)


-- | Get function usage info by source position (must exist).
getFunction :: [Position] -> TACM FullFunctionTable
getFunction pos = TACM $ do
    st <- get
    case Map.lookup pos (tacFunUses st) of
        Just f -> pure f
        Nothing -> error ("getFunction: no function info at " ++ show pos)


-- | Lookup the current (top) version info for a var key.
peekVarStack :: VarKey -> TACM (Class, Int)
peekVarStack key = TACM $ do
    st <- get
    let stacks = tacVarStacks st
    return $ case Map.lookup key stacks of
        Just (x:_) -> x
        Just [] -> error "stack is empty!"
        Nothing -> error "cannot fin the variable!"


-- | Lookup the current (top) version info for the current variable context.
peekCVarStack :: TACM (Class, Int)
peekCVarStack = do
    mKey <- getCurrentVar
    case mKey of
        Just key -> peekVarStack key
        Nothing -> error "peekCVarStack: no current variable context"

-- | Create a new SSA-style version for a variable key.
--   Pushes the new (Class, versionIndex) on the stack and
--   returns the corresponding Var atom.
newSubVar :: Class -> VarKey -> TACM IRAtom
newSubVar cls key@(name, vid) = TACM $ do
    st <- get
    let stacks = tacVarStacks st
        (newIdx, newStack) = case Map.lookup key stacks of
            Nothing -> (0, [(cls, 0)])
            Just [] -> (0, [(cls, 0)])
            Just l@((_, idx):_) -> (succ idx, (cls, succ idx) : l)
    put st { tacVarStacks = Map.insert key newStack stacks }
    return (Var (name, vid, newIdx))


-- | Create a new SSA-style version for the current variable context.
--   Requires 'tacCurrentVar' to be set.
newSubCVar :: Class -> TACM IRAtom
newSubCVar cls = do
    mKey <- getCurrentVar
    case mKey of
        Just key -> newSubVar cls key
        Nothing -> error "newSubCVar: no current variable context"


data IRAtom
    = BoolC Bool
    | CharC Char
    | Int8C Int
    | Int16C Int 
    | Int32C Int
    | Int64C Int 
    | Float32C Double 
    | Float64C Double 
    | Float128C Rational 
    | Var (String, Int, Int)                      -- name, varId, versionIndex
    | Param Int                                   -- param for function and class
    deriving (Eq, Show)


getAtomType :: IRAtom -> TACM Class
getAtomType (BoolC _) = return AST.Bool
getAtomType (CharC _) = return AST.Char
getAtomType (Int8C _) = return AST.Int8T
getAtomType (Int16C _) = return AST.Int16T
getAtomType (Int32C _) = return AST.Int32T
getAtomType (Int64C _) = return AST.Int64T
getAtomType (Float32C _) = return AST.Float32T
getAtomType (Float64C _) = return AST.Float64T
getAtomType (Float128C _) = return AST.Float128T
getAtomType (Var (name, varId, index)) = let varKey = (name, varId) in do
    st <- get
    let stackMap = tacVarStacks st
    let classId = Map.lookup varKey stackMap

    case classId of
        Just list -> let rel = filter (\(_, num) -> num == index) list in return $ fst $ head rel
        Nothing -> error "cannot find atom in VarStackMap!"

getAtomType (Param index) = do
    funSig <- getCurrentFun
    case funSig of
        Just sig -> return $ TEnv.funParams sig !! index
        Nothing -> error "this is not in a function"
    



data IRInstr
    = Jump Int                                      -- jump to intId
    | ConJump IRAtom Int Int                        -- condition Jump by condition

    | IAssign IRAtom IRAtom                         -- dst = src (move/copy)
    | IUnary IRAtom Operator IRAtom                 -- dst = op x
    | IBinary IRAtom Operator IRAtom IRAtom         -- dst = x op y
    --              class1, class2
    | ICast IRAtom (Class, Class) IRAtom            -- dst = cast atom from class1 to class2
    | ICall IRAtom String [IRAtom]                  -- dst = call f(args)
    | ICallStatic IRAtom [String] [IRAtom]          -- dst = call C.f(args)

    -- for class
    | IGetField IRAtom IRAtom [String]              -- dst = obj.f
    | IPutField IRAtom [String] IRAtom              -- obj.f = v

    | IGetStatic IRAtom [String]                    -- dst = C.f
    | IPutStatic [String] IRAtom                    -- C.f = v
    deriving (Eq, Show)


-- | A basic block of IR statements.
newtype IRBlock = IRBlock (Int, [IRStmt])
    deriving (Eq, Show)


-- | TAC-level statement.
data IRStmt
    = IRInstr IRInstr
    | IRBlockStmt IRBlock -- for branch
    deriving (Eq, Show)


-- | Class/struct field: access, type, name.
data FieldVar
    = FieldVar
        AccessModified -- ^ access modifier
        Class          -- ^ field type
        String         -- ^ field name
    deriving (Eq, Show)

type AttributeVar = FieldVar

-- | Method signature: access, name, signature.
data FieldFun
    = FieldFun
        AccessModified -- ^ access modifier
        String         -- ^ method name
        FunSig         -- ^ method signature
    deriving (Eq, Show)

type AttributeFun = FieldFun

-- | Function definition: access, name, signature, body.
data IRFunction
    = IRFunction
        AccessModified -- ^ access modifier
        String         -- ^ function name
        FunSig         -- ^ function signature
        [IRStmt]       -- ^ function body
    deriving (Eq, Show)

-- | Static initializer body statements.
newtype StaticInit
  = StaticInit [IRStmt]
  deriving (Eq, Show)
 
-- | Class definition: name, static init, methods.
data IRClass
    = IRClass
        String        -- ^ class name
        StaticInit    -- ^ static initializer
        [IRFunction]  -- ^ methods
    deriving (Eq, Show)

-- | Whole program in IR.
data IRProgm
    = IRProgm
        [String]  -- ^ package segments (if any)
        [IRClass] -- ^ top-level classes
    deriving (Eq, Show)
