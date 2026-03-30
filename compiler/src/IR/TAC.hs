{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IR.TAC where

import Control.Monad.State.Strict (State, get, put, modify, MonadState)
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Parse.SyntaxTree (Block, Class, Expression, Operator, Statement)
import Parse.ParserBasic (Decl, prettyDecl)
import Semantic.NameEnv (QName)
import Semantic.TypeEnv (FullVarTable, FullFunctionTable, FunSig)
import Util.Type (Position)
import Util.Exception (Warning)
import Util.Basic (insertTab)

import qualified Semantic.TypeEnv as TEnv
import qualified Parse.SyntaxTree as AST
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


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

-- Ternary
expandExpr (AST.Ternary c (a, b) toks) =
    let (ssC, c') = expandExpr c
        (ss1, a') = expandExpr a
        (ss2, b') = expandExpr b
    in (ssC ++ ss1 ++ ss2, AST.Ternary c' (a', b') toks)

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
expandStmt (AST.Exprs es) = let (ss, es') = expandExprList es in ss ++ [AST.Exprs es']
expandStmt (AST.StmtGroup ss) = [AST.StmtGroup (concatMap expandStmt ss)]
expandStmt (AST.BlockStmt b) = [AST.BlockStmt $ expandBlock b]
expandStmt (AST.If cond th el toks) =
    let (ssC, cond') = expandExpr cond
        th' = fmap expandBlock th
        el' = fmap expandBlock el
    in ssC ++ [AST.If cond' th' el' toks]

expandStmt (AST.For (mi, mc, ms) body elseB tokFor) =
    let
        (ssi, mi') = expandMaybeStmt mi
        (_, mc') = expandMaybeExpr mc
        (sss, ms') = expandMaybeStmt ms
        (AST.Multiple body') = maybe (AST.Multiple []) expandBlock body
        bodyFinal = Just $ AST.Multiple (body' ++ sss)
        elseFinal = fmap expandBlock elseB

    in
        -- Execute lifted init statements once before the loop
        ssi ++ [AST.For (mi', mc', ms') bodyFinal elseFinal tokFor]
    where
        expandMaybeExpr :: Maybe Expression -> ([Statement], Maybe Expression)
        expandMaybeExpr Nothing  = ([], Nothing)
        expandMaybeExpr (Just e) =
            let (ss, e') = expandExpr e
            in (ss, Just e')

        expandMaybeStmt :: Maybe Statement -> ([Statement], Maybe Statement)
        expandMaybeStmt Nothing = ([], Nothing)
        expandMaybeStmt (Just st) = case st of
            AST.Expr e ->
                let (ss, e') = expandExpr e in (ss, Just (AST.Expr e'))
            AST.Exprs es ->
                let (ss, es') = expandExprList es in (ss, Just (AST.Exprs es'))
            AST.StmtGroup ss ->
                ([], Just (AST.StmtGroup (concatMap expandStmt ss)))
            AST.BlockStmt b ->
                ([], Just (AST.BlockStmt (expandBlock b)))
            other ->
                ([], Just other)

expandStmt (AST.Repeat countExpr body elseB tokRepeat) =
    let
        (ss, countExpr') = expandExpr countExpr
        body' = fmap expandBlock body
        elseB' = fmap expandBlock elseB
    in ss ++ [AST.Repeat countExpr' body' elseB' tokRepeat]

expandStmt (AST.Until cond body elseB tokUntil) =
    let
        (ss, cond') = expandExpr cond
        body' = fmap expandBlock body
        elseB' = fmap expandBlock elseB
    in ss ++ [AST.Until cond' body' elseB' tokUntil]

expandStmt (AST.DoWhile body cond elseB tokDoWhile) =
    let
        body' = fmap expandBlock body
        (ss, cond') = expandExpr cond
        elseB' = fmap expandBlock elseB
    in ss ++ [AST.DoWhile body' cond' elseB' tokDoWhile]

expandStmt (AST.DoUntil body cond elseB tokDoUntil) =
    let
        body' = fmap expandBlock body
        (ss, cond') = expandExpr cond
        elseB' = fmap expandBlock elseB
    in ss ++ [AST.DoUntil body' cond' elseB' tokDoUntil]

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
    tacStaticVars :: Set VarKey, -- ^ static variables keyed by (name, varId)
    tacVarUses :: Map [Position] FullVarTable,
    tacFunUses :: Map [Position] FullFunctionTable,
    tacWarnings :: [Warning],
    tacAtomTypes :: Map IRAtom Class,

    tacCurrentVar :: Maybe VarKey,
    tacCurrentFun :: [(FunSig, Int, Map Int IRAtom)],

    --                 start, after
    tacCurrentLoop :: [(Int, Int)],
    tacLoopPhis :: [[(VarKey, IRAtom)]],
    tacBlockId :: Int,
    tempId :: Int,
    tacVarNextId :: Map VarKey Int
} deriving (Eq, Show)


newtype TACM a = TACM { runTACM :: State TACState a }
    deriving (Functor, Applicative, Monad, MonadState TACState)


mkTACState :: Map [Position] FullVarTable -> Map [Position] FullFunctionTable -> TACState
mkTACState vUses fUses = TACState {
    tacVarStacks = Map.empty,
    tacStaticVars = Set.empty,
    tacVarUses = vUses,
    tacFunUses = fUses,
    tacWarnings = [],
    tacAtomTypes = Map.empty,
    tacCurrentVar = Nothing,
    tacCurrentFun = [],
    tacCurrentLoop = [],
    tacLoopPhis = [],
    tacBlockId = -1,
    tempId = 0,
    tacVarNextId = Map.empty
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


-- | Mark a variable key as static.
addStaticVar :: VarKey -> TACM ()
addStaticVar key = TACM $ modify $ \st ->
    st { tacStaticVars = Set.insert key (tacStaticVars st) }


-- | Mark multiple variable keys as static.
addStaticVars :: [VarKey] -> TACM ()
addStaticVars keys = TACM $ modify $ \st ->
    st { tacStaticVars = foldr Set.insert (tacStaticVars st) keys }


-- | Check whether a variable key is static.
isStaticVar :: VarKey -> TACM Bool
isStaticVar key = do
    Set.member key . tacStaticVars <$> get



-- | Push current function context onto the stack.
--   Tuple is (signature, unifiedExitLabel).
pushCurrentFun :: (FunSig, Int, Map Int IRAtom) -> TACM ()
pushCurrentFun f = TACM $ modify $ \st -> st { tacCurrentFun = f : tacCurrentFun st }


-- | Get current function context (must exist).
--   Tuple is (signature, unifiedExitLabel).
getCurrentFun :: TACM (FunSig, Int, Map Int IRAtom)
getCurrentFun = do
    fromMaybe (error "getCurrentFun: no current function context") .
        listToMaybe .
        tacCurrentFun <$> get


-- | Get current function context (if any).
getCurrentFunMaybe :: TACM (Maybe (FunSig, Int, Map Int IRAtom))
getCurrentFunMaybe = listToMaybe . tacCurrentFun <$> get


-- | Pop current function context from the stack (must exist).
popCurrentFun :: TACM (FunSig, Int, Map Int IRAtom)
popCurrentFun = TACM $ do
    st <- get
    case tacCurrentFun st of
        [] -> error "popCurrentFun: no current function context"
        (f:fs) -> do
            put st { tacCurrentFun = fs }
            pure f


-- | Run an action with a function context pushed, then pop it.
withFun :: (FunSig, Int, Map Int IRAtom) -> TACM a -> TACM a
withFun f action = do
    pushCurrentFun f
    result <- action
    _ <- popCurrentFun
    pure result


-- | Push current loop context onto the stack.
--   Tuple is (continueTarget, breakTarget).
pushCurrentLoop :: (Int, Int) -> TACM ()
pushCurrentLoop w = TACM $ modify $ \st -> st { tacCurrentLoop = w : tacCurrentLoop st }


-- | Get current loop context (must exist).
getCurrentLoop :: TACM (Int, Int)
getCurrentLoop = do
    fromMaybe (error "getCurrentLoop: no current loop context")
        . listToMaybe
        . tacCurrentLoop <$> get


-- | Pop current loop context from the stack (must exist).
popCurrentLoop :: TACM (Int, Int)
popCurrentLoop = TACM $ do
    st <- get
    case tacCurrentLoop st of
        [] -> error "popCurrentLoop: no current loop context"
        (w:ws) -> do
            put st { tacCurrentLoop = ws }
            pure w


-- | Run an action with a loop context pushed, then pop it.
withLoop :: (Int, Int) -> TACM a -> TACM a
withLoop w action = do
    pushCurrentLoop w
    result <- action
    _ <- popCurrentLoop
    pure result


-- | Push current loop phi mapping onto the stack.
--   Mapping is from variable key to its loop-phi atom.
pushLoopPhis :: [(VarKey, IRAtom)] -> TACM ()
pushLoopPhis phis = TACM $ modify $ \st -> st { tacLoopPhis = phis : tacLoopPhis st }


-- | Pop current loop phi mapping from the stack.
popLoopPhis :: TACM [(VarKey, IRAtom)]
popLoopPhis = TACM $ do
    st <- get
    case tacLoopPhis st of
        [] -> error "popLoopPhis: no loop phi context"
        (p:ps) -> do
            put st { tacLoopPhis = ps }
            pure p


-- | Get current loop phi mapping (if any).
getCurrentLoopPhis :: TACM [(VarKey, IRAtom)]
getCurrentLoopPhis = do
    st <- get
    case tacLoopPhis st of
        (p:_) -> pure p
        [] -> pure []




-- | Increment the global block id and return the new value.
incBlockId :: TACM Int
incBlockId = TACM $ do
    st <- get
    let bid = succ $ tacBlockId st
    put st { tacBlockId = bid }
    pure bid


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
        Nothing ->
            let names = map fst (Map.keys stacks)
            in error ("peekVarStack: missing key " ++ show key ++ ", available names=" ++ show names)


-- | Resolve a var key against current stacks.
--   If exact (name, vid) is missing but there is exactly one key with the same
--   name, return that key as a fallback. This helps synthetic/hoisted nodes
--   where source-position keyed var-use may carry an outdated vid.
resolveVarKey :: VarKey -> TACM VarKey
resolveVarKey key@(name, _) = TACM $ do
    st <- get
    let stacks = tacVarStacks st
    case Map.lookup key stacks of
        Just _ -> pure key
        Nothing ->
            let sameName = [k | k@(n, _) <- Map.keys stacks, n == name]
            in case sameName of
                [k] -> pure k
                _ -> pure key


-- | Get the full var stacks map.
getVarStacks :: TACM VarStackMap
getVarStacks = tacVarStacks <$> get


-- | Replace the var stacks map.
setVarStacks :: VarStackMap -> TACM ()
setVarStacks stacks = TACM $ modify $ \st -> st { tacVarStacks = stacks }


-- | Get the atom type map recorded during lowering.
getAtomTypes :: TACM (Map IRAtom Class)
getAtomTypes = tacAtomTypes <$> get


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
        nextId = Map.findWithDefault 0 key (tacVarNextId st)
        newIdx = nextId
        newStack = case Map.lookup key stacks of
            Nothing -> [(cls, newIdx)]
            Just l -> (cls, newIdx) : l
        atom = Var (name, vid, newIdx)
    put st {
        tacVarStacks = Map.insert key newStack stacks,
        tacVarNextId = Map.insert key (succ newIdx) (tacVarNextId st),
        tacAtomTypes = Map.insert atom cls (tacAtomTypes st)
        }
    return atom


-- | Generate a fresh temporary SSA variable.
--   Temp varId starts at 0 and increases on each allocation.
generateTempVar :: Class -> TACM IRAtom
generateTempVar cls = TACM $ do
    st <- get
    let tid = tempId st
    put st { tempId = succ tid }
    runTACM (newSubVar cls ("#temp", tid))


-- | Create a new SSA-style version for the current variable context.
--   Requires 'tacCurrentVar' to be set.
newSubCVar :: Class -> TACM IRAtom
newSubCVar cls = do
    mKey <- getCurrentVar
    case mKey of
        Just key -> newSubVar cls key
        Nothing -> generateTempVar cls


data IRAtom
    = BoolC Bool
    | CharC Char
    | StringC String
    | Int8C Int
    | Int16C Int
    | Int32C Int
    | Int64C Int64
    | Float32C Double
    | Float64C Double
    | Float128C Rational
    | Var (String, Int, Int)                      -- name, varId, versionIndex
    | Phi [(Int, IRAtom)]                         -- incoming block id -> atom
    | Param Int                                   -- param for function and class
    deriving (Eq, Ord, Show)


prettyIRAtom :: IRAtom -> String
prettyIRAtom (BoolC b) = if b then "true" else "false"
prettyIRAtom (CharC c) = [c]
prettyIRAtom (StringC s) = show s
prettyIRAtom (Int8C i) = show i
prettyIRAtom (Int16C i) = show i
prettyIRAtom (Int32C i) = show i
prettyIRAtom (Int64C i) = show i
prettyIRAtom (Float32C f) = show f
prettyIRAtom (Float64C f) = show f
prettyIRAtom (Float128C r) = show r
prettyIRAtom (Var (name, vid, ver)) = concat [name, "$", show vid, "$", show ver]
prettyIRAtom (Phi pairs) =
    let showPair (bid, atom) = concat [".L", show bid, ":", prettyIRAtom atom]
    in concat ["phi(", intercalate ", " (map showPair pairs), ")"]
prettyIRAtom (Param i) = "param" ++ show i


getVarKey :: IRAtom -> (String, Int, Int)
getVarKey (Var key) = key
getVarKey _ = error "getVarKey: not a Var atom"


-- get the typr for the atom.
getAtomType :: IRAtom -> TACM Class
getAtomType (BoolC _) = return AST.Bool
getAtomType (CharC _) = return AST.Char
getAtomType (StringC _) = return (AST.Class ["java", "lang", "String"] [])
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
        Nothing ->
            error ("getAtomType: cannot find key " ++ show varKey ++ " for atom index " ++ show index ++
                ", available keys=" ++ show (Map.keys stackMap))

getAtomType (Phi _) = error "getAtomType: phi should be stripped before type query"

getAtomType (Param index) = do
    (funSig, _, _) <- getCurrentFun
    return $ TEnv.funParams funSig !! index


data IRInstr
    = Jump Int                                      -- jump to intId
    | Ifeq IRAtom IRAtom Int                        -- if a == b then jump
    | Ifne IRAtom IRAtom Int                        -- if a != b then jump
    | Iflt IRAtom IRAtom Int                        -- if a < b then jump
    | Ifle IRAtom IRAtom Int                        -- if a <= b then jump
    | Ifgt IRAtom IRAtom Int                        -- if a > b then jump
    | Ifge IRAtom IRAtom Int                        -- if a >= b then jump

    | SetIRet IRAtom                                -- set return value
    | IReturn                                       -- return with RetVar
    | Return                                        -- this is return void

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


-- | Collect the successor block id from a control flow instruction, if any.
getInstrSucc :: IRInstr -> Maybe Int
getInstrSucc (Jump bid) = Just bid
getInstrSucc (Ifeq _ _ bid) = Just bid
getInstrSucc (Ifne _ _ bid) = Just bid
getInstrSucc (Iflt _ _ bid) = Just bid
getInstrSucc (Ifle _ _ bid) = Just bid
getInstrSucc (Ifgt _ _ bid) = Just bid
getInstrSucc (Ifge _ _ bid) = Just bid
getInstrSucc _ = Nothing


prettyIRInstr :: Int -> IRInstr -> String
prettyIRInstr n instr = insertTab n ++ case instr of
    Jump bid -> "goto .L" ++ show bid
    Ifeq a b t -> concat ["if ", prettyIRAtom a, " == ", prettyIRAtom b, " goto .L", show t]
    Ifne a b t -> concat ["if ", prettyIRAtom a, " != ", prettyIRAtom b, " goto .L", show t]
    Iflt a b t -> concat ["if ", prettyIRAtom a, " < ", prettyIRAtom b, " goto .L", show t]
    Ifle a b t -> concat ["if ", prettyIRAtom a, " <= ", prettyIRAtom b, " goto .L", show t]
    Ifgt a b t -> concat ["if ", prettyIRAtom a, " > ", prettyIRAtom b, " goto .L", show t]
    Ifge a b t -> concat ["if ", prettyIRAtom a, " >= ", prettyIRAtom b, " goto .L", show t]
    SetIRet atom -> "$ret = " ++ prettyIRAtom atom
    IReturn -> "ireturn"
    Return -> "return"
    IAssign dst src -> concat [prettyIRAtom dst, " = ", prettyIRAtom src]
    IUnary dst op x -> concat [prettyIRAtom dst, " = ", AST.prettyOp op, prettyIRAtom x]
    IBinary dst op x y -> concat [prettyIRAtom dst, " = ", prettyIRAtom x, " ", AST.prettyOp op, " ", prettyIRAtom y]
    ICast dst (fromC, toC) x -> concat [prettyIRAtom dst, " = cast(", AST.prettyClass fromC, "->", AST.prettyClass toC, ") ", prettyIRAtom x]
    ICall dst name args -> concat [prettyIRAtom dst, " = call ", name, "(", intercalate ", " (map prettyIRAtom args), ")"]
    ICallStatic dst qname args -> concat [prettyIRAtom dst, " = call ", intercalate "." qname, "(", intercalate ", " (map prettyIRAtom args), ")"]
    IGetField dst obj field -> concat [
        "getfield ", prettyIRAtom dst, " ", prettyIRAtom obj, " ", intercalate "." field]
    IPutField obj field v -> concat [
        "putfield ", prettyIRAtom obj, " ", intercalate "." field, " ", prettyIRAtom v]
    IGetStatic dst qname -> concat [
        "getstatic ", prettyIRAtom dst, " ", intercalate "." qname]
    IPutStatic qname v -> concat [
        "putstatic ", intercalate "." qname, " ", prettyIRAtom v]


-- | A basic block of IR instructions.
newtype IRBlock = IRBlock (Int, [IRInstr])
    deriving (Eq, Show)


prettyIRBlock :: Int -> IRBlock -> String
prettyIRBlock n (IRBlock (bid, instrs)) =
    let header = concat [insertTab n, ".L", show bid, ":\n"]
        body = concatMap (\instr -> prettyIRInstr (n + 1) instr ++ "\n") instrs
    in header ++ body


-- | Origin kind for members in IR:
--   class         -> declared in a real class body
--   class-wrapped -> top-level declarations wrapped into a synthetic class
data IRMemberType
    = MemberClass
    | MemberClassWrapped
    deriving (Eq, Ord, Show)

prettyIRMemberType :: IRMemberType -> String
prettyIRMemberType MemberClass = "class"
prettyIRMemberType MemberClassWrapped = "class-wrapped"

type Attribute = (Decl, Class, String, IRMemberType)

prettyAttribute :: Int -> Attribute -> String
prettyAttribute n (decl, cls, name, memberType) =
    let indent = insertTab n
        declS = prettyDecl decl
        prefix = if null declS then "" else declS ++ " "
        typeS = "[" ++ prettyIRMemberType memberType ++ "] "
    in concat [indent, typeS, prefix, AST.prettyClass cls, " ", name]

-- | Function definition: access, name, signature, body.
data IRFunction
    = IRFunction
        Decl           -- ^ declaration (access + flags)
        String         -- ^ function name
        FunSig         -- ^ function signature
        (Map IRAtom Class) -- ^ atom -> type map

        [IRBlock]      -- ^ function body
        IRMemberType   -- ^ origin kind: class / class-wrapped
    deriving (Eq, Show)

prettyIRFunction :: Int -> IRFunction -> String
prettyIRFunction n (IRFunction decl name sig atomTypes body memberType) =
    let indent = insertTab n
        declS = prettyDecl decl
        declPrefix = if null declS then "" else declS ++ " "
        retS = AST.prettyClass (TEnv.funReturn sig)
        paramS = intercalate ", " (map AST.prettyClass (TEnv.funParams sig))
        typeS = concat ["[", prettyIRMemberType memberType, "] "]
        header = concat [indent, typeS, declPrefix, retS, " ", name, "(", paramS, "):\n"]
        bodyS = concatMap (prettyIRBlock (n + 1)) body
        typesS = prettyTypeMap (n + 1) atomTypes
    in concat [header, bodyS, typesS]


-- | Static initializer body blocks.
newtype StaticInit = StaticInit [IRBlock] deriving (Eq, Show)

prettyStaticInit :: Int -> StaticInit -> String
prettyStaticInit n (StaticInit blocks) =
    let indent = insertTab n
        header = indent ++ "static {}:\n"
        body = concatMap (prettyIRBlock (n + 1)) blocks
    in header ++ body


-- | Main-entry flavor detected from methods in a class.
data MainKind
    = NoMain
    | MainInt QName
    | MainVoid QName
    | MainIntArgs QName
    | MainVoidArgs QName
    deriving (Eq, Show)


prettyMainKind :: MainKind -> String
prettyMainKind kind = case kind of
    NoMain -> "none"
    MainInt qn -> "int main() @ " ++ intercalate "." qn
    MainVoid qn -> "void main() @ " ++ intercalate "." qn
    MainIntArgs qn -> "int main(String[]) @ " ++ intercalate "." qn
    MainVoidArgs qn -> "void main(String[]) @ " ++ intercalate "." qn


-- | Class definition: name, static init, methods.
data IRClass
    = IRClass
        Decl          -- ^ property
        String        -- ^ class name
        [Attribute]
        StaticInit    -- ^ static initializer
        (Map IRAtom Class) -- ^ atom -> type map (static init)
        [IRFunction]  -- ^ methods
        MainKind      -- ^ detected class-level main entry
    deriving (Eq, Show)

prettyIRClass :: Int -> IRClass -> String
prettyIRClass n (IRClass decl name attrs sInit atomTypes funs mainKind) =
    let indent = insertTab n
        declS = prettyDecl decl
        declPrefix = if null declS then "" else declS ++ " "
        header = concat [indent, declPrefix, "class ", name, ":\n"]
        mainS = indent ++ "    main: " ++ prettyMainKind mainKind ++ "\n"
        attrsS = concatMap (\a -> prettyAttribute (n + 1) a ++ "\n") attrs
        staticS = prettyStaticInit (n + 1) sInit
        typesS = prettyTypeMap (n + 1) atomTypes
        funsS = concatMap (prettyIRFunction (n + 1)) funs
    in concat [header, mainS, attrsS, staticS, typesS, funsS]

prettyTypeMap :: Int -> Map IRAtom Class -> String
prettyTypeMap n atomTypes
    | Map.null atomTypes = ""
    | otherwise =
        let indent = insertTab n
            entryIndent = insertTab (n + 1)
            header = indent ++ "type:\n"
            entries = concatMap
                (\(atom, cls) -> entryIndent ++ prettyIRAtom atom ++ ": " ++ AST.prettyClass cls ++ "\n")
                (Map.toList atomTypes)
        in header ++ entries

-- | Whole program in IR.
data IRProgm
    = IRProgm
        [String]  -- ^ package segments (if any)
        [IRClass] -- ^ top-level classes
    deriving (Eq, Show)

prettyIRProgm :: IRProgm -> String
prettyIRProgm (IRProgm pkgSegs classes) =
    let pkgLine = case pkgSegs of
            [] -> ""
            _ -> concat ["package ", intercalate "." pkgSegs, "\n\n"]
        classesS = concatMap (prettyIRClass 0) classes
        gap = if null pkgLine || null classesS then "" else "\n\n"
    in concat [pkgLine, gap, classesS]


-- | Flatten nested IR blocks so that block bodies contain only IRInstr.
--   Since IRBlock now stores only [IRInstr], this is currently identity.
flattenIRProgm :: IRProgm -> IRProgm
flattenIRProgm (IRProgm pkg classes) = IRProgm pkg (map flattenIRClass classes)

flattenIRClass :: IRClass -> IRClass
flattenIRClass (IRClass decl name fields (StaticInit blocks) atomTypes funs mainKind) =
    IRClass decl name fields (StaticInit (flattenBlocks blocks)) atomTypes (map flattenIRFunction funs) mainKind

flattenIRFunction :: IRFunction -> IRFunction
flattenIRFunction (IRFunction acc name sig atomTypes blocks memberType) =
    IRFunction acc name sig atomTypes (flattenBlocks blocks) memberType

flattenBlocks :: [IRBlock] -> [IRBlock]
flattenBlocks = id


-- | Remove redundant gotos:
--   1) `goto Lx` immediately followed by `.Lx:`
--   2) trailing `goto Lx` at end of a block when the next block is `.Lx:`
pruneIRProgm :: IRProgm -> IRProgm
pruneIRProgm (IRProgm pkg classes) = IRProgm pkg (map pruneIRClass classes)

pruneIRClass :: IRClass -> IRClass
pruneIRClass (IRClass decl name fields (StaticInit blocks) atomTypes funs mainKind) =
    IRClass decl name fields (StaticInit (pruneBlocks blocks)) atomTypes (map pruneIRFunction funs) mainKind

pruneIRFunction :: IRFunction -> IRFunction
pruneIRFunction (IRFunction acc name sig atomTypes blocks memberType) =
    IRFunction acc name sig atomTypes (pruneBlocks blocks) memberType

pruneBlocks :: [IRBlock] -> [IRBlock]
pruneBlocks [] = []
pruneBlocks [blk] = [blk]
pruneBlocks (IRBlock (bid, instrs) : IRBlock (nextBid, nextInstrs) : rest) =
    let instrs' = dropTrailingJump instrs nextBid
    in IRBlock (bid, instrs') : pruneBlocks (IRBlock (nextBid, nextInstrs) : rest)


dropTrailingJump :: [IRInstr] -> Int -> [IRInstr]
dropTrailingJump instrs nextId = case reverse instrs of
    (Jump tgt : rest) | tgt == nextId -> reverse rest
    _ -> instrs

-- | Remove empty blocks after pruning.
rmEBInProg :: IRProgm -> IRProgm
rmEBInProg (IRProgm pkg classes) = IRProgm pkg (map rmEBInClass classes)

rmEBInClass :: IRClass -> IRClass
rmEBInClass (IRClass decl name fields (StaticInit blocks) atomTypes funs mainKind) =
    IRClass decl name fields (StaticInit (rmEBInBlocks blocks)) atomTypes (map rmEBInFunc funs) mainKind

rmEBInFunc :: IRFunction -> IRFunction
rmEBInFunc (IRFunction acc name sig atomTypes blocks memberType) =
    IRFunction acc name sig atomTypes (rmEBInBlocks blocks) memberType

rmEBInBlocks :: [IRBlock] -> [IRBlock]
rmEBInBlocks = fixpoint
    where
        fixpoint :: [IRBlock] -> [IRBlock]
        fixpoint blocks =
            let blocks' = pruneBlocks (rmEmptyOnce blocks)
            in if blocks' == blocks then blocks else fixpoint blocks'

        rmEmptyOnce :: [IRBlock] -> [IRBlock]
        rmEmptyOnce blocks =
            let redirectMap = buildRedirectMap blocks
                unresolved = Set.fromList [bid | (bid, Nothing) <- Map.toList redirectMap]
                rewritten = map (rewriteBlock redirectMap) blocks
            in filter (not . isRemovableEmptyBlock unresolved) rewritten

        buildRedirectMap :: [IRBlock] -> Map Int (Maybe Int)
        buildRedirectMap = Map.fromList . go
            where
                go [] = []
                go (IRBlock (bid, instrs) : rest)
                    | null instrs = (bid, nextBlockId rest) : go rest
                    | otherwise = go rest

                nextBlockId :: [IRBlock] -> Maybe Int
                nextBlockId [] = Nothing
                nextBlockId (IRBlock (bid, _) : _) = Just bid

        rewriteBlock :: Map Int (Maybe Int) -> IRBlock -> IRBlock
        rewriteBlock redirectMap (IRBlock (bid, instrs)) =
            IRBlock (bid, map (rewriteInstr redirectMap) instrs)

        rewriteInstr :: Map Int (Maybe Int) -> IRInstr -> IRInstr
        rewriteInstr redirectMap instr = case instr of
            Jump tgt -> Jump (redirect redirectMap tgt)
            Ifeq a b tgt -> Ifeq a b (redirect redirectMap tgt)
            Ifne a b tgt -> Ifne a b (redirect redirectMap tgt)
            Iflt a b tgt -> Iflt a b (redirect redirectMap tgt)
            Ifle a b tgt -> Ifle a b (redirect redirectMap tgt)
            Ifgt a b tgt -> Ifgt a b (redirect redirectMap tgt)
            Ifge a b tgt -> Ifge a b (redirect redirectMap tgt)
            _ -> instr

        redirect :: Map Int (Maybe Int) -> Int -> Int
        redirect redirectMap = go Set.empty
            where
                go :: Set Int -> Int -> Int
                go seen cur
                    | Set.member cur seen = cur
                    | otherwise =
                        case Map.lookup cur redirectMap of
                            Just (Just next) -> go (Set.insert cur seen) next
                            _ -> cur

        -- Keep empty blocks that have no resolvable successor to avoid dangling labels.
        isRemovableEmptyBlock :: Set Int -> IRBlock -> Bool
        isRemovableEmptyBlock unresolved (IRBlock (bid, [])) =
            not (Set.member bid unresolved)
        isRemovableEmptyBlock _ _ = False
