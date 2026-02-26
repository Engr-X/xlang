{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IR.TAC where

import Control.Monad.State.Strict (State, get, put, modify, MonadState)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Parse.SyntaxTree (Block, Class, Expression, Operator, Statement)
import Parse.ParserBasic (Decl, prettyDecl)
import Semantic.TypeEnv (FullVarTable, FullFunctionTable, FunSig)
import Util.Type (Position)
import Util.Exception (Warning)

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
    tacStaticVars :: Set VarKey, -- ^ static variables keyed by (name, varId)
    tacVarUses :: Map [Position] FullVarTable,
    tacFunUses :: Map [Position] FullFunctionTable,
    tacWarnings :: [Warning],

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
        Nothing -> error "cannot fin the variable!"


-- | Get the full var stacks map.
getVarStacks :: TACM VarStackMap
getVarStacks = tacVarStacks <$> get


-- | Replace the var stacks map.
setVarStacks :: VarStackMap -> TACM ()
setVarStacks stacks = TACM $ modify $ \st -> st { tacVarStacks = stacks }


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
    put st {
        tacVarStacks = Map.insert key newStack stacks,
        tacVarNextId = Map.insert key (succ newIdx) (tacVarNextId st)
        }
    return (Var (name, vid, newIdx))


-- | Create a new SSA-style version for the current variable context.
--   Requires 'tacCurrentVar' to be set.
newSubCVar :: Class -> TACM IRAtom
newSubCVar cls = do
    mKey <- getCurrentVar
    case mKey of
        Just key -> newSubVar cls key
        Nothing -> do
            st <- get
            let newId = succ $ tempId st
            put st { tempId = newId }
            newSubVar cls ("#temp", newId)


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
    | Phi [(Int, IRAtom)]                         -- incoming block id -> atom
    | Param Int                                   -- param for function and class
    deriving (Eq, Show)


prettyIRAtom :: IRAtom -> String
prettyIRAtom (BoolC b) = if b then "true" else "false"
prettyIRAtom (CharC c) = [c]
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


-- get the typr for the atom.
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

getAtomType (Phi _) = error "getAtomType: phi should be stripped before type query"

getAtomType (Param index) = do
    (funSig, _, _) <- getCurrentFun
    return $ TEnv.funParams funSig !! index
    

data IRInstr
    = Jump Int                                      -- jump to intId
    | ConJump IRAtom Int                            -- condition Jump by condition

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


prettyIRInstr :: Int -> IRInstr -> String
prettyIRInstr n instr = replicate (n * 4) ' ' ++ case instr of
    Jump bid -> "goto .L" ++ show bid
    ConJump cond t -> concat ["if ", prettyIRAtom cond, " goto .L" ++ show t]
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


-- | A basic block of IR statements.
newtype IRBlock = IRBlock (Int, [IRStmt])
    deriving (Eq, Show)


prettyIRBlock :: Int -> IRBlock -> String
prettyIRBlock n (IRBlock (bid, stmts)) =
    let header = concat [replicate (n * 4) ' ', ".L", show bid, ":\n"]
        body = concatMap (prettyStmt (n + 1)) stmts
    in header ++ body


-- | TAC-level statement.
data IRStmt
    = IRInstr IRInstr
    | IRBlockStmt IRBlock -- for branch
    deriving (Eq, Show)


prettyStmt :: Int -> IRStmt -> String
prettyStmt n (IRInstr instr) = prettyIRInstr n instr ++ "\n"
prettyStmt n (IRBlockStmt blk) = prettyIRBlock n blk


type Attribute = (Decl, Class, String)

prettyAttribute :: Int -> Attribute -> String
prettyAttribute n (decl, cls, name) =
    let indent = replicate (n * 4) ' '
        declS = prettyDecl decl
        prefix = if null declS then "" else declS ++ " "
    in concat [indent, prefix, AST.prettyClass cls, " ", name]

-- | Function definition: access, name, signature, body.
data IRFunction
    = IRFunction
        Decl           -- ^ declaration (access + flags)
        String         -- ^ function name
        FunSig         -- ^ function signature
        
        [IRStmt]       -- ^ function body
    deriving (Eq, Show)

prettyIRFunction :: Int -> IRFunction -> String
prettyIRFunction n (IRFunction decl name sig body) =
    let indent = replicate (n * 4) ' '
        declS = prettyDecl decl
        declPrefix = if null declS then "" else declS ++ " "
        retS = AST.prettyClass (TEnv.funReturn sig)
        paramS = intercalate ", " (map AST.prettyClass (TEnv.funParams sig))
        header = concat [indent, declPrefix, retS, " ", name, "(", paramS, "):\n"]
        bodyS = concatMap (prettyStmt (n + 1)) body
    in header ++ bodyS


-- | Static initializer body statements.
newtype StaticInit = StaticInit [IRStmt] deriving (Eq, Show)

prettyStaticInit :: Int -> StaticInit -> String
prettyStaticInit n (StaticInit stmts) =
    let indent = replicate (n * 4) ' '
        header = indent ++ "static {}:\n"
        body = concatMap (prettyStmt (n + 1)) stmts
    in header ++ body
 
-- | Class definition: name, static init, methods.
data IRClass
    = IRClass
        Decl          -- ^ property
        String        -- ^ class name
        [Attribute]
        StaticInit    -- ^ static initializer
        [IRFunction]  -- ^ methods
    deriving (Eq, Show)

prettyIRClass :: Int -> IRClass -> String
prettyIRClass n (IRClass decl name attrs sInit funs) =
    let indent = replicate (n * 4) ' '
        declS = prettyDecl decl
        declPrefix = if null declS then "" else declS ++ " "
        header = concat [indent, declPrefix, "class ", name, ":\n"]
        attrsS = concatMap (\a -> prettyAttribute (n + 1) a ++ "\n") attrs
        staticS = prettyStaticInit (n + 1) sInit
        funsS = concatMap (prettyIRFunction (n + 1)) funs
    in header ++ attrsS ++ staticS ++ funsS

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
--   Nested blocks are lifted in depth-first order.
flattenIRProgm :: IRProgm -> IRProgm
flattenIRProgm (IRProgm pkg classes) = IRProgm pkg (map flattenIRClass classes)

flattenIRClass :: IRClass -> IRClass
flattenIRClass (IRClass decl name fields (StaticInit stmts) funs) = 
    IRClass decl name fields (StaticInit (flattenTopStmts stmts)) (map flattenIRFunction funs)

flattenIRFunction :: IRFunction -> IRFunction
flattenIRFunction (IRFunction acc name sig stmts) =
    IRFunction acc name sig (flattenTopStmts stmts)

flattenTopStmts :: [IRStmt] -> [IRStmt]
flattenTopStmts [] = []
flattenTopStmts (IRInstr instr : ss) = IRInstr instr : flattenTopStmts ss
flattenTopStmts (IRBlockStmt blk : ss) =
    let (blk', lifted) = flattenBlock blk
    in IRBlockStmt blk' : lifted ++ flattenTopStmts ss

flattenBlock :: IRBlock -> (IRBlock, [IRStmt])
flattenBlock (IRBlock (bid, stmts)) =
    let (flatBody, lifted) = flattenBlockStmts stmts
    in (IRBlock (bid, flatBody), lifted)

flattenBlockStmts :: [IRStmt] -> ([IRStmt], [IRStmt])
flattenBlockStmts [] = ([], [])
flattenBlockStmts (IRInstr instr : ss) =
    let (flatRest, liftedRest) = flattenBlockStmts ss
    in (IRInstr instr : flatRest, liftedRest)
flattenBlockStmts (IRBlockStmt blk : ss) =
    let (blk', liftedInside) = flattenBlock blk
        (flatRest, liftedRest) = flattenBlockStmts ss
    in (flatRest, IRBlockStmt blk' : liftedInside ++ liftedRest)


-- | Remove redundant gotos:
--   1) `goto Lx` immediately followed by `.Lx:`
--   2) trailing `goto Lx` at end of a block when the next block is `.Lx:`
pruneIRProgm :: IRProgm -> IRProgm
pruneIRProgm (IRProgm pkg classes) = IRProgm pkg (map pruneIRClass classes)

pruneIRClass :: IRClass -> IRClass
pruneIRClass (IRClass decl name fields (StaticInit stmts) funs) =
    IRClass decl name fields (StaticInit (pruneTopStmts stmts)) (map pruneIRFunction funs)

pruneIRFunction :: IRFunction -> IRFunction
pruneIRFunction (IRFunction acc name sig stmts) =
    IRFunction acc name sig (pruneTopStmts stmts)

pruneTopStmts :: [IRStmt] -> [IRStmt]
pruneTopStmts [] = []
pruneTopStmts [s] = [s]
pruneTopStmts (s1:s2:rest) =
    case (s1, s2) of
        (IRInstr (Jump tgt), IRBlockStmt (IRBlock (bid, _))) | tgt == bid ->
            pruneTopStmts (s2:rest)
        (IRBlockStmt (IRBlock (bid, stmts)), IRBlockStmt (IRBlock (nextId, _))) ->
            let stmts' = dropTrailingJump stmts nextId
            in IRBlockStmt (IRBlock (bid, stmts')) : pruneTopStmts (s2:rest)
        _ ->
            s1 : pruneTopStmts (s2:rest)


dropTrailingJump :: [IRStmt] -> Int -> [IRStmt]
dropTrailingJump stmts nextId = case reverse stmts of
    (IRInstr (Jump tgt) : rest) | tgt == nextId -> reverse rest
    _ -> stmts

-- | Remove empty blocks after pruning.
rmEBInProg :: IRProgm -> IRProgm
rmEBInProg (IRProgm pkg classes) = IRProgm pkg (map rmEBInClass classes)

rmEBInClass :: IRClass -> IRClass
rmEBInClass (IRClass decl name fields (StaticInit stmts) funs) =
    IRClass decl name fields (StaticInit (rmEBInStmts stmts)) (map rmEBInFunc funs)

rmEBInFunc :: IRFunction -> IRFunction
rmEBInFunc (IRFunction acc name sig stmts) =
    IRFunction acc name sig (unwrapSingleBlock (rmEBInStmts stmts))
    where
        unwrapSingleBlock :: [IRStmt] -> [IRStmt]
        unwrapSingleBlock [IRBlockStmt (IRBlock (_, stmts'))] = stmts'
        unwrapSingleBlock stmts' = stmts'

rmEBInStmts :: [IRStmt] -> [IRStmt]
rmEBInStmts = fixpoint
    where
        fixpoint :: [IRStmt] -> [IRStmt]
        fixpoint stmts =
            let stmts' = pruneTopStmts (rmEmptyOnce stmts)
            in if stmts' == stmts then stmts else fixpoint stmts'

        rmEmptyOnce :: [IRStmt] -> [IRStmt]
        rmEmptyOnce stmts =
            let redirectMap = buildRedirectMap stmts
                rewritten = map (rewriteStmt redirectMap) stmts
            in filter (not . isEmptyBlock) rewritten

        buildRedirectMap :: [IRStmt] -> Map Int (Maybe Int)
        buildRedirectMap = Map.fromList . go
            where
                go [] = []
                go (IRBlockStmt (IRBlock (bid, stmts)) : rest)
                    | null stmts = (bid, nextBlockId rest) : go rest
                    | otherwise = go rest
                go (_ : rest) = go rest

                nextBlockId :: [IRStmt] -> Maybe Int
                nextBlockId [] = Nothing
                nextBlockId (IRBlockStmt (IRBlock (bid, _)) : _) = Just bid
                nextBlockId (_ : _) = Nothing

        rewriteStmt :: Map Int (Maybe Int) -> IRStmt -> IRStmt
        rewriteStmt redirectMap stmt = case stmt of
            IRInstr instr -> IRInstr (rewriteInstr redirectMap instr)
            IRBlockStmt (IRBlock (bid, stmts)) ->
                IRBlockStmt (IRBlock (bid, map (rewriteStmt redirectMap) stmts))

        rewriteInstr :: Map Int (Maybe Int) -> IRInstr -> IRInstr
        rewriteInstr redirectMap instr = case instr of
            Jump tgt -> Jump (redirect redirectMap tgt)
            ConJump cond tgt -> ConJump cond (redirect redirectMap tgt)
            _ -> instr

        redirect :: Map Int (Maybe Int) -> Int -> Int
        redirect redirectMap tgt =
            case Map.lookup tgt redirectMap of
                Nothing -> tgt
                Just (Just newTgt) -> newTgt
                Just Nothing -> error "invalid ir"

        isEmptyBlock :: IRStmt -> Bool
        isEmptyBlock (IRBlockStmt (IRBlock (_, []))) = True
        isEmptyBlock _ = False


