module Lowing.JVMLowing where

import Control.Monad.State.Strict (State, get, put, runState)
import Data.Map.Strict (Map)
import Parse.SyntaxTree (Class(..), Operator(..))
import Semantic.NameEnv (QName)

import qualified Data.Map.Strict as Map
import qualified IR.TAC as IR
import qualified Lowing.JVM as JVM
import qualified Semantic.TypeEnv as TEnv


data LowerState = LowerState {
    nextLocal :: Int,
    locals :: Map IR.IRAtom Int,
    paramSlots :: Map Int Int,
    atomTypes :: Map IR.IRAtom Class,
    retType :: Maybe Class,
    retLocal :: Maybe Int
}

-- | Backward-compatible name (typo-friendly).
jvmCIinitLowing :: IR.StaticInit -> Map IR.IRAtom Class -> JVM.JClinit
jvmCIinitLowing = jvmClinitLowing


-- | Lower a list of IR statements into JVM command stream.
jvmLowerStmts :: [IR.IRStmt] -> State LowerState [JVM.JCommand]
jvmLowerStmts stmts = fmap concat (mapM lowerStmt stmts)


-- | Lower a single IR statement (instruction or block) into JVM commands.
lowerStmt :: IR.IRStmt -> State LowerState [JVM.JCommand]
lowerStmt stmt = case stmt of
    IR.IRInstr instr -> do
        ops <- lowerInstr instr
        return (map JVM.OP ops)
    IR.IRBlockStmt (IR.IRBlock (bid, stmts)) -> do
        cmds <- jvmLowerStmts stmts
        return [JVM.Label (bid, cmds)]


-- | Lower a single IR instruction into JVM ops (stack machine).
lowerInstr :: IR.IRInstr -> State LowerState [JVM.JOP]
lowerInstr (IR.Jump bid) = return  [JVM.Goto bid]
lowerInstr (IR.ConJump cond bid) = do
    condOps <- loadAtom cond
    return  (condOps ++ [JVM.Ifne bid])
lowerInstr (IR.SetIRet atom) = do
    ops <- loadAtom atom
    st <- get
    cls <- atomClass atom
    case retLocal st of
        Just idx -> return (ops ++ [JVM.Store cls idx])
        Nothing -> error "SetIRet in void function"

lowerInstr IR.IReturn = do
    st <- get
    case (retType st, retLocal st) of
        (Just cls, Just idx) -> return [JVM.Load cls idx, JVM.ReturnWV cls]
        _ -> error "IReturn in void function"
lowerInstr IR.Return = return [JVM.Return]

lowerInstr (IR.IAssign dst src) = do
    srcOps <- loadAtom src
    dstOps <- storeAtom dst
    return  (srcOps ++ dstOps)
lowerInstr (IR.IUnary dst op src) = do
    srcOps <- loadAtom src
    cls <- atomClass dst
    let jop = unaryOp op cls
    dstOps <- storeAtom dst
    return $ concat [srcOps, [jop], dstOps]

lowerInstr (IR.IBinary dst Pow a b) = do
    aOps <- loadAtom a
    bOps <- loadAtom b
    dstOps <- storeAtom dst

    let powFunSig = TEnv.FunSig {
        TEnv.funParams = [Float64T, Float64T],
        TEnv.funReturn = Float64T
    }

    return $ concat [
        aOps, bOps,
        [JVM.InvokeStatic ["java", "lang", "Math", "pow"] powFunSig], dstOps]

lowerInstr (IR.IBinary dst op a b) = do
    case binaryOp op of
        Nothing -> error "not implemnt"
        Just mkOp -> do
            aOps <- loadAtom a
            bOps <- loadAtom b
            dstOps <- storeAtom dst
            cls <- atomClass dst

            return $ concat [aOps, bOps, [mkOp cls], dstOps]
lowerInstr (IR.ICast dst (fromC, toC) atom) = do
    atomOps <- loadAtom atom
    let castOp = JVM.Cast fromC toC
    dstOps <- storeAtom dst
    return $ concat [atomOps, [castOp], dstOps]
lowerInstr (IR.ICall {}) = error "ICall is not supported; use ICallStatic"
lowerInstr (IR.ICallStatic dst qname args) = do
    argOps <- loadArgs args
    sig <- callSig dst args
    dstOps <- storeAtom dst
    return (argOps ++ [JVM.InvokeStatic qname sig] ++ dstOps)
lowerInstr (IR.IGetStatic dst qname) = do
    cls <- atomClass dst
    dstOps <- storeAtom dst
    return (JVM.GetStatic cls qname : dstOps)
lowerInstr (IR.IPutStatic qname v) = do
    vOps <- loadAtom v
    cls <- atomClass v
    return (vOps ++ [JVM.PutStatic cls qname])
lowerInstr (IR.IGetField {}) =
    error "IGetField is not supported yet"
lowerInstr (IR.IPutField {}) =
    error "IPutField is not supported yet"


-- | Map unary IR operators to JVM ops; unsupported ops are errors.
unaryOp :: Operator -> Class -> JVM.JOP
unaryOp UnaryMinus cls = JVM.Neg cls
unaryOp UnaryPlus _ = error "unsupported unary op: UnaryPlus"
unaryOp op _ = error ("unsupported unary op: " ++ show op)


-- | Map binary IR operators to JVM ops when supported.
binaryOp :: Operator -> Maybe (Class -> JVM.JOP)
binaryOp Add = Just JVM.Add
binaryOp Sub = Just JVM.Sub
binaryOp Mul = Just JVM.Mul
binaryOp Div = Just JVM.Div
binaryOp Mod = Just JVM.Rem
binaryOp BitAnd = Just JVM.And
binaryOp BitOr = Just JVM.Or
binaryOp BitXor = Just JVM.Xor
binaryOp BitLShift = Just JVM.Shl
binaryOp BitRShift = Just JVM.Shr
binaryOp _ = Nothing


-- | Emit ops to load an atom onto the JVM stack.
loadAtom :: IR.IRAtom -> State LowerState [JVM.JOP]
loadAtom atom = case atomToConst atom of
    Just c -> return  [JVM.CPush c]
    Nothing -> do
        idx <- ensureLocal atom
        cls <- atomClass atom
        return  [JVM.Load cls idx]


-- | Emit ops to load call arguments left-to-right onto the JVM stack.
loadArgs :: [IR.IRAtom] -> State LowerState [JVM.JOP]
loadArgs args = fmap concat (mapM loadAtom args)


-- | Build a call signature from dst/args types.
callSig :: IR.IRAtom -> [IR.IRAtom] -> State LowerState TEnv.FunSig
callSig dst args = do
    retT <- atomClass dst
    argTs <- mapM atomClass args
    return TEnv.FunSig { TEnv.funParams = argTs, TEnv.funReturn = retT }


atomClass :: IR.IRAtom -> State LowerState Class
atomClass atom = case atom of
    IR.BoolC _ -> return Bool
    IR.CharC _ -> return Char
    IR.Int8C _ -> return Int8T
    IR.Int16C _ -> return Int16T
    IR.Int32C _ -> return Int32T
    IR.Int64C _ -> return Int64T
    IR.Float32C _ -> return Float32T
    IR.Float64C _ -> return Float64T
    IR.Float128C _ -> return Float128T
    _ -> do
        st <- get
        case Map.lookup atom (atomTypes st) of
            Just cls -> return cls
            Nothing -> error ("missing atom type for call: " ++ show atom)


-- | Emit ops to store stack top into a local for the given atom.
storeAtom :: IR.IRAtom -> State LowerState [JVM.JOP]
storeAtom atom = do
    idx <- ensureLocal atom
    cls <- atomClass atom
    return  [JVM.Store cls idx]


-- | Ensure a local slot exists for the atom and return its index.
ensureLocal :: IR.IRAtom -> State LowerState Int
ensureLocal atom = case atom of
    IR.Param i -> do
        st <- get
        case Map.lookup i (paramSlots st) of
            Just idx -> return  idx
            Nothing -> error ("missing param slot for index: " ++ show i)
    _ -> do
        st <- get
        case Map.lookup atom (locals st) of
            Just idx -> return  idx
            Nothing -> do
                cls <- atomClass atom
                let idx = nextLocal st
                    nextIdx = idx + slotSize cls
                put st { nextLocal = nextIdx, locals = Map.insert atom idx (locals st) }
                return  idx


-- | Emit ops to push a default value for the atom's type and store it.
defaultValue :: IR.IRAtom -> State LowerState [JVM.JOP]
defaultValue atom = do
    c <- defaultConst atom
    dstOps <- storeAtom atom
    return  (JVM.CPush c : dstOps)


-- | Compute a zero-like constant based on the atom's inferred type.
defaultConst :: IR.IRAtom -> State LowerState JVM.JConst
defaultConst atom = do
    st <- get
    let mCls = Map.lookup atom (atomTypes st)
    return  $ case mCls of
        Just Int64T -> JVM.JL 0
        Just Float32T -> JVM.JF 0
        Just Float64T -> JVM.JD 0
        Just Float128T -> JVM.JD 0
        _ -> JVM.JI 0


-- | Convert literal atoms to JVM constants when possible.
atomToConst :: IR.IRAtom -> Maybe JVM.JConst
atomToConst (IR.Int32C i) = Just (JVM.JI i)
atomToConst (IR.Int64C i) = Just (JVM.JL i)
atomToConst (IR.Float32C f) = Just (JVM.JF (realToFrac f))
atomToConst (IR.Float64C f) = Just (JVM.JD f)
atomToConst (IR.Float128C r) =  Just (JVM.JD (fromRational r))
atomToConst (IR.BoolC b) = Just (JVM.JI (if b then 1 else 0))
atomToConst (IR.CharC c) = Just (JVM.JI (fromEnum c))
atomToConst _ = Nothing


-- | Lower a single IR function into JVM commands with a fresh local mapping.
jvmLowingFun :: IR.IRFunction -> JVM.JFunction
jvmLowingFun (IR.IRFunction decl name sig atomT body) =
    let (paramSlotMap, nextAfterParams) = buildParamSlots (TEnv.funParams sig)
        retLocalSlot = case TEnv.funReturn sig of
            Void -> Nothing
            _ -> Just nextAfterParams
        nextLocal0 = case TEnv.funReturn sig of
            Void -> nextAfterParams
            cls -> nextAfterParams + slotSize cls
        initState = LowerState {
            nextLocal = nextLocal0,
            locals = Map.empty,
            paramSlots = paramSlotMap,
            atomTypes = atomT,
            retType = case TEnv.funReturn sig of
                Void -> Nothing
                cls -> Just cls,
            retLocal = retLocalSlot
        }
        (cmds, _) = runState (jvmLowerStmts body) initState
    in JVM.JFunction decl name sig cmds


-- | Lower a static initializer into JVM <clinit> commands.
jvmClinitLowing :: IR.StaticInit -> Map IR.IRAtom Class -> JVM.JClinit
jvmClinitLowing (IR.StaticInit body) atomT =
    let initState = LowerState {
            nextLocal = 0,
            locals = Map.empty,
            paramSlots = Map.empty,
            atomTypes = atomT,
            retType = Nothing,
            retLocal = Nothing
        }
        (cmds, _) = runState (jvmLowerStmts body) initState
    in JVM.JClinit cmds


-- | JVM local slot size for a class (long/double are wide).
slotSize :: Class -> Int
slotSize cls = case cls of
    Int64T -> 2
    Float64T -> 2
    Float128T -> 2
    _ -> 1


-- | Build parameter index -> local slot mapping and return next free slot.
buildParamSlots :: [Class] -> (Map Int Int, Int)
buildParamSlots params = foldl step (Map.empty, 0) (zip [0..] params)
    where
        step :: (Map Int Int, Int) -> (Int, Class) -> (Map Int Int, Int)
        step (m, nextIdx) (i, cls) =
            (Map.insert i nextIdx m, nextIdx + slotSize cls)


-- | Lower an IR class into JVM class (constructors not supported yet).
jvmClassLowing :: QName -> IR.IRClass -> JVM.JClass
jvmClassLowing pkg (IR.IRClass decl name attrs sInit atomT funs) =
    let qname = if null pkg then [name] else pkg ++ [name]
        extendQ = []
        interfaces = []
        fields = map (\(d, cls, fname) -> JVM.JField d cls fname) attrs
        clinit = jvmClinitLowing sInit atomT
        inits = []
        methods = map jvmLowingFun funs
    in JVM.JClass decl qname extendQ interfaces fields clinit inits methods


-- | Lower a whole IR program into JVM classes.
jvmProgmLowing :: IR.IRProgm -> [JVM.JClass]
jvmProgmLowing (IR.IRProgm pkg classes) =
    map (jvmClassLowing pkg) classes
