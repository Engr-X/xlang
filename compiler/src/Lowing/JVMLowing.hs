module Lowing.JVMLowing where

import Control.Monad.State.Strict (State, get, put, runState)
import Data.Map.Strict (Map)
import Parse.SyntaxTree (Class(..), Operator(..))
import Semantic.NameEnv (QName)
import Text.Printf (printf)

import qualified Data.Map.Strict as Map
import qualified IR.TAC as IR
import qualified Lowing.JVM as JVM
import qualified Semantic.TypeEnv as TEnv

ownerTypeToString :: IR.IRMemberType -> String
ownerTypeToString IR.MemberClass = "xlang-class"
ownerTypeToString IR.MemberClassWrapped = "xlang-top-level"


data LowerState = LowerState {
    nextLocal :: Int,
    locals :: Map IR.IRAtom Int,
    paramSlots :: Map Int Int,
    atomTypes :: Map IR.IRAtom Class,
    retType :: Maybe Class,
    retLocal :: Maybe Int
}

float128JvmMsg :: String -> String
float128JvmMsg = printf "float128 is native-only; JVM target does not support float128 (%s)"

rejectFloat128 :: String -> a
rejectFloat128 whereAt = errorWithoutStackTrace (float128JvmMsg whereAt)

ensureAll :: [a] -> (a -> ()) -> ()
ensureAll xs check = go xs
  where
    go [] = ()
    go (x:rest) = check x `seq` go rest

ensureJvmClass :: String -> Class -> ()
ensureJvmClass whereAt cls = case cls of
    Float128T -> rejectFloat128 whereAt
    Array elemCls _ -> ensureJvmClass whereAt elemCls
    _ -> ()

ensureJvmAtom :: IR.IRAtom -> ()
ensureJvmAtom atom = case atom of
    IR.Float128C _ -> rejectFloat128 "literal"
    IR.Phi pairs -> ensureAll (map snd pairs) ensureJvmAtom
    _ -> ()

ensureJvmInstr :: IR.IRInstr -> ()
ensureJvmInstr instr = case instr of
    IR.Jump _ -> ()
    IR.Ifeq a b _ -> ensureJvmAtom a `seq` ensureJvmAtom b
    IR.Ifne a b _ -> ensureJvmAtom a `seq` ensureJvmAtom b
    IR.Iflt a b _ -> ensureJvmAtom a `seq` ensureJvmAtom b
    IR.Ifle a b _ -> ensureJvmAtom a `seq` ensureJvmAtom b
    IR.Ifgt a b _ -> ensureJvmAtom a `seq` ensureJvmAtom b
    IR.Ifge a b _ -> ensureJvmAtom a `seq` ensureJvmAtom b
    IR.SetRet atom -> ensureJvmAtom atom
    IR.Return -> ()
    IR.VReturn -> ()
    IR.IAssign dst src -> ensureJvmAtom dst `seq` ensureJvmAtom src
    IR.IUnary dst _ src -> ensureJvmAtom dst `seq` ensureJvmAtom src
    IR.IBinary dst _ a b -> ensureJvmAtom dst `seq` ensureJvmAtom a `seq` ensureJvmAtom b
    IR.ICast dst (fromC, toC) atom ->
        ensureJvmAtom dst
            `seq` ensureJvmClass "cast-from type" fromC
            `seq` ensureJvmClass "cast-to type" toC
            `seq` ensureJvmAtom atom
    IR.ICall dst _ args -> ensureJvmAtom dst `seq` ensureAll args ensureJvmAtom
    IR.ICallStatic dst _ args -> ensureJvmAtom dst `seq` ensureAll args ensureJvmAtom
    IR.IGetField dst obj _ -> ensureJvmAtom dst `seq` ensureJvmAtom obj
    IR.IPutField obj _ v -> ensureJvmAtom obj `seq` ensureJvmAtom v
    IR.IGetStatic dst _ -> ensureJvmAtom dst
    IR.IPutStatic _ v -> ensureJvmAtom v

ensureJvmBlock :: IR.IRBlock -> ()
ensureJvmBlock (IR.IRBlock (_, instrs)) = ensureAll instrs ensureJvmInstr

ensureJvmFunction :: IR.IRFunction -> ()
ensureJvmFunction (IR.IRFunction _ name sig atomT blocks _) =
    ensureJvmClass ("function " ++ name ++ " return type") (TEnv.funReturn sig)
        `seq` ensureAll (zip [0 :: Int ..] (TEnv.funParams sig))
            (\(idx, cls) -> ensureJvmClass ("function " ++ name ++ " param #" ++ show idx) cls)
        `seq` ensureAll (Map.toList atomT)
            (\(atom, cls) -> ensureJvmAtom atom `seq` ensureJvmClass ("function " ++ name ++ " inferred type") cls)
        `seq` ensureAll blocks ensureJvmBlock

ensureJvmIRClass :: IR.IRClass -> ()
ensureJvmIRClass (IR.IRClass _ name attrs (IR.StaticInit sBody) atomT funs _) =
    ensureAll attrs
        (\(_, cls, fieldName, _) -> ensureJvmClass ("class " ++ name ++ " field " ++ fieldName) cls)
        `seq` ensureAll (Map.toList atomT)
            (\(atom, cls) -> ensureJvmAtom atom `seq` ensureJvmClass ("class " ++ name ++ " static inferred type") cls)
        `seq` ensureAll sBody ensureJvmBlock
        `seq` ensureAll funs ensureJvmFunction


jvmLowerBlocks :: [IR.IRBlock] -> State LowerState [JVM.JCommand]
jvmLowerBlocks blocks = fmap concat (mapM lowerBlock blocks)


lowerBlock :: IR.IRBlock -> State LowerState [JVM.JCommand]
lowerBlock (IR.IRBlock (bid, instrs)) = do
    ops <- fmap concat (mapM lowerInstr instrs)
    let cmds = map JVM.OP ops
    return [JVM.Label (bid, cmds)]


-- | Lower a single IR instruction into JVM ops (stack machine).
lowerInstr :: IR.IRInstr -> State LowerState [JVM.JOP]
lowerInstr (IR.Jump bid) = return  [JVM.Goto bid]
lowerInstr (IR.Ifeq a b bids) = lowerCmp CmpEq a b bids
lowerInstr (IR.Ifne a b bids) = lowerCmp CmpNe a b bids
lowerInstr (IR.Iflt a b bids) = lowerCmp CmpLt a b bids
lowerInstr (IR.Ifle a b bids) = lowerCmp CmpLe a b bids
lowerInstr (IR.Ifgt a b bids) = lowerCmp CmpGt a b bids
lowerInstr (IR.Ifge a b bids) = lowerCmp CmpGe a b bids
lowerInstr (IR.SetRet atom) = do
    ops <- loadAtom atom
    st <- get
    cls <- atomClass atom
    case retLocal st of
        Just idx -> return (ops ++ [JVM.Store cls idx])
        Nothing -> error "SetRet in void function"

lowerInstr IR.Return = do
    st <- get
    case (retType st, retLocal st) of
        (Just cls, Just idx) -> return [JVM.Load cls idx, JVM.ReturnWV cls]
        _ -> error "Return in void function"
lowerInstr IR.VReturn = return [JVM.Return]

lowerInstr (IR.IAssign dst src) = do
    srcOps <- loadAtom src
    srcCls <- atomClass src
    dstCls <- atomClass dst
    dstOps <- storeAtom dst
    let castOps = ([JVM.Cast srcCls dstCls | not (isNoOpCast srcCls dstCls)])
    return  (srcOps ++ castOps ++ dstOps)

lowerInstr (IR.IUnary dst BitInv src) = do
    srcOps <- loadAtom src
    cls <- atomClass dst
    dstOps <- storeAtom dst
    return $ concat [srcOps, bitInvOps cls, dstOps]

lowerInstr (IR.IUnary dst LogicalNot src) = do
    srcOps <- loadAtom src
    cls <- atomClass dst
    dstOps <- storeAtom dst
    return $ concat [srcOps, logicalNotOps cls, dstOps]

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

lowerInstr (IR.IBinary dst BitXnor a b) = do
    aOps <- loadAtom a
    bOps <- loadAtom b
    dstOps <- storeAtom dst
    cls <- atomClass dst
    return $ concat [aOps, bOps, [JVM.Xor cls], bitInvOps cls, dstOps]

lowerInstr (IR.IBinary dst BitNor a b) = do
    aOps <- loadAtom a
    bOps <- loadAtom b
    dstOps <- storeAtom dst
    cls <- atomClass dst
    return $ concat [aOps, bOps, [JVM.Or cls], bitInvOps cls, dstOps]

lowerInstr (IR.IBinary dst BitNand a b) = do
    aOps <- loadAtom a
    bOps <- loadAtom b
    dstOps <- storeAtom dst
    cls <- atomClass dst
    return $ concat [aOps, bOps, [JVM.And cls], bitInvOps cls, dstOps]

lowerInstr (IR.IBinary dst BitImply a b) = do
    aOps <- loadAtom a
    bOps <- loadAtom b
    dstOps <- storeAtom dst
    cls <- atomClass dst
    return $ concat [aOps, bitInvOps cls, bOps, [JVM.Or cls], dstOps]

lowerInstr (IR.IBinary dst BitNimply a b) = do
    aOps <- loadAtom a
    bOps <- loadAtom b
    dstOps <- storeAtom dst
    cls <- atomClass dst
    return $ concat [aOps, bOps, bitInvOps cls, [JVM.And cls], dstOps]

lowerInstr (IR.IBinary dst op a b) = do
    aOps <- loadAtom a
    bOps <- loadAtom b
    dstOps <- storeAtom dst
    cls <- atomClass dst
    let mkOp = binaryOp op
    return $ concat [aOps, bOps, [mkOp cls], dstOps]
lowerInstr (IR.ICast dst (fromC, toC) atom) = do
    atomOps <- loadAtom atom
    dstOps <- storeAtom dst
    if isNoOpCast fromC toC
        then return (atomOps ++ dstOps)
        else return (atomOps ++ [JVM.Cast fromC toC] ++ dstOps)
lowerInstr (IR.ICall {}) = error "ICall is not supported; use ICallStatic"
lowerInstr (IR.ICallStatic dst qname args) = do
    argOps <- loadArgs args
    sig <- callSig dst args
    retCls <- atomClass dst
    case retCls of
        Void -> return (argOps ++ [JVM.InvokeStatic qname sig])
        _ -> do
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


data CmpKind = CmpEq | CmpNe | CmpLt | CmpLe | CmpGt | CmpGe

lowerCmp :: CmpKind -> IR.IRAtom -> IR.IRAtom -> (Int, Int) -> State LowerState [JVM.JOP]
lowerCmp kind a b (thenBid, elseBid) = do
    aOps <- loadAtom a
    bOps <- loadAtom b
    clsA <- atomClass a
    clsB <- atomClass b
    let cmpCls = pickCmpClass clsA clsB
        op = cmpOp kind cmpCls thenBid
    return (concat [aOps, bOps, [op, JVM.Goto elseBid]])

pickCmpClass :: Class -> Class -> Class
pickCmpClass a b
    | isFloat64Like a || isFloat64Like b = Float64T
    | isFloat32Like a || isFloat32Like b = Float32T
    | isLongLike a || isLongLike b = Int64T
    | isIntLike a && isIntLike b = Int32T
    | otherwise = error ("unsupported cmp types: " ++ show a ++ ", " ++ show b)
    where
        isFloat32Like cls = cls == Float32T
        isFloat64Like cls = cls `elem` [Float64T, Float128T]
        isLongLike cls = cls == Int64T
        isIntLike cls = cls `elem` [Bool, Char, Int8T, Int16T, Int32T]

cmpOp :: CmpKind -> Class -> Int -> JVM.JOP
cmpOp kind cls bid = case kind of
    CmpEq -> JVM.IfcmpEq cls bid
    CmpNe -> JVM.IfcmpNe cls bid
    CmpLt -> JVM.IfcmpLt cls bid
    CmpLe -> JVM.IfcmpLe cls bid
    CmpGt -> JVM.IfcmpGt cls bid
    CmpGe -> JVM.IfcmpGe cls bid


castGroupMap :: Map Class Char
castGroupMap = Map.fromList [
    (Int8T, 'i'),
    (Int16T, 'i'),
    (Int32T, 'i'),
    (Bool, 'i'),
    (Char, 'i'),
    (Int64T, 'l'),
    (Float32T, 'f'),
    (Float64T, 'd'),
    (Float128T, 'd')]


isNoOpCast :: Class -> Class -> Bool
isNoOpCast fromC toC = castGroup fromC == castGroup toC
    where
        castGroup :: Class -> Maybe Char
        castGroup cls = case cls of
            Class _ _ -> Just 'a'
            Array _ _ -> Just 'a'
            _ -> Map.lookup cls castGroupMap


bitInvConstMap :: Map Class JVM.JValue
bitInvConstMap = Map.fromList [
    (Int64T, JVM.JL (-1)),
    (Bool, JVM.JI (-1)),
    (Char, JVM.JI (-1)),
    (Int8T, JVM.JI (-1)),
    (Int16T, JVM.JI (-1)),
    (Int32T, JVM.JI (-1))]


-- | Expand bitwise inversion to primitive JVM ops (x ^ -1).
bitInvOps :: Class -> [JVM.JOP]
bitInvOps cls =
    let c = Map.findWithDefault (error ("unsupported bit inv type: " ++ show cls))
                cls
                bitInvConstMap
    in [JVM.CPush c, JVM.Xor cls]


-- | Expand logical not for bool (0/1) to xor with 1.
logicalNotOps :: Class -> [JVM.JOP]
logicalNotOps Bool = [JVM.CPush (JVM.JI 1), JVM.Xor Bool]
logicalNotOps cls = error ("unsupported logical not type: " ++ show cls)


-- | Map unary IR operators to JVM ops; unsupported ops are errors.
unaryOp :: Operator -> Class -> JVM.JOP
unaryOp UnaryMinus cls = JVM.Neg cls
unaryOp LogicalNot _ = error "LogicalNot should be expanded in lowerInstr"
unaryOp BitInv _ = error "BitInv should be expanded in lowerInstr"
unaryOp UnaryPlus _ = error "unsupported unary op: UnaryPlus"
unaryOp op _ = error ("unsupported unary op: " ++ show op)


-- | Map binary IR operators to JVM ops; unsupported ops are errors.
binaryOp :: Operator -> (Class -> JVM.JOP)
binaryOp op = Map.findWithDefault unsupported op binaryOpMap
    where
        unsupported _ = error ("unsupported binary op: " ++ show op)

binaryOpMap :: Map Operator (Class -> JVM.JOP)
binaryOpMap = Map.fromList [
    (Add, JVM.Add),
    (Sub, JVM.Sub),
    (Mul, JVM.Mul),
    (Div, JVM.Div),
    (Mod, JVM.Rem),
    (BitAnd, JVM.And),
    (BitOr, JVM.Or),
    (BitXor, JVM.Xor),
    (LogicalAnd, JVM.And),
    (LogicalOr, JVM.Or),
    (BitLShift, JVM.Shl),
    (BitRShift, JVM.Shr),
    (BitURShift, JVM.UShr)]


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
    IR.StringC _ -> return (Class ["java", "lang", "String"] [])
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
    cls <- atomClass atom
    dstOps <- storeAtom atom
    case cls of
        Class _ _ -> return (JVM.PushNull : dstOps)
        Array _ _ -> return (JVM.PushNull : dstOps)
        _ -> do
            c <- defaultConst atom
            return (JVM.CPush c : dstOps)


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
atomToConst (IR.StringC s) = Just (JVM.JString s)
atomToConst _ = Nothing


-- | Lower a single IR function into JVM commands with a fresh local mapping.
jvmLowingFun :: IR.IRFunction -> JVM.JFunction
jvmLowingFun (IR.IRFunction decl name sig atomT body ownerType) =
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
        (cmds, _) = runState (jvmLowerBlocks body) initState
    in JVM.JFunction decl name sig (ownerTypeToString ownerType) cmds


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
        (cmds, _) = runState (jvmLowerBlocks body) initState
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
jvmClassLowing pkg irClass@(IR.IRClass decl name attrs sInit atomT funs mainKind) =
    ensureJvmIRClass irClass `seq`
    let qname = if null pkg then [name] else pkg ++ [name]
        extendQ = []
        interfaces = []
        fields = map (\(d, cls, fname, ownerType) -> JVM.JField d cls fname (ownerTypeToString ownerType)) attrs
        clinit = jvmClinitLowing sInit atomT
        inits = []
        methods = map jvmLowingFun funs
    in JVM.JClass decl qname extendQ interfaces fields clinit inits methods mainKind


-- | Lower a whole IR program into JVM classes.
jvmProgmLowing :: IR.IRProgm -> [JVM.JClass]
jvmProgmLowing (IR.IRProgm pkg classes) =
    ensureAll classes ensureJvmIRClass `seq`
    map (jvmClassLowing pkg) classes


-- | Lower IR programs into JVM classes.
jvmProgmsLowing :: [IR.IRProgm] -> [JVM.JClass]
jvmProgmsLowing = concatMap jvmProgmLowing


