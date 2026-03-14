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
float128JvmMsg whereAt =
    printf "float128 is native-only; JVM target does not support float128 (%s)" whereAt

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
    IR.SetIRet atom -> ensureJvmAtom atom
    IR.IReturn -> ()
    IR.Return -> ()
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

ensureJvmStmt :: IR.IRStmt -> ()
ensureJvmStmt stmt = case stmt of
    IR.IRInstr instr -> ensureJvmInstr instr
    IR.IRBlockStmt (IR.IRBlock (_, stmts)) -> ensureAll stmts ensureJvmStmt

ensureJvmFunction :: IR.IRFunction -> ()
ensureJvmFunction (IR.IRFunction _ name sig atomT body _) =
    ensureJvmClass ("function " ++ name ++ " return type") (TEnv.funReturn sig)
        `seq` ensureAll (zip [0 :: Int ..] (TEnv.funParams sig))
            (\(idx, cls) -> ensureJvmClass ("function " ++ name ++ " param #" ++ show idx) cls)
        `seq` ensureAll (Map.toList atomT)
            (\(atom, cls) -> ensureJvmAtom atom `seq` ensureJvmClass ("function " ++ name ++ " inferred type") cls)
        `seq` ensureAll body ensureJvmStmt

ensureJvmIRClass :: IR.IRClass -> ()
ensureJvmIRClass (IR.IRClass _ name attrs (IR.StaticInit sBody) atomT funs _) =
    ensureAll attrs
        (\(_, cls, fieldName, _) -> ensureJvmClass ("class " ++ name ++ " field " ++ fieldName) cls)
        `seq` ensureAll (Map.toList atomT)
            (\(atom, cls) -> ensureJvmAtom atom `seq` ensureJvmClass ("class " ++ name ++ " static inferred type") cls)
        `seq` ensureAll sBody ensureJvmStmt
        `seq` ensureAll funs ensureJvmFunction


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
lowerInstr (IR.Ifeq a b bid) = lowerCmp CmpEq a b bid
lowerInstr (IR.Ifne a b bid) = lowerCmp CmpNe a b bid
lowerInstr (IR.Iflt a b bid) = lowerCmp CmpLt a b bid
lowerInstr (IR.Ifle a b bid) = lowerCmp CmpLe a b bid
lowerInstr (IR.Ifgt a b bid) = lowerCmp CmpGt a b bid
lowerInstr (IR.Ifge a b bid) = lowerCmp CmpGe a b bid
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
    srcCls <- atomClass src
    dstCls <- atomClass dst
    dstOps <- storeAtom dst
    let castOps = if isNoOpCast srcCls dstCls then [] else [JVM.Cast srcCls dstCls]
    return  (srcOps ++ castOps ++ dstOps)
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

lowerCmp :: CmpKind -> IR.IRAtom -> IR.IRAtom -> Int -> State LowerState [JVM.JOP]
lowerCmp kind a b bid = do
    aOps <- loadAtom a
    bOps <- loadAtom b
    clsA <- atomClass a
    clsB <- atomClass b
    let cmpCls = pickCmpClass clsA clsB
        op = cmpOp kind cmpCls bid
    return (aOps ++ bOps ++ [op])

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


isNoOpCast :: Class -> Class -> Bool
isNoOpCast fromC toC = castGroup fromC == castGroup toC
    where
        castGroup :: Class -> Maybe Char
        castGroup cls = case cls of
            Int8T -> Just 'i'
            Int16T -> Just 'i'
            Int32T -> Just 'i'
            Bool -> Just 'i'
            Char -> Just 'i'
            Int64T -> Just 'l'
            Float32T -> Just 'f'
            Float64T -> Just 'd'
            Float128T -> Just 'd'
            Class _ _ -> Just 'a'
            Array _ _ -> Just 'a'
            _ -> Nothing


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
        (cmds, _) = runState (jvmLowerStmts body) initState
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
