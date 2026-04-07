{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE TupleSections #-}


module IR.TACLowing where

import Data.Bits ((.&.))
import Control.Monad.State.Strict (evalState)
import Data.List (partition, foldl')
import Data.Char (toUpper, toLower)
import Text.Read (readMaybe)
import Data.Maybe (listToMaybe, fromMaybe, catMaybes)
import Data.Foldable (foldrM)
import Data.Map.Strict (Map)
import Numeric (readHex)

import Lex.Token (Token(..), tokenPos)
import Parse.SyntaxTree (Block, Class(..), Expression, Program, Statement)
import Semantic.OpInfer (binaryOpCastType, inferUnaryOp, inferBinaryOp, isBasicType, isCompareOp, promoteBasicType)
import IR.TAC (IRInstr, IRAtom, TACM, IRProgm, IRFunction, IRClass, newSubVar, newSubCVar, getVar, peekVarStack,
    resolveVarKey,
    getAtomType, incBlockId, getCurrentLoop, withLoop, getCurrentFun, getCurrentFunMaybe, addStaticVars, isStaticVar,
    getVarStacks, setVarStacks, pushLoopPhis, popLoopPhis, getCurrentLoopPhis, getAtomTypes)
import Semantic.NameEnv (QName)
import Util.Exception (Warning(..))
import Util.Type (Path, Position)

import qualified Data.Map.Strict as Map
import qualified Parse.ParserBasic as PB
import qualified Parse.SyntaxTree as AST
import qualified Semantic.TypeEnv as TEnv
import qualified IR.TAC as TAC
import qualified Util.Exception as UE


-- | Local lowering stream node.
--   We keep nested block markers here while building control flow, then
--   pack everything into final TAC blocks.
data IRNode
    = IRInstr IRInstr
    | IRBlockStmt (Int, [IRNode])
    deriving (Eq, Show)


-- | Local packed block used during stream -> block conversion.
--   Body is already flat instructions, matching final TAC block shape.
newtype IRBlock = IRBlock (Int, [IRInstr])
    deriving (Eq, Show)


-- | Lift nested block markers to the top level in depth-first order.
--   After this pass, a nested block no longer appears inside another block body.
flattenTopStmts :: [IRNode] -> [IRNode]
flattenTopStmts [] = []
flattenTopStmts (IRInstr instr : ss) = IRInstr instr : flattenTopStmts ss
flattenTopStmts (IRBlockStmt blk : ss) =
    let (blk', lifted) = flattenBlock blk
    in IRBlockStmt blk' : lifted ++ flattenTopStmts ss


flattenBlock :: (Int, [IRNode]) -> ((Int, [IRNode]), [IRNode])
flattenBlock (bid, stmts) =
    let (flatBody, lifted) = flattenBlockStmts stmts
    in ((bid, flatBody), lifted)


flattenBlockStmts :: [IRNode] -> ([IRNode], [IRNode])
flattenBlockStmts [] = ([], [])
flattenBlockStmts (IRInstr instr : ss) =
    let (flatRest, liftedRest) = flattenBlockStmts ss
    in (IRInstr instr : flatRest, liftedRest)
flattenBlockStmts (IRBlockStmt blk : ss) =
    let (blk', liftedInside) = flattenBlock blk
        (flatRest, liftedRest) = flattenBlockStmts ss
    in (flatRest, IRBlockStmt blk' : liftedInside ++ liftedRest)


-- | Convert a mixed IRNode stream into final TAC blocks.
--   Any leading instructions are grouped into a synthetic entry block.
packIRBlocks :: [IRNode] -> [TAC.IRBlock]
packIRBlocks = map toTACBlock . packLocalBlocks
    where
        toTACBlock :: IRBlock -> TAC.IRBlock
        toTACBlock (IRBlock (bid, body)) = TAC.IRBlock (bid, body)

        toInstr :: IRNode -> IRInstr
        toInstr (IRInstr instr) = instr
        toInstr (IRBlockStmt _) = error "packIRBlocks: nested block remains after flatten"

        mkBlock :: Int -> [IRNode] -> IRBlock
        mkBlock bid revBody = IRBlock (bid, map toInstr (reverse revBody))

        packLocalBlocks :: [IRNode] -> [IRBlock]
        packLocalBlocks stmts =
            let flat = flattenTopStmts stmts
                bids = [bid | IRBlockStmt (bid, _) <- flat]
                entryBid = if null bids then 0 else minimum bids - 1
                (mCur, accRev) = go entryBid Nothing [] flat
            in reverse (finish mCur accRev)

        go :: Int -> Maybe (Int, [IRNode]) -> [IRBlock] -> [IRNode] -> (Maybe (Int, [IRNode]), [IRBlock])
        go _ mCur acc [] = (mCur, acc)
        go entryBid mCur acc (stmt:rest) =
            case stmt of
                IRInstr instr ->
                    let nextCur = case mCur of
                            Just (bid, revBody) -> Just (bid, IRInstr instr : revBody)
                            Nothing -> Just (entryBid, [IRInstr instr])
                    in go entryBid nextCur acc rest
                IRBlockStmt (bid, body) ->
                    let acc' = case mCur of
                            Just (cbid, revBody) -> mkBlock cbid revBody : acc
                            Nothing -> acc
                        nextCur = Just (bid, reverse body)
                    in go entryBid nextCur acc' rest

        finish :: Maybe (Int, [IRNode]) -> [IRBlock] -> [IRBlock]
        finish mCur acc = case mCur of
            Just (bid, revBody) -> mkBlock bid revBody : acc
            Nothing -> acc


-- | Path placeholder for IR-stage warnings.
tacWarnPath :: Path
tacWarnPath = "<tac>"

-- | Explicit numeric ranges (IEEE 754 for floats).
int32Range :: (Integer, Integer)
int32Range = (-0x80000000,  0x7fffffff)

int64Range :: (Integer, Integer)
int64Range = (-0x8000000000000000, 0x7fffffffffffffff)

float32Range :: (Double, Double)
float32Range = (-0x1.fffffeP+127, 0x1.fffffeP+127)

float64Range :: (Double, Double)
float64Range = (-0x1.fffffffffffffP+1023, 0x1.fffffffffffffP+1023)

float128Range :: (Rational, Rational)
float128Range = (-0x1.ffffffffffffffffffffffffffffP+16383, 0x1.ffffffffffffffffffffffffffffP+16383)


-- | Strip integer literal suffixes (only 'L' / 'l').
stripIntSuffix :: String -> String
stripIntSuffix s = case reverse s of
    (c:cs) | c `elem` "lL" -> reverse cs
    _ -> s

-- | Strip floating literal suffixes ('F'/'f' or 'L'/'l').
stripFloatSuffix :: String -> String
stripFloatSuffix s = case reverse s of
    (c:cs) | c `elem` "fFlL" -> reverse cs
    _ -> s

-- | Normalize floating literals for `readMaybe`:
--   - ".5"   -> "0.5"
--   - "-.5"  -> "-0.5"
--   - "10."  -> "10.0"
normalizeFloatLiteral :: String -> String
normalizeFloatLiteral raw =
    let s0 = stripFloatSuffix raw
        s1 = case s0 of
            ('.':_) -> '0' : s0
            ('-':'.':rest) -> "-0." ++ rest
            ('+':'.':rest) -> "+0." ++ rest
            _ -> s0
    in case reverse s1 of
        ('.':_) -> s1 ++ "0"
        _ -> s1

-- | Read integer literals supporting hex (0x/0X) and optional suffix.
readIntegerLiteral :: String -> Maybe Integer
readIntegerLiteral raw =
    let s = stripIntSuffix raw
    in case s of
        ('0':'x':rest) -> readHexExact rest
        ('0':'X':rest) -> readHexExact rest
        _ -> readMaybe s
    where
        readHexExact :: String -> Maybe Integer
        readHexExact xs = do
            (n, tailS) <- listToMaybe (readHex xs)
            if null tailS then Just n else Nothing


-- | Wrap an integer into a target range; prefers bitmask for two's-complement.
wrapInt :: (Integer, Integer) -> Integer -> Integer
wrapInt (minI, maxI) n =
    let range = maxI - minI + 1
        isPow2 x = x > 0 && (x .&. (x - 1)) == 0
    in if isPow2 range && minI == negate (range `div` 2) && maxI == (range `div` 2) - 1
        then
            let mask = range - 1
                u = n .&. mask
            in if u > maxI then u - range else u
        else ((n - minI) `mod` range) + minI


-- | Safe read for integer literals with overflow warning and wrap-around.
safeInteger :: Position -> (Integer, Integer) -> String -> TACM Integer
safeInteger pos (minI, maxI) raw = do
    let wrap = wrapInt (minI, maxI)
        warn msg = TAC.addWarn $ OverflowWarning (UE.makeError tacWarnPath [pos] msg)
    case readIntegerLiteral raw of
        Nothing -> error "this string must be valid any how"
        Just n ->
            if n < minI || n > maxI
                then do
                    warn $ UE.overflowCastMsg "number" "int"
                    return (wrap n)
                else return n


-- | Safe read for rational literals; out of range -> clamp to min/max.
safeRational :: Position -> (Rational, Rational) -> String -> TACM Rational
safeRational pos (minR, maxR) raw = do
    let warn msg = TAC.addWarn $ OverflowWarning (UE.makeError tacWarnPath [pos] msg)
    case readMaybe (normalizeFloatLiteral raw) :: Maybe Double of
        Nothing -> error "this string must be valid any how"
        Just n
            | isNaN n -> do
                warn $ UE.overflowCastMsg "number" "float128"
                return 0
            | isInfinite n -> do
                warn $ UE.overflowCastMsg "number" "float128"
                return (if n > 0 then maxR else minR)
            | otherwise ->
                let r = toRational n
                in if r > maxR
                    then warn (UE.overflowCastMsg "number" "float128") >> return maxR
                    else if r < minR
                        then warn (UE.overflowCastMsg "number" "float128") >> return minR
                        else return r


-- | Safe read for floating literals; out of range -> +/-Infinity.
safeDouble :: Position -> (Double, Double) -> String -> TACM Double
safeDouble pos (minD, maxD) raw = do
    let warn msg = TAC.addWarn $ OverflowWarning (UE.makeError tacWarnPath [pos] msg)
    case readMaybe (normalizeFloatLiteral raw) :: Maybe Double of
        Nothing -> error "this string must be valid any how"
        Just n
            | n > maxD -> do
                warn $ UE.overflowCastMsg "number" "double"
                return (1.0 / 0.0)
            | n < minD -> do
                warn $ UE.overflowCastMsg "number" "double"
                return $ -(1.0 / 0.0)
            | otherwise -> return n


-- | Lower a single expression into a TAC atom.
--   Use lookupVarUse / lookupFunUse when you need resolved ids/types.
atomLowing :: Expression -> TACM ([IRNode], IRAtom)
atomLowing (AST.Error _ _) = error "this error is catching in semantic already"

atomLowing (AST.IntConst value tok) = do
    v <- safeInteger (tokenPos tok) int32Range value
    return ([], TAC.Int32C (fromInteger v))
atomLowing (AST.LongConst value tok) = do
    v <- safeInteger (tokenPos tok) int64Range value
    return ([], TAC.Int64C (fromInteger v))
atomLowing (AST.FloatConst value tok) = do
    v <- safeDouble (tokenPos tok) float32Range value
    return ([], TAC.Float32C v)
atomLowing (AST.DoubleConst value tok) = do
    v <- safeDouble (tokenPos tok) float64Range value
    return ([], TAC.Float64C v)
atomLowing (AST.LongDoubleConst value tok) = do
    v <- safeRational (tokenPos tok) float128Range value
    return ([], TAC.Float128C v)
    
atomLowing (AST.CharConst value _) = return ([], TAC.CharC value)
atomLowing (AST.BoolConst value _) = return ([], TAC.BoolC value)
atomLowing (AST.StringConst value _) = return ([], TAC.StringC value)

atomLowing (AST.Variable _ tok) = do
    let pos = tokenPos tok
    vinfo <- getVar [pos]
    case vinfo of
        -- ????????? VarId + ??????
        TEnv.VarLocal _ realName vid -> do
            key <- resolveVarKey (realName, vid)
            isStatic <- isStaticVar key
            if isStatic
                then do
                    (clazz, _) <- peekVarStack key
                    nAtom <- newSubCVar clazz
                    return ([IRInstr (TAC.IGetStatic nAtom [realName])], nAtom)
                else do
                    let (resolvedName, resolvedVid) = key
                    (_, ver) <- peekVarStack key
                    let atom = TAC.Var (resolvedName, resolvedVid, ver)

                    mFun <- TAC.getCurrentFunMaybe
                    case mFun >>= (\(_, _, argMap) -> lookupParamIndex atom argMap) of
                        Just idx -> return ([], TAC.Param idx)
                        Nothing -> return ([], atom)

        -- ??????
        TEnv.VarImported _ clazz _ fullQname -> do
            nAtom <- newSubCVar clazz
            return ([IRInstr (TAC.IGetStatic nAtom fullQname)], nAtom)

atomLowing (AST.Qualified _ tokens) = do
    vinfo <- getVar (map tokenPos tokens)
    case vinfo of
        TEnv.VarImported _ clazz _ fullQname -> do
            nAtom <- newSubCVar clazz
            return ([IRInstr (TAC.IGetStatic nAtom fullQname)], nAtom)
        _ -> error "qualified name is not an imported variable"

atomLowing _ = error "other type is not allowed for IR atom"


-- | Find parameter index for a given atom, if it is a function argument.
lookupParamIndex :: IRAtom -> Map Int IRAtom -> Maybe Int
lookupParamIndex atom =
    Map.foldrWithKey (\k v acc -> if v == atom then Just k else acc) Nothing


castIfNeeded :: Class -> IRAtom -> Class -> TACM ([IRNode], IRAtom)
castIfNeeded from atom to
    | from == to = return ([], atom)
    | otherwise = do
        castAtom <- newSubCVar to
        return ([IRInstr (TAC.ICast castAtom (from, to) atom)], castAtom)


-- | Numeric literal `1` with a concrete class shape.
oneAtomForClass :: Class -> IRAtom
oneAtomForClass cls = case cls of
    Int8T -> TAC.Int8C 1
    Int16T -> TAC.Int16C 1
    Int32T -> TAC.Int32C 1
    Int64T -> TAC.Int64C 1
    Float32T -> TAC.Float32C 1.0
    Float64T -> TAC.Float64C 1.0
    Float128T -> TAC.Float128C 1
    Char -> TAC.CharC '\1'
    _ -> error "oneAtomForClass: unsupported type for ++"

zeroAtomForClass :: Class -> IRAtom
zeroAtomForClass cls = case cls of
    Int8T -> TAC.Int8C 0
    Int16T -> TAC.Int16C 0
    Int32T -> TAC.Int32C 0
    Int64T -> TAC.Int64C 0
    _ -> error "zeroAtomForClass: unsupported repeat counter type"

isRepeatCounterClass :: Class -> Bool
isRepeatCounterClass cls = cls `elem` [Int8T, Int16T, Int32T, Int64T]


-- return instruction and output atom eg: a = a + b => a1 = a0 + b0 return a1
-- warning [IRNode] is reversed!
exprLowing :: Expression -> TACM ([IRNode], IRAtom)
exprLowing (AST.Cast (toClass, _) inner _) = do
    (instrs, oAtom) <- if AST.isAtom inner then atomLowing inner else exprLowing inner
    fromClass <- getAtomType oAtom
    (castInstrs, castAtom) <- castIfNeeded fromClass oAtom toClass
    return (castInstrs ++ instrs, castAtom)

-- ++a
exprLowing (AST.Unary AST.IncSelf inner _) = do
    (instr, oAtom) <- if AST.isAtom inner then atomLowing inner else exprLowing inner
    fromClass <- getAtomType oAtom
    let oneAtom = oneAtomForClass fromClass
    case oAtom of
        TAC.Var (name, vid, _) -> do
            nextAtom <- newSubVar fromClass (name, vid)
            let addInstr = IRInstr (TAC.IBinary nextAtom AST.Add oAtom oneAtom)
            return (addInstr : instr, nextAtom)
        TAC.Param _ -> do
            tmpAtom <- newSubCVar fromClass
            let addInstr = IRInstr (TAC.IBinary tmpAtom AST.Add oAtom oneAtom)
            let writeBack = IRInstr (TAC.IAssign oAtom tmpAtom)
            return ([writeBack, addInstr] ++ instr, oAtom)
        _ -> error "IncSelf expects Var/Param atom"

-- --a
exprLowing (AST.Unary AST.DecSelf inner _) = do
    (instr, oAtom) <- if AST.isAtom inner then atomLowing inner else exprLowing inner
    fromClass <- getAtomType oAtom
    let oneAtom = oneAtomForClass fromClass
    case oAtom of
        TAC.Var (name, vid, _) -> do
            nextAtom <- newSubVar fromClass (name, vid)
            let subInstr = IRInstr (TAC.IBinary nextAtom AST.Sub oAtom oneAtom)
            return (subInstr : instr, nextAtom)
        TAC.Param _ -> do
            tmpAtom <- newSubCVar fromClass
            let subInstr = IRInstr (TAC.IBinary tmpAtom AST.Sub oAtom oneAtom)
            let writeBack = IRInstr (TAC.IAssign oAtom tmpAtom)
            return ([writeBack, subInstr] ++ instr, oAtom)
        _ -> error "DecSelf expects Var/Param atom"

-- a++
exprLowing (AST.Unary AST.SelfInc inner _) = do
    (instr, oAtom) <- if AST.isAtom inner then atomLowing inner else exprLowing inner
    fromClass <- getAtomType oAtom
    let oneAtom = oneAtomForClass fromClass
    case oAtom of
        TAC.Var (name, vid, _) -> do
            oldAtom <- newSubCVar fromClass
            nextAtom <- newSubVar fromClass (name, vid)
            let copyInstr = IRInstr (TAC.IAssign oldAtom oAtom)
            let addInstr = IRInstr (TAC.IBinary nextAtom AST.Add oAtom oneAtom)
            return ([addInstr, copyInstr] ++ instr, oldAtom)
        TAC.Param _ -> do
            oldAtom <- newSubCVar fromClass
            tempAtom <- newSubCVar fromClass
            let copyInstr = IRInstr (TAC.IAssign oldAtom oAtom)
            let addInstr = IRInstr (TAC.IBinary tempAtom AST.Add oAtom oneAtom)
            let writeBack = IRInstr (TAC.IAssign oAtom tempAtom)
            return ([writeBack, addInstr, copyInstr] ++ instr, oldAtom)
        _ -> error "SelfInc expects Var/Param atom"

-- a--
exprLowing (AST.Unary AST.SelfDec inner _) = do
    (instr, oAtom) <- if AST.isAtom inner then atomLowing inner else exprLowing inner
    fromClass <- getAtomType oAtom
    let oneAtom = oneAtomForClass fromClass
    case oAtom of
        TAC.Var (name, vid, _) -> do
            oldAtom <- newSubCVar fromClass
            nextAtom <- newSubVar fromClass (name, vid)
            let copyInstr = IRInstr (TAC.IAssign oldAtom oAtom)
            let subInstr = IRInstr (TAC.IBinary nextAtom AST.Sub oAtom oneAtom)
            return ([subInstr, copyInstr] ++ instr, oldAtom)
        TAC.Param _ -> do
            oldAtom <- newSubCVar fromClass
            tempAtom <- newSubCVar fromClass
            let copyInstr = IRInstr (TAC.IAssign oldAtom oAtom)
            let subInstr = IRInstr (TAC.IBinary tempAtom AST.Sub oAtom oneAtom)
            let writeBack = IRInstr (TAC.IAssign oAtom tempAtom)
            return ([writeBack, subInstr, copyInstr] ++ instr, oldAtom)
        _ -> error "SelfDec expects Var/Param atom"


exprLowing (AST.Unary op inner _) = do
    (instr, oAtom) <- if AST.isAtom inner then atomLowing inner else exprLowing inner
    fromClass <- getAtomType oAtom
    let toClass = inferUnaryOp op fromClass

    if fromClass == toClass then do
        nAtom <- newSubCVar toClass
        return (IRInstr (TAC.IUnary nAtom op oAtom) : instr, nAtom)
    else do
        (castInstrs, castAtom) <- castIfNeeded fromClass oAtom toClass
        nAtom <- newSubCVar toClass
        return (IRInstr (TAC.IUnary nAtom op castAtom) : castInstrs ++ instr, nAtom)

exprLowing (AST.Binary AST.Assign (AST.Variable _ tok) e2 _) = do
    let pos = tokenPos tok
    vinfo <- getVar [pos]
    case vinfo of
        TEnv.VarLocal _ realName vid -> do
            key <- resolveVarKey (realName, vid)
            isStatic <- isStaticVar key

            oldCur <- TAC.getCurrentVar

            -- Keep RHS lowering in the original current-var context.
            -- If we force current-var to the assignment target here, RHS
            -- temporaries may become target versions and pollute reads like:
            --   x = x - eta * 2.0 * x
            -- into effectively using updated x-temps on RHS.
            TAC.setCurrentVar oldCur
            (instrs, rhsAtom) <- if AST.isAtom e2 then atomLowing e2 else exprLowing e2
            clazz <- getAtomType rhsAtom
            lhsAtom <- newSubVar clazz key

            TAC.setCurrentVar oldCur

            let rhsForward = reverse instrs
                assignTail = IRInstr (TAC.IAssign lhsAtom rhsAtom)
                writeTail =
                    if isStatic
                        then [assignTail, IRInstr (TAC.IPutStatic [realName] lhsAtom)]
                        else [assignTail]
                seqForward = appendAfterCond rhsForward writeTail
            return (reverse seqForward, lhsAtom)
             
        TEnv.VarImported _ _ _ fullQname -> do
            (instrs, rhsAtom) <- if AST.isAtom e2 then atomLowing e2 else exprLowing e2
            let rhsForward = reverse instrs
                seqForward = appendAfterCond rhsForward [IRInstr (TAC.IPutStatic fullQname rhsAtom)]
            return (reverse seqForward, rhsAtom)

-- TODO for class
exprLowing (AST.Binary AST.Assign (AST.Qualified names toks) e2 _) =
    case names of
        ("this":_) -> error "TODO: assign to this.field is not supported yet"
        _ -> do
            vinfo <- getVar (map tokenPos toks)
            case vinfo of
                TEnv.VarImported _ _ _ fullQname -> do
                    (instrs, rhsAtom) <- if AST.isAtom e2 then atomLowing e2 else exprLowing e2
                    let rhsForward = reverse instrs
                        seqForward = appendAfterCond rhsForward [IRInstr (TAC.IPutStatic fullQname rhsAtom)]
                    return (reverse seqForward, rhsAtom)
                _ -> error "assign lhs is not an imported qualified name"


exprLowing (AST.Binary AST.Assign _ _ _) = error "cannot be assign in his case. the error should be catched in context check"


exprLowing (AST.Binary AST.LogicalAnd e1 e2 _) = shortCircuitLogical AST.LogicalAnd e1 e2
exprLowing (AST.Binary AST.LogicalNand e1 e2 _) = shortCircuitLogical AST.LogicalNand e1 e2
exprLowing (AST.Binary AST.LogicalOr e1 e2 _) = shortCircuitLogical AST.LogicalOr e1 e2
exprLowing (AST.Binary AST.LogicalNor e1 e2 _) = shortCircuitLogical AST.LogicalNor e1 e2
exprLowing (AST.Binary AST.LogicalImply e1 e2 _) = shortCircuitLogical AST.LogicalImply e1 e2
exprLowing (AST.Binary AST.LogicalNimply e1 e2 _) = shortCircuitLogical AST.LogicalNimply e1 e2


exprLowing (AST.Binary op e1 e2 _) = do
    (instr1, oAtom1) <- if AST.isAtom e1 then atomLowing e1 else exprLowing e1
    (instr2, oAtom2) <- if AST.isAtom e2 then atomLowing e2 else exprLowing e2
    fromClass1 <- getAtomType oAtom1
    fromClass2 <- getAtomType oAtom2
    let castClass = binaryOpCastType op fromClass1 fromClass2
        resClass = inferBinaryOp op fromClass1 fromClass2

    (cast1Instrs, atom1') <- castIfNeeded fromClass1 oAtom1 castClass
    (cast2Instrs, atom2') <- castIfNeeded fromClass2 oAtom2 castClass
    if isCompareOp op then do
        nAtom <- newSubCVar resClass
        lTrue <- incBlockId
        lFalse <- incBlockId
        lJoin <- incBlockId

        let jumpInstr = case op of
                AST.Equal -> TAC.Ifeq atom1' atom2' (lTrue, lFalse)
                AST.NotEqual -> TAC.Ifne atom1' atom2' (lTrue, lFalse)
                AST.LessThan -> TAC.Iflt atom1' atom2' (lTrue, lFalse)
                AST.LessEqual -> TAC.Ifle atom1' atom2' (lTrue, lFalse)
                AST.GreaterThan -> TAC.Ifgt atom1' atom2' (lTrue, lFalse)
                AST.GreaterEqual -> TAC.Ifge atom1' atom2' (lTrue, lFalse)
                _ -> error "not a compare op"

            trueBlock = IRBlockStmt (lTrue, [
                IRInstr (TAC.IAssign nAtom (TAC.BoolC True)),
                IRInstr (TAC.Jump lJoin)
                ])
            falseBlock = IRBlockStmt (lFalse, [
                IRInstr (TAC.IAssign nAtom (TAC.BoolC False)),
                IRInstr (TAC.Jump lJoin)
                ])
            phiInstr = IRInstr (TAC.IAssign nAtom (TAC.Phi [(lTrue, TAC.BoolC True), (lFalse, TAC.BoolC False)]))
            joinBlock = IRBlockStmt (lJoin, [phiInstr])

        let seqRev =
                [joinBlock, falseBlock, trueBlock, IRInstr jumpInstr]
                ++ cast2Instrs ++ instr2 ++ cast1Instrs ++ instr1
        return (seqRev, nAtom)
    else do
        let tailRev = concat [cast2Instrs, instr2, cast1Instrs, instr1]
        case op of
            AST.BitNand -> do
                andAtom <- newSubCVar resClass
                nAtom <- newSubCVar resClass
                return (
                    [ IRInstr (TAC.IUnary nAtom AST.BitInv andAtom)
                    , IRInstr (TAC.IBinary andAtom AST.BitAnd atom1' atom2')
                    ] ++ tailRev,
                    nAtom)

            AST.BitXnor -> do
                xorAtom <- newSubCVar resClass
                nAtom <- newSubCVar resClass
                return (
                    [ IRInstr (TAC.IUnary nAtom AST.BitInv xorAtom)
                    , IRInstr (TAC.IBinary xorAtom AST.BitXor atom1' atom2')
                    ] ++ tailRev,
                    nAtom)

            AST.BitNor -> do
                orAtom <- newSubCVar resClass
                nAtom <- newSubCVar resClass
                return (
                    [ IRInstr (TAC.IUnary nAtom AST.BitInv orAtom)
                    , IRInstr (TAC.IBinary orAtom AST.BitOr atom1' atom2')
                    ] ++ tailRev,
                    nAtom)

            AST.BitImply -> do
                notA <- newSubCVar resClass
                nAtom <- newSubCVar resClass
                return (
                    [ IRInstr (TAC.IBinary nAtom AST.BitOr notA atom2')
                    , IRInstr (TAC.IUnary notA AST.BitInv atom1')
                    ] ++ tailRev,
                    nAtom)

            AST.BitNimply -> do
                notB <- newSubCVar resClass
                nAtom <- newSubCVar resClass
                return (
                    [ IRInstr (TAC.IBinary nAtom AST.BitAnd atom1' notB)
                    , IRInstr (TAC.IUnary notB AST.BitInv atom2')
                    ] ++ tailRev,
                    nAtom)

            _ -> do
                nAtom <- newSubCVar resClass
                return (IRInstr (TAC.IBinary nAtom op atom1' atom2') : tailRev, nAtom)

exprLowing (AST.Ternary cond (thenE, elseE) _) = do
    (condInstrs, condAtom) <- if AST.isAtom cond then atomLowing cond else exprLowing cond
    (thenInstrs, thenAtom0) <- if AST.isAtom thenE then atomLowing thenE else exprLowing thenE
    (elseInstrs, elseAtom0) <- if AST.isAtom elseE then atomLowing elseE else exprLowing elseE

    thenClass <- getAtomType thenAtom0
    elseClass <- getAtomType elseAtom0
    let resClass
            | thenClass == elseClass = thenClass
            | isBasicType thenClass && isBasicType elseClass = promoteBasicType thenClass elseClass
            | otherwise = error "ternary branches must be both basic or same type"

    (thenCastInstrs, thenAtom) <- castIfNeeded thenClass thenAtom0 resClass
    (elseCastInstrs, elseAtom) <- castIfNeeded elseClass elseAtom0 resClass

    nAtom <- newSubCVar resClass
    lThen <- incBlockId
    lElse <- incBlockId
    lJoin <- incBlockId

    let condStmts = appendAfterCond (reverse condInstrs) [
            IRInstr (TAC.Ifeq condAtom (TAC.Int32C 1) (lThen, lElse))]
        thenBody = appendAfterCond (reverse thenInstrs) (
            thenCastInstrs ++ [
                IRInstr (TAC.IAssign nAtom thenAtom),
                IRInstr (TAC.Jump lJoin)])
        elseBody = appendAfterCond (reverse elseInstrs) (
            elseCastInstrs ++ [
                IRInstr (TAC.IAssign nAtom elseAtom),
                IRInstr (TAC.Jump lJoin)])
        thenBlock = IRBlockStmt (lThen, thenBody)
        elseBlock = IRBlockStmt (lElse, elseBody)
        phiInstr = IRInstr (TAC.IAssign nAtom (TAC.Phi [(lThen, thenAtom), (lElse, elseAtom)]))
        joinBlock = IRBlockStmt (lJoin, [phiInstr])
        seqFwd = condStmts ++ [thenBlock, elseBlock, joinBlock]

    return (reverse seqFwd, nAtom)

exprLowing (AST.Call funName params) = do
    funInfo <- case funName of
        AST.Variable _ tok -> TAC.getFunction [tokenPos tok]
        AST.Qualified _ toks -> TAC.getFunction (map tokenPos toks)
        _ -> error "call target is not a function name"

    case funInfo of
        TEnv.FunLocal _ qname sig -> do
            let retT = TEnv.funReturn sig
            (argInstrs, argAtoms) <- lowerArgsWithSig (TEnv.funParams sig) params
            dst <- newSubCVar retT
            let instr = case qname of
                    [name] -> TAC.ICall dst name argAtoms
                    _ -> TAC.ICallStatic dst qname argAtoms
            return (IRInstr instr : argInstrs, dst)
        TEnv.FunImported _ _ fullQname sig -> do
            let retT = TEnv.funReturn sig
            (argInstrs, argAtoms) <- lowerArgsWithSig (TEnv.funParams sig) params
            dst <- newSubCVar retT
            return (IRInstr (TAC.ICallStatic dst fullQname argAtoms) : argInstrs, dst)
    where
        lowerArgsWithSig :: [Class] -> [Expression] -> TACM ([IRNode], [IRAtom])
        lowerArgsWithSig paramTs args
            | length paramTs /= length args = error "internal error: argument count mismatch after typecheck"
            | otherwise = foldrM step ([], []) (zip args paramTs)
            where
                step :: (Expression, Class) -> ([IRNode], [IRAtom]) -> TACM ([IRNode], [IRAtom])
                step (p, paramT) (instrsRest, atomsRest) = do
                    (instrsP, atomP) <- if AST.isAtom p then atomLowing p else exprLowing p
                    argT <- getAtomType atomP
                    (castInstrs, castAtom) <- castIfNeeded argT atomP paramT
                    return (instrsRest ++ castInstrs ++ instrsP, castAtom : atomsRest)

exprLowing (AST.CallT {}) = error "template is not support!"

exprLowing _ = error "other type is not support for IR ast"


shortCircuitLogical :: AST.Operator -> Expression -> Expression -> TACM ([IRNode], IRAtom)
shortCircuitLogical op e1 e2 = do
    (lhsInstrs, lhsAtom0) <- if AST.isAtom e1 then atomLowing e1 else exprLowing e1
    (rhsInstrs, rhsAtom0) <- if AST.isAtom e2 then atomLowing e2 else exprLowing e2

    lhsClass <- getAtomType lhsAtom0
    rhsClass <- getAtomType rhsAtom0

    (lhsCastInstrs, lhsAtom) <- castIfNeeded lhsClass lhsAtom0 Bool
    (rhsCastInstrs, rhsAtom) <- castIfNeeded rhsClass rhsAtom0 Bool

    nAtom <- newSubCVar Bool

    case op of
        AST.LogicalAnd -> do
            lRhs <- incBlockId
            lFalse <- incBlockId
            lJoin <- incBlockId

            let condStmts = appendAfterCond (reverse lhsInstrs) (
                    lhsCastInstrs ++ [
                        IRInstr (TAC.Ifeq lhsAtom (TAC.Int32C 1) (lRhs, lFalse))])
                rhsBody = appendAfterCond (reverse rhsInstrs) (
                    rhsCastInstrs ++ [
                        IRInstr (TAC.IAssign nAtom rhsAtom),
                        IRInstr (TAC.Jump lJoin)])
                falseBody = [
                    IRInstr (TAC.IAssign nAtom (TAC.BoolC False)),
                    IRInstr (TAC.Jump lJoin)]
                rhsBlock = IRBlockStmt (lRhs, rhsBody)
                falseBlock = IRBlockStmt (lFalse, falseBody)
                phiInstr = IRInstr (TAC.IAssign nAtom (TAC.Phi [(lRhs, rhsAtom), (lFalse, TAC.BoolC False)]))
                joinBlock = IRBlockStmt (lJoin, [phiInstr])
                seqFwd = condStmts ++ [rhsBlock, falseBlock, joinBlock]

            return (reverse seqFwd, nAtom)

        AST.LogicalNand -> do
            lTrue <- incBlockId
            lRhs <- incBlockId
            lJoin <- incBlockId
            rhsNot <- newSubCVar Bool

            let condStmts = appendAfterCond (reverse lhsInstrs) (
                    lhsCastInstrs ++ [
                        IRInstr (TAC.Ifeq lhsAtom (TAC.Int32C 1) (lRhs, lTrue))])
                trueBody = [
                    IRInstr (TAC.IAssign nAtom (TAC.BoolC True)),
                    IRInstr (TAC.Jump lJoin)]
                rhsBody = appendAfterCond (reverse rhsInstrs) (
                    rhsCastInstrs ++ [
                        IRInstr (TAC.IUnary rhsNot AST.LogicalNot rhsAtom),
                        IRInstr (TAC.IAssign nAtom rhsNot),
                        IRInstr (TAC.Jump lJoin)])
                trueBlock = IRBlockStmt (lTrue, trueBody)
                rhsBlock = IRBlockStmt (lRhs, rhsBody)
                phiInstr = IRInstr (TAC.IAssign nAtom (TAC.Phi [(lTrue, TAC.BoolC True), (lRhs, rhsNot)]))
                joinBlock = IRBlockStmt (lJoin, [phiInstr])
                seqFwd = condStmts ++ [trueBlock, rhsBlock, joinBlock]

            return (reverse seqFwd, nAtom)

        AST.LogicalOr -> do
            lTrue <- incBlockId
            lRhs <- incBlockId
            lJoin <- incBlockId

            let condStmts = appendAfterCond (reverse lhsInstrs) (
                    lhsCastInstrs ++ [
                        IRInstr (TAC.Ifeq lhsAtom (TAC.Int32C 1) (lTrue, lRhs))])
                trueBody = [
                    IRInstr (TAC.IAssign nAtom (TAC.BoolC True)),
                    IRInstr (TAC.Jump lJoin)]
                rhsBody = appendAfterCond (reverse rhsInstrs) (
                    rhsCastInstrs ++ [
                        IRInstr (TAC.IAssign nAtom rhsAtom),
                        IRInstr (TAC.Jump lJoin)])
                trueBlock = IRBlockStmt (lTrue, trueBody)
                rhsBlock = IRBlockStmt (lRhs, rhsBody)
                phiInstr = IRInstr (TAC.IAssign nAtom (TAC.Phi [(lTrue, TAC.BoolC True), (lRhs, rhsAtom)]))
                joinBlock = IRBlockStmt (lJoin, [phiInstr])
                seqFwd = condStmts ++ [trueBlock, rhsBlock, joinBlock]

            return (reverse seqFwd, nAtom)

        AST.LogicalNor -> do
            lFalse <- incBlockId
            lRhs <- incBlockId
            lJoin <- incBlockId
            rhsNot <- newSubCVar Bool

            let condStmts = appendAfterCond (reverse lhsInstrs) (
                    lhsCastInstrs ++ [
                        IRInstr (TAC.Ifeq lhsAtom (TAC.Int32C 1) (lFalse, lRhs))])
                falseBody = [
                    IRInstr (TAC.IAssign nAtom (TAC.BoolC False)),
                    IRInstr (TAC.Jump lJoin)]
                rhsBody = appendAfterCond (reverse rhsInstrs) (
                    rhsCastInstrs ++ [
                        IRInstr (TAC.IUnary rhsNot AST.LogicalNot rhsAtom),
                        IRInstr (TAC.IAssign nAtom rhsNot),
                        IRInstr (TAC.Jump lJoin)])
                falseBlock = IRBlockStmt (lFalse, falseBody)
                rhsBlock = IRBlockStmt (lRhs, rhsBody)
                phiInstr = IRInstr (TAC.IAssign nAtom (TAC.Phi [(lFalse, TAC.BoolC False), (lRhs, rhsNot)]))
                joinBlock = IRBlockStmt (lJoin, [phiInstr])
                seqFwd = condStmts ++ [falseBlock, rhsBlock, joinBlock]

            return (reverse seqFwd, nAtom)

        AST.LogicalImply -> do
            lRhs <- incBlockId
            lTrue <- incBlockId
            lJoin <- incBlockId

            let condStmts = appendAfterCond (reverse lhsInstrs) (
                    lhsCastInstrs ++ [
                        IRInstr (TAC.Ifeq lhsAtom (TAC.Int32C 1) (lRhs, lTrue))])
                rhsBody = appendAfterCond (reverse rhsInstrs) (
                    rhsCastInstrs ++ [
                        IRInstr (TAC.IAssign nAtom rhsAtom),
                        IRInstr (TAC.Jump lJoin)])
                trueBody = [
                    IRInstr (TAC.IAssign nAtom (TAC.BoolC True)),
                    IRInstr (TAC.Jump lJoin)]
                rhsBlock = IRBlockStmt (lRhs, rhsBody)
                trueBlock = IRBlockStmt (lTrue, trueBody)
                phiInstr = IRInstr (TAC.IAssign nAtom (TAC.Phi [(lRhs, rhsAtom), (lTrue, TAC.BoolC True)]))
                joinBlock = IRBlockStmt (lJoin, [phiInstr])
                seqFwd = condStmts ++ [rhsBlock, trueBlock, joinBlock]

            return (reverse seqFwd, nAtom)

        AST.LogicalNimply -> do
            lRhs <- incBlockId
            lFalse <- incBlockId
            lJoin <- incBlockId
            rhsNot <- newSubCVar Bool

            let condStmts = appendAfterCond (reverse lhsInstrs) (
                    lhsCastInstrs ++ [
                        IRInstr (TAC.Ifeq lhsAtom (TAC.Int32C 1) (lRhs, lFalse))])
                rhsBody = appendAfterCond (reverse rhsInstrs) (
                    rhsCastInstrs ++ [
                        IRInstr (TAC.IUnary rhsNot AST.LogicalNot rhsAtom),
                        IRInstr (TAC.IAssign nAtom rhsNot),
                        IRInstr (TAC.Jump lJoin)])
                falseBody = [
                    IRInstr (TAC.IAssign nAtom (TAC.BoolC False)),
                    IRInstr (TAC.Jump lJoin)]
                rhsBlock = IRBlockStmt (lRhs, rhsBody)
                falseBlock = IRBlockStmt (lFalse, falseBody)
                phiInstr = IRInstr (TAC.IAssign nAtom (TAC.Phi [(lRhs, rhsNot), (lFalse, TAC.BoolC False)]))
                joinBlock = IRBlockStmt (lJoin, [phiInstr])
                seqFwd = condStmts ++ [rhsBlock, falseBlock, joinBlock]

            return (reverse seqFwd, nAtom)

        _ -> error "shortCircuitLogical only supports LogicalAnd / LogicalNand / LogicalOr / LogicalNor / LogicalImply / LogicalNimply"


type VarKey = (String, Int)

paramifyAtom :: IRAtom -> TACM IRAtom
paramifyAtom atom@(TAC.Var {}) = do
    mFun <- getCurrentFunMaybe
    case mFun >>= (\(_, _, argMap) -> lookupParamIndex atom argMap) of
        Just idx -> return (TAC.Param idx)
        Nothing -> return atom
paramifyAtom atom = return atom

topAtomFromStacksM :: Map VarKey [(Class, Int)] -> VarKey -> TACM (Maybe (Class, IRAtom))
topAtomFromStacksM stacks key@(name, vid) = case Map.lookup key stacks of
    Just ((cls, ver):_) -> do
        atom <- paramifyAtom (TAC.Var (name, vid, ver))
        return (Just (cls, atom))
    _ -> return Nothing

topAtomInStacksM :: Map VarKey [(Class, Int)] -> VarKey -> TACM (Maybe IRAtom)
topAtomInStacksM stacks key = fmap snd <$> topAtomFromStacksM stacks key

diffVarKeys :: Map VarKey [(Class, Int)] -> Map VarKey [(Class, Int)] -> [VarKey]
diffVarKeys a b =
    let keys = Map.keys (Map.union a b)
        topVer stacks key = case Map.lookup key stacks of
            Just ((_, ver):_) -> Just ver
            _ -> Nothing
    in filter (\k -> topVer a k /= topVer b k) keys

currentAtomForKey :: VarKey -> TACM IRAtom
currentAtomForKey key@(name, vid) = do
    (_, ver) <- peekVarStack key
    return (TAC.Var (name, vid, ver))

assignPhiFromCurrent :: (VarKey, IRAtom) -> TACM (Maybe IRInstr)
assignPhiFromCurrent (key, dest) = do
    cur <- currentAtomForKey key
    return $ if cur == dest then Nothing else Just (TAC.IAssign dest cur)

collectAssignKeysBlock :: Block -> TACM [VarKey]
collectAssignKeysBlock (AST.Multiple ss) = do
    keys <- concat <$> mapM collectAssignKeysStmt ss
    return $ Map.keys (Map.fromList (map (, ()) keys))
    where
        collectAssignKeysStmt :: Statement -> TACM [VarKey]
        collectAssignKeysStmt stmt = case stmt of
            AST.DefField names _ mRhs toks -> case (names, mRhs, reverse toks) of
                ([name], Just rhs, nameTok:_) ->
                    collectAssignKeysExpr (AST.Binary AST.Assign (AST.Variable name nameTok) rhs nameTok)
                _ ->
                    maybe (return []) collectAssignKeysExpr mRhs
            AST.DefConstField names _ mRhs toks -> case (names, mRhs, reverse toks) of
                ([name], Just rhs, nameTok:_) ->
                    collectAssignKeysExpr (AST.Binary AST.Assign (AST.Variable name nameTok) rhs nameTok)
                _ ->
                    maybe (return []) collectAssignKeysExpr mRhs
            AST.DefVar names _ mRhs toks -> case (names, mRhs, reverse toks) of
                ([name], Just rhs, nameTok:_) ->
                    collectAssignKeysExpr (AST.Binary AST.Assign (AST.Variable name nameTok) rhs nameTok)
                _ ->
                    maybe (return []) collectAssignKeysExpr mRhs
            AST.DefConstVar names _ mRhs toks -> case (names, mRhs, reverse toks) of
                ([name], Just rhs, nameTok:_) ->
                    collectAssignKeysExpr (AST.Binary AST.Assign (AST.Variable name nameTok) rhs nameTok)
                _ ->
                    maybe (return []) collectAssignKeysExpr mRhs
            AST.Expr e -> collectAssignKeysExpr e
            AST.Exprs es -> concat <$> mapM collectAssignKeysExpr es
            AST.StmtGroup groupStmts -> concat <$> mapM collectAssignKeysStmt groupStmts
            AST.Command (AST.Return (Just e)) _ -> collectAssignKeysExpr e
            AST.BlockStmt b -> collectAssignKeysBlock b
            AST.If e b1 b2 _ -> do
                ks1 <- collectAssignKeysExpr e
                ks2 <- maybe (return []) collectAssignKeysBlock b1
                ks3 <- maybe (return []) collectAssignKeysBlock b2
                return $ concat [ks1, ks2, ks3]
            AST.For (s1, e2, s3) b1 b2 _ -> do
                ks1 <- maybe (return []) collectAssignKeysStmt s1
                ks2 <- maybe (return []) collectAssignKeysExpr e2
                ks3 <- maybe (return []) collectAssignKeysStmt s3
                ks4 <- maybe (return []) collectAssignKeysBlock b1
                ks5 <- maybe (return []) collectAssignKeysBlock b2
                return $ concat [ks1, ks2, ks3, ks4, ks5]
            AST.While e b1 b2 _ -> do
                ks1 <- collectAssignKeysExpr e
                ks2 <- maybe (return []) collectAssignKeysBlock b1
                ks3 <- maybe (return []) collectAssignKeysBlock b2
                return $ concat[ks1, ks2, ks3]
            AST.Until e b1 b2 _ -> do
                ks1 <- collectAssignKeysExpr e
                ks2 <- maybe (return []) collectAssignKeysBlock b1
                ks3 <- maybe (return []) collectAssignKeysBlock b2
                return $ concat [ks1, ks2, ks3]
            AST.Loop b _ -> maybe (return []) collectAssignKeysBlock b
            AST.Repeat e b1 b2 _ -> do
                ks1 <- collectAssignKeysExpr e
                ks2 <- maybe (return []) collectAssignKeysBlock b1
                ks3 <- maybe (return []) collectAssignKeysBlock b2
                return $ concat [ks1, ks2, ks3]
            AST.DoWhile b1 e b2 _ -> do
                ks1 <- maybe (return []) collectAssignKeysBlock b1
                ks2 <- collectAssignKeysExpr e
                ks3 <- maybe (return []) collectAssignKeysBlock b2
                return $ concat [ks1, ks2, ks3]
            AST.DoUntil b1 e b2 _ -> do
                ks1 <- maybe (return []) collectAssignKeysBlock b1
                ks2 <- collectAssignKeysExpr e
                ks3 <- maybe (return []) collectAssignKeysBlock b2
                return $ concat [ks1, ks2, ks3]
            AST.Switch e scs _ -> do
                ks1 <- collectAssignKeysExpr e
                ks2 <- concat <$> mapM collectAssignKeysCase scs
                return (ks1 ++ ks2)
            AST.Function {} -> return []
            AST.FunctionT {} -> return []
            AST.Command {} -> return []

        collectAssignKeysCase :: AST.SwitchCase -> TACM [VarKey]
        collectAssignKeysCase sc = case sc of
            AST.Case e b _ -> do
                ks1 <- collectAssignKeysExpr e
                ks2 <- maybe (return []) collectAssignKeysBlock b
                return (ks1 ++ ks2)
            AST.Default b _ -> collectAssignKeysBlock b

        collectAssignKeysExpr :: Expression -> TACM [VarKey]
        collectAssignKeysExpr expr = case expr of
            AST.Binary op lhs rhs _ | isAssignLikeOp op -> do
                ks1 <- collectAssignKeysLhs lhs
                ks2 <- collectAssignKeysExpr rhs
                return (ks1 ++ ks2)
            AST.Binary _ e1 e2 _ -> do
                ks1 <- collectAssignKeysExpr e1
                ks2 <- collectAssignKeysExpr e2
                return (ks1 ++ ks2)
            AST.Unary op e _ | isIncDecOp op -> do
                ks1 <- collectAssignKeysLhs e
                ks2 <- collectAssignKeysExpr e
                return (ks1 ++ ks2)
            AST.Unary _ e _ -> collectAssignKeysExpr e
            AST.Cast _ e _ -> collectAssignKeysExpr e
            AST.Call callee args -> concat <$> mapM collectAssignKeysExpr (callee : args)
            AST.CallT callee _ args -> concat <$> mapM collectAssignKeysExpr (callee : args)
            _ -> return []

        isAssignLikeOp :: AST.Operator -> Bool
        isAssignLikeOp op = op `elem` [
            AST.Assign,
            AST.PlusAssign, AST.MinusAssign,
            AST.MultiplyAssign, AST.DivideAssign,
            AST.ModuloAssign, AST.PowerAssign]

        isIncDecOp :: AST.Operator -> Bool
        isIncDecOp op = op `elem` [AST.IncSelf, AST.DecSelf, AST.SelfInc, AST.SelfDec]

        collectAssignKeysLhs :: Expression -> TACM [VarKey]
        collectAssignKeysLhs lhs = case lhs of
            AST.Variable _ tok -> do
                vinfo <- getVar [tokenPos tok]
                case vinfo of
                    TEnv.VarLocal _ realName vid -> do
                        key <- resolveVarKey (realName, vid)
                        return [key]
                    _ -> return []
            _ -> return []

-- | Append trailing nodes after a condition lowering sequence.
--   If the condition already ends in a nested block marker, append into its
--   deepest tail so jump/cfg layout stays stable; otherwise append directly.
appendAfterCond :: [IRNode] -> [IRNode] -> [IRNode]
appendAfterCond condStmts tailStmts = case reverse condStmts of
    (IRBlockStmt (bid, body) : rest) ->
        reverse (IRBlockStmt (appendTail (bid, body) tailStmts) : rest)
    _ ->
        dedupAppend condStmts tailStmts
    where
        appendTail :: (Int, [IRNode]) -> [IRNode] -> (Int, [IRNode])
        appendTail (bid, body) extra = case reverse body of
            (IRBlockStmt blk : rest) ->
                (bid, reverse (IRBlockStmt (appendTail blk extra) : rest))
            _ ->
                (bid, dedupAppend body extra)

        dedupAppend :: [IRNode] -> [IRNode] -> [IRNode]
        dedupAppend base extra = case (reverse base, extra) of
            (IRInstr (TAC.Jump a) : _, [IRInstr (TAC.Jump b)]) | a == b ->
                base
            _ ->
                base ++ extra


emitDefaultDeclInit :: Maybe Class -> Token -> TACM [IRNode]
emitDefaultDeclInit mTy nameTok = do
    vinfo <- getVar [tokenPos nameTok]
    case vinfo of
        TEnv.VarLocal _ realName vid -> do
            key <- resolveVarKey (realName, vid)
            let
                declT = fromMaybe Int32T mTy
            lhsAtom <- newSubVar declT key
            isStatic <- isStaticVar key
            let initInstrs = case defaultAtomForClass declT of
                    Just rhsAtom -> [IRInstr (TAC.IAssign lhsAtom rhsAtom)]
                    Nothing -> []
                putStaticInstrs = ([IRInstr (TAC.IPutStatic [realName] lhsAtom) | isStatic])
            return (initInstrs ++ putStaticInstrs)
        _ -> return []


defaultAtomForClass :: Class -> Maybe IRAtom
defaultAtomForClass cls = case cls of
    Int8T -> Just (TAC.Int8C 0)
    Int16T -> Just (TAC.Int16C 0)
    Int32T -> Just (TAC.Int32C 0)
    Int64T -> Just (TAC.Int64C 0)
    Float32T -> Just (TAC.Float32C 0)
    Float64T -> Just (TAC.Float64C 0)
    Float128T -> Just (TAC.Float128C 0)
    Bool -> Just (TAC.BoolC False)
    Char -> Just (TAC.CharC '\0')
    Class ["String"] [] -> Just (TAC.StringC "")
    Class ["java", "lang", "String"] [] -> Just (TAC.StringC "")
    Class ["xlang", "String"] [] -> Just (TAC.StringC "")
    _ -> Nothing


stmtsLowing :: [Statement] -> TACM [IRNode]
stmtsLowing [] = return []
stmtsLowing ((AST.Command AST.Pass _):stmts) = stmtsLowing stmts
stmtsLowing ((AST.Command AST.Continue _):stmts) = do
    (startId, _) <- getCurrentLoop
    phis <- getCurrentLoopPhis
    phiInstrs <- fmap catMaybes (mapM assignPhiFromCurrent phis)
    rest <- stmtsLowing stmts
    return (map IRInstr phiInstrs ++ IRInstr (TAC.Jump startId) : rest)

stmtsLowing ((AST.Command AST.Break _):stmts) = do
    (_, afterId) <- getCurrentLoop
    phis <- getCurrentLoopPhis
    phiInstrs <- fmap catMaybes (mapM assignPhiFromCurrent phis)
    rest <- stmtsLowing stmts
    return (map IRInstr phiInstrs ++ IRInstr (TAC.Jump afterId) : rest)

stmtsLowing ((AST.Command (AST.Return Nothing) _):stmts) = do
    (_, retBId, _) <- getCurrentFun
    rest <- stmtsLowing stmts
    return (IRInstr (TAC.Jump retBId) : rest)

stmtsLowing ((AST.Command (AST.Return (Just e)) _):stmts) = do
    (_, retBId, _) <- getCurrentFun
    (setInstrs, returnAtom) <- if AST.isAtom e then atomLowing e else exprLowing e
    let setReturnStmt = IRInstr (TAC.SetRet returnAtom)
        retStmts = appendAfterCond (reverse setInstrs)
            [setReturnStmt, IRInstr (TAC.Jump retBId)]
    rest <- stmtsLowing stmts
    return $ appendAfterCond retStmts rest

stmtsLowing ((AST.Expr e):stmts) = let
    exprLowing' ex = exprLowing ex >>= \(instrs, _) -> return $ reverse instrs in do
        current <- exprLowing' e
        rest <- stmtsLowing stmts
        return $ appendAfterCond current rest

stmtsLowing ((AST.Exprs es):stmts) = do
    current <- fmap concat $ mapM lowerExprInstr es
    rest <- stmtsLowing stmts
    return $ appendAfterCond current rest
    where
        lowerExprInstr :: Expression -> TACM [IRNode]
        lowerExprInstr e = do
            (instrs, _) <- if AST.isAtom e then atomLowing e else exprLowing e
            return (reverse instrs)

stmtsLowing ((AST.StmtGroup ss):stmts) = stmtsLowing (ss ++ stmts)

-- Nested functions are hoisted by 'promoteTopLevelFunctions' before lowering.
-- Keep this as a safe fallback to avoid internal compiler panics.
stmtsLowing ((AST.Function {}):stmts) = stmtsLowing stmts
stmtsLowing ((AST.FunctionT {}):stmts) = stmtsLowing stmts

stmtsLowing ((AST.BlockStmt b):stmts) = do
    current <- blockLowing b
    rest <- stmtsLowing stmts
    return $ appendAfterCond current rest

stmtsLowing ((AST.For (mInit, mCond, mStep) bodyB elseB (forTok, _)):stmts) = do
    l0ID <- incBlockId
    l1ID <- incBlockId
    l2ID <- incBlockId
    lStepID <- incBlockId
    lElseID <- incBlockId
    lAfterID <- incBlockId

    initStmts <- case mInit of
        Nothing -> return []
        Just st -> lowerForPartStmt st

    preStacks <- getVarStacks

    let bodyForKeys = fromMaybe (AST.Multiple []) bodyB
        stepForKeys = maybe [] pure mStep
        allLoopBlock = case bodyForKeys of
            AST.Multiple ss -> AST.Multiple (ss ++ stepForKeys)
    loopKeys <- collectAssignKeysBlock allLoopBlock
    phiInfos <- catMaybes <$> mapM (mkPhi preStacks) loopKeys

    let condExpr = fromMaybe (AST.BoolConst True forTok) mCond
    (condInstrs, condAtom) <- if AST.isAtom condExpr then atomLowing condExpr else exprLowing condExpr
    headerStacks <- getVarStacks

    (bodyStmts, stepStmts, bodyStacks, elseStmts) <- withLoop (lStepID, lAfterID) $ do
        pushLoopPhis [(key, dst) | (key, dst, _) <- phiInfos]

        bodyStmts' <- blockLowing bodyForKeys
        stepStmts' <- case mStep of
            Nothing -> return []
            Just st -> lowerForPartStmt st

        bodyStacks' <- getVarStacks

        setVarStacks headerStacks
        elseStmts' <- blockLowing $ fromMaybe (AST.Multiple []) elseB

        _ <- popLoopPhis
        return (bodyStmts', stepStmts', bodyStacks', elseStmts')

    let condStmts = appendAfterCond (reverse condInstrs) (
            [IRInstr (TAC.Ifeq condAtom (TAC.Int32C 1) (l2ID, lElseID))])

    phiInfos' <- mapM (attachBodyAtom bodyStacks) phiInfos
    let preAssigns = map (\(_, dst, preAtom, _) -> IRInstr (TAC.IAssign dst preAtom)) phiInfos'
    let backAssigns = map (\(_, dst, _, bodyAtom) -> IRInstr (TAC.IAssign dst bodyAtom)) phiInfos'
    let phiInstrs = map (\(_, dst, preAtom, bodyAtom) ->
            IRInstr (TAC.IAssign dst (TAC.Phi [(l0ID, preAtom), (lStepID, bodyAtom)]))) phiInfos'

    let preBlock = IRBlockStmt (l0ID, initStmts ++ preAssigns ++ [IRInstr (TAC.Jump l1ID)])
    let bodyBlock = IRBlockStmt (l2ID, appendAfterCond bodyStmts [IRInstr (TAC.Jump lStepID)])
    let stepTail = backAssigns ++ [IRInstr (TAC.Jump l1ID)]
    let stepBlock = IRBlockStmt (lStepID, appendAfterCond stepStmts stepTail)
    let headerBlock = IRBlockStmt (l1ID, phiInstrs ++ condStmts)
    let elseBlock = IRBlockStmt (lElseID, elseStmts ++ [IRInstr (TAC.Jump lAfterID)])

    afterStmts <- stmtsLowing stmts
    let afterBlock = IRBlockStmt (lAfterID, afterStmts)

    return [preBlock, bodyBlock, stepBlock, headerBlock, elseBlock, afterBlock]
    where
        mkPhi :: Map VarKey [(Class, Int)] -> VarKey -> TACM (Maybe (VarKey, IRAtom, IRAtom))
        mkPhi stacks key = do
            mPre <- topAtomFromStacksM stacks key
            case mPre of
                Just (cls, preAtom) -> do
                    dst <- newSubVar cls key
                    return (Just (key, dst, preAtom))
                Nothing -> return Nothing

        attachBodyAtom :: Map VarKey [(Class, Int)] -> (VarKey, IRAtom, IRAtom) -> TACM (VarKey, IRAtom, IRAtom, IRAtom)
        attachBodyAtom stacks (key, dst, preAtom) = do
            mBody <- topAtomInStacksM stacks key
            let bodyAtom = fromMaybe dst mBody
            return (key, dst, preAtom, bodyAtom)

        lowerForPartStmt :: Statement -> TACM [IRNode]
        lowerForPartStmt st = case st of
            AST.Expr e -> lowerForPartExpr e
            AST.Exprs es -> fmap concat $ mapM lowerForPartExpr es
            AST.StmtGroup ss -> fmap concat $ mapM lowerForPartStmt ss
            AST.BlockStmt b -> blockLowing b
            AST.DefField {} -> stmtsLowing [st]
            AST.DefConstField {} -> stmtsLowing [st]
            AST.DefVar {} -> stmtsLowing [st]
            AST.DefConstVar {} -> stmtsLowing [st]
            _ -> return []

        lowerForPartExpr :: Expression -> TACM [IRNode]
        lowerForPartExpr e = do
            (instrs, _) <- if AST.isAtom e then atomLowing e else exprLowing e
            return (reverse instrs)

-- loop is an infinite loop: equivalent to `while true` without else
stmtsLowing ((AST.Loop bodyB loopTok):stmts) =
    stmtsLowing (AST.While (AST.BoolConst True loopTok) bodyB Nothing (loopTok, Nothing) : stmts)

-- repeat(count) means run body exactly count times.
stmtsLowing ((AST.Repeat countExpr bodyB elseB _):stmts) = do
    l0ID <- incBlockId
    l1ID <- incBlockId
    l2ID <- incBlockId
    lStepID <- incBlockId
    lElseID <- incBlockId
    lAfterID <- incBlockId

    preStacks <- getVarStacks
    loopKeys <- collectAssignKeysBlock (fromMaybe (AST.Multiple []) bodyB)
    phiInfos <- catMaybes <$> mapM (mkPhi preStacks) loopKeys

    (countInstrs, countAtom) <- if AST.isAtom countExpr then atomLowing countExpr else exprLowing countExpr
    countClass <- getAtomType countAtom
    if not (isRepeatCounterClass countClass)
        then error "repeat counter type must be byte/short/int/long"
        else pure ()

    headerStacks <- getVarStacks

    let zeroAtom = zeroAtomForClass countClass
        oneAtom = oneAtomForClass countClass

    idxCurr <- newSubCVar countClass
    idxNext <- newSubCVar countClass

    (bodyStmts, bodyStacks, elseStmts) <- withLoop (lStepID, lAfterID) $ do
        pushLoopPhis [(key, dst) | (key, dst, _) <- phiInfos]
        bodyStmts' <- blockLowing $ fromMaybe (AST.Multiple []) bodyB
        bodyStacks' <- getVarStacks
        setVarStacks headerStacks
        elseStmts' <- blockLowing $ fromMaybe (AST.Multiple []) elseB
        _ <- popLoopPhis
        return (bodyStmts', bodyStacks', elseStmts')

    phiInfos' <- mapM (attachBodyAtom bodyStacks) phiInfos
    let preAssigns = map (\(_, dst, preAtom, _) -> IRInstr (TAC.IAssign dst preAtom)) phiInfos'
    let backAssigns = map (\(_, dst, _, bodyAtom) -> IRInstr (TAC.IAssign dst bodyAtom)) phiInfos'
    let preTail = [IRInstr (TAC.IAssign idxCurr zeroAtom)] ++ preAssigns ++ [IRInstr (TAC.Jump l1ID)]
    let preBlockBody = appendAfterCond (reverse countInstrs) preTail
    let preBlock = IRBlockStmt (l0ID, preBlockBody)

    let condStmts = [
            IRInstr (TAC.Iflt idxCurr countAtom (l2ID, lElseID))
            ]
    let headerBlock = IRBlockStmt (l1ID, condStmts)
    let elseBlock = IRBlockStmt (lElseID, elseStmts ++ [IRInstr (TAC.Jump lAfterID)])

    let bodyBlock = IRBlockStmt (l2ID, appendAfterCond bodyStmts [IRInstr (TAC.Jump lStepID)])
    let stepTail = [
            IRInstr (TAC.IBinary idxNext AST.Add idxCurr oneAtom),
            IRInstr (TAC.IAssign idxCurr idxNext)
            ] ++ backAssigns ++ [IRInstr (TAC.Jump l1ID)]
    let stepBlock = IRBlockStmt (lStepID, stepTail)

    afterStmts <- stmtsLowing stmts
    let afterBlock = IRBlockStmt (lAfterID, afterStmts)

    return [preBlock, bodyBlock, stepBlock, headerBlock, elseBlock, afterBlock]
    where
        mkPhi :: Map VarKey [(Class, Int)] -> VarKey -> TACM (Maybe (VarKey, IRAtom, IRAtom))
        mkPhi stacks key = do
            mPre <- topAtomFromStacksM stacks key
            case mPre of
                Just (cls, preAtom) -> do
                    dst <- newSubVar cls key
                    return (Just (key, dst, preAtom))
                Nothing -> return Nothing

        attachBodyAtom :: Map VarKey [(Class, Int)] -> (VarKey, IRAtom, IRAtom) -> TACM (VarKey, IRAtom, IRAtom, IRAtom)
        attachBodyAtom stacks (key, dst, preAtom) = do
            mBody <- topAtomInStacksM stacks key
            let bodyAtom = fromMaybe dst mBody
            return (key, dst, preAtom, bodyAtom)

stmtsLowing ((AST.DefField names mDeclType mRhs toks):stmts) =
    stmtsLowing (AST.DefVar names mDeclType mRhs toks:stmts)

stmtsLowing ((AST.DefConstField names mDeclType mRhs toks):stmts) =
    stmtsLowing (AST.DefConstVar names mDeclType mRhs toks:stmts)

stmtsLowing ((AST.DefVar names mDeclType mRhs toks):stmts) = do
    let exprLowing' ex = exprLowing ex >>= \(instrs, _) -> return (reverse instrs)
    current <- case (names, mRhs, reverse toks) of
        ([name], Just rhs, nameTok:_) ->
            let assignTok = case toks of
                    (_:tokEq:_) -> tokEq
                    _ -> nameTok
                rhsTyped = case mDeclType of
                    Just declT -> AST.Cast (declT, []) rhs assignTok
                    Nothing -> rhs
                assignExpr = AST.Binary AST.Assign (AST.Variable name nameTok) rhsTyped assignTok
            in exprLowing' assignExpr
        ([_], Nothing, nameTok:_) ->
            emitDefaultDeclInit mDeclType nameTok
        (_, Just rhs, _) ->
            exprLowing' rhs
        (_, Nothing, _) ->
            return []
    rest <- stmtsLowing stmts
    return $ appendAfterCond current rest

stmtsLowing ((AST.DefConstVar names mDeclType mRhs toks):stmts) = do
    let exprLowing' ex = exprLowing ex >>= \(instrs, _) -> return (reverse instrs)
    current <- case (names, mRhs, reverse toks) of
        ([name], Just rhs, nameTok:_) ->
            let assignTok = case toks of
                    (_:tokEq:_) -> tokEq
                    _ -> nameTok
                rhsTyped = case mDeclType of
                    Just declT -> AST.Cast (declT, []) rhs assignTok
                    Nothing -> rhs
                assignExpr = AST.Binary AST.Assign (AST.Variable name nameTok) rhsTyped assignTok
            in exprLowing' assignExpr
        ([_], Nothing, nameTok:_) ->
            emitDefaultDeclInit mDeclType nameTok
        (_, Just rhs, _) ->
            exprLowing' rhs
        (_, Nothing, _) ->
            return []
    rest <- stmtsLowing stmts
    return $ appendAfterCond current rest


{-
code:
    if cond
        b() nullable
    else
        c() nullable

    d()

IR:
    if cond goto .L_then
    goto .L_else

    .L_then
        b()
        goto .L_join

    .L_else
        c()
        goto .L_join

    .L_join
        d()
-}
stmtsLowing ((AST.If e thenB elseB _):stmts) = do
    lThenID <- incBlockId
    lElseID <- incBlockId
    lJoinID <- incBlockId

    _preStacks <- getVarStacks

    (condInstrs, condAtom) <- if AST.isAtom e then atomLowing e else exprLowing e
    let condStmts = appendAfterCond (reverse condInstrs) [
            IRInstr (TAC.Ifeq condAtom (TAC.Int32C 1) (lThenID, lElseID))]
    condStacks <- getVarStacks

    thenStmts <- blockLowing $ fromMaybe (AST.Multiple []) thenB
    thenStacks <- getVarStacks

    setVarStacks condStacks
    elseStmts <- blockLowing $ fromMaybe (AST.Multiple []) elseB
    elseStacks <- getVarStacks

    let phiKeys = diffVarKeys thenStacks elseStacks

    setVarStacks condStacks
    phiInfos <- catMaybes <$> mapM (mkPhi thenStacks elseStacks) phiKeys

    afterStmts <- stmtsLowing stmts

    let thenAssigns = map (\(dst, thenAtom, _) -> IRInstr (TAC.IAssign dst thenAtom)) phiInfos
    let elseAssigns = map (\(dst, _, elseAtom) -> IRInstr (TAC.IAssign dst elseAtom)) phiInfos
    let phiInstrs = map (\(dst, thenAtom, elseAtom) ->
            IRInstr (TAC.IAssign dst (TAC.Phi [(lThenID, thenAtom), (lElseID, elseAtom)]))) phiInfos

    let thenTail = thenAssigns ++ [IRInstr (TAC.Jump lJoinID)]
    let elseTail = elseAssigns ++ [IRInstr (TAC.Jump lJoinID)]
    let thenBlock = IRBlockStmt (lThenID, appendAfterCond thenStmts thenTail)
    let elseBlock = IRBlockStmt (lElseID, appendAfterCond elseStmts elseTail)
    let joinBlock = IRBlockStmt (lJoinID, phiInstrs ++ afterStmts)

    return (condStmts ++ [thenBlock, elseBlock, joinBlock])
    where
        mkPhi :: Map VarKey [(Class, Int)] -> Map VarKey [(Class, Int)] -> VarKey -> TACM (Maybe (IRAtom, IRAtom, IRAtom))
        mkPhi thenStacks elseStacks key = do
            mThen <- topAtomFromStacksM thenStacks key
            mElse <- topAtomFromStacksM elseStacks key
            case (mThen, mElse) of
                (Just (cls, thenAtom), Just (_, elseAtom)) -> do
                    dst <- newSubVar cls key
                    return (Just (dst, thenAtom, elseAtom))
                _ -> return Nothing


{-
code:
    while cond
        a()
    else:
        b()
    c()

IR:
    goto L1.

    .L2:
        a()
        goto .L1

    .L1:                    ; start of the loop
        if cond got .L2
        b()
        goto .L3
    .L3:                    ; after of the loop
        c()


if there is continue goto .L1
if there is break then goto .L3

-- so pushCurrentLoop (L1, L3)
-}
stmtsLowing ((AST.While e bodyB elseB _):stmts) = do
    l0ID <- incBlockId
    l1ID <- incBlockId
    l2ID <- incBlockId
    lElseID <- incBlockId
    l3ID <- incBlockId

    preStacks <- getVarStacks
    loopKeys <- collectAssignKeysBlock (fromMaybe (AST.Multiple []) bodyB)

    phiInfos <- catMaybes <$> mapM (mkPhi preStacks) loopKeys

    (condInstrs, condAtom) <- if AST.isAtom e then atomLowing e else exprLowing e
    headerStacks <- getVarStacks

    (bodyStmts, bodyStacks, elseStmts) <- withLoop (l1ID, l3ID) $ do
        pushLoopPhis [(key, dst) | (key, dst, _) <- phiInfos]

        bodyStmts' <- blockLowing $ fromMaybe (AST.Multiple []) bodyB
        bodyStacks' <- getVarStacks

        setVarStacks headerStacks
        elseStmts' <- blockLowing $ fromMaybe (AST.Multiple []) elseB

        _ <- popLoopPhis
        return (bodyStmts', bodyStacks', elseStmts')

    let condStmts = appendAfterCond (reverse condInstrs) (
            [IRInstr (TAC.Ifeq condAtom (TAC.Int32C 1) (l2ID, lElseID))])

    phiInfos' <- mapM (attachBodyAtom bodyStacks) phiInfos
    let preAssigns = map (\(_, dst, preAtom, _) -> IRInstr (TAC.IAssign dst preAtom)) phiInfos'
    let backAssigns = map (\(_, dst, _, bodyAtom) -> IRInstr (TAC.IAssign dst bodyAtom)) phiInfos'
    let phiInstrs = map (\(_, dst, preAtom, bodyAtom) ->
            IRInstr (TAC.IAssign dst (TAC.Phi [(l0ID, preAtom), (l2ID, bodyAtom)]))) phiInfos'

    let preBlock = IRBlockStmt (l0ID, preAssigns ++ [IRInstr (TAC.Jump l1ID)])
    let bodyTail = backAssigns ++ [IRInstr (TAC.Jump l1ID)]
    l2TailID <- incBlockId
    let bodyTailBlock = IRBlockStmt (l2TailID, bodyTail)
    let bodyBlock = IRBlockStmt (l2ID, bodyStmts ++ [bodyTailBlock])
    let headerBlock = IRBlockStmt (l1ID, phiInstrs ++ condStmts)
    let elseBlock = IRBlockStmt (lElseID, elseStmts ++ [IRInstr (TAC.Jump l3ID)])

    afterStmts <- stmtsLowing stmts
    let afterBlock = IRBlockStmt (l3ID, afterStmts)

    return [preBlock, bodyBlock, headerBlock, elseBlock, afterBlock]
    where
        mkPhi :: Map VarKey [(Class, Int)] -> VarKey -> TACM (Maybe (VarKey, IRAtom, IRAtom))
        mkPhi stacks key = do
            mPre <- topAtomFromStacksM stacks key
            case mPre of
                Just (cls, preAtom) -> do
                    dst <- newSubVar cls key
                    return (Just (key, dst, preAtom))
                Nothing -> return Nothing

        attachBodyAtom :: Map VarKey [(Class, Int)] -> (VarKey, IRAtom, IRAtom) -> TACM (VarKey, IRAtom, IRAtom, IRAtom)
        attachBodyAtom stacks (key, dst, preAtom) = do
            mBody <- topAtomInStacksM stacks key
            let bodyAtom = fromMaybe dst mBody
            return (key, dst, preAtom, bodyAtom)

stmtsLowing ((AST.Until e bodyB elseB _):stmts) = do
    l0ID <- incBlockId
    l1ID <- incBlockId
    l2ID <- incBlockId
    lElseID <- incBlockId
    l3ID <- incBlockId

    preStacks <- getVarStacks
    loopKeys <- collectAssignKeysBlock (fromMaybe (AST.Multiple []) bodyB)

    phiInfos <- catMaybes <$> mapM (mkPhi preStacks) loopKeys

    (condInstrs, condAtom) <- if AST.isAtom e then atomLowing e else exprLowing e
    headerStacks <- getVarStacks

    (bodyStmts, bodyStacks, elseStmts) <- withLoop (l1ID, l3ID) $ do
        pushLoopPhis [(key, dst) | (key, dst, _) <- phiInfos]

        bodyStmts' <- blockLowing $ fromMaybe (AST.Multiple []) bodyB
        bodyStacks' <- getVarStacks

        setVarStacks headerStacks
        elseStmts' <- blockLowing $ fromMaybe (AST.Multiple []) elseB

        _ <- popLoopPhis
        return (bodyStmts', bodyStacks', elseStmts')

    let condStmts = appendAfterCond (reverse condInstrs) (
            [IRInstr (TAC.Ifeq condAtom (TAC.Int32C 0) (l2ID, lElseID))])

    phiInfos' <- mapM (attachBodyAtom bodyStacks) phiInfos
    let preAssigns = map (\(_, dst, preAtom, _) -> IRInstr (TAC.IAssign dst preAtom)) phiInfos'
    let backAssigns = map (\(_, dst, _, bodyAtom) -> IRInstr (TAC.IAssign dst bodyAtom)) phiInfos'
    let phiInstrs = map (\(_, dst, preAtom, bodyAtom) ->
            IRInstr (TAC.IAssign dst (TAC.Phi [(l0ID, preAtom), (l2ID, bodyAtom)]))) phiInfos'

    let preBlock = IRBlockStmt (l0ID, preAssigns ++ [IRInstr (TAC.Jump l1ID)])
    let bodyTail = backAssigns ++ [IRInstr (TAC.Jump l1ID)]
    l2TailID <- incBlockId
    let bodyTailBlock = IRBlockStmt (l2TailID, bodyTail)
    let bodyBlock = IRBlockStmt (l2ID, bodyStmts ++ [bodyTailBlock])
    let headerBlock = IRBlockStmt (l1ID, phiInstrs ++ condStmts)
    let elseBlock = IRBlockStmt (lElseID, elseStmts ++ [IRInstr (TAC.Jump l3ID)])

    afterStmts <- stmtsLowing stmts
    let afterBlock = IRBlockStmt (l3ID, afterStmts)

    return [preBlock, bodyBlock, headerBlock, elseBlock, afterBlock]
    where
        mkPhi :: Map VarKey [(Class, Int)] -> VarKey -> TACM (Maybe (VarKey, IRAtom, IRAtom))
        mkPhi stacks key = do
            mPre <- topAtomFromStacksM stacks key
            case mPre of
                Just (cls, preAtom) -> do
                    dst <- newSubVar cls key
                    return (Just (key, dst, preAtom))
                Nothing -> return Nothing

        attachBodyAtom :: Map VarKey [(Class, Int)] -> (VarKey, IRAtom, IRAtom) -> TACM (VarKey, IRAtom, IRAtom, IRAtom)
        attachBodyAtom stacks (key, dst, preAtom) = do
            mBody <- topAtomInStacksM stacks key
            let bodyAtom = fromMaybe dst mBody
            return (key, dst, preAtom, bodyAtom)

stmtsLowing ((AST.DoWhile bodyB e elseB _):stmts) = do
    l0ID <- incBlockId
    l1ID <- incBlockId
    l2ID <- incBlockId
    l3ID <- incBlockId
    lElseID <- incBlockId
    l4ID <- incBlockId

    preStacks <- getVarStacks
    loopKeys <- collectAssignKeysBlock (fromMaybe (AST.Multiple []) bodyB)
    phiInfos <- catMaybes <$> mapM (mkPhi preStacks) loopKeys

    (bodyStmts, bodyStacks) <- withLoop (l2ID, l4ID) $ do
        pushLoopPhis [(key, dst) | (key, dst, _) <- phiInfos]
        bodyStmts' <- blockLowing $ fromMaybe (AST.Multiple []) bodyB
        bodyStacks' <- getVarStacks
        _ <- popLoopPhis
        return (bodyStmts', bodyStacks')

    setVarStacks bodyStacks
    (condInstrs, condAtom) <- if AST.isAtom e then atomLowing e else exprLowing e
    condStacks <- getVarStacks

    setVarStacks condStacks
    elseStmts <- blockLowing $ fromMaybe (AST.Multiple []) elseB

    phiInfos' <- mapM (attachBodyAtom bodyStacks) phiInfos
    let preAssigns = map (\(_, dst, preAtom, _) -> IRInstr (TAC.IAssign dst preAtom)) phiInfos'
    let backAssigns = map (\(_, dst, _, bodyAtom) -> IRInstr (TAC.IAssign dst bodyAtom)) phiInfos'
    let phiInstrs = map (\(_, dst, preAtom, bodyAtom) ->
            IRInstr (TAC.IAssign dst (TAC.Phi [(l0ID, preAtom), (l3ID, bodyAtom)]))) phiInfos'

    let preBlock = IRBlockStmt (l0ID, preAssigns ++ [IRInstr (TAC.Jump l1ID)])
    let bodyBlock = IRBlockStmt (l1ID, phiInstrs ++ appendAfterCond bodyStmts [IRInstr (TAC.Jump l2ID)])
    let condTail = appendAfterCond (reverse condInstrs) (
            [IRInstr (TAC.Ifeq condAtom (TAC.Int32C 1) (l3ID, lElseID))])
    let condBlock = IRBlockStmt (l2ID, condTail)
    let backBlock = IRBlockStmt (l3ID, backAssigns ++ [IRInstr (TAC.Jump l1ID)])
    let elseBlock = IRBlockStmt (lElseID, elseStmts ++ [IRInstr (TAC.Jump l4ID)])

    afterStmts <- stmtsLowing stmts
    let afterBlock = IRBlockStmt (l4ID, afterStmts)
    return [preBlock, bodyBlock, condBlock, backBlock, elseBlock, afterBlock]
    where
        mkPhi :: Map VarKey [(Class, Int)] -> VarKey -> TACM (Maybe (VarKey, IRAtom, IRAtom))
        mkPhi stacks key = do
            mPre <- topAtomFromStacksM stacks key
            case mPre of
                Just (cls, preAtom) -> do
                    dst <- newSubVar cls key
                    return (Just (key, dst, preAtom))
                Nothing -> return Nothing

        attachBodyAtom :: Map VarKey [(Class, Int)] -> (VarKey, IRAtom, IRAtom) -> TACM (VarKey, IRAtom, IRAtom, IRAtom)
        attachBodyAtom stacks (key, dst, preAtom) = do
            mBody <- topAtomInStacksM stacks key
            let bodyAtom = fromMaybe dst mBody
            return (key, dst, preAtom, bodyAtom)

stmtsLowing ((AST.DoUntil bodyB e elseB _):stmts) = do
    l0ID <- incBlockId
    l1ID <- incBlockId
    l2ID <- incBlockId
    l3ID <- incBlockId
    lElseID <- incBlockId
    l4ID <- incBlockId

    preStacks <- getVarStacks
    loopKeys <- collectAssignKeysBlock (fromMaybe (AST.Multiple []) bodyB)
    phiInfos <- catMaybes <$> mapM (mkPhi preStacks) loopKeys

    (bodyStmts, bodyStacks) <- withLoop (l2ID, l4ID) $ do
        pushLoopPhis [(key, dst) | (key, dst, _) <- phiInfos]
        bodyStmts' <- blockLowing $ fromMaybe (AST.Multiple []) bodyB
        bodyStacks' <- getVarStacks
        _ <- popLoopPhis
        return (bodyStmts', bodyStacks')

    setVarStacks bodyStacks
    (condInstrs, condAtom) <- if AST.isAtom e then atomLowing e else exprLowing e
    condStacks <- getVarStacks

    setVarStacks condStacks
    elseStmts <- blockLowing $ fromMaybe (AST.Multiple []) elseB

    phiInfos' <- mapM (attachBodyAtom bodyStacks) phiInfos
    let preAssigns = map (\(_, dst, preAtom, _) -> IRInstr (TAC.IAssign dst preAtom)) phiInfos'
    let backAssigns = map (\(_, dst, _, bodyAtom) -> IRInstr (TAC.IAssign dst bodyAtom)) phiInfos'
    let phiInstrs = map (\(_, dst, preAtom, bodyAtom) ->
            IRInstr (TAC.IAssign dst (TAC.Phi [(l0ID, preAtom), (l3ID, bodyAtom)]))) phiInfos'

    let preBlock = IRBlockStmt (l0ID, preAssigns ++ [IRInstr (TAC.Jump l1ID)])
    let bodyBlock = IRBlockStmt (l1ID, phiInstrs ++ appendAfterCond bodyStmts [IRInstr (TAC.Jump l2ID)])
    let condTail = appendAfterCond (reverse condInstrs) (
            [IRInstr (TAC.Ifeq condAtom (TAC.Int32C 0) (l3ID, lElseID))])
    let condBlock = IRBlockStmt (l2ID, condTail)
    let backBlock = IRBlockStmt (l3ID, backAssigns ++ [IRInstr (TAC.Jump l1ID)])
    let elseBlock = IRBlockStmt (lElseID, elseStmts ++ [IRInstr (TAC.Jump l4ID)])

    afterStmts <- stmtsLowing stmts
    let afterBlock = IRBlockStmt (l4ID, afterStmts)
    return [preBlock, bodyBlock, condBlock, backBlock, elseBlock, afterBlock]
    where
        mkPhi :: Map VarKey [(Class, Int)] -> VarKey -> TACM (Maybe (VarKey, IRAtom, IRAtom))
        mkPhi stacks key = do
            mPre <- topAtomFromStacksM stacks key
            case mPre of
                Just (cls, preAtom) -> do
                    dst <- newSubVar cls key
                    return (Just (key, dst, preAtom))
                Nothing -> return Nothing

        attachBodyAtom :: Map VarKey [(Class, Int)] -> (VarKey, IRAtom, IRAtom) -> TACM (VarKey, IRAtom, IRAtom, IRAtom)
        attachBodyAtom stacks (key, dst, preAtom) = do
            mBody <- topAtomInStacksM stacks key
            let bodyAtom = fromMaybe dst mBody
            return (key, dst, preAtom, bodyAtom)


stmtsLowing _ = error "other branch is not supporting TODO"


collectAtomTypes :: TEnv.FunSig -> [IRNode] -> TACM (Map IRAtom Class)
collectAtomTypes sig stmts = do
    varTypeMap <- getAtomTypes
    pure $ foldl' (collectStmt sig varTypeMap) Map.empty stmts
    where
        collectStmt ::
            TEnv.FunSig ->
            Map IRAtom Class ->
            Map IRAtom Class ->
            IRNode ->
            Map IRAtom Class
        collectStmt sig' varTypeMap acc stmt = case stmt of
            IRInstr instr -> foldl' (collectAtom sig' varTypeMap) acc (atomsInInstr instr)
            IRBlockStmt (_, stmts') ->
                foldl' (collectStmt sig' varTypeMap) acc stmts'

        collectAtom ::
            TEnv.FunSig ->
            Map IRAtom Class ->
            Map IRAtom Class ->
            IRAtom ->
            Map IRAtom Class
        collectAtom sig' varTypeMap acc atom =
            let acc1 = case inferAtomType sig' varTypeMap atom of
                    Just cls -> Map.insert atom cls acc
                    Nothing -> acc
            in case atom of
                TAC.Phi pairs -> foldl' (collectAtom sig' varTypeMap) acc1 (map snd pairs)
                _ -> acc1

        inferAtomType :: TEnv.FunSig -> Map IRAtom Class -> IRAtom -> Maybe Class
        inferAtomType sig' varTypeMap atom = case atom of
            TAC.BoolC _ -> Just Bool
            TAC.CharC _ -> Just Char
            TAC.Int8C _ -> Just Int8T
            TAC.Int16C _ -> Just Int16T
            TAC.Int32C _ -> Just Int32T
            TAC.Int64C _ -> Just Int64T
            TAC.Float32C _ -> Just Float32T
            TAC.Float64C _ -> Just Float64T
            TAC.Float128C _ -> Just Float128T
            TAC.StringC _ -> Just (Class ["String"] [])
            TAC.Var _ -> Map.lookup atom varTypeMap
            TAC.Param i -> Just (TEnv.funParams sig' !! i)
            TAC.Phi pairs -> case pairs of
                ((_, a) : _) -> inferAtomType sig' varTypeMap a
                [] -> Nothing

        atomsInInstr :: IRInstr -> [IRAtom]
        atomsInInstr instr = case instr of
            TAC.Jump _ -> []
            TAC.Ifeq a b _ -> [a, b]
            TAC.Ifne a b _ -> [a, b]
            TAC.Iflt a b _ -> [a, b]
            TAC.Ifle a b _ -> [a, b]
            TAC.Ifgt a b _ -> [a, b]
            TAC.Ifge a b _ -> [a, b]
            TAC.SetRet atom -> [atom]
            TAC.Return -> []
            TAC.VReturn -> []
            TAC.IAssign dst src -> [dst, src]
            TAC.IUnary dst _ x -> [dst, x]
            TAC.IBinary dst _ x y -> [dst, x, y]
            TAC.ICast dst _ x -> [dst, x]
            TAC.ICall dst _ args -> dst : args
            TAC.ICallStatic dst _ args -> dst : args
            TAC.IGetField dst obj _ -> [dst, obj]
            TAC.IPutField obj _ v -> [obj, v]
            TAC.IGetStatic dst _ -> [dst]
            TAC.IPutStatic _ v -> [v]


collectAtomTypesStatic :: [IRNode] -> TACM (Map IRAtom Class)
collectAtomTypesStatic stmts = do
    varTypeMap <- getAtomTypes
    pure $ foldl' (collectStmt varTypeMap) Map.empty stmts
    where
        collectStmt ::
            Map IRAtom Class ->
            Map IRAtom Class ->
            IRNode ->
            Map IRAtom Class
        collectStmt varTypeMap acc stmt = case stmt of
            IRInstr instr -> foldl' (collectAtom varTypeMap) acc (atomsInInstr instr)
            IRBlockStmt (_, stmts') ->
                foldl' (collectStmt varTypeMap) acc stmts'

        collectAtom ::
            Map IRAtom Class ->
            Map IRAtom Class ->
            IRAtom ->
            Map IRAtom Class
        collectAtom varTypeMap acc atom =
            let acc1 = case inferAtomType varTypeMap atom of
                    Just cls -> Map.insert atom cls acc
                    Nothing -> acc
            in case atom of
                TAC.Phi pairs -> foldl' (collectAtom varTypeMap) acc1 (map snd pairs)
                _ -> acc1

        inferAtomType :: Map IRAtom Class -> IRAtom -> Maybe Class
        inferAtomType varTypeMap atom = case atom of
            TAC.BoolC _ -> Just Bool
            TAC.CharC _ -> Just Char
            TAC.Int8C _ -> Just Int8T
            TAC.Int16C _ -> Just Int16T
            TAC.Int32C _ -> Just Int32T
            TAC.Int64C _ -> Just Int64T
            TAC.Float32C _ -> Just Float32T
            TAC.Float64C _ -> Just Float64T
            TAC.Float128C _ -> Just Float128T
            TAC.StringC _ -> Just (Class ["String"] [])
            TAC.Var _ -> Map.lookup atom varTypeMap
            TAC.Param _ -> Nothing
            TAC.Phi pairs -> case pairs of
                ((_, a) : _) -> inferAtomType varTypeMap a
                [] -> Nothing

        atomsInInstr :: IRInstr -> [IRAtom]
        atomsInInstr instr = case instr of
            TAC.Jump _ -> []
            TAC.Ifeq a b _ -> [a, b]
            TAC.Ifne a b _ -> [a, b]
            TAC.Iflt a b _ -> [a, b]
            TAC.Ifle a b _ -> [a, b]
            TAC.Ifgt a b _ -> [a, b]
            TAC.Ifge a b _ -> [a, b]
            TAC.SetRet atom -> [atom]
            TAC.Return -> []
            TAC.VReturn -> []
            TAC.IAssign dst src -> [dst, src]
            TAC.IUnary dst _ x -> [dst, x]
            TAC.IBinary dst _ x y -> [dst, x, y]
            TAC.ICast dst _ x -> [dst, x]
            TAC.ICall dst _ args -> dst : args
            TAC.ICallStatic dst _ args -> dst : args
            TAC.IGetField dst obj _ -> [dst, obj]
            TAC.IPutField obj _ v -> [obj, v]
            TAC.IGetStatic dst _ -> [dst]
            TAC.IPutStatic _ v -> [v]


functionLowering :: Statement -> TACM IRFunction
functionLowering (AST.Function (clazz, declToks) (AST.Variable funName _) args functB) = do
    -- load all args
    atoms <- mapM loadArgs args
    let _argsMap = genArgMap atoms -- TODO: use this map to rewrite args into Param 0..n


    let funSig = TEnv.FunSig {
        TEnv.funParams = map (\(a, _, _) -> a) args,
        TEnv.funReturn = clazz
    }

    retBId <- incBlockId
    let retInstr = if clazz == Void then TAC.VReturn else TAC.Return
    let retBlock = IRBlockStmt (retBId, [IRInstr retInstr])

    funStmts <- TAC.withFun (funSig, retBId, _argsMap) $ blockLowing functB
    let stmtsWithRet = funStmts ++ [retBlock]
    atomTypes <- collectAtomTypes funSig stmtsWithRet
    let bodyBlocks = packIRBlocks stmtsWithRet

    let access0 = accessFromDeclTokens declToks
        generated = '$' `elem` funName
        access = if generated then PB.Private else access0
        flags = if generated then [PB.Static, PB.Final] else [PB.Static]
        decl = (access, flags)
    return $ TAC.IRFunction decl funName funSig atomTypes bodyBlocks TAC.MemberClassWrapped
        
    where
        loadArgs :: (Class, String, [Token]) -> TACM IRAtom
        loadArgs (argClazz, _, tokens) = do
            let pos = tokenPos $ head  tokens

            -- the first one is type bro
            vinfo <- getVar [pos]
            case vinfo of
                TEnv.VarImported {} -> error "args cannot be imported"
                TEnv.VarLocal _ str vid -> newSubVar argClazz (str, vid)

        genArgMap :: [IRAtom] -> Map Int IRAtom
        genArgMap atoms = Map.fromList $ zip [0..] atoms

functionLowering (AST.Function _ (AST.Qualified _ _) _ _ ) = error "this is not supported yet"
functionLowering _ = error "this is not a function!!!"

accessFromDeclTokens :: [Token] -> PB.AccessModified
accessFromDeclTokens toks = case toks of
    (Ident s _ : _) ->
        let k = map toLower s
        in if k == "private"
            then PB.Private
            else if k == "protected"
                then PB.Protected
                else PB.Public
    _ -> PB.Public


-- | Lower non-function class statements into a synthetic class:
--   non-function statements become static-init, functions become methods.
classStmtsLowing :: [String] -> String -> [Statement] -> TACM IRClass
classStmtsLowing pkgSegs name stmts = do
    let (funcDef, rest0) = partition AST.isFunction stmts
    let (_, rest1) = partition AST.isFunctionT rest0

    let staticKeyMap = collectAssignKey rest1
        constKeyMap = collectConstKey rest1
    staticKeys <- mapM resolveStaticKey (Map.toList staticKeyMap)
    addStaticVars staticKeys

    staticStmts0 <- stmtsLowing rest1
    staticAtomTypes <- collectAtomTypesStatic staticStmts0
    staticFields <- mapM (staticFieldFor constKeyMap) staticKeys
    funcs0 <- mapM functionLowering funcDef
    let classQName = pkgSegs ++ [name]
        staticBlocks0 = packIRBlocks staticStmts0
        staticBlocks = qualifyBlocks classQName staticBlocks0
        funcs = map (qualifyFunction classQName) funcs0
        mainKind = detectMainKind classQName funcs
    let decl = (PB.Public, []) -- TODO: default class decl until parser carries modifiers.
    return $ TAC.IRClass decl name staticFields (TAC.StaticInit staticBlocks) staticAtomTypes funcs mainKind
    where
        resolveStaticKey :: (String, [Position]) -> TACM (String, Int)
        resolveStaticKey (_, poss) = do
            pos <- case poss of
                (p:_) -> return p
                [] -> error "collectAssignKey: empty position list"
            vinfo <- getVar [pos]
            case vinfo of
                TEnv.VarLocal _ str vid -> return (str, vid)
                _ -> error "collectAssignKey: this should be catched in Semantic"
        staticFieldFor :: Map String [Position] -> (String, Int) -> TACM (PB.Decl, Class, String, TAC.IRMemberType)
        staticFieldFor consts (varName, vid) = do
            (clazz, _) <- peekVarStack (varName, vid)
            let flags = if Map.member varName consts then [PB.Static, PB.Final] else [PB.Static]
                declStatic = (PB.Public, flags) -- TODO: use parsed modifiers
            return (declStatic, clazz, varName, TAC.MemberClassWrapped)
        qualifyFunction :: [String] -> TAC.IRFunction -> TAC.IRFunction
        qualifyFunction cls (TAC.IRFunction decl fname sig atomTypes body memberType) =
            TAC.IRFunction decl fname sig atomTypes (qualifyBlocks cls body) memberType

        qualifyBlocks :: [String] -> [TAC.IRBlock] -> [TAC.IRBlock]
        qualifyBlocks cls = map (qualifyBlock cls)

        qualifyBlock :: [String] -> TAC.IRBlock -> TAC.IRBlock
        qualifyBlock cls (TAC.IRBlock (bid, instrs)) =
            TAC.IRBlock (bid, map (qualifyInstr cls) instrs)

        qualifyInstr :: [String] -> IRInstr -> IRInstr
        qualifyInstr cls instr = case instr of
            TAC.ICall dst name' args -> TAC.ICallStatic dst (cls ++ [name']) args
            TAC.ICallStatic dst qn args -> TAC.ICallStatic dst (qualifyQName cls qn) args
            TAC.IGetStatic dst qn -> TAC.IGetStatic dst (qualifyQName cls qn)
            TAC.IPutStatic qn v -> TAC.IPutStatic (qualifyQName cls qn) v
            _ -> instr

        qualifyQName :: [String] -> [String] -> [String]
        qualifyQName cls qn
            | length qn == 1 = cls ++ qn
            | isPkgOnly = cls ++ [last qn]
            | otherwise = qn
            where
                pkg = take (length cls - 1) cls
                isPkgOnly = length qn == length cls && take (length cls - 1) qn == pkg


detectMainKind :: QName -> [IRFunction] -> TAC.MainKind
detectMainKind classQName = foldl' pick TAC.NoMain . map classify
    where
        pick :: TAC.MainKind -> TAC.MainKind -> TAC.MainKind
        pick acc kind
            | rank kind > rank acc = kind
            | otherwise = acc

        rank :: TAC.MainKind -> Int
        rank TAC.NoMain = 0
        rank (TAC.MainInt _) = 1
        rank (TAC.MainVoid _) = 2
        rank (TAC.MainIntArgs _) = 3
        rank (TAC.MainVoidArgs _) = 4

        classify :: IRFunction -> TAC.MainKind
        classify (TAC.IRFunction _ "main" sig _ _ _) =
            case (TEnv.funReturn sig, TEnv.funParams sig) of
                (Int32T, []) -> TAC.MainInt mainQName
                (Void, []) -> TAC.MainVoid mainQName
                (Int32T, [param]) | isStringArray param -> TAC.MainIntArgs mainQName
                (Void, [param]) | isStringArray param -> TAC.MainVoidArgs mainQName
                _ -> TAC.NoMain
        classify _ = TAC.NoMain

        mainQName :: QName
        mainQName = classQName ++ ["main"]

        isStringArray :: Class -> Bool
        isStringArray (Array elemT 1) = isStringClass elemT
        isStringArray _ = False

        isStringClass :: Class -> Bool
        isStringClass (Class qn []) =
            qn == ["String"] || qn == ["java", "lang", "String"] || qn == ["xlang", "String"]
        isStringClass _ = False


-- | Lower a class statement into IR (not implemented yet).
classLowing :: Statement -> TACM IRClass
classLowing _ = error "the class is not implement"


-- | Lower a whole program into IR:
--   extract class declarations, and wrap remaining stmts into a synthetic class.
progmLowing :: Path -> Program -> IRProgm
progmLowing path (decls, stmts) =
    let (decls', stmts') = AST.inlineProgramFunctions (AST.promoteTopLevelFunctions (decls, stmts))
        (classStmts, otherStmts) = partition AST.isClassDeclar stmts'
        pkgSegs = case filter AST.isPackageDecl decls' of
            (d:_) -> AST.declPath d
            [] -> []
        mainClassName = fromMaybe (toMainClassName path) (AST.getJavaName (decls', stmts'))
        action = do
            classIRs <- mapM classLowing classStmts
            extraIRs <- if null otherStmts
                then return []
                else (:[]) <$> classStmtsLowing pkgSegs mainClassName otherStmts
            return $ TAC.IRProgm pkgSegs (classIRs ++ extraIRs)
    in
        let st0 = TAC.mkTACState Map.empty Map.empty -- TODO: pass semantic var/fun uses
        in evalState (TAC.runTACM action) st0
    where
        -- | Build synthetic class name from file name: MainX style.
        toMainClassName :: Path -> String
        toMainClassName p =
            let fileName = takeFileName p
                base = takeWhile (/= '.') fileName
                cap = case base of
                    [] -> "Main"
                    (c:cs) -> toUpper c : cs
            in cap ++ "X"
        -- | Extract file name from a path (no path module dependency).
        takeFileName :: FilePath -> FilePath
        takeFileName p =
            let rev = reverse p
                nameRev = takeWhile (\c -> c /= '/' && c /= '\\') rev
            in reverse nameRev


-- | Lower a block into a list of IR statements.
blockLowing :: Block -> TACM [IRNode]
blockLowing (AST.Multiple stmts) = stmtsLowing stmts


-- | Collect LHS variable names from assignment statements.
--   Stores all positions for the same name.
collectAssignKey :: [Statement] -> Map String [Position]
collectAssignKey = foldl step Map.empty
    where
        step :: Map String [Position] -> Statement -> Map String [Position]
        step acc stmt = case stmt of
            AST.Expr (AST.Binary _ (AST.Variable name tok) _ _) ->
                insertOnce name (tokenPos tok) acc
            AST.DefField [name] _ _ toks ->
                case reverse toks of
                    (nameTok:_) -> insertOnce name (tokenPos nameTok) acc
                    [] -> acc
            AST.DefConstField [name] _ _ toks ->
                case reverse toks of
                    (nameTok:_) -> insertOnce name (tokenPos nameTok) acc
                    [] -> acc
            AST.DefVar [name] _ _ toks ->
                case reverse toks of
                    (nameTok:_) -> insertOnce name (tokenPos nameTok) acc
                    [] -> acc
            AST.DefConstVar [name] _ _ toks ->
                case reverse toks of
                    (nameTok:_) -> insertOnce name (tokenPos nameTok) acc
                    [] -> acc
            _ -> acc

        insertOnce :: String -> Position -> Map String [Position] -> Map String [Position]
        insertOnce name pos = Map.insertWith (++) name [pos]


collectConstKey :: [Statement] -> Map String [Position]
collectConstKey = foldl step Map.empty
    where
        step :: Map String [Position] -> Statement -> Map String [Position]
        step acc stmt = case stmt of
            AST.DefConstField [name] _ _ toks ->
                case reverse toks of
                    (nameTok:_) -> Map.insertWith (++) name [tokenPos nameTok] acc
                    [] -> acc
            AST.DefConstVar [name] _ _ toks ->
                case reverse toks of
                    (nameTok:_) -> Map.insertWith (++) name [tokenPos nameTok] acc
                    [] -> acc
            _ -> acc








