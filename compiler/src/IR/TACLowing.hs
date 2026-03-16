{-# LANGUAGE HexFloatLiterals #-}
{-# LANGUAGE TupleSections #-}


module IR.TACLowing where

import Data.Bits ((.&.))
import Control.Monad.State.Strict (evalState)
import Data.List (partition, foldl')
import Data.Char (toUpper)
import Text.Read (readMaybe)
import Data.Maybe (listToMaybe, fromMaybe, catMaybes)
import Data.Foldable (foldrM)
import Data.Map.Strict (Map)
import Numeric (readHex)

import Lex.Token (Token, tokenPos)
import Parse.SyntaxTree (Block, Class(..), Expression, Program, Statement)
import Semantic.OpInfer (binaryOpCastType, inferUnaryOp, inferBinaryOp, isBasicType, isCompareOp, promoteBasicType)
import IR.TAC (IRInstr, IRAtom, TACM, IRStmt, IRProgm, IRFunction, IRClass, newSubVar, newSubCVar, getVar, peekVarStack,
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
    case readMaybe (stripFloatSuffix raw) :: Maybe Double of
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
    case readMaybe (stripFloatSuffix raw) :: Maybe Double of
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
atomLowing :: Expression -> TACM ([IRStmt], IRAtom)
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
            let key = (realName, vid)
            isStatic <- isStaticVar key
            if isStatic
                then do
                    (clazz, _) <- peekVarStack key
                    nAtom <- newSubCVar clazz
                    return ([TAC.IRInstr (TAC.IGetStatic nAtom [realName])], nAtom)
                else do
                    (_, ver) <- peekVarStack key
                    let atom = TAC.Var (realName, vid, ver)

                    mFun <- TAC.getCurrentFunMaybe
                    case mFun >>= (\(_, _, argMap) -> lookupParamIndex atom argMap) of
                        Just idx -> return ([], TAC.Param idx)
                        Nothing -> return ([], atom)

        -- ??????
        TEnv.VarImported _ clazz _ fullQname -> do
            nAtom <- newSubCVar clazz
            return ([TAC.IRInstr (TAC.IGetStatic nAtom fullQname)], nAtom)

atomLowing (AST.Qualified _ tokens) = do
    vinfo <- getVar (map tokenPos tokens)
    case vinfo of
        TEnv.VarImported _ clazz _ fullQname -> do
            nAtom <- newSubCVar clazz
            return ([TAC.IRInstr (TAC.IGetStatic nAtom fullQname)], nAtom)
        _ -> error "qualified name is not an imported variable"

atomLowing _ = error "other type is not allowed for IR atom"


-- | Find parameter index for a given atom, if it is a function argument.
lookupParamIndex :: IRAtom -> Map Int IRAtom -> Maybe Int
lookupParamIndex atom =
    Map.foldrWithKey (\k v acc -> if v == atom then Just k else acc) Nothing


castIfNeeded :: Class -> IRAtom -> Class -> TACM ([IRStmt], IRAtom)
castIfNeeded from atom to
    | from == to = return ([], atom)
    | otherwise = do
        castAtom <- newSubCVar to
        return ([TAC.IRInstr (TAC.ICast castAtom (from, to) atom)], castAtom)


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


-- return instruction and output atom eg: a = a + b => a1 = a0 + b0 return a1
-- warning [IRStmt] is reversed!
exprLowing :: Expression -> TACM ([IRStmt], IRAtom)
exprLowing (AST.Cast (toClass, _) inner _) = do
    (instrs, oAtom) <- if AST.isAtom inner then atomLowing inner else exprLowing inner
    fromClass <- getAtomType oAtom
    nAtom <- newSubCVar toClass
    return (TAC.IRInstr (TAC.ICast nAtom (fromClass, toClass) oAtom) : instrs, nAtom)

-- ++a
exprLowing (AST.Unary AST.IncSelf inner _) = do
    (instr, oAtom) <- if AST.isAtom inner then atomLowing inner else exprLowing inner
    fromClass <- getAtomType oAtom
    let oneAtom = oneAtomForClass fromClass
    case oAtom of
        TAC.Var (name, vid, _) -> do
            nextAtom <- newSubVar fromClass (name, vid)
            let addInstr = TAC.IRInstr (TAC.IBinary nextAtom AST.Add oAtom oneAtom)
            return (addInstr : instr, nextAtom)
        TAC.Param _ -> do
            tmpAtom <- newSubCVar fromClass
            let addInstr = TAC.IRInstr (TAC.IBinary tmpAtom AST.Add oAtom oneAtom)
            let writeBack = TAC.IRInstr (TAC.IAssign oAtom tmpAtom)
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
            let subInstr = TAC.IRInstr (TAC.IBinary nextAtom AST.Sub oAtom oneAtom)
            return (subInstr : instr, nextAtom)
        TAC.Param _ -> do
            tmpAtom <- newSubCVar fromClass
            let subInstr = TAC.IRInstr (TAC.IBinary tmpAtom AST.Sub oAtom oneAtom)
            let writeBack = TAC.IRInstr (TAC.IAssign oAtom tmpAtom)
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
            let copyInstr = TAC.IRInstr (TAC.IAssign oldAtom oAtom)
            let addInstr = TAC.IRInstr (TAC.IBinary nextAtom AST.Add oAtom oneAtom)
            return ([addInstr, copyInstr] ++ instr, oldAtom)
        TAC.Param _ -> do
            oldAtom <- newSubCVar fromClass
            tempAtom <- newSubCVar fromClass
            let copyInstr = TAC.IRInstr (TAC.IAssign oldAtom oAtom)
            let addInstr = TAC.IRInstr (TAC.IBinary tempAtom AST.Add oAtom oneAtom)
            let writeBack = TAC.IRInstr (TAC.IAssign oAtom tempAtom)
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
            let copyInstr = TAC.IRInstr (TAC.IAssign oldAtom oAtom)
            let subInstr = TAC.IRInstr (TAC.IBinary nextAtom AST.Sub oAtom oneAtom)
            return ([subInstr, copyInstr] ++ instr, oldAtom)
        TAC.Param _ -> do
            oldAtom <- newSubCVar fromClass
            tempAtom <- newSubCVar fromClass
            let copyInstr = TAC.IRInstr (TAC.IAssign oldAtom oAtom)
            let subInstr = TAC.IRInstr (TAC.IBinary tempAtom AST.Sub oAtom oneAtom)
            let writeBack = TAC.IRInstr (TAC.IAssign oAtom tempAtom)
            return ([writeBack, subInstr, copyInstr] ++ instr, oldAtom)
        _ -> error "SelfDec expects Var/Param atom"


exprLowing (AST.Unary op inner _) = do
    (instr, oAtom) <- if AST.isAtom inner then atomLowing inner else exprLowing inner
    fromClass <- getAtomType oAtom
    let toClass = inferUnaryOp op fromClass

    if fromClass == toClass then do
        nAtom <- newSubCVar toClass
        return (TAC.IRInstr (TAC.IUnary nAtom op oAtom) : instr, nAtom)
    else do
        (castInstrs, castAtom) <- castIfNeeded fromClass oAtom toClass
        nAtom <- newSubCVar toClass
        return (TAC.IRInstr (TAC.IUnary nAtom op castAtom) : castInstrs ++ instr, nAtom)

exprLowing (AST.Binary AST.Assign (AST.Variable _ tok) e2 _) = do
    let pos = tokenPos tok
    vinfo <- getVar [pos]
    case vinfo of
        TEnv.VarLocal _ realName vid -> do
            let key = (realName, vid)
            isStatic <- isStaticVar key

            oldCur <- TAC.getCurrentVar
                    
            TAC.setCurrentVar (Just key)

            (instrs, rhsAtom) <- if AST.isAtom e2 then atomLowing e2 else exprLowing e2
            clazz <- getAtomType rhsAtom
            lhsAtom <- newSubCVar clazz

            TAC.setCurrentVar oldCur

            return $
                if isStatic
                    then (TAC.IRInstr (TAC.IPutStatic [realName] lhsAtom) : TAC.IRInstr (TAC.IAssign lhsAtom rhsAtom) : instrs, lhsAtom)
                    else (TAC.IRInstr (TAC.IAssign lhsAtom rhsAtom) : instrs, lhsAtom)
            
        TEnv.VarImported _ _ _ fullQname -> do
            (instrs, rhsAtom) <- if AST.isAtom e2 then atomLowing e2 else exprLowing e2
            return (TAC.IRInstr (TAC.IPutStatic fullQname rhsAtom) : instrs, rhsAtom)

-- TODO for class
exprLowing (AST.Binary AST.Assign (AST.Qualified names toks) e2 _) =
    case names of
        ("this":_) -> error "TODO: assign to this.field is not supported yet"
        _ -> do
            vinfo <- getVar (map tokenPos toks)
            case vinfo of
                TEnv.VarImported _ _ _ fullQname -> do
                    (instrs, rhsAtom) <- if AST.isAtom e2 then atomLowing e2 else exprLowing e2
                    return (TAC.IRInstr (TAC.IPutStatic fullQname rhsAtom) : instrs, rhsAtom)
                _ -> error "assign lhs is not an imported qualified name"


exprLowing (AST.Binary AST.Assign _ _ _) = error "cannot be assign in his case. the error should be catched in context check"


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
                AST.Equal -> TAC.Ifeq atom1' atom2' lTrue
                AST.NotEqual -> TAC.Ifne atom1' atom2' lTrue
                AST.LessThan -> TAC.Iflt atom1' atom2' lTrue
                AST.LessEqual -> TAC.Ifle atom1' atom2' lTrue
                AST.GreaterThan -> TAC.Ifgt atom1' atom2' lTrue
                AST.GreaterEqual -> TAC.Ifge atom1' atom2' lTrue
                _ -> error "not a compare op"

            trueBlock = TAC.IRBlockStmt (TAC.IRBlock (lTrue, [
                TAC.IRInstr (TAC.IAssign nAtom (TAC.BoolC True)),
                TAC.IRInstr (TAC.Jump lJoin)
                ]))
            falseBlock = TAC.IRBlockStmt (TAC.IRBlock (lFalse, [
                TAC.IRInstr (TAC.IAssign nAtom (TAC.BoolC False)),
                TAC.IRInstr (TAC.Jump lJoin)
                ]))
            phiInstr = TAC.IRInstr (TAC.IAssign nAtom (TAC.Phi [(lTrue, TAC.BoolC True), (lFalse, TAC.BoolC False)]))
            joinBlock = TAC.IRBlockStmt (TAC.IRBlock (lJoin, [phiInstr]))

        let seqRev =
                [joinBlock, falseBlock, trueBlock, TAC.IRInstr (TAC.Jump lFalse), TAC.IRInstr jumpInstr]
                ++ cast2Instrs ++ instr2 ++ cast1Instrs ++ instr1
        return (seqRev, nAtom)
    else do
        nAtom <- newSubCVar resClass
        return (TAC.IRInstr (TAC.IBinary nAtom op atom1' atom2') : concat [cast2Instrs, instr2, cast1Instrs, instr1], nAtom)

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
            TAC.IRInstr (TAC.Ifeq condAtom (TAC.Int32C 1) lThen),
            TAC.IRInstr (TAC.Jump lElse)]
        thenBody = appendAfterCond (reverse thenInstrs) (
            thenCastInstrs ++ [
                TAC.IRInstr (TAC.IAssign nAtom thenAtom),
                TAC.IRInstr (TAC.Jump lJoin)])
        elseBody = appendAfterCond (reverse elseInstrs) (
            elseCastInstrs ++ [
                TAC.IRInstr (TAC.IAssign nAtom elseAtom),
                TAC.IRInstr (TAC.Jump lJoin)])
        thenBlock = TAC.IRBlockStmt (TAC.IRBlock (lThen, thenBody))
        elseBlock = TAC.IRBlockStmt (TAC.IRBlock (lElse, elseBody))
        phiInstr = TAC.IRInstr (TAC.IAssign nAtom (TAC.Phi [(lThen, thenAtom), (lElse, elseAtom)]))
        joinBlock = TAC.IRBlockStmt (TAC.IRBlock (lJoin, [phiInstr]))
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
            return (TAC.IRInstr instr : argInstrs, dst)
        TEnv.FunImported _ _ fullQname sig -> do
            let retT = TEnv.funReturn sig
            (argInstrs, argAtoms) <- lowerArgsWithSig (TEnv.funParams sig) params
            dst <- newSubCVar retT
            return (TAC.IRInstr (TAC.ICallStatic dst fullQname argAtoms) : argInstrs, dst)
    where
        lowerArgsWithSig :: [Class] -> [Expression] -> TACM ([IRStmt], [IRAtom])
        lowerArgsWithSig paramTs args
            | length paramTs /= length args = error "internal error: argument count mismatch after typecheck"
            | otherwise = foldrM step ([], []) (zip args paramTs)
            where
                step :: (Expression, Class) -> ([IRStmt], [IRAtom]) -> TACM ([IRStmt], [IRAtom])
                step (p, paramT) (instrsRest, atomsRest) = do
                    (instrsP, atomP) <- if AST.isAtom p then atomLowing p else exprLowing p
                    argT <- getAtomType atomP
                    (castInstrs, castAtom) <- castIfNeeded argT atomP paramT
                    return (instrsRest ++ castInstrs ++ instrsP, castAtom : atomsRest)

exprLowing (AST.CallT {}) = error "template is not support!"

exprLowing _ = error "other type is not support for IR ast"


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
            AST.Command (AST.Return (Just e)) _ -> collectAssignKeysExpr e
            AST.BlockStmt b -> collectAssignKeysBlock b
            AST.If e b1 b2 _ -> do
                ks1 <- collectAssignKeysExpr e
                ks2 <- maybe (return []) collectAssignKeysBlock b1
                ks3 <- maybe (return []) collectAssignKeysBlock b2
                return $ concat [ks1, ks2, ks3]
            AST.For (e1, e2, e3) b _ -> do
                ks1 <- maybe (return []) collectAssignKeysExpr e1
                ks2 <- maybe (return []) collectAssignKeysExpr e2
                ks3 <- maybe (return []) collectAssignKeysExpr e3
                ks4 <- maybe (return []) collectAssignKeysBlock b
                return $ concat [ks1, ks2, ks3, ks4]
            AST.While e b1 b2 _ -> do
                ks1 <- collectAssignKeysExpr e
                ks2 <- maybe (return []) collectAssignKeysBlock b1
                ks3 <- maybe (return []) collectAssignKeysBlock b2
                return $ concat[ks1, ks2, ks3]
            AST.DoWhile b1 e b2 _ -> do
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
            AST.Binary AST.Assign lhs rhs _ -> do
                ks1 <- collectAssignKeysLhs lhs
                ks2 <- collectAssignKeysExpr rhs
                return (ks1 ++ ks2)
            AST.Binary _ e1 e2 _ -> do
                ks1 <- collectAssignKeysExpr e1
                ks2 <- collectAssignKeysExpr e2
                return (ks1 ++ ks2)
            AST.Unary _ e _ -> collectAssignKeysExpr e
            AST.Cast _ e _ -> collectAssignKeysExpr e
            AST.Call callee args -> concat <$> mapM collectAssignKeysExpr (callee : args)
            AST.CallT callee _ args -> concat <$> mapM collectAssignKeysExpr (callee : args)
            _ -> return []

        collectAssignKeysLhs :: Expression -> TACM [VarKey]
        collectAssignKeysLhs lhs = case lhs of
            AST.Variable _ tok -> do
                vinfo <- getVar [tokenPos tok]
                case vinfo of
                    TEnv.VarLocal _ realName vid -> return [(realName, vid)]
                    _ -> return []
            _ -> return []

appendAfterCond :: [IRStmt] -> [IRStmt] -> [IRStmt]
appendAfterCond condStmts tailStmts = case reverse condStmts of
    (TAC.IRBlockStmt (TAC.IRBlock (bid, body)) : rest) ->
        reverse (TAC.IRBlockStmt (appendTail (TAC.IRBlock (bid, body)) tailStmts) : rest)
    _ ->
        dedupAppend condStmts tailStmts
    where
        appendTail :: TAC.IRBlock -> [IRStmt] -> TAC.IRBlock
        appendTail (TAC.IRBlock (bid, body)) extra = case reverse body of
            (TAC.IRBlockStmt blk : rest) ->
                TAC.IRBlock (bid, reverse (TAC.IRBlockStmt (appendTail blk extra) : rest))
            _ ->
                TAC.IRBlock (bid, dedupAppend body extra)

        dedupAppend :: [IRStmt] -> [IRStmt] -> [IRStmt]
        dedupAppend base extra = case (reverse base, extra) of
            (TAC.IRInstr (TAC.Jump a) : _, [TAC.IRInstr (TAC.Jump b)]) | a == b ->
                base
            _ ->
                base ++ extra


emitDefaultDeclInit :: Maybe Class -> Token -> TACM [IRStmt]
emitDefaultDeclInit mTy nameTok = do
    vinfo <- getVar [tokenPos nameTok]
    case vinfo of
        TEnv.VarLocal _ realName vid -> do
            let key = (realName, vid)
                declT = fromMaybe Int32T mTy
            lhsAtom <- newSubVar declT key
            isStatic <- isStaticVar key
            let initInstrs = case defaultAtomForClass declT of
                    Just rhsAtom -> [TAC.IRInstr (TAC.IAssign lhsAtom rhsAtom)]
                    Nothing -> []
                putStaticInstrs = ([TAC.IRInstr (TAC.IPutStatic [realName] lhsAtom) | isStatic])
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
    Class ["java", "lang", "String"] [] -> Just (TAC.StringC "")
    _ -> Nothing


stmtsLowing :: [Statement] -> TACM [IRStmt]
stmtsLowing [] = return []
stmtsLowing ((AST.Command AST.Continue _):stmts) = do
    (startId, _) <- getCurrentLoop
    phis <- getCurrentLoopPhis
    phiInstrs <- fmap catMaybes (mapM assignPhiFromCurrent phis)
    rest <- stmtsLowing stmts
    return (map TAC.IRInstr phiInstrs ++ TAC.IRInstr (TAC.Jump startId) : rest)

stmtsLowing ((AST.Command AST.Break _):stmts) = do
    (_, afterId) <- getCurrentLoop
    phis <- getCurrentLoopPhis
    phiInstrs <- fmap catMaybes (mapM assignPhiFromCurrent phis)
    rest <- stmtsLowing stmts
    return (map TAC.IRInstr phiInstrs ++ TAC.IRInstr (TAC.Jump afterId) : rest)

stmtsLowing ((AST.Command (AST.Return Nothing) _):stmts) = do
    (_, retBId, _) <- getCurrentFun
    rest <- stmtsLowing stmts
    return (TAC.IRInstr (TAC.Jump retBId) : rest)

stmtsLowing ((AST.Command (AST.Return (Just e)) _):stmts) = do
    (_, retBId, _) <- getCurrentFun
    (setInstrs, returnAtom) <- if AST.isAtom e then atomLowing e else exprLowing e
    let setReturnStmt = TAC.IRInstr (TAC.SetIRet returnAtom)
        retStmts = appendAfterCond (reverse setInstrs)
            [setReturnStmt, TAC.IRInstr (TAC.Jump retBId)]
    rest <- stmtsLowing stmts
    return $ appendAfterCond retStmts rest

stmtsLowing ((AST.Expr e):stmts) = let
    exprLowing' ex = exprLowing ex >>= \(instrs, _) -> return $ reverse instrs in do
        current <- exprLowing' e
        rest <- stmtsLowing stmts
        return $ appendAfterCond current rest

stmtsLowing ((AST.BlockStmt b):stmts) = do
    current <- blockLowing b
    rest <- stmtsLowing stmts
    return $ appendAfterCond current rest

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
            TAC.IRInstr (TAC.Ifeq condAtom (TAC.Int32C 1) lThenID),
            TAC.IRInstr (TAC.Jump lElseID)]
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

    let thenAssigns = map (\(dst, thenAtom, _) -> TAC.IRInstr (TAC.IAssign dst thenAtom)) phiInfos
    let elseAssigns = map (\(dst, _, elseAtom) -> TAC.IRInstr (TAC.IAssign dst elseAtom)) phiInfos
    let phiInstrs = map (\(dst, thenAtom, elseAtom) ->
            TAC.IRInstr (TAC.IAssign dst (TAC.Phi [(lThenID, thenAtom), (lElseID, elseAtom)]))) phiInfos

    let thenTail = thenAssigns ++ [TAC.IRInstr (TAC.Jump lJoinID)]
    let elseTail = elseAssigns ++ [TAC.IRInstr (TAC.Jump lJoinID)]
    let thenBlock = TAC.IRBlockStmt (TAC.IRBlock (lThenID, appendAfterCond thenStmts thenTail))
    let elseBlock = TAC.IRBlockStmt (TAC.IRBlock (lElseID, appendAfterCond elseStmts elseTail))
    let joinBlock = TAC.IRBlockStmt (TAC.IRBlock (lJoinID, phiInstrs ++ afterStmts))

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
            [TAC.IRInstr (TAC.Ifeq condAtom (TAC.Int32C 1) l2ID)] ++
            elseStmts ++
            [TAC.IRInstr (TAC.Jump l3ID)])

    phiInfos' <- mapM (attachBodyAtom bodyStacks) phiInfos
    let preAssigns = map (\(_, dst, preAtom, _) -> TAC.IRInstr (TAC.IAssign dst preAtom)) phiInfos'
    let backAssigns = map (\(_, dst, _, bodyAtom) -> TAC.IRInstr (TAC.IAssign dst bodyAtom)) phiInfos'
    let phiInstrs = map (\(_, dst, preAtom, bodyAtom) ->
            TAC.IRInstr (TAC.IAssign dst (TAC.Phi [(l0ID, preAtom), (l2ID, bodyAtom)]))) phiInfos'

    let preBlock = TAC.IRBlockStmt (TAC.IRBlock (l0ID, preAssigns ++ [TAC.IRInstr (TAC.Jump l1ID)]))
    let bodyTail = backAssigns ++ [TAC.IRInstr (TAC.Jump l1ID)]
    l2TailID <- incBlockId
    let bodyTailBlock = TAC.IRBlockStmt (TAC.IRBlock (l2TailID, bodyTail))
    let bodyBlock = TAC.IRBlockStmt (TAC.IRBlock (l2ID, bodyStmts ++ [bodyTailBlock]))
    let headerBlock = TAC.IRBlockStmt (TAC.IRBlock (l1ID, phiInstrs ++ condStmts))

    afterStmts <- stmtsLowing stmts
    let afterBlock = TAC.IRBlockStmt (TAC.IRBlock (l3ID, afterStmts))

    return [preBlock, bodyBlock, headerBlock, afterBlock]
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


collectAtomTypes :: TEnv.FunSig -> [IRStmt] -> TACM (Map IRAtom Class)
collectAtomTypes sig stmts = do
    varTypeMap <- getAtomTypes
    pure $ foldl' (collectStmt sig varTypeMap) Map.empty stmts
    where
        collectStmt ::
            TEnv.FunSig ->
            Map IRAtom Class ->
            Map IRAtom Class ->
            IRStmt ->
            Map IRAtom Class
        collectStmt sig' varTypeMap acc stmt = case stmt of
            TAC.IRInstr instr -> foldl' (collectAtom sig' varTypeMap) acc (atomsInInstr instr)
            TAC.IRBlockStmt (TAC.IRBlock (_, stmts')) ->
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
            TAC.StringC _ -> Just (Class ["java", "lang", "String"] [])
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
            TAC.SetIRet atom -> [atom]
            TAC.IReturn -> []
            TAC.Return -> []
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


collectAtomTypesStatic :: [IRStmt] -> TACM (Map IRAtom Class)
collectAtomTypesStatic stmts = do
    varTypeMap <- getAtomTypes
    pure $ foldl' (collectStmt varTypeMap) Map.empty stmts
    where
        collectStmt ::
            Map IRAtom Class ->
            Map IRAtom Class ->
            IRStmt ->
            Map IRAtom Class
        collectStmt varTypeMap acc stmt = case stmt of
            TAC.IRInstr instr -> foldl' (collectAtom varTypeMap) acc (atomsInInstr instr)
            TAC.IRBlockStmt (TAC.IRBlock (_, stmts')) ->
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
            TAC.StringC _ -> Just (Class ["java", "lang", "String"] [])
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
            TAC.SetIRet atom -> [atom]
            TAC.IReturn -> []
            TAC.Return -> []
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
functionLowering (AST.Function (clazz, _) (AST.Variable funName _) args functB) = do
    -- load all args
    atoms <- mapM loadArgs args
    let _argsMap = genArgMap atoms -- TODO: use this map to rewrite args into Param 0..n


    let funSig = TEnv.FunSig {
        TEnv.funParams = map (\(a, _, _) -> a) args,
        TEnv.funReturn = clazz
    }

    retBId <- incBlockId
    let retInstr = if clazz == Void then TAC.Return else TAC.IReturn
    let retBlock = TAC.IRBlockStmt (TAC.IRBlock (retBId, [TAC.IRInstr retInstr]))

    funStmts <- TAC.withFun (funSig, retBId, _argsMap) $ blockLowing functB
    atomTypes <- collectAtomTypes funSig (funStmts ++ [retBlock])

    let decl = (PB.Public, [PB.Static]) -- default: public static
    return $ TAC.IRFunction decl funName funSig atomTypes (funStmts ++ [retBlock]) TAC.MemberClassWrapped
        
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
        staticStmts = qualifyStmts classQName staticStmts0
        funcs = map (qualifyFunction classQName) funcs0
        mainKind = detectMainKind classQName funcs
    let decl = (PB.Public, []) -- TODO: default class decl until parser carries modifiers.
    return $ TAC.IRClass decl name staticFields (TAC.StaticInit staticStmts) staticAtomTypes funcs mainKind
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
            TAC.IRFunction decl fname sig atomTypes (qualifyStmts cls body) memberType

        qualifyStmts :: [String] -> [TAC.IRStmt] -> [TAC.IRStmt]
        qualifyStmts cls = map (qualifyStmt cls)

        qualifyStmt :: [String] -> TAC.IRStmt -> TAC.IRStmt
        qualifyStmt cls stmt = case stmt of
            TAC.IRInstr instr -> TAC.IRInstr (qualifyInstr cls instr)
            TAC.IRBlockStmt (TAC.IRBlock (bid, ss)) ->
                TAC.IRBlockStmt (TAC.IRBlock (bid, qualifyStmts cls ss))

        qualifyInstr :: [String] -> TAC.IRInstr -> TAC.IRInstr
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
        isStringClass (Class qn []) = qn == ["String"] || qn == ["java", "lang", "String"]
        isStringClass _ = False


-- | Lower a class statement into IR (not implemented yet).
classLowing :: Statement -> TACM IRClass
classLowing _ = error "the class is not implement"


-- | Lower a whole program into IR:
--   extract class declarations, and wrap remaining stmts into a synthetic class.
progmLowing :: Path -> Program -> IRProgm
progmLowing path (decls, stmts) =
    let (classStmts, otherStmts) = partition AST.isClassDeclar stmts
        pkgSegs = case filter AST.isPackageDecl decls of
            (d:_) -> AST.declPath d
            [] -> []
        mainClassName = fromMaybe (toMainClassName path) (AST.getJavaName (decls, stmts))
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
blockLowing :: Block -> TACM [IRStmt]
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






