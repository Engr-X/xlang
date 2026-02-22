{-# LANGUAGE HexFloatLiterals #-}


module IR.TACLowing where

import Data.Bits ((.&.))
import Data.Map.Strict (Map)
import Text.Read (readMaybe)
import Data.Maybe (listToMaybe)
import Data.Foldable (foldrM)
import Numeric (readHex)

import Lex.Token (tokenPos)
import Parse.SyntaxTree (Program, Expression, Statement)
import Semantic.OpInfer (inferUnaryOp, inferBinaryOp)
import Semantic.TypeEnv (FullVarTable, FullFunctionTable)
import IR.TAC (IRInstr(..), IRAtom, TACM(..), IRStmt(..), newSubCVar, getVar, peekVarStack, getAtomType)
import Util.Exception (Warning(..))
import Util.Type (Path, Position)

import qualified Parse.SyntaxTree as AST
import qualified Semantic.TypeEnv as TEnv
import qualified IR.TAC as TAC
import qualified Util.Exception as UE


-- | Path placeholder for IR-stage warnings.
tacWarnPath :: Path
tacWarnPath = "<tac>"

-- | Explicit numeric ranges (IEEE 754 for floats).
int32Range :: (Integer, Integer)
int32Range = (0x80000000,  0x7fffffff)

int64Range :: (Integer, Integer)
int64Range = (0x8000000000000000, 0x7fffffffffffffff)

float32Range :: (Double, Double)
float32Range = (0x0.000002P-126, 0x1.fffffeP+127)

float64Range :: (Double, Double)
float64Range = (0x0.0000000000001P-1022, 0x1.fffffffffffffP+1023)

float128Range :: (Rational, Rational)
float128Range = (0x0.0000000000000000000000000001P-16382, 0x1.ffffffffffffffffffffffffffffP+16383)


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
                    pure (wrap n)
                else pure n


-- | Safe read for rational literals; out of range -> clamp to min/max.
safeRational :: Position -> (Rational, Rational) -> String -> TACM Rational
safeRational pos (minR, maxR) raw = do
    let warn msg = TAC.addWarn $ OverflowWarning (UE.makeError tacWarnPath [pos] msg)
    case readMaybe (stripFloatSuffix raw) :: Maybe Double of
        Nothing -> error "this string must be valid any how"
        Just n
            | isNaN n -> do
                warn $ UE.overflowCastMsg "number" "float128"
                pure 0
            | isInfinite n -> do
                warn $ UE.overflowCastMsg "number" "float128"
                pure (if n > 0 then maxR else minR)
            | otherwise ->
                let r = toRational n
                in if r > maxR
                    then warn (UE.overflowCastMsg "number" "float128") >> pure maxR
                    else if r < minR
                        then warn (UE.overflowCastMsg "number" "float128") >> pure minR
                        else pure r


-- | Safe read for floating literals; out of range -> +/-Infinity.
safeDouble :: Position -> (Double, Double) -> String -> TACM Double
safeDouble pos (minD, maxD) raw = do
    let warn msg = TAC.addWarn $ OverflowWarning (UE.makeError tacWarnPath [pos] msg)
    case readMaybe (stripFloatSuffix raw) :: Maybe Double of
        Nothing -> error "this string must be valid any how"
        Just n
            | n > maxD -> do
                warn $ UE.overflowCastMsg "number" "double"
                pure (1.0 / 0.0)
            | n < minD -> do
                warn $ UE.overflowCastMsg "number" "double"
                pure $ -(1.0 / 0.0)
            | otherwise -> pure n


-- | Lower a single expression into a TAC atom.
--   Use lookupVarUse / lookupFunUse when you need resolved ids/types.
atomLowing :: Expression -> TACM ([IRInstr], IRAtom)
atomLowing (AST.Error _ _) = error "this error is catching in semantic already"

atomLowing (AST.IntConst value tok) = do
    v <- safeInteger (tokenPos tok) int32Range value
    pure ([], TAC.Int32C (fromInteger v))
atomLowing (AST.LongConst value tok) = do
    v <- safeInteger (tokenPos tok) int64Range value
    pure ([], TAC.Int64C (fromInteger v))
atomLowing (AST.FloatConst value tok) = do
    v <- safeDouble (tokenPos tok) float32Range value
    pure ([], TAC.Float32C v)
atomLowing (AST.DoubleConst value tok) = do
    v <- safeDouble (tokenPos tok) float64Range value
    pure ([], TAC.Float64C v)
atomLowing (AST.LongDoubleConst value tok) = do
    v <- safeRational (tokenPos tok) float128Range value
    pure ([], TAC.Float128C v)
    
atomLowing (AST.CharConst value _) = pure ([], TAC.CharC value)
atomLowing (AST.BoolConst value _) = pure ([], TAC.BoolC value)
atomLowing (AST.StringConst _ _) = error "string literal is not supported in TAC"

atomLowing (AST.Variable _ tok) = do
    let pos = tokenPos tok
    vinfo <- getVar [pos]
    case vinfo of
        -- 本地变量：取 VarId + 当前版本
        TEnv.VarLocal _ realName vid -> do
            (_, ver) <- peekVarStack (realName, vid)
            return ([], TAC.Var (realName, vid, ver))

        -- 导入变量
        TEnv.VarImported clazz qname -> do
            nAtom <- newSubCVar clazz
            return ([IGetStatic nAtom qname], nAtom)

atomLowing (AST.Qualified _ tokens) = do
    vinfo <- getVar (map tokenPos tokens)
    case vinfo of
        TEnv.VarImported clazz varName -> do
            nAtom <- newSubCVar clazz
            return ([IGetStatic nAtom varName], nAtom)
        _ -> error "qualified name is not an imported variable"

atomLowing _ = error "other type is not allowed for IR atom"


castIfNeeded :: AST.Class -> IRAtom -> AST.Class -> TACM ([IRInstr], IRAtom)
castIfNeeded from atom to
    | from == to = return ([], atom)
    | otherwise = do
        castAtom <- newSubCVar to
        return ([TAC.ICast castAtom (from, to) atom], castAtom)

-- return instruction and output atom eg: a = a + b => a1 = a0 + b0 return a1
-- warning [IRInstr] is reversed!
exprLowing :: Expression -> TACM ([IRInstr], IRAtom)
exprLowing (AST.Cast (toClass, _) inner _) = do
    (instrs, oAtom) <- if AST.isAtom inner then atomLowing inner else exprLowing inner
    fromClass <- getAtomType oAtom
    nAtom <- newSubCVar toClass
    return (TAC.ICast nAtom (fromClass, toClass) oAtom : instrs, nAtom)

exprLowing (AST.Unary op inner _) = do
    (instr, oAtom) <- if AST.isAtom inner then atomLowing inner else exprLowing inner
    fromClass <- getAtomType oAtom
    let toClass = inferUnaryOp op fromClass

    if fromClass == toClass then do
        nAtom <- newSubCVar toClass
        return (IUnary nAtom op oAtom : instr, nAtom)
    else do
        (castInstrs, castAtom) <- castIfNeeded fromClass oAtom toClass
        nAtom <- newSubCVar toClass
        return (IUnary nAtom op castAtom : castInstrs ++ instr, nAtom)

exprLowing (AST.Binary AST.Assign (AST.Variable _ tok) e2 _) = do
    let pos = tokenPos tok
    vinfo <- getVar [pos]
    case vinfo of
        TEnv.VarLocal _ realName vid -> do
            let key = (realName, vid)
            oldCur <- TAC.getCurrentVar
            
            TAC.setCurrentVar (Just key)
            (instrs, rhsAtom) <- if AST.isAtom e2 then atomLowing e2 else exprLowing e2
            clazz <- getAtomType rhsAtom
            lhsAtom <- newSubCVar clazz

            TAC.setCurrentVar oldCur
            return (IAssign lhsAtom rhsAtom : instrs, lhsAtom)
        TEnv.VarImported _ qname -> do
            (instrs, rhsAtom) <- if AST.isAtom e2 then atomLowing e2 else exprLowing e2
            return (IPutStatic qname rhsAtom : instrs, rhsAtom)

-- TODO for class
exprLowing (AST.Binary AST.Assign (AST.Qualified names toks) e2 _) =
    case names of
        ("this":_) -> error "TODO: assign to this.field is not supported yet"
        _ -> do
            vinfo <- getVar (map tokenPos toks)
            case vinfo of
                TEnv.VarImported _ qname -> do
                    (instrs, rhsAtom) <- if AST.isAtom e2 then atomLowing e2 else exprLowing e2
                    return (IPutStatic qname rhsAtom : instrs, rhsAtom)
                _ -> error "assign lhs is not an imported qualified name"


exprLowing (AST.Binary AST.Assign _ _ _) = error "cannot be assign in his case. the error should be catched in context check"


exprLowing (AST.Binary op e1 e2 _) = do
    (instr1, oAtom1) <- if AST.isAtom e1 then atomLowing e1 else exprLowing e1
    (instr2, oAtom2) <- if AST.isAtom e2 then atomLowing e2 else exprLowing e2
    fromClass1 <- getAtomType oAtom1
    fromClass2 <- getAtomType oAtom2
    let toClass = inferBinaryOp op fromClass1 fromClass2

    (cast1Instrs, atom1') <- castIfNeeded fromClass1 oAtom1 toClass
    (cast2Instrs, atom2') <- castIfNeeded fromClass2 oAtom2 toClass
    nAtom <- newSubCVar toClass
    return (IBinary nAtom op atom1' atom2' : concat [cast2Instrs, instr2, cast1Instrs, instr1], nAtom)

exprLowing (AST.Call funName params) = do
    (argInstrs, argAtoms) <- lowerArgs params
    
    funInfo <- case funName of
        AST.Variable _ tok -> TAC.getFunction [tokenPos tok]
        AST.Qualified _ toks -> TAC.getFunction (map tokenPos toks)
        _ -> error "call target is not a function name"

    case funInfo of
        TEnv.FunLocal _ name sig -> do
            let retT = TEnv.funReturn sig
            dst <- newSubCVar retT
            return (ICall dst name argAtoms : argInstrs, dst)
        TEnv.FunImported _ qname sig -> do
            let retT = TEnv.funReturn sig
            dst <- newSubCVar retT
            return (ICallStatic dst qname argAtoms : argInstrs, dst)
    where
        lowerArgs :: [Expression] -> TACM ([IRInstr], [IRAtom])
        lowerArgs = foldrM step ([], [])
            where
                step :: Expression -> ([IRInstr], [IRAtom]) -> TACM ([IRInstr], [IRAtom])
                step p (instrsRest, atomsRest) = do
                    (instrsP, atomP) <- if AST.isAtom p then atomLowing p else exprLowing p
                    return (instrsRest ++ instrsP, atomP : atomsRest)

exprLowing (AST.CallT {}) = error "template is not support!"

exprLowing _ = error "other type is not support for IR ast"



stmtLowing :: Statement -> TACM [IRStmt]
stmtLowing (AST.Expr e) = do
    (instrs, _) <- exprLowing e
    return $ map IRInstr (reverse instrs)

stmtLowing (AST.BlockStmt (AST.Multiple stmts)) = 
    fmap concat (mapM stmtLowing stmts)
