{-# LANGUAGE TupleSections #-}

module Semantic.TypeCheckTest where

import Control.Monad.State.Strict (runState, execState, get, put, modify)
import Data.Maybe (mapMaybe)
import Test.Tasty
import Test.Tasty.HUnit
import Util.Type (Position, makePosition)
import Parse.ParseExpr (replLexparseExpr)
import Parse.ParseStmt (replLexparseStmt)
import Parse.ParseProgm (replLexparseProgm)
import Parse.SyntaxTree (Block(..), Class(..), Expression(..), Statement(..), SwitchCase(..), prettyClass)
import Semantic.TypeCheck
import Semantic.NameEnv (CheckState(..), ImportEnv(..), QName, Scope(..), VarId, lookupVarId)
import Semantic.TypeEnv (FunSig(..), FunTable, TypedImportEnv(..), VarTable)

import qualified Data.Map.Strict as Map
import qualified Lex.Token as Lex
import qualified Parse.SyntaxTree as AST
import qualified Semantic.ContextCheck as CC
import qualified Util.Exception as UE


pos1 :: Position
pos1 = makePosition 1 1 1

pos2 :: Position
pos2 = makePosition 1 2 1


emptyScope :: Scope
emptyScope = Scope { scopeId = 0, sVars = Map.empty, sFuncs = Map.empty }


stEmpty :: CheckState
stEmpty = CheckState {
    depth = 0,
    varCounter = 0,
    scopeCounter = 1,
    ctrlStack = [],
    scope = [emptyScope],
    classScope = []
}


mkVarEntries :: [String] -> Map.Map String (VarId, Position)
mkVarEntries names =
    Map.fromList $ zip names (map (, pos1) [0 .. ])


stWithVars :: [String] -> CheckState
stWithVars names =
    let vars = mkVarEntries names
    in stEmpty {
        varCounter = length names,
        scope = [emptyScope { sVars = vars }]
    }


stWithVarsFuncs :: [String] -> [String] -> CheckState
stWithVarsFuncs varNames funNames =
    let vars = mkVarEntries varNames
        funs = Map.fromList $ map ((, [pos1]) . (: [])) funNames
    in stEmpty {
        varCounter = length varNames,
        scope = [emptyScope { sVars = vars, sFuncs = funs }]
    }


mkVarTable :: CheckState -> [(String, Class)] -> VarTable
mkVarTable st xs =
    Map.fromList $ mapMaybe toEntry xs
    where
        toEntry (name, cls) = case lookupVarId name st of
            Just (vid, pos) -> Just (vid, (cls, pos))
            Nothing -> Nothing


mkTypeCtx :: CheckState -> Map.Map Position VarId -> VarTable -> [FunTable] -> TypeCtx
mkTypeCtx st uses vts funScopes = TypeCtx {
    tcCtx = CC.Ctx { CC.st = st, CC.errs = [], CC.varUses = uses },
    tcVarTypes = vts,
    tcVarFlags = Map.empty,
    tcFunScopes = funScopes,
    tcClassStack = [],
    tcClassTypeStack = [],
    tcCurrentReturn = Nothing,
    tcErrors = [],
    tcWarnings = [],
    tcFullVarUses = Map.empty,
    tcFullVarUsesList = [],
    tcFullFunUses = Map.empty
}


noExtraTc :: TypeCtx -> Assertion
noExtraTc _ = pure ()


firstWhy :: [UE.ErrorKind] -> Maybe String
firstWhy errors = case errors of
    (UE.Syntax (UE.BasicError { UE.why = whyMsg }) : _) -> Just whyMsg
    (UE.Parsing (UE.BasicError { UE.why = whyMsg }) : _) -> Just whyMsg
    (UE.Lexer (UE.BasicError { UE.why = whyMsg }) : _) -> Just whyMsg
    _ -> Nothing


assertTcErrs :: Maybe String -> TypeCtx -> Assertion
assertTcErrs Nothing ctx = tcErrors ctx @?= []
assertTcErrs (Just msg) ctx = case firstWhy (tcErrors ctx) of
    Just whyMsg -> whyMsg @?= msg
    Nothing -> assertFailure $ "unexpected errors: " ++ show (tcErrors ctx)


assertWarnCount :: Int -> TypeCtx -> Assertion
assertWarnCount n ctx = length (tcWarnings ctx) @?= n

warnTag :: UE.Warning -> String
warnTag w = case w of
    UE.Null -> "null"
    UE.ImplicitCast _ -> "implicit"
    UE.OverflowWarning _ -> "overflow"
    UE.UnderflowWarning _ -> "underflow"

assertWarnTags :: [String] -> TypeCtx -> Assertion
assertWarnTags tags ctx = map warnTag (tcWarnings ctx) @?= tags


assertWarnsNonEmpty :: TypeCtx -> Assertion
assertWarnsNonEmpty ctx = assertBool "expected warnings" (not (null (tcWarnings ctx)))


assertVarType :: String -> Class -> TypeCtx -> Assertion
assertVarType name expected ctx = case lookupVarId name (CC.st (tcCtx ctx)) of
    Nothing -> assertFailure $ "missing var id for: " ++ name
    Just (vid, _) -> case Map.lookup vid (tcVarTypes ctx) of
        Nothing -> assertFailure $ "missing var type for: " ++ name
        Just (cls, _) -> cls @?= expected


parseExprOrFail :: String -> IO AST.Expression
parseExprOrFail src = case replLexparseExpr src of
    Left errors -> assertFailure ("parse failed: " ++ show errors)
    Right expr -> pure expr


parseStmtOrFail :: String -> IO AST.Statement
parseStmtOrFail src = case replLexparseStmt src of
    Left errors -> assertFailure ("parse failed: " ++ show errors)
    Right stmt -> pure stmt


parseProgmOrFail :: String -> IO AST.Program
parseProgmOrFail src = case replLexparseProgm src of
    Left errors -> assertFailure ("parse failed: " ++ show errors)
    Right prog -> pure prog


importVars :: [QName] -> ImportEnv
importVars names = IEnv {
    file = "stdin",
    iVars = Map.fromList $ map (, [pos1]) names,
    iFuncs = Map.empty
}


typedVarsEnv :: [(QName, Class)] -> TypedImportEnv
typedVarsEnv vars = TIEnv {
    tFile = "stdin",
    tVars = Map.fromList $ map (\(q, c) -> (q, (c, [pos1]))) vars,
    tFuncs = Map.empty
}


mkSyntaxErr :: String -> UE.ErrorKind
mkSyntaxErr msg = UE.Syntax (UE.makeError "stdin" [pos1] msg)


warnImplicit :: UE.Warning
warnImplicit = UE.ImplicitCast (UE.makeError "stdin" [pos1] "implicit")


warnOverflow :: UE.Warning
warnOverflow = UE.OverflowWarning (UE.makeError "stdin" [pos1] "overflow")


prepExpr :: String -> CheckState -> [ImportEnv] -> IO (AST.Expression, CheckState, Map.Map Position VarId)
prepExpr src st0 envs = do
    expr <- parseExprOrFail src
    let ctx0 = CC.Ctx { CC.st = st0, CC.errs = [], CC.varUses = Map.empty }
        (_, ctx1) = runState (CC.checkExpr "stdin" [] envs expr) ctx0
    pure (expr, CC.st ctx1, CC.varUses ctx1)


runInferExprFromSrc ::
    String ->
    CheckState ->
    VarTable ->
    [FunTable] ->
    [TypedImportEnv] ->
    [ImportEnv] ->
    IO (Class, TypeCtx)
runInferExprFromSrc src st0 vts funScopes typedEnvs importEnvs = do
    (expr, st1, uses) <- prepExpr src st0 importEnvs
    let ctx0 = mkTypeCtx st1 uses vts funScopes
        (t, ctx1) = runState (inferExpr "stdin" [] typedEnvs expr) ctx0
    pure (t, ctx1)


addErrTests :: TestTree
addErrTests = testGroup "Semantic.TypeCheck.addErr" $ map mkCase [
    ("0", mkSyntaxErr "e0",  mkTypeCtx stEmpty Map.empty Map.empty [Map.empty],
        \ctx1 -> tcErrors ctx1 @?= [mkSyntaxErr "e0"]),
    ("1", mkSyntaxErr "e1", (mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]) { tcErrors = [mkSyntaxErr "e0"] },
        \ctx1 -> tcErrors ctx1 @?= [mkSyntaxErr "e1", mkSyntaxErr "e0"]),
    ("2", mkSyntaxErr "e0", (mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]) { tcWarnings = [warnImplicit] },
        assertWarnTags ["implicit"]),
    ("3", mkSyntaxErr "e0", mkTypeCtx stEmpty Map.empty vts [Map.empty],
        \ctx1 -> tcVarTypes ctx1 @?= vts)]
    where
        vts = Map.fromList [(0, (Int32T, pos1))]
        mkCase (name, err, ctx0, assertCtx) = testCase name $ do
            let ctx1 = execState (addErr err) ctx0
            assertCtx ctx1


errClassTests :: TestTree
errClassTests = testGroup "Semantic.TypeCheck.errClass" $ map (uncurry testCase) [
    ("0", do
        let ctx0 = mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]
            (t, ctx1) = runState (errClass (mkSyntaxErr "e0")) ctx0
        t @?= ErrorClass
        tcErrors ctx1 @?= [mkSyntaxErr "e0"]),
    ("1", do
        let err0 = mkSyntaxErr "e0"
            err1 = mkSyntaxErr "e1"
            ctx0 = (mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]) { tcErrors = [err0] }
            (t, ctx1) = runState (errClass err1) ctx0
        t @?= ErrorClass
        tcErrors ctx1 @?= [err1, err0]),
    ("2", do
        let ctx0 = (mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]) { tcWarnings = [warnImplicit] }
            (t, ctx1) = runState (errClass (mkSyntaxErr "e0")) ctx0
        t @?= ErrorClass
        assertWarnTags ["implicit"] ctx1),
    ("3", do
        let vts = Map.fromList [(0, (Int32T, pos1))]
            ctx0 = mkTypeCtx stEmpty Map.empty vts [Map.empty]
            (t, ctx1) = runState (errClass (mkSyntaxErr "e0")) ctx0
        t @?= ErrorClass
        tcVarTypes ctx1 @?= vts)]


addWarnTests :: TestTree
addWarnTests = testGroup "Semantic.TypeCheck.addWarn" $ map (uncurry testCase) [
    ("0", do
        let ctx0 = mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]
            ctx1 = execState (addWarn warnImplicit) ctx0
        assertWarnTags ["implicit"] ctx1
    ),
    ("1", do
        let ctx0 = (mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]) { tcWarnings = [warnImplicit] }
            ctx1 = execState (addWarn warnOverflow) ctx0
        assertWarnTags ["overflow", "implicit"] ctx1
    ),
    ("2", do
        let err0 = mkSyntaxErr "e0"
            ctx0 = (mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]) { tcErrors = [err0] }
            ctx1 = execState (addWarn warnImplicit) ctx0
        tcErrors ctx1 @?= [err0]
    ),
    ("3", do
        let vts = Map.fromList [(0, (Int32T, pos1))]
            ctx0 = mkTypeCtx stEmpty Map.empty vts [Map.empty]
            ctx1 = execState (addWarn warnImplicit) ctx0
        tcVarTypes ctx1 @?= vts)]


withScopeTests :: TestTree
withScopeTests = testGroup "Semantic.TypeCheck.withScope" $ map (uncurry testCase) [
    ("0", do
        let ctx0 = mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]
            action = do
                c <- get
                let d = depth (CC.st (tcCtx c))
                    sLen = length (scope (CC.st (tcCtx c)))
                    fLen = length (tcFunScopes c)
                pure (d, sLen, fLen)
            ((dIn, sIn, fIn), ctx1) = runState (withScope action) ctx0
        dIn @?= 1
        sIn @?= 2
        fIn @?= 2
        depth (CC.st (tcCtx ctx1)) @?= 0
        length (scope (CC.st (tcCtx ctx1))) @?= 1
        length (tcFunScopes ctx1) @?= 1),
    ("1", do
        let ctx0 = mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]
            addInnerVar = do
                c <- get
                let cctx = tcCtx c
                    cState = CC.st cctx
                case scope cState of
                    [] -> error "expected non-empty scope"
                    (scTop:rest) -> do
                        let scTop' = scTop { sVars = Map.insert "a" (99, pos1) (sVars scTop) }
                            cState' = cState { scope = scTop' : rest }
                        put $ c { tcCtx = cctx { CC.st = cState' } }
            (_, ctx1) = runState (withScope addInnerVar) ctx0
        lookupVarId "a" (CC.st (tcCtx ctx1)) @?= Nothing),
    ("2", do
        let sig0 = FunSig [] Int32T
            origFuns = Map.fromList [(["g"], [sig0])]
            ctx0 = mkTypeCtx stEmpty Map.empty Map.empty [origFuns]
            addInnerFun = modify $ \c -> case tcFunScopes c of
                [] -> c
                (fs:rest) -> c { tcFunScopes = Map.insert ["f"] [FunSig [Int32T] Int32T] fs : rest }
            (_, ctx1) = runState (withScope addInnerFun) ctx0
        tcFunScopes ctx1 @?= [origFuns]
    ),
    ("3", do
        let ctx0 = mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]
            sc0 = scopeCounter (CC.st (tcCtx ctx0))
            (_, ctx1) = runState (withScope (pure ())) ctx0
            sc1 = scopeCounter (CC.st (tcCtx ctx1))
        sc1 @?= sc0 + 1)]


inferThisTests :: TestTree
inferThisTests = testGroup "Semantic.TypeCheck.inferThis" $ map (uncurry testCase) [
    ("0", do
        let ctx0 = (mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]) { tcClassTypeStack = [Class ["Foo"] []] }
            (t, ctx1) = runState (inferThis "stdin" pos1) ctx0
        t @?= Class ["Foo"] []
        assertTcErrs Nothing ctx1),
    ("1", do
        let ctx0 = (mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]) { tcClassTypeStack = [Class ["Top"] [], Class ["Other"] []] }
            (t, ctx1) = runState (inferThis "stdin" pos1) ctx0
        t @?= Class ["Top"] []
        assertTcErrs Nothing ctx1),
    ("2", do
        let ctx0 = mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]
            (t, ctx1) = runState (inferThis "stdin" pos1) ctx0
        t @?= ErrorClass
        assertTcErrs (Just (UE.undefinedIdentity "this")) ctx1),
    ("3", do
        let err0 = mkSyntaxErr "old"
            err1 = mkSyntaxErr (UE.undefinedIdentity "this")
            ctx0 = (mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]) { tcErrors = [err0] }
            (t, ctx1) = runState (inferThis "stdin" pos1) ctx0
        t @?= ErrorClass
        tcErrors ctx1 @?= [err1, err0])]


inferThisFieldTests :: TestTree
inferThisFieldTests = testGroup "Semantic.TypeCheck.inferThisField" $ map (uncurry testCase) [
    ("0", do
        let ctx0 = mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]
            (t, ctx1) = runState (inferThisField "stdin" [pos1] "field") ctx0
        t @?= ErrorClass
        assertTcErrs (Just (UE.undefinedIdentity "this")) ctx1),
    ("1", do
        let classDef = ClassDef "Foo" (Map.fromList [(0, (Int32T, pos1))]) Map.empty
            ctx0 = (mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]) { tcClassStack = [classDef] }
            (t, ctx1) = runState (inferThisField "stdin" [pos1] "field") ctx0
        t @?= ErrorClass
        assertTcErrs (Just (UE.undefinedIdentity "field")) ctx1),
    ("2", do
        let classDef = ClassDef "Foo" (Map.fromList [(0, (Float64T, pos1))]) Map.empty
            classScope0 = emptyScope { sVars = Map.fromList [("field", (0, pos1))] }
            st0 = stEmpty { classScope = [classScope0] }
            ctx0 = (mkTypeCtx st0 Map.empty Map.empty [Map.empty]) { tcClassStack = [classDef] }
            (t, ctx1) = runState (inferThisField "stdin" [pos1] "field") ctx0
        t @?= Float64T
        assertTcErrs Nothing ctx1),
    ("3", do
        let classDef = ClassDef "Foo" Map.empty Map.empty
            classScope0 = emptyScope { sVars = Map.fromList [("field", (0, pos1))] }
            st0 = stEmpty { classScope = [classScope0] }
            ctx0 = (mkTypeCtx st0 Map.empty Map.empty [Map.empty]) { tcClassStack = [classDef] }
            (t, ctx1) = runState (inferThisField "stdin" [pos1] "field") ctx0
        t @?= ErrorClass
        assertTcErrs (Just (UE.undefinedIdentity "field")) ctx1)]


inferLiteralTests :: TestTree
inferLiteralTests = testGroup "Semantic.TypeCheck.inferLiteral" $ map (uncurry testCase) [
    ("0", do
        let expr = IntConst "1" (Lex.NumberConst "1" pos1)
            ctx0 = mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]
            (t, ctx1) = runState (inferLiteral "stdin" expr) ctx0
        t @?= Int32T
        assertTcErrs Nothing ctx1),
    ("1", do
        let expr = LongConst "1" (Lex.NumberConst "1" pos1)
            ctx0 = mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]
            (t, ctx1) = runState (inferLiteral "stdin" expr) ctx0
        t @?= Int64T
        assertTcErrs Nothing ctx1),
    ("2", do
        let expr = FloatConst "1.0" (Lex.NumberConst "1.0" pos1)
            ctx0 = mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]
            (t, ctx1) = runState (inferLiteral "stdin" expr) ctx0
        t @?= Float32T
        assertTcErrs Nothing ctx1),
    ("3", do
        let expr = BoolConst True (Lex.Ident "true" pos1)
            ctx0 = mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]
            (t, ctx1) = runState (inferLiteral "stdin" expr) ctx0
        t @?= Bool
        assertTcErrs Nothing ctx1)]


getVarIdTests :: TestTree
getVarIdTests = testGroup "Semantic.TypeCheck.getVarId" $ map (uncurry testCase) [
    ("0", do
        let ctx0 = CC.Ctx { CC.st = stEmpty, CC.errs = [], CC.varUses = Map.fromList [(pos1, 0)] }
        getVarId pos1 ctx0 @?= Just 0),
    ("1", do
        let ctx0 = CC.Ctx { CC.st = stEmpty, CC.errs = [], CC.varUses = Map.empty }
        getVarId pos1 ctx0 @?= Nothing),
    ("2", do
        let ctx0 = CC.Ctx { CC.st = stEmpty, CC.errs = [], CC.varUses = Map.fromList [(pos1, 0), (pos2, 1)] }
        getVarId pos2 ctx0 @?= Just 1),
    ("3", do
        let st0 = stWithVars ["x"]
            ctx0 = CC.Ctx { CC.st = st0, CC.errs = [], CC.varUses = Map.fromList [(pos1, 2)] }
        getVarId pos1 ctx0 @?= Just 2)]


getImportedVarTypeTests :: TestTree
getImportedVarTypeTests = testGroup "Semantic.TypeCheck.getImportedVarType" $ map (uncurry testCase) [
    ("0", do
        getImportedVarType ["a"] [] @?= Nothing),
    ("1", do
        let envs = [typedVarsEnv [(["a"], Int32T)]]
        getImportedVarType ["a"] envs @?= Just Int32T),
    ("2", do
        let envs = [typedVarsEnv [], typedVarsEnv [(["a"], Int16T)]]
        getImportedVarType ["a"] envs @?= Just Int16T),
    ("3", do
        let envs = [typedVarsEnv [(["a"], Int32T)], typedVarsEnv [(["a"], Int16T)]]
        getImportedVarType ["a"] envs @?= Just Int32T)]


lookupFunTests :: TestTree
lookupFunTests = testGroup "Semantic.TypeCheck.lookupFun" $ map (uncurry testCase) [
    ("0", do
        lookupFun ["f"] [] @?= []),
    ("1", do
        let sig0 = FunSig [] Int32T
            scopes = [Map.fromList [(["f"], [sig0])]]
        lookupFun ["f"] scopes @?= [sig0]),
    ("2", do
        let sig0 = FunSig [] Int32T
            sig1 = FunSig [Int32T] Int32T
            scopes = [Map.fromList [(["f"], [sig1])], Map.fromList [(["f"], [sig0])]]
        lookupFun ["f"] scopes @?= [sig1]),
    ("3", do
        let sig0 = FunSig [] Int32T
            scopes = [Map.empty, Map.fromList [(["f"], [sig0])]]
        lookupFun ["f"] scopes @?= [sig0])]


inferOptBlockTests :: TestTree
inferOptBlockTests = testGroup "Semantic.TypeCheck.inferOptBlock" $ map (uncurry testCase) [
    ("0", do
        let ctx0 = mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]
            (_, ctx1) = runState (inferOptBlock "stdin" [] [] Nothing) ctx0
        assertTcErrs Nothing ctx1
        Map.size (tcVarTypes ctx1) @?= 0),
    ("1", do
        block <- mkBlock "a = 1;"
        let ctx0 = mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]
            (_, ctx1) = runState (inferOptBlock "stdin" [] [] (Just block)) ctx0
        assertTcErrs Nothing ctx1
        lookupVarId "a" (CC.st (tcCtx ctx1)) @?= Nothing
        case Map.elems (tcVarTypes ctx1) of
            [(cls, _)] -> cls @?= Int32T
            _ -> assertFailure "expected one var type"),
    ("2", do
        block <- mkBlock "x = 1;"
        let st0 = stWithVars ["x"]
            ctx0 = mkTypeCtx st0 Map.empty Map.empty [Map.empty]
            (_, ctx1) = runState (inferOptBlock "stdin" [] [] (Just block)) ctx0
        lookupVarId "x" (CC.st (tcCtx ctx1)) @?= Just (0, pos1)),
    ("3", do
        block <- mkBlock "a.b = 1;"
        let ctx0 = mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]
            (_, ctx1) = runState (inferOptBlock "stdin" [] [] (Just block)) ctx0
        assertTcErrs (Just UE.assignErrorMsg) ctx1)]
    where
        mkBlock :: String -> IO Block
        mkBlock src = do
            stmt <- parseStmtOrFail src
            pure (Multiple [stmt])


checkTypeCompatTests :: TestTree
checkTypeCompatTests = testGroup "Semantic.TypeCheck.checkTypeCompat" $ map (\(name, expected, actual, errMsg, warnCount) ->
    testCase name $ do
        let ctx0 = mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]
            ctx1 = execState (checkTypeCompat "stdin" [pos1] expected actual) ctx0
        assertTcErrs errMsg ctx1
        case warnCount of
            Nothing -> pure ()
            Just n -> assertWarnCount n ctx1) [
        ("0", Int32T, Int32T, Nothing, Just 0),
        ("1", Int64T, Int32T, Nothing, Just 1),
        ("2", Int32T, Void, Just (UE.typeMismatchMsg (prettyClass Int32T) (prettyClass Void)), Nothing),
        ("3", Class ["Foo"] [], Int32T, Just (UE.staticCastError (prettyClass Int32T) (prettyClass (Class ["Foo"] []))), Nothing)]


inferExprTests :: TestTree
inferExprTests = testGroup "Semantic.TypeCheck.inferExpr" $ map mkCase [
    ("0", "1", stEmpty, Map.empty, [Map.empty], [], [], Int32T, Nothing, Just 0, noExtraTc),
    ("1", "x", stWithVars ["x"], mkVarTable (stWithVars ["x"]) [("x", Int16T)], [Map.empty], [], [], Int16T, Nothing, Just 0, noExtraTc),
    ("2", "x + 1", stWithVars ["x"], mkVarTable (stWithVars ["x"]) [("x", Int16T)], [Map.empty], [], [], Int32T, Nothing, Nothing, noExtraTc),
    ("3", "y = 1", stEmpty, Map.empty, [Map.empty], [], [], Int32T, Nothing, Just 0, assertVarType "y" Int32T),
    ("4", "x = true", stWithVars ["x"], mkVarTable (stWithVars ["x"]) [("x", Int32T)], [Map.empty], [], [], Bool, Nothing, Nothing, assertWarnsNonEmpty),
    ("5", "1 == 2", stEmpty, Map.empty, [Map.empty], [], [], Bool, Nothing, Nothing, noExtraTc),
    ("6", "f(1)", stWithVarsFuncs [] ["f"], Map.empty, [Map.fromList [(["f"], [FunSig [Int32T] Int64T])]], [], [], Int64T, Nothing, Just 0, noExtraTc),
    ("7", "a.b", stEmpty, Map.empty, [Map.empty], [typedVarsEnv [(["a", "b"], Int16T)]], [importVars [["a", "b"]]], Int16T, Nothing, Just 0, noExtraTc)]
    where
        mkCase (name, src, st0, vts, fScopes, typedEnvs, importEnvs, expectedT, errMsg, warnCount, extra) =
            testCase name $ do
                (t, ctx1) <- runInferExprFromSrc src st0 vts fScopes typedEnvs importEnvs
                t @?= expectedT
                assertTcErrs errMsg ctx1
                case warnCount of
                    Nothing -> pure ()
                    Just n -> assertWarnCount n ctx1
                extra ctx1


inferStmtTests :: TestTree
inferStmtTests = testGroup "Semantic.TypeCheck.inferStmt" $ map mkCase [
    ("0", "return;", Just Void, Nothing),
    ("1", "return;", Just Int32T, Just (UE.typeMismatchMsg (prettyClass Int32T) (prettyClass Void))),
    ("2", "return 1;", Just Int32T, Nothing),
    ("3", "return 1;", Just (Class ["Foo"] []), Just (UE.staticCastError (prettyClass Int32T) (prettyClass (Class ["Foo"] []))))]
    where
        mkCase (name, src, retT, errMsg) = testCase name $ do
            stmt <- parseStmtOrFail src
            let ctx0 = (mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]) { tcCurrentReturn = retT }
                (_, ctx1) = runState (inferStmt "stdin" [] [] stmt) ctx0
            assertTcErrs errMsg ctx1


conditionBoolTests :: TestTree
conditionBoolTests = testGroup "Semantic.TypeCheck.conditionBool" $ map (uncurry testCase) [
    ("0", do
        let stmt = If intExpr Nothing Nothing (tokIf, Nothing)
        assertCondErr stmt),
    ("1", do
        let stmt = While intExpr Nothing Nothing (tokWhile, Nothing)
        assertCondErr stmt),
    ("2", do
        let stmt = For (Nothing, Just intExpr, Nothing) Nothing tokFor
        assertCondErr stmt),
    ("3", do
        let stmt = DoWhile Nothing intExpr Nothing (tokDo, tokWhile, Nothing)
        assertCondErr stmt)]
    where
        intExpr = IntConst "1" (Lex.NumberConst "1" pos1)
        expected = UE.conditionBoolMsg (prettyClass Int32T)
        tokIf = Lex.Ident "if" pos1
        tokWhile = Lex.Ident "while" pos1
        tokFor = Lex.Ident "for" pos1
        tokDo = Lex.Ident "do" pos1
        assertCondErr stmt = do
            let ctx0 = mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]
                (_, ctx1) = runState (inferStmt "stdin" [] [] stmt) ctx0
            assertTcErrs (Just expected) ctx1


inferSwitchCaseTests :: TestTree
inferSwitchCaseTests = testGroup "Semantic.TypeCheck.inferSwitchCase" $ map mkCase [
    ("0", pure (scCaseLiteral Nothing), Nothing, noExtraTc),
    ("1", scCaseLiteral . Just <$> mkBlock "a = 1;", Nothing, noExtraTc),
    ("2", scDefault <$> mkBlock "b = 2;", Nothing, noExtraTc),
    ("3", scCaseLiteral . Just <$> mkBlock "a.b = 1;", Just UE.assignErrorMsg, noExtraTc)]
    where
        tokCase = Lex.Ident "case" pos1
        tokDefault = Lex.Ident "default" pos1

        mkCase (name, mkSc, errMsg, extra) = testCase name $ do
            sc <- mkSc
            let ctx0 = mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]
                (_, ctx1) = runState (inferSwitchCase "stdin" [] [] sc) ctx0
            assertTcErrs errMsg ctx1
            extra ctx1

        scCaseLiteral mb = Case (IntConst "1" (Lex.NumberConst "1" pos1)) mb tokCase
        scDefault blk = Default blk tokDefault

        mkBlock :: String -> IO Block
        mkBlock src = do
            stmt <- parseStmtOrFail src
            pure (Multiple [stmt])


inferBlockTests :: TestTree
inferBlockTests = testGroup "Semantic.TypeCheck.inferBlock" $ map mkCase [
    ("0", pure (Multiple []), Nothing, noExtraTc),
    ("1", mkBlock "a = 1;", Nothing, assertVarType "a" Int32T),
    ("2", mkBlockMany ["a = 1;", "b = 2;"], Nothing, \ctx -> do assertVarType "a" Int32T ctx; assertVarType "b" Int32T ctx),
    ("3", mkBlock "a.b = 1;", Just UE.assignErrorMsg, noExtraTc)]
    where
        mkCase (name, mkBlk, errMsg, extra) = testCase name $ do
            block <- mkBlk
            let ctx0 = mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]
                (_, ctx1) = runState (inferBlock "stdin" [] [] block) ctx0
            assertTcErrs errMsg ctx1
            extra ctx1

        mkBlock :: String -> IO Block
        mkBlock src = do
            stmt <- parseStmtOrFail src
            pure (Multiple [stmt])

        mkBlockMany :: [String] -> IO Block
        mkBlockMany srcs = do
            stmts <- mapM parseStmtOrFail srcs
            pure (Multiple stmts)


inferStmtsTests :: TestTree
inferStmtsTests = testGroup "Semantic.TypeCheck.inferStmts" $ map mkCase [
    ("0", [], Nothing),
    ("1", [stmtCallF0, stmtFunF0], Nothing),
    ("2", [stmtFunF0, stmtFunF0], Just (UE.duplicateMethodMsg "int f()")),
    ("3", [stmtFunF1, stmtFunF2, stmtCallF1], Nothing)]
    where
        tokF = Lex.Ident "f" pos1
        tokX = Lex.Ident "x" pos1
        tokNum1 = Lex.NumberConst "1" pos1

        stmtCallF0 = Expr (Call (Variable "f" tokF) [])
        stmtCallF1 = Expr (Call (Variable "f" tokF) [IntConst "1" tokNum1])
        stmtFunF0 = Function (Int32T, []) (Variable "f" tokF) [] (Multiple [])
        stmtFunF1 = Function (Int32T, []) (Variable "f" tokF) [(Int32T, "x", [tokX])] (Multiple [])
        stmtFunF2 = Function (Float32T, []) (Variable "f" tokF) [(Float32T, "x", [tokX])] (Multiple [])

        mkCase (name, stmts, errMsg) = testCase name $ do
            let ctx0 = mkTypeCtx stEmpty Map.empty Map.empty [Map.empty]
                (_, ctx1) = runState (inferStmts "stdin" [] [] stmts) ctx0
            assertTcErrs errMsg ctx1


inferProgmTests :: TestTree
inferProgmTests = testGroup "Semantic.TypeCheck.inferProgm" $ map mkCase [
    ("0", unlines [
        "a = 1;",
        "b = a + 2;",
        "c = b + 3;"
    ], Nothing, \ctx -> do assertVarType "a" Int32T ctx; assertVarType "b" Int32T ctx),

    ("1", unlines [
        "a = true;",
        "b = a;",
        "c = b;"
    ], Nothing, \ctx -> do assertVarType "a" Bool ctx; assertVarType "b" Bool ctx),

    ("2", unlines [
        "a = 1;",
        "a = true;",
        "b = a;"
    ], Nothing, assertWarnsNonEmpty),

    ("3", unlines [
        "int f() {",
        "    if true:",
        "        return;",
        "    else:",
        "        a = 1;",
        "}",
        "x = 1;",
        "y = x + 1;"
    ], Just (UE.typeMismatchMsg (prettyClass Int32T) (prettyClass Void)), noExtraTc),

    ("4", unlines [
        "void f() {",
        "    while true:",
        "        a = 1;",
        "    else:",
        "        a = 2;",
        "}",
        "x = 1;",
        "y = x + 1;"
    ], Nothing, noExtraTc),

    ("5", unlines [
        "int f() {",
        "    if true:",
        "        return 1;",
        "    else:",
        "        return 2;",
        "}",
        "x = 1;",
        "y = x + 1;"
    ], Nothing, noExtraTc),

    ("6", unlines [
        "int f() {",
        "    if true:",
        "        return true;",
        "    else:",
        "        return 1;",
        "}",
        "x = 1;",
        "y = x + 1;"
    ], Nothing, assertWarnsNonEmpty),

    ("7", unlines [
        "int f(int x) {",
        "    if true:",
        "        return 1;",
        "    else:",
        "        return 2;",
        "}",
        "int g() { return 1; }",
        "a = f(true);"
    ], Just (UE.typeMismatchMsg "(int)" "(bool)"), noExtraTc),

    ("8", unlines [
        "float f(float x) {",
        "    while true:",
        "        x = x + 1;",
        "    else:",
        "        x = x + 2;",
        "    return 1;",
        "}",
        "a = f(1);",
        "b = 1;"
    ], Nothing, assertWarnsNonEmpty),

    ("9", unlines [
        "int h() {",
        "    if true:",
        "        return 1;",
        "    else:",
        "        return 2;",
        "}",
        "c = 1;",
        "a = b + 1;",
        "d = c + 1;"
    ], Just (UE.undefinedIdentity "b"), noExtraTc),

    ("10", unlines [
        "int g() {",
        "    while true:",
        "        a = 1;",
        "    else:",
        "        a = 2;",
        "    return 1;",
        "}",
        "a = 1;",
        "b = f(1);",
        "c = a + 1;"
    ], Just (UE.undefinedIdentity "f"), noExtraTc),

    ("11", unlines [
        "a = 1 as double;",
        "b = a;",
        "c = b;"
    ], Nothing, assertVarType "a" Float64T),

    ("12", unlines [
        "a = 1;",
        "a.b = 1;",
        "c = a;"
    ], Just UE.assignErrorMsg, noExtraTc),

    ("13", unlines [
        "int f(int x) { return 1; }",
        "float f(float x) { return 1; }",
        "a = f(1);"
    ], Nothing, noExtraTc),

    ("14", unlines [
        "int f(int x) { return 1; }",
        "int g() { return 1; }",
        "a = f(1, 2);"
    ], Just (UE.typeMismatchMsg "(int)" "(int, int)"), noExtraTc),

    ("15", unlines [
        "int f() { return 1; }",
        "int g() {",
        "    if true:",
        "        return f();",
        "    else:",
        "        return 1;",
        "}",
        "a = g();"
    ], Nothing, noExtraTc)]
    where
        mkCase (name, src, expected, extra) = testCase name $ do
            prog <- parseProgmOrFail src
            case inferProgm "stdin" prog [] [] of
                Left errors -> case expected of
                    Nothing -> assertFailure $ "unexpected errors: " ++ show errors
                    Just msg -> case firstWhy errors of
                        Just whyMsg -> whyMsg @?= msg
                        Nothing -> assertFailure $ "unexpected errors: " ++ show errors
                Right ctx -> case expected of
                    Just _ -> assertFailure "expected errors but got success"
                    Nothing -> extra ctx


tests :: TestTree
tests = testGroup "Semantic.TypeCheck" [
    addErrTests, errClassTests, addWarnTests, withScopeTests,
    inferThisTests, inferThisFieldTests, inferLiteralTests,
    getVarIdTests, getImportedVarTypeTests,
    lookupFunTests, inferOptBlockTests, checkTypeCompatTests,
    inferExprTests, inferStmtTests, conditionBoolTests,
    inferSwitchCaseTests, inferBlockTests, inferStmtsTests, inferProgmTests]
