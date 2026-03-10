{-# LANGUAGE PatternSynonyms #-}

module Semantic.CheckProgramTest where

import Data.Graph (SCC(..))
import Data.List (isInfixOf)
import Data.Map.Strict (Map)
import Parse.SyntaxTree (Block(..), Class(..), Command(..), Declaration(..), Expression(..), Operator(..), Program, Statement(..), pattern Function, pattern FunctionT)
import Semantic.CheckProgram
import Semantic.NameEnv (ImportEnv(..), QName, toHiddenQName)
import Semantic.TypeEnv (FunSig(..), TypedImportEnv(..), emptyTypedImportEnv)
import Test.Tasty
import Test.Tasty.HUnit
import Util.Type (Path, Position, makePosition)

import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import qualified Lex.Token as Lex
import qualified Semantic.TypeCheck as TC
import qualified Util.Exception as UE


pos :: Int -> Position
pos n = makePosition n 1 1


identTok :: String -> Int -> Lex.Token
identTok s n = Lex.Ident s (pos n)


numTok :: String -> Int -> Lex.Token
numTok s n = Lex.NumberConst s (pos n)


symTok :: Lex.Symbol -> Int -> Lex.Token
symTok s n = Lex.Symbol s (pos n)


intExpr :: Int -> Int -> Expression
intExpr n p = IntConst (show n) (numTok (show n) p)


varExpr :: String -> Int -> Expression
varExpr s p = Variable s (identTok s p)


assignStmt :: String -> Expression -> Int -> Statement
assignStmt name rhs p = Expr (Binary Assign (varExpr name p) rhs (symTok Lex.Assign p))


addExpr :: Expression -> Expression -> Int -> Expression
addExpr a b p = Binary Add a b (symTok Lex.Plus p)


mkFunOk :: String -> Int -> Statement
mkFunOk name p =
    Function (Int32T, [identTok "int" p]) (varExpr name (p + 1)) [] $
        Multiple [Command (Return (Just (intExpr 1 (p + 2)))) (identTok "return" (p + 3))]


mkFunNoReturn :: String -> Int -> Statement
mkFunNoReturn name p =
    Function (Int32T, [identTok "int" p]) (varExpr name (p + 1)) [] (Multiple [])


mkFunQualified :: [String] -> Int -> Statement
mkFunQualified qn p =
    Function (Int32T, [identTok "int" p]) (Qualified qn (zipWith identTok qn [p + 1 ..])) [] (Multiple [])


mkFunTOk :: String -> Int -> Statement
mkFunTOk name p =
    FunctionT (Int32T, [identTok "int" p]) (varExpr name (p + 1)) [(Int32T, [identTok "T" (p + 2)])] [] $
        Multiple [Command (Return (Just (intExpr 1 (p + 3)))) (identTok "return" (p + 4))]


mkPackageDecl :: QName -> Int -> Declaration
mkPackageDecl qn p = Package qn [identTok "package" p]


mkImportDecl :: QName -> Int -> Declaration
mkImportDecl qn p = Import qn [identTok "import" p]


mkProgram :: QName -> [QName] -> [Statement] -> Program
mkProgram pkg imports stmts =
    let pkgDecls = ([mkPackageDecl pkg 1 | not (null pkg)])
        impDecls = zipWith mkImportDecl imports [2 ..]
    in (pkgDecls ++ impDecls, stmts)


mustRight :: Show e => Either e a -> a
mustRight = either (error . show) id


assertRightWith :: Show e => Either e a -> (a -> Assertion) -> Assertion
assertRightWith (Left e) _ = assertFailure ("expected Right, got Left: " ++ show e)
assertRightWith (Right x) f = f x


assertLeftWith :: Either [UE.ErrorKind] a -> ([UE.ErrorKind] -> Assertion) -> Assertion
assertLeftWith (Right _) _ = assertFailure "expected Left, got Right"
assertLeftWith (Left es) f = f es


errWhy :: UE.ErrorKind -> String
errWhy (UE.Syntax be) = UE.why be
errWhy (UE.Parsing be) = UE.why be
errWhy (UE.Lexer be) = UE.why be
errWhy _ = ""


errPath :: UE.ErrorKind -> String
errPath (UE.Syntax be) = UE.filePath be
errPath (UE.Parsing be) = UE.filePath be
errPath (UE.Lexer be) = UE.filePath be
errPath (UE.Unkown p) = p
errPath (UE.Reading p) = p
errPath UE.None = ""


mkModuleInfo :: Path -> QName -> [QName] -> [Statement] -> ModuleInfo
mkModuleInfo path pkg imports stmts =
    mustRight (toModuleInfo (path, mkProgram pkg imports stmts))


mkGroup :: String -> [(String, Assertion)] -> TestTree
mkGroup groupName cases =
    testGroup groupName (map (uncurry testCase) cases)


toModuleInfoTests :: TestTree
toModuleInfoTests = mkGroup "Semantic.CheckProgram.toModuleInfo" [
    ("0", do
        assertRightWith (toModuleInfo ("a.x", mkProgram [] [] [])) $ \mi -> do
            miPath mi @?= "a.x"
            miPkg mi @?= []
            miImports mi @?= []),

    ("1", do
        assertRightWith (toModuleInfo ("a.x", mkProgram ["com", "wd"] [] [])) $ \mi ->
            miPkg mi @?= ["com", "wd"]),

    ("2", do
        assertRightWith (toModuleInfo ("a.x", mkProgram ["p"] [["a"], ["b", "c"]] [])) $ \mi -> do
            map fst (miImports mi) @?= [["a"], ["b", "c"]]
            length (snd (head (miImports mi))) @?= 1),

    ("3", do
        assertLeftWith (toModuleInfo ("a.x", ([mkPackageDecl ["a"] 1, mkPackageDecl ["b"] 2], []))) $ \errs ->
            assertBool "should report multiple package declarations"
                (any ((== UE.multiplePackageMsg) . errWhy) errs))]


groupByPackageTests :: TestTree
groupByPackageTests = mkGroup "Semantic.CheckProgram.groupByPackage" [
    ("0", do
        groupByPackage [] @?= Map.empty),

    ("1", do
        let m1 = mkModuleInfo "a.x" ["p"] [] []
            mp = groupByPackage [m1]
        Map.lookup ["p"] mp @?= Just [m1]),

    ("2", do
        let m1 = mkModuleInfo "a.x" ["p"] [] []
            m2 = mkModuleInfo "b.x" ["p"] [] []
            mp = groupByPackage [m1, m2]
        maybe (assertFailure "missing package key") (\xs -> length xs @?= 2) (Map.lookup ["p"] mp)),

    ("3", do
        let m1 = mkModuleInfo "a.x" ["p"] [] []
            m2 = mkModuleInfo "b.x" ["q"] [] []
            mp = groupByPackage [m1, m2]
        Map.size mp @?= 2)
    ]


resolveImportOwnerTests :: TestTree
resolveImportOwnerTests = mkGroup "Semantic.CheckProgram.resolveImportOwner" [
    ("0", do
        resolveImportOwner [["a"], ["b"]] ["x"] @?= Nothing),

    ("1", do
        resolveImportOwner [["a"], ["b"]] ["a"] @?= Just ["a"]),

    ("2", do
        resolveImportOwner [["a"], ["a", "b"], ["a", "b", "c"]] ["a", "b", "c", "d"] @?= Just ["a", "b", "c"]),

    ("3", do
        resolveImportOwner [[], ["x"]] ["y"] @?= Just [])]


resolveAllImportsTests :: TestTree
resolveAllImportsTests = mkGroup "Semantic.CheckProgram.resolveAllImports" [
    ("0", do
        let m1 = mkModuleInfo "a.x" ["a"] [] []
            pkgMap = Map.fromList [(["a"], [m1])]
        resolveAllImports pkgMap [m1] @?= Right Map.empty),

    ("1", do
        let mA = mkModuleInfo "a.x" ["a"] [["b"]] []
            mB = mkModuleInfo "b.x" ["b"] [] []
            pkgMap = Map.fromList [(["a"], [mA]), (["b"], [mB])]
        assertRightWith (resolveAllImports pkgMap [mA, mB]) $ \owners ->
            Map.lookup ("a.x", ["b"]) owners @?= Just ["b"]),

    ("2", do
        let mA = mkModuleInfo "a.x" ["a"] [["b", "c", "d"]] []
            mB = mkModuleInfo "b.x" ["b"] [] []
            mBC = mkModuleInfo "bc.x" ["b", "c"] [] []
            pkgMap = Map.fromList [(["a"], [mA]), (["b"], [mB]), (["b", "c"], [mBC])]
        assertRightWith (resolveAllImports pkgMap [mA, mB, mBC]) $ \owners ->
            Map.lookup ("a.x", ["b", "c", "d"]) owners @?= Just ["b", "c"]),

    ("3", do
        let mA = mkModuleInfo "a.x" ["a"] [["missing", "pkg"]] []
            pkgMap = Map.fromList [(["a"], [mA])]
        assertLeftWith (resolveAllImports pkgMap [mA]) $ \errs ->
            assertBool "must mention undefined import"
                (any (isInfixOf "'missing.pkg' is not defined in this context." . errWhy) errs))
    ]


buildDepMapTests :: TestTree
buildDepMapTests = mkGroup "Semantic.CheckProgram.buildDepMap" [
    ("0", do
        let mA = mkModuleInfo "a.x" ["a"] [] []
            pkgMap = Map.fromList [(["a"], [mA])]
            deps = buildDepMap pkgMap Map.empty
        Map.lookup ["a"] deps @?= Just []),

    ("1", do
        let mA = mkModuleInfo "a.x" ["a"] [["a"]] []
            pkgMap = Map.fromList [(["a"], [mA])]
            owners = Map.fromList [(("a.x", ["a"]), ["a"])]
            deps = buildDepMap pkgMap owners
        Map.lookup ["a"] deps @?= Just []),

    ("2", do
        let mA = mkModuleInfo "a.x" ["a"] [["b"]] []
            mB = mkModuleInfo "b.x" ["b"] [] []
            pkgMap = Map.fromList [(["a"], [mA]), (["b"], [mB])]
            owners = Map.fromList [(("a.x", ["b"]), ["b"])]
            deps = buildDepMap pkgMap owners
        Map.lookup ["a"] deps @?= Just [["b"]]),

    ("3", do
        let mA = mkModuleInfo "a.x" ["a"] [["b"], ["b"]] []
            mB = mkModuleInfo "b.x" ["b"] [] []
            pkgMap = Map.fromList [(["a"], [mA]), (["b"], [mB])]
            owners = Map.fromList [(("a.x", ["b"]), ["b"])]
            deps = buildDepMap pkgMap owners
        case Map.lookup ["a"] deps of
            Nothing -> assertFailure "missing package deps"
            Just xs -> HashSet.fromList xs @?= HashSet.fromList [["b"]])
    ]


cycleErrorsTests :: TestTree
cycleErrorsTests = mkGroup "Semantic.CheckProgram.cycleErrors" [
    ("0", do
        let mA = mkModuleInfo "a.x" ["a"] [] []
            mB = mkModuleInfo "b.x" ["b"] [] []
            pkgMap = Map.fromList [(["a"], [mA]), (["b"], [mB])]
            depMap = Map.fromList [(["a"], [["b"]]), (["b"], [])]
        cycleErrors pkgMap depMap @?= []),

    ("1", do
        let mA = mkModuleInfo "a.x" ["a"] [] []
            pkgMap = Map.fromList [(["a"], [mA])]
            depMap = Map.fromList [(["a"], [["a"]])]
            errs = cycleErrors pkgMap depMap
        length errs @?= 1
        assertBool "must mention circular import" ("circular import detected" `isInfixOf` errWhy (head errs))),

    ("2", do
        let mA = mkModuleInfo "a.x" ["a"] [] []
            mB = mkModuleInfo "b.x" ["b"] [] []
            pkgMap = Map.fromList [(["a"], [mA]), (["b"], [mB])]
            depMap = Map.fromList [(["a"], [["b"]]), (["b"], [["a"]])]
            errs = cycleErrors pkgMap depMap
        length errs @?= 1
        assertBool "must mention two-node loop"
            ("a -> b -> a" `isInfixOf` errWhy (head errs) || "b -> a -> b" `isInfixOf` errWhy (head errs))),

    ("3", do
        let depMap = Map.fromList [(["ghost"], [["ghost"]])]
            errs = cycleErrors Map.empty depMap
        length errs @?= 1
        errPath (head errs) @?= "<unknown>")
    ]


sccToErrorTests :: TestTree
sccToErrorTests = mkGroup "Semantic.CheckProgram.sccToError" [
    ("0", do
        let mA = mkModuleInfo "a.x" ["a"] [] []
            pkgMap = Map.fromList [(["a"], [mA])]
            depMap = Map.fromList [(["a"], [])]
        sccToError pkgMap depMap (AcyclicSCC ["a"]) @?= []),

    ("1", do
        let mA = mkModuleInfo "a.x" ["a"] [] []
            pkgMap = Map.fromList [(["a"], [mA])]
            depMap = Map.fromList [(["a"], [["a"]])]
            errs = sccToError pkgMap depMap (AcyclicSCC ["a"])
        length errs @?= 1
        errPath (head errs) @?= "a.x"),

    ("2", do
        let mA = mkModuleInfo "a.x" ["a"] [] []
            mB = mkModuleInfo "b.x" ["b"] [] []
            pkgMap = Map.fromList [(["a"], [mA]), (["b"], [mB])]
            depMap = Map.fromList [(["a"], [["b"]]), (["b"], [["a"]])]
            errs = sccToError pkgMap depMap (CyclicSCC [["a"], ["b"]])
        length errs @?= 1
        assertBool "should mention loop" ("circular import detected" `isInfixOf` errWhy (head errs))),

    ("3", do
        let errs = sccToError Map.empty Map.empty (CyclicSCC [["x"]])
        length errs @?= 1
        errPath (head errs) @?= "<unknown>")
    ]


topoOrderTests :: TestTree
topoOrderTests = mkGroup "Semantic.CheckProgram.topoOrder" [
    ("0", do
        topoOrder Map.empty [] @?= Right []),

    ("1", do
        let depMap = Map.fromList [(["a"], [["b"]]), (["b"], [])]
        topoOrder depMap [["a"]] @?= Right [["b"], ["a"]]),

    ("2", do
        let depMap = Map.fromList [(["a"], [])]
        topoOrder depMap [["a"], ["a"]] @?= Right [["a"]]),

    ("3", do
        let depMap = Map.fromList [(["a"], [["b"]]), (["b"], [["a"]])]
        case topoOrder depMap [["a"]] of
            Left _ -> pure ()
            Right v -> assertFailure ("expected Left, got Right: " ++ show v))
    ]


mergeImportEnvTests :: TestTree
mergeImportEnvTests = mkGroup "Semantic.CheckProgram.mergeImportEnv" [
    ("0", do
        let a = IEnv "a" Map.empty Map.empty
            b = IEnv "b" Map.empty Map.empty
            out = mergeImportEnv a b
        iVars out @?= Map.empty
        iFuncs out @?= Map.empty),

    ("1", do
        let a = IEnv "a" (Map.fromList [(["x"], [pos 1])]) Map.empty
            b = IEnv "b" (Map.fromList [(["y"], [pos 2])]) Map.empty
            out = mergeImportEnv a b
        HashSet.fromList (Map.keys (iVars out)) @?= HashSet.fromList [["x"], ["y"]]),

    ("2", do
        let a = IEnv "a" Map.empty (Map.fromList [(["f"], [pos 1])])
            b = IEnv "b" Map.empty (Map.fromList [(["g"], [pos 2])])
            out = mergeImportEnv a b
        HashSet.fromList (Map.keys (iFuncs out)) @?= HashSet.fromList [["f"], ["g"]]),

    ("3", do
        let a = IEnv "a" (Map.fromList [(["x"], [pos 1])]) Map.empty
            b = IEnv "b" (Map.fromList [(["x"], [pos 2])]) Map.empty
            out = mergeImportEnv a b
        Map.lookup ["x"] (iVars out) @?= Just [pos 1, pos 2])
    ]


mergeTypedEnvTests :: TestTree
mergeTypedEnvTests = mkGroup "Semantic.CheckProgram.mergeTypedEnv" [
    ("0", do
        let a = emptyTypedImportEnv "a"
            b = emptyTypedImportEnv "b"
            out = mergeTypedEnv a b
        tVars out @?= Map.empty
        tFuncs out @?= Map.empty),

    ("1", do
        let a = (emptyTypedImportEnv "a") { tVars = Map.fromList [(["x"], (Int32T, [pos 1], ["a", "X"]))] }
            b = (emptyTypedImportEnv "b") { tVars = Map.fromList [(["y"], (Bool, [pos 2], ["b", "Y"]))] }
            out = mergeTypedEnv a b
        HashSet.fromList (Map.keys (tVars out)) @?= HashSet.fromList [["x"], ["y"]]),

    ("2", do
        let s1 = FunSig [Int32T] Int32T
            s2 = FunSig [Bool] Int32T
            a = (emptyTypedImportEnv "a") { tFuncs = Map.fromList [(["f"], ([s1], [pos 1], ["pkg", "A", "f"]))] }
            b = (emptyTypedImportEnv "b") { tFuncs = Map.fromList [(["f"], ([s1, s2], [pos 2], ["pkg", "B", "f"]))] }
            out = mergeTypedEnv a b
        case Map.lookup ["f"] (tFuncs out) of
            Nothing -> assertFailure "missing merged function"
            Just (sigs, poses, full) -> do
                sigs @?= [s1, s2]
                poses @?= [pos 1, pos 2]
                full @?= ["pkg", "A", "f"]),

    ("3", do
        let s1 = FunSig [Int32T] Int32T
            a = (emptyTypedImportEnv "a") { tFuncs = Map.fromList [(["f"], ([s1], [pos 1], []))] }
            b = (emptyTypedImportEnv "b") { tFuncs = Map.fromList [(["f"], ([s1], [pos 2], ["pkg", "B", "f"]))] }
            out = mergeTypedEnv a b
        case Map.lookup ["f"] (tFuncs out) of
            Nothing -> assertFailure "missing merged function"
            Just (_, _, full) -> full @?= ["pkg", "B", "f"])
    ]


symbolAliasesTests :: TestTree
symbolAliasesTests = mkGroup "Semantic.CheckProgram.symbolAliases" [
    ("0", do
        let (fullQn, aliases) = symbolAliases "src/m1.x" ["com", "wd"] "a"
        fullQn @?= ["com", "wd", "M1Xl", "a"]
        HashSet.fromList aliases @?=
            HashSet.fromList [["com", "wd", "a"], ["com", "wd", "M1Xl", "a"], ["a"], ["M1Xl", "a"]]),

    ("1", do
        let (_, aliases) = symbolAliases "f1.x" [] "a"
        HashSet.fromList aliases @?= HashSet.fromList [["a"], ["F1Xl", "a"]]),

    ("2", do
        let (fullQn, aliases) = symbolAliases "folder/util" ["p"] "x"
        fullQn @?= ["p", "UtilXl", "x"]
        assertBool "contains file alias" (["UtilXl", "x"] `elem` aliases)),

    ("3", do
        let (fullQn, _) = symbolAliases "" ["p"] "v"
        fullQn @?= ["p", "MainXl", "v"])
    ]


fileClassNameTests :: TestTree
fileClassNameTests = mkGroup "Semantic.CheckProgram.fileClassName" [
    ("0", do
        fileClassName "foo.x" @?= "FooXl"),

    ("1", do
        fileClassName "src/pkg/main.x" @?= "MainXl"),

    ("2", do
        fileClassName "src\\pkg\\test_file.x" @?= "Test_fileXl"),

    ("3", do
        fileClassName "" @?= "MainXl")
    ]


choosePosTests :: TestTree
choosePosTests = mkGroup "Semantic.CheckProgram.choosePos" [
    ("0", do
        let out = choosePos (varExpr "x" 10) [identTok "ret" 1]
        out @?= [pos 10]),

    ("1", do
        let out = choosePos (Qualified ["a", "b"] [identTok "a" 3, identTok "b" 4]) [identTok "ret" 1]
        out @?= [pos 3, pos 4]),

    ("2", do
        let out = choosePos (Error [] "x") [identTok "ret" 7, identTok "ret2" 8]
        out @?= [pos 7, pos 8]),

    ("3", do
        choosePos (Error [] "x") [] @?= [])
    ]


prettyQNameTests :: TestTree
prettyQNameTests = mkGroup "Semantic.CheckProgram.prettyQName" [
    ("0", do
        prettyQName [] @?= ""),

    ("1", do
        prettyQName ["a"] @?= "a"),

    ("2", do
        prettyQName ["a", "b", "c"] @?= "a.b.c"),

    ("3", do
        prettyQName ["a", "", "c"] @?= "a..c")
    ]


safeHeadTests :: TestTree
safeHeadTests = mkGroup "Semantic.CheckProgram.safeHead" [
    ("0", do
        safeHead ([] :: [Int]) @?= Nothing),

    ("1", do
        safeHead [1 :: Int] @?= Just 1),

    ("2", do
        safeHead [1 :: Int, 2, 3] @?= Just 1),

    ("3", do
        safeHead ["x", "y"] @?= Just "x")
    ]


checkOneProgramTests :: TestTree
checkOneProgramTests = mkGroup "Semantic.CheckProgram.checkOneProgram" [
    ("0", do
        let prog = mkProgram [] [] [mkFunOk "f" 1]
        case checkOneProgram "a.x" prog [] [] of
            Right _ -> pure ()
            Left e -> assertFailure ("expected Right, got Left: " ++ show e)),

    ("1", do
        let prog = mkProgram [] [] [mkFunNoReturn "f" 1]
        assertLeftWith (checkOneProgram "a.x" prog [] []) $ \errs ->
            assertBool "must report missing return"
                (any (isInfixOf "missing return statement" . errWhy) errs)),

    ("2", do
        let bad = Expr (Binary Assign (intExpr 1 1) (intExpr 2 2) (symTok Lex.Assign 3))
            prog = mkProgram [] [] [bad]
        assertLeftWith (checkOneProgram "a.x" prog [] []) $ \errs ->
            assertBool "must report cannot assign"
                (any (isInfixOf "cannot be assigned" . errWhy) errs)),

    ("3", do
        let prog = mkProgram [] [] [assignStmt "b" (varExpr "a" 2) 2]
            ien = IEnv "dep.x" (Map.fromList [(["a"], [pos 2])]) Map.empty
            ten = (emptyTypedImportEnv "dep.x") {
                tVars = Map.fromList [(["a"], (Int32T, [pos 2], ["dep", "DepXl", "a"]))]
            }
        case checkOneProgram "a.x" prog [ien] [ten] of
            Right _ -> pure ()
            Left e -> assertFailure ("expected Right, got Left: " ++ show e)),

    ("4", do
        let prog = mkProgram [] [] [assignStmt "b" (varExpr "a" 2) 2]
            ien = IEnv "dep.x" (Map.fromList [(toHiddenQName ["a"], [pos 2])]) Map.empty
        assertLeftWith (checkOneProgram "a.x" prog [ien] []) $ \errs ->
            assertBool "must report not visible for imported variable"
                (any ((== UE.notVisibleMsg) . errWhy) errs)),

    ("5", do
        let callStmt = Expr (Binary Assign (varExpr "b" 1) (Call (varExpr "f" 2) []) (symTok Lex.Assign 3))
            prog = mkProgram [] [] [callStmt]
            ien = IEnv "dep.x" Map.empty (Map.fromList [(toHiddenQName ["f"], [pos 2])])
        assertLeftWith (checkOneProgram "a.x" prog [ien] []) $ \errs ->
            assertBool "must report not visible for imported function"
                (any ((== UE.notVisibleMsg) . errWhy) errs))
    ]


checkProgmTests :: TestTree
checkProgmTests = mkGroup "Semantic.CheckProgram.checkProgm" [
    ("0", do
        assertRightWith (checkProgm "." []) $ \xs -> assertBool "expected empty result" (null xs)),

    ("1", do
        let files = [("a.x", mkProgram [] [] [assignStmt "a" (intExpr 1 1) 1])]
        assertRightWith (checkProgm "." files) $ \xs ->
            length xs @?= 1),

    ("2", do
        let files = [("a/main.x", mkProgram ["a"] [["missing", "pkg"]] [])]
        assertLeftWith (checkProgm "." files) $ \errs ->
            assertBool "must mention missing package"
                (any (isInfixOf "'missing.pkg' is not defined in this context." . errWhy) errs)),

    ("3", do
        let aProg = mkProgram ["a"] [["b"]] []
            bProg = mkProgram ["b"] [["a"]] []
            files = [("a/main.x", aProg), ("b/main.x", bProg)]
        assertLeftWith (checkProgm "." files) $ \errs ->
            assertBool "must report circular import"
                (any (isInfixOf "circular import detected" . errWhy) errs))
    ]


checkPackageFixpointTests :: TestTree
checkPackageFixpointTests = mkGroup "Semantic.CheckProgram.checkPackageFixpoint" [
    ("0", do
        let i0 = IEnv "seed.x" Map.empty Map.empty
            t0 = emptyTypedImportEnv "seed.x"
        assertRightWith (checkPackageFixpoint [] [] [] i0 t0 Map.empty) $ \(i, t, ctxs) -> do
            i @?= i0
            t @?= t0
            assertBool "expected empty ctx map" (Map.null ctxs)),

    ("1", do
        let mi = mkModuleInfo "m1.x" ["p"] [] [assignStmt "a" (intExpr 1 1) 1]
            i0 = IEnv "seed.x" Map.empty Map.empty
            t0 = emptyTypedImportEnv "seed.x"
        assertRightWith (checkPackageFixpoint [] [] [mi] i0 t0 Map.empty) $ \(_, _, ctxs) ->
            Map.size ctxs @?= 1),

    ("2", do
        let miBad = mkModuleInfo "m1.x" ["p"] [] [mkFunNoReturn "f" 1]
            i0 = IEnv "seed.x" Map.empty Map.empty
            t0 = emptyTypedImportEnv "seed.x"
        assertLeftWith (checkPackageFixpoint [] [] [miBad] i0 t0 Map.empty) $ \errs ->
            assertBool "must fail" (not (null errs))),

    ("3", do
        let miNeed = mkModuleInfo "need.x" ["p"] [] [assignStmt "b" (addExpr (varExpr "a" 2) (intExpr 1 3) 2) 2]
            miDef = mkModuleInfo "def.x" ["p"] [] [assignStmt "a" (intExpr 1 1) 1]
            i0 = IEnv "seed.x" Map.empty Map.empty
            t0 = emptyTypedImportEnv "seed.x"
        assertRightWith (checkPackageFixpoint [] [] [miNeed, miDef] i0 t0 Map.empty) $ \(_, _, ctxs) ->
            Map.size ctxs @?= 2)
    ]


checkPackageTests :: TestTree
checkPackageTests = mkGroup "Semantic.CheckProgram.checkPackage" [
    ("0", do
        assertLeftWith (checkPackage Map.empty Map.empty Map.empty ["p"]) $ \errs ->
            assertBool "must fail on missing package" (not (null errs))),

    ("1", do
        let mi = mkModuleInfo "p.x" ["p"] [] [assignStmt "a" (intExpr 1 1) 1]
            pkgMap = Map.fromList [(["p"], [mi])]
            depMap = Map.fromList [(["p"], [])]
        assertRightWith (checkPackage pkgMap depMap Map.empty ["p"]) $ \checked ->
            assertBool "package should be checked" (Map.member ["p"] checked)),

    ("2", do
        let miBad = mkModuleInfo "p.x" ["p"] [] [mkFunNoReturn "f" 1]
            pkgMap = Map.fromList [(["p"], [miBad])]
            depMap = Map.fromList [(["p"], [])]
        assertLeftWith (checkPackage pkgMap depMap Map.empty ["p"]) $ \errs ->
            assertBool "must have semantic errors" (not (null errs))),

    ("3", do
        let miMain = mkModuleInfo "main.x" ["main"] [["dep"]] [assignStmt "b" (intExpr 1 1) 1]
            pkgMap = Map.fromList [(["main"], [miMain])]
            depMap = Map.fromList [(["main"], [["dep"]])]
            depChecked = CheckedPackage {
                cpImportEnv = IEnv "dep.x" Map.empty Map.empty,
                cpTypedEnv = emptyTypedImportEnv "dep.x",
                cpTypeCtxs = Map.empty :: Map Path TC.TypeCtx
            }
            checked0 = Map.fromList [(["dep"], depChecked)]
        assertRightWith (checkPackage pkgMap depMap checked0 ["main"]) $ \checked ->
            assertBool "main package should be added" (Map.member ["main"] checked))
    ]


buildExportTests :: TestTree
buildExportTests = mkGroup "Semantic.CheckProgram.buildExport" [
    ("0", do
        let path = "m1.x"
            prog = mkProgram ["p"] [] [assignStmt "a" (intExpr 1 1) 1]
            mi = mustRight (toModuleInfo (path, prog))
            ctx = mustRight (checkOneProgram path prog [] [])
            me = buildExport mi ctx
        assertBool "must expose short alias" (Map.member ["a"] (iVars (meImportEnv me)))
        assertBool "must expose package alias" (Map.member ["p", "a"] (iVars (meImportEnv me)))),

    ("1", do
        let path = "m2.x"
            prog = mkProgram ["p"] [] [mkFunOk "add" 1]
            mi = mustRight (toModuleInfo (path, prog))
            ctx = mustRight (checkOneProgram path prog [] [])
            me = buildExport mi ctx
        assertBool "must expose short function alias" (Map.member ["add"] (iFuncs (meImportEnv me)))
        assertBool "must expose package function alias" (Map.member ["p", "add"] (iFuncs (meImportEnv me)))),

    ("2", do
        let path = "m3.x"
            prog = mkProgram ["p"] [] [assignStmt "a" (intExpr 1 1) 1, mkFunOk "f" 10]
            mi = mustRight (toModuleInfo (path, prog))
            ctx = mustRight (checkOneProgram path prog [] [])
            me = buildExport mi ctx
        assertBool "vars non-empty" (not (Map.null (iVars (meImportEnv me))))
        assertBool "funs non-empty" (not (Map.null (iFuncs (meImportEnv me))))),

    ("3", do
        let path = "m4.x"
            prog = mkProgram ["p"] [] [assignStmt "a" (intExpr 1 1) 1]
            mi = mustRight (toModuleInfo (path, prog))
            ctx = mustRight (checkOneProgram path prog [] [])
            me = buildExport mi ctx
        case Map.lookup ["a"] (tVars (meTypedEnv me)) of
            Nothing -> assertFailure "missing var alias in typed export"
            Just (_, _, full) -> full @?= ["p", "M4Xl", "a"])
    ]


collectFunctionsTests :: TestTree
collectFunctionsTests = mkGroup "Semantic.CheckProgram.collectFunctions" [
    ("0", do
        let out = collectFunctions "m1.x" ["p"] [mkFunOk "f" 1]
            fulls = HashSet.fromList (map (\(_, qn, _, _) -> qn) out)
            keys = HashSet.fromList (map (\(k, _, _, _) -> k) out)
        HashSet.size fulls @?= 1
        HashSet.member ["p", "M1Xl", "f"] fulls @?= True
        HashSet.size keys @?= 4),

    ("1", do
        let out = collectFunctions "m2.x" ["p"] [mkFunTOk "tf" 1]
            keys = HashSet.fromList (map (\(k, _, _, _) -> k) out)
        HashSet.size keys @?= 4),

    ("2", do
        let qn = ["pkg", "C", "f"]
            out = collectFunctions "m3.x" ["p"] [mkFunQualified qn 1]
        out @?= [(qn, qn, FunSig { funParams = [], funReturn = Int32T }, [pos 2, pos 3, pos 4])]),

    ("3", do
        collectFunctions "m4.x" ["p"] [assignStmt "a" (intExpr 1 1) 1] @?= [])
    ]


collectTopLevelVarsTests :: TestTree
collectTopLevelVarsTests = mkGroup "Semantic.CheckProgram.collectTopLevelVars" [
    ("0", do
        let path = "v1.x"
            stmts = [assignStmt "a" (intExpr 1 1) 1]
            prog = mkProgram ["p"] [] stmts
            ctx = mustRight (checkOneProgram path prog [] [])
            out = collectTopLevelVars path ["p"] stmts ctx
            keys = HashSet.fromList (map (\(k, _, _, _) -> k) out)
        HashSet.member ["a"] keys @?= True
        HashSet.member ["p", "a"] keys @?= True
        length out @?= 4),

    ("1", do
        let path = "v2.x"
            stmts = [assignStmt "a" (intExpr 1 1) 1, assignStmt "b" (intExpr 2 2) 2]
            prog = mkProgram ["p"] [] stmts
            ctx = mustRight (checkOneProgram path prog [] [])
            out = collectTopLevelVars path ["p"] stmts ctx
        length out @?= 8),

    ("2", do
        let path = "v3.x"
            stmts = [mkFunOk "f" 1]
            prog = mkProgram ["p"] [] stmts
            ctx = mustRight (checkOneProgram path prog [] [])
        collectTopLevelVars path ["p"] stmts ctx @?= []),

    ("3", do
        let ctx = mustRight (checkOneProgram "seed.x" (mkProgram ["p"] [] [assignStmt "x" (intExpr 1 1) 1]) [] [])
            stmt = Expr (Binary Assign (Qualified ["a", "b"] [identTok "a" 1, identTok "b" 2]) (intExpr 1 3) (symTok Lex.Assign 4))
        collectTopLevelVars "v4.x" ["p"] [stmt] ctx @?= [])
    ]


tests :: TestTree
tests = testGroup "Semantic.CheckProgram" [
    toModuleInfoTests,
    groupByPackageTests,
    resolveImportOwnerTests,
    resolveAllImportsTests,
    buildDepMapTests,
    cycleErrorsTests,
    sccToErrorTests,
    topoOrderTests,
    mergeImportEnvTests,
    mergeTypedEnvTests,
    symbolAliasesTests,
    fileClassNameTests,
    choosePosTests,
    prettyQNameTests,
    safeHeadTests,
    checkOneProgramTests,
    checkProgmTests,
    checkPackageFixpointTests,
    checkPackageTests,
    buildExportTests,
    collectFunctionsTests,
    collectTopLevelVarsTests
    ]

