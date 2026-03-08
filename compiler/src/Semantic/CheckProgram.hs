module Semantic.CheckProgram where

import Control.Monad (foldM)
import Data.Char (toUpper)
import Data.Graph (SCC(..), stronglyConnComp)
import Data.List (foldl', intercalate, isPrefixOf, stripPrefix, maximumBy, sortOn)
import Data.Map.Strict (Map)
import Data.HashSet (HashSet)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import Parse.SyntaxTree (Declaration(..), Expression(..), Program, Statement(..), declPath, isImportDecl, normalizeClass, getPackage)
import Semantic.NameEnv (ImportEnv(..), QName, defaultImportEnv, getPackageName)
import Semantic.TypeEnv (FunSig(..), TypedImportEnv(..), emptyTypedImportEnv)
import Util.Exception (ErrorKind)
import Util.Type (Path, Position)

import qualified Data.Map.Strict as Map
import qualified Data.HashSet as HashSet
import qualified Lex.Token as Lex
import qualified Parse.SyntaxTree as AST
import qualified Semantic.ContextCheck as CC
import qualified Semantic.ReturnCheck as RC
import qualified Semantic.TypeCheck as TC
import qualified Util.Exception as UE


data ModuleInfo = ModuleInfo {
    miPath :: Path,
    miProg :: Program,
    miPkg :: QName,
    miImports :: [(QName, [Position])]
} deriving (Eq, Show)


data CheckedPackage = CheckedPackage {
    cpImportEnv :: ImportEnv,
    cpTypedEnv :: TypedImportEnv,
    cpTypeCtxs :: Map Path TC.TypeCtx
}


data ModuleExport = ModuleExport {
    meImportEnv :: ImportEnv,
    meTypedEnv :: TypedImportEnv
}

splitPath :: Path -> [String]
splitPath = filter validSeg . go . map slash
    where
        slash '\\' = '/'
        slash c = c

        validSeg :: String -> Bool
        validSeg s = not (null s) && s /= "."

        go :: String -> [String]
        go "" = []
        go s =
            let (seg, rest) = break (== '/') s
            in case rest of
                [] -> [seg]
                (_:xs) -> seg : go xs


checkPackageByRelative :: [String] -> Program -> Bool
checkPackageByRelative relative p = getPackage p == relativeToPackage relative

checkPackageByAbsolute :: Path -> (Path, Program) -> Bool
checkPackageByAbsolute root (fullPath, p) = case stripPrefix (splitPath root) (splitPath fullPath) of
    (Just a) -> checkPackageByRelative a p
    Nothing -> False

relativeToPackage :: [String] -> [String]
relativeToPackage segs = case reverse segs of
    [] -> []
    (_fileName:dirsRev) -> reverse dirsRev


checkPackageDecl :: Path -> (Path, Program) -> Either [ErrorKind] ()
checkPackageDecl root fp@(fullPath, prog@(decls, _))
    | checkPackageByAbsolute root fp = Right ()
    | otherwise =
        let declared = getPackage prog
            expected = maybe [] relativeToPackage
                    (stripPrefix (splitPath root) (splitPath fullPath))
            declaredS = if null declared then "<default>" else prettyQName declared
            expectedS = if null expected then "<default>" else prettyQName expected
            msg = UE.invalidPackageNameMsg (declaredS ++ ", expected: " ++ expectedS)
        in Left [UE.Syntax (UE.makeError fullPath (packagePositions decls) msg)]
    where
        packagePositions :: [Declaration] -> [Position]
        packagePositions ds = case filter AST.isPackageDecl ds of
            (Package _ toks:_) -> map Lex.tokenPos toks
            _ -> []


checkProgm :: Path -> [(Path, Program)] -> Either [ErrorKind] [(Path, TC.TypeCtx)]
checkProgm root files = do
    mapM_ (checkPackageDecl root) files
    modules <- traverse toModuleInfo files

    let pkgMap = groupByPackage modules

    owners <- resolveAllImports pkgMap modules

    let depMap = buildDepMap pkgMap owners
        cycErrs = cycleErrors pkgMap depMap

    if not (null cycErrs)
        then Left cycErrs
        else do
            order <- topoOrder depMap (Map.keys pkgMap)
            checked <- foldM (checkPackage pkgMap depMap) Map.empty order
            traverse (toResult checked) files
    where
        toResult :: Map QName CheckedPackage -> (Path, Program) -> Either [ErrorKind] (Path, TC.TypeCtx)
        toResult checked (path, (decls, _)) =
            case getPackageName path decls of
                Left errs -> Left errs
                Right pkg -> case Map.lookup pkg checked of
                    Nothing -> Left [UE.Syntax (UE.makeError path [] UE.internalErrorMsg)]
                    Just cp -> case Map.lookup path (cpTypeCtxs cp) of
                        Just ctx -> Right (path, ctx)
                        Nothing -> Left [UE.Syntax (UE.makeError path [] UE.internalErrorMsg)]


toModuleInfo :: (Path, Program) -> Either [ErrorKind] ModuleInfo
toModuleInfo (path, prog@(decls, _)) = do
    pkg <- getPackageName path decls
    let imports = [(declPath d, map Lex.tokenPos toks) | d@(Import _ toks) <- decls, isImportDecl d]
    pure ModuleInfo {
        miPath = path,
        miProg = prog,
        miPkg = pkg,
        miImports = imports
    }


groupByPackage :: [ModuleInfo] -> Map QName [ModuleInfo]
groupByPackage = foldl' insertOne Map.empty
    where
        insertOne :: Map QName [ModuleInfo] -> ModuleInfo -> Map QName [ModuleInfo]
        insertOne mp mi = Map.insertWith (++) (miPkg mi) [mi] mp


resolveAllImports ::
    Map QName [ModuleInfo] ->
    [ModuleInfo] ->
    Either [ErrorKind] (Map (Path, QName) QName)
resolveAllImports pkgMap mods =
    let imports = [(miPath m, importQn, pos)
            | m <- mods, (importQn, pos) <- miImports m]

        foldOne (errs0, acc) (ownerPath, importQn, pos) =
            case resolveImportOwner (Map.keys pkgMap) importQn of
                Nothing ->
                    let msg = UE.undefinedIdentity (prettyQName importQn)
                    in (UE.Syntax (UE.makeError ownerPath pos msg) : errs0, acc)
                Just depPkg -> (errs0, Map.insert (ownerPath, importQn) depPkg acc)

        (errsRev, owners) = foldl' foldOne ([], Map.empty) imports
        errs = reverse errsRev
    in if null errs then Right owners else Left errs


resolveImportOwner :: [QName] -> QName -> Maybe QName
resolveImportOwner pkgs importQn =
    let candidates = filter (`isPrefixOf` importQn) pkgs
    in case candidates of
        [] -> Nothing
        xs -> Just (maximumBy (comparing length) xs)


buildDepMap :: Map QName [ModuleInfo] -> Map (Path, QName) QName -> Map QName [QName]
buildDepMap pkgMap owners =
    Map.mapWithKey one pkgMap
    where
        one :: QName -> [ModuleInfo] -> [QName]
        one pkg mods =
            let deps =
                    [ fromMaybe pkg (Map.lookup (miPath m, rawQn) owners)
                    | m <- mods
                    , (rawQn, _) <- miImports m
                    ]
            in dedup (filter (/= pkg) deps)

        dedup :: [QName] -> [QName]
        dedup = HashSet.toList . HashSet.fromList


cycleErrors :: Map QName [ModuleInfo] -> Map QName [QName] -> [ErrorKind]
cycleErrors pkgMap depMap =
    let nodes = [(pkg, pkg, Map.findWithDefault [] pkg depMap) | pkg <- Map.keys depMap]
        sccs = stronglyConnComp nodes
    in concatMap (sccToError pkgMap depMap) sccs


sccToError :: Map QName [ModuleInfo] -> Map QName [QName] -> SCC QName -> [ErrorKind]
sccToError pkgMap depMap scc =
    case scc of
        AcyclicSCC pkg ->
            let selfLoop = pkg `elem` Map.findWithDefault [] pkg depMap
            in [mkErr [pkg] | selfLoop]
        CyclicSCC pkgs -> [mkErr pkgs]
    where
        mkErr :: [QName] -> ErrorKind
        mkErr pkgs =
            let firstPkg = head pkgs
                path = case Map.lookup firstPkg pkgMap of
                    Just (m:_) -> miPath m
                    _ -> "<unknown>"
                loop = map prettyQName pkgs
                msg = "circular import detected: " ++ intercalate " -> " (loop ++ [head loop])
            in UE.Syntax (UE.makeError path [] msg)


topoOrder :: Map QName [QName] -> [QName] -> Either [ErrorKind] [QName]
topoOrder depMap roots = fmap snd (foldM step (HashSet.empty, []) roots)
    where
        step :: (HashSet QName, [QName]) -> QName -> Either [ErrorKind] (HashSet QName, [QName])
        step (visited, acc) q
            | HashSet.member q visited = Right (visited, acc)
            | otherwise = visit HashSet.empty visited acc q

        visit ::
            HashSet QName ->
            HashSet QName ->
            [QName] ->
            QName ->
            Either [ErrorKind] (HashSet QName, [QName])
        visit visiting visited acc q
            | HashSet.member q visited = Right (visited, acc)
            | HashSet.member q visiting = Left [UE.Syntax (UE.makeError "<internal>" [] UE.internalErrorMsg)]
            | otherwise = do
                let visiting' = HashSet.insert q visiting
                    deps = Map.findWithDefault [] q depMap
                (visited1, acc1) <- foldM (\(v, a) dep -> visit visiting' v a dep) (visited, acc) deps
                let visited2 = HashSet.insert q visited1
                Right (visited2, acc1 ++ [q])


checkPackage ::
    Map QName [ModuleInfo] ->
    Map QName [QName] ->
    Map QName CheckedPackage ->
    QName ->
    Either [ErrorKind] (Map QName CheckedPackage)
checkPackage pkgMap depMap checked pkg =
    case Map.lookup pkg pkgMap of
        Nothing -> Left [UE.Syntax (UE.makeError "<internal>" [] UE.internalErrorMsg)]
        Just mods0 -> do
            let mods = sortOn miPath mods0
                depPkgs = Map.findWithDefault [] pkg depMap
                depImportEnvs = mapMaybe (fmap cpImportEnv . (`Map.lookup` checked)) depPkgs
                depTypedEnvs = mapMaybe (fmap cpTypedEnv . (`Map.lookup` checked)) depPkgs
                seedPath = maybe "<pkg>" miPath (safeHead mods)
                seedImport = IEnv { file = seedPath, iVars = Map.empty, iFuncs = Map.empty }
                seedTyped = emptyTypedImportEnv seedPath

            (pkgImport, pkgTyped, ctxMap) <-
                checkPackageFixpoint depImportEnvs depTypedEnvs mods seedImport seedTyped Map.empty

            let cp = CheckedPackage {
                    cpImportEnv = pkgImport,
                    cpTypedEnv = pkgTyped,
                    cpTypeCtxs = ctxMap
                }
            Right (Map.insert pkg cp checked)


checkPackageFixpoint ::
    [ImportEnv] ->
    [TypedImportEnv] ->
    [ModuleInfo] ->
    ImportEnv ->
    TypedImportEnv ->
    Map Path TC.TypeCtx ->
    Either [ErrorKind] (ImportEnv, TypedImportEnv, Map Path TC.TypeCtx)
checkPackageFixpoint depImportEnvs depTypedEnvs mods0 importAcc0 typedAcc0 ctxAcc0 =
    go mods0 (importAcc0, typedAcc0, ctxAcc0)
    where
        go ::
            [ModuleInfo] ->
            (ImportEnv, TypedImportEnv, Map Path TC.TypeCtx) ->
            Either [ErrorKind] (ImportEnv, TypedImportEnv, Map Path TC.TypeCtx)
        go [] (importAcc, typedAcc, ctxAcc) = Right (importAcc, typedAcc, ctxAcc)
        go pending (importAcc, typedAcc, ctxAcc) =
            let (importAcc', typedAcc', ctxAcc', failedRev, progressed) =
                    foldl' step (importAcc, typedAcc, ctxAcc, [], False) pending
                failed = reverse failedRev
            in if null failed
                then Right (importAcc', typedAcc', ctxAcc')
                else if progressed
                    then go (map fst failed) (importAcc', typedAcc', ctxAcc')
                    else Left (concatMap snd failed)

        step ::
            (ImportEnv, TypedImportEnv, Map Path TC.TypeCtx, [(ModuleInfo, [ErrorKind])], Bool) ->
            ModuleInfo ->
            (ImportEnv, TypedImportEnv, Map Path TC.TypeCtx, [(ModuleInfo, [ErrorKind])], Bool)
        step (accImport, accTyped, accCtx, failed, progressed) mi =
            let importEnvs = depImportEnvs ++ [accImport]
                typedEnvs = depTypedEnvs ++ [accTyped]
            in case checkOneProgram (miPath mi) (miProg mi) importEnvs typedEnvs of
                Left errs -> (accImport, accTyped, accCtx, (mi, errs) : failed, progressed)
                Right ctx ->
                    let me = buildExport mi ctx
                        mergedImport = mergeImportEnv accImport (meImportEnv me)
                        mergedTyped = mergeTypedEnv accTyped (meTypedEnv me)
                        ctxMap' = Map.insert (miPath mi) ctx accCtx
                    in (mergedImport, mergedTyped, ctxMap', failed, True)


mergeImportEnv :: ImportEnv -> ImportEnv -> ImportEnv
mergeImportEnv a b =
    a {
        iVars = Map.unionWith (++) (iVars a) (iVars b),
        iFuncs = Map.unionWith (++) (iFuncs a) (iFuncs b)
    }


mergeTypedEnv :: TypedImportEnv -> TypedImportEnv -> TypedImportEnv
mergeTypedEnv a b =
    a {
        tVars = Map.union (tVars a) (tVars b),
        tFuncs = Map.unionWith mergeFunEntry (tFuncs a) (tFuncs b)
    }
    where
        mergeFunEntry :: ([FunSig], [Position], QName) -> ([FunSig], [Position], QName) -> ([FunSig], [Position], QName)
        mergeFunEntry (sigsA, posA, fullA) (sigsB, posB, fullB) =
            let sigs = sigsA ++ filter (`notElem` sigsA) sigsB
                poses = posA ++ posB
                full = if null fullA then fullB else fullA
            in (sigs, poses, full)


checkOneProgram :: Path -> Program -> [ImportEnv] -> [TypedImportEnv] -> Either [ErrorKind] TC.TypeCtx
checkOneProgram path prog@(decls, stmts) importEnvs typedEnvs =
    case RC.returnCheckProg path prog of
        Left errs -> Left errs
        Right () -> do
            packageName <- getPackageName path decls
            let importEnvs0 = defaultImportEnv path : importEnvs
            case CC.checkProgmWithUses path prog importEnvs0 of
                Left errs -> Left errs
                Right (st, uses) ->
                    TC.inferProgmWithCtx path packageName stmts st uses typedEnvs


buildExport :: ModuleInfo -> TC.TypeCtx -> ModuleExport
buildExport mi ctx =
    let path = miPath mi
        pkg = miPkg mi
        (_, stmts) = miProg mi

        funDecls = collectFunctions path pkg stmts
        varDecls = collectTopLevelVars path pkg stmts ctx

        iVarsMap = Map.fromListWith (++) [(keyQn, pos) | (keyQn, _, _, pos) <- varDecls]
        iFuncsMap = Map.fromListWith (++) [(keyQn, pos) | (keyQn, _, _, pos) <- funDecls]

        importEnv = IEnv {
            file = path,
            iVars = iVarsMap,
            iFuncs = iFuncsMap
        }

        tVarsMap = Map.fromList [(keyQn, (cls, pos, fullQn)) | (keyQn, fullQn, cls, pos) <- varDecls]
        tFuncsMap = foldl' insertFun Map.empty funDecls

        typedEnv0 = emptyTypedImportEnv path
        typedEnv = typedEnv0 { tVars = tVarsMap, tFuncs = tFuncsMap }
    in ModuleExport {
        meImportEnv = importEnv,
        meTypedEnv = typedEnv
    }
    where
        insertFun ::
            Map QName ([FunSig], [Position], QName) ->
            (QName, QName, FunSig, [Position]) ->
            Map QName ([FunSig], [Position], QName)
        insertFun mp (keyQn, fullQn, sig, pos) =
            let entry = ([sig], pos, fullQn)
                merge (newSigs, newPos, newFull) (oldSigs, oldPos, oldFull) =
                    let sigs = oldSigs ++ filter (`notElem` oldSigs) newSigs
                        poses = oldPos ++ newPos
                        full = if null oldFull then newFull else oldFull
                    in (sigs, poses, full)
            in Map.insertWith merge keyQn entry mp


collectFunctions :: Path -> QName -> [Statement] -> [(QName, QName, FunSig, [Position])]
collectFunctions path pkg = concatMap one
    where
        one :: Statement -> [(QName, QName, FunSig, [Position])]
        one (Function (retT, retToks) nameExpr params _) =
            let sig = FunSig {
                    funParams = map (normalizeClass . fst3) params,
                    funReturn = normalizeClass retT
                }
                pos = choosePos nameExpr retToks
            in case nameExpr of
                Variable name _ ->
                    let (fullQn, aliases) = symbolAliases path pkg name
                    in [ (alias, fullQn, sig, pos) | alias <- aliases ]
                Qualified names _ -> [ (names, names, sig, pos) ]
                _ -> []
        one (FunctionT (retT, retToks) nameExpr _ params _) =
            let sig = FunSig {
                    funParams = map (normalizeClass . fst3) params,
                    funReturn = normalizeClass retT
                }
                pos = choosePos nameExpr retToks
            in case nameExpr of
                Variable name _ ->
                    let (fullQn, aliases) = symbolAliases path pkg name
                    in [ (alias, fullQn, sig, pos) | alias <- aliases ]
                Qualified names _ -> [ (names, names, sig, pos) ]
                _ -> []
        one _ = []

        fst3 :: (a, b, c) -> a
        fst3 (a, _, _) = a


collectTopLevelVars :: Path -> QName -> [Statement] -> TC.TypeCtx -> [(QName, QName, AST.Class, [Position])]
collectTopLevelVars path pkg stmts ctx = mapMaybe one stmts >>= expandAlias
    where
        varUses = CC.varUses (TC.tcCtx ctx)
        varTypes = TC.tcVarTypes ctx

        one :: Statement -> Maybe (String, AST.Class, [Position])
        one (Expr (Binary AST.Assign (Variable name tok) _ _)) =
            let pos = Lex.tokenPos tok
                mCls = do
                    vid <- Map.lookup pos varUses
                    (cls, _) <- Map.lookup vid varTypes
                    pure cls
            in case mCls of
                Just cls -> Just (name, cls, [pos])
                Nothing -> Nothing
        one _ = Nothing

        expandAlias :: (String, AST.Class, [Position]) -> [(QName, QName, AST.Class, [Position])]
        expandAlias (name, cls, pos) =
            let (fullQn, aliases) = symbolAliases path pkg name
            in [ (alias, fullQn, cls, pos) | alias <- aliases ]


symbolAliases :: Path -> QName -> String -> (QName, [QName])
symbolAliases path pkg name =
    let fileCls = fileClassName path
        fullQn = pkg ++ [fileCls, name]
        qPkg = pkg ++ [name]
        qPkgFile = fullQn
        qBare = [name]
        qFile = [fileCls, name]
    in (fullQn, dedup [qPkg, qPkgFile, qBare, qFile])
    where
        dedup :: [QName] -> [QName]
        dedup = HashSet.toList . HashSet.fromList


fileClassName :: Path -> String
fileClassName path =
    let fileName = reverse (takeWhile (\c -> c /= '/' && c /= '\\') (reverse path))
        base = takeWhile (/= '.') fileName
    in case base of
        [] -> "MainXl"
        (c:cs) -> toUpper c : cs ++ "Xl"


choosePos :: Expression -> [Lex.Token] -> [Position]
choosePos nameExpr retToks =
    let namePos = map Lex.tokenPos (AST.exprTokens nameExpr)
    in if null namePos then map Lex.tokenPos retToks else namePos


prettyQName :: QName -> String
prettyQName = intercalate "."


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x
