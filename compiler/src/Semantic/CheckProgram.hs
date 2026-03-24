{-# LANGUAGE PatternSynonyms #-}

module Semantic.CheckProgram where

import Control.Monad (foldM)
import Data.Char (toUpper)
import Data.Graph (SCC(..), stronglyConnComp)
import Data.List (foldl', intercalate, isPrefixOf, stripPrefix, maximumBy, sortOn)
import Data.Map.Strict (Map)
import Data.HashSet (HashSet)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import Parse.SyntaxTree (Declaration(..), Expression(..), Program, Statement(..), pattern Function, pattern FunctionT, declPath, getJavaName, getPackage, isImportDecl, normalizeClass)
import Parse.ParserBasic (AccessModified(..))
import Semantic.NameEnv (ImportEnv(..), QName, defaultImportEnv, getPackageName, toHiddenQName)
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

checkJavaNameDecl :: (Path, Program) -> Either [ErrorKind] ()
checkJavaNameDecl (fullPath, (decls, _)) =
    case [tok | JavaName _ tok <- decls] of
        [] -> Right ()
        [_] -> Right ()
        (_:dups) ->
            Left [UE.Syntax (UE.makeError fullPath [Lex.tokenPos tok] UE.multipleJavaNameMsg) | tok <- dups]


checkProgm :: Path -> [(Path, Program)] -> Either [ErrorKind] [(Path, TC.TypeCtx)]
checkProgm root = checkProgmWithDeps root [] []


checkProgmWithDeps ::
    Path ->
    [ImportEnv] ->
    [TypedImportEnv] ->
    [(Path, Program)] ->
    Either [ErrorKind] [(Path, TC.TypeCtx)]
checkProgmWithDeps root extImportEnvs extTypedEnvs files = do
    mapM_ (checkPackageDecl root) files
    mapM_ checkJavaNameDecl files
    modules <- traverse toModuleInfo files

    let pkgMap = groupByPackage modules
        extPkgs = externalPackages extTypedEnvs

    owners <- resolveAllImportsWithExternal pkgMap extPkgs modules

    let depMap = buildDepMap pkgMap owners
        cycErrs = cycleErrors pkgMap depMap

    if not (null cycErrs)
        then Left cycErrs
        else do
            order <- topoOrder depMap (Map.keys pkgMap)
            checked <- foldM (checkPackageWithDeps pkgMap depMap extImportEnvs extTypedEnvs) Map.empty order
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
resolveAllImports pkgMap = resolveAllImportsWithExternal pkgMap HashSet.empty


resolveAllImportsWithExternal ::
    Map QName [ModuleInfo] ->
    HashSet QName ->
    [ModuleInfo] ->
    Either [ErrorKind] (Map (Path, QName) QName)
resolveAllImportsWithExternal pkgMap extPkgs mods =
    let imports = [(miPath m, importQn, pos)
            | m <- mods, (importQn, pos) <- miImports m]

        foldOne (errs0, acc) (ownerPath, importQn, pos) =
            case resolveImportOwner (Map.keys pkgMap) importQn of
                Just depPkg -> (errs0, Map.insert (ownerPath, importQn) depPkg acc)
                Nothing ->
                    if isExternalImport extPkgs importQn
                        then (errs0, acc)
                        else
                            let msg = UE.undefinedIdentity (prettyQName importQn)
                                err = UE.Syntax (UE.makeError ownerPath pos msg)
                            in (err : errs0, acc)

        (errsRev, owners) = foldl' foldOne ([], Map.empty) imports
        errs = reverse errsRev
    in if null errs then Right owners else Left errs


isExternalImport :: HashSet QName -> QName -> Bool
isExternalImport extPkgs importQn =
    any (`isPrefixOf` importQn) (HashSet.toList extPkgs)


externalPackages :: [TypedImportEnv] -> HashSet QName
externalPackages envs =
    HashSet.fromList (concatMap fromEnv envs)
    where
        fromEnv :: TypedImportEnv -> [QName]
        fromEnv env =
            mapMaybe (pkgFromFull . (\(_, _, full) -> full)) (Map.elems (tVars env))
            ++ mapMaybe (pkgFromFull . (\(_, _, full) -> full)) (Map.elems (tFuncs env))

        pkgFromFull :: QName -> Maybe QName
        pkgFromFull full
            | length full >= 2 = Just (take (length full - 2) full)
            | otherwise = Nothing


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
            let deps = [fromMaybe pkg (Map.lookup (miPath m, rawQn) owners) | m <- mods,
                    (rawQn, _) <- miImports m]
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
checkPackage pkgMap depMap = checkPackageWithDeps pkgMap depMap [] []


checkPackageWithDeps ::
    Map QName [ModuleInfo] ->
    Map QName [QName] ->
    [ImportEnv] ->
    [TypedImportEnv] ->
    Map QName CheckedPackage ->
    QName ->
    Either [ErrorKind] (Map QName CheckedPackage)
checkPackageWithDeps pkgMap depMap extImportEnvs extTypedEnvs checked pkg =
    case Map.lookup pkg pkgMap of
        Nothing -> Left [UE.Syntax (UE.makeError "<internal>" [] UE.internalErrorMsg)]
        Just mods0 -> do
            let mods = sortOn miPath mods0
                depPkgs = Map.findWithDefault [] pkg depMap
                depImportEnvs = mapMaybe (fmap cpImportEnv . (`Map.lookup` checked)) depPkgs ++ extImportEnvs
                depTypedEnvs = mapMaybe (fmap cpTypedEnv . (`Map.lookup` checked)) depPkgs ++ extTypedEnvs
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


data ImportSpec
    = ImportClass QName
    | ImportWildcard QName
    deriving (Eq, Show)


isWildcardImportPath :: QName -> Bool
isWildcardImportPath qn = not (null qn) && last qn == "*"


toImportSpec :: QName -> ImportSpec
toImportSpec qn
    | isWildcardImportPath qn = ImportWildcard (init qn)
    | otherwise = ImportClass qn


importDeclSpecs :: [Declaration] -> [(ImportSpec, [Position])]
importDeclSpecs decls = [(toImportSpec qn, map Lex.tokenPos toks) | d <- decls,
    isImportDecl d, 
    let qn = declPath d,
    let toks = case d of
            Import _ ts -> ts
            _ -> []]


dedupImportSpecs :: [ImportSpec] -> [ImportSpec]
dedupImportSpecs = reverse . foldl' step []
  where
    step :: [ImportSpec] -> ImportSpec -> [ImportSpec]
    step acc spec
        | spec `elem` acc = acc
        | otherwise = spec : acc


collectFullQNames :: [TypedImportEnv] -> [QName]
collectFullQNames envs =
    HashSet.toList $ HashSet.fromList $
        concatMap oneEnv envs
  where
    oneEnv :: TypedImportEnv -> [QName]
    oneEnv env =
        map (\(_, _, full) -> full) (Map.elems (tVars env))
        ++ map (\(_, _, full) -> full) (Map.elems (tFuncs env))


splitFullQName :: QName -> Maybe (QName, String, String)
splitFullQName full
    | length full < 2 = Nothing
    | otherwise =
        let member = last full
            owner = init full
            cls = last owner
            pkg = init owner
        in Just (pkg, cls, member)


classExistsInEnvs :: [QName] -> QName -> Bool
classExistsInEnvs fulls classQn =
    any matches fulls
  where
    matches :: QName -> Bool
    matches full = case splitFullQName full of
        Just (pkg, cls, _) -> pkg ++ [cls] == classQn
        Nothing -> False


packageExistsInEnvs :: [QName] -> QName -> Bool
packageExistsInEnvs fulls pkgQn =
    any matches fulls
  where
    matches :: QName -> Bool
    matches full = case splitFullQName full of
        Just (pkg, _, _) -> pkgQn `isPrefixOf` pkg
        Nothing -> False


validateImportDeclSpecs ::
    Path ->
    [TypedImportEnv] ->
    [(ImportSpec, [Position])] ->
    Either [ErrorKind] [ImportSpec]
validateImportDeclSpecs path typedEnvs specsWithPos =
    let fulls = collectFullQNames typedEnvs
        (errsRev, specsRev) = foldl' (step fulls) ([], []) specsWithPos
        errs = reverse errsRev
    in if null errs
        then Right (dedupImportSpecs (reverse specsRev))
        else Left errs
  where
    step ::
        [QName] ->
        ([ErrorKind], [ImportSpec]) ->
        (ImportSpec, [Position]) ->
        ([ErrorKind], [ImportSpec])
    step fulls (errs, accSpecs) (spec, pos) = case spec of
        ImportClass qn ->
            if classExistsInEnvs fulls qn
                then (errs, spec : accSpecs)
                else
                    let msg = UE.invalidImportDeclMsg (prettyQName qn)
                    in (UE.Syntax (UE.makeError path pos msg) : errs, accSpecs)
        ImportWildcard pkgQn ->
            if packageExistsInEnvs fulls pkgQn
                then (errs, spec : accSpecs)
                else
                    let target = if null pkgQn then "*" else prettyQName pkgQn ++ ".*"
                        msg = UE.undefinedIdentity target
                    in (UE.Syntax (UE.makeError path pos msg) : errs, accSpecs)


splitHiddenQName :: QName -> (Bool, QName)
splitHiddenQName ("$hidden$" : rest) = (True, rest)
splitHiddenQName qn = (False, qn)


matchesImportSpec :: ImportSpec -> QName -> Bool
matchesImportSpec spec full = case splitFullQName full of
    Nothing -> False
    Just (pkg, cls, _) -> case spec of
        ImportClass classQn -> classQn == pkg ++ [cls]
        ImportWildcard pkgQn -> pkgQn `isPrefixOf` pkg


isTopLevelAliasPayload :: QName -> QName -> Bool
isTopLevelAliasPayload payload full = case splitFullQName full of
    Just (pkg, _, member) -> payload == pkg ++ [member]
    Nothing -> False


aliasTargetsForSpec :: QName -> Bool -> QName -> [QName]
aliasTargetsForSpec currentPkg isTopLevel full = case splitFullQName full of
    Nothing -> [full]
    Just (_, cls, member) ->
        let shortClass = [cls, member]
            samePkgTopLevel = ([currentPkg ++ [member] | isTopLevel])
        in dedupQNames (full : shortClass : samePkgTopLevel)
  where
    dedupQNames :: [QName] -> [QName]
    dedupQNames = HashSet.toList . HashSet.fromList


expandTypedImportEnvBySpecs :: QName -> [ImportSpec] -> TypedImportEnv -> TypedImportEnv
expandTypedImportEnvBySpecs currentPkg specs env =
    env {
        tVars = expandVarMap (tVars env),
        tFuncs = expandFunMap (tFuncs env)
    }
  where
    expandVarMap :: Map QName (AST.Class, [Position], QName) -> Map QName (AST.Class, [Position], QName)
    expandVarMap mp =
        let entries = Map.toList mp
            topLevels = HashSet.fromList [
                fullQn | (keyQn, (_, _, fullQn)) <- entries,
                let (_, payload) = splitHiddenQName keyQn,
                isTopLevelAliasPayload payload fullQn]
            aliases = [
                (aliasKey, entry) | (keyQn, entry) <- entries,
                let (isHidden, _) = splitHiddenQName keyQn,
                let full = fullFromEntry entry,
                isSamePackageFull full || any (`matchesImportSpec` full) specs,
                alias <- aliasTargetsForSpec currentPkg (HashSet.member (fullFromEntry entry) topLevels) (fullFromEntry entry),
                let aliasKey = if isHidden then toHiddenQName alias else alias]
            keepOld :: (AST.Class, [Position], QName) -> (AST.Class, [Position], QName) -> (AST.Class, [Position], QName)
            keepOld _ old = old
        in foldl' (\acc (k, v) -> Map.insertWith keepOld k v acc) Map.empty aliases

    expandFunMap :: Map QName ([FunSig], [Position], QName) -> Map QName ([FunSig], [Position], QName)
    expandFunMap mp =
        let entries = Map.toList mp
            topLevels = HashSet.fromList [
                fullQn | (keyQn, (_, _, fullQn)) <- entries,
                let (_, payload) = splitHiddenQName keyQn, isTopLevelAliasPayload payload fullQn]
            aliases = [
                (aliasKey, entry) | (keyQn, entry) <- entries,
                let (isHidden, _) = splitHiddenQName keyQn,
                let full = fullFromEntry entry,
                isSamePackageFull full || any (`matchesImportSpec` full) specs,
                alias <- aliasTargetsForSpec currentPkg (HashSet.member (fullFromEntry entry) topLevels) (fullFromEntry entry),
                let aliasKey = if isHidden then toHiddenQName alias else alias]
            mergeFunEntry :: ([FunSig], [Position], QName) -> ([FunSig], [Position], QName) -> ([FunSig], [Position], QName)
            mergeFunEntry (newSigs, newPos, newFull) (oldSigs, oldPos, oldFull) =
                let sigs = oldSigs ++ filter (`notElem` oldSigs) newSigs
                    poses = oldPos ++ newPos
                    full = if null oldFull then newFull else oldFull
                in (sigs, poses, full)
        in foldl' (\acc (k, v) -> Map.insertWith mergeFunEntry k v acc) Map.empty aliases

    fullFromEntry :: (a, b, QName) -> QName
    fullFromEntry (_, _, full) = full

    isSamePackageFull :: QName -> Bool
    isSamePackageFull full = case splitFullQName full of
        Just (pkg, _, _) -> pkg == currentPkg
        Nothing -> False


typedToImportEnv :: TypedImportEnv -> ImportEnv
typedToImportEnv env =
    IEnv {
        file = tFile env,
        iVars = Map.map (\(_, pos, _) -> pos) (tVars env),
        iFuncs = Map.map (\(_, pos, _) -> pos) (tFuncs env)
    }


checkOneProgram :: Path -> Program -> [ImportEnv] -> [TypedImportEnv] -> Either [ErrorKind] TC.TypeCtx
checkOneProgram path prog0 importEnvs typedEnvs =
    let prog@(decls, stmts) = AST.promoteTopLevelFunctions prog0
    in
    case RC.returnCheckProg path prog of
        Left errs -> Left errs
        Right () -> do
            packageName <- getPackageName path decls
            importedDeclSpecs <- validateImportDeclSpecs path typedEnvs (importDeclSpecs decls)
            let importedSpecs = dedupImportSpecs (defaultImportedSpecs ++ importedDeclSpecs)
                typedEnvs0 =
                    map (expandTypedImportEnvBySpecs packageName importedSpecs) typedEnvs
                importEnvs0 =
                    if null typedEnvs
                        then defaultImportEnv path : importEnvs
                        else defaultImportEnv path : map typedToImportEnv typedEnvs0
            case CC.checkProgmWithUses path prog importEnvs0 of
                Left errs -> Left errs
                Right (st, uses) ->
                    TC.inferProgmWithCtx path packageName stmts st uses typedEnvs0
    where
        defaultImportedSpecs :: [ImportSpec]
        defaultImportedSpecs = [ImportWildcard ["xlang", "io"]]


buildExport :: ModuleInfo -> TC.TypeCtx -> ModuleExport
buildExport mi ctx =
    let path = miPath mi
        pkg = miPkg mi
        prog@(_, stmts) = miProg mi
        wrapperClass = fromMaybe (fileClassName path) (getJavaName prog)

        funDecls = collectFunctionsWithClass wrapperClass pkg stmts
        varDecls = collectTopLevelVarsWithClass wrapperClass pkg stmts ctx

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


accessFromTokens :: [Lex.Token] -> AccessModified
accessFromTokens toks = case toks of
    (Lex.Ident "private" _ : _) -> Private
    (Lex.Ident "protected" _ : _) -> Protected
    (Lex.Ident "public" _ : _) -> Public
    _ -> Public


accessOfStmt :: Statement -> AccessModified
accessOfStmt (DefField _ _ _ toks) = accessFromTokens toks
accessOfStmt (DefConstField _ _ _ toks) = accessFromTokens toks
accessOfStmt (DefVar _ _ _ toks) = accessFromTokens toks
accessOfStmt (DefConstVar _ _ _ toks) = accessFromTokens toks
accessOfStmt (Function (_, retToks) _ _ _) = accessFromTokens retToks
accessOfStmt (FunctionT (_, retToks) _ _ _ _) = accessFromTokens retToks
accessOfStmt _ = Public


applyVisibilityKey :: AccessModified -> QName -> QName
applyVisibilityKey Public qn = qn
applyVisibilityKey _ qn = toHiddenQName qn


collectFunctions :: Path -> QName -> [Statement] -> [(QName, QName, FunSig, [Position])]
collectFunctions path = collectFunctionsWithClass (fileClassName path)


collectFunctionsWithClass :: String -> QName -> [Statement] -> [(QName, QName, FunSig, [Position])]
collectFunctionsWithClass fileCls pkg = concatMap one . flattenTopStmtGroups
    where
        one :: Statement -> [(QName, QName, FunSig, [Position])]
        one stmt@(Function (retT, retToks) nameExpr params _) =
            let sig = FunSig {
                    funParams = map (normalizeClass . fst3) params,
                    funReturn = normalizeClass retT
                }
                pos = choosePos nameExpr retToks
                access = accessOfStmt stmt
            in case nameExpr of
                Variable name _ ->
                    let (fullQn, aliases) = symbolAliasesWithClass fileCls pkg name
                        keys = map (applyVisibilityKey access) aliases
                    in [ (key, fullQn, sig, pos) | key <- keys ]
                Qualified names _ ->
                    let key = applyVisibilityKey access names
                    in [ (key, names, sig, pos) ]
                _ -> []
        one stmt@(FunctionT (retT, retToks) nameExpr _ params _) =
            let sig = FunSig {
                    funParams = map (normalizeClass . fst3) params,
                    funReturn = normalizeClass retT
                }
                pos = choosePos nameExpr retToks
                access = accessOfStmt stmt
            in case nameExpr of
                Variable name _ ->
                    let (fullQn, aliases) = symbolAliasesWithClass fileCls pkg name
                        keys = map (applyVisibilityKey access) aliases
                    in [ (key, fullQn, sig, pos) | key <- keys ]
                Qualified names _ ->
                    let key = applyVisibilityKey access names
                    in [ (key, names, sig, pos) ]
                _ -> []
        one _ = []

        fst3 :: (a, b, c) -> a
        fst3 (a, _, _) = a


collectTopLevelVars :: Path -> QName -> [Statement] -> TC.TypeCtx -> [(QName, QName, AST.Class, [Position])]
collectTopLevelVars path = collectTopLevelVarsWithClass (fileClassName path)


collectTopLevelVarsWithClass :: String -> QName -> [Statement] -> TC.TypeCtx -> [(QName, QName, AST.Class, [Position])]
collectTopLevelVarsWithClass fileCls pkg stmts ctx = mapMaybe one (flattenTopStmtGroups stmts) >>= expandAlias
    where
        varUses = CC.varUses (TC.tcCtx ctx)
        varTypes = TC.tcVarTypes ctx

        one :: Statement -> Maybe (AccessModified, String, AST.Class, [Position])
        one (Expr (Binary AST.Assign (Variable name tok) _ _)) = resolveByToken Public name tok
        one stmt@(DefField [name] _ _ toks) = case reverse toks of
            (nameTok:_) -> resolveByToken (accessOfStmt stmt) name nameTok
            [] -> Nothing
        one stmt@(DefConstField [name] _ _ toks) = case reverse toks of
            (nameTok:_) -> resolveByToken (accessOfStmt stmt) name nameTok
            [] -> Nothing
        one stmt@(DefVar [name] _ _ toks) = case reverse toks of
            (nameTok:_) -> resolveByToken (accessOfStmt stmt) name nameTok
            [] -> Nothing
        one stmt@(DefConstVar [name] _ _ toks) = case reverse toks of
            (nameTok:_) -> resolveByToken (accessOfStmt stmt) name nameTok
            [] -> Nothing
        one _ = Nothing

        resolveByToken :: AccessModified -> String -> Lex.Token -> Maybe (AccessModified, String, AST.Class, [Position])
        resolveByToken access name tok =
            let pos = Lex.tokenPos tok
                mCls = do
                    vid <- Map.lookup pos varUses
                    (cls, _) <- Map.lookup vid varTypes
                    pure cls
            in case mCls of
                Just cls -> Just (access, name, cls, [pos])
                Nothing -> Nothing

        expandAlias :: (AccessModified, String, AST.Class, [Position]) -> [(QName, QName, AST.Class, [Position])]
        expandAlias (access, name, cls, pos) =
            let (fullQn, aliases) = symbolAliasesWithClass fileCls pkg name
                keys = map (applyVisibilityKey access) aliases
            in [ (key, fullQn, cls, pos) | key <- keys ]


flattenTopStmtGroups :: [Statement] -> [Statement]
flattenTopStmtGroups = concatMap go
    where
        go :: Statement -> [Statement]
        go (StmtGroup ss) = flattenTopStmtGroups ss
        go st = [st]


symbolAliases :: Path -> QName -> String -> (QName, [QName])
symbolAliases path = symbolAliasesWithClass (fileClassName path)


symbolAliasesWithClass :: String -> QName -> String -> (QName, [QName])
symbolAliasesWithClass fileCls pkg name =
    let fullQn = pkg ++ [fileCls, name]
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
        [] -> "MainX"
        (c:cs) -> toUpper c : cs ++ "X"


choosePos :: Expression -> [Lex.Token] -> [Position]
choosePos nameExpr retToks =
    let namePos = map Lex.tokenPos (AST.exprTokens nameExpr)
    in if null namePos then map Lex.tokenPos retToks else namePos


prettyQName :: QName -> String
prettyQName = intercalate "."


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x
