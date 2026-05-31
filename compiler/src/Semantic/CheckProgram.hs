{-# LANGUAGE PatternSynonyms #-}

module Semantic.CheckProgram where

import Control.Monad (foldM)
import Data.Char (toLower, toUpper, isSpace)
import Data.Graph (SCC(..), stronglyConnComp)
import Data.List (foldl', intercalate, isPrefixOf, stripPrefix, maximumBy, sortOn, nub)
import Data.Map.Strict (Map)
import Data.HashSet (HashSet)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import System.Info (os)
import Parse.SyntaxTree (Declaration(..), Expression(..), Program, Statement(..), pattern Function, pattern FunctionT, declPath, getJavaName, getPackage, isImportDecl, normalizeClass)
import Parse.SyntaxTree (AccessModified(..))
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


data SemanticTarget
    = SemanticTargetNative
    | SemanticTargetJvm
    deriving (Eq, Show)

splitPath :: Path -> [String]
splitPath = filter validSeg . go . normalizeCase . map slash
    where
        slash '\\' = '/'
        slash c = c

        normalizeCase :: String -> String
        normalizeCase
            | os == "mingw32" = map toLower
            | otherwise = id

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
checkProgm root = checkProgmWithDepsForTarget SemanticTargetNative root [] []


checkProgmWithDeps ::
    Path ->
    [ImportEnv] ->
    [TypedImportEnv] ->
    [(Path, Program)] ->
    Either [ErrorKind] [(Path, TC.TypeCtx)]
checkProgmWithDeps root extImportEnvs extTypedEnvs files =
    checkProgmWithDepsForTarget SemanticTargetNative root extImportEnvs extTypedEnvs files


checkProgmWithDepsForTarget ::
    SemanticTarget ->
    Path ->
    [ImportEnv] ->
    [TypedImportEnv] ->
    [(Path, Program)] ->
    Either [ErrorKind] [(Path, TC.TypeCtx)]
checkProgmWithDepsForTarget target root extImportEnvs extTypedEnvs files = do
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
            checked <- foldM (checkPackageWithDepsForTarget target pkgMap depMap extImportEnvs extTypedEnvs) Map.empty order
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
            ++ mapMaybe (pkgFromFull . (\(_, _, full) -> full)) (Map.elems (tTemplates env))

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
checkPackage pkgMap depMap = checkPackageWithDepsForTarget SemanticTargetNative pkgMap depMap [] []


checkPackageWithDeps ::
    Map QName [ModuleInfo] ->
    Map QName [QName] ->
    [ImportEnv] ->
    [TypedImportEnv] ->
    Map QName CheckedPackage ->
    QName ->
    Either [ErrorKind] (Map QName CheckedPackage)
checkPackageWithDeps pkgMap depMap extImportEnvs extTypedEnvs checked pkg =
    checkPackageWithDepsForTarget SemanticTargetNative pkgMap depMap extImportEnvs extTypedEnvs checked pkg


checkPackageWithDepsForTarget ::
    SemanticTarget ->
    Map QName [ModuleInfo] ->
    Map QName [QName] ->
    [ImportEnv] ->
    [TypedImportEnv] ->
    Map QName CheckedPackage ->
    QName ->
    Either [ErrorKind] (Map QName CheckedPackage)
checkPackageWithDepsForTarget target pkgMap depMap extImportEnvs extTypedEnvs checked pkg =
    case Map.lookup pkg pkgMap of
        Nothing -> Left [UE.Syntax (UE.makeError "<internal>" [] UE.internalErrorMsg)]
        Just mods0 -> do
            let mods = sortOn miPath mods0
                depPkgs = Map.findWithDefault [] pkg depMap
                depImportEnvs = mapMaybe (fmap cpImportEnv . (`Map.lookup` checked)) depPkgs ++ extImportEnvs
                depTypedEnvs = mapMaybe (fmap cpTypedEnv . (`Map.lookup` checked)) depPkgs ++ extTypedEnvs
                depStructStmts =
                    let depMods = concatMap (\q -> Map.findWithDefault [] q pkgMap) depPkgs
                    in collectTopLevelStructStmts depMods
                pkgStructStmts = collectTopLevelStructStmts mods
                externalStructStmts = nub (pkgStructStmts ++ depStructStmts)
                seedPath = maybe "<pkg>" miPath (safeHead mods)
                seedImport = IEnv { file = seedPath, iVars = Map.empty, iFuncs = Map.empty }
                seedTyped = emptyTypedImportEnv seedPath

            (pkgImport, pkgTyped, ctxMap) <-
                checkPackageFixpoint target depImportEnvs depTypedEnvs mods seedImport seedTyped Map.empty externalStructStmts

            let cp = CheckedPackage {
                    cpImportEnv = pkgImport,
                    cpTypedEnv = pkgTyped,
                    cpTypeCtxs = ctxMap
                }
            Right (Map.insert pkg cp checked)


checkPackageFixpoint ::
    SemanticTarget ->
    [ImportEnv] ->
    [TypedImportEnv] ->
    [ModuleInfo] ->
    ImportEnv ->
    TypedImportEnv ->
    Map Path TC.TypeCtx ->
    [Statement] ->
    Either [ErrorKind] (ImportEnv, TypedImportEnv, Map Path TC.TypeCtx)
checkPackageFixpoint target depImportEnvs depTypedEnvs mods0 importAcc0 typedAcc0 ctxAcc0 externalStructStmts =
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
            in case checkOneProgramForTarget target (miPath mi) (miProg mi) importEnvs typedEnvs externalStructStmts of
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
        tFuncs = Map.unionWith mergeFunEntry (tFuncs a) (tFuncs b),
        tTemplates = Map.unionWith mergeTemplateEntry (tTemplates a) (tTemplates b)
    }
    where
        mergeFunEntry :: ([FunSig], [Position], QName) -> ([FunSig], [Position], QName) -> ([FunSig], [Position], QName)
        mergeFunEntry (sigsA, posA, fullA) (sigsB, posB, fullB) =
            let sigs = sigsA ++ filter (`notElem` sigsA) sigsB
                poses = posA ++ posB
                full = if null fullA then fullB else fullA
            in (sigs, poses, full)

        mergeTemplateEntry :: ([Statement], [Position], QName) -> ([Statement], [Position], QName) -> ([Statement], [Position], QName)
        mergeTemplateEntry (defsA, posA, fullA) (defsB, posB, fullB) =
            let defs = defsA ++ filter (`notElem` defsA) defsB
                poses = posA ++ posB
                full = if null fullA then fullB else fullA
            in (defs, poses, full)


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
        ++ map (\(_, _, full) -> full) (Map.elems (tTemplates env))


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
validateImportDeclSpecs _path typedEnvs specsWithPos =
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
    step fulls (errs, accSpecs) (spec, _pos) = case spec of
        ImportClass qn ->
            if classExistsInEnvs fulls qn
                then (errs, spec : accSpecs)
                else (errs, spec : accSpecs)
        ImportWildcard pkgQn ->
            if packageExistsInEnvs fulls pkgQn
                then (errs, spec : accSpecs)
                else (errs, spec : accSpecs)


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
            memberAliases =
                case splitStructMemberName member of
                    Just (owner, m) ->
                        [ [m]
                        , [owner, m]
                        , currentPkg ++ [m]
                        , currentPkg ++ [owner, m]
                        ]
                    Nothing ->
                        []
        in dedupQNames (full : shortClass : samePkgTopLevel ++ memberAliases)
  where
    splitStructMemberName :: String -> Maybe (String, String)
    splitStructMemberName s =
        let (owner, rest) = break (== '$') s
        in case rest of
            ('$' : m) | not (null owner) && not (null m) -> Just (owner, m)
            _ -> Nothing

    dedupQNames :: [QName] -> [QName]
    dedupQNames = HashSet.toList . HashSet.fromList


expandTypedImportEnvBySpecs :: QName -> [ImportSpec] -> TypedImportEnv -> TypedImportEnv
expandTypedImportEnvBySpecs currentPkg specs env =
    env {
        tVars = expandVarMap (tVars env),
        tFuncs = expandFunMap (tFuncs env),
        tTemplates = expandTemplateMap (tTemplates env)
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
                alias <- aliasesForEntry full (HashSet.member (fullFromEntry entry) topLevels),
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
                alias <- aliasesForEntry full (HashSet.member (fullFromEntry entry) topLevels),
                let aliasKey = if isHidden then toHiddenQName alias else alias]
            mergeFunEntry :: ([FunSig], [Position], QName) -> ([FunSig], [Position], QName) -> ([FunSig], [Position], QName)
            mergeFunEntry (newSigs, newPos, newFull) (oldSigs, oldPos, oldFull) =
                let sigs = oldSigs ++ filter (`notElem` oldSigs) newSigs
                    poses = oldPos ++ newPos
                    full = if null oldFull then newFull else oldFull
                in (sigs, poses, full)
        in foldl' (\acc (k, v) -> Map.insertWith mergeFunEntry k v acc) Map.empty aliases

    expandTemplateMap :: Map QName ([Statement], [Position], QName) -> Map QName ([Statement], [Position], QName)
    expandTemplateMap mp =
        let entries = Map.toList mp
            topLevels = HashSet.fromList [
                fullQn | (keyQn, (_, _, fullQn)) <- entries,
                let (_, payload) = splitHiddenQName keyQn, isTopLevelAliasPayload payload fullQn]
            aliases = [
                (aliasKey, entry) | (keyQn, entry) <- entries,
                let (isHidden, _) = splitHiddenQName keyQn,
                let full = fullFromEntry entry,
                alias <- aliasesForEntry full (HashSet.member (fullFromEntry entry) topLevels),
                let aliasKey = if isHidden then toHiddenQName alias else alias]
            mergeTemplateEntry :: ([Statement], [Position], QName) -> ([Statement], [Position], QName) -> ([Statement], [Position], QName)
            mergeTemplateEntry (newDefs, newPos, newFull) (oldDefs, oldPos, oldFull) =
                let defs = oldDefs ++ filter (`notElem` oldDefs) newDefs
                    poses = oldPos ++ newPos
                    full = if null oldFull then newFull else oldFull
                in (defs, poses, full)
        in foldl' (\acc (k, v) -> Map.insertWith mergeTemplateEntry k v acc) Map.empty aliases

    fullFromEntry :: (a, b, QName) -> QName
    fullFromEntry (_, _, full) = full

    isSamePackageFull :: QName -> Bool
    isSamePackageFull full = case splitFullQName full of
        Just (pkg, _, _) -> pkg == currentPkg
        Nothing -> False

    aliasesForEntry :: QName -> Bool -> [QName]
    aliasesForEntry full isTopLevel
        | isSamePackageFull full || any (`matchesImportSpec` full) specs =
            aliasTargetsForSpec currentPkg isTopLevel full
        | isTopLevel =
            case splitFullQName full of
                Just (pkg, _, member) ->
                    dedupQNames [full, pkg ++ [member]]
                Nothing ->
                    [full]
        | otherwise =
            [full]
      where
        dedupQNames :: [QName] -> [QName]
        dedupQNames = HashSet.toList . HashSet.fromList


typedToImportEnv :: TypedImportEnv -> ImportEnv
typedToImportEnv env =
    let funPos = Map.map (\(_, pos, _) -> pos) (tFuncs env)
        templatePos = Map.map (\(_, pos, _) -> pos) (tTemplates env)
        mergedFunPos = Map.unionWith (++) funPos templatePos
    in
    IEnv {
        file = tFile env,
        iVars = Map.map (\(_, pos, _) -> pos) (tVars env),
        iFuncs = mergedFunPos
    }


checkPointerRulesForTarget :: SemanticTarget -> Path -> Program -> Either [ErrorKind] ()
checkPointerRulesForTarget SemanticTargetNative _ _ = Right ()
checkPointerRulesForTarget SemanticTargetJvm path (_, stmts) =
    let errs = concatMap stmtPointerErrors stmts
    in if null errs then Right () else Left errs
  where
    stmtPointerErrors :: Statement -> [ErrorKind]
    stmtPointerErrors stmt = case stmt of
        DefField _ mTy mExpr toks ->
            classErrs mTy toks ++ maybe [] exprPointerErrors mExpr
        DefConstField _ mTy mExpr toks ->
            classErrs mTy toks ++ maybe [] exprPointerErrors mExpr
        DefVar _ mTy mExpr toks ->
            classErrs mTy toks ++ maybe [] exprPointerErrors mExpr
        DefConstVar _ mTy mExpr toks ->
            classErrs mTy toks ++ maybe [] exprPointerErrors mExpr
        Expr e -> exprPointerErrors e
        Exprs es -> concatMap exprPointerErrors es
        StmtGroup ss -> concatMap stmtPointerErrors ss
        BlockStmt (AST.Multiple ss) -> concatMap stmtPointerErrors ss
        If cond b1 b2 _ ->
            exprPointerErrors cond ++ blockPointerErrors b1 ++ blockPointerErrors b2
        For (mInit, mCond, mStep) b1 b2 _ ->
            maybe [] stmtPointerErrors mInit
                ++ maybe [] exprPointerErrors mCond
                ++ maybe [] stmtPointerErrors mStep
                ++ blockPointerErrors b1
                ++ blockPointerErrors b2
        Loop b _ ->
            blockPointerErrors b
        Repeat cnt b1 b2 _ ->
            exprPointerErrors cnt ++ blockPointerErrors b1 ++ blockPointerErrors b2
        While cond b1 b2 _ ->
            exprPointerErrors cond ++ blockPointerErrors b1 ++ blockPointerErrors b2
        Until cond b1 b2 _ ->
            exprPointerErrors cond ++ blockPointerErrors b1 ++ blockPointerErrors b2
        DoWhile b cond b2 _ ->
            blockPointerErrors b ++ exprPointerErrors cond ++ blockPointerErrors b2
        DoUntil b cond b2 _ ->
            blockPointerErrors b ++ exprPointerErrors cond ++ blockPointerErrors b2
        Switch cond scs _ ->
            exprPointerErrors cond ++ concatMap switchCasePointerErrors scs
        Function (retC, retToks) nameExpr params body ->
            classErrs (Just retC) retToks
                ++ exprPointerErrors nameExpr
                ++ concatMap (\(pty, _, ptoks) -> classErrs (Just pty) ptoks) params
                ++ blockPointerErrors (Just body)
        FunctionT (retC, retToks) nameExpr gens params body ->
            classErrs (Just retC) retToks
                ++ exprPointerErrors nameExpr
                ++ concatMap (\(gty, gtoks) -> classErrs (Just gty) gtoks) gens
                ++ concatMap (\(pty, _, ptoks) -> classErrs (Just pty) ptoks) params
                ++ blockPointerErrors (Just body)
        NativeMethod _ _ (retC, retToks) nameExpr params _ ->
            classErrs (Just retC) retToks
                ++ exprPointerErrors nameExpr
                ++ concatMap (\(pty, _, ptoks) -> classErrs (Just pty) ptoks) params
        _ -> []

    exprPointerErrors :: Expression -> [ErrorKind]
    exprPointerErrors expr = case expr of
        Qualified names toks ->
            case parsePointerSuffixQualified names toks of
                Just _ -> [mkErr toks "pointer operator is not supported on JVM target"]
                Nothing -> []
        Cast (cls, toks) inner _ ->
            classErrs (Just cls) toks ++ exprPointerErrors inner
        Unary op inner tok ->
            opErrs op tok ++ exprPointerErrors inner
        Binary _ lhs rhs _ ->
            exprPointerErrors lhs ++ exprPointerErrors rhs
        Call callee args ->
            exprPointerErrors callee ++ concatMap exprPointerErrors args
        CallT callee typeArgs args ->
            exprPointerErrors callee
                ++ concatMap (\(ty, toks) -> classErrs (Just ty) toks) typeArgs
                ++ concatMap exprPointerErrors args
        IfExpr cond thenE elseE _ ->
            exprPointerErrors cond
                ++ exprPointerErrors thenE
                ++ exprPointerErrors elseE
        BlockExpr (AST.Multiple ss) ->
            concatMap stmtPointerErrors ss
        _ -> []

    switchCasePointerErrors :: AST.SwitchCase -> [ErrorKind]
    switchCasePointerErrors sc = case sc of
        AST.Case e mb _ -> exprPointerErrors e ++ blockPointerErrors mb
        AST.Default b _ -> blockPointerErrors (Just b)

    blockPointerErrors :: Maybe AST.Block -> [ErrorKind]
    blockPointerErrors Nothing = []
    blockPointerErrors (Just (AST.Multiple ss)) = concatMap stmtPointerErrors ss

    classErrs :: Maybe AST.Class -> [Lex.Token] -> [ErrorKind]
    classErrs Nothing _ = []
    classErrs (Just cls) toks
        | containsPointerType cls = [mkErr toks "pointer/blob type is not supported on JVM target"]
        | otherwise = []

    opErrs :: AST.Operator -> Lex.Token -> [ErrorKind]
    opErrs op tok
        | op == AST.AddrOf || op == AST.DeRef =
            [mkErr [tok] "pointer operator is not supported on JVM target"]
        | otherwise = []

    containsPointerType :: AST.Class -> Bool
    containsPointerType ty = case ty of
        AST.Pointer _ -> True
        AST.Blob _ -> True
        AST.Class _ args -> any containsPointerType args
        _ -> False

    parsePointerSuffixQualified :: [String] -> [Lex.Token] -> Maybe ()
    parsePointerSuffixQualified names toks = case (names, toks) of
        (_base : suffixes, _ : _) ->
            if null suffixes then Nothing
            else if all isPointerSuffix suffixes then Just () else Nothing
        _ -> Nothing
      where
        isPointerSuffix :: String -> Bool
        isPointerSuffix s = case map toLower s of
            "ref" -> True
            "deref" -> True
            "dref" -> True
            _ -> False

    mkErr :: [Lex.Token] -> String -> ErrorKind
    mkErr toks why = UE.Syntax (UE.makeError path (map Lex.tokenPos toks) why)


checkOneProgram :: Path -> Program -> [ImportEnv] -> [TypedImportEnv] -> Either [ErrorKind] TC.TypeCtx
checkOneProgram path prog0 importEnvs typedEnvs =
    checkOneProgramForTarget SemanticTargetNative path prog0 importEnvs typedEnvs []


checkOneProgramForTarget :: SemanticTarget -> Path -> Program -> [ImportEnv] -> [TypedImportEnv] -> [Statement] -> Either [ErrorKind] TC.TypeCtx
checkOneProgramForTarget target path prog0 importEnvs typedEnvs externalStructStmts =
    let decls0 = fst prog0
    in do
        packageName <- getPackageName path decls0
        importedDeclSpecs <- validateImportDeclSpecs path typedEnvs (importDeclSpecs decls0)
        let importedSpecs = dedupImportSpecs (defaultImportedSpecs ++ importedDeclSpecs)
            typedEnvs0 = map (expandTypedImportEnvBySpecs packageName importedSpecs) typedEnvs
            importedTemplateStmts = collectImportedTemplateStmts typedEnvs0
            prog@(_, stmts) = AST.normalizeProgramWithExternalTemplatesAndStructs importedTemplateStmts externalStructStmts prog0
            importEnvs0 =
                if null typedEnvs
                    then defaultImportEnv path : importEnvs
                    else defaultImportEnv path : map typedToImportEnv typedEnvs0
        case checkNativeLinkConflict path prog of
            Left errs -> Left errs
            Right () ->
                case checkPointerRulesForTarget target path prog of
                    Left errs -> Left errs
                    Right () ->
                        case RC.returnCheckProg path prog of
                            Left errs -> Left errs
                            Right () ->
                                case CC.checkProgmWithUses path prog importEnvs0 of
                                    Left errs -> Left errs
                                    Right (st, uses) ->
                                        TC.inferProgmWithCtx path packageName stmts st uses typedEnvs0
    where
        defaultImportedSpecs :: [ImportSpec]
        defaultImportedSpecs = [ImportWildcard ["xlang", "io"]]

        collectImportedTemplateStmts :: [TypedImportEnv] -> [Statement]
        collectImportedTemplateStmts envs =
            dedupStmts (concatMap oneEnv envs)

        oneEnv :: TypedImportEnv -> [Statement]
        oneEnv env = concat [defs | (defs, _, _) <- Map.elems (tTemplates env)]

        dedupStmts :: [Statement] -> [Statement]
        dedupStmts = nub


collectTopLevelStructStmts :: [ModuleInfo] -> [Statement]
collectTopLevelStructStmts mods =
    [ st
    | mi <- mods
    , st <- AST.flattenStmtGroups (snd (miProg mi))
    , case st of
        Struct {} -> True
        _ -> False
    ]


checkNativeLinkConflict :: Path -> Program -> Either [ErrorKind] ()
checkNativeLinkConflict path (_, stmts) =
    let errs = concatMap stmtErrs stmts
    in if null errs then Right () else Left errs
    where
        stmtErrs :: Statement -> [ErrorKind]
        stmtErrs stmt = case stmt of
            Function (_, retToks) _ _ _ -> declErrs retToks
            FunctionT (_, retToks) _ _ _ _ -> declErrs retToks
            NativeMethod _ _ (_, retToks) _ _ _ -> declErrs retToks
            StmtGroup ss -> concatMap stmtErrs ss
            BlockStmt (AST.Multiple ss) -> concatMap stmtErrs ss
            If _ b1 b2 _ ->
                blockErrs b1 ++ blockErrs b2
            For (mInit, _, mStep) b1 b2 _ ->
                maybe [] stmtErrs mInit ++ maybe [] stmtErrs mStep ++ blockErrs b1 ++ blockErrs b2
            Loop b _ -> blockErrs b
            Repeat _ b1 b2 _ -> blockErrs b1 ++ blockErrs b2
            While _ b1 b2 _ -> blockErrs b1 ++ blockErrs b2
            Until _ b1 b2 _ -> blockErrs b1 ++ blockErrs b2
            DoWhile b1 _ b2 _ -> blockErrs b1 ++ blockErrs b2
            DoUntil b1 _ b2 _ -> blockErrs b1 ++ blockErrs b2
            Switch _ cases _ -> concatMap caseErrs cases
            _ -> []

        blockErrs :: Maybe AST.Block -> [ErrorKind]
        blockErrs Nothing = []
        blockErrs (Just (AST.Multiple ss)) = concatMap stmtErrs ss

        caseErrs :: AST.SwitchCase -> [ErrorKind]
        caseErrs sc = case sc of
            AST.Case _ mb _ -> blockErrs mb
            AST.Default b _ -> blockErrs (Just b)

        declErrs :: [Lex.Token] -> [ErrorKind]
        declErrs toks
            | hasNative && hasLink = [UE.Syntax (UE.makeError path poss why)]
            | otherwise = []
            where
                hasNative = any isNativeTok toks
                hasLink = hasLinkCTok toks
                poss = map Lex.tokenPos toks
                why = "invalid declaration: @native and @link(\"C\") cannot be used together on the same function"

        isNativeTok :: Lex.Token -> Bool
        isNativeTok tok = case tok of
            Lex.Ident s _ -> map toLower s == "native"
            Lex.Annotation name _ _ -> map toLower name == "native"
            _ -> False

        hasLinkCTok :: [Lex.Token] -> Bool
        hasLinkCTok toks = any isLinkAnnTok toks || isLinkKeywordC toks
            where
                isLinkAnnTok t = case t of
                    Lex.Annotation name [Lex.StrConst s _] _ ->
                        map toLower name == "link" && isCLinkTarget s
                    _ -> False

                isLinkKeywordC ts = case ts of
                    (_ : Lex.Ident name _ : _ : Lex.StrConst s _ : _)
                        | map toLower name == "link" && isCLinkTarget s -> True
                    (Lex.Ident name _ : _ : Lex.StrConst s _ : _)
                        | map toLower name == "link" && isCLinkTarget s -> True
                    _ -> False

                isCLinkTarget :: String -> Bool
                isCLinkTarget = (== "c") . map toLower . trimSpaces

                trimSpaces :: String -> String
                trimSpaces = dropWhileEnd isSpace . dropWhile isSpace

                dropWhileEnd :: (a -> Bool) -> [a] -> [a]
                dropWhileEnd p = reverse . dropWhile p . reverse


buildExport :: ModuleInfo -> TC.TypeCtx -> ModuleExport
buildExport mi ctx =
    let path = miPath mi
        pkg = miPkg mi
        progNorm@(_, stmts) = AST.normalizeProgramWithExternalTemplates [] (miProg mi)
        wrapperClass = fromMaybe (fileClassName path) (getJavaName progNorm)

        funDecls = collectFunctionsWithClass wrapperClass pkg stmts
        templateDecls = collectTemplatesWithClass wrapperClass pkg stmts
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
        tTemplatesMap = foldl' insertTemplate Map.empty templateDecls
        typedEnv = typedEnv0 { tVars = tVarsMap, tFuncs = tFuncsMap, tTemplates = tTemplatesMap }
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

        insertTemplate ::
            Map QName ([Statement], [Position], QName) ->
            (QName, QName, Statement, [Position]) ->
            Map QName ([Statement], [Position], QName)
        insertTemplate mp (keyQn, fullQn, defStmt, pos) =
            let entry = ([defStmt], pos, fullQn)
                merge (newDefs, newPos, newFull) (oldDefs, oldPos, oldFull) =
                    let defs = oldDefs ++ filter (`notElem` oldDefs) newDefs
                        poses = oldPos ++ newPos
                        full = if null oldFull then newFull else oldFull
                    in (defs, poses, full)
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
accessOfStmt (AST.InstanceMethod decls _ (_, retToks) _ _ _) = accessFromDeclsOrTokens decls retToks
accessOfStmt (AST.StaticMethod decls _ (_, retToks) _ _ _) = accessFromDeclsOrTokens decls retToks
accessOfStmt (AST.InstanceMethodT decls _ (_, retToks) _ _ _ _) = accessFromDeclsOrTokens decls retToks
accessOfStmt (AST.StaticMethodT decls _ (_, retToks) _ _ _ _) = accessFromDeclsOrTokens decls retToks
accessOfStmt (NativeMethod decls _ (_, retToks) _ _ _) = accessFromDeclsOrTokens decls retToks
accessOfStmt _ = Public


accessFromDecls :: [AST.Decl] -> AccessModified
accessFromDecls decls = case decls of
    ((acc, _):_) -> acc
    _ -> Public


accessFromDeclsOrTokens :: [AST.Decl] -> [Lex.Token] -> AccessModified
accessFromDeclsOrTokens decls retToks = case decls of
    ((acc, _):_) -> acc
    _ -> accessFromTokens retToks


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
                    funParams = map (TC.normalizeTypeAlias . normalizeClass . fst3) params,
                    funReturn = TC.normalizeTypeAlias (normalizeClass retT)
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
                    funParams = map (TC.normalizeTypeAlias . normalizeClass . fst3) params,
                    funReturn = TC.normalizeTypeAlias (normalizeClass retT)
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
        one stmt@(NativeMethod _ _ (retT, retToks) nameExpr params _) =
            let sig = FunSig {
                    funParams = map (TC.normalizeTypeAlias . normalizeClass . fst3) params,
                    funReturn = TC.normalizeTypeAlias (normalizeClass retT)
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


collectTemplatesWithClass :: String -> QName -> [Statement] -> [(QName, QName, Statement, [Position])]
collectTemplatesWithClass fileCls pkg = concatMap one . flattenTopStmtGroups
  where
    one :: Statement -> [(QName, QName, Statement, [Position])]
    one stmt@(FunctionT (_, retToks) nameExpr _ _ _) =
        let pos = choosePos nameExpr retToks
            access = accessOfStmt stmt
        in case nameExpr of
            Variable name _ ->
                let (fullQn, aliases) = symbolAliasesWithClass fileCls pkg name
                    keys = map (applyVisibilityKey access) aliases
                in [(key, fullQn, stmt, pos) | key <- keys]
            Qualified names _ ->
                let key = applyVisibilityKey access names
                in [(key, names, stmt, pos)]
            _ -> []
    one _ = []


collectTopLevelVars :: Path -> QName -> [Statement] -> TC.TypeCtx -> [(QName, QName, AST.Class, [Position])]
collectTopLevelVars path = collectTopLevelVarsWithClass (fileClassName path)


collectTopLevelVarsWithClass :: String -> QName -> [Statement] -> TC.TypeCtx -> [(QName, QName, AST.Class, [Position])]
collectTopLevelVarsWithClass fileCls pkg stmts ctx = mapMaybe one (flattenTopStmtGroups stmts) >>= expandAlias
    where
        varUses = CC.varUses (TC.tcCtx ctx)
        varTypes = TC.tcVarTypes ctx

        one :: Statement -> Maybe (AccessModified, String, AST.Class, [Position])
        one (Expr (Binary AST.Assign (Variable name tok) _ _)) = resolveByToken Public name tok
        one stmt@(DefField [name] mTy _ toks) =
            resolveByDeclOrToken (accessOfStmt stmt) name mTy toks
        one stmt@(DefConstField [name] mTy _ toks) =
            resolveByDeclOrToken (accessOfStmt stmt) name mTy toks
        one stmt@(DefVar [name] mTy _ toks) =
            resolveByDeclOrToken (accessOfStmt stmt) name mTy toks
        one stmt@(DefConstVar [name] mTy _ toks) =
            resolveByDeclOrToken (accessOfStmt stmt) name mTy toks
        one _ = Nothing

        resolveByDeclOrToken :: AccessModified -> String -> Maybe AST.Class -> [Lex.Token] -> Maybe (AccessModified, String, AST.Class, [Position])
        resolveByDeclOrToken access name mTy toks =
            let poss = map Lex.tokenPos toks
                declared = fmap (TC.normalizeTypeAlias . AST.normalizeClass) mTy
            in case declared of
                Just cls -> Just (access, name, cls, poss)
                Nothing -> case nameTokenFromTokens name toks of
                    Just nameTok -> resolveByToken access name nameTok
                    Nothing -> Nothing

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

        nameTokenFromTokens :: String -> [Lex.Token] -> Maybe Lex.Token
        nameTokenFromTokens name = go
          where
            go [] = Nothing
            go (t:ts) = case t of
                Lex.Ident s _ | s == name -> Just t
                _ -> go ts

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
    let mStructMember = splitStructMemberName name
        ownerCls = case mStructMember of
            Just (owner, _) -> owner
            Nothing -> fileCls
        fullQn = pkg ++ [ownerCls, name]
        qPkg = pkg ++ [name]
        qPkgFile = fullQn
        qBare = [name]
        qFile = [ownerCls, name]
        memberAliases =
            case mStructMember of
                Just (owner, member) ->
                    [ [member]
                    , [owner, member]
                    , pkg ++ [member]
                    , pkg ++ [owner, member]
                    ]
                Nothing ->
                    []
    in
        ( fullQn
        , dedup ([qPkg, qPkgFile, qBare, qFile] ++ memberAliases)
        )
    where
        dedup :: [QName] -> [QName]
        dedup = HashSet.toList . HashSet.fromList

        splitStructMemberName :: String -> Maybe (String, String)
        splitStructMemberName s =
            let (owner, rest) = break (== '$') s
            in case rest of
                ('$' : member) | not (null owner) && not (null member) -> Just (owner, member)
                _ -> Nothing


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
