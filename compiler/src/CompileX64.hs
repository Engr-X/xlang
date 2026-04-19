{-# LANGUAGE OverloadedStrings #-}

module CompileX64 (
    compileX64,
    compileX64WithAssembler,
    X64CompilerChoice(..)
) where

import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (bracket_, evaluate)
import Control.Monad (unless, when)
import Control.Monad.State.Strict (evalState)
import Data.Char (toLower)
import Data.List (dropWhileEnd, foldl', intercalate, isPrefixOf, nub, sort, sortOn)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getExecutablePath)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, findExecutable, getCurrentDirectory, getTemporaryDirectory, listDirectory, removeFile)
import System.FilePath ((</>), (<.>), takeBaseName, takeDirectory, takeExtension, replaceExtension)
import System.IO (hClose, openTempFile)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Info (os)
import System.Process (callProcess)
import Text.Printf (printf)

import qualified CompileJava as CJ
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import qualified IR.Lowing as IR
import qualified IR.TAC as TAC
import qualified Lex.Tokenizer as Tokenizer
import qualified Parse.ParseProgm as Parse
import qualified Parse.ParserBasic as PB
import qualified Parse.SyntaxTree as AST
import qualified Semantic.LibLoader as LibLoader
import qualified Semantic.TypeEnv as TEnv
import qualified Util.Exception as UE
import qualified Util.FileHelper as FH
import qualified X64Lowing.ASM as X64
import qualified X64Lowing.Lowing as X64L


mapConcurrentlyLimit :: Int -> (a -> IO b) -> [a] -> IO [b]
mapConcurrentlyLimit jobs action xs
    | jobs <= 1 = mapM action xs
    | otherwise = do
        sem <- newQSem jobs
        forConcurrently xs $ \x ->
            bracket_ (waitQSem sem) (signalQSem sem) (action x)


timedIO :: IO a -> IO (a, Word64)
timedIO action = do
    begin <- getMonotonicTimeNSec
    out <- action
    end <- getMonotonicTimeNSec
    pure (out, end - begin)


formatDurationMs :: Word64 -> String
formatDurationMs dtNs
    | dtNs == 0 = "<1 tick"
    | ms >= 1.0 = printf "%.2f ms" ms
    | us >= 1.0 = printf "%.2f us" us
    | otherwise = printf "%.2f ns" ns
  where
    ns = fromIntegral dtNs :: Double
    us = ns / 1000.0
    ms = ns / 1000000.0


data StageTiming = StageTiming {
    stagePath :: FilePath,
    stageElapsedNs :: Word64
} deriving (Eq, Show)


data CompileTimingSummary = CompileTimingSummary {
    tokenizeTimings :: [StageTiming],
    parseTimings :: [StageTiming],
    importLoadNs :: Word64,
    semanticNs :: Word64,
    irTimings :: [StageTiming],
    x64LowerTimings :: [StageTiming],
    objectEmitNs :: Word64,
    linkNs :: Word64
} deriving (Eq, Show)


sumStageTimings :: [StageTiming] -> Word64
sumStageTimings = sum . map stageElapsedNs


printStagePerFile :: String -> [StageTiming] -> IO ()
printStagePerFile label xs = do
    putStrLn ("[DEBUG] " ++ label ++ ":")
    mapM_
        (\(StageTiming path elapsed) ->
            putStrLn ("[DEBUG]   " ++ path ++ ": " ++ formatDurationMs elapsed))
        xs


printCompileSummary :: CompileTimingSummary -> IO ()
printCompileSummary summary = do
    printStagePerFile "tokenize per file" (tokenizeTimings summary)
    printStagePerFile "parse per file" (parseTimings summary)
    putStrLn ("[DEBUG] import load: " ++ formatDurationMs (importLoadNs summary))
    putStrLn ("[DEBUG] semantic total: " ++ formatDurationMs (semanticNs summary))
    printStagePerFile "IR per file" (irTimings summary)
    printStagePerFile "x64 lowering per file" (x64LowerTimings summary)
    putStrLn ("[DEBUG] object emit: " ++ formatDurationMs (objectEmitNs summary))
    putStrLn ("[DEBUG] link: " ++ formatDurationMs (linkNs summary))
    let totalNs =
            sumStageTimings (tokenizeTimings summary)
                + sumStageTimings (parseTimings summary)
                + importLoadNs summary
                + semanticNs summary
                + sumStageTimings (irTimings summary)
                + sumStageTimings (x64LowerTimings summary)
                + objectEmitNs summary
                + linkNs summary
    putStrLn ("[DEBUG] total: " ++ formatDurationMs totalNs)


writePerClassIRFiles :: FilePath -> [TAC.IRProgm] -> IO ()
writePerClassIRFiles classesRoot irs = mapM_ writeOne (collectArtifacts irs)
  where
    collectArtifacts :: [TAC.IRProgm] -> [(FilePath, String)]
    collectArtifacts = concatMap artifactsFromProgm

    artifactsFromProgm :: TAC.IRProgm -> [(FilePath, String)]
    artifactsFromProgm (TAC.IRProgm pkgSegs classes) =
        map (artifactFromClass pkgSegs) classes

    artifactFromClass :: [String] -> TAC.IRClass -> (FilePath, String)
    artifactFromClass pkgSegs cls@(TAC.IRClass _ className _ _ _ _ _) =
        let pkgDir = foldl' (</>) classesRoot pkgSegs
            irPath = pkgDir </> className <.> "ir"
            pkgPrefix = case pkgSegs of
                [] -> ""
                _ -> "package " ++ intercalate "." pkgSegs ++ "\n\n"
            irText = pkgPrefix ++ TAC.prettyIRClass 0 cls
        in (irPath, irText)

    writeOne :: (FilePath, String) -> IO ()
    writeOne (path, text) = do
        createDirectoryIfMissing True (takeDirectory path)
        writeFile path text


collectSourceImports :: [(FilePath, String)] -> [String]
collectSourceImports sources =
    let programs = mapMaybeParse sources
        importsMap = AST.collectInputPrograms programs
    in sort . nub $
        "xlang.io.*" : [toPattern pkg cls |
                           (pkg, classSet) <- Map.toList importsMap,
                           cls <- HashSet.toList classSet]
  where
    mapMaybeParse :: [(FilePath, String)] -> [AST.Program]
    mapMaybeParse = foldr step []
      where
        step :: (FilePath, String) -> [AST.Program] -> [AST.Program]
        step (path, code) acc =
            let (lexErrs, tokens) = Tokenizer.tokenizeWithNL path code
            in if null lexErrs
                then
                    let prog = Parse.parseProgm tokens
                    in if null (AST.getErrorProgram prog) then prog : acc else acc
                else acc

    toPattern :: [String] -> String -> String
    toPattern pkg cls = intercalate "." (pkg ++ [cls])


data ClassAsmUnit = ClassAsmUnit {
    unitSourcePath :: FilePath,
    unitOwnerQName :: [String],
    unitAsmText :: String
}


data LinkKind
    = LinkExe
    | LinkDyn
    | LinkStatic


data OutputMode
    = OutputDir FilePath
    | OutputSingleObject FilePath
    | OutputLinked FilePath LinkKind FilePath


data X64CompilerChoice
    = X64CompilerNasm
    | X64CompilerAs
    deriving (Eq, Show)


xlangBuildVersion :: String
xlangBuildVersion = "alpha-1.1.0"


x64AsmBackendTag :: X64.CallConv64 -> String
x64AsmBackendTag cc = case X64.ccCompiler cc of
    X64.NASM -> "nasm"
    X64.GAS_INTEL -> "as-intel"
    X64.GAS_ATT -> "as-att"


x64IdentPayload :: X64.CallConv64 -> String
x64IdentPayload cc =
    concat [
        "xlang (",
        intercalate "/" ["x64", x64AsmBackendTag cc, os],
        ") ",
        xlangBuildVersion]


x64IdentDirective :: X64.CallConv64 -> String
x64IdentDirective cc = case X64.ccCompiler cc of
    X64.NASM -> concat ["; .ident \"", x64IdentPayload cc, "\"\n"]
    _ -> concat [".ident \"", x64IdentPayload cc, "\"\n"]


withX64IdentFooter :: X64.CallConv64 -> String -> String
withX64IdentFooter cc asmText =
    let base =
            if null asmText
                then ""
                else if last asmText == '\n' then asmText else asmText ++ "\n"
    in base ++ "\n" ++ x64IdentDirective cc

compileX64 ::
    Int ->
    FilePath ->
    FilePath ->
    [FilePath] ->
    [FilePath] ->
    Maybe FilePath ->
    Maybe [String] ->
    Maybe X64CompilerChoice ->
    Bool ->
    IO (Maybe [TAC.IRProgm])
compileX64 = compileX64WithAssembler True


compileX64WithAssembler ::
    Bool ->
    Int ->
    FilePath ->
    FilePath ->
    [FilePath] ->
    [FilePath] ->
    Maybe FilePath ->
    Maybe [String] ->
    Maybe X64CompilerChoice ->
    Bool ->
    IO (Maybe [TAC.IRProgm])
compileX64WithAssembler runAssembler jobs toolkitJar rootPath srcPaths libPaths mOutput mMainClassOverride mCompilerChoice debugOut = do
    sourceRes <- mapConcurrentlyLimit jobs
        (\path -> do
            fileRes <- FH.readFile path
            pure (path, fileRes))
        srcPaths

    let readErrs = [err | (_, Left err) <- sourceRes]
        sources = [(path, code) | (path, Right code) <- sourceRes]

    if not (null readErrs)
        then do
            mapM_ (putStrLn . UE.errorToString) readErrs
            pure Nothing
        else do
            let sourceImports = collectSourceImports sources
            (libsRes, libsTime) <- timedIO (LibLoader.loadLibEnvsWithJobs jobs toolkitJar libPaths sourceImports)
            case libsRes of
                Left errs -> do
                    mapM_ (putStrLn . UE.errorToString) errs
                    pure Nothing
                Right (depImportEnvs, depTypedEnvs) -> do
                    mIrRes <- IR.codeToIRWithRootAndDepsTimed depImportEnvs depTypedEnvs rootPath sources
                    case mIrRes of
                        Left errs -> do
                            mapM_ (putStrLn . UE.errorToString) errs
                            pure Nothing
                        Right (irPairs, warns, pipelineTimingRaw) -> do
                            mapM_ print warns
                            printPerFileProgress irPairs
                            let ccUsed = selectCallConv64 mCompilerChoice
                            lowered <- mapM
                                (\one@(path, _) -> do
                                    (u, t) <- timedIO $ do
                                        let us = lowerOneFile ccUsed one
                                        _ <- evaluate (sum (map (length . unitAsmText) us))
                                        pure us
                                    pure (path, u, t))
                                irPairs
                            let irs = map snd irPairs
                                baseUnits = concat [u | (_, u, _) <- lowered]
                                outMode = resolveOutputMode rootPath mOutput

                            case addExeEntryUnit64 ccUsed outMode mMainClassOverride irPairs baseUnits of
                                Left errMsg -> do
                                    putStrLn ("[ERROR] " ++ errMsg)
                                    pure Nothing
                                Right units ->
                                    case validateOutputMode outMode units of
                                        Left errMsg -> do
                                            putStrLn ("[ERROR] " ++ errMsg)
                                            pure Nothing
                                        Right () -> do
                                            (objPaths, emitTimeNs) <- timedIO $
                                                mapConcurrentlyLimit jobs (emitOne runAssembler debugOut outMode ccUsed rootPath) units
                                            linkTimeNs <- if runAssembler
                                                then snd <$> timedIO (linkIfNeeded debugOut rootPath outMode objPaths libPaths)
                                                else pure 0
                                            when debugOut $ do
                                                writePerClassIRFiles (debugIrRootFromOutputMode rootPath outMode) irs
                                                let pipelineTiming = CompileTimingSummary {
                                                        tokenizeTimings = [
                                                            StageTiming (IR.fePath t) (IR.feTokenizeNs t) |
                                                            t <- IR.frontendStageTimings pipelineTimingRaw
                                                        ],
                                                        parseTimings = [
                                                            StageTiming (IR.fePath t) (IR.feParseNs t) |
                                                            t <- IR.frontendStageTimings pipelineTimingRaw
                                                        ],
                                                        importLoadNs = libsTime,
                                                        semanticNs = IR.semanticStageNs pipelineTimingRaw,
                                                        irTimings = [
                                                            StageTiming (IR.irPath t) (IR.irLowerNs t) |
                                                            t <- IR.irLowerStageTimings pipelineTimingRaw
                                                        ],
                                                        x64LowerTimings = [
                                                            StageTiming path elapsed |
                                                            (path, _, elapsed) <- lowered
                                                        ],
                                                        objectEmitNs = emitTimeNs,
                                                        linkNs = linkTimeNs
                                                    }
                                                printCompileSummary pipelineTiming
                                            pure (Just irs)


printPerFileProgress :: [(FilePath, TAC.IRProgm)] -> IO ()
printPerFileProgress irPairs = do
    let total = length irPairs
    mapM_
        (\(idx, (path, _)) ->
            putStrLn (concat ["[", show idx, " / ", show total, "]: compile ", path, ","]))
        (zip [1 :: Int ..] irPairs)


lowerOneFile :: X64.CallConv64 -> (FilePath, TAC.IRProgm) -> [ClassAsmUnit]
lowerOneFile ccUsed (srcPath, TAC.IRProgm pkgSegs classes) = map (lowerOneClass srcPath pkgSegs) classes
  where
    lowerOneClass :: FilePath -> [String] -> TAC.IRClass -> ClassAsmUnit
    lowerOneClass path pkg cls@(TAC.IRClass _ className _ _ _ _ _) =
        let st0 = X64L.initClassState64 ccUsed cls
            (x64Class, refs) = evalState (X64L.x64LowingClass pkg cls) st0
            (staticData, segs) = x64Class
            ownerQName = pkg ++ [className]
            decls = classGlobalDecls64 pkg cls ++ classExternDecls64 ownerQName refs
        in ClassAsmUnit {
            unitSourcePath = path,
            unitOwnerQName = ownerQName,
            unitAsmText = X64.prettyX64ProgmIntel ccUsed (X64.X64Progm decls staticData segs)
        }


classGlobalDecls64 :: [String] -> TAC.IRClass -> [X64.X64Decl]
classGlobalDecls64 pkgSegs irCls@(TAC.IRClass _ className _ _ _ funs _) =
    let ownerQName = pkgSegs ++ [className]
        clinitGlobal = X64.Global (ownerQName ++ [X64.staticInitName]) [AST.Void]
        classModifiersGlobal = X64.GlobalClassModifiers ownerQName
        classParentsGlobal = X64.GlobalClassParents ownerQName
        pubFunSymbols = [
            (ownerQName ++ [funName], TEnv.funParams sig ++ [TEnv.funReturn sig])
            | TAC.IRFunction decl funName sig _ _ _ <- funs, isPublicDecl64 decl]
        wrappedMainSymbols = classWrappedMainSymbols64 pkgSegs irCls
        funSymbols = nub (pubFunSymbols ++ wrappedMainSymbols)
        funGlobals = map (uncurry X64.Global) funSymbols
        funModifierGlobals = map (uncurry X64.GlobalModifiers) funSymbols
    in classModifiersGlobal : classParentsGlobal : clinitGlobal : (funGlobals ++ funModifierGlobals)


isPublicDecl64 :: PB.Decl -> Bool
isPublicDecl64 (acc, _) = acc == PB.Public


classWrappedMainSymbols64 :: [String] -> TAC.IRClass -> [([String], [AST.Class])]
classWrappedMainSymbols64 pkgSegs (TAC.IRClass _ className _ _ _ funs _) = [
    (pkgSegs ++ [className, "main"], [retT]) |
    TAC.IRFunction _ "main" sig _ _ memberType <- funs,
    memberType == TAC.MemberClassWrapped,
    null (TEnv.funParams sig),
    let retT = TEnv.funReturn sig,
    retT `elem` [AST.Int32T, AST.Void]]


classExternDecls64 :: [String] -> [X64L.ExternRef] -> [X64.X64Decl]
classExternDecls64 ownerQName refs =
    X64L.collectExternDeclsFromRefs64 [ownerQName] refs


addExeEntryUnit64 ::
    X64.CallConv64 ->
    OutputMode ->
    Maybe [String] ->
    [(FilePath, TAC.IRProgm)] ->
    [ClassAsmUnit] ->
    Either String [ClassAsmUnit]
addExeEntryUnit64 cc outMode mMainClassOverride irPairs units = case outMode of
    OutputLinked _ LinkExe _ ->
        case mkExeEntryUnit64 cc mMainClassOverride irPairs of
            Left errMsg -> Left errMsg
            Right entryUnit -> Right (entryUnit : units)
    _ -> Right units


mkExeEntryUnit64 :: X64.CallConv64 -> Maybe [String] -> [(FilePath, TAC.IRProgm)] -> Either String ClassAsmUnit
mkExeEntryUnit64 cc mMainClassOverride irPairs = do
    (entryQn, entrySig) <- pickExeEntry64 mMainClassOverride irPairs
    let targetSym = X64.mangleQNameWithSig True entryQn entrySig
        asmText = mkExeEntryAsm64 cc targetSym
    Right ClassAsmUnit {
        unitSourcePath = "main.x64",
        unitOwnerQName = ["main"],
        unitAsmText = asmText}


pickExeEntry64 :: Maybe [String] -> [(FilePath, TAC.IRProgm)] -> Either String ([String], [AST.Class])
pickExeEntry64 mMainClassOverride irPairs =
    let entries = nub (concatMap gatherOne irPairs)
    in case mMainClassOverride of
        Nothing -> pickDefault entries
        Just mainQnRaw -> pickFromOverride mainQnRaw entries
  where
    pickDefault :: [([String], [AST.Class])] -> Either String ([String], [AST.Class])
    pickDefault entries = case entries of
        [] ->
            Left "x64 executable output needs one wrapped main function, but no wrapped main() was detected"
        _ ->
            pickByPolicy entries "x64 executable output found multiple wrapped main() candidates"

    pickFromOverride :: [String] -> [([String], [AST.Class])] -> Either String ([String], [AST.Class])
    pickFromOverride mainQnRaw entries =
        let mainClsQn = normalizeMainClassQName64 mainQnRaw
            exactMatches = filter (\(mainQn, _) -> normalizeMainClassQName64 mainQn == mainClsQn) entries
            pkgPrefix = case mainClsQn of
                [] -> []
                [_] -> []
                _ -> init mainClsQn
            pkgMatches = filter (\(mainQn, _) -> pkgPrefix `isPrefixOf` normalizeMainClassQName64 mainQn) entries
        in case exactMatches of
            [one] -> Right one
            many@(_:_:_) ->
                Left ("x64 executable --main matched multiple entries: " ++ intercalate ", " (map (intercalate "." . fst) many))
            [] -> case pkgMatches of
                [] ->
                    Left ("x64 executable --main did not match any wrapped main() in package/class: " ++ intercalate "." mainClsQn)
                _ ->
                    pickByPolicy pkgMatches "x64 executable --main package scan found multiple wrapped main() candidates"

    normalizeMainClassQName64 :: [String] -> [String]
    normalizeMainClassQName64 qn = case reverse qn of
        ("main" : revCls) | not (null revCls) -> reverse revCls
        _ -> qn

    pickByPolicy :: [([String], [AST.Class])] -> String -> Either String ([String], [AST.Class])
    pickByPolicy [] errPrefix = Left (errPrefix ++ ": <none>")
    pickByPolicy pool errPrefix =
        let withMeta = map (\cand@(qn, _) -> (cand, pkgLen qn, classRank qn)) pool
            minPkg = minimum (map (\(_, p, _) -> p) withMeta)
            minPkgSet = [cand | (cand, p, _) <- withMeta, p == minPkg]
            preferred = [cand | cand@(qn, _) <- minPkgSet, classRank qn <= 1]
            rankedPreferred = sortOn candKey preferred
            rankedAll = sortOn candKey minPkgSet
        in case rankedPreferred of
            (x:_) -> Right x
            [] -> case rankedAll of
                [x] -> Right x
                many ->
                    Left (errPrefix ++ ": " ++ intercalate ", " (map (intercalate "." . fst) many))

    pkgLen :: [String] -> Int
    pkgLen qn = max 0 (length (normalizeMainClassQName64 qn) - 1)

    classRank :: [String] -> Int
    classRank qn =
        let clsName = case reverse (normalizeMainClassQName64 qn) of
                (x:_) -> x
                [] -> ""
        in if clsName `elem` ["App", "AppX"]
            then 0
            else if clsName `elem` ["Main", "MainX", "main", "mainX"]
                then 1
                else 2

    candKey :: ([String], [AST.Class]) -> (Int, Int, String)
    candKey (qn, _) = (pkgLen qn, classRank qn, intercalate "." qn)

    gatherOne :: (FilePath, TAC.IRProgm) -> [([String], [AST.Class])]
    gatherOne (_, TAC.IRProgm pkgSegs classes) = concatMap (classWrappedMainSymbols64 pkgSegs) classes


mkExeEntryAsm64 :: X64.CallConv64 -> String -> String
mkExeEntryAsm64 cc targetSym = case X64.ccCompiler cc of
    X64.NASM -> unlines [
        "global main",
        "extern " ++ targetSym,
        "section .text",
        "main:",
        "    call " ++ targetSym,
        "    ret"]
    X64.GAS_INTEL -> unlines [
        ".intel_syntax noprefix",
        "",
        ".global main",
        ".extern " ++ targetSym,
        ".text",
        "main:",
        "    call " ++ targetSym,
        "    ret"]
    X64.GAS_ATT -> unlines [
        ".att_syntax prefix",
        "",
        ".global main",
        ".extern " ++ targetSym,
        ".text",
        "main:",
        "    call " ++ targetSym,
        "    ret"]


selectCallConv64 :: Maybe X64CompilerChoice -> X64.CallConv64
selectCallConv64 mCompilerChoice =
    let base = case os of
            "mingw32" -> X64.winCC64
            "darwin" -> X64.macCC64
            _ -> X64.linuxCC64
    in case mCompilerChoice of
        Nothing -> base
        Just X64CompilerNasm -> base { X64.ccCompiler = X64.NASM }
        Just X64CompilerAs -> base { X64.ccCompiler = X64.GAS_INTEL }


resolveOutputMode :: FilePath -> Maybe FilePath -> OutputMode
resolveOutputMode rootPath mOutput = case mOutput of
    Nothing -> OutputDir (rootPath </> "out")
    Just outRaw ->
        let outPath = CJ.resolveFromRoot rootPath outRaw
            ext = map toLower (takeExtension outPath)
            objRoot = takeDirectory outPath </> (takeBaseName outPath ++ ".objs")
        in case ext of
            ".o" -> OutputSingleObject outPath
            ".obj" -> OutputSingleObject outPath
            ".exe" -> OutputLinked outPath LinkExe objRoot
            ".out" -> OutputLinked outPath LinkExe objRoot
            ".dll" -> OutputLinked outPath LinkDyn objRoot
            ".so" -> OutputLinked outPath LinkDyn objRoot
            ".dylib" -> OutputLinked outPath LinkDyn objRoot
            ".a" -> OutputLinked outPath LinkStatic objRoot
            ".lib" -> OutputLinked outPath LinkStatic objRoot
            _ -> OutputDir outPath


validateOutputMode :: OutputMode -> [ClassAsmUnit] -> Either String ()
validateOutputMode (OutputSingleObject _) units
    | length units /= 1 =
        Left ("single .o output requires exactly one lowered class, but got " ++ show (length units))
validateOutputMode _ _ = Right ()


emitOne :: Bool -> Bool -> OutputMode -> X64.CallConv64 -> FilePath -> ClassAsmUnit -> IO FilePath
emitOne runAssembler debugOut outMode cc rootPath unit = do
    let asmPath = unitAsmPath outMode unit
        objPath = unitObjPath outMode unit
        asmText = withX64IdentFooter cc (unitAsmText unit)
    createDirectoryIfMissing True (takeDirectory objPath)
    if debugOut
        then do
            createDirectoryIfMissing True (takeDirectory asmPath)
            writeFile asmPath asmText
            when runAssembler (assembleOne rootPath cc asmPath objPath)
            pure objPath
        else do
            when runAssembler $ withTempAsmFile asmText (\tmpAsm -> assembleOne rootPath cc tmpAsm objPath)
            pure objPath


withTempAsmFile :: String -> (FilePath -> IO a) -> IO a
withTempAsmFile asmText action = do
    tmpDir <- getTemporaryDirectory
    (tmpPath, h) <- openTempFile tmpDir "xlang-x64-"
    hClose h
    let asmPath = replaceExtension tmpPath ".asm"
    writeFile asmPath asmText
    out <- action asmPath
    removeFile asmPath `catchIOError` (\_ -> pure ())
    pure out


nasmObjectFormat :: String
nasmObjectFormat = case os of
    "mingw32" -> "win64"
    "darwin" -> "macho64"
    _ -> "elf64"


assembleOne :: FilePath -> X64.CallConv64 -> FilePath -> FilePath -> IO ()
assembleOne rootPath cc asmPath objPath = case X64.ccCompiler cc of
    X64.NASM -> do
        nasmCmd <- resolveToolPath64 rootPath "nasm"
        callProcess nasmCmd ["-f", nasmObjectFormat, asmPath, "-o", objPath]
    _ -> do
        asCmd <- resolveToolPath64 rootPath "as"
        callProcess asCmd [asmPath, "-o", objPath]


linkIfNeeded :: Bool -> FilePath -> OutputMode -> [FilePath] -> [FilePath] -> IO ()
linkIfNeeded debugOut rootPath outMode objPaths linkLibPaths = case outMode of
    OutputLinked outPath linkKind _ -> do
        createDirectoryIfMissing True (takeDirectory outPath)
        case linkKind of
            LinkStatic -> do
                arCmd <- resolveToolPath64 rootPath "ar"
                callProcess arCmd (["rcs", outPath] ++ objPaths)
            LinkDyn ->
                linkDynWithLd outPath objPaths linkLibPaths >> cleanupLegacyOutIfNeeded debugOut outPath
            LinkExe ->
                linkExeWithLd outPath objPaths linkLibPaths
    _ -> pure ()
  where
    linkDynWithLd :: FilePath -> [FilePath] -> [FilePath] -> IO ()
    linkDynWithLd target objs libs = do
        sysLibs <- collectNativeStaticLibs
        case os of
            "mingw32" -> do
                let implib = replaceExtension target ".dll.a"
                    args = ["--dll", "-o", target] ++ objs ++ libs ++ sysLibs ++ ["--out-implib", implib]
                callProcessCandidates rootPath [("x86_64-w64-mingw32-ld", args), ("ld", args)]
            "darwin" ->
                callProcessCandidates rootPath [("ld", ["-dylib", "-o", target] ++ objs ++ libs ++ sysLibs)]
            _ ->
                callProcessCandidates rootPath [("ld", ["-shared", "-o", target] ++ objs ++ libs ++ sysLibs)]

    linkExeWithLd :: FilePath -> [FilePath] -> [FilePath] -> IO ()
    linkExeWithLd target objs libs = do
        sysLibs <- collectNativeStaticLibs
        case os of
            "mingw32" -> do
                let args = ["-e", "main", "-o", target] ++ objs ++ libs
                        ++ sysLibs
                callProcessCandidates rootPath [("x86_64-w64-mingw32-ld", args), ("ld", args)]
            _ ->
                callProcessCandidates rootPath [("ld", ["-e", "main", "-o", target] ++ objs ++ libs ++ sysLibs)]

    collectNativeStaticLibs :: IO [FilePath]
    collectNativeStaticLibs = do
        cwd <- getCurrentDirectory
        exeDir <- takeDirectory <$> getExecutablePath
        let syslibDirs = nub [
                exeDir </> "native",
                rootPath </> "native",
                cwd </> "native",
                exeDir </> "native-libs",
                rootPath </> "native-libs",
                cwd </> "native-libs"
                ]
            stdNativeDirs = nub [
                exeDir </> "libs" </> "native",
                rootPath </> "libs" </> "native",
                cwd </> "libs" </> "native",
                exeDir </> "std" </> "native",
                rootPath </> "std" </> "native",
                cwd </> "std" </> "native"
                ]
        grouped <- mapM collectOne syslibDirs
        stdLibs <- collectStdNativeLibs stdNativeDirs
        pure (sort (nub (concat grouped ++ stdLibs)))
      where
        collectOne :: FilePath -> IO [FilePath]
        collectOne dir = do
            exists <- doesDirectoryExist dir
            if not exists
                then pure []
                else do
                    names <- listDirectory dir
                    pure [dir </> n | n <- sort names, map toLower (takeExtension n) == ".a"]

        collectStdNativeLibs :: [FilePath] -> IO [FilePath]
        collectStdNativeLibs dirs = do
            let names = ["libxlang-base.a", "libxlang-std.a"]
                paths = [dir </> name | dir <- dirs, name <- names]
            existing <- mapM doesFileExist paths
            pure [p | (p, True) <- zip paths existing]

    callProcessCandidates :: FilePath -> [(FilePath, [String])] -> IO ()
    callProcessCandidates _ [] = ioError (userError "no usable GNU linker found (tried ld variants)")
    callProcessCandidates root ((cmd, args) : rest) = do
        resolvedCmd <- resolveToolPath64 root cmd
        catchIOError
            (callProcess resolvedCmd args)
            (\e -> if isDoesNotExistError e then callProcessCandidates root rest else ioError e)

    cleanupLegacyOutIfNeeded :: Bool -> FilePath -> IO ()
    cleanupLegacyOutIfNeeded keepDebug linkedOut =
        unless keepDebug $ do
            let legacySameBase = replaceExtension linkedOut ".out"
            removeIfExists "a.out"
            removeIfExists legacySameBase

    removeIfExists :: FilePath -> IO ()
    removeIfExists p = do
        ex <- doesFileExist p
        when ex (removeFile p)


toolNameCandidates64 :: String -> [String]
toolNameCandidates64 tool
    | os == "mingw32" = [tool ++ ".exe", tool]
    | otherwise = [tool]


resolveToolPath64 :: FilePath -> String -> IO FilePath
resolveToolPath64 rootPath tool = do
    let localDir = rootPath </> "tools"
        localCandidates = [localDir </> name | name <- toolNameCandidates64 tool]
    mLocal <- firstExistingFile localCandidates
    case mLocal of
        Just localPath -> pure localPath
        Nothing -> do
            mGlobal <- firstExecutable (toolNameCandidates64 tool)
            pure (maybe tool id mGlobal)
  where
    firstExistingFile :: [FilePath] -> IO (Maybe FilePath)
    firstExistingFile [] = pure Nothing
    firstExistingFile (p : rest) = do
        exists <- doesFileExist p
        if exists then pure (Just p) else firstExistingFile rest

    firstExecutable :: [String] -> IO (Maybe FilePath)
    firstExecutable [] = pure Nothing
    firstExecutable (name : rest) = do
        mPath <- findExecutable name
        case mPath of
            Just p -> pure (Just p)
            Nothing -> firstExecutable rest


unitAsmPath :: OutputMode -> ClassAsmUnit -> FilePath
unitAsmPath outMode unit = case outMode of
    OutputSingleObject objPath -> replaceExtension objPath ".asm"
    OutputDir outDir ->
        let rel = ownerQNameToRelPath (unitOwnerQName unit) (takeBaseName (unitSourcePath unit))
        in outDir </> rel <.> "asm"
    OutputLinked _ _ objRoot ->
        let rel = ownerQNameToRelPath (unitOwnerQName unit) (takeBaseName (unitSourcePath unit))
        in objRoot </> rel <.> "asm"


unitObjPath :: OutputMode -> ClassAsmUnit -> FilePath
unitObjPath outMode unit = case outMode of
    OutputSingleObject objPath -> objPath
    OutputDir outDir ->
        let rel = ownerQNameToRelPath (unitOwnerQName unit) (takeBaseName (unitSourcePath unit))
        in outDir </> rel <.> "o"
    OutputLinked _ _ objRoot ->
        let rel = ownerQNameToRelPath (unitOwnerQName unit) (takeBaseName (unitSourcePath unit))
        in objRoot </> rel <.> "o"


ownerQNameToRelPath :: [String] -> String -> FilePath
ownerQNameToRelPath qn fallbackBase =
    case dropWhileEnd null qn of
        [] -> fallbackBase
        xs -> foldl' (</>) (head xs) (tail xs)


debugIrRootFromOutputMode :: FilePath -> OutputMode -> FilePath
debugIrRootFromOutputMode rootPath outMode = case outMode of
    OutputSingleObject objPath ->
        let dir = takeDirectory objPath
        in if null dir then rootPath else dir
    OutputDir outDir -> outDir
    OutputLinked _ _ objRoot -> objRoot
