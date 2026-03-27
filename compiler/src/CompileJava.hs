module CompileJava (
    resolveFromRoot,
    isJarOutput,
    invalidSourceFiles,
    invalidLibFiles,
    duplicateLibRefs,
    findDefaultLibJars,
    findDefaultNativeJsons,
    isDefaultStdlibJar,
    hasJavaImportPrefix,
    isJavaNativeMetadataPath,
    sourceNeedsJavaMetadata,
    compileJVM,
    compileJVMToJar
) where

import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (bracket_, evaluate, finally)
import Control.Monad (when)
import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Char (toLower)
import Data.List (foldl', intercalate, sort, sortOn, nub, isPrefixOf)
import Data.Maybe (mapMaybe)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (lookupEnv)
import System.Directory (
    copyFile,
    createDirectoryIfMissing,
    doesFileExist,
    doesDirectoryExist,
    listDirectory,
    removeFile,
    removePathForcibly,
    withCurrentDirectory
    )
import System.FilePath ((</>), isAbsolute, makeRelative, normalise, splitDirectories, takeDirectory, takeExtension, takeFileName)
import System.IO.Error (catchIOError)
import Text.Printf (printf)
import System.Process (callProcess)

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import qualified IR.Lowing as IR
import qualified IR.TAC as TAC
import qualified Lex.Tokenizer as Tokenizer
import qualified Lowing.JVM as JVM
import qualified Lowing.JVMLowing as JVML
import qualified Lowing.JVMJson as JVMJson
import qualified Parse.ParseProgm as Parse
import qualified Parse.SyntaxTree as AST
import qualified Semantic.LibLoader as LibLoader
import qualified Util.Exception as UE
import qualified Util.FileHelper as FH


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
    jvmLowerTimings :: [StageTiming],
    bytecodeEmitNs :: Word64
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


printCompileSummary :: CompileTimingSummary -> Word64 -> IO ()
printCompileSummary summary jarPackageNs = do
    printStagePerFile "tokenize per file" (tokenizeTimings summary)
    printStagePerFile "parse per file" (parseTimings summary)
    putStrLn ("[DEBUG] import load: " ++ formatDurationMs (importLoadNs summary))
    putStrLn ("[DEBUG] semantic total: " ++ formatDurationMs (semanticNs summary))
    printStagePerFile "IR per file" (irTimings summary)
    printStagePerFile "JVM lowering per file" (jvmLowerTimings summary)
    putStrLn ("[DEBUG] bytecode generate: " ++ formatDurationMs (bytecodeEmitNs summary))
    putStrLn ("[DEBUG] jar package: " ++ formatDurationMs jarPackageNs)
    let totalNs =
            sumStageTimings (tokenizeTimings summary)
                + sumStageTimings (parseTimings summary)
                + importLoadNs summary
                + semanticNs summary
                + sumStageTimings (irTimings summary)
                + sumStageTimings (jvmLowerTimings summary)
                + bytecodeEmitNs summary
                + jarPackageNs
    putStrLn ("[DEBUG] total: " ++ formatDurationMs totalNs)


forceJClasses :: [JVM.JClass] -> Int
forceJClasses = sum . map forceJClass
  where
    forceJClass (JVM.JClass decl clsName superName interfaces fields (JVM.JClinit clinitOps) inits methods mainKind) =
        length (show decl)
            + forceQName clsName
            + forceQName superName
            + sum (map forceQName interfaces)
            + sum (map forceJField fields)
            + sum (map forceJCommand clinitOps)
            + sum (map forceJInit inits)
            + sum (map forceJFunction methods)
            + length (show mainKind)

    forceJField (JVM.JField decl cls name ownerType) =
        length (show decl)
            + length (show cls)
            + length name
            + length ownerType

    forceJInit (JVM.JInit decl sig commands) =
        length (show decl)
            + length (show sig)
            + sum (map forceJCommand commands)

    forceJFunction (JVM.JFunction decl name sig ownerType commands) =
        length (show decl)
            + length name
            + length (show sig)
            + length ownerType
            + sum (map forceJCommand commands)

    forceJCommand (JVM.OP op) = 1 + length (show op)
    forceJCommand (JVM.Label (labelId, commands)) = labelId + sum (map forceJCommand commands)

    forceQName :: [String] -> Int
    forceQName = sum . map length


resolveFromRoot :: FilePath -> FilePath -> FilePath
resolveFromRoot rootDir path
    | isAbsolute path = path
    | otherwise = rootDir </> path


isJarOutput :: FilePath -> Bool
isJarOutput outPath = map toLower (takeExtension outPath) == ".jar"


allowedSourceExtensions :: [String]
allowedSourceExtensions = [".x", ".xl", ".xlang"]


isAllowedSourceFile :: FilePath -> Bool
isAllowedSourceFile path = map toLower (takeExtension path) `elem` allowedSourceExtensions


invalidSourceFiles :: [FilePath] -> [FilePath]
invalidSourceFiles = filter (not . isAllowedSourceFile)


allowedLibExtensions :: [String]
allowedLibExtensions = [".class", ".jar", ".json", ".db", ".jmod"]


isAllowedLibFile :: FilePath -> Bool
isAllowedLibFile path = map toLower (takeExtension path) `elem` allowedLibExtensions


invalidLibFiles :: [FilePath] -> [FilePath]
invalidLibFiles = filter (not . isAllowedLibFile)


libRefKey :: FilePath -> String
libRefKey = map toLower . normalise


duplicateLibRefs :: [FilePath] -> [FilePath]
duplicateLibRefs paths =
    let buckets = foldl' insertOne Map.empty paths
    in sort [head refs | refs <- Map.elems buckets, length refs > 1]
  where
    insertOne mp ref = Map.insertWith (++) (libRefKey ref) [ref] mp


normalizeMainClassQName :: [String] -> [String]
normalizeMainClassQName qn = case reverse qn of
    ("main" : revCls) | not (null revCls) -> reverse revCls
    _ -> qn


mainQNameFromKind :: TAC.MainKind -> Maybe [String]
mainQNameFromKind kind = case kind of
    TAC.NoMain -> Nothing
    TAC.MainInt qn -> Just (normalizeMainClassQName qn)
    TAC.MainVoid qn -> Just (normalizeMainClassQName qn)
    TAC.MainIntArgs qn -> Just (normalizeMainClassQName qn)
    TAC.MainVoidArgs qn -> Just (normalizeMainClassQName qn)


isWrappedMainFunction :: TAC.IRFunction -> Bool
isWrappedMainFunction (TAC.IRFunction _ fname _ _ _ ownerType) =
    fname == "main" && ownerType == TAC.MemberClassWrapped


selectJarMainClass :: [TAC.IRProgm] -> Maybe [String]
selectJarMainClass irs = case sortOn candidateKey candidates of
    [] -> Nothing
    (qn:_) -> Just qn
  where
    candidates :: [[String]]
    candidates = concatMap progmCandidates irs

    progmCandidates :: TAC.IRProgm -> [[String]]
    progmCandidates (TAC.IRProgm _ classes) = mapMaybe classCandidate classes

    classCandidate :: TAC.IRClass -> Maybe [String]
    classCandidate (TAC.IRClass _ _ _ _ _ funs mainKind) =
        case mainQNameFromKind mainKind of
            Just qn | any isWrappedMainFunction funs -> Just qn
            _ -> Nothing

    candidateKey :: [String] -> (Int, Int, String)
    candidateKey qn =
        let clsName = case reverse qn of
                (x:_) -> x
                [] -> ""
            rank
                | clsName == "MainX" = 0
                | clsName == "AppX" = 1
                | otherwise = 2
            pathText = intercalate "." qn
        in (rank, length pathText, pathText)


findDefaultLibJars :: FilePath -> IO [FilePath]
findDefaultLibJars exeDir = do
    let libsDir = exeDir </> "libs"
    exists <- doesDirectoryExist libsDir
    if not exists
        then pure []
        else do
            jars <- collect libsDir
            pure (sort (filter isDefaultStdlibJar jars))
  where
    collect :: FilePath -> IO [FilePath]
    collect dir = do
        names <- listDirectory dir
        concat <$> mapM
            (\name -> do
                let path = dir </> name
                isDir <- doesDirectoryExist path
                if isDir
                    then collect path
                    else pure [path | map toLower (takeExtension path) == ".jar"])
            names


isDefaultStdlibJar :: FilePath -> Bool
isDefaultStdlibJar path =
    map toLower (takeFileName path) == "xlang-stdlib-alpha.jar"


findDefaultNativeJsons :: FilePath -> Int -> IO [FilePath]
findDefaultNativeJsons exeDir targetJvm = do
    let nativeDir = exeDir </> "libs" </> "java-native"
        versionDir = nativeDir </> ("jdk-" ++ show targetJvm)
    versionedExists <- doesDirectoryExist versionDir
    if versionedExists
        then sort <$> collect versionDir
        else do
            nativeExists <- doesDirectoryExist nativeDir
            if not nativeExists
                then pure []
                else sort <$> collect nativeDir
  where
    collect :: FilePath -> IO [FilePath]
    collect dir = do
        names <- listDirectory dir
        sort . concat <$> mapM
            (\name -> do
                let path = dir </> name
                isDir <- doesDirectoryExist path
                if isDir
                    then collect path
                    else pure [path |
                        let ext = map toLower (takeExtension path)
                        in ext `elem` allowedLibExtensions])
            names

collectSourceImports :: [(FilePath, String)] -> [String]
collectSourceImports sources =
    let programs = mapMaybe parseProgram sources
        importsMap = AST.collectInputPrograms programs
    in sort . nub $
        defaultImportPatterns ++
        [toPattern pkg cls |
            (pkg, classSet) <- Map.toList importsMap,
            cls <- HashSet.toList classSet]
  where
    defaultImportPatterns :: [String]
    defaultImportPatterns = ["xlang.io.*"]

    parseProgram :: (FilePath, String) -> Maybe AST.Program
    parseProgram (path, code) =
        let (lexErrs, tokens) = Tokenizer.tokenizeWithNL path code
        in if null lexErrs
            then
                let prog = Parse.parseProgm tokens
                in if null (AST.getErrorProgram prog) then Just prog else Nothing
            else Nothing

    toPattern :: [String] -> String -> String
    toPattern pkg cls = intercalate "." (pkg ++ [cls])


sourceNeedsJavaMetadata :: [FilePath] -> IO Bool
sourceNeedsJavaMetadata srcPaths = do
    loaded <- mapM
        (\path -> do
            res <- FH.readFile path
            pure (path, res))
        srcPaths
    let sources = [(path, code) | (path, Right code) <- loaded]
        imports = collectSourceImports sources
    pure (hasJavaImportPrefix imports)


hasJavaImportPrefix :: [String] -> Bool
hasJavaImportPrefix =
    any (\imp -> "java." `isPrefixOf` map toLower imp)


resolveJavaExe :: IO FilePath
resolveJavaExe = do
    mJavaHome <- lookupEnv "JAVA_HOME"
    case mJavaHome of
        Nothing -> pure "java"
        Just javaHome -> do
            let javaExe = javaHome </> "bin" </> "java.exe"
                javaBin = javaHome </> "bin" </> "java"
            hasExe <- doesFileExist javaExe
            if hasExe
                then pure javaExe
                else do
                    hasBin <- doesFileExist javaBin
                    if hasBin then pure javaBin else pure "java"


isJavaNativeMetadataPath :: FilePath -> Bool
isJavaNativeMetadataPath path =
    hasJavaNativeSegments (map (map toLower) (splitDirectories (normalise path)))
  where
    hasJavaNativeSegments :: [FilePath] -> Bool
    hasJavaNativeSegments ("libs":"java-native":_) = True
    hasJavaNativeSegments (_:rest) = hasJavaNativeSegments rest
    hasJavaNativeSegments [] = False


compileJVMCore :: Int -> Int -> FilePath -> FilePath -> [FilePath] -> [FilePath] -> Maybe FilePath -> Bool -> IO (Maybe ([TAC.IRProgm], CompileTimingSummary))
compileJVMCore jobs targetJvm toolkitJar rootPath srcPaths libPaths mOutput debugOut = do
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
                effectiveLibPaths
                    | hasJavaImportPrefix sourceImports = libPaths
                    | otherwise = filter (not . isJavaNativeMetadataPath) libPaths
            (libsRes, libsTime) <- timedIO (LibLoader.loadLibEnvsWithJobs jobs toolkitJar effectiveLibPaths sourceImports)
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
                        Right (irPairs, warns, pipelineTiming) -> do
                            mapM_ print warns

                            let total = length irPairs
                            mapM_
                                (\(idx, (path, _)) ->
                                    putStrLn (concat ["[", show idx, " / ", show total, "]: compile ", path, ","])
                                )
                                (zip [1 :: Int ..] irPairs)

                            lowered <- mapConcurrentlyLimit jobs
                                (\(path, ir) -> do
                                    (jClasses, elapsed) <- timedIO $ do
                                        let classes = JVML.jvmProgmLowing ir
                                        _ <- evaluate (forceJClasses classes)
                                        pure classes
                                    pure (path, jClasses, elapsed))
                                irPairs

                            let irs = map snd irPairs
                                classes = concat [jClasses | (_, jClasses, _) <- lowered]
                                jsonVal = JVMJson.jProgmToJSON targetJvm classes
                                jsonStr = BL.unpack (encode jsonVal)
                                debugDir = case srcPaths of
                                    [one] -> takeDirectory one
                                    _ -> rootPath
                                jobsArgs = ["--jobs", show jobs]

                            when debugOut $ BL.writeFile (debugDir </> "debug.json") (encodePretty jsonVal)
                            when debugOut $ writeFile (debugDir </> "Ir.txt") (unlines (map TAC.prettyIRProgm irs))

                            javaExe <- resolveJavaExe
                            (_, emitTimeNs) <- timedIO $
                                case mOutput of
                                    Just outPath -> do
                                        createDirectoryIfMissing True outPath
                                        callProcess javaExe (["-jar", toolkitJar, "-s", jsonStr] ++ jobsArgs ++ ["-o", outPath])
                                    Nothing ->
                                        callProcess javaExe (["-jar", toolkitJar, "-s", jsonStr] ++ jobsArgs)

                            let summary = CompileTimingSummary {
                                    tokenizeTimings = [
                                        StageTiming (IR.fePath t) (IR.feTokenizeNs t) |
                                        t <- IR.frontendStageTimings pipelineTiming
                                    ],
                                    parseTimings = [
                                        StageTiming (IR.fePath t) (IR.feParseNs t) |
                                        t <- IR.frontendStageTimings pipelineTiming
                                    ],
                                    importLoadNs = libsTime,
                                    semanticNs = IR.semanticStageNs pipelineTiming,
                                    irTimings = [
                                        StageTiming (IR.irPath t) (IR.irLowerNs t) |
                                        t <- IR.irLowerStageTimings pipelineTiming
                                    ],
                                    jvmLowerTimings = [
                                        StageTiming path elapsed |
                                        (path, _, elapsed) <- lowered
                                    ],
                                    bytecodeEmitNs = emitTimeNs
                                }
                            pure (Just (irs, summary))


compileJVM :: Int -> Int -> FilePath -> FilePath -> [FilePath] -> [FilePath] -> Maybe FilePath -> Bool -> IO (Maybe [TAC.IRProgm])
compileJVM jobs targetJvm toolkitJar rootPath srcPaths libPaths mOutput debugOut = do
    mRes <- compileJVMCore jobs targetJvm toolkitJar rootPath srcPaths libPaths mOutput debugOut
    case mRes of
        Just (irs, summary) -> do
            when debugOut (printCompileSummary summary 0)
            pure (Just irs)
        Nothing -> pure Nothing


writeJarFromDir :: FilePath -> FilePath -> Maybe [String] -> IO ()
writeJarFromDir classesDir jarOutput mMainClass = do
    let jarDir = takeDirectory jarOutput
        manifestPath = jarDir </> ".xlang-manifest.mf"

    createDirectoryIfMissing True jarDir
    writeFile manifestPath (renderManifest mMainClass)
    callProcess "jar" ["cfm", jarOutput, manifestPath, "-C", classesDir, "."]
    removeFile manifestPath `catchIOError` (\_ -> pure ())


renderManifest :: Maybe [String] -> String
renderManifest mMainClass =
    let mainClassLines = case mMainClass of
            Just qn -> ["Main-Class: " ++ intercalate "." (normalizeMainClassQName qn)]
            Nothing -> []
    in unlines (
        ["Manifest-Version: 1.0", "Built-By: xlang", "Xlang-Info: build by xlang"]
        ++ mainClassLines
        ++ [""])

isOutsideRootRelative :: FilePath -> Bool
isOutsideRootRelative relPath =
    ".." `elem` filter (not . null) (splitDirectories (normalise relPath))


mergeLibsIntoJar :: FilePath -> [FilePath] -> FilePath -> IO ()
mergeLibsIntoJar rootPath libPaths jarOutput = do
    let jarDir = takeDirectory jarOutput
        mergeDir = jarDir </> ".xlang-runtime-merge"

    existed <- doesDirectoryExist mergeDir
    when existed (removePathForcibly mergeDir)
    createDirectoryIfMissing True mergeDir

    mapM_ (stageOneLib mergeDir) libPaths
    stripMergedManifest mergeDir
    callProcess "jar" ["uf", jarOutput, "-C", mergeDir, "."]
    removePathForcibly mergeDir `catchIOError` (\_ -> pure ())
  where
    stripMergedManifest :: FilePath -> IO ()
    stripMergedManifest mergeDir = do
        let manifestPath = mergeDir </> "META-INF" </> "MANIFEST.MF"
        hasManifest <- doesFileExist manifestPath
        when hasManifest (removeFile manifestPath)

    stageOneLib :: FilePath -> FilePath -> IO ()
    stageOneLib mergeDir libPath =
        case map toLower (takeExtension libPath) of
            ".jar" -> withCurrentDirectory mergeDir (callProcess "jar" ["xf", libPath])
            ".class" -> do
                let rel0 = normalise (makeRelative rootPath libPath)
                    relPath = if isOutsideRootRelative rel0 then takeFileName libPath else rel0
                    dstPath = mergeDir </> relPath
                createDirectoryIfMissing True (takeDirectory dstPath)
                copyFile libPath dstPath
            _ -> pure ()


compileJVMToJar :: Int -> Int -> FilePath -> FilePath -> [FilePath] -> [FilePath] -> FilePath -> Bool -> Bool -> Maybe [String] -> IO ()
compileJVMToJar jobs targetJvm toolkitJar rootPath srcPaths libPaths jarOutput includeRuntime debugOut mMainClassOverride = do
    let classesOut = rootPath </> "out"
        cleanupOut = do
            exists <- doesDirectoryExist classesOut
            when exists (removePathForcibly classesOut `catchIOError` (\_ -> pure ()))

    cleanupOut
    createDirectoryIfMissing True classesOut
    putStrLn ("[INFO] -d is jar; classes output dir: " ++ classesOut)

    (`finally` cleanupOut) $ do
        mRes <- compileJVMCore jobs targetJvm toolkitJar rootPath srcPaths libPaths (Just classesOut) debugOut
        case mRes of
            Just (irs, summary) -> do
                let mMainClassAuto = selectJarMainClass irs
                    mMainClass = case mMainClassOverride of
                        Just qn -> Just (normalizeMainClassQName qn)
                        Nothing -> mMainClassAuto
                (_, jarPackTimeNs) <- timedIO (writeJarFromDir classesOut jarOutput mMainClass)
                case mMainClassOverride of
                    Just qn -> putStrLn ("[INFO] manifest Main-Class (override): " ++ intercalate "." (normalizeMainClassQName qn))
                    Nothing ->
                        case mMainClass of
                            Just qn -> putStrLn ("[INFO] manifest Main-Class: " ++ intercalate "." qn)
                            Nothing -> putStrLn "[WARN] no wrapped-class main found; jar has no Main-Class"
                mergeTimeNs <- if includeRuntime
                    then do
                        putStrLn ("[INFO] include-runtime: merging " ++ show (length libPaths) ++ " lib(s) into jar")
                        snd <$> timedIO (mergeLibsIntoJar rootPath libPaths jarOutput)
                    else pure 0
                when debugOut $
                    printCompileSummary summary (jarPackTimeNs + mergeTimeNs)
                putStrLn ("[DONE] jar generated: " ++ jarOutput)
            Nothing ->
                putStrLn "[ERROR] xlang compile failed; jar packaging skipped"
