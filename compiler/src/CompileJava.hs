module CompileJava (
    resolveFromRoot,
    isJarOutput,
    invalidSourceFiles,
    invalidLibFiles,
    duplicateLibRefs,
    findDefaultLibJars,
    compileJVM,
    compileJVMToJar
) where

import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (bracket_)
import Control.Monad (when)
import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Char (toLower)
import Data.List (foldl', intercalate, sort, sortOn)
import Data.Maybe (mapMaybe)
import System.Directory (
    copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    listDirectory,
    removeFile,
    removePathForcibly,
    withCurrentDirectory
    )
import System.FilePath ((</>), isAbsolute, makeRelative, normalise, splitDirectories, takeDirectory, takeExtension, takeFileName)
import System.IO.Error (catchIOError)
import System.Process (callProcess)

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as Map
import qualified IR.Lowing as IR
import qualified IR.TAC as TAC
import qualified Lowing.JVMLowing as JVML
import qualified Lowing.JVMJson as JVMJson
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
allowedLibExtensions = [".class", ".jar"]


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


mainQNameFromKind :: TAC.MainKind -> Maybe [String]
mainQNameFromKind kind = case kind of
    TAC.NoMain -> Nothing
    TAC.MainInt qn -> Just qn
    TAC.MainVoid qn -> Just qn
    TAC.MainIntArgs qn -> Just qn
    TAC.MainVoidArgs qn -> Just qn


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
        else sort <$> collect libsDir
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


compileJVM :: Int -> FilePath -> FilePath -> [FilePath] -> [FilePath] -> Maybe FilePath -> Bool -> IO (Maybe [TAC.IRProgm])
compileJVM jobs toolkitJar rootPath srcPaths libPaths mOutput debugOut = do
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
        else
            do
                libsRes <- LibLoader.loadLibEnvsWithJobs jobs toolkitJar libPaths
                case libsRes of
                    Left errs -> do
                        mapM_ (putStrLn . UE.errorToString) errs
                        pure Nothing
                    Right (depImportEnvs, depTypedEnvs) ->
                        case IR.codeToIRWithRootAndDeps depImportEnvs depTypedEnvs rootPath sources of
                            Left errs -> do
                                mapM_ (putStrLn . UE.errorToString) errs
                                pure Nothing
                            Right (irPairs, warns) -> do
                                mapM_ print warns

                                let total = length irPairs
                                mapM_
                                    (\(idx, (path, _)) ->
                                        putStrLn (concat ["[ ", show idx, "/", show total, " ]: compile ", path, ","])
                                    )
                                    (zip [1 :: Int ..] irPairs)

                                lowered <- mapConcurrentlyLimit jobs
                                    (\(_, ir) -> pure (JVML.jvmProgmLowing ir))
                                    irPairs

                                let irs = map snd irPairs
                                    classes = concat lowered
                                    jsonVal = JVMJson.jProgmToJSON 8 classes
                                    jsonStr = BL.unpack (encode jsonVal)
                                    debugDir = case srcPaths of
                                        [one] -> takeDirectory one
                                        _ -> rootPath
                                    jobsArgs = ["--jobs", show jobs]

                                when debugOut $ BL.writeFile (debugDir </> "debug.json") (encodePretty jsonVal)
                                when debugOut $ writeFile (debugDir </> "Ir.txt") (unlines (map TAC.prettyIRProgm irs))

                                case mOutput of
                                    Just outPath -> do
                                        createDirectoryIfMissing True outPath
                                        callProcess "java" (["-jar", toolkitJar, "-s", jsonStr] ++ jobsArgs ++ ["-o", outPath])
                                    Nothing ->
                                        callProcess "java" (["-jar", toolkitJar, "-s", jsonStr] ++ jobsArgs)

                                pure (Just irs)


writeJarFromDir :: FilePath -> FilePath -> Maybe [String] -> IO ()
writeJarFromDir classesDir jarOutput mMainClass = do
    let jarDir = takeDirectory jarOutput
        manifestPath = jarDir </> ".xlang-manifest.mf"
        mainClassLines = case mMainClass of
            Just qn -> ["Main-Class: " ++ intercalate "." qn]
            Nothing -> []
        manifestContent = unlines (
            ["Manifest-Version: 1.0", "Built-By: xlang", "Xlang-Info: build by xlang"]
            ++ mainClassLines
            ++ [""])

    createDirectoryIfMissing True jarDir
    writeFile manifestPath manifestContent
    callProcess "jar" ["cfm", jarOutput, manifestPath, "-C", classesDir, "."]
    removeFile manifestPath `catchIOError` (\_ -> pure ())


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
    callProcess "jar" ["uf", jarOutput, "-C", mergeDir, "."]
    removePathForcibly mergeDir `catchIOError` (\_ -> pure ())
  where
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


compileJVMToJar :: Int -> FilePath -> FilePath -> [FilePath] -> [FilePath] -> FilePath -> Bool -> Bool -> IO ()
compileJVMToJar jobs toolkitJar rootPath srcPaths libPaths jarOutput includeRuntime debugOut = do
    let classesOut = rootPath </> "out"

    existed <- doesDirectoryExist classesOut
    when existed (removePathForcibly classesOut)

    createDirectoryIfMissing True classesOut
    putStrLn ("[INFO] -d is jar; classes output dir: " ++ classesOut)

    mIrs <- compileJVM jobs toolkitJar rootPath srcPaths libPaths (Just classesOut) debugOut
    case mIrs of
        Just irs -> do
            let mMainClass = selectJarMainClass irs
            writeJarFromDir classesOut jarOutput mMainClass
            case mMainClass of
                Just qn -> putStrLn ("[INFO] manifest Main-Class: " ++ intercalate "." qn)
                Nothing -> putStrLn "[WARN] no wrapped-class main found; jar has no Main-Class"
            when includeRuntime $ do
                putStrLn ("[INFO] include-runtime: merging " ++ show (length libPaths) ++ " lib(s) into jar")
                mergeLibsIntoJar rootPath libPaths jarOutput
            putStrLn ("[DONE] jar generated: " ++ jarOutput)
        Nothing ->
            putStrLn "[ERROR] xlang compile failed; jar packaging skipped"

