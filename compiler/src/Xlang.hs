module Xlang where

import Control.Monad (void)
import Data.Char (isDigit)
import Data.Foldable (for_)
import Data.List (intercalate, sort, stripPrefix)
import Data.Map.Strict (Map)
import Data.Maybe (isJust)
import GHC.Conc (setNumCapabilities)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.Process (readProcessWithExitCode)

import qualified CompileJava as CJ
import qualified Data.Map.Strict as Map


bytecodegenFile :: FilePath -> FilePath
bytecodegenFile exePath = map slash exePath ++ "/tools/BytecodeToolkit-alpha.jar"
  where
    slash '\\' = '/'
    slash c = c


data Options = Options {
    optHelp :: Bool,
    optVersion :: Bool,
    optDownload :: Maybe Int,
    optMainEntry :: Maybe [String],
    optTargetJvm :: Maybe Int,
    optInputs :: [FilePath],
    optLibs :: [FilePath],
    optOutput :: Maybe FilePath,
    optIncludeRuntime :: Bool,
    optRoot :: FilePath,
    optJobs :: Int,
    optDebug :: Bool,
    optError :: Maybe String
    }


defaultOptions :: Options
defaultOptions = Options {
    optHelp = False,
    optVersion = False,
    optDownload = Nothing,
    optMainEntry = Nothing,
    optTargetJvm = Nothing,
    optInputs = [],
    optLibs = [],
    optOutput = Nothing,
    optIncludeRuntime = False,
    optRoot = ".",
    optJobs = 1,
    optDebug = False,
    optError = Nothing}


xlangVersion :: String
xlangVersion = "alpha 0.0.0"


jdkMetadataUrlMap :: Map Int String
jdkMetadataUrlMap = Map.fromList [
    (8, "https://github.com/Engr-X/xlang-jdk-metadata/releases/download/jdk-std-main/jdk-1.8.0_202.db"),
    (11, "https://github.com/Engr-X/xlang-jdk-metadata/releases/download/jdk-std-main/jdk-11.0.29.db"),
    (17, "https://github.com/Engr-X/xlang-jdk-metadata/releases/download/jdk-std-main/jdk-17.0.10.db"),
    (21, "https://github.com/Engr-X/xlang-jdk-metadata/releases/download/jdk-std-main/jdk-21.0.10.db"),
    (25, "https://github.com/Engr-X/xlang-jdk-metadata/releases/download/jdk-std-main/jdk-25.0.1.db")]


defaultJvmTargetVersion :: Int
defaultJvmTargetVersion = 8


supportedJdkMetadataVersions :: [Int]
supportedJdkMetadataVersions = sort (Map.keys jdkMetadataUrlMap)


supportedJdkMetadataVersionsText :: String
supportedJdkMetadataVersionsText = intercalate ", " (map show supportedJdkMetadataVersions)


validateSupportedJvmVersion :: Int -> Either String Int
validateSupportedJvmVersion version
    | Map.member version jdkMetadataUrlMap = Right version
    | otherwise = Left ("unsupported --target-jvm version: " ++ show version ++ "; supported versions: " ++ supportedJdkMetadataVersionsText)


downloadJdkMetadata :: FilePath -> Int -> IO ()
downloadJdkMetadata exeDir version =
    case Map.lookup version jdkMetadataUrlMap of
        Nothing -> do
            putStrLn ("unsupported java metadata version: " ++ show version)
            putStrLn ("supported versions: " ++ supportedJdkMetadataVersionsText)
        Just url -> do
            let outDir = versionedMetadataDir exeDir version
                outFile = outDir </> takeFileName url
                downloadScript = concat [
                    "$ErrorActionPreference='Stop'; ",
                    "$ProgressPreference='SilentlyContinue'; ",
                    "Invoke-WebRequest -Uri \"", url, "\" -OutFile \"", outFile, "\""]

            createDirectoryIfMissing True outDir
            putStrLn ("[DOWNLOAD] jdk metadata v" ++ show version)
            putStrLn ("           url : " ++ url)
            putStrLn ("           file: " ++ outFile)
            (downloadCode, _, downloadErr) <- readProcessWithExitCode "powershell" ["-NoProfile", "-Command", downloadScript] ""
            case downloadCode of
                ExitFailure _ -> do
                    putStrLn "[ERROR] failed to download jdk metadata db"
                    if null downloadErr then pure () else putStrLn downloadErr
                ExitSuccess ->
                    putStrLn ("[DONE] downloaded: " ++ outFile)

hasAnyEntries :: FilePath -> IO Bool
hasAnyEntries dir = do
    exists <- doesDirectoryExist dir
    if not exists
        then pure False
        else do
            entries <- listDirectory dir
            pure (not (null entries))


versionedMetadataDir :: FilePath -> Int -> FilePath
versionedMetadataDir exeDir version = exeDir </> "libs" </> "java-native" </> ("jdk-" ++ show version)


hasVersionedMetadata :: FilePath -> Int -> IO Bool
hasVersionedMetadata exeDir version = hasAnyEntries (versionedMetadataDir exeDir version)


ensureTargetJdkMetadata :: FilePath -> Int -> IO ()
ensureTargetJdkMetadata exeDir version = do
    versionedReady <- hasVersionedMetadata exeDir version
    legacyReady <- if version == defaultJvmTargetVersion
        then hasAnyEntries (exeDir </> "libs" </> "java-native")
        else pure False
    if versionedReady || legacyReady
        then pure ()
        else do
            putStrLn ("[INFO] java-native metadata for jdk v" ++ show version ++ " not found; auto download")
            downloadJdkMetadata exeDir version

splitOnDot :: String -> [String]
splitOnDot s =
    case break (== '.') s of
        (a, []) -> [a]
        (a, _ : rest) -> a : splitOnDot rest


parseMainEntryClass :: String -> Either String [String]
parseMainEntryClass raw =
    let segs = splitOnDot raw
    in if null raw || any null segs
        then Left "invalid --main format; expected: com.wangdi.MainKt.main"
        else
            case reverse segs of
                ("main" : revCls) | not (null revCls) -> Right (reverse revCls)
                _ -> Right segs

parseArgs :: [String] -> Options
parseArgs = go defaultOptions
    where
        addInput :: Options -> FilePath -> Options
        addInput opts fp = opts {optInputs = optInputs opts ++ [fp]}

        isLikelyPath :: String -> Bool
        isLikelyPath s = not (null s) && head s /= '-'

        parsePositiveInt :: String -> Maybe Int
        parsePositiveInt raw = case reads raw of
            [(n, "")] | n > 0 -> Just n
            _ -> Nothing

        parseTargetJvmArg :: String -> Either String Int
        parseTargetJvmArg arg = case stripPrefix "--target-jvm" arg of
            Nothing -> Left "internal parse error for --target-jvm"
            Just "" -> Right defaultJvmTargetVersion
            Just ('=' : _) -> Left "invalid --target-jvm format; use --target-jvm25 (no '=')"
            Just suffix
                | all isDigit suffix ->
                    case parsePositiveInt suffix of
                        Just n -> validateSupportedJvmVersion n
                        Nothing -> Left "invalid --target-jvm value; expected positive integer suffix"
                | otherwise ->
                    Left "invalid --target-jvm format; use --target-jvm25"

        parseLegacyTargetValue :: String -> Either String Int
        parseLegacyTargetValue raw = case raw of
            "jvm" -> Right defaultJvmTargetVersion
            _ ->
                case stripPrefix "jvm" raw of
                    Just suffix | not (null suffix) && all isDigit suffix ->
                        case parsePositiveInt suffix of
                            Just n -> validateSupportedJvmVersion n
                            Nothing -> Left "invalid --target value; expected jvm<number>"
                    _ -> Left "unsupported --target value; use --target-jvm<number>"

        parseTargetJvmValue :: String -> Options -> [String] -> Options
        parseTargetJvmValue raw opts xs =
            case parseTargetJvmArg raw of
                Right n -> go opts {optTargetJvm = Just n} xs
                Left msg -> opts {optHelp = True, optError = Just msg}

        parseLegacyTarget :: String -> Options -> [String] -> Options
        parseLegacyTarget raw opts xs =
            case parseLegacyTargetValue raw of
                Right n -> go opts {optTargetJvm = Just n} xs
                Left msg -> opts {optHelp = True, optError = Just msg}

        parseJobsValue :: String -> Options -> [String] -> Options
        parseJobsValue raw opts xs = case parsePositiveInt raw of
            Just n -> go opts {optJobs = n} xs
            Nothing -> opts {
                optHelp = True,
                optError = Just "invalid value for -j/--jobs; expected positive integer"
            }

        parseDownloadValue :: String -> Options -> [String] -> Options
        parseDownloadValue raw opts xs = case parsePositiveInt raw of
            Just n -> go opts {optDownload = Just n} xs
            Nothing -> opts {
                optHelp = True,
                optError = Just "invalid value for -download/--download; expected positive integer"
            }

        parseMainValue :: String -> Options -> [String] -> Options
        parseMainValue raw opts xs =
            case parseMainEntryClass raw of
                Left msg -> opts {optHelp = True, optError = Just msg}
                Right qn -> go opts {optMainEntry = Just qn} xs

        go :: Options -> [String] -> Options
        go opts [] = opts

        go opts ("-h" : _) = opts {optHelp = True}
        go opts ("--help" : _) = opts {optHelp = True}
        go opts ("-v" : _) = opts {optVersion = True}
        go opts ("--version" : _) = opts {optVersion = True}

        go opts ("--target=jvm" : xs) = go opts {optTargetJvm = Just defaultJvmTargetVersion} xs
        go opts ("--target" : t : xs) = parseLegacyTarget t opts xs
        go opts ["--target"] = opts {optHelp = True, optError = Just "missing value for --target"}

        go opts (arg : xs)
            | Just targetRaw <- stripPrefix "--target=" arg =
                parseLegacyTarget targetRaw opts xs
            | Just _ <- stripPrefix "--target-jvm" arg =
                parseTargetJvmValue arg opts xs
            | Just jobsRaw <- stripPrefix "--jobs=" arg =
                parseJobsValue jobsRaw opts xs
            | Just jobsRaw <- stripPrefix "-j=" arg =
                parseJobsValue jobsRaw opts xs
            | Just jobsRaw <- stripPrefix "-j" arg, not (null jobsRaw), all isDigit jobsRaw =
                parseJobsValue jobsRaw opts xs
            | Just verRaw <- stripPrefix "-download=" arg =
                parseDownloadValue verRaw opts xs
            | Just verRaw <- stripPrefix "--download=" arg =
                parseDownloadValue verRaw opts xs
            | Just mainRaw <- stripPrefix "--main=" arg =
                parseMainValue mainRaw opts xs
            | Just rootDir <- stripPrefix "--root=" arg =
                go opts {optRoot = rootDir} xs
        go opts ("--root" : rootDir : xs) = go opts {optRoot = rootDir} xs
        go opts ("-r" : rootDir : xs) = go opts {optRoot = rootDir} xs
        go opts ["--root"] = opts {optHelp = True, optError = Just "missing value for --root"}
        go opts ["-r"] = opts {optHelp = True, optError = Just "missing value for -r"}

        go opts ("-j" : n : xs) = parseJobsValue n opts xs
        go opts ("--jobs" : n : xs) = parseJobsValue n opts xs
        go opts ["-j"] = opts {optHelp = True, optError = Just "missing value for -j"}
        go opts ["--jobs"] = opts {optHelp = True, optError = Just "missing value for --jobs"}

        go opts ("-download" : n : xs) = parseDownloadValue n opts xs
        go opts ("--download" : n : xs) = parseDownloadValue n opts xs
        go opts ["-download"] = opts {optHelp = True, optError = Just "missing value for -download"}
        go opts ["--download"] = opts {optHelp = True, optError = Just "missing value for --download"}

        go opts ("--main" : v : xs) = parseMainValue v opts xs
        go opts ["--main"] = opts {optHelp = True, optError = Just "missing value for --main"}

        -- Compatibility: old style "-c file.x"
        go opts ("-c" : src : xs) = go (addInput opts src) xs
        go opts ["-c"] = opts {optHelp = True, optError = Just "missing value for -c"}

        go opts ("-lib" : xs) =
            let (libs, rest) = span isLikelyPath xs
            in if null libs
                then opts {optHelp = True, optError = Just "missing value(s) for -lib"}
                else go opts {optLibs = optLibs opts ++ libs} rest

        -- Kotlin style output: -d <dir|jar>. Keep "-d" alone as debug shorthand.
        go opts ("-d" : out : xs)
            | isLikelyPath out = go opts {optOutput = Just out} xs
            | otherwise = go opts {optDebug = True} (out : xs)
        go opts ["-d"] = go opts {optDebug = True} []

        go opts ("--include-runtime" : xs) = go opts {optIncludeRuntime = True} xs
        go opts ("--imclude-runtime" : xs) = go opts {optIncludeRuntime = True} xs
        go opts ("-include-runtime" : xs) = go opts {optIncludeRuntime = True} xs

        go opts ("--debug" : xs) = go opts {optDebug = True} xs
        go opts ("-debug" : xs) = go opts {optDebug = True} xs

        go opts (arg : xs)
            | isLikelyPath arg = go (addInput opts arg) xs

        go opts (arg : _) = opts {optHelp = True, optError = Just ("unknown arg: " ++ arg)}


printHelp :: IO ()
printHelp = putStrLn $ unlines [
    "Usage:",
    "   xlang --target-jvm<n> <file.x|file.xl|file.xlang> [more files ...]",
    "         [-lib <a.jar b.class ...>] [--root=<dir>|--root <dir>|-r <dir>] [-d <dir|jar>]",
    "         [--main=<qname.main>] [-j <n>|--jobs <n>] [--include-runtime|-include-runtime] [--debug|-debug|-d]",
    "",
    "   xlang -v | --version",
    "   xlang -h | --help",
    "   xlang -download=<n>",
    "",
    "Options:",
    "   --target-jvm<n>  compile to JVM bytecode for target n (e.g. --target-jvm25)",
    "   --target-jvm     compile to JVM bytecode (defaults to jvm8)",
    "   --target=jvm     compatibility alias for --target-jvm",
    "   --target=jvm<n>  compatibility alias for --target-jvm<n> (e.g. --target=jvm25)",
    "   --target jvm<n>  compatibility alias for --target-jvm<n>",
    "   -download=<n>, --download=<n>",
    "                    download jdk metadata db to <xlang.exe dir>/libs/java-native/jdk-<target>",
    "                    supported versions: " ++ supportedJdkMetadataVersionsText,
    "   <file.*>         source files to compile (extensions: .x/.xl/.xlang)",
    "   -c <file.*>      compatibility alias for one input file",
    "   --root=<dir>     source root (preferred form), e.g. --root=./abcde",
    "   --root <dir>     source root (compatible form)",
    "   -r <dir>         source root (short form)",
    "   -j<n>, -j <n>, --jobs <n>, --jobs=<n>",
    "                    use n worker threads",
    "                    applies to: 1) batch -lib loading, 2) post-IR JVM lowering + bytecode generation",
    "   -lib <files...>  external libs (.class/.jar/.json/.db/.jmod), e.g. -lib a.jar b.class c.json d.db e.jmod",
    "                    plus default: all .jar under <xlang.exe dir>/libs and .class/.jar/.json/.db/.jmod under <xlang.exe dir>/libs/java-native/jdk-<target>",
    "                    fallback for legacy metadata: all .class/.jar/.json/.db/.jmod under <xlang.exe dir>/libs/java-native",
    "   --main=<qname.main>",
    "                    explicit entrypoint for jar manifest (e.g. --main=com.wangdi.MainKt.main)",
    "                    class-only is also accepted: --main=com.wangdi.MainKt",
    "   -d <dir|jar>     output directory or jar path",
    "                    if .jar: classes are emitted to <root>/out, then packed",
    "                    manifest includes: build by xlang",
    "   --include-runtime",
    "   -include-runtime  merge all -lib files and default <xlang.exe>/libs jars into output jar",
    "                    requires: -d <something>.jar",
    "   --debug|-debug   write debug.json and Ir.txt next to source(s)",
    "   -d               debug shorthand when used without output path",
    "   -v, --version    print xlang version",
    "   -h, --help       show this help"]


main :: IO ()
main = do
    args <- getArgs
    let opts = parseArgs args

    case optError opts of
        Just msg -> do
            putStrLn msg
            printHelp
        Nothing
            | optVersion opts -> putStrLn xlangVersion
            | optHelp opts || null args -> printHelp
            | otherwise -> do
                exePath <- getExecutablePath
                absExePath <- canonicalizePath exePath
                let exeDir = takeDirectory absExePath

                case optDownload opts of
                    Just ver -> downloadJdkMetadata exeDir ver
                    Nothing -> do
                        rootAbs <- canonicalizePath (optRoot opts)

                        setNumCapabilities (optJobs opts)

                        let mTargetJvm = optTargetJvm opts
                        for_ mTargetJvm (ensureTargetJdkMetadata exeDir)
                        defaultLibJars <- CJ.findDefaultLibJars exeDir
                        defaultNativeJsons <-
                            case mTargetJvm of
                                Just targetJvm -> CJ.findDefaultNativeJsons exeDir targetJvm
                                Nothing -> pure []

                        let toolkitJar = bytecodegenFile exeDir
                            srcPaths = map (CJ.resolveFromRoot rootAbs) (optInputs opts)
                            userLibPaths = map (CJ.resolveFromRoot rootAbs) (optLibs opts)
                            combinedLibPaths = userLibPaths ++ defaultLibJars ++ defaultNativeJsons
                            duplicateLibs = CJ.duplicateLibRefs combinedLibPaths
                            libPaths = combinedLibPaths
                            invalidInputs = CJ.invalidSourceFiles srcPaths
                            invalidLibs = CJ.invalidLibFiles userLibPaths
                            includeRuntime = optIncludeRuntime opts
                            jobs = optJobs opts
                            mMainClassOverride = optMainEntry opts

                        case (optTargetJvm opts, srcPaths, optOutput opts) of
                            (Just _, [], _) -> do
                                putStrLn "missing input xlang file"
                                printHelp
                            (Just _, _, _) | not (null invalidInputs) -> do
                                putStrLn "invalid source file extension; only .x, .xl, .xlang are allowed"
                                mapM_ (\p -> putStrLn ("  - " ++ p)) invalidInputs
                                printHelp
                            (Just _, _, _) | not (null duplicateLibs) -> do
                                putStrLn "duplicate library reference detected"
                                mapM_ (\p -> putStrLn ("  - " ++ p)) duplicateLibs
                                printHelp
                            (Just _, _, _) | not (null invalidLibs) -> do
                                putStrLn "invalid -lib extension; only .class, .jar, .json, .db and .jmod are allowed"
                                mapM_ (\p -> putStrLn ("  - " ++ p)) invalidLibs
                                printHelp
                            (Just targetJvm, _, Just outPath0) -> do
                                let outPath = CJ.resolveFromRoot rootAbs outPath0
                                if includeRuntime && not (CJ.isJarOutput outPath)
                                    then do
                                        putStrLn "cannot use -include-runtime with classes output type; expected a .jar"
                                        printHelp
                                    else if not (CJ.isJarOutput outPath) && isJust mMainClassOverride
                                        then do
                                            putStrLn "--main is only valid when -d points to a .jar output"
                                            printHelp
                                        else if CJ.isJarOutput outPath
                                        then CJ.compileJVMToJar jobs targetJvm toolkitJar rootAbs srcPaths libPaths outPath includeRuntime (optDebug opts) mMainClassOverride
                                        else void (CJ.compileJVM jobs targetJvm toolkitJar rootAbs srcPaths libPaths (Just outPath) (optDebug opts))
                            (Just targetJvm, _, Nothing) ->
                                if includeRuntime
                                    then do
                                        putStrLn "cannot use -include-runtime without -d <output>.jar"
                                        printHelp
                                    else if isJust mMainClassOverride
                                        then do
                                            putStrLn "--main is only valid with -d <output>.jar"
                                            printHelp
                                        else void (CJ.compileJVM jobs targetJvm toolkitJar rootAbs srcPaths libPaths Nothing (optDebug opts))
                            _ -> do
                                putStrLn "missing --target-jvm<number>"
                                printHelp
