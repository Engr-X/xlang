module Xlang where

import Control.Monad (void, when)
import Data.Char (isDigit, toLower)
import Data.Foldable (for_)
import Data.List (intercalate, sort, stripPrefix)
import Data.Map.Strict (Map)
import Data.Maybe (isJust)
import GHC.Conc (setNumCapabilities)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory, takeExtension, takeFileName)
import System.Process (readProcessWithExitCode)

import qualified CompileJava as CJ
import qualified CompileX64 as CX64
import qualified Data.Map.Strict as Map


bytecodegenFile :: FilePath -> FilePath
bytecodegenFile exePath = map slash exePath ++ "/tools/BytecodeToolkit-alpha.jar"
  where
    slash '\\' = '/'
    slash c = c


data CompileTarget
    = TargetJvm Int
    | TargetX64


data Options = Options {
    optHelp :: Bool,
    optVersion :: Bool,
    optDownload :: Maybe Int,
    optMainEntry :: Maybe [String],
    optTargetJvm :: Maybe Int,
    optTargetX64 :: Bool,
    optInputs :: [FilePath],
    optLibs :: [FilePath],
    optX64Compiler :: Maybe CX64.X64CompilerChoice,
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
    optTargetX64 = False,
    optInputs = [],
    optLibs = [],
    optX64Compiler = Nothing,
    optOutput = Nothing,
    optIncludeRuntime = False,
    optRoot = ".",
    optJobs = 1,
    optDebug = False,
    optError = Nothing}


xlangVersion :: String
xlangVersion = "alpha-1.1.0"


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


normalizeRootArg :: FilePath -> FilePath
normalizeRootArg raw =
    let ext = map toLower (takeExtension raw)
    in if ext `elem` [".x", ".xl", ".xlang"]
        then takeDirectory raw
        else raw


canonicalizeIfExists :: FilePath -> IO FilePath
canonicalizeIfExists path = do
    isFile <- doesFileExist path
    if isFile
        then canonicalizePath path
        else pure path


validateSupportedJvmVersion :: Int -> Either String Int
validateSupportedJvmVersion version
    | Map.member version jdkMetadataUrlMap = Right version
    | otherwise = Left ("unsupported --target-jvm version: " ++ show version ++ "; supported versions: " ++ supportedJdkMetadataVersionsText)


resolveCompileTarget :: Options -> Maybe CompileTarget
resolveCompileTarget opts
    | optTargetX64 opts = Just TargetX64
    | otherwise = TargetJvm <$> optTargetJvm opts


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


findDefaultX64NativeLibs :: FilePath -> IO [FilePath]
findDefaultX64NativeLibs exeDir = do
    let nativeDir = exeDir </> "libs" </> "native"
    exists <- doesDirectoryExist nativeDir
    if not exists
        then pure []
        else sort <$> collect nativeDir
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
                    else
                        let ext = map toLower (takeExtension path)
                        in pure [path | ext `elem` [".dll", ".lib", ".a", ".so", ".dylib"]])
            names


isAllowedX64LibFile :: FilePath -> Bool
isAllowedX64LibFile path =
    let ext = map toLower (takeExtension path)
    in ext == ".dll" || ext == ".lib" || ext == ".a" || ext == ".so" || ext == ".dylib"


invalidX64LibFiles :: [FilePath] -> [FilePath]
invalidX64LibFiles = filter (not . isAllowedX64LibFile)

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
        stripWrappingQuotes :: String -> String
        stripWrappingQuotes raw
            | length raw >= 2
            , let h = head raw
            , let t = last raw
            , (h == '"' && t == '"') || (h == '\'' && t == '\'') =
                init (tail raw)
            | otherwise = raw

        normalizePathArg :: String -> String
        normalizePathArg = stripWrappingQuotes

        addInput :: Options -> FilePath -> Options
        addInput opts fp = opts {optInputs = optInputs opts ++ [normalizePathArg fp]}

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

        parseLegacyTargetValue :: String -> Either String CompileTarget
        parseLegacyTargetValue raw = case raw of
            "jvm" -> Right (TargetJvm defaultJvmTargetVersion)
            "x64" -> Right TargetX64
            _ ->
                case stripPrefix "jvm" raw of
                    Just suffix | not (null suffix) && all isDigit suffix ->
                        case parsePositiveInt suffix of
                            Just n -> TargetJvm <$> validateSupportedJvmVersion n
                            Nothing -> Left "invalid --target value; expected jvm<number>"
                    _ -> Left "unsupported --target value; use --target-jvm<number> or --target=x64"

        parseTargetJvmValue :: String -> Options -> [String] -> Options
        parseTargetJvmValue raw opts xs =
            case parseTargetJvmArg raw of
                Right n -> go opts {optTargetJvm = Just n, optTargetX64 = False} xs
                Left msg -> opts {optHelp = True, optError = Just msg}

        parseLegacyTarget :: String -> Options -> [String] -> Options
        parseLegacyTarget raw opts xs =
            case parseLegacyTargetValue raw of
                Right (TargetJvm n) -> go opts {optTargetJvm = Just n, optTargetX64 = False} xs
                Right TargetX64 -> go opts {optTargetJvm = Nothing, optTargetX64 = True} xs
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
            case parseMainEntryClass (stripWrappingQuotes raw) of
                Left msg -> opts {optHelp = True, optError = Just msg}
                Right qn -> go opts {optMainEntry = Just qn} xs

        parseX64CompilerValue :: String -> Options -> [String] -> Options
        parseX64CompilerValue raw opts xs = case map toLower (stripWrappingQuotes raw) of
            "nasm" -> go opts {optX64Compiler = Just CX64.X64CompilerNasm} xs
            "as" -> go opts {optX64Compiler = Just CX64.X64CompilerAs} xs
            _ -> opts {
                optHelp = True,
                optError = Just "invalid value for --compiler; expected nasm or as"}

        go :: Options -> [String] -> Options
        go opts [] = opts

        go opts ("-h" : _) = opts {optHelp = True}
        go opts ("--help" : _) = opts {optHelp = True}
        go opts ("-v" : _) = opts {optVersion = True}
        go opts ("--version" : _) = opts {optVersion = True}

        go opts ("--target=jvm" : xs) = go opts {optTargetJvm = Just defaultJvmTargetVersion, optTargetX64 = False} xs
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
                go opts {optRoot = normalizePathArg rootDir} xs
            | Just rootDir <- stripPrefix "root=" arg =
                go opts {optRoot = normalizePathArg rootDir} xs
        go opts ("--root" : rootDir : xs) = go opts {optRoot = normalizePathArg rootDir} xs
        go opts ("-r" : rootDir : xs) = go opts {optRoot = normalizePathArg rootDir} xs
        go opts ("root" : rootDir : xs) = go opts {optRoot = normalizePathArg rootDir} xs
        go opts ["--root"] = opts {optHelp = True, optError = Just "missing value for --root"}
        go opts ["-r"] = opts {optHelp = True, optError = Just "missing value for -r"}
        go opts ["root"] = opts {optHelp = True, optError = Just "missing value for root"}

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

        go opts ("--compiler" : c : xs) = parseX64CompilerValue c opts xs
        go opts ["--compiler"] = opts {optHelp = True, optError = Just "missing value for --compiler"}

        -- Compatibility: old style "-c file.x"
        go opts ("-c" : src : xs) = go (addInput opts src) xs
        go opts ["-c"] = opts {optHelp = True, optError = Just "missing value for -c"}

        go opts ("-lib" : xs) =
            let (libs, rest) = span isLikelyPath xs
            in if null libs
                then opts {optHelp = True, optError = Just "missing value(s) for -lib"}
                else go opts {optLibs = optLibs opts ++ map normalizePathArg libs} rest

        -- Kotlin style output: -d <dir|jar>. Keep "-d" alone as debug shorthand.
        go opts ("-d" : out : xs)
            | isLikelyPath out = go opts {optOutput = Just (normalizePathArg out)} xs
            | otherwise = go opts {optDebug = True} (out : xs)
        go opts ["-d"] = go opts {optDebug = True} []

        go opts ("--include-runtime" : xs) = go opts {optIncludeRuntime = True} xs
        go opts ("--imclude-runtime" : xs) = go opts {optIncludeRuntime = True} xs
        go opts ("-include-runtime" : xs) = go opts {optIncludeRuntime = True} xs

        go opts ("--debug" : xs) = go opts {optDebug = True} xs
        go opts ("-debug" : xs) = go opts {optDebug = True} xs

        go opts (arg : xs)
            | isLikelyPath arg = go (addInput opts arg) xs

        go opts (arg : xs)
            | Just c <- stripPrefix "--compiler=" arg = parseX64CompilerValue c opts xs

        go opts (arg : _) = opts {optHelp = True, optError = Just ("unknown arg: " ++ arg)}


printHelp :: IO ()
printHelp = putStrLn $ unlines [
    "Usage:",
    "   xlang --target-jvm<n> <file.x|file.xl|file.xlang> [more files ...]",
    "         [-lib <a.jar b.class ...>] [--root=<dir>|--root <dir>|-r <dir>|root=<dir>] [-d <dir|jar>]",
    "         [--main=<qname.main>] [-j <n>|--jobs <n>] [--include-runtime|-include-runtime] [--debug|-debug|-d]",
    "   xlang --target=x64 <file.x|file.xl|file.xlang> [more files ...]",
    "         [-lib <a.jar b.class ...>] [--root=<dir>|--root <dir>|-r <dir>|root=<dir>] [--compiler=<nasm|as>] [-d <dir|file.o|file.exe|file.out|file.dll|file.so|file.dylib|file.a|file.lib>]",
    "         [-j <n>|--jobs <n>] [--debug|-debug|-d]",
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
    "   --target=x64     compile to x64 object output (and link when -d is exe/out/dll/so/dylib/a/lib)",
    "                    each lowered unit emits .o (and .asm only in debug mode)",
    "   --compiler=<nasm|as>",
    "                    x64-only assembler choice (default: nasm)",
    "   --compiler <nasm|as>",
    "                    same as above",
    "   --target x64     compatibility alias for x64 target",
    "   -download=<n>, --download=<n>",
    "                    download jdk metadata db to <xlang.exe dir>/libs/java-native/jdk-<target>",
    "                    supported versions: " ++ supportedJdkMetadataVersionsText,
    "   <file.*>         source files to compile (extensions: .x/.xl/.xlang)",
    "   -c <file.*>      compatibility alias for one input file",
    "   --root=<dir>     source root (preferred form), e.g. --root=./abcde",
    "   --root <dir>     source root (compatible form)",
    "   -r <dir>         source root (short form)",
    "   root=<dir>       source root (legacy compatible form)",
    "                    when root points to *.x/*.xl/*.xlang, its parent dir is used",
    "   <path> in ''/\"\" quoted paths are accepted and unwrapped",
    "   -j<n>, -j <n>, --jobs <n>, --jobs=<n>",
    "                    use n worker threads",
    "                    applies to: 1) batch -lib loading, 2) post-IR JVM lowering + bytecode generation",
    "   -lib <files...>  external libs: JVM(.class/.jar/.json/.db/.jmod), x64(.dll/.lib/.a/.so/.dylib)",
    "                    plus default: <xlang.exe dir>/libs/**/xlang-stdlib-alpha.jar",
    "                    java-native metadata under <xlang.exe dir>/libs/java-native/** is loaded only when source imports include java.*",
    "   --main=<qname.main>",
    "                    explicit entrypoint for jar manifest (e.g. --main=com.wangdi.MainKt.main)",
    "                    class-only is also accepted: --main=com.wangdi.MainKt",
    "                    x64 exe/out: used as main selector hint (exact class first, then package scan)",
    "   -d <dir|jar>     output directory or jar path",
    "                    if .jar: classes are emitted to <root>/out, then packed",
    "                    manifest includes: build by xlang",
    "   --include-runtime",
    "   -include-runtime  merge all -lib files and default xlang stdlib jar into output jar",
    "                    requires: -d <something>.jar",
    "   --debug|-debug   JVM: write debug.json + per-class .ir; x64: write per-class .ir (+ timing logs)",
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
                        rootAbs <- canonicalizePath (normalizeRootArg (optRoot opts))

                        setNumCapabilities (optJobs opts)

                        let srcPathsRaw = map (CJ.resolveFromRoot rootAbs) (optInputs opts)
                            userLibPathsRaw = map (CJ.resolveFromRoot rootAbs) (optLibs opts)
                        srcPaths <- mapM canonicalizeIfExists srcPathsRaw
                        userLibPaths <- mapM canonicalizeIfExists userLibPathsRaw

                        let mTarget = resolveCompileTarget opts
                            mTargetJvm = case mTarget of
                                Just (TargetJvm n) -> Just n
                                _ -> Nothing
                            invalidInputs = CJ.invalidSourceFiles srcPaths
                            invalidLibs = case mTarget of
                                Just TargetX64 -> invalidX64LibFiles userLibPaths
                                _ -> CJ.invalidLibFiles userLibPaths
                            includeRuntime = optIncludeRuntime opts
                            jobs = optJobs opts
                            mMainClassOverride = optMainEntry opts
                            toolkitJar = bytecodegenFile exeDir

                        needsJavaNative <- case mTarget of
                            Just (TargetJvm _) | not (null srcPaths) && null invalidInputs ->
                                CJ.sourceNeedsJavaMetadata srcPaths
                            _ -> pure False

                        for_ mTargetJvm $ \targetJvm ->
                            when needsJavaNative (ensureTargetJdkMetadata exeDir targetJvm)

                        defaultLibJars <- case mTarget of
                            Just (TargetJvm _) -> CJ.findDefaultLibJars exeDir
                            _ -> pure []
                        defaultRuntimeLibJars <- case mTarget of
                            Just (TargetJvm _) -> CJ.findDefaultRuntimeLibJars exeDir
                            _ -> pure []
                        defaultNativeJsons <-
                            case mTargetJvm of
                                Just targetJvm | needsJavaNative ->
                                    CJ.findDefaultNativeJsons exeDir targetJvm
                                _ -> pure []
                        defaultX64NativeLibs <- case mTarget of
                            Just TargetX64 -> findDefaultX64NativeLibs exeDir
                            _ -> pure []

                        let combinedLibPaths = case mTarget of
                                Just TargetX64 -> userLibPaths ++ defaultX64NativeLibs
                                _ -> userLibPaths ++ defaultLibJars ++ defaultNativeJsons
                            duplicateLibs = CJ.duplicateLibRefs combinedLibPaths
                            libPaths = combinedLibPaths
                            runtimeLibPaths = userLibPaths ++ defaultRuntimeLibJars

                        case (mTarget, srcPaths, optOutput opts) of
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
                                case mTarget of
                                    Just TargetX64 ->
                                        putStrLn "invalid -lib extension for x64; only .dll, .lib, .a, .so and .dylib are allowed"
                                    _ ->
                                        putStrLn "invalid -lib extension; only .class, .jar, .json, .db and .jmod are allowed"
                                mapM_ (\p -> putStrLn ("  - " ++ p)) invalidLibs
                                printHelp
                            (Just (TargetJvm targetJvm), _, Just outPath0) -> do
                                let outPath = CJ.resolveFromRoot rootAbs outPath0
                                if isJust (optX64Compiler opts)
                                    then do
                                        putStrLn "--compiler is only valid with --target=x64"
                                        printHelp
                                    else if includeRuntime && not (CJ.isJarOutput outPath)
                                    then do
                                        putStrLn "cannot use -include-runtime with classes output type; expected a .jar"
                                        printHelp
                                    else if not (CJ.isJarOutput outPath) && isJust mMainClassOverride
                                        then do
                                            putStrLn "--main is only valid when -d points to a .jar output"
                                            printHelp
                                        else if CJ.isJarOutput outPath
                                        then CJ.compileJVMToJar jobs targetJvm toolkitJar rootAbs srcPaths libPaths runtimeLibPaths outPath includeRuntime (optDebug opts) mMainClassOverride
                                        else void (CJ.compileJVM jobs targetJvm toolkitJar rootAbs srcPaths libPaths (Just outPath) (optDebug opts))
                            (Just (TargetJvm targetJvm), _, Nothing) ->
                                if isJust (optX64Compiler opts)
                                    then do
                                        putStrLn "--compiler is only valid with --target=x64"
                                        printHelp
                                    else if includeRuntime
                                    then do
                                        putStrLn "cannot use -include-runtime without -d <output>.jar"
                                        printHelp
                                    else if isJust mMainClassOverride
                                        then do
                                            putStrLn "--main is only valid with -d <output>.jar"
                                            printHelp
                                        else void (CJ.compileJVM jobs targetJvm toolkitJar rootAbs srcPaths libPaths Nothing (optDebug opts))
                            (Just TargetX64, _, _) | includeRuntime -> do
                                putStrLn "cannot use -include-runtime with --target=x64"
                                printHelp
                            (Just TargetX64, _, mOut) ->
                                void (CX64.compileX64 jobs toolkitJar rootAbs srcPaths libPaths mOut mMainClassOverride (optX64Compiler opts) (optDebug opts))
                            _ -> do
                                putStrLn "missing target; use --target-jvm<number> or --target=x64"
                                printHelp
