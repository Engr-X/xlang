module Xlang where

import Control.Monad (void)
import Data.List (stripPrefix)
import GHC.Conc (setNumCapabilities)
import System.Directory (canonicalizePath)
import System.Environment (getArgs, getExecutablePath)
import System.FilePath (takeDirectory)

import qualified CompileJava as CJ


bytecodegenFile :: FilePath -> FilePath
bytecodegenFile exePath = map slash exePath ++ "/tools/BytecodeToolkit-alpha.jar"
  where
    slash '\\' = '/'
    slash c = c


data Options = Options {
    optHelp :: Bool,
    optVersion :: Bool,
    optTarget :: Maybe String,
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
    optTarget = Nothing,
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

    parseJobsValue :: String -> Options -> [String] -> Options
    parseJobsValue raw opts xs = case parsePositiveInt raw of
        Just n -> go opts {optJobs = n} xs
        Nothing -> opts {
            optHelp = True,
            optError = Just "invalid value for -j/--jobs; expected positive integer"
        }

    go :: Options -> [String] -> Options
    go opts [] = opts

    go opts ("-h" : _) = opts {optHelp = True}
    go opts ("--help" : _) = opts {optHelp = True}
    go opts ("-v" : _) = opts {optVersion = True}
    go opts ("--version" : _) = opts {optVersion = True}

    go opts ("--target=jvm" : xs) = go opts {optTarget = Just "jvm"} xs
    go opts ("--target" : t : xs) = go opts {optTarget = Just t} xs
    go opts ["--target"] = opts {optHelp = True, optError = Just "missing value for --target"}

    go opts (arg : xs)
        | Just jobsRaw <- stripPrefix "--jobs=" arg =
            parseJobsValue jobsRaw opts xs
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
    "   xlang --target=jvm <file.x|file.xl|file.xlang> [more files ...]",
    "         [-lib <a.jar b.class ...>] [--root=<dir>|--root <dir>|-r <dir>]",
    "         [-d <dir|jar>] [-j <n>|--jobs <n>] [--include-runtime|-include-runtime] [--debug|-debug|-d]",
    "   xlang -v | --version",
    "   xlang -h | --help",
    "",
    "Options:",
    "   --target=jvm     compile to JVM bytecode (required)",
    "   <file.*>         source files to compile (extensions: .x/.xl/.xlang)",
    "   -c <file.*>      compatibility alias for one input file",
    "   --root=<dir>     source root (preferred form), e.g. --root=./abcde",
    "   --root <dir>     source root (compatible form)",
    "   -r <dir>         source root (short form)",
    "   -j <n>, --jobs <n>",
    "                    use n worker threads",
    "                    applies to: 1) batch -lib loading, 2) post-IR JVM lowering + bytecode generation",
    "   -lib <files...>  external libs (.class/.jar), e.g. -lib a.jar b.class",
    "                    plus default: all .jar under <xlang.exe dir>/libs (recursive)",
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
                rootAbs <- canonicalizePath (optRoot opts)

                setNumCapabilities (optJobs opts)

                let exeDir = takeDirectory absExePath
                defaultLibJars <- CJ.findDefaultLibJars exeDir

                let toolkitJar = bytecodegenFile exeDir
                    srcPaths = map (CJ.resolveFromRoot rootAbs) (optInputs opts)
                    userLibPaths = map (CJ.resolveFromRoot rootAbs) (optLibs opts)
                    combinedLibPaths = userLibPaths ++ defaultLibJars
                    duplicateLibs = CJ.duplicateLibRefs combinedLibPaths
                    libPaths = combinedLibPaths
                    invalidInputs = CJ.invalidSourceFiles srcPaths
                    invalidLibs = CJ.invalidLibFiles userLibPaths
                    includeRuntime = optIncludeRuntime opts
                    jobs = optJobs opts

                case (optTarget opts, srcPaths, optOutput opts) of
                    (Just "jvm", [], _) -> do
                        putStrLn "missing input xlang file"
                        printHelp
                    (Just "jvm", _, _) | not (null invalidInputs) -> do
                        putStrLn "invalid source file extension; only .x, .xl, .xlang are allowed"
                        mapM_ (\p -> putStrLn ("  - " ++ p)) invalidInputs
                        printHelp
                    (Just "jvm", _, _) | not (null duplicateLibs) -> do
                        putStrLn "duplicate library reference detected"
                        mapM_ (\p -> putStrLn ("  - " ++ p)) duplicateLibs
                        printHelp
                    (Just "jvm", _, _) | not (null invalidLibs) -> do
                        putStrLn "invalid -lib extension; only .class and .jar are allowed"
                        mapM_ (\p -> putStrLn ("  - " ++ p)) invalidLibs
                        printHelp
                    (Just "jvm", _, Just outPath0) -> do
                        let outPath = CJ.resolveFromRoot rootAbs outPath0
                        if includeRuntime && not (CJ.isJarOutput outPath)
                            then do
                                putStrLn "cannot use -include-runtime with classes output type; expected a .jar"
                                printHelp
                            else if CJ.isJarOutput outPath
                                then CJ.compileJVMToJar jobs toolkitJar rootAbs srcPaths libPaths outPath includeRuntime (optDebug opts)
                                else void (CJ.compileJVM jobs toolkitJar rootAbs srcPaths libPaths (Just outPath) (optDebug opts))
                    (Just "jvm", _, Nothing) ->
                        if includeRuntime
                            then do
                                putStrLn "cannot use -include-runtime without -d <output>.jar"
                                printHelp
                            else void (CJ.compileJVM jobs toolkitJar rootAbs srcPaths libPaths Nothing (optDebug opts))
                    (Just _, _, _) -> do
                        putStrLn "unsupported target"
                        printHelp
                    _ -> do
                        putStrLn "missing --target"
                        printHelp
