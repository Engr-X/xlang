module Xlang where

import Control.Monad (void, when)
import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Char (toLower)
import Data.List (stripPrefix)
import System.Directory (
    canonicalizePath,
    createDirectoryIfMissing,
    doesDirectoryExist,
    removeFile,
    removePathForcibly
    )
import System.Environment (getArgs, getExecutablePath)
import System.FilePath ((</>), isAbsolute, takeDirectory, takeExtension)
import System.IO.Error (catchIOError)
import System.Process (callProcess)

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified IR.Lowing as IR
import qualified IR.TAC as TAC
import qualified Lowing.JVMLowing as JVML
import qualified Lowing.JVMJson as JVMJson
import qualified Util.Exception as UE
import qualified Util.FileHelper as FH


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
    optOutput :: Maybe FilePath,
    optRoot :: FilePath,
    optDebug :: Bool,
    optError :: Maybe String
    }


defaultOptions :: Options
defaultOptions = Options {
    optHelp = False,
    optVersion = False,
    optTarget = Nothing,
    optInputs = [],
    optOutput = Nothing,
    optRoot = ".", 
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
        | Just rootDir <- stripPrefix "--root=" arg =
            go opts {optRoot = rootDir} xs
    go opts ("--root" : rootDir : xs) = go opts {optRoot = rootDir} xs
    go opts ("-r" : rootDir : xs) = go opts {optRoot = rootDir} xs
    go opts ["--root"] = opts {optHelp = True, optError = Just "missing value for --root"}
    go opts ["-r"] = opts {optHelp = True, optError = Just "missing value for -r"}

    -- Compatibility: old style "-c file.x"
    go opts ("-c" : src : xs) = go (addInput opts src) xs
    go opts ["-c"] = opts {optHelp = True, optError = Just "missing value for -c"}

    -- Kotlin style output: -d <dir|jar>. Keep "-d" alone as debug shorthand.
    go opts ("-d" : out : xs)
        | isLikelyPath out = go opts {optOutput = Just out} xs
        | otherwise = go opts {optDebug = True} (out : xs)
    go opts ["-d"] = go opts {optDebug = True} []

    go opts ("--debug" : xs) = go opts {optDebug = True} xs
    go opts ("-debug" : xs) = go opts {optDebug = True} xs

    go opts (arg : xs)
        | isLikelyPath arg = go (addInput opts arg) xs

    go opts (arg : _) = opts {optHelp = True, optError = Just ("unknown arg: " ++ arg)}


printHelp :: IO ()
printHelp =
    putStrLn
        ( unlines
            [ "Usage:"
            , "   xlang --target=jvm <file.x|file.xl|file.xlang> [more files ...] [--root=<dir>|--root <dir>|-r <dir>] [-d <dir|jar>] [--debug|-debug|-d]"
            , "   xlang -v | --version"
            , "   xlang -h | --help"
            , ""
            , "Options:"
            , "   --target=jvm     compile to JVM bytecode (required)"
            , "   <file.*>         source files to compile (extensions: .x/.xl/.xlang)"
            , "   -c <file.*>      compatibility alias for one input file"
            , "   --root=<dir>     source root (preferred form), e.g. --root=./abcde"
            , "   --root <dir>     source root (compatible form)"
            , "   -r <dir>         source root (short form)"
            , "   -d <dir|jar>     output directory or jar path"
            , "                    if .jar: classes are emitted to <root>/out, then packed"
            , "                    manifest includes: build by xlang"
            , "   --debug|-debug   write debug.json and Ir.txt next to source(s)"
            , "   -d               debug shorthand when used without output path"
            , "   -v, --version    print xlang version"
            , "   -h, --help       show this help"
            ]
        )


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


compileJVM :: FilePath -> FilePath -> [FilePath] -> Maybe FilePath -> Bool -> IO Bool
compileJVM toolkitJar rootPath srcPaths mOutput debugOut = do
    sourceRes <-
        mapM
            (\path -> do
                fileRes <- FH.readFile path
                pure (path, fileRes)
            )
            srcPaths

    let readErrs = [err | (_, Left err) <- sourceRes]
        sources = [(path, code) | (path, Right code) <- sourceRes]

    if not (null readErrs)
        then do
            mapM_ (putStrLn . UE.errorToString) readErrs
            pure False
        else
            case IR.codeToIRWithRoot rootPath sources of
                Left errs -> do
                    mapM_ (putStrLn . UE.errorToString) errs
                    pure False
                Right (irPairs, warns) -> do
                    mapM_ print warns

                    let total = length irPairs
                    mapM_
                        (\(idx, (path, _)) ->
                            putStrLn (concat ["[", show idx, "/", show total, "] compile ", path, ","])
                        )
                        (zip [1 :: Int ..] irPairs)

                    let irs = map snd irPairs
                        classes = JVML.jvmProgmsLowing irs
                        jsonVal = JVMJson.jProgmToJSON 8 classes
                        jsonStr = BL.unpack (encode jsonVal)
                        debugDir = case srcPaths of
                            [one] -> takeDirectory one
                            _ -> rootPath

                    when debugOut $ BL.writeFile (debugDir </> "debug.json") (encodePretty jsonVal)
                    when debugOut $ writeFile (debugDir </> "Ir.txt") (unlines (map TAC.prettyIRProgm irs))

                    case mOutput of
                        Just outPath -> do
                            createDirectoryIfMissing True outPath
                            callProcess "java" ["-jar", toolkitJar, "-s", jsonStr, "-o", outPath]
                        Nothing ->
                            callProcess "java" ["-jar", toolkitJar, "-s", jsonStr]

                    pure True


writeJarFromDir :: FilePath -> FilePath -> IO ()
writeJarFromDir classesDir jarOutput = do
    let jarDir = takeDirectory jarOutput
        manifestPath = jarDir </> ".xlang-manifest.mf"
        manifestContent =
            unlines
                [ "Manifest-Version: 1.0"
                , "Built-By: xlang"
                , "Xlang-Info: build by xlang"
                , ""
                ]

    createDirectoryIfMissing True jarDir
    writeFile manifestPath manifestContent
    callProcess "jar" ["cfm", jarOutput, manifestPath, "-C", classesDir, "."]
    removeFile manifestPath `catchIOError` (\_ -> pure ())


compileJVMToJar :: FilePath -> FilePath -> [FilePath] -> FilePath -> Bool -> IO ()
compileJVMToJar toolkitJar rootPath srcPaths jarOutput debugOut = do
    let classesOut = rootPath </> "out"

    existed <- doesDirectoryExist classesOut
    when existed (removePathForcibly classesOut)

    createDirectoryIfMissing True classesOut
    putStrLn ("[INFO] -d is jar; classes output dir: " ++ classesOut)

    ok <- compileJVM toolkitJar rootPath srcPaths (Just classesOut) debugOut
    if ok
        then do
            writeJarFromDir classesOut jarOutput
            putStrLn ("[DONE] jar generated: " ++ jarOutput)
        else putStrLn "[ERROR] xlang compile failed; jar packaging skipped"


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

                let exeDir = takeDirectory absExePath
                    toolkitJar = bytecodegenFile exeDir
                    srcPaths = map (resolveFromRoot rootAbs) (optInputs opts)
                    invalidInputs = invalidSourceFiles srcPaths

                case (optTarget opts, srcPaths, optOutput opts) of
                    (Just "jvm", [], _) -> do
                        putStrLn "missing input xlang file"
                        printHelp
                    (Just "jvm", _, _) | not (null invalidInputs) -> do
                        putStrLn "invalid source file extension; only .x, .xl, .xlang are allowed"
                        mapM_ (\p -> putStrLn ("  - " ++ p)) invalidInputs
                        printHelp
                    (Just "jvm", _, Just outPath0) -> do
                        let outPath = resolveFromRoot rootAbs outPath0
                        if isJarOutput outPath
                            then compileJVMToJar toolkitJar rootAbs srcPaths outPath (optDebug opts)
                            else void (compileJVM toolkitJar rootAbs srcPaths (Just outPath) (optDebug opts))
                    (Just "jvm", _, Nothing) ->
                        void (compileJVM toolkitJar rootAbs srcPaths Nothing (optDebug opts))
                    (Just _, _, _) -> do
                        putStrLn "unsupported target"
                        printHelp
                    _ -> do
                        putStrLn "missing --target"
                        printHelp

