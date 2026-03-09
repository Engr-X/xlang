module Xlang where

import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Monad (when, void)
import System.Environment (getArgs, getExecutablePath)
import Data.Char (toLower)
import Data.List (stripPrefix)
import System.Directory (
    canonicalizePath,
    createDirectoryIfMissing,
    doesDirectoryExist,
    removeFile,
    removePathForcibly
    )
import System.FilePath (takeDirectory, (</>), takeExtension, isAbsolute)
import System.IO.Error (catchIOError)
import System.Process (callProcess)

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified IR.Lowing as IR
import qualified IR.TAC as TAC
import qualified Lowing.JVMLowing as JVML
import qualified Lowing.JVMJson as JVMJson
import qualified Util.Exception as UE
import qualified Util.FileHelper as FH


bytecodegenFile :: String -> String
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
defaultOptions = Options False False Nothing [] Nothing "." False Nothing


xlangVersion :: String
xlangVersion = "alpha 0.0.0"


parseArgs :: [String] -> Options
parseArgs = go defaultOptions
    where
        addInput :: Options -> FilePath -> Options
        addInput opts fp = opts { optInputs = optInputs opts ++ [fp] }

        isLikelyPath :: String -> Bool
        isLikelyPath s = not (null s) && head s /= '-'

        go opts [] = opts
        go opts ("-h":_) = opts { optHelp = True }
        go opts ("--help":_) = opts { optHelp = True }
        go opts ("-v":_) = opts { optVersion = True }
        go opts ("--version":_) = opts { optVersion = True }

        go opts ("--target=jvm":xs) = go opts { optTarget = Just "jvm" } xs
        go opts ("--target":t:xs) = go opts { optTarget = Just t } xs
        go opts ["--target"] = opts { optHelp = True, optError = Just "missing value for --target" }

        go opts (arg:xs)
            | Just rootDir <- stripPrefix "--root=" arg =
                go opts { optRoot = rootDir } xs
        go opts ("--root":rootDir:xs) = go opts { optRoot = rootDir } xs
        go opts ("-r":rootDir:xs) = go opts { optRoot = rootDir } xs
        go opts ["--root"] = opts { optHelp = True, optError = Just "missing value for --root" }
        go opts ["-r"] = opts { optHelp = True, optError = Just "missing value for -r" }
        -- Kotlin-style destination: -d <dir|jar>, while preserving old "-d" debug flag.
        go opts ("-d":out:xs)
            | isLikelyPath out = go opts { optOutput = Just out } xs
            | otherwise = go opts { optDebug = True } (out:xs)
        go opts ["-d"] = go opts { optDebug = True } []
        go opts ("--debug":xs) = go opts { optDebug = True } xs
        go opts ("-debug":xs) = go opts { optDebug = True } xs
        go opts (arg:xs)
            | not (null arg) && head arg /= '-' = go (addInput opts arg) xs
        go opts (arg:_) = opts { optHelp = True, optError = Just ("unknown arg: " ++ arg) }


printHelp :: IO ()
printHelp = putStrLn $ unlines [
    "Usage:",
    "   xlang --target=jvm <file.x> [more.x ...] [--root=<dir>|--root <dir>|-r <dir>] [-d <dir|jar>] [--debug|-debug|-d]",
    "   xlang -v | --version",
    "   xlang -h | --help",
    "",
    "Options:",
    "   --target=jvm     compile to JVM bytecode (required)",
    "   <file.x>         source files to compile (one or more)",
    "   --root=<dir>     source root (preferred form), e.g. --root=./abcde",
    "   --root <dir>     source root (compatible form)",
    "   -r <dir>         source root (short form)",
    "   -d <dir|jar>     output directory or jar path",
    "                    if .jar: classes are emitted to <root>/out, then packed",
    "                    manifest includes: build by xlang",
    "   --debug|-debug   write debug.json and Ir.txt next to the source file",
    "   -d               debug shorthand when used without output directory",
    "   -v, --version    print xlang version",
    "   -h, --help       show this help"
    ]


compileJVM :: FilePath -> FilePath -> [FilePath] -> Maybe FilePath -> Bool -> IO Bool
compileJVM bytecodegenJar rootPath srcPaths mOutput debugOut = do
    sourceRes <- mapM (\path -> do
        fileRes <- FH.readFile path
        pure (path, fileRes)) srcPaths
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
                    mapM_ (\(idx, (path, _)) ->
                        putStrLn (concat ["[ ", show idx, "/", show total, " ]: compile ", path])) (zip [1 :: Int ..] irPairs)
                    let irs = map snd irPairs
                        classes = JVML.jvmProgmsLowing irs
                        jsonVal = JVMJson.jProgmToJSON 8 classes
                        jsonStr = BL.unpack (encode jsonVal)
                        debugDir = case srcPaths of
                            [one] -> takeDirectory one
                            _ -> rootPath
                    when debugOut $
                        BL.writeFile (debugDir </> "debug.json") (encodePretty jsonVal)
                    when debugOut $
                        writeFile (debugDir </> "Ir.txt") (unlines (map TAC.prettyIRProgm irs))
                    case mOutput of
                        Just outPath -> do
                            createDirectoryIfMissing True outPath
                            callProcess "java" ["-jar", bytecodegenJar, "-s", jsonStr, "-o", outPath]
                        Nothing ->
                            callProcess "java" ["-jar", bytecodegenJar, "-s", jsonStr]
                    pure True


writeJarFromDir :: FilePath -> FilePath -> IO ()
writeJarFromDir classesDir jarOutput = do
    let jarDir = takeDirectory jarOutput
        manifestPath = jarDir </> ".xlang-manifest.mf"
        manifestContent = unlines [
            "Manifest-Version: 1.0",
            "Built-By: xlang",
            "Xlang-Info: build by xlang",
            ""]
    createDirectoryIfMissing True jarDir
    writeFile manifestPath manifestContent
    callProcess "jar" ["cfm", jarOutput, manifestPath, "-C", classesDir, "."]
    removeFile manifestPath `catchIOError` (\_ -> pure ())


compileJVMToJar :: FilePath -> FilePath -> [FilePath] -> FilePath -> Bool -> IO ()
compileJVMToJar bytecodegenJar rootPath srcPaths jarOutput debugOut = do
    let classesOut = rootPath </> "out"
    existed <- doesDirectoryExist classesOut
    when existed (removePathForcibly classesOut)
    createDirectoryIfMissing True classesOut
    putStrLn $ "[INFO] -d is jar; classes output dir: " ++ classesOut
    ok <- compileJVM bytecodegenJar rootPath srcPaths (Just classesOut) debugOut
    if ok
        then do
            writeJarFromDir classesOut jarOutput
            putStrLn $ "[OK] jar generated: " ++ jarOutput
            putStrLn "[DONE] jar packaging completed."
        else
            putStrLn "[ERROR] xlang compile failed; jar packaging skipped"


isJarOutput :: FilePath -> Bool
isJarOutput outPath = map toLower (takeExtension outPath) == ".jar"

resolveFromRoot :: FilePath -> FilePath -> FilePath
resolveFromRoot rootDir path
    | isAbsolute path = path
    | otherwise = rootDir </> path


main :: IO ()
main = do
    args <- getArgs
    let opts = parseArgs args
    case optError opts of
        Just msg -> do
            putStrLn msg
            printHelp
        Nothing ->
            if optVersion opts
                then putStrLn xlangVersion
                else if optHelp opts || null args
                then printHelp
                else do
                    exePath <- getExecutablePath
                    absExePath <- canonicalizePath exePath
                    rootAbs <- canonicalizePath (optRoot opts)
                    let exeDir = takeDirectory absExePath
                        jarPath = bytecodegenFile exeDir
                    case (optTarget opts, optInputs opts, optOutput opts) of
                        (Just "jvm", [], _) -> do
                            putStrLn "missing input xlang file"
                            printHelp
                        (Just "jvm", srcPath0s, Just outPath0) ->
                            let srcPaths = map (resolveFromRoot rootAbs) srcPath0s
                                outPath = resolveFromRoot rootAbs outPath0
                            in
                            if isJarOutput outPath
                                then compileJVMToJar jarPath rootAbs srcPaths outPath (optDebug opts)
                                else void (compileJVM jarPath rootAbs srcPaths (Just outPath) (optDebug opts))
                        (Just "jvm", srcPath0s, Nothing) ->
                            let srcPaths = map (resolveFromRoot rootAbs) srcPath0s
                            in
                            void (compileJVM jarPath rootAbs srcPaths Nothing (optDebug opts))
                        _ -> printHelp
