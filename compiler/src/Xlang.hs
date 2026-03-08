module Xlang where

import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Monad (when)
import System.Environment (getArgs, getExecutablePath)
import Data.Char (toLower)
import Data.List (stripPrefix)
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>), takeExtension, isAbsolute)
import System.Process (callProcess)

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified IR.Lowing as IR
import qualified IR.TAC as TAC
import qualified Lowing.JVMLowing as JVML
import qualified Lowing.JVMJson as JVMJson
import qualified Util.Exception as UE
import qualified Util.FileHelper as FH


bytecodegenFile :: String -> String
bytecodegenFile exePath = map slash exePath ++ "/tools/BytecodeGenerator-alpha.jar"
  where
    slash '\\' = '/'
    slash c = c


data Options = Options {
    optHelp :: Bool,
    optTarget :: Maybe String,
    optInputs :: [FilePath],
    optOutput :: Maybe FilePath,
    optRoot :: FilePath,
    optDebug :: Bool,
    optError :: Maybe String
}

defaultOptions :: Options
defaultOptions = Options False Nothing [] Nothing "." False Nothing


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
    "   xlang --target=jvm <file.x> [--root=<dir>|--root <dir>|-r <dir>] [-d <dir|jar>] [--debug|-debug|-d]",
    "   xlang -h | --help",
    "",
    "Options:",
    "   --target=jvm     compile to JVM bytecode (required)",
    "   <file.x>         source file to compile (currently one file only)",
    "   --root=<dir>     source root (preferred form), e.g. --root=./abcde",
    "   --root <dir>     source root (compatible form)",
    "   -r <dir>         source root (short form)",
    "   -d <dir|jar>     output destination (jar is not supported yet)",
    "   --debug|-debug   write debug.json and Ir.txt next to the source file",
    "   -d               debug shorthand when used without output directory",
    "   -h, --help       show this help"
    ]


compileJVM :: FilePath -> FilePath -> String -> Maybe FilePath -> Bool -> IO ()
compileJVM jarPath rootPath srcPath mOutput debugOut = do
    fileRes <- FH.readFile srcPath
    case fileRes of
        Left err -> putStrLn (UE.errorToString err)
        Right code ->
            case IR.codeToIRSingleWithRoot rootPath srcPath code of
                Left errs -> mapM_ (putStrLn . UE.errorToString) errs
                Right (ir, warns) -> do
                    mapM_ print warns
                    let classes = JVML.jvmProgmLowing ir
                        jsonVal = JVMJson.jProgmToJSON 8 classes
                        jsonStr = BL.unpack (encode jsonVal)
                    when debugOut $
                        BL.writeFile (takeDirectory srcPath </> "debug.json") (encodePretty jsonVal)
                    when debugOut $
                        writeFile (takeDirectory srcPath </> "Ir.txt") (TAC.prettyIRProgm ir)
                    case mOutput of
                        Just outPath -> do
                            createDirectoryIfMissing True outPath
                            callProcess "java" ["-jar", jarPath, "-s", jsonStr, "-o", outPath]
                        Nothing ->
                            callProcess "java" ["-jar", jarPath, "-s", jsonStr]


isJarOutput :: FilePath -> Bool
isJarOutput outPath = map toLower (takeExtension outPath) == ".jar"

resolveFromRoot :: FilePath -> FilePath -> FilePath
resolveFromRoot rootDir path
    | isAbsolute path = path
    | otherwise = rootDir </> path


main :: IO ()
main = do
    args <- getArgs
    exePath <- getExecutablePath
    absExePath <- canonicalizePath exePath
    let opts = parseArgs args
    rootAbs <- canonicalizePath (optRoot opts)
    let exeDir = takeDirectory absExePath
        jarPath = bytecodegenFile exeDir
        showHelp = optHelp opts || null args
    case optError opts of
        Just msg -> do
            putStrLn msg
            printHelp
        Nothing ->
            if showHelp
                then printHelp
                else case (optTarget opts, optInputs opts, optOutput opts) of
                    (Just "jvm", [srcPath0], Just outPath0) ->
                        let srcPath = resolveFromRoot rootAbs srcPath0
                            outPath = resolveFromRoot rootAbs outPath0
                        in
                        if isJarOutput outPath
                            then putStrLn "output to .jar is not supported yet; please use a directory path"
                            else compileJVM jarPath rootAbs srcPath (Just outPath) (optDebug opts)
                    (Just "jvm", [srcPath0], Nothing) ->
                        let srcPath = resolveFromRoot rootAbs srcPath0
                        in
                        compileJVM jarPath rootAbs srcPath Nothing (optDebug opts)
                    (Just "jvm", [], _) -> do
                        putStrLn "missing input xlang file"
                        printHelp
                    (Just "jvm", _, _) -> do
                        putStrLn "currently only one input xlang file is supported"
                        printHelp
                    _ -> printHelp
