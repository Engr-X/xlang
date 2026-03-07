module Xlang where

import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Control.Monad (when)
import System.Environment (getArgs, getExecutablePath)
import System.Directory (canonicalizePath)
import System.FilePath (takeDirectory, (</>))
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
    optCompile :: Maybe FilePath,
    optDebug :: Bool,
    optError :: Maybe String
}

defaultOptions :: Options
defaultOptions = Options False Nothing Nothing False Nothing


parseArgs :: [String] -> Options
parseArgs = go defaultOptions
  where
    go opts [] = opts
    go opts ("-h":_) = opts { optHelp = True }
    go opts ("--help":_) = opts { optHelp = True }
    go opts ("--target=jvm":xs) = go opts { optTarget = Just "jvm" } xs
    go opts ("--target":t:xs) = go opts { optTarget = Just t } xs
    go opts ("-c":path:xs) = go opts { optCompile = Just path } xs
    go opts ("-d":xs) = go opts { optDebug = True } xs
    go opts ("--debug":xs) = go opts { optDebug = True } xs
    go opts (arg:_) = opts { optHelp = True, optError = Just ("unknown arg: " ++ arg) }


printHelp :: IO ()
printHelp = putStrLn $ unlines [
    "Usage:",
    "   xlang --target=jvm -c <file>",
    "   xlang -h | --help",
    "",
    "Options:",
    "   --target=jvm     compile to JVM bytecode (required)",
    "   -c <file>        source file to compile",
    "   -d, --debug      write debug.json next to the source file",
    "   -h, --help       show this help"
    ]


compileJVM :: FilePath -> String -> Bool -> IO ()
compileJVM jarPath srcPath debugOut = do
    fileRes <- FH.readFile srcPath
    case fileRes of
        Left err -> putStrLn (UE.errorToString err)
        Right code ->
            case IR.codeToIR srcPath code of
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
                    callProcess "java" ["-jar", jarPath, "-s", jsonStr]


main :: IO ()
main = do
    args <- getArgs
    exePath <- getExecutablePath
    absExePath <- canonicalizePath exePath
    let exeDir = takeDirectory absExePath
        jarPath = bytecodegenFile exeDir
        opts = parseArgs args
        showHelp = optHelp opts || null args
    case optError opts of
        Just msg -> do
            putStrLn msg
            printHelp
        Nothing ->
            if showHelp
                then printHelp
                else case (optTarget opts, optCompile opts) of
                    (Just "jvm", Just srcPath) -> compileJVM jarPath srcPath (optDebug opts)
                    _ -> printHelp
