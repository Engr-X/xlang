module Semantic.LibLoader (
    loadLibEnvs,
    loadLibEnvsWithJobs
) where

import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (bracket_)
import Data.Char (toLower)
import Data.Either (lefts, rights)
import Data.List (partition)
import Semantic.NameEnv (ImportEnv)
import Semantic.TypeEnv (TypedImportEnv)
import Util.Exception (ErrorKind)
import Util.Type (Path)
import System.FilePath (takeExtension)

import qualified Semantic.JavaLibLoader as Java
import qualified Semantic.NativeLibLoader as Native


mapConcurrentlyLimit :: Int -> (a -> IO b) -> [a] -> IO [b]
mapConcurrentlyLimit jobs action xs
    | jobs <= 1 = mapM action xs
    | otherwise = do
        sem <- newQSem jobs
        forConcurrently xs $ \x ->
            bracket_ (waitQSem sem) (signalQSem sem) (action x)


loadLibEnvs :: FilePath -> [Path] -> [String] -> IO (Either [ErrorKind] ([ImportEnv], [TypedImportEnv]))
loadLibEnvs = loadLibEnvsWithJobs 1


loadLibEnvsWithJobs :: Int -> FilePath -> [Path] -> [String] -> IO (Either [ErrorKind] ([ImportEnv], [TypedImportEnv]))
loadLibEnvsWithJobs _ _ [] _ = pure (Right ([], []))
loadLibEnvsWithJobs jobs toolkitJar libPaths imports = do
    let (jsonLibPaths, nonJsonLibPaths0) = partition isJsonLibPath libPaths
        (nativeBinLibPaths, nonJsonLibPaths) = partition isNativeBinLibPath nonJsonLibPaths0

    loadedJson <- mapConcurrentlyLimit jobs (loadJsonOne imports) jsonLibPaths
    loadedNativeBin <- mapConcurrentlyLimit jobs (Native.loadNativeBinaryOne imports) nativeBinLibPaths
    loadedNonJson <- Java.loadJavaNonJsonBatch toolkitJar imports nonJsonLibPaths

    let jsonErrs = concat (lefts loadedJson)
        nativeBinErrs = concat (lefts loadedNativeBin)
        jsonOk = rights loadedJson
        nativeBinOk = rights loadedNativeBin
        nonJsonErrList = case loadedNonJson of
            Left oneErrs -> oneErrs
            Right _ -> []
        nonJsonOk = case loadedNonJson of
            Left _ -> []
            Right Nothing -> []
            Right (Just (envI, envT)) -> [(envI, envT)]
        errs = jsonErrs ++ nativeBinErrs ++ nonJsonErrList
        ok = jsonOk ++ nativeBinOk ++ nonJsonOk

    if null errs
        then
            let (importEnvs, typeds) = unzip ok
            in pure (Right (importEnvs, typeds))
        else pure (Left errs)


isJsonLibPath :: Path -> Bool
isJsonLibPath path = map toLower (takeExtension path) == ".json"

isNativeBinLibPath :: Path -> Bool
isNativeBinLibPath path =
    let ext = map toLower (takeExtension path)
    in ext == ".dll" || ext == ".lib" || ext == ".a" || ext == ".so" || ext == ".dylib"


loadJsonOne :: [String] -> Path -> IO (Either [ErrorKind] (ImportEnv, TypedImportEnv))
loadJsonOne imports libPath = do
    nativeRes <- Native.loadNativeJsonOne imports libPath
    case nativeRes of
        Right ok -> pure (Right ok)
        Left nativeErrs -> do
            javaRes <- Java.loadJavaJsonOne imports libPath
            case javaRes of
                Right ok -> pure (Right ok)
                Left javaErrs -> pure (Left (javaErrs ++ nativeErrs))
