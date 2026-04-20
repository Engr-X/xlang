module Semantic.LibLoader (
    loadLibEnvs,
    loadLibEnvsWithJobs
) where

import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (bracket_)
import Data.Char (toLower)
import Data.Either (lefts, rights)
import Data.List (foldl', isSuffixOf, partition, sortOn)
import Data.HashSet (HashSet)
import Data.Map.Strict (Map)
import Semantic.NameEnv (ImportEnv)
import Semantic.TypeEnv (TypedImportEnv)
import Util.Exception (ErrorKind)
import Util.Type (Path)
import System.FilePath (takeExtension, takeFileName)

import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
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
        (nativeBinLibPaths0, nonJsonLibPaths) = partition isNativeBinLibPath nonJsonLibPaths0
        nativeBinLibPaths = dedupNativeBinLibPaths nativeBinLibPaths0

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


dedupNativeBinLibPaths :: [Path] -> [Path]
dedupNativeBinLibPaths paths =
    let uniquePaths = stableDedupPaths paths
        picked = foldl' step Map.empty (zip [0 :: Int ..] uniquePaths)
    in map (\(p, _, _) -> p) (sortOn third (Map.elems picked))
  where
    stableDedupPaths :: [Path] -> [Path]
    stableDedupPaths xs = reverse (fst (foldl' go ([], HashSet.empty) xs))
      where
        go :: ([Path], HashSet Path) -> Path -> ([Path], HashSet Path)
        go (acc, seen) p
            | HashSet.member p seen = (acc, seen)
            | otherwise = (p : acc, HashSet.insert p seen)

    step :: Map String (Path, Int, Int) -> (Int, Path) -> Map String (Path, Int, Int)
    step acc (idx, p) =
        let key = stemKey p
            rank = variantRank p
            candidate = (p, rank, idx)
        in Map.insertWith pickBetter key candidate acc

    pickBetter :: (Path, Int, Int) -> (Path, Int, Int) -> (Path, Int, Int)
    pickBetter new@(_, rankNew, idxNew) old@(_, rankOld, idxOld)
        | rankNew < rankOld = new
        | rankNew > rankOld = old
        | idxNew < idxOld = new
        | otherwise = old

    variantRank :: Path -> Int
    variantRank p =
        let lowerName = map toLower (takeFileName p)
            ext = map toLower (takeExtension lowerName)
        in if ext == ".a" && not (".dll.a" `isSuffixOf` lowerName)
            then 0
            else if ".dll.a" `isSuffixOf` lowerName
                then 1
                else if ext == ".lib"
                    then 2
                    else if ext == ".dll" || ext == ".so" || ext == ".dylib"
                        then 3
                        else 4

    stemKey :: Path -> String
    stemKey p =
        let lowerName = map toLower (takeFileName p)
        in if ".dll.a" `isSuffixOf` lowerName
            then take (length lowerName - length (".dll.a" :: String)) lowerName
            else
                let ext = map toLower (takeExtension lowerName)
                in if null ext
                    then lowerName
                    else take (length lowerName - length ext) lowerName

    third :: (a, b, c) -> c
    third (_, _, c) = c


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
