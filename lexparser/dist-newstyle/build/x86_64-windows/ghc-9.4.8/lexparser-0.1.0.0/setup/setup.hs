{-# LANGUAGE ScopedTypeVariables #-}

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.BuildPaths (autogenComponentModulesDir)
import Distribution.Types.ComponentName (ComponentName)

import qualified Data.Map.Strict as M
import Data.List (find)

import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import System.Process (callProcess)
import Control.Exception (catch, IOException)


main :: IO ()
main = defaultMainWithHooks simpleUserHooks {
    buildHook = \pkgDesc lbi hooks flags -> do
        genAllParsers lbi
        buildHook simpleUserHooks pkgDesc lbi hooks flags,
    replHook = \pkgDesc lbi hooks flags args -> do
        genAllParsers lbi
        replHook simpleUserHooks pkgDesc lbi hooks flags args
    }


-- Generate multiple parsers. Add new entries here.
genAllParsers :: LocalBuildInfo -> IO ()
genAllParsers lbi = case pickOneCLBI lbi of
    Nothing -> pure ()
    Just clbi -> do
      let autogenDir = autogenComponentModulesDir lbi clbi
      let parsers :: [(FilePath, FilePath)]
          parsers = [
            ("Parse/ParseExpr", "src/Parse/ParseExpr.ypp"),
            ("Parse/ParseStmt", "src/Parse/ParseStmt.ypp"),
            ("Parse/ParseBlock", "src/Parse/ParseBlock.ypp"),
            ("Parse/ParseProgm", "src/Parse/ParseProgm.ypp")]

      mapM_ (genOneParser autogenDir) parsers


-- srcYpp -> (autogen)/<base>.y -> (autogen)/<base>.hs
genOneParser :: FilePath -> (FilePath, FilePath) -> IO ()
genOneParser autogenDir (baseUnderParse, srcYpp) = do
    ok <- doesFileExist srcYpp
    if not ok
        then pure ()
        else do
            let outY  = autogenDir </> (baseUnderParse ++ ".y")
            let outHs = autogenDir </> (baseUnderParse ++ ".hs")

            createDirectoryIfMissing True (takeDirectory outY)

            -- ypp -> y (CPP)
            cppOneTo srcYpp outY

            -- y -> hs (Happy)
            createDirectoryIfMissing True (takeDirectory outHs)
            callProcess "happy" ["--ghc", "-o", outHs, outY]


-- Pick a component to derive autogen dir.
-- Prefer the library component if present; otherwise pick the first available.
pickOneCLBI :: LocalBuildInfo -> Maybe ComponentLocalBuildInfo
pickOneCLBI lbi =
    let mp = componentNameMap lbi
        libFirst = case find (isLibName . fst) (M.toList mp) of
            Just (_, clbis) -> listToMaybe clbis
            Nothing -> Nothing
        anyFirst = listToMaybe (concat (M.elems mp))
    in libFirst <|> anyFirst


-- Heuristic: treat the "lib" component as preferred.
-- If your package has multiple libs, you can refine this.
isLibName :: ComponentName -> Bool
isLibName cn = show cn == "lib"

-- Preprocess *.ypp into *.y using gcc/clang/cpp fallback chain.
cppOneTo :: FilePath -> FilePath -> IO ()
cppOneTo ypp outY =
    tryCmd "gcc"   ["-E","-P","-x","c","-I","src", ypp, "-o", outY] $
    tryCmd "clang" ["-E","-P","-x","c","-I","src", ypp, "-o", outY] $
    callProcess "cpp" ["-P","-I","src", ypp, outY]

-- Try running a command; if it fails (missing cmd etc.), run fallback.
tryCmd :: FilePath -> [String] -> IO () -> IO ()
tryCmd cmd args fallback = callProcess cmd args `catch` (\(_ :: IOException) -> fallback)


-- Minimal Maybe helpers (avoid extra dependencies in Setup).
listToMaybe :: [a] -> Maybe a
listToMaybe (x:_) = Just x
listToMaybe []    = Nothing


infixr 3 <|>
(<|>) :: Maybe a -> Maybe a -> Maybe a
(<|>) (Just x) _ = Just x
(<|>) Nothing  y = y
