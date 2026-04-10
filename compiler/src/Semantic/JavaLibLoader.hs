{-# LANGUAGE OverloadedStrings #-}

module Semantic.JavaLibLoader (
    loadJavaJsonOne,
    loadJavaNonJsonBatch
) where

import Control.Exception (IOException, try)
import Control.Monad (filterM)
import Data.Aeson (FromJSON(..), Value(Object), eitherDecode, withObject, (.:), (.:?), (.!=))
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, foldl', intercalate, nub)
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import Parse.SyntaxTree (Class(..), normalizeClass)
import Semantic.NameEnv (ImportEnv(..), QName, toHiddenQName)
import Semantic.TypeEnv (FunSig(..), TypedImportEnv(..), emptyTypedImportEnv)
import Util.Exception (ErrorKind)
import Util.Type (Path, Position)
import System.Directory (doesFileExist, findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Info (os)
import System.Process (readProcessWithExitCode)

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as Map
import qualified Util.Exception as UE


newtype LibEnvelope = LibEnvelope [LibClass]


data LibClass = LibClass {
    lcPackage :: QName,
    lcClassName :: String,
    lcStaticFields :: [LibField],
    lcStaticMethods :: [LibMethod]
}


data LibField = LibField {
    lfAccess :: [String],
    lfType :: [String],
    lfName :: String,
    lfOwnerType :: String
}


data LibMethod = LibMethod {
    lmAccess :: [String],
    lmReturnType :: [String],
    lmName :: String,
    lmParamTypes :: [[String]],
    lmOwnerType :: String
}


instance FromJSON LibEnvelope where
    parseJSON = withObject "LibEnvelope" $ \o -> do
        mClasses <- o .:? "classes"
        case mClasses of
            Just cs -> pure (LibEnvelope cs)
            Nothing -> do
                one <- parseJSON (Object o)
                pure (LibEnvelope [one])


instance FromJSON LibClass where
    parseJSON = withObject "LibClass" $ \o ->
        LibClass
            <$> o .:? "package" .!= []
            <*> o .: "class"
            <*> o .:? "static_fields" .!= []
            <*> o .:? "static_methods" .!= []


instance FromJSON LibField where
    parseJSON = withObject "LibField" $ \o ->
        LibField
            <$> o .:? "access" .!= []
            <*> o .: "type"
            <*> o .: "name"
            <*> (normalizeOwnerType <$> o .:? "owner_type" .!= "class")


instance FromJSON LibMethod where
    parseJSON = withObject "LibMethod" $ \o ->
        LibMethod
            <$> o .:? "access" .!= []
            <*> o .: "return_type"
            <*> o .: "name"
            <*> o .:? "param_types" .!= []
            <*> (normalizeOwnerType <$> o .:? "owner_type" .!= "class")


loadJavaJsonOne :: [String] -> Path -> IO (Either [ErrorKind] (ImportEnv, TypedImportEnv))
loadJavaJsonOne imports libPath = do
    fileRes <- (Right <$> BL.readFile libPath) `catchIOError` (pure . Left . show)
    case fileRes of
        Left readErr ->
            pure $ Left [mkSyntax libPath ("failed to read java metadata json file: " ++ readErr)]
        Right bytes ->
            case eitherDecode bytes of
                Left e ->
                    pure $ Left [mkSyntax libPath ("failed to parse java metadata json: " ++ e)]
                Right env ->
                    pure $ Right (buildEnv libPath (filterEnvelopeByImports imports env))


loadJavaNonJsonBatch :: FilePath -> [String] -> [Path] -> IO (Either [ErrorKind] (Maybe (ImportEnv, TypedImportEnv)))
loadJavaNonJsonBatch _ _ [] = pure (Right Nothing)
loadJavaNonJsonBatch toolkitJar imports libPaths = do
    let importArgs = if null imports then [] else ["--imports", intercalate "," imports]
        readArgs = concatMap (\path -> ["--read", path]) libPaths
        args = ["-jar", toolkitJar] ++ readArgs ++ importArgs
    (ec, out, errText) <- readJavaProcessWithExitCode args ""
    case ec of
        ExitSuccess ->
            case eitherDecode (BL.pack out) of
                Left e ->
                    pure $ Left [mkSyntax "<batched-libs>" ("failed to parse batched lib metadata json: " ++ e)]
                Right env ->
                    pure $ Right (Just (buildEnv "<batched-libs>" env))
        ExitFailure code ->
            let details = trim (if null errText then out else errText)
                msg = "failed to read batched library metadata (exit " ++ show code ++ "): " ++ details
            in pure $ Left [mkSyntax "<batched-libs>" msg]
  where
    trim :: String -> String
    trim = reverse . dropWhile (`elem` ("\r\n\t " :: String)) . reverse . dropWhile (`elem` ("\r\n\t " :: String))


resolveJavaCommands :: IO [FilePath]
resolveJavaCommands = do
    mJavaHome <- lookupEnv "JAVA_HOME"
    let fromHomeRaw = case fmap normalizeJavaHome mJavaHome of
            Just home | not (null home) -> [home </> "bin" </> "java.exe", home </> "bin" </> "java"]
            _ -> []
    fromHome <- filterM doesFileExist fromHomeRaw
    fromPath <- mapMaybe id <$> mapM findExecutable ["java", "java.exe"]
    pure (nub (fromHome ++ fromPath ++ ["java", "java.exe"]))
  where
    normalizeJavaHome :: String -> String
    normalizeJavaHome = stripWrappingQuotes . trimSpaces

    trimSpaces :: String -> String
    trimSpaces = dropWhileEnd isSpace . dropWhile isSpace

    stripWrappingQuotes :: String -> String
    stripWrappingQuotes raw
        | length raw >= 2
        , let h = head raw
        , let t = last raw
        , (h == '"' && t == '"') || (h == '\'' && t == '\'') =
            init (tail raw)
        | otherwise = raw


readJavaProcessWithExitCode :: [String] -> String -> IO (ExitCode, String, String)
readJavaProcessWithExitCode args stdIn = do
    commands <- resolveJavaCommands
    go commands Nothing
  where
    go :: [FilePath] -> Maybe IOException -> IO (ExitCode, String, String)
    go [] (Just e) = ioError e
    go [] Nothing = ioError (userError "java executable not found")
    go (cmd:rest) _ = do
        res <- try (runOne cmd args stdIn) :: IO (Either IOException (ExitCode, String, String))
        case res of
            Right out -> pure out
            Left e
                | isDoesNotExistError e -> go rest (Just e)
                | otherwise -> ioError e

    runOne :: FilePath -> [String] -> String -> IO (ExitCode, String, String)
    runOne cmd cmdArgs input
        | os == "mingw32" = runViaPowerShell cmd cmdArgs input
        | otherwise = readProcessWithExitCode cmd cmdArgs input

    runViaPowerShell :: FilePath -> [String] -> String -> IO (ExitCode, String, String)
    runViaPowerShell cmd cmdArgs input = do
        psCommands <- resolvePowerShellCommands
        psCmd <- case psCommands of
            (one:_) -> pure one
            [] -> ioError (userError "powershell executable not found")
        let script = "& " ++ quotePS cmd ++ concatMap ((" " ++) . quotePS) cmdArgs
        readProcessWithExitCode psCmd ["-NoProfile", "-Command", script] input

    quotePS :: String -> String
    quotePS s = "'" ++ concatMap escapeChar s ++ "'"
      where
        escapeChar '\'' = "''"
        escapeChar c = [c]

    resolvePowerShellCommands :: IO [FilePath]
    resolvePowerShellCommands = do
        mSystemRoot <- lookupEnv "SystemRoot"
        mWinDir <- lookupEnv "WINDIR"
        let envRoots = nub [
                normalizeEnvPath root
                | Just root <- [mSystemRoot, mWinDir]
                , not (null (normalizeEnvPath root))
                ]
            fromEnvRaw = [
                root </> "System32" </> "WindowsPowerShell" </> "v1.0" </> "powershell.exe"
                | root <- envRoots
                ]
        fromEnv <- filterM doesFileExist fromEnvRaw
        fromPath <- mapMaybe id <$> mapM findExecutable ["powershell", "powershell.exe", "pwsh", "pwsh.exe"]
        pure (nub (fromEnv ++ fromPath))

    normalizeEnvPath :: String -> String
    normalizeEnvPath = stripWrappingQuotes . trimSpaces

    trimSpaces :: String -> String
    trimSpaces = dropWhileEnd isSpace . dropWhile isSpace

    stripWrappingQuotes :: String -> String
    stripWrappingQuotes raw
        | length raw >= 2
        , let h = head raw
        , let t = last raw
        , (h == '"' && t == '"') || (h == '\'' && t == '\'') =
            init (tail raw)
        | otherwise = raw


filterEnvelopeByImports :: [String] -> LibEnvelope -> LibEnvelope
filterEnvelopeByImports imports (LibEnvelope classes)
    | null normalizedImports = LibEnvelope classes
    | otherwise = LibEnvelope (filter keep classes)
  where
    normalizedImports = map normalizeImportPattern imports

    keep :: LibClass -> Bool
    keep cls =
        let fullName = intercalate "." (lcPackage cls ++ [lcClassName cls])
        in any (`globMatch` fullName) normalizedImports

    normalizeImportPattern :: String -> String
    normalizeImportPattern = map (\c -> if c == '/' then '.' else c)

    globMatch :: String -> String -> Bool
    globMatch = go
      where
        go :: String -> String -> Bool
        go [] [] = True
        go [] _ = False
        go ('*':ps) xs = go ps xs || case xs of
            [] -> False
            (_:rest) -> go ('*':ps) rest
        go (p:ps) (x:xs)
            | p == x = go ps xs
            | otherwise = False
        go _ _ = False


buildEnv :: Path -> LibEnvelope -> (ImportEnv, TypedImportEnv)
buildEnv path (LibEnvelope classes) =
    let fieldDecls = concatMap collectFieldDecls classes
        methodDecls = concatMap collectMethodDecls classes

        iVarMap = Map.fromListWith (++) [(k, pos) | (k, _, _, pos) <- fieldDecls]
        iFunMap = Map.fromListWith (++) [(k, pos) | (k, _, _, pos) <- methodDecls]
        iEnv = IEnv { file = path, iVars = iVarMap, iFuncs = iFunMap }

        tVarMap = Map.fromListWith keepFirst [(k, (t, pos, full)) | (k, full, t, pos) <- fieldDecls]
        tFunMap = foldl' insertFun Map.empty methodDecls
        tEnv0 = emptyTypedImportEnv path
        tEnv = tEnv0 { tVars = tVarMap, tFuncs = tFunMap }
    in (iEnv, tEnv)
  where
    keepFirst :: a -> a -> a
    keepFirst old _ = old

    insertFun ::
        Map QName ([FunSig], [Position], QName) ->
        (QName, QName, FunSig, [Position]) ->
        Map QName ([FunSig], [Position], QName)
    insertFun mp (keyQn, fullQn, sig, pos) =
        let entry = ([sig], pos, fullQn)
            merge (newSigs, newPos, newFull) (oldSigs, oldPos, oldFull) =
                let sigs = oldSigs ++ filter (`notElem` oldSigs) newSigs
                    poses = oldPos ++ newPos
                    full = if null oldFull then newFull else oldFull
                in (sigs, poses, full)
        in Map.insertWith merge keyQn entry mp


collectFieldDecls :: LibClass -> [(QName, QName, Class, [Position])]
collectFieldDecls cls = concatMap one (lcStaticFields cls)
  where
    one :: LibField -> [(QName, QName, Class, [Position])]
    one f =
        let fullQn = lcPackage cls ++ [lcClassName cls, lfName f]
            aliases = aliasesFor (lcPackage cls) (lcClassName cls) (lfName f) (lfOwnerType f)
            keys = map (applyVisibility (isPublicAccess (lfAccess f))) aliases
            t = parseTypeParts (lfType f)
        in [(k, fullQn, t, []) | k <- keys]


collectMethodDecls :: LibClass -> [(QName, QName, FunSig, [Position])]
collectMethodDecls cls = concatMap one (lcStaticMethods cls)
  where
    one :: LibMethod -> [(QName, QName, FunSig, [Position])]
    one m =
        let fullQn = lcPackage cls ++ [lcClassName cls, lmName m]
            aliases = aliasesFor (lcPackage cls) (lcClassName cls) (lmName m) (lmOwnerType m)
            keys = map (applyVisibility (isPublicAccess (lmAccess m))) aliases
            sig = FunSig {
                funParams = map parseTypeParts (lmParamTypes m),
                funReturn = parseTypeParts (lmReturnType m)
            }
        in [(k, fullQn, sig, []) | k <- keys]


aliasesFor :: QName -> String -> String -> String -> [QName]
aliasesFor pkg clsName memberName ownerType
    | ownerType == "xlang-top-level" =
        nub [pkg ++ [clsName, memberName], pkg ++ [memberName]]
    | otherwise =
        [pkg ++ [clsName, memberName]]


applyVisibility :: Bool -> QName -> QName
applyVisibility True qn = qn
applyVisibility False qn = toHiddenQName qn


isPublicAccess :: [String] -> Bool
isPublicAccess = any ((== "public") . map toLower)


normalizeOwnerType :: String -> String
normalizeOwnerType raw =
    let s = map toLower raw
    in case s of
        "xlang-top-level" -> "xlang-top-level"
        _ -> "xlang-class"


parseTypeParts :: [String] -> Class
parseTypeParts parts =
    let base = case parts of
            [] -> []
            [one] -> [normalizePrimitive one]
            many -> many
    in normalizeClass (Class base [])


normalizePrimitive :: String -> String
normalizePrimitive raw =
    let normalized = map toLower raw
    in Map.findWithDefault raw normalized primitiveTypeMap


primitiveTypeMap :: Map String String
primitiveTypeMap = Map.fromList [
    ("boolean", "bool"),
    ("byte", "int8"),
    ("short", "int16"),
    ("int", "int32"),
    ("long", "int64"),
    ("float", "float32"),
    ("double", "float64"),
    ("string", "String")]


mkSyntax :: Path -> String -> ErrorKind
mkSyntax p msg = UE.Syntax (UE.makeError p [] msg)

