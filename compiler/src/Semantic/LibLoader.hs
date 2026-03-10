{-# LANGUAGE OverloadedStrings #-}

module Semantic.LibLoader (
    loadLibEnvs
) where

import Data.Aeson (FromJSON(..), Value(Object), (.:), (.:?), (.!=), withObject, eitherDecode)
import Data.Char (toLower)
import Data.List (foldl', nub)
import Data.Map.Strict (Map)
import Parse.SyntaxTree (Class(..), normalizeClass)
import Semantic.NameEnv (ImportEnv(..), QName, toHiddenQName)
import Semantic.TypeEnv (FunSig(..), TypedImportEnv(..), emptyTypedImportEnv)
import Util.Exception (ErrorKind)
import Util.Type (Path, Position)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as Map
import qualified Util.Exception as UE


data LibEnvelope = LibEnvelope [LibClass]


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


loadLibEnvs :: FilePath -> [Path] -> IO (Either [ErrorKind] ([ImportEnv], [TypedImportEnv]))
loadLibEnvs _ [] = pure (Right ([], []))
loadLibEnvs toolkitJar libPaths = do
    loaded <- mapM (loadOne toolkitJar) libPaths
    let errs = concat [es | Left es <- loaded]
        ok = [x | Right x <- loaded]
    if null errs
        then
            let (imports, typeds) = unzip ok
            in pure (Right (imports, typeds))
        else pure (Left errs)


loadOne :: FilePath -> Path -> IO (Either [ErrorKind] (ImportEnv, TypedImportEnv))
loadOne toolkitJar libPath = do
    (ec, out, errText) <- readProcessWithExitCode "java" ["-jar", toolkitJar, "--read", libPath] ""
    case ec of
        ExitSuccess ->
            case eitherDecode (BL.pack out) of
                Left e ->
                    pure $ Left [mkSyntax libPath ("failed to parse lib metadata json: " ++ e)]
                Right env ->
                    pure $ Right (buildEnv libPath env)
        ExitFailure code ->
            let details = trim (if null errText then out else errText)
                msg = "failed to read library metadata (exit " ++ show code ++ "): " ++ details
            in pure $ Left [mkSyntax libPath msg]
    where
        trim :: String -> String
        trim = reverse . dropWhile (`elem` ("\r\n\t " :: String)) . reverse . dropWhile (`elem` ("\r\n\t " :: String))


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
    | ownerType == "wrapped-class" =
        nub [pkg ++ [clsName, memberName], pkg ++ [memberName]]
    | otherwise =
        [pkg ++ [clsName, memberName]]


applyVisibility :: Bool -> QName -> QName
applyVisibility True qn = qn
applyVisibility False qn = toHiddenQName qn


isPublicAccess :: [String] -> Bool
isPublicAccess accs = any ((== "public") . map toLower) accs


normalizeOwnerType :: String -> String
normalizeOwnerType raw =
    let s = map toLower raw
    in case s of
        "class" -> "class"
        "wrapped-class" -> "wrapped-class"
        "class-wrapped" -> "wrapped-class"
        _ -> "class"


parseTypeParts :: [String] -> Class
parseTypeParts parts =
    let dims = length (takeWhile (== "[]") (reverse parts))
        baseRaw = take (length parts - dims) parts
        base = case baseRaw of
            [one] -> [normalizePrimitive one]
            many -> many
        baseCls = normalizeClass (Class base [])
    in if dims > 0 then Array baseCls dims else baseCls


normalizePrimitive :: String -> String
normalizePrimitive raw = case map toLower raw of
    "boolean" -> "bool"
    "byte" -> "int8"
    "short" -> "int16"
    "int" -> "int32"
    "long" -> "int64"
    "float" -> "float32"
    "double" -> "float64"
    other -> other


mkSyntax :: Path -> String -> ErrorKind
mkSyntax path msg = UE.Syntax (UE.makeError path [] msg)
