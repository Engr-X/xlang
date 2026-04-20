{-# LANGUAGE OverloadedStrings #-}

module Semantic.NativeLibLoader (
    loadNativeJsonOne,
    loadNativeAsmOne,
    loadNativeBinaryOne
) where

import Control.Monad (filterM)
import Data.Aeson (FromJSON(..), Result(..), Value(..), eitherDecode, fromJSON, withObject, (.:), (.:?), (.!=))
import Data.Aeson.Types (Parser)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Char (isDigit, isSpace, toLower)
import Data.HashSet (HashSet)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (dropWhileEnd, foldl', intercalate, isInfixOf, isPrefixOf, isSuffixOf, nub, stripPrefix)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Numeric (readHex)
import Parse.SyntaxTree (Class(..), classDemangleEither, normalizeClass)
import Semantic.NameEnv (ImportEnv(..), QName, toHiddenQName)
import Semantic.TypeEnv (FunSig(..), TypedImportEnv(..), emptyTypedImportEnv)
import Util.Exception (ErrorKind)
import Util.Basic (demangleNameFromMetaSymbol)
import Util.Type (Path, Position)

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import qualified Util.Exception as UE
import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.FilePath (takeExtension, takeFileName)
import System.IO.Error (catchIOError)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)


newtype NativeEnvelope = NativeEnvelope [NativeClass]


data NativeClass = NativeClass {
    ncQName :: QName,
    ncAttrs :: [NativeField],
    ncMethods :: [NativeMethod]
}


data NativeField = NativeField {
    nfAccessMask :: Int,
    nfType :: Class,
    nfName :: String,
    nfOwnerType :: OwnerType
}


data NativeMethod = NativeMethod {
    nmAccessMask :: Int,
    nmReturnType :: Class,
    nmName :: String,
    nmParamTypes :: [Class],
    nmOwnerType :: OwnerType
}


data OwnerType
    = OwnerClass
    | OwnerTopLevel
    deriving (Eq, Show)


accessPublicBit :: Int
accessPublicBit = 0


accessPrivateBit :: Int
accessPrivateBit = 1


accessProtectedBit :: Int
accessProtectedBit = 2


accessStaticBit :: Int
accessStaticBit = 3


accessFinalBit :: Int
accessFinalBit = 4


accessInlineBit :: Int
accessInlineBit = 5


ownerTypeClassCode :: Int
ownerTypeClassCode = 0


ownerTypeTopLevelCode :: Int
ownerTypeTopLevelCode = 1


instance FromJSON NativeEnvelope where
    parseJSON = withObject "NativeEnvelope" $ \o -> do
        target <- fmap (map toLower) <$> o .:? "target"
        case target of
            Just "x64" -> NativeEnvelope <$> o .: "classes"
            Just other -> fail ("native metadata target must be x64, got: " ++ other)
            Nothing -> fail "native metadata missing target=x64"


instance FromJSON NativeClass where
    parseJSON = withObject "NativeClass" $ \o -> do
        qnVal <- o .: "class"
        attrs <- o .:? "attributes" .!= []
        methods <- o .:? "methods" .!= []
        NativeClass
            <$> parseQName qnVal
            <*> pure attrs
            <*> pure methods


instance FromJSON NativeField where
    parseJSON = withObject "NativeField" $ \o -> do
        accessVal <- o .:? "access" .!= Number 0
        ownerVal <- o .:? "owner_type" .!= Number (fromIntegral ownerTypeClassCode)
        tyVal <- o .: "attr_type"
        NativeField
            <$> parseAccessMask accessVal
            <*> parseTypeValue tyVal
            <*> o .: "attr_name"
            <*> parseOwnerType ownerVal


instance FromJSON NativeMethod where
    parseJSON = withObject "NativeMethod" $ \o -> do
        accessVal <- o .:? "access" .!= Number 0
        ownerVal <- o .:? "owner_type" .!= Number (fromIntegral ownerTypeClassCode)
        mRetVal <- (o .:? "return" >>= \m -> case m of
            Just v -> pure (Just v)
            Nothing -> o .:? "return_type")
        paramsVal <- (o .:? "param_types" .!= Array mempty)
        allParamTypes <- parseParamTypes paramsVal
        (retTy, paramsOnly) <- case mRetVal of
            Just retVal -> do
                r <- parseTypeValue retVal
                pure (r, allParamTypes)
            Nothing -> case allParamTypes of
                [] -> fail "param_types must include return type as the last element when return/return_type is omitted"
                _ -> pure (last allParamTypes, init allParamTypes)
        NativeMethod
            <$> parseAccessMask accessVal
            <*> pure retTy
            <*> o .: "name"
            <*> pure paramsOnly
            <*> parseOwnerType ownerVal


loadNativeJsonOne :: [String] -> Path -> IO (Either [ErrorKind] (ImportEnv, TypedImportEnv))
loadNativeJsonOne imports libPath = do
    fileRes <- (Right <$> BL.readFile libPath) `catchReadError` (pure . Left . show)
    case fileRes of
        Left readErr ->
            pure $ Left [mkSyntax libPath ("failed to read native metadata json file: " ++ readErr)]
        Right bytes ->
            case eitherDecode bytes of
                Left e ->
                    pure $ Left [mkSyntax libPath ("failed to parse native metadata json: " ++ e)]
                Right env ->
                    pure $ Right (buildEnv libPath (filterEnvelopeByImports imports env))
  where
    catchReadError :: IO a -> (IOError -> IO a) -> IO a
    catchReadError = catchIOError


loadNativeAsmOne :: [String] -> Path -> IO (Either [ErrorKind] (ImportEnv, TypedImportEnv))
loadNativeAsmOne imports libPath = do
    fileRes <- (Right <$> readFile libPath) `catchReadError` (pure . Left . show)
    case fileRes of
        Left readErr ->
            pure $ Left [mkSyntax libPath ("failed to read native metadata asm file: " ++ readErr)]
        Right txt ->
            case parseNativeAsmEnvelope txt of
                Left e ->
                    pure $ Left [mkSyntax libPath ("failed to parse native metadata asm: " ++ e)]
                Right env ->
                    pure $ Right (buildEnv libPath (filterEnvelopeByImports imports env))
  where
    catchReadError :: IO a -> (IOError -> IO a) -> IO a
    catchReadError = catchIOError


loadNativeBinaryOne :: [String] -> Path -> IO (Either [ErrorKind] (ImportEnv, TypedImportEnv))
loadNativeBinaryOne imports libPath = do
    symRes <- readNativeSymbols libPath
    case symRes of
        Left e ->
            pure $ Left [mkSyntax libPath ("failed to read native symbols from binary: " ++ e)]
        Right syms -> do
            mEmbeddedEnv <- if hasClassInfoSymbols syms
                then readEmbeddedClassInfoEnvelope libPath
                else pure Nothing
            let symbolEnv = parseNativeSymbolsEnvelope syms
                env = fromMaybe symbolEnv mEmbeddedEnv
            pure $ Right (buildEnv libPath (filterEnvelopeByImports imports env))


{-# NOINLINE symbolReaderAvailableCache #-}
symbolReaderAvailableCache :: IORef (Map String Bool)
symbolReaderAvailableCache = unsafePerformIO (newIORef Map.empty)


isSymbolReaderAvailable :: String -> IO Bool
isSymbolReaderAvailable cmd = do
    cached <- readIORef symbolReaderAvailableCache
    case Map.lookup cmd cached of
        Just ok -> pure ok
        Nothing -> do
            ok <- maybe False (const True) <$> findExecutable cmd
            atomicModifyIORef' symbolReaderAvailableCache $ \m ->
                (Map.insert cmd ok m, ())
            pure ok


readNativeSymbols :: Path -> IO (Either String [String])
readNativeSymbols libPath
    | shouldSkipSystemLibSymbolScan libPath = pure (Right [])
    | otherwise = do
        readers <- filterM (isSymbolReaderAvailable . fst) (symbolReadersForPath libPath)
        runCandidates False [] readers
  where
    runCandidates :: Bool -> [String] -> [(String, [String])] -> IO (Either String [String])
    runCandidates hadSuccess errs [] =
        if hadSuccess
            then pure (Right [])
            else pure (Left (if null errs then "no symbol reader available" else intercalate " | " (reverse errs)))
    runCandidates hadSuccess errs ((cmd, args) : rest) = do
        outRes <- (Right <$> readProcessWithExitCode cmd args "") `catchIOError` (\e -> pure (Left (show e)))
        case outRes of
            Left runErr ->
                runCandidates hadSuccess (concat [cmd, ": ", runErr] : errs) rest
            Right (ExitSuccess, out, _) ->
                let syms = parseMangledSymbolsFromText out
                in if null syms
                    then runCandidates True errs rest
                    else pure (Right syms)
            Right (ExitFailure _, _, errTxt) ->
                runCandidates hadSuccess (concat [cmd, ": ", trim errTxt] : errs) rest

    symbolReadersForPath :: Path -> [(String, [String])]
    symbolReadersForPath path =
        let ext = map toLower (takeExtension path)
            nmReaders = [
                ("llvm-nm", ["-g", "--defined-only", path]),
                ("nm", ["-g", "--defined-only", path])]
            dumpbinReaders = case ext of
                ".dll" -> [("dumpbin", ["/exports", path])]
                ".lib" -> [("dumpbin", ["/linkermember:1", path]), ("dumpbin", ["/symbols", path])]
                _ -> []
        in case ext of
            ".dll" -> dumpbinReaders ++ nmReaders
            ".lib" -> dumpbinReaders ++ nmReaders
            _ -> nmReaders ++ dumpbinReaders


shouldSkipSystemLibSymbolScan :: Path -> Bool
shouldSkipSystemLibSymbolScan libPath =
    let stem = normalizedLibStem (map toLower (takeFileName libPath))
        knownPrefixes = [
            "libkernel32", "kernel32",
            "libmsvcrt", "msvcrt",
            "libmingw32", "mingw32",
            "libmingwex", "mingwex",
            "libgcc_eh", "libgcc",
            "libstdc++", "libc", "libm",
            "crt2", "crtbegin", "crtend"]
    in any (`isPrefixOf` stem) knownPrefixes
  where
    normalizedLibStem :: String -> String
    normalizedLibStem name
        | ".dll.a" `isSuffixOf` name = take (length name - length (".dll.a" :: String)) name
        | otherwise =
            let ext = takeExtension name
            in if null ext
                then name
                else take (length name - length ext) name


hasClassInfoSymbols :: [String] -> Bool
hasClassInfoSymbols = any isClassInfoSymbol
  where
    isClassInfoSymbol :: String -> Bool
    isClassInfoSymbol s = "_info" `isSuffixOf` s || "_info_len" `isSuffixOf` s


readEmbeddedClassInfoEnvelope :: Path -> IO (Maybe NativeEnvelope)
readEmbeddedClassInfoEnvelope libPath = do
    bytesRes <- (Right <$> BL.readFile libPath) `catchIOError` (\_ -> pure (Left ()))
    case bytesRes of
        Left _ -> pure Nothing
        Right bytes -> do
            let objects = extractJsonObjectsFromBinary (BL.unpack bytes)
                classes = dedupClassesByQName (concatMap decodeOneJsonObject objects)
            if null classes
                then pure Nothing
                else pure (Just (NativeEnvelope classes))
  where
    decodeOneJsonObject :: String -> [NativeClass]
    decodeOneJsonObject rawJson =
        case eitherDecode (BL.pack rawJson) :: Either String NativeClass of
            Right cls -> [cls]
            Left _ -> case eitherDecode (BL.pack rawJson) :: Either String NativeEnvelope of
                Right (NativeEnvelope classes) -> classes
                Left _ -> []

    dedupClassesByQName :: [NativeClass] -> [NativeClass]
    dedupClassesByQName classes =
        Map.elems (Map.fromListWith keepFirst [(ncQName cls, cls) | cls <- classes])

    keepFirst :: NativeClass -> NativeClass -> NativeClass
    keepFirst old _ = old


extractJsonObjectsFromBinary :: String -> [String]
extractJsonObjectsFromBinary s = case s of
    [] -> []
    ('{':_) -> case parseOne s of
        Just (obj, rest) ->
            let xs = if isLikelyClassInfoJson obj then [obj] else []
            in xs ++ extractJsonObjectsFromBinary rest
        Nothing -> extractJsonObjectsFromBinary (tail s)
    (_:rest) -> extractJsonObjectsFromBinary rest
  where
    maxLen = 1 * 1024 * 1024

    parseOne :: String -> Maybe (String, String)
    parseOne ('{':rest) = go 1 False False 1 ['{'] rest
    parseOne _ = Nothing

    go :: Int -> Bool -> Bool -> Int -> String -> String -> Maybe (String, String)
    go _ _ _ _ _ [] = Nothing
    go depth inString escaped count acc (c:rest)
        | count > maxLen = Nothing
        | inString =
            if escaped
                then go depth True False (count + 1) (c : acc) rest
                else if c == '\\'
                    then go depth True True (count + 1) (c : acc) rest
                    else if c == '"'
                        then go depth False False (count + 1) (c : acc) rest
                        else go depth True False (count + 1) (c : acc) rest
        | c == '"' = go depth True False (count + 1) (c : acc) rest
        | c == '{' = go (depth + 1) False False (count + 1) (c : acc) rest
        | c == '}' =
            if depth == 1
                then Just (reverse (c : acc), rest)
                else go (depth - 1) False False (count + 1) (c : acc) rest
        | otherwise = go depth False False (count + 1) (c : acc) rest

    isLikelyClassInfoJson :: String -> Bool
    isLikelyClassInfoJson txt =
        "\"class\"" `isInfixOf` txt &&
        ("\"attributes\"" `isInfixOf` txt || "\"methods\"" `isInfixOf` txt || "\"classes\"" `isInfixOf` txt)


parseMangledSymbolsFromText :: String -> [String]
parseMangledSymbolsFromText txt = stableDedup (concatMap symbolsInLine (lines txt))
  where
    stableDedup :: [String] -> [String]
    stableDedup xs = reverse (fst (foldl' go ([], HashSet.empty) xs))
      where
        go :: ([String], HashSet String) -> String -> ([String], HashSet String)
        go (acc, seen) s
            | HashSet.member s seen = (acc, seen)
            | otherwise = (s : acc, HashSet.insert s seen)

    symbolsInLine :: String -> [String]
    symbolsInLine line = mapMaybe tokenToSymbol (words line)

    tokenToSymbol :: String -> Maybe String
    tokenToSymbol raw =
        let tok0 = trimToken raw
            tok1 = case stripPrefix "__imp_" tok0 of
                Just s -> s
                Nothing -> tok0
        in if "_X" `isPrefixOf` tok1 then Just tok1 else Nothing

    trimToken :: String -> String
    trimToken =
        dropWhile (`elem` ("[,(:<>{}" :: String)) .
        dropWhileEnd (`elem` (",;:])>}" :: String))


parseNativeSymbolsEnvelope :: [String] -> NativeEnvelope
parseNativeSymbolsEnvelope symbols =
    let parsed = mapMaybe parseOne symbols
        hasClassInfo = any (\p -> symSuffix p == SymInfo) parsed
        funcModBases = [
            symBase p |
            p <- parsed,
            symSuffix p == SymModifiers,
            length (symQName p) >= 2,
            not (null (symSig p))]
        classInfoOwners = [
            symQName p |
            p <- parsed,
            symSuffix p == SymInfo,
            not (null (symQName p)),
            null (symSig p)]

        methods = mapMaybe (toMethod hasClassInfo funcModBases) parsed
        fields = mapMaybe (toField hasClassInfo funcModBases) parsed

        classOwners = nub (classInfoOwners ++ map fst classMods ++ map (init . nmQNameWithName) methods ++ map (init . nfQNameWithName) fields)
        classMods = [(symQName p, (0 :: Int)) | p <- parsed, symSuffix p == SymModifiers, not (null (symQName p)), null (symSig p)]

        methodsByOwner = foldl' (\mp m -> Map.insertWith (++) (init (nmQNameWithName m)) [m] mp) Map.empty methods
        fieldsByOwner = foldl' (\mp f -> Map.insertWith (++) (init (nfQNameWithName f)) [f] mp) Map.empty fields
        mkClass qn = NativeClass {
            ncQName = qn,
            ncAttrs = map dropFieldQName (Map.findWithDefault [] qn fieldsByOwner),
            ncMethods = map dropMethodQName (Map.findWithDefault [] qn methodsByOwner)
        }
    in NativeEnvelope (map mkClass classOwners)
  where
    parseOne :: String -> Maybe ParsedSym
    parseOne rawSym =
        let (base, suf) = classifySuffix rawSym
        in case demangleNameFromMetaSymbol rawSym of
            Right (qn, sig) ->
                Just ParsedSym {
                    symRaw = rawSym,
                    symBase = base,
                    symQName = qn,
                    symSig = sig,
                    symSuffix = suf
                }
            Left _ -> Nothing

    classifySuffix :: String -> (String, SymSuffix)
    classifySuffix s
        | "_info_len" `isSuffixOf` s = (dropSuffix "_info_len" s, SymInfoLen)
        | "_info" `isSuffixOf` s = (dropSuffix "_info" s, SymInfo)
        | "_modifiers" `isSuffixOf` s = (dropSuffix "_modifiers" s, SymModifiers)
        | "_parrentsLen" `isSuffixOf` s = (dropSuffix "_parrentsLen" s, SymParrentsLen)
        | "_parrentsData" `isSuffixOf` s = (dropSuffix "_parrentsData" s, SymParrentsData)
        | "_parrents" `isSuffixOf` s = (dropSuffix "_parrents" s, SymParrents)
        | otherwise = (s, SymPlain)

    dropSuffix :: String -> String -> String
    dropSuffix suf s = take (length s - length suf) s

    toMethod :: Bool -> [String] -> ParsedSym -> Maybe NativeMethodWithQName
    toMethod hasInfo methodBases p
        | symSuffix p /= SymPlain = Nothing
        | length (symQName p) < 2 = Nothing
        | null (symSig p) = Nothing
        | not (isMethodLike hasInfo methodBases p) = Nothing
        | last (symQName p) == "@clinit" = Nothing
        | otherwise =
            let fullSig = symSig p
                retTy = last fullSig
                paramTys = init fullSig
            in Just NativeMethodWithQName {
                nmqQName = symQName p,
                nmqMethod = NativeMethod {
                    nmAccessMask = bitMask accessPublicBit .|. bitMask accessStaticBit,
                    nmReturnType = retTy,
                    nmName = last (symQName p),
                    nmParamTypes = paramTys,
                    nmOwnerType = OwnerTopLevel
                }
            }

    isMethodLike :: Bool -> [String] -> ParsedSym -> Bool
    isMethodLike hasInfo methodBases p =
        symBase p `elem` methodBases || length (symSig p) > 1 || (hasInfo && not (null (symSig p)))

    toField :: Bool -> [String] -> ParsedSym -> Maybe NativeFieldWithQName
    toField hasInfo methodBases p
        | symSuffix p /= SymPlain = Nothing
        | length (symQName p) < 2 = Nothing
        | isMethodLike hasInfo methodBases p = Nothing
        | hasInfo && not (null (symSig p)) = Nothing
        | otherwise = case symSig p of
            [fieldTy]
                | let fieldName = last (symQName p)
                , not (isInternalFieldName fieldName) ->
                    Just NativeFieldWithQName {
                        nfqQName = symQName p,
                        nfqField = NativeField {
                            nfAccessMask = bitMask accessPublicBit .|. bitMask accessStaticBit,
                            nfType = fieldTy,
                            nfName = fieldName,
                            nfOwnerType = OwnerTopLevel
                        }
                    }
            _ -> Nothing

    isInternalFieldName :: String -> Bool
    isInternalFieldName name = name == "@isInit" || name == "@clinit"


parseNativeAsmEnvelope :: String -> Either String NativeEnvelope
parseNativeAsmEnvelope txt =
    let blocks = collectLabelBlocks txt
        blockMap = Map.fromList blocks
        infoClasses = mapMaybe (parseClassInfo blockMap) blocks
    in if null infoClasses
        then parseLegacyAsmEnvelope blocks
        else Right (NativeEnvelope infoClasses)
  where
    parseClassInfo :: Map String [String] -> (String, [String]) -> Maybe NativeClass
    parseClassInfo blockMap (label, body)
        | not ("_info" `isSuffixOf` label) = Nothing
        | "_info_len" `isSuffixOf` label = Nothing
        | otherwise = case demangleNameFromMetaSymbol label of
            Right (qn, sig)
                | not (null qn) && null sig -> do
                    dataLabel <- parseInfoDataLabel body
                    dataBody <- Map.lookup dataLabel blockMap
                    rawJson <- extractDataString dataBody
                    let baseSym = trimMetaSuffixLabel label
                        mLenBody = Map.lookup (baseSym ++ "_info_len") blockMap
                        payload = case mLenBody >>= parseInfoLen of
                            Just n | n > 0 -> take n rawJson
                            _ -> rawJson
                    decodeInfoClass payload
            _ -> Nothing

    parseInfoDataLabel :: [String] -> Maybe String
    parseInfoDataLabel body =
        listToMaybe (mapMaybe one body)
      where
        one :: String -> Maybe String
        one rawLine =
            let ws = words (map sanitize rawLine)
                cands = filter ("_X" `isPrefixOf`) ws
            in case reverse cands of
                (x:_) -> Just x
                _ -> Nothing

    parseInfoLen :: [String] -> Maybe Int
    parseInfoLen body = listToMaybe (mapMaybe parseMovMask body)

    parseMovMask :: String -> Maybe Int
    parseMovMask line =
        let s = map toLower (trim line)
            ws = words (map sanitize line)
            isMov = case words s of
                [] -> False
                (op:_) -> op == "mov"
        in if not isMov
            then Nothing
            else listToMaybeInt (mapMaybe parseIntToken (reverse ws))

    parseIntToken :: String -> Maybe Int
    parseIntToken rawTok =
        let tok = stripPunc rawTok
            lowerTok = map toLower tok
        in case parseSignedHex lowerTok of
            Just n -> Just n
            Nothing -> case reads tok of
                [(n, "")] -> Just n
                _ -> Nothing

    parseSignedHex :: String -> Maybe Int
    parseSignedHex tok0 = do
        let (sign, tok) = case tok0 of
                ('+':xs) -> (1, xs)
                ('-':xs) -> (-1, xs)
                _ -> (1, tok0)
        hexPart <- strip0x tok
        case readHex hexPart of
            [(n, "")] -> Just (sign * n)
            _ -> Nothing

    strip0x :: String -> Maybe String
    strip0x ('0':'x':xs) = Just xs
    strip0x _ = Nothing

    trimMetaSuffixLabel :: String -> String
    trimMetaSuffixLabel s =
        foldl' (flip dropKnownSuffix) s ["_info_len", "_info", "_modifiers", "_parrentsLen", "_parrents", "_parrentsData"]

    dropKnownSuffix :: String -> String -> String
    dropKnownSuffix suf s
        | suf `isSuffixOf` s = take (length s - length suf) s
        | otherwise = s

    sanitize :: Char -> Char
    sanitize c
        | isDigit c = c
        | c `elem` ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_+-xX@$." :: String) = c
        | otherwise = ' '

    stripPunc :: String -> String
    stripPunc = dropWhileEnd (`elem` [',', ';', ':']) . dropWhile (`elem` ['[', ']', '(', ')'])

    extractDataString :: [String] -> Maybe String
    extractDataString body = listToMaybe (mapMaybe extractOne body)
      where
        extractOne :: String -> Maybe String
        extractOne rawLine =
            let line = trim rawLine
                quoted = dropWhile (/= '"') line
            in if null quoted
                then Nothing
                else case reads quoted of
                    [(s, _)] -> Just s
                    _ -> Nothing

    decodeInfoClass :: String -> Maybe NativeClass
    decodeInfoClass rawJson =
        case eitherDecode (BL.pack rawJson) :: Either String NativeClass of
            Right cls -> Just cls
            Left _ -> case eitherDecode (BL.pack rawJson) :: Either String NativeEnvelope of
                Right (NativeEnvelope classes) -> listToMaybe classes
                Left _ -> Nothing

    listToMaybeInt :: [Int] -> Maybe Int
    listToMaybeInt [] = Nothing
    listToMaybeInt (x:_) = Just x

parseLegacyAsmEnvelope :: [(String, [String])] -> Either String NativeEnvelope
parseLegacyAsmEnvelope blocks =
    let blockMap = Map.fromList blocks
        classMods = mapMaybe parseClassModifier blocks
        funMods = mapMaybe (parseMethodModifier blockMap) blocks
        attrs = mapMaybe parseFieldData blocks
        methodOwners = map (init . nmQNameWithName) funMods
        fieldOwners = map (init . nfQNameWithName) attrs
        classOwners = map fst classMods
        allOwners = nub (classOwners ++ methodOwners ++ fieldOwners)
        methodsByOwner = foldl' (\mp m -> Map.insertWith (++) (init (nmQNameWithName m)) [m] mp) Map.empty funMods
        fieldsByOwner = foldl' (\mp f -> Map.insertWith (++) (init (nfQNameWithName f)) [f] mp) Map.empty attrs
        mkClass qn = NativeClass {
            ncQName = qn,
            ncAttrs = map dropFieldQName (Map.findWithDefault [] qn fieldsByOwner),
            ncMethods = map dropMethodQName (Map.findWithDefault [] qn methodsByOwner)
        }
    in Right (NativeEnvelope (map mkClass allOwners))
  where
    parseClassModifier :: (String, [String]) -> Maybe (QName, Int)
    parseClassModifier (label, body)
        | not ("_modifiers" `isSuffixOf` label) = Nothing
        | otherwise = case demangleNameFromMetaSymbol label of
            Right (qn, sig)
                | not (null qn) && null sig ->
                    let metaMask = fromMaybeAccessMask (extractMask body)
                    in Just (qn, metaMask)
            _ -> Nothing

    parseMethodModifier :: Map String [String] -> (String, [String]) -> Maybe NativeMethodWithQName
    parseMethodModifier bm (label, body)
        | not ("_modifiers" `isSuffixOf` label) = Nothing
        | otherwise = case demangleNameFromMetaSymbol label of
            Right (qn, sig)
                | length qn >= 2 && not (null sig) ->
                    let baseSym = trimMetaSuffixLabel label
                        maybeBody = Map.lookup baseSym bm
                    in if isLikelyDataBlock maybeBody
                        then Nothing
                        else
                            let metaMask = fromMaybeAccessMask (extractMask body)
                                access = accessMaskFromMetaMask metaMask
                                owner = ownerTypeFromFuncMetaMask metaMask
                                retTy = last sig
                                paramTys = init sig
                            in Just NativeMethodWithQName {
                                nmqQName = qn,
                                nmqMethod = NativeMethod {
                                    nmAccessMask = access,
                                    nmReturnType = retTy,
                                    nmName = last qn,
                                    nmParamTypes = paramTys,
                                    nmOwnerType = owner
                                }
                            }
            _ -> Nothing

    parseFieldData :: (String, [String]) -> Maybe NativeFieldWithQName
    parseFieldData (label, body)
        | isMetaSuffixLabel label = Nothing
        | not (isDataBlock body) = Nothing
        | otherwise = case demangleNameFromMetaSymbol label of
            Right (qn, [fieldTy])
                | length qn >= 2
                , let fieldName = last qn
                , not (isInternalFieldName fieldName) ->
                    Just NativeFieldWithQName {
                        nfqQName = qn,
                        nfqField = NativeField {
                            nfAccessMask = bitMask accessPublicBit .|. bitMask accessStaticBit,
                            nfType = fieldTy,
                            nfName = fieldName,
                            nfOwnerType = OwnerClass
                        }
                    }
            _ -> Nothing

    fromMaybeAccessMask :: Maybe Int -> Int
    fromMaybeAccessMask (Just m) = m
    fromMaybeAccessMask Nothing = 0

    ownerTypeFromFuncMetaMask :: Int -> OwnerType
    ownerTypeFromFuncMetaMask mask =
        let wrappedNib = (mask `shiftR` 12) .&. 0xF
        in if wrappedNib == 0 then OwnerClass else OwnerTopLevel

    accessMaskFromMetaMask :: Int -> Int
    accessMaskFromMetaMask mask =
        let visNib = (mask `shiftR` 8) .&. 0xF
            staticNib = (mask `shiftR` 4) .&. 0xF
            finalNib = mask .&. 0xF
            visBits = case visNib of
                1 -> bitMask accessPublicBit
                2 -> bitMask accessPrivateBit
                3 -> bitMask accessProtectedBit
                _ -> 0
            stBits = if staticNib /= 0 then bitMask accessStaticBit else 0
            finBits = if finalNib /= 0 then bitMask accessFinalBit else 0
        in visBits .|. stBits .|. finBits

    isMetaSuffixLabel :: String -> Bool
    isMetaSuffixLabel s =
        any (`isSuffixOf` s) ["_info", "_info_len", "_modifiers", "_parrents", "_parrentsLen", "_parrentsData"]

    trimMetaSuffixLabel :: String -> String
    trimMetaSuffixLabel s = foldl' (flip dropKnownSuffix) s ["_info_len", "_info", "_modifiers", "_parrentsLen", "_parrents", "_parrentsData"]

    dropKnownSuffix :: String -> String -> String
    dropKnownSuffix suf s
        | suf `isSuffixOf` s = take (length s - length suf) s
        | otherwise = s

    isInternalFieldName :: String -> Bool
    isInternalFieldName name =
        name == "@isInit" || name == "@clinit"

    isLikelyDataBlock :: Maybe [String] -> Bool
    isLikelyDataBlock Nothing = False
    isLikelyDataBlock (Just body) = isDataBlock body

    isDataBlock :: [String] -> Bool
    isDataBlock body = case dropWhile null (map trimLine body) of
        [] -> False
        (x:_) ->
            let l = map toLower x
                firstTok = case words l of
                    [] -> ""
                    (t:_) -> t
            in firstTok `elem` ["db", "dw", "dd", "dq", ".byte", ".value", ".long", ".quad", ".zero", ".asciz", "resb", "resw", "resd", "resq"]

    extractMask :: [String] -> Maybe Int
    extractMask body = listToMaybeInt (mapMaybe parseMovMask body)

    parseMovMask :: String -> Maybe Int
    parseMovMask line =
        let s = map toLower (trimLine line)
            ws = words (map sanitize line)
            isMov = case words s of
                [] -> False
                (op:_) -> op == "mov"
        in if not isMov
            then Nothing
            else listToMaybeInt (mapMaybe parseIntToken (reverse ws))

    parseIntToken :: String -> Maybe Int
    parseIntToken rawTok =
        let tok = stripPunc rawTok
            lowerTok = map toLower tok
        in case parseSignedHex lowerTok of
            Just n -> Just n
            Nothing -> case reads tok of
                [(n, "")] -> Just n
                _ -> Nothing

    parseSignedHex :: String -> Maybe Int
    parseSignedHex tok0 = do
        let (sign, tok) = case tok0 of
                ('+':xs) -> (1, xs)
                ('-':xs) -> (-1, xs)
                _ -> (1, tok0)
        hexPart <- strip0x tok
        case readHex hexPart of
            [(n, "")] -> Just (sign * n)
            _ -> Nothing

    strip0x :: String -> Maybe String
    strip0x ('0':'x':xs) = Just xs
    strip0x _ = Nothing

    sanitize :: Char -> Char
    sanitize c
        | isDigit c = c
        | c `elem` ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_+-xX" :: String) = c
        | otherwise = ' '

    stripPunc :: String -> String
    stripPunc = dropWhileEnd (`elem` [',', ';', ':']) . dropWhile (`elem` ['[', ']', '(', ')'])

    trimLine :: String -> String
    trimLine = trim . stripComment

    stripComment :: String -> String
    stripComment = takeWhile (/= ';')

    listToMaybeInt :: [Int] -> Maybe Int
    listToMaybeInt [] = Nothing
    listToMaybeInt (x:_) = Just x


collectLabelBlocks :: String -> [(String, [String])]
collectLabelBlocks txt = reverse (flush currentLabel currentBody acc)
  where
    ls = lines txt
    (currentLabel, currentBody, acc) = foldl' step (Nothing, [], []) ls

    step :: (Maybe String, [String], [(String, [String])]) -> String -> (Maybe String, [String], [(String, [String])])
    step (mLab, body, blocks) rawLine =
        let line = trim (takeWhile (/= ';') rawLine)
        in case parseLabel line of
            Just lab ->
                let blocks' = flush mLab body blocks
                in (Just lab, [], blocks')
            Nothing ->
                case mLab of
                    Nothing -> (Nothing, [], blocks)
                    Just _ -> (mLab, body ++ [rawLine], blocks)

    flush :: Maybe String -> [String] -> [(String, [String])] -> [(String, [String])]
    flush Nothing _ blocks = blocks
    flush (Just lab) body blocks = (lab, body) : blocks

    parseLabel :: String -> Maybe String
    parseLabel s
        | null s = Nothing
        | last s /= ':' = Nothing
        | otherwise =
            let name = init s
            in if null name || "." `isPrefixOf` name || any isSpace name
                then Nothing
                else Just name


data NativeFieldWithQName = NativeFieldWithQName {
    nfqQName :: QName,
    nfqField :: NativeField
}

data NativeMethodWithQName = NativeMethodWithQName {
    nmqQName :: QName,
    nmqMethod :: NativeMethod
}

data SymSuffix
    = SymPlain
    | SymInfo
    | SymInfoLen
    | SymModifiers
    | SymParrents
    | SymParrentsLen
    | SymParrentsData
    deriving (Eq, Show)

data ParsedSym = ParsedSym {
    symRaw :: String,
    symBase :: String,
    symQName :: QName,
    symSig :: [Class],
    symSuffix :: SymSuffix
}

nfQNameWithName :: NativeFieldWithQName -> QName
nfQNameWithName = nfqQName

nmQNameWithName :: NativeMethodWithQName -> QName
nmQNameWithName = nmqQName

dropFieldQName :: NativeFieldWithQName -> NativeField
dropFieldQName = nfqField

dropMethodQName :: NativeMethodWithQName -> NativeMethod
dropMethodQName = nmqMethod


buildEnv :: Path -> NativeEnvelope -> (ImportEnv, TypedImportEnv)
buildEnv path (NativeEnvelope classes) =
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


collectFieldDecls :: NativeClass -> [(QName, QName, Class, [Position])]
collectFieldDecls cls = concatMap one (ncAttrs cls)
  where
    one :: NativeField -> [(QName, QName, Class, [Position])]
    one f =
        let (pkg, clsName) = splitOwnerQName (ncQName cls)
            fullQn = ncQName cls ++ [nfName f]
            aliases = aliasesFor pkg clsName (nfName f) (nfOwnerType f)
            keys = map (applyVisibility (isPublicAccessMask (nfAccessMask f))) aliases
        in [(k, fullQn, nfType f, []) | k <- keys]


collectMethodDecls :: NativeClass -> [(QName, QName, FunSig, [Position])]
collectMethodDecls cls = concatMap one (ncMethods cls)
  where
    one :: NativeMethod -> [(QName, QName, FunSig, [Position])]
    one m =
        let (pkg, clsName) = splitOwnerQName (ncQName cls)
            fullQn = ncQName cls ++ [nmName m]
            aliases = aliasesFor pkg clsName (nmName m) (nmOwnerType m)
            keys = map (applyVisibility (isPublicAccessMask (nmAccessMask m))) aliases
            sig = FunSig {
                funParams = nmParamTypes m,
                funReturn = nmReturnType m
            }
        in [(k, fullQn, sig, []) | k <- keys]


splitOwnerQName :: QName -> (QName, String)
splitOwnerQName qn = case qn of
    [] -> ([], "")
    _ -> (init qn, last qn)


aliasesFor :: QName -> String -> String -> OwnerType -> [QName]
aliasesFor pkg clsName memberName ownerType
    | ownerType == OwnerTopLevel =
        nub [pkg ++ [clsName, memberName], pkg ++ [memberName]]
    | otherwise =
        [pkg ++ [clsName, memberName]]


applyVisibility :: Bool -> QName -> QName
applyVisibility True qn = qn
applyVisibility False qn = toHiddenQName qn


isPublicAccessMask :: Int -> Bool
isPublicAccessMask mask = (mask .&. bitMask accessPublicBit) /= 0


bitMask :: Int -> Int
bitMask b = 1 `shiftL` b


parseQName :: Value -> Parser QName
parseQName v = case (fromJSON v :: Result [String]) of
    Success qn -> pure qn
    Error _ -> case (fromJSON v :: Result String) of
        Success txt -> pure (splitDots txt)
        Error _ -> fail "class qname must be string or array of strings"
  where
    splitDots :: String -> [String]
    splitDots raw =
        let trimmed = trim raw
        in case trimmed of
            "" -> []
            _ -> splitByDot trimmed

    splitByDot :: String -> [String]
    splitByDot s = case break (== '.') s of
        (a, []) -> [a]
        (a, _:rest) -> a : splitByDot rest


parseAccessMask :: Value -> Parser Int
parseAccessMask v = case (fromJSON v :: Result Int) of
    Success i -> pure i
    Error _ -> case (fromJSON v :: Result [String]) of
        Success tags -> pure (foldl' (.|.) 0 (catMaybes (map tagBit tags)))
        Error _ -> fail "access must be int or list of strings"
  where
    tagBit :: String -> Maybe Int
    tagBit s = case map toLower s of
        "public" -> Just (bitMask accessPublicBit)
        "private" -> Just (bitMask accessPrivateBit)
        "protected" -> Just (bitMask accessProtectedBit)
        "static" -> Just (bitMask accessStaticBit)
        "final" -> Just (bitMask accessFinalBit)
        "inline" -> Just (bitMask accessInlineBit)
        _ -> Nothing

parseOwnerType :: Value -> Parser OwnerType
parseOwnerType v = case (fromJSON v :: Result Int) of
    Success code
        | code == ownerTypeTopLevelCode -> pure OwnerTopLevel
        | code == ownerTypeClassCode -> pure OwnerClass
        | otherwise -> fail "owner_type int must be 0(class) or 1(top-level)"
    Error _ -> case (fromJSON v :: Result String) of
        Success raw ->
            let s = map toLower raw
            in case s of
                "xlang-top-level" -> pure OwnerTopLevel
                "xlang-class" -> pure OwnerClass
                _ -> pure OwnerClass
        Error _ -> fail "owner_type must be int or string"


parseTypeValue :: Value -> Parser Class
parseTypeValue v = case (fromJSON v :: Result String) of
    Success txt -> parseTypeToken txt
    Error _ -> case (fromJSON v :: Result [String]) of
        Success parts -> pure (parseLegacyParts parts)
        Error _ -> fail "type value must be string or string-array"


parseParamTypes :: Value -> Parser [Class]
parseParamTypes v = case (fromJSON v :: Result [String]) of
    Success tokens -> mapM parseTypeToken tokens
    Error _ -> case (fromJSON v :: Result [[String]]) of
        Success parts -> pure (map parseLegacyParts parts)
        Error _ -> fail "param_types must be [String] or [[String]]"


parseTypeToken :: String -> Parser Class
parseTypeToken raw =
    let token = trim raw
    in case classDemangleEither token of
        Right cls -> pure (normalizeClass cls)
        Left _ -> pure (parseLegacyParts (splitDots token))
  where
    splitDots :: String -> [String]
    splitDots s = case break (== '.') s of
        (a, []) -> [a]
        (a, _:rest) -> a : splitDots rest


parseLegacyParts :: [String] -> Class
parseLegacyParts parts =
    let normalized = map normalizePrimitive parts
    in normalizeClass (Class normalized [])


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


filterEnvelopeByImports :: [String] -> NativeEnvelope -> NativeEnvelope
filterEnvelopeByImports imports (NativeEnvelope classes)
    | null normalizedImports = NativeEnvelope classes
    | otherwise = NativeEnvelope (filter keep classes)
  where
    normalizedImports = map normalizeImportPattern imports

    keep :: NativeClass -> Bool
    keep cls =
        let fullName = intercalate "." (ncQName cls)
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


trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace


mkSyntax :: Path -> String -> ErrorKind
mkSyntax p msg = UE.Syntax (UE.makeError p [] msg)
