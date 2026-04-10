module Util.Basic where

import Data.Bits (shiftR, (.&.))
import Data.Char (isDigit, toLower, toUpper)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Word (Word32)
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import Numeric (showHex)
import Parse.SyntaxTree (Class(..), classMangle)
import System.Info (arch, os)
import System.IO.Unsafe (unsafePerformIO)
import Text.Regex.TDFA (Regex, makeRegex)
import Text.Regex.Base.RegexLike (matchTest)
import Data.List (isSuffixOf, stripPrefix)

import qualified Data.Map.Strict as Map


intLitPat, longLitPat :: String

-- | Regular expression for a decimal integer literal.
--
-- Supports:
--   * optional leading '+' or '-'
--   * one or more decimal digits
--
-- Examples:
--   123, -42, +0
intLitPat = "[+-]?[0-9]+"

-- | Regular expression for a decimal long integer literal.
--
-- Same as 'intLitPat', but requires a trailing 'l' or 'L'.
--
-- Examples:
--   123L, -42l
longLitPat = intLitPat ++ "(l|L)"


hexIntLitlPat, hexLongLitPat :: String

-- | Regular expression for a hexadecimal integer literal.
--
-- Supports:
--   * optional leading '+' or '-'
--   * '0x' or '0X' prefix
--   * one or more hexadecimal digits
--
-- Examples:
--   0xFF, -0X10
hexIntLitlPat = "[+-]?0(x|X)[0-9a-fA-F]+"

-- | Regular expression for a hexadecimal long integer literal.
--
-- Same as 'hexIntLitlPat', but requires a trailing 'l' or 'L'.
--
-- Examples:
--   0xFFL, -0x10l
hexLongLitPat = hexIntLitlPat ++ "(l|L)"


floatLitPat, doubleLitPat, longDoubleLitPat :: String
-- | Regular expression for a float literal.
--
-- A float literal is a double literal followed by 'f' or 'F'.
--
-- Examples:
--   1.0f, -3.14F, 1e10f
floatLitPat = doubleLitPat ++ "(f|F)"

-- | Regular expression for a double literal.
--
-- Supports:
--   * decimal form with optional fractional part
--   * scientific notation using 'e' or 'E'
--   * optional leading '+' or '-'
--
-- Examples:
--   1.0, .5, 3., 1e10, -2.3E-4
doubleLitPat = "[+-]?((([0-9]+([.][0-9]*)|[.][0-9]+)([eE][+-]?[0-9]+)?)|([0-9]+([eE][+-]?[0-9]+)))"

-- | Regular expression for a long double literal.
--
-- Same as 'doubleLiteralPattern', but requires a trailing 'l' or 'L'.
--
-- Examples:
--   1.0L, 3e5l
longDoubleLitPat = doubleLitPat ++ "(l|L)"


intLitReg, longLitReg, hexIntLitlReg, hexLongLitReg, floatLitReg, doubleLitReg, longDoubleLitReg :: Regex
intLitReg = makeRegex $ full intLitPat
longLitReg = makeRegex $ full longLitPat
hexIntLitlReg = makeRegex $ full hexIntLitlPat
hexLongLitReg = makeRegex $ full hexLongLitPat
floatLitReg = makeRegex $ full floatLitPat
doubleLitReg = makeRegex $ full doubleLitPat
longDoubleLitReg = makeRegex $ full longDoubleLitPat


intReg, longReg :: Regex
intReg = makeRegex $ full $ intLitPat ++ ('|' : hexIntLitlPat)
longReg = makeRegex $ full $ longLitPat ++ ('|' : hexLongLitPat)


-- | Anchor a regular expression to match the entire input.
--
-- Equivalent to wrapping the pattern with '^(' and ')$'.
--
-- Used to ensure a string is fully matched by a pattern.
full :: String -> String
full s = concat ["^(", s, ")$"]


-- | Check whether a regex matches a string (partial match allowed).
--
-- This uses the TDFA '=~' operator directly.
match :: Regex -> String -> Bool
match = matchTest


-- | Check whether a string is any kind of integer literal.
isInt :: String -> Bool
isInt = match intReg
    

-- | Check whether a string is a long double literal.
isLong :: String -> Bool
isLong = match longReg


-- | Check whether a string is a float literal.
isFloat :: String -> Bool
isFloat = match floatLitReg


-- | Check whether a string is a double literal.
isDouble :: String -> Bool
isDouble = match doubleLitReg


-- | Check whether a string is a long double literal.
isLongDouble :: String -> Bool
isLongDouble = match longDoubleLitReg


-- | Generate a string consisting of a given number of spaces.
--
--   This function takes an integer @n@ and returns a string
--   containing exactly @n@ space characters.
--
insertSpace :: Int -> String
insertSpace = (`replicate` ' ')


-- | Generate a string consisting of a given number of tabs (1 tab = 4 space).
--
--   This function takes an integer @n@ and returns a string
--   containing exactly @n@ space characters.
--
insertTab :: Int -> String
insertTab = insertSpace . (4*)


-- | pair 2 D point (x, y) into on D point a it is bijection function
paring :: (Int, Int) -> Int
paring (x, y) = let acc = x + y in (acc * succ acc) `div` 2 + y


-- | Split a non-empty list into its prefix and last element.
--
-- Uses a tail-recursive worker with an accumulator.
-- Throws an error on empty input.
-- Example: @splitLast [1,2,3] == ([1,2],3)@.
splitLast :: [a] -> ([a], a)
splitLast [] = error "splitLast: empty list"
splitLast (x : xs) = go [] x xs
    where
        go acc lastOne [] = (reverse acc, lastOne)
        go acc lastOne (y : ys) = go (lastOne : acc) y ys


-- | IEEE754 float bit pattern in hex.
--
-- Example:
--   toData 10.0 == "0x41200000"
toData :: Float -> String
toData = word32Hex . castFloatToWord32


-- | IEEE754 double bit pattern split into (high32, low32) hex words.
toData64 :: Double -> (String, String)
toData64 d =
    let w64 = castDoubleToWord64 d
        hi = fromIntegral (w64 `shiftR` 32) :: Word32
        lo = fromIntegral (w64 .&. 0xffffffff) :: Word32
    in (word32Hex hi, word32Hex lo)


word32Hex :: Word32 -> String
word32Hex w =
    let hex = map toUpper (showHex w "")
        pad = replicate (max 0 (8 - length hex)) '0'
    in concat ["0x", pad, hex]


data HostArch
    = ArchX86
    | ArchX64
    | ArchArm32
    | ArchArm64
    | ArchRiscV32
    | ArchRiscV64
    | ArchWasm32
    | ArchUnknown String
    deriving (Eq, Ord, Show)


data HostPlatform
    = PlatformWindows
    | PlatformLinux
    | PlatformMac
    | PlatformUnknown String
    deriving (Eq, Ord, Show)


parseHostArch :: String -> HostArch
parseHostArch raw = case map toLower raw of
    "x86" -> ArchX86
    "i386" -> ArchX86
    "i486" -> ArchX86
    "i586" -> ArchX86
    "i686" -> ArchX86
    "x86_64" -> ArchX64
    "amd64" -> ArchX64
    "arm" -> ArchArm32
    "armv7" -> ArchArm32
    "armv7l" -> ArchArm32
    "armhf" -> ArchArm32
    "aarch64" -> ArchArm64
    "arm64" -> ArchArm64
    "riscv32" -> ArchRiscV32
    "riscv64" -> ArchRiscV64
    "riscv64gc" -> ArchRiscV64
    "wasm32" -> ArchWasm32
    other -> ArchUnknown other


parseHostPlatform :: String -> HostPlatform
parseHostPlatform raw = case map toLower raw of
    "windows" -> PlatformWindows
    "win32" -> PlatformWindows
    "mingw32" -> PlatformWindows
    "linux" -> PlatformLinux
    "darwin" -> PlatformMac
    "mac" -> PlatformMac
    "macos" -> PlatformMac
    "osx" -> PlatformMac
    other -> PlatformUnknown other


hostArch :: HostArch
hostArch = parseHostArch arch


hostPlatform :: HostPlatform
hostPlatform = parseHostPlatform os


hostArchTag :: HostArch -> String
hostArchTag ArchX86 = "x86"
hostArchTag ArchX64 = "x64"
hostArchTag ArchArm32 = "arm32"
hostArchTag ArchArm64 = "arm64"
hostArchTag ArchRiscV32 = "riscv32"
hostArchTag ArchRiscV64 = "riscv64"
hostArchTag ArchWasm32 = "wasm32"
hostArchTag (ArchUnknown s) = s


hostPlatformTag :: HostPlatform -> String
hostPlatformTag PlatformWindows = "windows"
hostPlatformTag PlatformLinux = "linux"
hostPlatformTag PlatformMac = "mac"
hostPlatformTag (PlatformUnknown s) = s


hostSpecTag :: String
hostSpecTag = concat [hostPlatformTag hostPlatform, "-", hostArchTag hostArch]


mangleName :: [String] -> [String] -> [Class] -> Bool -> String
mangleName namespaces classes sigClasses isFunction = unsafePerformIO $
    atomicModifyIORef' mangleNameCacheRef $ \cache ->
        case Map.lookup key cache of
            Just value -> (cache, value)
            Nothing ->
                let value = mangleNameRaw namespaces classes sigClasses isFunction
                in (Map.insert key value cache, value)
    where
        key = (namespaces, classes, sigClasses, isFunction)


mangleNameRaw :: [String] -> [String] -> [Class] -> Bool -> String
mangleNameRaw namespaces classes sigClasses isFunction =
    let scopes = namespaces ++ classes
        qname = encodeScopeQName scopes
        sig = encodeSig sigClasses
        useSig = isFunction || not (null sigClasses)
    in "_X" ++ qname ++ (if useSig then sig else "")
    where
        encodeScopeQName :: [String] -> String
        encodeScopeQName [] = error "mangleNameRaw: empty qname"
        encodeScopeQName [x] = encodeIdent x
        encodeScopeQName xs = concat ["N", concatMap encodeIdent xs, "E"]

        encodeSig :: [Class] -> String
        encodeSig [] = "v"
        encodeSig xs = concatMap classMangle xs


type MangleKey = ([String], [String], [Class], Bool)
type MangleCache = Map.Map MangleKey String


{-# NOINLINE mangleNameCacheRef #-}
mangleNameCacheRef :: IORef MangleCache
mangleNameCacheRef = unsafePerformIO (newIORef Map.empty)

encodeIdent :: String -> String
encodeIdent s = show (length s) ++ s


dropSuffix :: String -> String -> String
dropSuffix suf s
    | suf `isSuffixOf` s = take (length s - length suf) s
    | otherwise = s


trimMetaSuffix :: String -> String
trimMetaSuffix raw =
    foldl (flip dropSuffix) raw ["_modifiers", "_parrentsLen", "_parrents", "_parrentsData"]


demangleNameFromMetaSymbol :: String -> Either String ([String], [Class])
demangleNameFromMetaSymbol = demangleName . trimMetaSuffix


demangleName :: String -> Either String ([String], [Class])
demangleName raw = do
    body <- case stripPrefix "_X" raw of
        Just s -> Right s
        Nothing -> Left ("demangleName: missing _X prefix: " ++ raw)
    (qname, rest) <- parseQName body
    sig <- parseSig rest
    Right (qname, sig)
    where
        parseQName :: String -> Either String ([String], String)
        parseQName [] = Left "demangleName: empty body"
        parseQName ('N':xs) = parseNestedScopes [] xs
        parseQName xs = do
            (one, rest) <- parseIdent xs
            Right ([one], rest)

        parseNestedScopes :: [String] -> String -> Either String ([String], String)
        parseNestedScopes acc ('E':rest)
            | null acc = Left "demangleName: empty nested qname"
            | otherwise = Right (reverse acc, rest)
        parseNestedScopes acc s = do
            (one, rest) <- parseIdent s
            parseNestedScopes (one : acc) rest

        parseIdent :: String -> Either String (String, String)
        parseIdent s = case span isDigit s of
            ("", _) -> Left ("demangleName: missing ident length in: " ++ s)
            (nTxt, rest0) ->
                let n = read nTxt :: Int
                in if n <= 0
                    then Left ("demangleName: invalid ident length: " ++ nTxt)
                    else if length rest0 < n
                        then Left ("demangleName: ident shorter than length: " ++ nTxt)
                        else
                            let (ident, rest1) = splitAt n rest0
                            in Right (ident, rest1)

        parseSig :: String -> Either String [Class]
        parseSig "" = Right []
        parseSig s = do
            (one, rest) <- parseClassPrefix s
            more <- parseSig rest
            Right (one : more)

        parseClassPrefix :: String -> Either String (Class, String)
        parseClassPrefix [] = Left "demangleName: unexpected eof in signature"
        parseClassPrefix (c:cs) = case c of
            'b' -> Right (Int8T, cs)
            's' -> Right (Int16T, cs)
            'i' -> Right (Int32T, cs)
            'j' -> Right (Int64T, cs)
            'f' -> Right (Float32T, cs)
            'd' -> Right (Float64T, cs)
            'q' -> Right (Float128T, cs)
            'z' -> Right (Bool, cs)
            'c' -> Right (Char, cs)
            'v' -> Right (Void, cs)
            'N' -> parseNestedClass cs
            _ | isDigit c -> parseSingleClass (c:cs)
              | otherwise -> Left ("demangleName: unexpected type tag: " ++ [c])

        parseNestedClass :: String -> Either String (Class, String)
        parseNestedClass s = do
            (scopes, rest0) <- parseClassScopes [] s
            (args, rest1) <- parseTemplateMaybe rest0
            case rest1 of
                ('E':rest2) -> Right (Class scopes args, rest2)
                _ -> Left "demangleName: missing closing E for nested class"

        parseClassScopes :: [String] -> String -> Either String ([String], String)
        parseClassScopes _ [] = Left "demangleName: unexpected eof in nested class scope"
        parseClassScopes acc s@('I':_) =
            if null acc then Left "demangleName: empty nested class scope" else Right (reverse acc, s)
        parseClassScopes acc s@('E':_) =
            if null acc then Left "demangleName: empty nested class scope" else Right (reverse acc, s)
        parseClassScopes acc s = do
            (one, rest) <- parseIdent s
            parseClassScopes (one : acc) rest

        parseSingleClass :: String -> Either String (Class, String)
        parseSingleClass s = do
            (name, rest0) <- parseIdent s
            (args, rest1) <- parseTemplateMaybe rest0
            Right (Class [name] args, rest1)

        parseTemplateMaybe :: String -> Either String ([Class], String)
        parseTemplateMaybe ('I':rest) = parseTemplateArgs [] rest
        parseTemplateMaybe s = Right ([], s)

        parseTemplateArgs :: [Class] -> String -> Either String ([Class], String)
        parseTemplateArgs _ [] = Left "demangleName: unexpected eof in template args"
        parseTemplateArgs acc ('E':rest) = Right (reverse acc, rest)
        parseTemplateArgs acc s = do
            (one, rest) <- parseClassPrefix s
            parseTemplateArgs (one : acc) rest
