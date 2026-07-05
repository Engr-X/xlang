{-# LANGUAGE OverloadedStrings #-}

module Semantic.NativeLibLoaderTest where

import Data.Aeson (Value, encode, object, (.=))
import Data.List (isInfixOf)
import Semantic.LibLoader (isNativeBinLibPath, loadLibEnvsWithJobs)
import Semantic.NameEnv (toHiddenQName)
import Semantic.TypeEnv (FunSig(..), TypedImportEnv(..))
import System.Directory (removeFile)
import System.IO (hClose, openTempFile)
import System.IO.Error (catchIOError)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Parse.SyntaxTree as AST
import qualified Semantic.NameEnv as NE
import qualified Semantic.NativeLibLoader as Native


tests :: TestTree
tests = testGroup "Semantic.NativeLibLoader" [
    nativeCompactJsonDecodeTests,
    nativeStructJsonDecodeTests,
    nativeAsmDecodeTests,
    fibQPowOddBranchRegressionTest,
    nativeBinPathClassificationTests
    ]


nativeBinPathClassificationTests :: TestTree
nativeBinPathClassificationTests = testGroup "native_bin_path_classification"
    [ testCase "x64 object files are treated as native binary libs" $ do
        isNativeBinLibPath "foo.o" @?= True
        isNativeBinLibPath "foo.obj" @?= True
    , testCase "java metadata files are not treated as native binary libs" $ do
        isNativeBinLibPath "foo.json" @?= False
        isNativeBinLibPath "foo.jar" @?= False
    ]


nativeCompactJsonDecodeTests :: TestTree
nativeCompactJsonDecodeTests = testCase "decode compact native json to import/type env" $ do
    let jsonVal = mkNativeClassJson
        jsonText = encode jsonVal
    withTempJson jsonText $ \jsonPath -> do
        loaded <- loadLibEnvsWithJobs 1 "" [jsonPath] []
        case loaded of
            Left errs ->
                assertFailure ("expected successful load, got errors: " ++ show errs)
            Right ([iEnv], [tEnv]) -> do
                assertBool "public top-level method should be imported by short alias"
                    (Map.member ["demo", "add"] (NE.iFuncs iEnv))
                assertBool "public top-level method should be imported by owner alias"
                    (Map.member ["demo", "Math", "add"] (NE.iFuncs iEnv))
                assertBool "private field should be hidden"
                    (Map.member (toHiddenQName ["demo", "Math", "secret"]) (NE.iVars iEnv))

                let funKey = ["demo", "add"]
                case Map.lookup funKey (tFuncs tEnv) of
                    Nothing -> assertFailure "typed env should contain function alias demo.add"
                    Just (sigs, _, _) -> do
                        let expected = FunSig {
                                funParams =
                                    [AST.Bool, AST.Char, AST.Int8T, AST.Int16T, AST.Int32T, AST.Int64T, AST.Float32T, AST.Float64T],
                                funReturn = AST.Float64T
                            }
                        assertBool "signature should contain decoded mangled types" (expected `elem` sigs)
            Right other ->
                assertFailure ("expected one import env + one typed env, got: " ++ show other)


nativeStructJsonDecodeTests :: TestTree
nativeStructJsonDecodeTests = testCase "decode native struct metadata to external struct stub" $ do
    let jsonText = encode mkNativeStructClassJson
    withTempJson jsonText $ \jsonPath -> do
        loaded <- Native.loadNativeJsonOne [] jsonPath
        case loaded of
            Left errs ->
                assertFailure ("expected successful load, got errors: " ++ show errs)
            Right (_, tEnv) -> do
                let key = ["demo", "StringBuilder", "$struct"]
                case Map.lookup key (tStructs tEnv) of
                    Nothing -> assertFailure "typed env should contain imported struct stub"
                    Just (defs, _, _) -> do
                        assertBool "struct stub should include one definition" (not (null defs))
                        assertBool "struct stub should expose append as instance method"
                            (any hasAppendInstance defs)
                        assertBool "struct stub should expose length as instance field"
                            (any hasLengthField defs)
  where
    hasAppendInstance :: AST.Statement -> Bool
    hasAppendInstance (AST.Struct _ _ _ members) =
        any isAppend members
      where
        isAppend (AST.InstanceMethod _ _ _ (AST.Variable "append" _) [(AST.Char, _, _)] _) = True
        isAppend _ = False
    hasAppendInstance _ = False

    hasLengthField :: AST.Statement -> Bool
    hasLengthField (AST.Struct _ _ _ members) =
        any isLength members
      where
        isLength (AST.DefField ["length"] (Just AST.Int32T) _ _) = True
        isLength _ = False
    hasLengthField _ = False


mkNativeStructClassJson :: Value
mkNativeStructClassJson = object [
    "source_lang" .= ("xlang" :: String),
    "target" .= ("x64" :: String),
    "classes" .=
        [object
            [ "class" .= (["demo", "StringBuilder"] :: [String])
            , "access" .= (1 :: Int)
            , "class_type" .= ("struct" :: String)
            , "struct_size" .= (24 :: Int)
            , "attributes" .=
                [ object
                    [ "owner_type" .= (0 :: Int)
                    , "access" .= (1 :: Int)
                    , "attr_name" .= ("length" :: String)
                    , "attr_type" .= ("i" :: String)
                    ]
                ]
            , "methods" .=
                [ object
                    [ "owner_type" .= (1 :: Int)
                    , "access" .= (1 + 8 :: Int)
                    , "name" .= ("StringBuilder$append" :: String)
                    , "param_types" .= (["P13StringBuilder", "c"] :: [String])
                    , "return" .= ("v" :: String)
                    ]
                ]
            ]
        ]
    ]


mkNativeClassJson :: Value
mkNativeClassJson = object [
    "source_lang" .= ("xlang" :: String),
    "target" .= ("x64" :: String),
    "classes" .=
        [object
            [ "class" .= (["demo", "Math"] :: [String])
            , "access" .= (1 :: Int)
            , "attributes" .=
                [ object
                    [ "owner_type" .= (0 :: Int)
                    , "access" .= (2 + 8 :: Int)
                    , "attr_name" .= ("secret" :: String)
                    , "attr_type" .= ("i" :: String)
                    ]
                ]
            , "methods" .=
                [ object
                    [ "owner_type" .= (1 :: Int)
                    , "access" .= (1 + 8 :: Int)
                    , "name" .= ("add" :: String)
                    , "param_types" .= (["z", "c", "b", "s", "i", "j", "f", "d", "d"] :: [String])
                    ]
                ]
            ]
        ]
    ]


withTempJson :: BL.ByteString -> (FilePath -> IO a) -> IO a
withTempJson content action = do
    (tmpPath, h) <- openTempFile "." "native-lib-compact.json"
    hClose h
    BL.writeFile tmpPath content
    out <- action tmpPath
    removeFile tmpPath `catchIOError` (\_ -> pure ())
    pure out


nativeAsmDecodeTests :: TestTree
nativeAsmDecodeTests = testCase "decode native asm metadata to import/type env" $ do
    let metaJson :: String
        metaJson = "{\"class\":[\"demo\",\"Math\"],\"attributes\":[{\"owner_type\":0,\"access\":9,\"attr_name\":\"secret\",\"attr_type\":\"i\"}],\"methods\":[{\"owner_type\":1,\"access\":9,\"name\":\"add\",\"param_types\":[\"i\",\"i\"],\"return\":\"d\"}]}"
        metaLen = length metaJson
        asmText = unlines [
            "global _XN4demo4MathE_info",
            "global _XN4demo4MathE_info_len",
            "",
            "section .data",
            "_XN4demo4MathE_infoData:",
            "    db " ++ show metaJson ++ ", 0",
            "",
            "section .text",
            "_XN4demo4MathE_info:",
            "    lea rax, [rel _XN4demo4MathE_infoData]",
            "    ret",
            "_XN4demo4MathE_info_len:",
            "    mov eax, " ++ show metaLen,
            "    ret"
            ]
    withTempAsm asmText $ \asmPath -> do
        loaded <- Native.loadNativeAsmOne [] asmPath
        case loaded of
            Left errs ->
                assertFailure ("expected successful asm load, got errors: " ++ show errs)
            Right (iEnv, tEnv) -> do
                assertBool "public top-level method should be imported by short alias"
                    (Map.member ["demo", "add"] (NE.iFuncs iEnv))
                assertBool "public top-level method should be imported by owner alias"
                    (Map.member ["demo", "Math", "add"] (NE.iFuncs iEnv))
                assertBool "decoded static field type should exist"
                    (Map.member ["demo", "Math", "secret"] (tVars tEnv))
                case Map.lookup ["demo", "add"] (tFuncs tEnv) of
                    Nothing -> assertFailure "typed env should contain function alias demo.add from asm metadata"
                    Just (sigs, _, _) -> do
                        let expected = FunSig {
                                funParams = [AST.Int32T, AST.Int32T],
                                funReturn = AST.Float64T
                            }
                        assertBool "signature should decode from embedded class-info json" (expected `elem` sigs)


withTempAsm :: String -> (FilePath -> IO a) -> IO a
withTempAsm content action = do
    (tmpPath, h) <- openTempFile "." "native-lib-metadata.asm"
    hClose h
    writeFile tmpPath content
    out <- action tmpPath
    removeFile tmpPath `catchIOError` (\_ -> pure ())
    pure out


fibQPowOddBranchRegressionTest :: TestTree
fibQPowOddBranchRegressionTest =
    testCase "native Math.fibQPow multiplies by Q on odd exponent" $ do
        src <- readFile "../libs/std/native/src/xlang/xlang/Math.x"
        assertBool
            "expected odd-branch multiply in fibQPow (regression guard against reversed parity)"
            ("if isOdd(x): { mulByFibQ(mat); }" `isInfixOf` src)
