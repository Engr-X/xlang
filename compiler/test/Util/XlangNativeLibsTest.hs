module Util.XlangNativeLibsTest where

import Control.Exception (finally)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>), takeDirectory)
import Test.Tasty
import Test.Tasty.HUnit
import Xlang (findDefaultX64NativeLibs)


withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir name action = do
    tmp <- getTemporaryDirectory
    let dir = tmp </> name
    exists <- doesDirectoryExist dir
    if exists then removePathForcibly dir else pure ()
    createDirectoryIfMissing True dir
    action dir `finally` removePathForcibly dir


touch :: FilePath -> IO ()
touch path = do
    createDirectoryIfMissing True (takeDirectory path)
    writeFile path ""


nativeRuntimeSearchTests :: TestTree
nativeRuntimeSearchTests = testGroup "Xlang.defaultX64NativeLibs"
    [ testCase "include-runtime finds static libs in build and runtime build dirs" $
        withTempDir "xlang-native-libs-test" $ \root -> do
            let buildCore = root </> "build" </> "libs" </> "native" </> "libxlang-core.a"
                runtimeCore = root </> "runtime" </> "build" </> "libs" </> "libxlang-core.a"
                importLib = root </> "build" </> "libs" </> "native" </> "libxlang-core.dll.a"
            touch buildCore
            touch runtimeCore
            touch importLib
            libs <- findDefaultX64NativeLibs True [root]
            assertBool "expected build/libs/native static core" (buildCore `elem` libs)
            assertBool "expected runtime/build/libs static core" (runtimeCore `elem` libs)
            assertBool "include-runtime must ignore dll import libs" (importLib `notElem` libs)
    ]


tests :: TestTree
tests = testGroup "Xlang.nativeLibs" [
    nativeRuntimeSearchTests
    ]
