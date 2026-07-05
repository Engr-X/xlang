module CompileX64Test where

import CompileX64 (TargetPlatform(..), X64CompilerChoice(..), compileX64WithAssembler)
import Control.Exception (catch, SomeException)
import Data.List (isInfixOf)
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory, removePathForcibly)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit


tests :: TestTree
tests = testGroup "CompileX64" [
    testCase "merged asm does not extern static field defined in same unit" $ do
        cwd <- getCurrentDirectory
        let root = cwd </> ".tmp-compilex64-static-extern"
            srcDir = root </> "xlang" </> "test"
            srcPath = srcDir </> "Collision.x"
            outObj = root </> "Collision.o"
            asmPath = root </> "Collision.asm"
            typeSym = "_XN5xlang4test8TestCase13TestCase$TYPEEi"

        removePathIfExists root
        createDirectoryIfMissing True srcDir
        writeFile srcPath collisionSource

        res <- compileX64WithAssembler
            False
            1
            ""
            root
            [srcPath]
            []
            (Just [outObj])
            Nothing
            (Just TargetPlatformWindows)
            (Just X64CompilerNasm)
            False
            True

        case res of
            Nothing -> assertFailure "compileX64WithAssembler failed"
            Just _ -> pure ()

        exists <- doesFileExist asmPath
        assertBool ("expected debug asm at " ++ asmPath) exists
        asm <- readFile asmPath
        assertBool "static field should be exported when generated in this asm" $
            ("global " ++ typeSym) `isInfixOf` asm
        assertBool "static field should be defined in this asm" $
            (typeSym ++ ":") `isInfixOf` asm
        assertBool "same-unit static field must not be declared extern" $
            not (("extern " ++ typeSym) `isInfixOf` asm)

        removePathIfExists root
    ,
    testCase "link-only void entry returns zero exit code" $ do
        cwd <- getCurrentDirectory
        let root = cwd </> ".tmp-compilex64-link-entry"
            outExe = root </> "out.exe"
            asmPath = root </> "out.objs" </> "xlang" </> "link" </> "entry.asm"

        removePathIfExists root
        createDirectoryIfMissing True root

        res <- compileX64WithAssembler
            False
            1
            ""
            root
            []
            []
            (Just [outExe])
            Nothing
            (Just TargetPlatformWindows)
            (Just X64CompilerNasm)
            True
            True

        case res of
            Nothing -> assertFailure "compileX64WithAssembler link-only mode failed"
            Just _ -> pure ()

        exists <- doesFileExist asmPath
        assertBool ("expected link-only entry asm at " ++ asmPath) exists
        asm <- readFile asmPath
        assertBool "link-only void entry should clear eax after main call" $
            ("call main\n    xor eax, eax\n" `isInfixOf` normalizeNewlines asm)

        removePathIfExists root
    ]


removePathIfExists :: FilePath -> IO ()
removePathIfExists path =
    removePathForcibly path `catch` ignore
  where
    ignore :: SomeException -> IO ()
    ignore _ = pure ()


normalizeNewlines :: String -> String
normalizeNewlines = filter (/= '\r')


collisionSource :: String
collisionSource = unlines [
    "package xlang.test",
    "",
    "struct TestRecord",
    "{",
    "    var total: int",
    "}",
    "",
    "struct TestCase",
    "{",
    "    static val TYPE: int = 0",
    "",
    "    fun runTest(n: int, record: pointer<TestRecord>)",
    "    {",
    "        record.total++",
    "    }",
    "}",
    "",
    "struct TestUnion",
    "{",
    "    var type: int",
    "    var testCase: TestCase",
    "",
    "    fun runTest(n: int, record: pointer<TestRecord>)",
    "    {",
    "        if this.type == TestCase.TYPE:",
    "            this.testCase.runTest(n, record)",
    "    }",
    "}",
    "",
    "struct Testgroup",
    "{",
    "    static val TYPE: int = 1",
    "}"
    ]
