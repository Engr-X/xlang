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
    testCase "chained member call compiles through x64 lowering" $ do
        cwd <- getCurrentDirectory
        let root = cwd </> ".tmp-compilex64-chain-call"
            srcDir = root </> "xlang" </> "test"
            srcPath = srcDir </> "ChainCall.x"
            outObj = root </> "ChainCall.o"
            asmPath = root </> "ChainCall.asm"

        removePathIfExists root
        createDirectoryIfMissing True srcDir
        writeFile srcPath chainCallSource

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
            Nothing -> assertFailure "compileX64WithAssembler failed for chained member call"
            Just _ -> pure ()

        exists <- doesFileExist asmPath
        assertBool ("expected debug asm at " ++ asmPath) exists

        removePathIfExists root
    ,
    testCase "struct value ref compiles as pointer to stack value" $ do
        cwd <- getCurrentDirectory
        let root = cwd </> ".tmp-compilex64-struct-ref"
            srcDir = root </> "xlang" </> "test"
            srcPath = srcDir </> "StructRef.x"
            outObj = root </> "StructRef.o"
            asmPath = root </> "StructRef.asm"

        removePathIfExists root
        createDirectoryIfMissing True srcDir
        writeFile srcPath valueRefSource

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
            Nothing -> assertFailure "compileX64WithAssembler failed for struct value ref"
            Just _ -> pure ()

        exists <- doesFileExist asmPath
        assertBool ("expected debug asm at " ++ asmPath) exists
        asm <- readFile asmPath
        assertBool "value.ref should lower through lea of a stack slot" $
            "lea rcx, QWORD [rbp -" `isInfixOf` asm

        removePathIfExists root
    ,
    testCase "heap struct deref compiles as value copy" $ do
        cwd <- getCurrentDirectory
        let root = cwd </> ".tmp-compilex64-heap-deref"
            sysDir = root </> "xlang"
            srcDir = root </> "xlang" </> "test"
            sysPath = sysDir </> "System.x"
            srcPath = srcDir </> "HeapDeref.x"
            sysObj = root </> "System.o"
            outObj = root </> "HeapDeref.o"
            asmPath = root </> "HeapDeref.asm"

        removePathIfExists root
        createDirectoryIfMissing True sysDir
        createDirectoryIfMissing True srcDir
        writeFile sysPath minimalSystemSource
        writeFile srcPath heapDerefSource

        res <- compileX64WithAssembler
            False
            1
            ""
            root
            [sysPath, srcPath]
            []
            (Just [sysObj, outObj])
            Nothing
            (Just TargetPlatformWindows)
            (Just X64CompilerNasm)
            False
            True

        case res of
            Nothing -> assertFailure "compileX64WithAssembler failed for heap struct deref"
            Just _ -> pure ()

        exists <- doesFileExist asmPath
        assertBool ("expected debug asm at " ++ asmPath) exists
        asm <- readFile asmPath
        assertBool "new Struct should call allocMemory" $
            "call _XN5xlang6System11allocMemoryEiPv" `isInfixOf` asm
        assertBool "heap pointer deref should copy the first eight-byte chunk into a stack value" $
            "mov r11, QWORD [rbp -" `isInfixOf` asm && "mov QWORD [r10], r9" `isInfixOf` asm
        assertBool "heap pointer deref should copy the second eight-byte chunk into a stack value" $
            "mov r9, QWORD [r11 + 8]" `isInfixOf` asm && "mov QWORD [r10 + 8], r9" `isInfixOf` asm

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


chainCallSource :: String
chainCallSource = unlines [
    "package xlang.test",
    "",
    "struct B",
    "{",
    "    fun done() -> int = 7",
    "}",
    "",
    "struct A",
    "{",
    "    fun next() -> pointer<B> = 0 as pointer<B>",
    "}",
    "",
    "fun main() -> int",
    "{",
    "    val a: pointer<A> = 0 as pointer<A>",
    "    return a.next().done()",
    "}"]


valueRefSource :: String
valueRefSource = unlines [
    "package xlang.test",
    "",
    "struct Pair",
    "{",
    "    var x: int",
    "    var y: int",
    "",
    "    fun __init__(x: int, y: int)",
    "    {",
    "        this.x = x",
    "        this.y = y",
    "    }",
    "}",
    "",
    "fun sum(ptr: pointer<Pair>) -> int",
    "{",
    "    val v: Pair = ptr.deref",
    "",
    "    return v.x + v.y",
    "}",
    "",
    "fun main() -> int",
    "{",
    "    val p: Pair = Pair(1, 2)",
    "    val ptr: pointer<Pair> = p.ref",
    "",
    "    return sum(ptr)",
    "}"]


minimalSystemSource :: String
minimalSystemSource = unlines [
    "@file.class(\"System\")",
    "package xlang",
    "",
    "@native(\"xlang_malloc\")",
    "native inline fun allocMemory(size: int) -> pointer<*>"]


heapDerefSource :: String
heapDerefSource = unlines [
    "package xlang.test",
    "",
    "import xlang.System",
    "",
    "struct Pair",
    "{",
    "    var x: int",
    "    var y: int",
    "",
    "    fun __init__(x: int, y: int)",
    "    {",
    "        this.x = x",
    "        this.y = y",
    "    }",
    "}",
    "",
    "fun main() -> int",
    "{",
    "    val p: Pair = (new Pair(3, 4)).deref",
    "",
    "    return p.x + p.y",
    "}"]

