{-# LANGUAGE OverloadedStrings #-}

module CompileX64 (
    compileX64,
    compileX64WithAssembler,
    X64CompilerChoice(..),
    TargetPlatform(..)
) where

import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (bracket_, evaluate)
import Control.Monad (filterM, unless, when)
import Control.Monad.State.Strict (evalState)
import Data.Char (toLower, isSpace)
import Data.List (dropWhileEnd, foldl', intercalate, isPrefixOf, isSuffixOf, nub, sort, sortOn)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getExecutablePath)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, findExecutable, getCurrentDirectory, getTemporaryDirectory, listDirectory, removeFile, removePathForcibly)
import System.FilePath ((</>), (<.>), takeBaseName, takeDirectory, takeExtension, replaceExtension, takeFileName)
import System.IO (hClose, openTempFile)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Info (os)
import System.Process (callProcess)
import Text.Printf (printf)

import qualified CompileJava as CJ
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified IR.Lowing as IR
import qualified IR.TAC as TAC
import qualified Lex.Tokenizer as Tokenizer
import qualified Parse.ParseProgm as Parse
import qualified Parse.SyntaxTree as AST
import qualified Semantic.CheckProgram as SCP
import qualified Semantic.LibLoader as LibLoader
import qualified Semantic.TypeEnv as TEnv
import qualified Util.Exception as UE
import qualified Util.FileHelper as FH
import qualified X64Lowing.ASM as X64
import qualified X64Lowing.Lowing as X64L


mapConcurrentlyLimit :: Int -> (a -> IO b) -> [a] -> IO [b]
mapConcurrentlyLimit jobs action xs
    | jobs <= 1 = mapM action xs
    | otherwise = do
        sem <- newQSem jobs
        forConcurrently xs $ \x ->
            bracket_ (waitQSem sem) (signalQSem sem) (action x)


timedIO :: IO a -> IO (a, Word64)
timedIO action = do
    begin <- getMonotonicTimeNSec
    out <- action
    end <- getMonotonicTimeNSec
    pure (out, end - begin)


formatDurationMs :: Word64 -> String
formatDurationMs dtNs
    | dtNs == 0 = "<1 tick"
    | ms >= 1.0 = printf "%.2f ms" ms
    | us >= 1.0 = printf "%.2f us" us
    | otherwise = printf "%.2f ns" ns
  where
    ns = fromIntegral dtNs :: Double
    us = ns / 1000.0
    ms = ns / 1000000.0


data StageTiming = StageTiming {
    stagePath :: FilePath,
    stageElapsedNs :: Word64
} deriving (Eq, Show)


data CompileTimingSummary = CompileTimingSummary {
    tokenizeTimings :: [StageTiming],
    parseTimings :: [StageTiming],
    importLoadNs :: Word64,
    semanticNs :: Word64,
    irTimings :: [StageTiming],
    x64LowerTimings :: [StageTiming],
    objectEmitNs :: Word64,
    linkNs :: Word64
} deriving (Eq, Show)


sumStageTimings :: [StageTiming] -> Word64
sumStageTimings = sum . map stageElapsedNs


printStagePerFile :: String -> [StageTiming] -> IO ()
printStagePerFile label xs = do
    putStrLn ("[DEBUG] " ++ label ++ ":")
    mapM_
        (\(StageTiming path elapsed) ->
            putStrLn ("[DEBUG]   " ++ path ++ ": " ++ formatDurationMs elapsed))
        xs


printCompileSummary :: CompileTimingSummary -> IO ()
printCompileSummary summary = do
    printStagePerFile "tokenize per file" (tokenizeTimings summary)
    printStagePerFile "parse per file" (parseTimings summary)
    putStrLn ("[DEBUG] import load: " ++ formatDurationMs (importLoadNs summary))
    putStrLn ("[DEBUG] semantic total: " ++ formatDurationMs (semanticNs summary))
    printStagePerFile "IR per file" (irTimings summary)
    printStagePerFile "x64 lowering per file" (x64LowerTimings summary)
    putStrLn ("[DEBUG] object emit: " ++ formatDurationMs (objectEmitNs summary))
    putStrLn ("[DEBUG] link: " ++ formatDurationMs (linkNs summary))
    let totalNs =
            sumStageTimings (tokenizeTimings summary)
                + sumStageTimings (parseTimings summary)
                + importLoadNs summary
                + semanticNs summary
                + sumStageTimings (irTimings summary)
                + sumStageTimings (x64LowerTimings summary)
                + objectEmitNs summary
                + linkNs summary
    putStrLn ("[DEBUG] total: " ++ formatDurationMs totalNs)


writePerClassIRFiles :: FilePath -> [TAC.IRProgm] -> IO ()
writePerClassIRFiles classesRoot irs = mapM_ writeOne (collectArtifacts irs)
  where
    collectArtifacts :: [TAC.IRProgm] -> [(FilePath, String)]
    collectArtifacts = concatMap artifactsFromProgm

    artifactsFromProgm :: TAC.IRProgm -> [(FilePath, String)]
    artifactsFromProgm (TAC.IRProgm pkgSegs classes) =
        map (artifactFromClass pkgSegs) classes

    artifactFromClass :: [String] -> TAC.IRClass -> (FilePath, String)
    artifactFromClass pkgSegs cls@(TAC.IRClass _ className _ _ _ _ _ _ _ _) =
        let pkgDir = foldl' (</>) classesRoot pkgSegs
            irPath = pkgDir </> className <.> "ir"
            pkgPrefix = case pkgSegs of
                [] -> ""
                _ -> "package " ++ intercalate "." pkgSegs ++ "\n\n"
            irText = pkgPrefix ++ TAC.prettyIRClass 0 cls
        in (irPath, irText)

    writeOne :: (FilePath, String) -> IO ()
    writeOne (path, text) = do
        createDirectoryIfMissing True (takeDirectory path)
        writeFile path text


collectSourceImports :: [(FilePath, String)] -> [String]
collectSourceImports sources =
    let programs = mapMaybeParse sources
        importsMap = AST.collectInputPrograms programs
    in sort . nub $
        "xlang.io.*" : [toPattern pkg cls |
                           (pkg, classSet) <- Map.toList importsMap,
                           cls <- HashSet.toList classSet]
  where
    mapMaybeParse :: [(FilePath, String)] -> [AST.Program]
    mapMaybeParse = foldr step []
      where
        step :: (FilePath, String) -> [AST.Program] -> [AST.Program]
        step (path, code) acc =
            let (lexErrs, tokens) = Tokenizer.tokenizeWithNL path code
            in if null lexErrs
                then
                    let prog = Parse.parseProgm tokens
                    in if null (AST.getErrorProgram prog) then prog : acc else acc
                else acc

    toPattern :: [String] -> String -> String
    toPattern pkg cls = intercalate "." (pkg ++ [cls])


data ClassAsmUnit = ClassAsmUnit {
    unitSourcePath :: FilePath,
    unitOwnerQName :: [String],
    unitAsmText :: String
}


data LinkKind
    = LinkExe
    | LinkDyn
    | LinkStatic


data OutputMode
    = OutputDir FilePath
    | OutputSingleObject FilePath
    | OutputLinked FilePath LinkKind FilePath


data X64CompilerChoice
    = X64CompilerNasm
    | X64CompilerAs
    deriving (Eq, Show)


data TargetPlatform
    = TargetPlatformWindows
    | TargetPlatformLinux
    | TargetPlatformMac
    deriving (Eq, Show)


xlangBuildVersion :: String
xlangBuildVersion = "alpha-1.1.0"


hostTargetPlatform :: TargetPlatform
hostTargetPlatform = case os of
    "mingw32" -> TargetPlatformWindows
    "darwin" -> TargetPlatformMac
    _ -> TargetPlatformLinux


platformOsTag :: TargetPlatform -> String
platformOsTag p = case p of
    TargetPlatformWindows -> "mingw32"
    TargetPlatformMac -> "darwin"
    TargetPlatformLinux -> "linux"


resolveTargetPlatform :: Maybe TargetPlatform -> TargetPlatform
resolveTargetPlatform = maybe hostTargetPlatform id


x64AsmBackendTag :: X64.CallConv64 -> String
x64AsmBackendTag cc = case X64.ccCompiler cc of
    X64.NASM -> "nasm"
    X64.GAS_INTEL -> "as-intel"
    X64.GAS_ATT -> "as-att"


x64IdentPayload :: String -> X64.CallConv64 -> String
x64IdentPayload targetOs cc =
    concat [
        "xlang (",
        intercalate "/" ["x64", x64AsmBackendTag cc, targetOs],
        ") ",
        xlangBuildVersion]


x64IdentDirective :: String -> X64.CallConv64 -> String
x64IdentDirective targetOs cc = case X64.ccCompiler cc of
    X64.NASM -> concat ["; .ident \"", x64IdentPayload targetOs cc, "\"\n"]
    _ -> concat [".ident \"", x64IdentPayload targetOs cc, "\"\n"]


withX64IdentFooter :: String -> X64.CallConv64 -> String -> String
withX64IdentFooter targetOs cc asmText =
    let base =
            if null asmText
                then ""
                else if last asmText == '\n' then asmText else asmText ++ "\n"
    in base ++ "\n" ++ x64IdentDirective targetOs cc

compileX64 ::
    Int ->
    FilePath ->
    FilePath ->
    [FilePath] ->
    [FilePath] ->
    Maybe FilePath ->
    Maybe [String] ->
    Maybe TargetPlatform ->
    Maybe X64CompilerChoice ->
    Bool ->
    Bool ->
    IO (Maybe [TAC.IRProgm])
compileX64 = compileX64WithAssembler True


compileX64WithAssembler ::
    Bool ->
    Int ->
    FilePath ->
    FilePath ->
    [FilePath] ->
    [FilePath] ->
    Maybe FilePath ->
    Maybe [String] ->
    Maybe TargetPlatform ->
    Maybe X64CompilerChoice ->
    Bool ->
    Bool ->
    IO (Maybe [TAC.IRProgm])
compileX64WithAssembler runAssembler jobs toolkitJar rootPath srcPaths libPaths mOutput mMainClassOverride mTargetPlatform mCompilerChoice includeRuntime debugOut = do
    sourceRes <- mapConcurrentlyLimit jobs
        (\path -> do
            fileRes <- FH.readFile path
            pure (path, fileRes))
        srcPaths

    let readErrs = [err | (_, Left err) <- sourceRes]
        sources = [(path, code) | (path, Right code) <- sourceRes]

    if not (null readErrs)
        then do
            mapM_ (putStrLn . UE.errorToString) readErrs
            pure Nothing
        else do
            let sourceImports = collectSourceImports sources
            (libsRes, libsTime) <- timedIO (LibLoader.loadLibEnvsWithJobs jobs toolkitJar libPaths sourceImports)
            case libsRes of
                Left errs -> do
                    mapM_ (putStrLn . UE.errorToString) errs
                    pure Nothing
                Right (depImportEnvs, depTypedEnvs) -> do
                    mIrRes <- IR.codeToIRWithRootAndDepsTimedForTarget SCP.SemanticTargetNative depImportEnvs depTypedEnvs rootPath sources
                    case mIrRes of
                        Left errs -> do
                            mapM_ (putStrLn . UE.errorToString) errs
                            pure Nothing
                        Right (irPairs, warns, pipelineTimingRaw) -> do
                            mapM_ print warns
                            printPerFileProgress irPairs
                            let targetPlatform = resolveTargetPlatform mTargetPlatform
                                targetOs = platformOsTag targetPlatform
                                ccUsed = selectCallConv64 targetPlatform mCompilerChoice
                            lowered <- mapM
                                (\one@(path, _) -> do
                                    (u, t) <- timedIO $ do
                                        let us = lowerOneFile ccUsed one
                                        _ <- evaluate (sum (map (length . unitAsmText) us))
                                        pure us
                                    pure (path, u, t))
                                irPairs
                            let irs = map snd irPairs
                                baseUnits = concat [u | (_, u, _) <- lowered]
                                outMode = resolveOutputMode rootPath mOutput

                            case addExeEntryUnit64 targetOs ccUsed outMode mMainClassOverride irPairs baseUnits of
                                Left errMsg -> do
                                    putStrLn ("[ERROR] " ++ errMsg)
                                    pure Nothing
                                Right units ->
                                    case validateOutputMode outMode units of
                                        Left errMsg -> do
                                            putStrLn ("[ERROR] " ++ errMsg)
                                            pure Nothing
                                        Right () -> do
                                            let emitUnits = coalesceUnitsForOutput outMode units
                                            (objPaths, emitTimeNs) <- timedIO $
                                                mapConcurrentlyLimit jobs (emitOne runAssembler debugOut outMode targetOs ccUsed rootPath) emitUnits
                                            linkTimeNs <- if runAssembler
                                                then snd <$> timedIO (linkIfNeeded debugOut rootPath outMode targetOs objPaths libPaths includeRuntime)
                                                else pure 0
                                            when debugOut $ do
                                                writePerClassIRFiles (debugIrRootFromOutputMode rootPath outMode) irs
                                                let pipelineTiming = CompileTimingSummary {
                                                        tokenizeTimings = [
                                                            StageTiming (IR.fePath t) (IR.feTokenizeNs t) |
                                                            t <- IR.frontendStageTimings pipelineTimingRaw
                                                        ],
                                                        parseTimings = [
                                                            StageTiming (IR.fePath t) (IR.feParseNs t) |
                                                            t <- IR.frontendStageTimings pipelineTimingRaw
                                                        ],
                                                        importLoadNs = libsTime,
                                                        semanticNs = IR.semanticStageNs pipelineTimingRaw,
                                                        irTimings = [
                                                            StageTiming (IR.irPath t) (IR.irLowerNs t) |
                                                            t <- IR.irLowerStageTimings pipelineTimingRaw
                                                        ],
                                                        x64LowerTimings = [
                                                            StageTiming path elapsed |
                                                            (path, _, elapsed) <- lowered
                                                        ],
                                                        objectEmitNs = emitTimeNs,
                                                        linkNs = linkTimeNs
                                                    }
                                                printCompileSummary pipelineTiming
                                            pure (Just irs)


printPerFileProgress :: [(FilePath, TAC.IRProgm)] -> IO ()
printPerFileProgress irPairs = do
    let total = length irPairs
    mapM_
        (\(idx, (path, _)) ->
            putStrLn (concat ["[", show idx, " / ", show total, "]: compile ", path, ","]))
        (zip [1 :: Int ..] irPairs)


lowerOneFile :: X64.CallConv64 -> (FilePath, TAC.IRProgm) -> [ClassAsmUnit]
lowerOneFile ccUsed (srcPath, TAC.IRProgm pkgSegs classes) =
    map (lowerOneClass structQSet structSizeMap srcPath pkgSegs) classes
  where
    structQSet = X64L.collectStructQNameSet64 pkgSegs classes
    structSizeMap = X64L.collectStructSizeMap64 pkgSegs classes

    lowerOneClass :: Set.Set [String] -> Map.Map [String] Int -> FilePath -> [String] -> TAC.IRClass -> ClassAsmUnit
    lowerOneClass qset szMap path pkg cls@(TAC.IRClass _ className _ _ _ _ _ _ _ _) =
        let st0 = X64L.initClassState64 ccUsed qset szMap cls
            (x64Class, refs) = evalState (X64L.x64LowingClass pkg cls) st0
            (staticData, segs) = x64Class
            ownerQName = pkg ++ [className]
            decls = classGlobalDecls64 pkg cls ++ classExternDecls64 ownerQName refs
        in ClassAsmUnit {
            unitSourcePath = path,
            unitOwnerQName = ownerQName,
            unitAsmText = X64.prettyX64ProgmIntel ccUsed (X64.X64Progm decls staticData segs)
        }


classGlobalDecls64 :: [String] -> TAC.IRClass -> [X64.X64Decl]
classGlobalDecls64 pkgSegs irCls@(TAC.IRClass _ className classType _ _ _ funs tFuns cFuns _) =
    let ownerQName = pkgSegs ++ [className]
        clinitGlobal = X64.Global (ownerQName ++ [X64.staticInitName]) [AST.Void]
        classInfoGlobal = X64.GlobalClassInfo ownerQName
        pubFunSymbols = [
            (ownerQName ++ [funName], TEnv.funParams sig ++ [TEnv.funReturn sig])
            | TAC.IRFunction decl funName sig _ _ _ <- funs, shouldExportDecl decl]
        pubCFunSymbols = [
            (X64.mkRawQName64 (takeWhile (/= '(') funName), TEnv.funParams sig ++ [TEnv.funReturn sig])
            | TAC.IRCFunction decl funName sig _ _ _ <- cFuns, shouldExportDecl decl]
        pubTemplateSymbols = [
            (X64.mkRawQName64 symName, [])
            | tf@(TAC.IRTemplateFunction decl _ _ _ _ _ _) <- tFuns
            , shouldExportDecl decl
            , symName <- templateSymbolNames ownerQName tf
            ]
        wrappedMainSymbols = classWrappedMainSymbols64 pkgSegs irCls
        funSymbols = nub (pubFunSymbols ++ pubCFunSymbols ++ pubTemplateSymbols ++ wrappedMainSymbols)
        funGlobals = map (uncurry X64.Global) funSymbols
    in classInfoGlobal : clinitGlobal : funGlobals
  where
    shouldExportDecl :: AST.Decl -> Bool
    shouldExportDecl decl
        | classType == TAC.IRClassTypeStruct = True
        | otherwise = isPublicDecl64 decl

    templateSymbolNames :: [String] -> TAC.IRTemplateFunction -> [String]
    templateSymbolNames clsQName (TAC.IRTemplateFunction _ funName sig _ _ _ _) =
        let fullSig = TEnv.funParams sig ++ [TEnv.funReturn sig]
            base = X64.mangleQNameWithSig True (clsQName ++ [funName]) fullSig ++ "T"
        in [base, base ++ "_len"]


isPublicDecl64 :: AST.Decl -> Bool
isPublicDecl64 (acc, _) = acc == AST.Public


classWrappedMainSymbols64 :: [String] -> TAC.IRClass -> [([String], [AST.Class])]
classWrappedMainSymbols64 pkgSegs (TAC.IRClass _ className _ _ _ _ funs _ _ _) = [
    (pkgSegs ++ [className, "main"], [retT]) |
    TAC.IRFunction _ "main" sig _ _ memberType <- funs,
    memberType == TAC.MemberClassWrapped,
    null (TEnv.funParams sig),
    let retT = TEnv.funReturn sig,
    retT `elem` [AST.Int32T, AST.Void]]


classExternDecls64 :: [String] -> [X64L.ExternRef] -> [X64.X64Decl]
classExternDecls64 ownerQName refs =
    X64L.collectExternDeclsFromRefs64 [ownerQName] refs


addExeEntryUnit64 ::
    String ->
    X64.CallConv64 ->
    OutputMode ->
    Maybe [String] ->
    [(FilePath, TAC.IRProgm)] ->
    [ClassAsmUnit] ->
    Either String [ClassAsmUnit]
addExeEntryUnit64 targetOs cc outMode mMainClassOverride irPairs units = case outMode of
    OutputLinked _ LinkExe _ ->
        case mkExeEntryUnit64 targetOs cc mMainClassOverride irPairs of
            Left errMsg -> Left errMsg
            Right entryUnit -> Right (entryUnit : units)
    _ -> Right units


mkExeEntryUnit64 :: String -> X64.CallConv64 -> Maybe [String] -> [(FilePath, TAC.IRProgm)] -> Either String ClassAsmUnit
mkExeEntryUnit64 targetOs cc mMainClassOverride irPairs = do
    (entryQn, entrySig) <- pickExeEntry64 mMainClassOverride irPairs
    let targetSym = X64.mangleQNameWithSig True entryQn entrySig
        forceZeroExit = case reverse entrySig of
            (retT : _) -> retT == AST.Void
            [] -> False
        asmText = mkExeEntryAsm64 targetOs cc targetSym forceZeroExit
    Right ClassAsmUnit {
        unitSourcePath = "main.x64",
        unitOwnerQName = ["main"],
        unitAsmText = asmText}


pickExeEntry64 :: Maybe [String] -> [(FilePath, TAC.IRProgm)] -> Either String ([String], [AST.Class])
pickExeEntry64 mMainClassOverride irPairs =
    let entries = nub (concatMap gatherOne irPairs)
    in case mMainClassOverride of
        Nothing -> pickDefault entries
        Just mainQnRaw -> pickFromOverride mainQnRaw entries
  where
    pickDefault :: [([String], [AST.Class])] -> Either String ([String], [AST.Class])
    pickDefault entries = case entries of
        [] ->
            Left "x64 executable output needs one wrapped main function, but no wrapped main() was detected"
        _ ->
            pickByPolicy entries "x64 executable output found multiple wrapped main() candidates"

    pickFromOverride :: [String] -> [([String], [AST.Class])] -> Either String ([String], [AST.Class])
    pickFromOverride mainQnRaw entries =
        let mainClsQn = normalizeMainClassQName64 mainQnRaw
            exactMatches = filter (\(mainQn, _) -> normalizeMainClassQName64 mainQn == mainClsQn) entries
            pkgPrefix = case mainClsQn of
                [] -> []
                [_] -> []
                _ -> init mainClsQn
            pkgMatches = filter (\(mainQn, _) -> pkgPrefix `isPrefixOf` normalizeMainClassQName64 mainQn) entries
        in case exactMatches of
            [one] -> Right one
            many@(_:_:_) ->
                Left ("x64 executable --main matched multiple entries: " ++ intercalate ", " (map (intercalate "." . fst) many))
            [] -> case pkgMatches of
                [] ->
                    Left ("x64 executable --main did not match any wrapped main() in package/class: " ++ intercalate "." mainClsQn)
                _ ->
                    pickByPolicy pkgMatches "x64 executable --main package scan found multiple wrapped main() candidates"

    normalizeMainClassQName64 :: [String] -> [String]
    normalizeMainClassQName64 qn = case reverse qn of
        ("main" : revCls) | not (null revCls) -> reverse revCls
        _ -> qn

    pickByPolicy :: [([String], [AST.Class])] -> String -> Either String ([String], [AST.Class])
    pickByPolicy [] errPrefix = Left (errPrefix ++ ": <none>")
    pickByPolicy pool errPrefix =
        let withMeta = map (\cand@(qn, _) -> (cand, pkgLen qn, classRank qn)) pool
            minPkg = minimum (map (\(_, p, _) -> p) withMeta)
            minPkgSet = [cand | (cand, p, _) <- withMeta, p == minPkg]
            preferred = [cand | cand@(qn, _) <- minPkgSet, classRank qn <= 1]
            rankedPreferred = sortOn candKey preferred
            rankedAll = sortOn candKey minPkgSet
        in case rankedPreferred of
            (x:_) -> Right x
            [] -> case rankedAll of
                [x] -> Right x
                many ->
                    Left (errPrefix ++ ": " ++ intercalate ", " (map (intercalate "." . fst) many))

    pkgLen :: [String] -> Int
    pkgLen qn = max 0 (length (normalizeMainClassQName64 qn) - 1)

    classRank :: [String] -> Int
    classRank qn =
        let clsName = case reverse (normalizeMainClassQName64 qn) of
                (x:_) -> x
                [] -> ""
        in if clsName `elem` ["App", "AppX"]
            then 0
            else if clsName `elem` ["Main", "MainX", "main", "mainX"]
                then 1
                else 2

    candKey :: ([String], [AST.Class]) -> (Int, Int, String)
    candKey (qn, _) = (pkgLen qn, classRank qn, intercalate "." qn)

    gatherOne :: (FilePath, TAC.IRProgm) -> [([String], [AST.Class])]
    gatherOne (_, TAC.IRProgm pkgSegs classes) = concatMap (classWrappedMainSymbols64 pkgSegs) classes


mkExeEntryAsm64 :: String -> X64.CallConv64 -> String -> Bool -> String
mkExeEntryAsm64 targetOs cc targetSym forceZeroExit =
    let minitSym = "xlang_minit"
        mexitSym = "xlang_mexit"
        needWinEntryAlign = targetOs == "mingw32"
        needPosixEntryExit = targetOs == "linux" || targetOs == "darwin"
        posixExitNo = if targetOs == "darwin" then "0x2000001" else "60"
        winPrologueNasm =
            [ "    push rbp"
            , "    mov rbp, rsp"
            , "    and rsp, -16"
            , "    sub rsp, 32"
            ]
        winEpilogueNasm =
            [ "    mov rsp, rbp"
            , "    pop rbp"
            ]
        posixPrologueNasm =
            [ "    and rsp, -16"
            ]
        posixExitNasm =
            [ "    mov edi, eax"
            , "    mov eax, " ++ posixExitNo
            , "    syscall"
            ]
        mexitCallNasm
            | needPosixEntryExit =
                [ "    sub rsp, 16"
                , "    mov DWORD [rsp], eax"
                , "    call " ++ mexitSym
                , "    mov eax, DWORD [rsp]"
                , "    add rsp, 16"
                ]
            | otherwise =
                [ "    mov DWORD [rsp], eax"
                , "    call " ++ mexitSym
                , "    mov eax, DWORD [rsp]"
                ]
        winPrologueIntel =
            [ "    push rbp"
            , "    mov rbp, rsp"
            , "    and rsp, -16"
            , "    sub rsp, 32"
            ]
        winEpilogueIntel =
            [ "    mov rsp, rbp"
            , "    pop rbp"
            ]
        posixPrologueIntel =
            [ "    and rsp, -16"
            ]
        posixExitIntel =
            [ "    mov edi, eax"
            , "    mov eax, " ++ posixExitNo
            , "    syscall"
            ]
        mexitCallIntel
            | needPosixEntryExit =
                [ "    sub rsp, 16"
                , "    mov DWORD PTR [rsp], eax"
                , "    call " ++ mexitSym
                , "    mov eax, DWORD PTR [rsp]"
                , "    add rsp, 16"
                ]
            | otherwise =
                [ "    mov DWORD PTR [rsp], eax"
                , "    call " ++ mexitSym
                , "    mov eax, DWORD PTR [rsp]"
                ]
        winPrologueAtt =
            [ "    pushq %rbp"
            , "    movq %rsp, %rbp"
            , "    andq $-16, %rsp"
            , "    subq $32, %rsp"
            ]
        winEpilogueAtt =
            [ "    movq %rbp, %rsp"
            , "    popq %rbp"
            ]
        posixPrologueAtt =
            [ "    andq $-16, %rsp"
            ]
        posixExitAtt =
            [ "    movl %eax, %edi"
            , "    movq $" ++ posixExitNo ++ ", %rax"
            , "    syscall"
            ]
        mexitCallAtt
            | needPosixEntryExit =
                [ "    subq $16, %rsp"
                , "    movl %eax, (%rsp)"
                , "    call " ++ mexitSym
                , "    movl (%rsp), %eax"
                , "    addq $16, %rsp"
                ]
            | otherwise =
                [ "    movl %eax, (%rsp)"
                , "    call " ++ mexitSym
                , "    movl (%rsp), %eax"
                ]
    in case X64.ccCompiler cc of
        X64.NASM -> unlines $
            [ "global main"
            , "extern " ++ targetSym
            , "extern " ++ minitSym
            , "extern " ++ mexitSym
            , "section .text"
            , "main:"
            ]
            ++ [ln | needWinEntryAlign, ln <- winPrologueNasm]
            ++ [ln | needPosixEntryExit, ln <- posixPrologueNasm]
            ++ [ "    call " ++ minitSym ]
            ++ [ "    call " ++ targetSym ]
            ++ mexitCallNasm
            ++ [ "    xor eax, eax" | forceZeroExit ]
            ++ [ln | needWinEntryAlign, ln <- winEpilogueNasm]
            ++ [ln | needPosixEntryExit, ln <- posixExitNasm]
            ++ [ "    ret" | not needPosixEntryExit ]
        X64.GAS_INTEL -> unlines $
            [ ".intel_syntax noprefix"
            , ""
            , ".global main"
            , ".extern " ++ targetSym
            , ".extern " ++ minitSym
            , ".extern " ++ mexitSym
            , ".text"
            , "main:"
            ]
            ++ [ln | needWinEntryAlign, ln <- winPrologueIntel]
            ++ [ln | needPosixEntryExit, ln <- posixPrologueIntel]
            ++ [ "    call " ++ minitSym ]
            ++ [ "    call " ++ targetSym ]
            ++ mexitCallIntel
            ++ [ "    xor eax, eax" | forceZeroExit ]
            ++ [ln | needWinEntryAlign, ln <- winEpilogueIntel]
            ++ [ln | needPosixEntryExit, ln <- posixExitIntel]
            ++ [ "    ret" | not needPosixEntryExit ]
        X64.GAS_ATT -> unlines $
            [ ".att_syntax prefix"
            , ""
            , ".global main"
            , ".extern " ++ targetSym
            , ".extern " ++ minitSym
            , ".extern " ++ mexitSym
            , ".text"
            , "main:"
            ]
            ++ [ln | needWinEntryAlign, ln <- winPrologueAtt]
            ++ [ln | needPosixEntryExit, ln <- posixPrologueAtt]
            ++ [ "    call " ++ minitSym ]
            ++ [ "    call " ++ targetSym ]
            ++ mexitCallAtt
            ++ [ "    xorl %eax, %eax" | forceZeroExit ]
            ++ [ln | needWinEntryAlign, ln <- winEpilogueAtt]
            ++ [ln | needPosixEntryExit, ln <- posixExitAtt]
            ++ [ "    ret" | not needPosixEntryExit ]


selectCallConv64 :: TargetPlatform -> Maybe X64CompilerChoice -> X64.CallConv64
selectCallConv64 targetPlatform mCompilerChoice =
    let base = case targetPlatform of
            TargetPlatformWindows -> X64.winCC64
            TargetPlatformMac -> X64.macCC64
            _ -> X64.linuxCC64
    in case mCompilerChoice of
        Nothing -> base
        Just X64CompilerNasm -> base { X64.ccCompiler = X64.NASM }
        Just X64CompilerAs -> base { X64.ccCompiler = X64.GAS_INTEL }


resolveOutputMode :: FilePath -> Maybe FilePath -> OutputMode
resolveOutputMode rootPath mOutput = case mOutput of
    Nothing -> OutputDir (rootPath </> "out")
    Just outRaw ->
        let outPath = CJ.resolveFromRoot rootPath outRaw
            ext = map toLower (takeExtension outPath)
            objRoot = takeDirectory outPath </> (takeBaseName outPath ++ ".objs")
        in case ext of
            ".o" -> OutputSingleObject outPath
            ".obj" -> OutputSingleObject outPath
            ".exe" -> OutputLinked outPath LinkExe objRoot
            ".out" -> OutputLinked outPath LinkExe objRoot
            ".dll" -> OutputLinked outPath LinkDyn objRoot
            ".so" -> OutputLinked outPath LinkDyn objRoot
            ".dylib" -> OutputLinked outPath LinkDyn objRoot
            ".a" -> OutputLinked outPath LinkStatic objRoot
            ".lib" -> OutputLinked outPath LinkStatic objRoot
            _ -> OutputDir outPath


validateOutputMode :: OutputMode -> [ClassAsmUnit] -> Either String ()
validateOutputMode (OutputSingleObject _) units
    | null units =
        Left "single .o output requires at least one lowered class, but got 0"
validateOutputMode _ _ = Right ()


coalesceUnitsForOutput :: OutputMode -> [ClassAsmUnit] -> [ClassAsmUnit]
coalesceUnitsForOutput outMode units = case outMode of
    OutputSingleObject _ -> case units of
        [] -> []
        [u] -> [u]
        many ->
            let srcHint = intercalate "+" (map (takeBaseName . unitSourcePath) many)
                ownerHint = concatMap unitOwnerQName many
                mergedText = coalesceAsmText many
            in [ClassAsmUnit {
                unitSourcePath = srcHint,
                unitOwnerQName = if null ownerHint then ["merged"] else ownerHint,
                unitAsmText = mergedText
            }]
    _ -> units
  where
    coalesceAsmText :: [ClassAsmUnit] -> String
    coalesceAsmText us =
        let allLines = concatMap (lines . unitAsmText) us
            globals = foldl' collectGlobal HashSet.empty allLines
            kept = filter (not . isShadowedExtern globals) allLines
        in unlines kept

    collectGlobal :: HashSet.HashSet String -> String -> HashSet.HashSet String
    collectGlobal acc ln = case parseDeclSymbol "global" ln of
        Just sym | not (null sym) -> HashSet.insert sym acc
        _ -> case parseDeclSymbol ".global" ln of
            Just sym | not (null sym) -> HashSet.insert sym acc
            _ -> acc

    isShadowedExtern :: HashSet.HashSet String -> String -> Bool
    isShadowedExtern globals ln = case parseDeclSymbol "extern" ln of
        Just sym -> HashSet.member sym globals
        Nothing -> case parseDeclSymbol ".extern" ln of
            Just sym -> HashSet.member sym globals
            Nothing -> False

    parseDeclSymbol :: String -> String -> Maybe String
    parseDeclSymbol kw rawLine =
        let t = trim rawLine
            prefix = kw ++ " "
        in if prefix `isPrefixOf` t
            then
                let rest = drop (length prefix) t
                    sym = takeWhile (not . isSpace) rest
                in if null sym then Nothing else Just sym
            else Nothing

    trim :: String -> String
    trim = dropWhileEnd isSpace . dropWhile isSpace


emitOne :: Bool -> Bool -> OutputMode -> String -> X64.CallConv64 -> FilePath -> ClassAsmUnit -> IO FilePath
emitOne runAssembler debugOut outMode targetOs cc rootPath unit = do
    let asmPath = unitAsmPath outMode unit
        objPath = unitObjPath outMode unit
        asmText = withX64IdentFooter targetOs cc (unitAsmText unit)
    createDirectoryIfMissing True (takeDirectory objPath)
    if debugOut
        then do
            createDirectoryIfMissing True (takeDirectory asmPath)
            writeFile asmPath asmText
            when runAssembler (assembleOne rootPath targetOs cc asmPath objPath)
            pure objPath
        else do
            when runAssembler $ withTempAsmFile asmText (\tmpAsm -> assembleOne rootPath targetOs cc tmpAsm objPath)
            pure objPath


withTempAsmFile :: String -> (FilePath -> IO a) -> IO a
withTempAsmFile asmText action = do
    tmpDir <- getTemporaryDirectory
    (tmpPath, h) <- openTempFile tmpDir "xlang-x64-"
    hClose h
    let asmPath = replaceExtension tmpPath ".asm"
    writeFile asmPath asmText
    out <- action asmPath
    removeFile asmPath `catchIOError` (\_ -> pure ())
    pure out


nasmObjectFormatFor :: String -> String
nasmObjectFormatFor targetOs = case targetOs of
    "mingw32" -> "win64"
    "darwin" -> "macho64"
    _ -> "elf64"


assembleOne :: FilePath -> String -> X64.CallConv64 -> FilePath -> FilePath -> IO ()
assembleOne rootPath targetOs cc asmPath objPath = case X64.ccCompiler cc of
    X64.NASM -> do
        nasmCmd <- resolveToolPath64 rootPath "nasm"
        callProcess nasmCmd ["-f", nasmObjectFormatFor targetOs, asmPath, "-o", objPath]
    _ -> do
        asCmd <- resolveToolPath64 rootPath "as"
        callProcess asCmd [asmPath, "-o", objPath]


linkIfNeeded :: Bool -> FilePath -> OutputMode -> String -> [FilePath] -> [FilePath] -> Bool -> IO ()
linkIfNeeded debugOut rootPath outMode targetOs objPaths linkLibPaths includeRuntime = case outMode of
    OutputLinked outPath linkKind objRoot -> do
        createDirectoryIfMissing True (takeDirectory outPath)
        case linkKind of
            LinkStatic -> do
                arCmd <- resolveToolPath64 rootPath "ar"
                callProcess arCmd (["rcs", outPath] ++ objPaths)
            LinkDyn ->
                linkDynWithLd outPath objPaths linkLibPaths >> cleanupLegacyOutIfNeeded debugOut outPath
            LinkExe ->
                linkExeWithLd outPath objPaths linkLibPaths
        cleanupLinkedObjRootIfNeeded debugOut objRoot
    _ -> pure ()
  where
    linkDynWithLd :: FilePath -> [FilePath] -> [FilePath] -> IO ()
    linkDynWithLd target objs libs = do
        sysLibs <- collectNativeStaticLibs
        mingwTail <- mingwRuntimeTail
        let libsUsedRaw = sanitizeDeprecatedBaseLibRefs $
                if includeRuntime then filterOutDynamicLibRefs libs else libs
            libsUsedPre =
                if targetOs == "mingw32" && not includeRuntime
                    then preferWindowsImportLibs libsUsedRaw
                    else libsUsedRaw
            libsUsed =
                if targetOs == "mingw32"
                    then stripMingwToolchainRuntimeLibRefs libsUsedPre
                    else libsUsedPre
            sysLibsUsed =
                if targetOs == "mingw32"
                    then stripMingwToolchainRuntimeLibRefs sysLibs
                    else sysLibs
            linkLibs = wrapLibGroup (mergeLinkLibs libsUsed sysLibsUsed)
        case targetOs of
            "mingw32" -> do
                let implib = replaceExtension target ".dll.a"
                    ldArgs = ["--dll", "-o", target]
                        ++ objs ++ linkLibs ++ mingwTail ++ ["--out-implib", implib]
                callProcessCandidates rootPath
                    [ ("x86_64-w64-mingw32-ld", ldArgs)
                    , ("ld", ldArgs)
                    ]
            "darwin" ->
                callProcessCandidates rootPath [("ld", ["-dylib", "-o", target] ++ objs ++ linkLibs)]
            _ ->
                callProcessCandidates rootPath [("ld", ["-shared", "-o", target] ++ objs ++ linkLibs)]

    linkExeWithLd :: FilePath -> [FilePath] -> [FilePath] -> IO ()
    linkExeWithLd target objs libs = do
        sysLibs <- if targetOs == "mingw32" && not includeRuntime
            then pure []
            else collectNativeStaticLibs
        mingwTail <- mingwRuntimeTail
        let libsUsedRaw = sanitizeDeprecatedBaseLibRefs $
                if includeRuntime then filterOutDynamicLibRefs libs else libs
            libsUsedPre =
                if targetOs == "mingw32" && not includeRuntime
                    then preferWindowsImportLibs libsUsedRaw
                    else libsUsedRaw
            libsUsed =
                if targetOs == "mingw32"
                    then stripMingwToolchainRuntimeLibRefs libsUsedPre
                    else libsUsedPre
            sysLibsUsed =
                if targetOs == "mingw32"
                    then stripMingwToolchainRuntimeLibRefs sysLibs
                    else sysLibs
            linkLibs = wrapLibGroup (mergeLinkLibs libsUsed sysLibsUsed)
        case targetOs of
            "mingw32" -> do
                let ldArgs = ["-e", "main", "-o", target]
                        ++ objs ++ linkLibs ++ mingwTail
                callProcessCandidates rootPath
                    [ ("x86_64-w64-mingw32-ld", ldArgs)
                    , ("ld", ldArgs)
                    ]
            _ ->
                callProcessCandidates rootPath [("ld", ["-e", "main", "-o", target] ++ objs ++ linkLibs)]

    collectNativeStaticLibs :: IO [FilePath]
    collectNativeStaticLibs = do
        cwd <- getCurrentDirectory
        exeDir <- takeDirectory <$> getExecutablePath
        let syslibDirs = nub [
                exeDir </> "native",
                rootPath </> "native",
                cwd </> "native",
                exeDir </> "native-libs",
                rootPath </> "native-libs",
                cwd </> "native-libs"
                ]
            stdNativeDirs = nub [
                exeDir </> "libs" </> "std" </> "native",
                rootPath </> "libs" </> "std" </> "native",
                cwd </> "libs" </> "std" </> "native",
                exeDir </> "libs" </> "native",
                rootPath </> "libs" </> "native",
                cwd </> "libs" </> "native",
                exeDir </> "std" </> "native",
                rootPath </> "std" </> "native",
                cwd </> "std" </> "native"
                ]
        grouped <- mapM collectOne syslibDirs
        stdLibs <- collectStdNativeLibs stdNativeDirs
        pure (sort (nub (concat grouped ++ stdLibs)))
      where
        collectOne :: FilePath -> IO [FilePath]
        collectOne dir = do
            exists <- doesDirectoryExist dir
            if not exists
                then pure []
                else do
                    names <- listDirectory dir
                    let libs = [dir </> n | n <- sort names, map toLower (takeExtension n) == ".a"]
                    pure (if targetOs == "mingw32" then stripMingwToolchainRuntimeLibRefs libs else libs)

        collectStdNativeLibs :: [FilePath] -> IO [FilePath]
        collectStdNativeLibs dirs = do
            let preferredGroups
                    | targetOs == "mingw32" && includeRuntime =
                        [ ["libxlang-std.a"]
                        , ["libxlang-core.a"]]
                    | targetOs == "mingw32" =
                        [ ["libxlang-core.dll.a"]
                        , ["libxlang-std.dll.a"]]
                    | otherwise =
                        [ ["libxlang-core.a"]
                        , ["libxlang-std.a"]]
            picked <- mapM (pickFirstExisting dirs) preferredGroups
            pure [p | Just p <- picked]

        pickFirstExisting :: [FilePath] -> [FilePath] -> IO (Maybe FilePath)
        pickFirstExisting dirs names = firstExisting [dir </> name | name <- names, dir <- dirs]

        firstExisting :: [FilePath] -> IO (Maybe FilePath)
        firstExisting [] = pure Nothing
        firstExisting (p : rest) = do
            exists <- doesFileExist p
            if exists then pure (Just p) else firstExisting rest

    mingwRuntimeTail :: IO [FilePath]
    mingwRuntimeTail
        | targetOs /= "mingw32" = pure []
        | otherwise = do
            resolved <- mapM (findMingwImportLib . fst) requiredLibs
            let pairs = zip resolved (map snd requiredLibs)
                foundPaths = [p | (Just p, _) <- pairs]
                fallbackFlags = [flag | (Nothing, flag) <- pairs]
            pure (["--start-group"] ++ foundPaths ++ fallbackFlags ++ ["--end-group"])
      where
        requiredLibs =
            [ ("libmingw32.a", "-lmingw32")
            , ("libmingwex.a", "-lmingwex")
            , ("libwinpthread.a", "-lwinpthread")
            , ("libmsvcrt.a", "-lmsvcrt")
            , ("libkernel32.a", "-lkernel32")
            , ("libgcc.a", "-lgcc")
            , ("libgcc_eh.a", "-lgcc_eh")
            ]

    findMingwImportLib :: FilePath -> IO (Maybe FilePath)
    findMingwImportLib libName = do
        ldCmd <- resolveToolPath64 rootPath "ld"
        crossLdCmd <- resolveToolPath64 rootPath "x86_64-w64-mingw32-ld"
        let binDirs = nub [takeDirectory ldCmd, takeDirectory crossLdCmd]
        libDirs <- nub . concat <$> mapM mingwLibDirsFromBin binDirs
        let candidates = [dir </> libName | dir <- libDirs]
        firstExistingPath candidates
      where
        mingwLibDirsFromBin :: FilePath -> IO [FilePath]
        mingwLibDirsFromBin binDir = do
            let baseDirs = [
                    binDir </> ".." </> "x86_64-w64-mingw32" </> "lib",
                    binDir </> ".." </> ".." </> "x86_64-w64-mingw32" </> "lib"
                    ]
                gccRoots = [
                    binDir </> ".." </> "lib" </> "gcc" </> "x86_64-w64-mingw32",
                    binDir </> ".." </> ".." </> "lib" </> "gcc" </> "x86_64-w64-mingw32"
                    ]
            gccVersionDirs <- concat <$> mapM listChildDirsIfExists gccRoots
            pure (baseDirs ++ gccVersionDirs)

        listChildDirsIfExists :: FilePath -> IO [FilePath]
        listChildDirsIfExists dir = do
            exists <- doesDirectoryExist dir
            if not exists
                then pure []
                else do
                    names <- listDirectory dir
                    let children = [dir </> n | n <- names]
                    filterM doesDirectoryExist children

        firstExistingPath :: [FilePath] -> IO (Maybe FilePath)
        firstExistingPath [] = pure Nothing
        firstExistingPath (p : rest) = do
            exists <- doesFileExist p
            if exists then pure (Just p) else firstExistingPath rest

    stripMingwToolchainRuntimeLibRefs :: [FilePath] -> [FilePath]
    stripMingwToolchainRuntimeLibRefs =
        filter (not . isMingwToolchainRuntimeLib . takeFileName)
      where
        isMingwToolchainRuntimeLib :: String -> Bool
        isMingwToolchainRuntimeLib name =
            map toLower name `elem`
                [ "libmingw32.a"
                , "libmingwex.a"
                , "libwinpthread.a"
                , "libmsvcrt.a"
                , "libkernel32.a"
                , "libgcc.a"
                , "libgcc_eh.a"
                ]

    filterOutDynamicLibRefs :: [FilePath] -> [FilePath]
    filterOutDynamicLibRefs =
        filter (\p ->
            let pL = map toLower p
                ext = map toLower (takeExtension p)
            in ext /= ".dll"
                && ext /= ".so"
                && ext /= ".dylib"
                && not (".dll.a" `isSuffixOf` pL))

    sanitizeDeprecatedBaseLibRefs :: [FilePath] -> [FilePath]
    sanitizeDeprecatedBaseLibRefs =
        filter (\p ->
            let f = map toLower (takeFileName p)
            in f /= "libxlang-base.a"
                && f /= "libxlang-base.dll.a"
                && f /= "libxlang-base.so"
                && f /= "libxlang-base.dylib"
                && f /= "libxlang-base.dll")

    preferWindowsImportLibs :: [FilePath] -> [FilePath]
    preferWindowsImportLibs libs =
        let lowered = map mapLower libs
            importStems = [
                dropSuffixCI ".dll.a" p |
                p <- lowered,
                ".dll.a" `isSuffixOf` p]
            keep p =
                let pL = mapLower p
                    ext = map toLower (takeExtension pL)
                    staticStem = dropSuffixCI ".a" pL
                in if ".dll.a" `isSuffixOf` pL
                    then True
                    else if ext == ".a"
                        then not (staticStem `elem` importStems)
                    else if ext == ".dll" || ext == ".so" || ext == ".dylib"
                        then False
                    else True
        in dedupPreserve (filter keep libs)
      where
        mapLower :: String -> String
        mapLower = map toLower

        dropSuffixCI :: String -> String -> String
        dropSuffixCI suf s
            | suf `isSuffixOf` s = take (length s - length suf) s
            | otherwise = s

        dedupPreserve :: [FilePath] -> [FilePath]
        dedupPreserve = reverse . foldl' step []
          where
            step :: [FilePath] -> FilePath -> [FilePath]
            step acc p =
                if any (\x -> libKey x == libKey p) acc
                    then acc
                    else p : acc

            libKey :: FilePath -> String
            libKey p =
                let file = mapLower (takeFileName p)
                in if ".dll.a" `isSuffixOf` file
                    then dropSuffixCI ".dll.a" file
                    else if ".a" `isSuffixOf` file
                        then dropSuffixCI ".a" file
                        else file

    mergeLinkLibs :: [FilePath] -> [FilePath] -> [FilePath]
    mergeLinkLibs libs sysLibs
        | targetOs == "mingw32" && includeRuntime = dedupByStem (libs ++ sysLibs)
        | targetOs == "mingw32" && not includeRuntime = preferWindowsImportLibs (libs ++ sysLibs)
        | otherwise = dedupCaseInsensitive (libs ++ sysLibs)
      where
        dedupCaseInsensitive :: [FilePath] -> [FilePath]
        dedupCaseInsensitive = reverse . foldl' step []
          where
            step :: [FilePath] -> FilePath -> [FilePath]
            step acc p =
                if any (\x -> map toLower x == map toLower p) acc
                    then acc
                    else p : acc

        dedupByStem :: [FilePath] -> [FilePath]
        dedupByStem = reverse . foldl' stepStem []
          where
            stepStem :: [FilePath] -> FilePath -> [FilePath]
            stepStem acc p =
                if any (\x -> stemKey x == stemKey p) acc
                    then acc
                    else p : acc

            stemKey :: FilePath -> String
            stemKey p =
                let f = map toLower (takeFileName p)
                in if ".dll.a" `isSuffixOf` f
                    then take (length f - length (".dll.a" :: String)) f
                    else if ".a" `isSuffixOf` f
                        then take (length f - length (".a" :: String)) f
                        else f

    wrapLibGroup :: [FilePath] -> [FilePath]
    wrapLibGroup libs
        | targetOs == "mingw32" = ["--start-group"] ++ libs ++ ["--end-group"]
        | otherwise = libs

    callProcessCandidates :: FilePath -> [(FilePath, [String])] -> IO ()
    callProcessCandidates _ [] = ioError (userError "no usable GNU linker found (tried ld variants)")
    callProcessCandidates root ((cmd, args) : rest) = do
        resolvedCmd <- resolveToolPath64 root cmd
        when debugOut $
            putStrLn ("[DEBUG] link command: " ++ unwords (resolvedCmd : args))
        catchIOError
            (callProcess resolvedCmd args)
            (\e -> if isDoesNotExistError e then callProcessCandidates root rest else ioError e)

    cleanupLegacyOutIfNeeded :: Bool -> FilePath -> IO ()
    cleanupLegacyOutIfNeeded keepDebug linkedOut =
        unless keepDebug $ do
            let legacySameBase = replaceExtension linkedOut ".out"
            removeIfExists "a.out"
            removeIfExists legacySameBase

    cleanupLinkedObjRootIfNeeded :: Bool -> FilePath -> IO ()
    cleanupLinkedObjRootIfNeeded keepDebug objRoot =
        unless keepDebug $ do
            ex <- doesDirectoryExist objRoot
            when ex (removePathForcibly objRoot `catchIOError` (\_ -> pure ()))

    removeIfExists :: FilePath -> IO ()
    removeIfExists p = do
        ex <- doesFileExist p
        when ex (removeFile p)


toolNameCandidates64 :: String -> [String]
toolNameCandidates64 tool
    | os == "mingw32" = [tool ++ ".exe", tool]
    | otherwise = [tool]


resolveToolPath64 :: FilePath -> String -> IO FilePath
resolveToolPath64 rootPath tool = do
    let localDir = rootPath </> "tools"
        localCandidates = [localDir </> name | name <- toolNameCandidates64 tool]
    mLocal <- firstExistingFile localCandidates
    case mLocal of
        Just localPath -> pure localPath
        Nothing -> do
            mGlobal <- firstExecutable (toolNameCandidates64 tool)
            pure (maybe tool id mGlobal)
  where
    firstExistingFile :: [FilePath] -> IO (Maybe FilePath)
    firstExistingFile [] = pure Nothing
    firstExistingFile (p : rest) = do
        exists <- doesFileExist p
        if exists then pure (Just p) else firstExistingFile rest

    firstExecutable :: [String] -> IO (Maybe FilePath)
    firstExecutable [] = pure Nothing
    firstExecutable (name : rest) = do
        mPath <- findExecutable name
        case mPath of
            Just p -> pure (Just p)
            Nothing -> firstExecutable rest


unitAsmPath :: OutputMode -> ClassAsmUnit -> FilePath
unitAsmPath outMode unit = case outMode of
    OutputSingleObject objPath -> replaceExtension objPath ".asm"
    OutputDir outDir ->
        let rel = ownerQNameToRelPath (unitOwnerQName unit) (takeBaseName (unitSourcePath unit))
        in outDir </> rel <.> "asm"
    OutputLinked _ _ objRoot ->
        let rel = ownerQNameToRelPath (unitOwnerQName unit) (takeBaseName (unitSourcePath unit))
        in objRoot </> rel <.> "asm"


unitObjPath :: OutputMode -> ClassAsmUnit -> FilePath
unitObjPath outMode unit = case outMode of
    OutputSingleObject objPath -> objPath
    OutputDir outDir ->
        let rel = ownerQNameToRelPath (unitOwnerQName unit) (takeBaseName (unitSourcePath unit))
        in outDir </> rel <.> "o"
    OutputLinked _ _ objRoot ->
        let rel = ownerQNameToRelPath (unitOwnerQName unit) (takeBaseName (unitSourcePath unit))
        in objRoot </> rel <.> "o"


ownerQNameToRelPath :: [String] -> String -> FilePath
ownerQNameToRelPath qn fallbackBase =
    case dropWhileEnd null qn of
        [] -> fallbackBase
        xs -> foldl' (</>) (head xs) (tail xs)


debugIrRootFromOutputMode :: FilePath -> OutputMode -> FilePath
debugIrRootFromOutputMode rootPath outMode = case outMode of
    OutputSingleObject objPath ->
        let dir = takeDirectory objPath
        in if null dir then rootPath else dir
    OutputDir outDir -> outDir
    OutputLinked _ _ objRoot -> objRoot
