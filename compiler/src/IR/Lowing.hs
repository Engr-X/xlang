module IR.Lowing (
    codeToIRWithRootAndDeps,
    codeToIRWithRootAndDepsTimed,
    FrontendStageTiming(..),
    IrLoweringStageTiming(..),
    PipelineStageTiming(..),
    codeToIRWithRoot,
    codeToIR,
    codeToIRSingleWithRoot,
    codeToIRSingle,
    lowerWithUses
) where

import Control.Exception (evaluate)
import Control.Monad.State.Strict (runState)
import Data.Char (toUpper)
import Data.Either (lefts, rights)
import Data.List (partition)
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Lex.Tokenizer (tokenizeWithNL)
import Parse.ParseProgm (parseProgm)
import Parse.ParserBasic (toException)
import Parse.SyntaxTree (Program, getErrorProgram)
import Semantic.CheckProgram (checkProgmWithDeps)
import Semantic.NameEnv (ImportEnv)
import Semantic.TypeCheck (tcFullFunUses, tcFullVarUses, tcWarnings)
import Semantic.TypeEnv (FullFunctionTable, FullVarTable, TypedImportEnv)
import Util.Exception (ErrorKind, Warning)
import Util.Type (Path, Position)

import qualified IR.TAC as TAC
import qualified IR.Optimize as Opt
import qualified IR.TACLowing as Low
import qualified Data.Map.Strict as Map
import qualified Parse.SyntaxTree as AST
import qualified Semantic.TypeCheck as TC
import qualified Util.Exception as UE


data FrontendStageTiming = FrontendStageTiming {
    fePath :: Path,
    feTokenizeNs :: Word64,
    feParseNs :: Word64
} deriving (Eq, Show)

data IrLoweringStageTiming = IrLoweringStageTiming {
    irPath :: Path,
    irLowerNs :: Word64
} deriving (Eq, Show)

data PipelineStageTiming = PipelineStageTiming {
    frontendStageTimings :: [FrontendStageTiming],
    semanticStageNs :: Word64,
    irLowerStageTimings :: [IrLoweringStageTiming]
} deriving (Eq, Show)


timedIO :: IO a -> IO (a, Word64)
timedIO action = do
    begin <- getMonotonicTimeNSec
    out <- action
    end <- getMonotonicTimeNSec
    pure (out, end - begin)


-- | Parse, semantically check, and lower source files under an explicit source root.
codeToIRWithRoot :: Path -> [(Path, String)] -> Either [ErrorKind] ([(Path, TAC.IRProgm)], [Warning])
codeToIRWithRoot = codeToIRWithRootAndDeps [] []


-- | Parse, semantically check, and lower source files with extra imported envs.
codeToIRWithRootAndDeps ::
    [ImportEnv] ->
    [TypedImportEnv] ->
    Path ->
    [(Path, String)] ->
    Either [ErrorKind] ([(Path, TAC.IRProgm)], [Warning])
codeToIRWithRootAndDeps depImportEnvs depTypedEnvs root files =
    let parsed = map parseOne files
        parseErrs = concat (lefts parsed)
        progms = rights parsed
    in if not (null parseErrs)
        then Left parseErrs
        else case checkProgmWithDeps root depImportEnvs depTypedEnvs progms of
            Left errs -> Left errs
            Right ctxs ->
                let ctxMap = Map.fromList ctxs
                    lowered = map (lowerOne ctxMap) progms
                    lowerErrs = concat (lefts lowered)
                    loweredOk = rights lowered
                in if not (null lowerErrs)
                    then Left lowerErrs
                    else
                        let (irs, warnsList) = unzip loweredOk
                        in Right (irs, concat warnsList)
    where
        parseOne :: (Path, String) -> Either [ErrorKind] (Path, Program)
        -- | Parse one source file and map lexer/parser failures to semantic-style errors.
        parseOne (path, code) =
            let (lexErrs, tokens) = tokenizeWithNL path code
            in if not (null lexErrs)
                then Left lexErrs
                else
                    let prog = parseProgm tokens
                        parseErrs = getErrorProgram prog
                    in if null parseErrs
                        then Right (path, prog)
                        else Left (map (toException path) parseErrs)

        lowerOne ::
            Map Path TC.TypeCtx ->
            (Path, Program) ->
            Either [ErrorKind] ((Path, TAC.IRProgm), [Warning])
        -- | Lower one already-checked program with its per-file type context.
        lowerOne ctxMap (path, prog) =
            case Map.lookup path ctxMap of
                Nothing ->
                    Left [UE.Syntax (UE.makeError path [] UE.internalErrorMsg)]
                Just ctx ->
                    let (ir, tacWarns) = lowerWithUses path prog (tcFullVarUses ctx) (tcFullFunUses ctx)
                        warns = tcWarnings ctx ++ tacWarns
                    in Right ((path, ir), warns)


codeToIRWithRootAndDepsTimed ::
    [ImportEnv] ->
    [TypedImportEnv] ->
    Path ->
    [(Path, String)] ->
    IO (Either [ErrorKind] ([(Path, TAC.IRProgm)], [Warning], PipelineStageTiming))
codeToIRWithRootAndDepsTimed depImportEnvs depTypedEnvs root files = do
    parsedWithTime <- mapM parseOneTimed files
    let frontendTimings = map fst parsedWithTime
        parsed = map snd parsedWithTime
        parseErrs = concat (lefts parsed)
        progms = rights parsed
    if not (null parseErrs)
        then pure (Left parseErrs)
        else do
            (semRes, semanticNs) <- timedIO $ do
                let res = checkProgmWithDeps root depImportEnvs depTypedEnvs progms
                case res of
                    Left errs -> do
                        _ <- evaluate (length errs)
                        pure res
                    Right ctxs -> do
                        _ <- evaluate (length ctxs)
                        pure res
            case semRes of
                Left errs -> pure (Left errs)
                Right ctxs -> do
                    let ctxMap = Map.fromList ctxs
                    loweredWithTime <- mapM (lowerOneTimed ctxMap) progms
                    let irTimings = map fst loweredWithTime
                        lowered = map snd loweredWithTime
                        lowerErrs = concat (lefts lowered)
                        loweredOk = rights lowered
                    if not (null lowerErrs)
                        then pure (Left lowerErrs)
                        else
                            let (irs, warnsList) = unzip loweredOk
                                timing = PipelineStageTiming {
                                    frontendStageTimings = frontendTimings,
                                    semanticStageNs = semanticNs,
                                    irLowerStageTimings = irTimings
                                }
                            in pure (Right (irs, concat warnsList, timing))
  where
    parseOneTimed :: (Path, String) -> IO (FrontendStageTiming, Either [ErrorKind] (Path, Program))
    parseOneTimed (path, code) = do
        ((lexErrs, tokens), tokenizeNs) <- timedIO $ do
            let res@(lexErrs0, tokens0) = tokenizeWithNL path code
            _ <- evaluate (length lexErrs0 + length tokens0)
            pure res
        if not (null lexErrs)
            then pure
                ( FrontendStageTiming {
                    fePath = path,
                    feTokenizeNs = tokenizeNs,
                    feParseNs = 0
                  }
                , Left lexErrs
                )
            else do
                ((prog, parseErrs), parseNs) <- timedIO $ do
                    let prog0 = parseProgm tokens
                        parseErrs0 = getErrorProgram prog0
                    _ <- evaluate (length parseErrs0)
                    pure (prog0, parseErrs0)
                let res =
                        if null parseErrs
                            then Right (path, prog)
                            else Left (map (toException path) parseErrs)
                pure
                    ( FrontendStageTiming {
                        fePath = path,
                        feTokenizeNs = tokenizeNs,
                        feParseNs = parseNs
                      }
                    , res
                    )

    lowerOneTimed ::
        Map Path TC.TypeCtx ->
        (Path, Program) ->
        IO (IrLoweringStageTiming, Either [ErrorKind] ((Path, TAC.IRProgm), [Warning]))
    lowerOneTimed ctxMap (path, prog) =
        case Map.lookup path ctxMap of
            Nothing ->
                pure
                    ( IrLoweringStageTiming {irPath = path, irLowerNs = 0}
                    , Left [UE.Syntax (UE.makeError path [] UE.internalErrorMsg)]
                    )
            Just ctx -> do
                ((ir, warns), irNs) <- timedIO $ do
                    let (ir0, tacWarns) = lowerWithUses path prog (tcFullVarUses ctx) (tcFullFunUses ctx)
                        warns0 = tcWarnings ctx ++ tacWarns
                    _ <- evaluate (length warns0)
                    pure (ir0, warns0)
                pure
                    ( IrLoweringStageTiming {irPath = path, irLowerNs = irNs}
                    , Right ((path, ir), warns)
                    )


-- | Compatibility wrapper using current directory as source root.
codeToIR :: [(Path, String)] -> Either [ErrorKind] ([(Path, TAC.IRProgm)], [Warning])
codeToIR = codeToIRWithRoot "."


-- | Compatibility wrapper for single-file compilation.
codeToIRSingle :: Path -> String -> Either [ErrorKind] (TAC.IRProgm, [Warning])
codeToIRSingle = codeToIRSingleWithRoot "."


-- | Single-file lowering with explicit source root for package validation.
codeToIRSingleWithRoot :: Path -> Path -> String -> Either [ErrorKind] (TAC.IRProgm, [Warning])
codeToIRSingleWithRoot root path code = do
    (irs, warns) <- codeToIRWithRoot root [(path, code)]
    case irs of
        [(_, ir)] -> Right (ir, warns)
        _ -> Left [UE.Syntax (UE.makeError path [] UE.internalErrorMsg)]



-- | Lower a parsed program to IR using resolved var/fun uses and format it.
lowerWithUses ::
    Path ->
    Program ->
    Map [Position] FullVarTable ->
    Map [Position] FullFunctionTable ->
    (TAC.IRProgm, [Warning])
lowerWithUses path (decls, stmts) vUses fUses =
    let (decls', stmts') = AST.inlineProgramFunctions (AST.promoteTopLevelFunctions (decls, stmts))
        (classStmts, otherStmts) = partition AST.isClassDeclar stmts'
        pkgSegs = case filter AST.isPackageDecl decls' of
            (d:_) -> AST.declPath d
            [] -> []
        mainClassName = fromMaybe (toMainClassName path) (AST.getJavaName (decls', stmts'))
        action = do
            classIRs <- mapM Low.classLowing classStmts
            extraIRs <- if null otherStmts
                then pure []
                else (:[]) <$> Low.classStmtsLowing pkgSegs mainClassName otherStmts
            pure $ TAC.IRProgm pkgSegs (classIRs ++ extraIRs)
        st0 = TAC.mkTACState vUses fUses
        (ir0, st1) = runState (TAC.runTACM action) st0
        ir = Opt.formateIR ir0
        tacWarns = reverse (TAC.tacWarnings st1)
    in (ir, tacWarns)
    where
        -- | Build the synthetic main class name from file base name.
        toMainClassName :: Path -> String
        toMainClassName p =
            let fileName = takeFileName p
                base = takeWhile (/= '.') fileName
                cap = case base of
                    [] -> "Main"
                    (c:cs) -> toUpper c : cs
            in cap ++ "X"

        -- | Cross-platform file-name extraction without depending on System.FilePath.
        takeFileName :: FilePath -> FilePath
        takeFileName p =
            let rev = reverse p
                nameRev = takeWhile (\c -> c /= '/' && c /= '\\') rev
            in reverse nameRev
