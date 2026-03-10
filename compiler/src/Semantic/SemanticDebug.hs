module Semantic.SemanticDebug where

import Data.Map.Strict (Map)
import Data.List (intercalate, sortOn)
import Data.Either (partitionEithers)
import Parse.ParseProgm (lexparseProgm)
import Parse.SyntaxTree (Program, prettyClass)
import Parse.ParserBasic (AccessModified(..), DeclFlag(..), DeclFlags, Decl)
import Semantic.NameEnv (ImportEnv, QName, getPackageName, defaultImportEnv)
import Semantic.TypeEnv (FullFunctionTable(..), FullVarTable(..), FunSig(..), TypedImportEnv)
import Util.Exception (ErrorKind)
import Util.Type (Path, Position)

import qualified Data.Map.Strict as Map
import qualified Semantic.CheckProgram as CP
import qualified Semantic.ContextCheck as CC
import qualified Semantic.ReturnCheck as RC
import qualified Semantic.TypeCheck as TC
import qualified Util.Exception as UE
import qualified Util.FileHelper as FileHelper


-- | Unified semantic checking pipeline for IR consumers.
--   Runs ReturnCheck, then ContextCheck, then TypeCheck.
checkProgm :: Path -> Program -> [ImportEnv] -> [TypedImportEnv] -> Either [ErrorKind] TC.TypeCtx
checkProgm path prog@(decls, stmts) importEnvs typedEnvs = do
    case RC.returnCheckProg path prog of
        Left errs -> Left errs
        Right () -> do
            packageName <- getPackageName path decls
            let importEnvs0 = defaultImportEnv path : importEnvs
            case CC.checkProgmWithUses path prog importEnvs0 of
                Left errs -> Left errs
                Right (st, uses) ->
                    TC.inferProgmWithCtx path packageName stmts st uses typedEnvs


-- | Read a file, run semantic checks, and dump full use maps.
--   Each map entry is printed on its own line, sorted by Position.
dumpFullUseMapsFromFile :: Path -> IO String
dumpFullUseMapsFromFile path = do
    fileRes <- FileHelper.readFile path
    case fileRes of
        Left err -> pure $ UE.errorToString err
        Right code -> case lexparseProgm path code of
            Left errs -> pure $ unlines (map UE.errorToString errs)
            Right prog -> case checkProgm path prog [] [] of
                Left errs -> pure $ unlines (map UE.errorToString errs)
                Right ctx -> pure $ renderUseMaps ctx


-- | Read multiple files, run cross-file semantic checks (imports/cycles),
--   and return use-map output as text.
dumpFullUseMapsFromFilesText :: [Path] -> IO String
dumpFullUseMapsFromFilesText [] = pure ""
dumpFullUseMapsFromFilesText paths = do
    loaded <- traverse loadProgramFromFile paths
    let (loadErrGroups, progms) = partitionEithers loaded
        loadErrs = concat loadErrGroups

    if not (null loadErrs)
        then pure $ unlines (map UE.errorToString loadErrs)
        else case CP.checkProgm "." progms of
            Left errs -> pure $ unlines (map UE.errorToString errs)
            Right ctxs -> pure $ renderUseMapsByFile ctxs


loadProgramFromFile :: Path -> IO (Either [ErrorKind] (Path, Program))
loadProgramFromFile path = do
    fileRes <- FileHelper.readFile path
    case fileRes of
        Left err -> pure (Left [err])
        Right code -> case lexparseProgm path code of
            Left errs -> pure (Left errs)
            Right prog -> pure (Right (path, prog))


renderUseMapsByFile :: [(Path, TC.TypeCtx)] -> String
renderUseMapsByFile [] = ""
renderUseMapsByFile xs =
    let one (path, ctx) = unlines (("[File] " ++ path) : renderUseMapsLines ctx)
    in intercalate "\n" (map one xs)


renderUseMaps :: TC.TypeCtx -> String
renderUseMaps ctx = unlines (renderUseMapsLines ctx)


renderUseMapsLines :: TC.TypeCtx -> [String]
renderUseMapsLines ctx =
    let vLines = renderVarEntries ctx (TC.tcFullVarUsesList ctx)
        fLines = renderFunEntries (TC.tcFullFunUses ctx)
    in ["[Vars]"] ++ vLines ++ ["[Funs]"] ++ fLines


renderVarEntries :: TC.TypeCtx -> [([Position], FullVarTable)] -> [String]
renderVarEntries ctx entries =
    let sorted = sortOn fst entries
    in map (renderVarEntry ctx) sorted


renderVarEntry :: TC.TypeCtx -> ([Position], FullVarTable) -> String
renderVarEntry ctx (pos, entry) = case entry of
    VarLocal decl name vid ->
        let declS = renderDecl decl
            declPart = if null declS then "" else " " ++ declS
            tyS = case Map.lookup vid (TC.tcVarTypes ctx) of
                Just (cls, _) -> prettyClass cls
                Nothing -> "<unknown>"
        in concat [name, "@", show vid, "(local", declPart, ": ", show pos, "): ", tyS]
    VarImported flags cls usedQname fullQname ->
        let flagS = renderFlags flags
            flagPart = if null flagS then "" else " " ++ flagS
            usedS = prettyQName usedQname
            fullS = prettyQName fullQname
            nameS = if usedQname == fullQname then fullS else usedS ++ " -> " ++ fullS
        in concat [nameS, "(", show pos, "): ", prettyClass cls, flagPart]


renderFunEntries :: Map [Position] FullFunctionTable -> [String]
renderFunEntries mp =
    let entries = sortOn fst (Map.toList mp)
    in map renderFunEntry entries


renderFunEntry :: ([Position], FullFunctionTable) -> String
renderFunEntry (pos, entry) = case entry of
    FunLocal decl qname sig -> formatFunDecl decl qname sig pos
    FunImported flags usedQname fullQname sig ->
        let usedS = prettyQName usedQname
            fullS = prettyQName fullQname
            nameS = if usedQname == fullQname then fullS else usedS ++ " -> " ++ fullS
        in formatFunFlags flags nameS sig pos


formatFunDecl :: Decl -> QName -> FunSig -> [Position] -> String
formatFunDecl decl qname sig pos =
    let retS = prettyClass (funReturn sig)
        paramS = intercalate ", " (map prettyClass (funParams sig))
        declS = renderDecl decl
        prefix = if null declS then "" else declS ++ " "
    in concat [prefix, retS, " ", prettyQName qname, "(", paramS, ") at ", show pos]


formatFunFlags :: DeclFlags -> String -> FunSig -> [Position] -> String
formatFunFlags flags name sig pos =
    let retS = prettyClass (funReturn sig)
        paramS = intercalate ", " (map prettyClass (funParams sig))
        flagS = renderFlags flags
        prefix = if null flagS then "" else flagS ++ " "
    in concat [prefix, retS, " ", name, "(", paramS, ") at ", show pos]


renderFlags :: DeclFlags -> String
renderFlags flags =
    let parts = ["static" | Static `elem` flags] ++ ["final" | Final `elem` flags]
    in unwords parts


renderDecl :: Decl -> String
renderDecl (acc, flags) =
    let accessS = case acc of
            Private -> "private"
            Protected -> "protected"
            Public -> "public"
        flagS = renderFlags flags
        parts = filter (not . null) [accessS, flagS]
    in unwords parts


prettyQName :: [String] -> String
prettyQName = intercalate "."

