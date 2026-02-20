module Semantic.SemanticCheck where

import Data.Map.Strict (Map)
import Data.List (intercalate, sortOn)
import Parse.ParseProgm (lexparseProgm)
import Parse.SyntaxTree (Program, prettyClass)
import Semantic.NameEnv (ImportEnv, getPackageName)
import Semantic.TypeEnv (FullFunctionTable(..), FullVarTable(..), FunSig(..), TypedImportEnv, VarFlag(..), VarFlags)
import Util.Exception (ErrorKind)
import Util.Type (Path, Position)

import qualified Semantic.ContextCheck as CC
import qualified Semantic.ReturnCheck as RC
import qualified Semantic.TypeCheck as TC
import qualified Util.Exception as UE
import qualified Util.FileHelper as FileHelper
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


-- | Unified semantic checking pipeline for IR consumers.
--   Runs ReturnCheck, then ContextCheck, then TypeCheck.
checkProgm :: Path -> Program -> [ImportEnv] -> [TypedImportEnv] -> Either [ErrorKind] TC.TypeCtx
checkProgm path prog@(decls, stmts) importEnvs typedEnvs = do
    case RC.returnCheckProg path prog of
        Left errs -> Left errs
        Right () -> do
            packageName <- getPackageName path decls
            case CC.checkProgmWithUses path prog importEnvs of
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
                Right ctx ->
                    let vLines = renderVarEntries (TC.tcFullVarUsesList ctx)
                        fLines = renderFunEntries (TC.tcFullFunUses ctx)
                    in pure $ unlines (["[Vars]"] ++ vLines ++ ["[Funs]"] ++ fLines)
    where
        renderVarEntries :: [(Position, FullVarTable)] -> [String]
        renderVarEntries entries =
            let sorted = sortOn fst entries
            in map renderVarEntry sorted

        renderVarEntry :: (Position, FullVarTable) -> String
        renderVarEntry (pos, entry) = case entry of
            VarLocal flags name vid ->
                let flagS = renderFlags flags
                    flagPart = if null flagS then "" else " " ++ flagS
                in concat [name, "@", show vid, "(local", flagPart, ": ", show pos, ")"]
            VarImported cls qname ->
                concat [prettyQName qname, "(", show pos, "): ", prettyClass cls]

        renderFunEntries :: Map Position FullFunctionTable -> [String]
        renderFunEntries mp =
            let entries = sortOn fst (Map.toList mp)
            in map renderFunEntry entries

        renderFunEntry :: (Position, FullFunctionTable) -> String
        renderFunEntry (pos, entry) = case entry of
            FunLocal flags name sig -> formatFun flags name sig pos
            FunImported flags qname sig -> formatFun flags (prettyQName qname) sig pos

        formatFun :: VarFlags -> String -> FunSig -> Position -> String
        formatFun flags name sig pos =
            let retS = prettyClass (funReturn sig)
                paramS = intercalate ", " (map prettyClass (funParams sig))
                flagS = renderFlags flags
                prefix = if null flagS then "" else flagS ++ " "
            in concat [prefix, retS, " ", name, "(", paramS, ") at ", show pos]

        renderFlags :: VarFlags -> String
        renderFlags flags =
            let parts = ["static" | Set.member Static flags] ++ ["final" | Set.member Final flags]
            in unwords parts

        prettyQName :: [String] -> String
        prettyQName = intercalate "."
