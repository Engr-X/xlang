module Semantic.SemanticCheck where

import Parse.SyntaxTree (Program)
import Semantic.NameEnv (ImportEnv, getPackageName)
import Semantic.TypeEnv (TypedImportEnv)
import Util.Exception (ErrorKind)
import Util.Type (Path)

import qualified Semantic.ContextCheck as CC
import qualified Semantic.ReturnCheck as RC
import qualified Semantic.TypeCheck as TC


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
