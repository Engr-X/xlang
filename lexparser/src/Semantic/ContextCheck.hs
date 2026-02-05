module Semantic.ContextCheck where

import Semantic.NameEnv
import Data.Maybe (listToMaybe)
import Control.Monad.State.Strict (State, get, put, modify, evalState, execState, runState)
import Lex.Token (tokenPos)
import Util.Type (Path)
import Util.Exception (ErrorKind, undefinedVariable, continueCtrlErrorMsg, breakCtrlErrorMsg, returnCtrlErrorMsg)
import Parse.SyntaxTree (Expression, Statement)

import qualified Data.Map.Strict as Map
import qualified Parse.SyntaxTree as AST
import qualified Util.Exception as UE


data Ctx = Ctx {
    st :: CheckState,
    errs :: [ErrorKind]
}
    deriving (Eq, Show)

type CheckM a = State Ctx a

--contextCheck :: Path -> AST.Program -> Either [ErrorKind] ImportEnv


addErr :: ErrorKind -> CheckM ()
addErr e = modify $ \c -> c { errs = e : errs c }

getState :: CheckM CheckState
getState = st <$> get

putState :: CheckState -> CheckM ()
putState s = modify $ \c -> c { st = s }


-- English comment: minimal expression traversal; no name/type rules yet.
checkExpr :: Path -> QName -> [ImportEnv] -> Expression -> CheckM ()
checkExpr p packages envs expr = case expr of
    AST.Error _ _ -> error "this error should really filter or catched after parser"
    AST.IntConst _ _ -> pure ()
    AST.LongConst _ _ -> pure ()
    AST.FloatConst _ _ -> pure ()
    AST.DoubleConst _ _ -> pure ()
    AST.LongDoubleConst _ _ -> pure ()
    AST.CharConst _ _ -> pure ()
    AST.StringConst _ _ -> pure ()
    AST.BoolConst _ _ -> pure ()

    AST.Variable name tok -> do
        c <- get
        let cState = st c
        if isVarDefine name cState || isVarImport (packages ++ [name]) envs
            then pure ()
            else addErr $ UE.Syntax $ UE.makeError p [tokenPos tok] (undefinedVariable name)

    AST.Qualified names tokens -> do
        c <- get
        let cState = st c

        case names of
            -- this.a / this.a.b ... => check 'a' exists in nearest class scope only.
            ("this":field:_) -> case classScope cState of
                [] -> addErr $ UE.Syntax $ UE.makeError p (map tokenPos tokens) (undefinedVariable field)

                (clsTop:_) ->
                    if Map.member field (sVars clsTop) then pure ()
                    else addErr $ UE.Syntax $ UE.makeError p (map tokenPos tokens) (undefinedVariable field)

            -- non-this qualified name is resolved only through imports.
            _ ->
                if isVarImport names envs
                    then pure ()
                    else addErr $ UE.Syntax $ UE.makeError p (map tokenPos tokens)
                            (undefinedVariable (last names))

    -- recurse into children.
    AST.Cast _ e _ -> checkExpr p packages envs e
    AST.Unary _ e _ -> checkExpr p packages envs e

    AST.Binary AST.Assign e1 e2 tok -> do
        checkExpr p packages envs e2

        c <- get
        let cState = st c

        case defineLocalVar p (AST.Binary AST.Assign e1 e2 tok) cState of
            Left err -> addErr err
            Right cState' -> put $ c { st = cState' }

    AST.Binary _ e1 e2 _ -> do
        checkExpr p packages envs e1
        checkExpr p packages envs e2

    AST.Call callee _mTypeArgs args -> do
        checkExpr p packages envs callee
        mapM_ (checkExpr p packages envs) args



isContinueValid :: [CtrlState] -> Bool
isContinueValid = elem InLoop


isBreakValid :: [CtrlState] -> Bool
isBreakValid ctrls = elem InLoop ctrls || elem InCase ctrls


isReturnValid :: [CtrlState] -> Bool
isReturnValid = elem InFunction


checkStmt :: Path -> QName -> [ImportEnv] -> Statement -> CheckM ()
checkStmt p package envs (AST.Command cmd token) = do
    c <- get
    let cState = st c
    let ctrls = ctrlStack cState

    case cmd of
        AST.Continue ->
            if isContinueValid ctrls then pure ()
            else addErr $ UE.Syntax $ UE.makeError p [tokenPos token] continueCtrlErrorMsg

        AST.Break ->
            if isBreakValid ctrls then pure ()
            else addErr $ UE.Syntax $ UE.makeError p [tokenPos token] breakCtrlErrorMsg

        AST.Return mExpr ->
            if isReturnValid ctrls then let checkReturnExpr = maybe (pure ()) (checkExpr p package envs) in checkReturnExpr mExpr
            else addErr $ UE.Syntax $ UE.makeError p [tokenPos token] returnCtrlErrorMsg
