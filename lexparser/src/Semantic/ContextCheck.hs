module Semantic.ContextCheck where

import Semantic.Environment
import Control.Monad.State.Strict (State, get, put, modify, evalState, execState, runState)
import Lex.Token (Token, tokenPos)
import Util.Type (Path, Position)
import Util.Exception (ErrorKind, undefinedVariable)

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


-- English comment: pick a reasonable anchor position from tokens.
anchorPos :: [Token] -> [Position]
anchorPos []    = []
anchorPos (t:_) = [tokenPos t]


-- English comment: minimal expression traversal; no name/type rules yet.
checkExpr :: Path -> [ImportEnv] -> AST.Expression -> CheckM ()
checkExpr p _envs expr = case expr of
    -- English comment: literals are always ok at this stage.
    AST.IntConst _ _ -> pure ()
    AST.LongConst _ _ -> pure ()
    AST.FloatConst _ _ -> pure ()
    AST.DoubleConst _ _ -> pure ()
    AST.LongDoubleConst _ _ -> pure ()
    AST.CharConst _ _ -> pure ()
    AST.StringConst _ _ -> pure ()
    AST.BoolConst _ _ -> pure ()

    -- English comment: identifiers: we will resolve them later (milestone 3/4).
    AST.Variable name tok -> do
        let q = [name]
        if isVarDefineBT q scopes envs then pure ()
        else addErr $ UE.Syntax $ UE.makeError p [tokenPos tok] (undefinedVariable name)


    AST.Qualified names tokens -> do
        let q = names
        if isVarDefineBT q scopes envs then pure ()
        else addErr $ UE.Syntax $ UE.makeError p (map tokenPos tokens) (undefinedVariable (last names))

    -- English comment: recurse into children.
    AST.Cast _ e _ -> checkExpr p _envs e
    AST.Unary _ e _ -> checkExpr p _envs e

    {-AST.Binary symbol e1 e2 _ -> do
        if symbol == AST.Assign then
        else
            checkExpr p _envs e1
            checkExpr p _envs e2-}

    AST.Call callee _mTypeArgs args -> do
        checkExpr p _envs callee
        mapM_ (checkExpr p _envs) args
