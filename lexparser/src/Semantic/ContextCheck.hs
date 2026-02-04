module Semantic.ContextCheck where

import Semantic.Environment
import Control.Monad.State.Strict (State, get, put, modify, evalState, execState, runState)
import Lex.Token (Token, tokenPos)
import Util.Type (Path, Position)
import Util.Exception (ErrorKind, undefinedVariable, multipleVariableDefMsg)
import Parse.SyntaxTree (Expression)

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
checkExpr :: Path -> [ImportEnv] -> AST.Expression -> CheckM ()
checkExpr p envs expr = case expr of
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
    {-AST.Variable name tok -> do
        c <- get
        let q = [name]
        let curScope = head (scope $ st c)
        if isVarDefine q curScope envs then pure ()
        else addErr $ UE.Syntax $ UE.makeError p [tokenPos tok] (undefinedVariable name)-}


    {-AST.Qualified names tokens -> do
        c <- get
        let q = names
        let curScope = head (scope $ st c)
        if isVarDefine q curScope envs then pure ()
        else addErr $ UE.Syntax $ UE.makeError p (map tokenPos tokens) (undefinedVariable (last names)) -}


    -- English comment: recurse into children.
    AST.Cast _ e _ -> checkExpr p envs e
    AST.Unary _ e _ -> checkExpr p envs e

    AST.Binary AST.Assign e1 e2 _ -> do
        c <- get
        let cState = st c
        let curScope = head $ scope cState
        checkExpr p envs e2

        -- if getStateDepth cState == 0 && isVarDefine var curScope envs
        -- then addErr $ UE.Syntax $ UE.makeError p (map tokenPos tokens) (multipleVariableDefMsg (last var))
        -- else pure() 

    AST.Binary symbol e1 e2 _ -> do
        checkExpr p envs e1
        checkExpr p envs e2

    AST.Call callee _mTypeArgs args -> do
        checkExpr p envs callee
        mapM_ (checkExpr p envs) args
