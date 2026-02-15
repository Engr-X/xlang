module Semantic.ImportLoader where

import Parse.SyntaxTree (Statement, Program, isVariable)
import Semantic.NameEnv
import Lex.Token (tokenPos)
import Util.Exception (ErrorKind, multipleVariableDefMsg, unsupportedErrorMsg, assignErrorMsg, unsupportedErrorMsg)
import Util.Type (Position, Path)
import Data.Map.Strict (Map)

import qualified Util.Exception as UE
import qualified Data.Map.Strict as Map
import qualified Parse.SyntaxTree as AST


-- | Load import environment from a source program.
--   Collects top-level variable assignments and function declarations
--   under the package name, or returns accumulated errors.
loadSrcI :: Path -> Program -> Either [ErrorKind] ImportEnv
loadSrcI p (dcls, stmts) = case getPackageName p dcls of
    Left errs -> Left errs
    Right packageName -> let (vars, funs, errs) = go packageName stmts in if null errs
        then Right IEnv { file = p, iVars = vars, iFuncs = funs }
        else Left $ reverse errs
    where
        go :: QName -> [Statement] -> (Map QName [Position], Map QName [Position], [ErrorKind])
        go _ [] = (Map.empty, Map.empty, [])
        go packageName (stmt : stmts') = let (vars, funs, errs) = go packageName stmts' in case stmt of
            AST.Expr (AST.Binary AST.Assign e _ _) | isVariable e -> case e of
                AST.Variable s token -> let fullName = packageName ++ [s] in if Map.member fullName vars
                    then (vars, funs, UE.Syntax (UE.makeError p [tokenPos token] (multipleVariableDefMsg s)) : errs)
                    else (Map.insert fullName [tokenPos token] vars, funs, errs)
                AST.Qualified _ tokens -> (vars, funs, UE.Syntax (UE.makeError p (map tokenPos tokens) assignErrorMsg) : errs)
                _ -> error "internal error this error should be catched in process of parser"


            AST.Function _ e _ _ -> case e of
                AST.Variable s token -> let fullName = packageName ++ [s] in (vars, Map.insert fullName [tokenPos token] funs, errs)
                AST.Qualified _ tokens -> (vars, funs, UE.Syntax (UE.makeError p (map tokenPos tokens) unsupportedErrorMsg) : errs)
                _ -> (vars, funs, errs)
            AST.FunctionT _ e _ _ _ -> case e of
                AST.Variable s token -> let fullName = packageName ++ [s] in (vars, Map.insert fullName [tokenPos token] funs, errs)
                AST.Qualified _ tokens -> (vars, funs, UE.Syntax (UE.makeError p (map tokenPos tokens) unsupportedErrorMsg) : errs)
                _ -> (vars, funs, errs)
            _ -> (vars, funs, errs)
