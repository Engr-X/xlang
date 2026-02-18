module Semantic.ReturnCheck where

import Data.List (intercalate)
import Parse.SyntaxTree (Block(..), Command(..), Expression, Program, Statement(..), SwitchCase(..), exprTokens)
import Semantic.TypeEnv (normalizeClass)
import Util.Exception (ErrorKind)
import Util.Type (Path, Position)

import qualified Lex.Token as Lex
import qualified Parse.SyntaxTree as AST
import qualified Util.Exception as UE


-- | Run return checking for a whole program.
--   Only non-void functions are required to return on all branches.
returnCheckProg :: Path -> Program -> Either [ErrorKind] ()
returnCheckProg path (_, stmts) =
    let errs = concatMap (checkStmt path) stmts
    in if null errs then Right () else Left errs


-- | Collect return-check errors for a single statement (recursing into blocks).
checkStmt :: Path -> Statement -> [ErrorKind]
checkStmt path stmt = case stmt of
    Function (retT, retToks) name params body ->
        checkFunc path retT retToks name params Nothing body ++ checkBlock path body
    FunctionT (retT, retToks) name tparams params body ->
        checkFunc path retT retToks name params (Just tparams) body ++ checkBlock path body
    BlockStmt b -> checkBlock path b
    If _ mb1 mb2 _ ->
        maybe [] (checkBlock path) mb1 ++ maybe [] (checkBlock path) mb2
    For (_, _, _) mb _ -> maybe [] (checkBlock path) mb
    While _ mb1 mb2 _ ->
        maybe [] (checkBlock path) mb1 ++ maybe [] (checkBlock path) mb2
    DoWhile mb1 _ mb2 _ ->
        maybe [] (checkBlock path) mb1 ++ maybe [] (checkBlock path) mb2
    Switch _ cases _ -> concatMap (checkSwitchCase path) cases
    _ -> []


-- | Collect return-check errors for all statements in a block.
checkBlock :: Path -> Block -> [ErrorKind]
checkBlock path (Multiple ss) = concatMap (checkStmt path) ss


-- | Collect return-check errors for a single switch case.
checkSwitchCase :: Path -> SwitchCase -> [ErrorKind]
checkSwitchCase path sc = case sc of
    Case _ mb _ -> maybe [] (checkBlock path) mb
    Default b _ -> checkBlock path b


-- | Check whether a non-void function body returns on all paths.
checkFunc :: Path -> AST.Class -> [Lex.Token] -> Expression -> [(AST.Class, String, [Lex.Token])] -> Maybe [(AST.Class, [Lex.Token])] -> Block -> [ErrorKind]
checkFunc path retT retToks name params mTParams body
    | normalizeClass retT == AST.Void = []
    | blockReturns body = []
    | otherwise =
        let pos = funcPos retToks name
            sig = funcSig retT name params mTParams
        in [UE.Syntax $ UE.makeError path pos (UE.missingReturnMsg sig)]


-- | Pick a source position for a function definition error.
funcPos :: [Lex.Token] -> Expression -> [Position]
funcPos (t:_) _ = [Lex.tokenPos t]
funcPos [] name = case exprTokens name of
    t:_ -> [Lex.tokenPos t]
    [] -> []


-- | Render a function signature for diagnostics.
funcSig :: AST.Class -> Expression -> [(AST.Class, String, [Lex.Token])] -> Maybe [(AST.Class, [Lex.Token])] -> String
funcSig retT name params mTParams =
    let retS = AST.prettyClass retT
        nameS = funcName name
        paramS = intercalate ", " [AST.prettyClass t ++ " " ++ n | (t, n, _) <- params]
        genS = case mTParams of
            Nothing -> ""
            Just ts -> "<" ++ intercalate ", " (map (AST.prettyClass . fst) ts) ++ ">"
    in concat [retS, " ", nameS, genS, "(", paramS, ")"]


funcName :: Expression -> String
funcName expr = case expr of
    AST.Variable s _ -> s
    AST.Qualified ss _ -> intercalate "." ss
    _ -> "<anonymous>"


-- | Whether a block guarantees a return on all paths.
blockReturns :: Block -> Bool
blockReturns (Multiple ss) = stmtsReturn ss


-- | Whether a statement list guarantees a return (stop at first guaranteed return).
stmtsReturn :: [Statement] -> Bool
stmtsReturn [] = False
stmtsReturn (s:ss) = case s of
    Command (Return _) _ -> True
    Command Break _ -> False
    Command Continue _ -> False
    _ -> stmtReturns s || stmtsReturn ss


-- | Whether a single statement guarantees a return.
stmtReturns :: Statement -> Bool
stmtReturns stmt = case stmt of
    BlockStmt b -> blockReturns b
    If _ mb1 mb2 _ ->
        case mb2 of
            Nothing -> False
            Just b2 -> blockReturnsMaybe mb1 && blockReturns b2
    Switch _ cases _ -> switchReturns cases
    _ -> False


-- | Like 'blockReturns', but for optional blocks.
blockReturnsMaybe :: Maybe Block -> Bool
blockReturnsMaybe = maybe False blockReturns


-- | A switch returns only if it has a default and every case returns.
switchReturns :: [SwitchCase] -> Bool
switchReturns cases =
    let hasDefault = any isDefaultCase cases
    in hasDefault && all caseReturns cases


-- | Whether a case/default block guarantees a return.
caseReturns :: SwitchCase -> Bool
caseReturns (Case _ mb _) = maybe False blockReturns mb
caseReturns (Default b _) = blockReturns b


-- | Whether a switch case is a default branch.
isDefaultCase :: SwitchCase -> Bool
isDefaultCase (Default {}) = True
isDefaultCase _ = False
