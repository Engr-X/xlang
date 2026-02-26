module Parse.ParserBasic where

import Data.Maybe (listToMaybe, fromMaybe)
import Data.List (find, sort)
import Lex.Token (Token, isLBracketToken, isRBracketToken, tokenPos)
import Parse.SyntaxTree
import Util.Basic (isInt, isLong, isFloat, isDouble, isLongDouble)
import Util.Exception (ErrorKind, expectedExpression)
import Util.Type (Path, makePosition)

import qualified Lex.Token as Lex
import qualified Util.Exception as UE


-- | Access modifiers for declarations.
data AccessModified = Private | Protected | Public
    deriving (Eq, Show)

prettyAccess :: AccessModified -> String
prettyAccess Private = "private"
prettyAccess Protected = "protected"
prettyAccess Public = "public"


-- | Declaration flags (used by variables/functions).
data DeclFlag = Static | Final
    deriving (Eq, Ord, Show)


prettyDeclFlag :: DeclFlag -> String
prettyDeclFlag Static = "static"
prettyDeclFlag Final = "final"


type DeclFlags = [DeclFlag]

-- | Render a list of declaration flags as a single string.
prettyDeclFlags :: DeclFlags -> String
prettyDeclFlags = unwords . map prettyDeclFlag . sort


type Decl = (AccessModified, DeclFlags)

-- | Render a declaration as "access [flags]".
prettyDecl :: Decl -> String
prettyDecl (acc, flags) =
    let accS = prettyAccess acc
        flagsS = prettyDeclFlags flags
        parts = filter (not . null) [accS, flagsS]
    in unwords parts


-- Convert a qualified name to an expression.
-- A single segment is treated as a Variable; otherwise Qualified.
qnameToExpr :: ([String], [Token]) -> Expression
qnameToExpr ([x], [t]) = Variable x t
qnameToExpr (xs, ts) = Qualified (reverse xs) (reverse ts)


--  Choose a token to anchor diagnostics.
nearestTok :: [Token] -> Token
nearestTok = fromMaybe (Lex.EOF (makePosition 0 0 0)) . listToMaybe


-- Canonical fatal parse error expression.
mkHappyErrorExpr :: [Token] -> Expression
mkHappyErrorExpr ts = let t = nearestTok ts in Error [t] ("invalid syntax: " ++ show t)


-- | Check for mismatched or unbalanced brackets in the token stream.
--   Returns the first offending token, or Nothing if brackets are balanced.
checkBracket :: [Token] -> Maybe Token
checkBracket = go []
    where
        go :: [Token] -> [Token] -> Maybe Token
        go [] [] = Nothing
        go (t:_) [] = Just t

        go [] (t:ts)
            | isLBracketToken t = go [t] ts
            | isRBracketToken t = Just t
            | otherwise = go [] ts

        go s@(t:ts) (t':ts')
            | isLBracketToken t' = go (t':s) ts'
            | isRBracketToken t' = if eq (matchBracket t) t' then go ts ts' else Just t'
            | otherwise = go s ts'

        matchBracket :: Token -> Token
        matchBracket (Lex.Symbol Lex.LParen p) = Lex.Symbol Lex.RParen p
        matchBracket (Lex.Symbol Lex.LBracket p) = Lex.Symbol Lex.RBracket p
        matchBracket (Lex.Symbol Lex.LBrace p) = Lex.Symbol Lex.RBrace p
        matchBracket _ = error "matchBracket: expected left bracket"

        eq :: Token -> Token -> Bool
        eq (Lex.Symbol a _) (Lex.Symbol b _) = a == b
        eq _ _ = error "what did u input?"


-- | Classify a numeric literal string into the first matching numeric type.
--   Returns Nothing if the string does not represent a valid number.
classifyNumber :: String -> Token -> Maybe Expression
classifyNumber s t = let xs = [(IntConst, isInt),
                              (LongConst, isLong),
                              (FloatConst, isFloat),
                              (DoubleConst, isDouble),
                              (LongDoubleConst, isLongDouble)]
                    in (\(ctor, _) -> ctor s t) <$> find (\(_, p) -> p s) xs


-- error from parser to a real catchable error
toException :: Path -> Expression -> ErrorKind
toException p (Error ts why) = UE.Parsing $ UE.makeError p (map tokenPos ts) why
toException _ _  = error "Expected an Error expression."


-- Convert a statement into a block.
-- If it's already a block statement, reuse it; otherwise wrap it.
stmtToBlock :: Statement -> Block
stmtToBlock (BlockStmt b) = b
stmtToBlock s = Multiple [s]


-- Convert a statement into a expression.
stmtToExpr :: Token -> Statement -> Expression
stmtToExpr _ (Expr e) = e
stmtToExpr t _ = Error [t] (expectedExpression 1 $ show t)
