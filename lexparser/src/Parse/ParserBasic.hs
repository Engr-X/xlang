module Parse.ParserBasic where

import Data.List (find)
import Lex.Token (Token, isLBracketToken, isRBracketToken)
import Parse.SyntaxTree (Expression(..))
import Util.Type (makePosition)
import Util.Basic (isInt, isLong, isFloat, isDouble, isLongDouble)

import qualified Lex.Token as Lex


--  Choose a token to anchor diagnostics.
nearestTok :: [Token] -> Token
nearestTok (t:_) = t
nearestTok [] = Lex.EOF (makePosition 0 0 0)  -- ideally real EOF pos from tokenizer


-- Canonical fatal parse error expression.
mkHappyErrorExpr :: [Token] -> Expression
mkHappyErrorExpr ts = let t = nearestTok ts in Error t ("Parse error near: " ++ show t)


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
classifyNumber :: String -> Maybe Expression
classifyNumber s = let types = [(IntConst, isInt), (LongConst, isLong), (FloatConst, isFloat), (DoubleConst, isDouble), (LongDoubleConst, isLongDouble)] in
    fmap (\(f, _) -> f s) $ find (\(_, f) -> f s) types
