module Lex.NewLine where

import Data.HashSet (HashSet)
import Lex.Token
import Util.Type

import qualified Data.HashSet as HashSet
import qualified Lex.Token as Lex


-- English comment:
-- Precomputed ban-sets for fast membership checks.
banPrevSet :: HashSet Lex.Symbol
banPrevSet = HashSet.fromList [
    Lex.BitReverse,
    Lex.PlusPlus, Lex.MinusMinus,
    Lex.At, Lex.Dollar,

    Lex.LBrace,

    Lex.Semicolon, Lex.Comma, Lex.Dot, Lex.DoubleDot, Lex.Colon, Lex.Arrow, Lex.FatArrow, Lex.QuestionArrow, Lex.Question, Lex.Backslash]


banNextSet :: HashSet Lex.Symbol
banNextSet = HashSet.fromList [
    Lex.Assign, Lex.BitLShiftAssign, Lex.BitRShiftAssign, Lex.BitOrAssign, Lex.BitXorAssign, Lex.BitXnorAssign,
    Lex.PlusAssign, Lex.MinusAssign, Lex.MultiplyAssign, Lex.DivideAssign, Lex.ModuloAssign, Lex.PowerAssign,
    
    Lex.Equal, Lex.NotEqual, Lex.GreaterThan, Lex.LessThan, Lex.GreaterEqual, Lex.LessEqual,

    Lex.BitRShift, Lex.BitLShift, Lex.BitOr, Lex.BitXor, Lex.BitXnor, Lex.BitAnd,
    
    Lex.Multiply, Lex.Divide, Lex.Modulo, Lex.Power,
    
    Lex.PlusPlus, Lex.MinusMinus,

    Lex.LBrace, Lex.RBrace,

    Lex.Semicolon, Lex.Comma, Lex.Dot, Lex.DoubleDot, Lex.Colon, Lex.Arrow, Lex.FatArrow, Lex.QuestionArrow, Lex.Question, Lex.Backslash]


-- English comment:
-- banPrev prevTok == True means: do NOT insert NL after prevTok.
{-# INLINE banPrev #-}
banPrev :: Token -> Bool
banPrev (Symbol sym _) = HashSet.member sym banPrevSet
banPrev _ = False


-- English comment:
-- banNext cursor == True means: do NOT insert NL before cursor.
{-# INLINE banNext #-}
banNext :: Token -> Bool
banNext (Symbol sym _) = HashSet.member sym banNextSet
banNext _ = False



-- | Insert NL tokens inferred from Position.line changes.
-- English comment:
-- The tokenizer removed NL, so we re-insert NL by checking whether
-- the line number changed between adjacent tokens.
-- We also recursively process inside '{ ... }' blocks.
-- Additionally, we do NOT insert NL inside () or [].
-- Finally, we collapse multiple consecutive NL into a single NL.
insertNewLine :: [Token] -> [Token]
insertNewLine = dedupNL . go 0 Nothing
    where
        -- English comment:
        -- Remove repeated NLs produced by blank lines / block stitching.
        dedupNL :: [Token] -> [Token]
        dedupNL = loop False
          where
            loop :: Bool -> [Token] -> [Token]
            loop _ [] = []
            loop prevWasNL (t:ts) =
              case t of
                NewLine _ ->
                  if prevWasNL
                    then loop True ts
                    else t : loop True ts
                _ -> t : loop False ts

        -- English comment:
        -- parenDepth tracks only () and [] nesting; braces {} are handled by takeBlock recursion.
        go :: Int -> Maybe Token -> [Token] -> [Token]
        go _ _ [] = []

        go parenDepth prev (cursor:rest)
            | isLBrace cursor =
                let (inner, rbrace, rest') = takeBlock rest
                    inner' = insertNewLine inner
                    out = maybeInsertNL parenDepth prev cursor ++ [cursor] ++ inner' ++ [rbrace]
                in out ++ go parenDepth (Just rbrace) rest'

            | otherwise =
                let out = maybeInsertNL parenDepth prev cursor ++ [cursor]
                    parenDepth' = updateParenDepth parenDepth cursor
                in out ++ go parenDepth' (Just cursor) rest

        -- English comment:
        -- Insert one NL token between prev and cursor iff:
        --   * line(cursor) > line(prev)
        --   * parenDepth == 0 (not inside () or [])
        --   * prev is NOT banned (banPrev prev == False)
        --   * cursor is NOT banned (banNext cursor == False)
        maybeInsertNL :: Int -> Maybe Token -> Token -> [Token]
        maybeInsertNL _ Nothing _ = []
        maybeInsertNL parenDepth (Just prevTok) cursor =
            let lp = line (tokenPos prevTok)
                lc = line (tokenPos cursor)
                okLine  = lc > lp
                okDepth = parenDepth == 0
                okPrev  = not (banPrev prevTok)
                okNext  = not (banNext cursor)
            in [mkNL cursor | okLine && okDepth && okPrev && okNext]

        -- English comment:
        -- Put NL at the beginning of the cursor token's line (col=0, len=0).
        mkNL :: Token -> Token
        mkNL cursor = let l = line (tokenPos cursor)
                          pos = makePosition l 0 0 in NewLine pos

        --------------------------------------------------------------------------------
        -- Paren depth tracking for () and []
        --------------------------------------------------------------------------------

        updateParenDepth :: Int -> Token -> Int
        updateParenDepth d tok
            | isLParenLike tok = d + 1
            | isRParenLike tok = d - 1
            | otherwise        = d

        isLParenLike :: Token -> Bool
        isLParenLike (Symbol Lex.LParen _)   = True
        isLParenLike (Symbol Lex.LBracket _) = True
        isLParenLike _ = False

        isRParenLike :: Token -> Bool
        isRParenLike (Symbol Lex.RParen _)   = True
        isRParenLike (Symbol Lex.RBracket _) = True
        isRParenLike _ = False

        --------------------------------------------------------------------------------
        -- Block handling (supports nested blocks)
        --------------------------------------------------------------------------------

        takeBlock :: [Token] -> ([Token], Token, [Token])
        takeBlock = loop 1 []
            where
                loop :: Int -> [Token] -> [Token] -> ([Token], Token, [Token])
                loop _ _ [] = error "Unterminated block: missing '}'"
                loop depth acc (x:xs)
                    | isLBrace x = loop (depth + 1) (x:acc) xs
                    | isRBrace x =
                        if depth == 1
                            then (reverse acc, x, xs)
                            else loop (depth - 1) (x:acc) xs
                    | otherwise  = loop depth (x:acc) xs

        isLBrace :: Token -> Bool
        isLBrace (Symbol Lex.LBrace _) = True
        isLBrace _ = False

        isRBrace :: Token -> Bool
        isRBrace (Symbol Lex.RBrace _) = True
        isRBrace _ = False

