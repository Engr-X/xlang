module Lex.NewLine where

import Data.HashSet (HashSet)
import Lex.Token
import Util.Type

import qualified Data.HashSet as HashSet
import qualified Lex.Token as Lex


--------------------------------------------------------------------------------
-- English comment:
-- Precomputed ban-sets for fast membership checks.
-- banPrev prevTok == True means: do NOT insert NL after prevTok.
-- banNext cursor  == True means: do NOT insert NL before cursor.
--------------------------------------------------------------------------------

banPrevSet :: HashSet Lex.Symbol
banPrevSet = HashSet.fromList [
    Lex.Assign, Lex.BitLShiftAssign, Lex.BitRShiftAssign, Lex.BitOrAssign, Lex.BitXorAssign,
    Lex.BitXnorAssign, Lex.PlusAssign, Lex.MinusAssign, Lex.MultiplyAssign, Lex.DivideAssign,
    Lex.ModuloAssign, Lex.PowerAssign,

    Lex.Equal, Lex.NotEqual, Lex.GreaterThan, Lex.LessThan, Lex.GreaterEqual, Lex.LessEqual,

    Lex.BitRShift, Lex.BitLShift, Lex.BitOr, Lex.BitXor, Lex.BitXnor, Lex.BitAnd, Lex.BitReverse,

    Lex.Plus, Lex.Minus, Lex.Multiply, Lex.Divide, Lex.Modulo, Lex.Power,

    Lex.At, Lex.Dollar,

    Lex.LParen, Lex.LBracket, Lex.LBrace,

    Lex.Semicolon, Lex.Comma, Lex.Question, Lex.Colon, Lex.Arrow, Lex.FatArrow,
    Lex.QuestionArrow, Lex.Dot, Lex.DoubleDot
  ]


banNextSet :: HashSet Lex.Symbol
banNextSet = HashSet.fromList [
    Lex.Assign, Lex.BitLShiftAssign, Lex.BitRShiftAssign, Lex.BitOrAssign, Lex.BitXorAssign,
    Lex.BitXnorAssign, Lex.PlusAssign, Lex.MinusAssign, Lex.MultiplyAssign, Lex.DivideAssign,
    Lex.ModuloAssign, Lex.PowerAssign,

    Lex.Equal, Lex.NotEqual, Lex.GreaterThan, Lex.LessThan, Lex.GreaterEqual, Lex.LessEqual,

    Lex.BitRShift, Lex.BitLShift, Lex.BitOr, Lex.BitXor, Lex.BitXnor, Lex.BitAnd,

    Lex.Multiply, Lex.Divide, Lex.Modulo, Lex.Power,

    Lex.LParen, Lex.LBracket, Lex.LBrace, Lex.RBrace,

    Lex.Semicolon, Lex.Comma, Lex.Question, Lex.Colon, Lex.Arrow, Lex.FatArrow,
    Lex.QuestionArrow, Lex.Dot, Lex.DoubleDot
  ]


{-# INLINE banPrev #-}
banPrev :: Token -> Bool
banPrev (Symbol sym _) = HashSet.member sym banPrevSet
banPrev _ = False


{-# INLINE banNext #-}
banNext :: Token -> Bool
banNext (Symbol sym _) = HashSet.member sym banNextSet
banNext _ = False


--------------------------------------------------------------------------------
-- Insert NL tokens inferred from Position.line changes.
-- Tokenizer removed NL, so we re-insert NL by checking line jumps.
-- We also recursively process inside '{ ... }' blocks.
-- We do NOT insert NL inside () or [].
-- Additionally: we "prefer" to put a statement boundary around '}' (before/after),
-- but skip if there is already a separator (NL or ';') adjacent.
--------------------------------------------------------------------------------

insertNewLine :: [Token] -> [Token]
insertNewLine = dedupNL . go 0 Nothing
    where
        --------------------------------------------------------------------------------
        -- Collapse multiple consecutive NL into a single NL.
        --------------------------------------------------------------------------------
        dedupNL :: [Token] -> [Token]
        dedupNL = loop False
            where
                loop :: Bool -> [Token] -> [Token]
                loop _ [] = []
                loop prevWasNL (t:ts) = case t of
                    NewLine _ -> if prevWasNL then loop True ts else t : loop True ts
                    _ -> t : loop False ts


        --------------------------------------------------------------------------------
        -- Main pass. parenDepth tracks only () and [] nesting.
        -- Braces {} are handled via takeBlock recursion.
        --------------------------------------------------------------------------------
        go :: Int -> Maybe Token -> [Token] -> [Token]
        go _ _ [] = []
        go parenDepth prev (cursor:rest)
            | isLBrace cursor =
                let (inner, rbrace, rest') = takeBlock rest
                    inner' = insertNewLine inner
                                    -- Prefer an NL right before '}' unless the inner already ends with NL or ';'.
                    inner'' = ensureSepAtEnd rbrace inner'

                    -- Prefer an NL right after '}' unless the next token is already NL or ';'.
                    after   = ensureSepAfter rbrace rest'

                    out = maybeInsertNL parenDepth prev cursor
                            ++ [cursor]
                            ++ inner''
                            ++ [rbrace]
                            ++ after
                in out ++ go parenDepth (Just (lastAnchor rbrace after)) rest'

            | otherwise =
                let out = maybeInsertNL parenDepth prev cursor ++ [cursor]
                    parenDepth' = updateParenDepth parenDepth cursor
                in out ++ go parenDepth' (Just cursor) rest

       
        -- Ensure there is a statement separator (NL or ';') right before EOF.
        -- This is a post-pass and does NOT change any existing insertion behavior.
        --------------------------------------------------------------------------------
        ensureSepAtEOF :: [Token] -> [Token]
        ensureSepAtEOF [] = []
        ensureSepAtEOF ts =
            case splitEOF ts of
                Nothing -> ts
                Just (prefix, eofTok) ->
                    case reverse prefix of
                        (t:_) | isSep t -> prefix ++ [eofTok]
                        [] -> [mkNL eofTok, eofTok]
                        _ -> prefix ++ [mkNL eofTok, eofTok]


        -- Split tokens into (tokens before EOF, EOF token).
        -- Return Nothing if EOF is absent.
        splitEOF :: [Token] -> Maybe ([Token], Token)
        splitEOF = goSplit []
            where
                goSplit :: [Token] -> [Token] -> Maybe ([Token], Token)
                goSplit _ [] = Nothing
                goSplit acc (t:ts) = case t of
                    EOF _ -> Just (reverse acc, t)
                    _ -> goSplit (t:acc) ts


        --------------------------------------------------------------------------------
        -- Insert one inferred NL between prev and cursor iff:
        --   * line(cursor) > line(prev)
        --   * parenDepth == 0 (not inside () or [])
        --   * prev is NOT banned
        --   * cursor is NOT banned
        --------------------------------------------------------------------------------
        maybeInsertNL :: Int -> Maybe Token -> Token -> [Token]
        maybeInsertNL _ Nothing _ = []
        maybeInsertNL parenDepth (Just prevTok) cursor =
            let lp      = line (tokenPos prevTok)
                lc      = line (tokenPos cursor)
                okLine  = lc > lp
                okDepth = parenDepth == 0
                okPrev  = not (banPrev prevTok)
                okNext  = not (banNext cursor)
            in [mkNL cursor | okLine && okDepth && okPrev && okNext]


        --------------------------------------------------------------------------------
        -- Construct a NL token anchored at the cursor's line (col=0, len=0).
        --------------------------------------------------------------------------------
        mkNL :: Token -> Token
        mkNL cursor = let l = line (tokenPos cursor)
                          pos = makePosition l 0 0 in NewLine pos


        --------------------------------------------------------------------------------
        -- Separator checks (used for "prefer NL around '}'" logic).
        --------------------------------------------------------------------------------
        isNL :: Token -> Bool
        isNL (NewLine _) = True
        isNL _ = False

        isSemi :: Token -> Bool
        isSemi (Symbol Lex.Semicolon _) = True
        isSemi _ = False

        isSep :: Token -> Bool
        isSep t = isNL t || isSemi t


        -- Ensure the token list ends with a separator; if already ends with NL or ';', do nothing.
        -- Otherwise, append a NL anchored at 'anchor'.
        ensureSepAtEnd :: Token -> [Token] -> [Token]
        ensureSepAtEnd anchor ts = case reverse ts of
            (t:_) | isSep t -> ts
            _               -> ts ++ [mkNL anchor]

        -- English comment:
        -- Ensure a separator right after '}' unless the next token is already NL or ';'.
        -- Return either [] or [NL].
        ensureSepAfter :: Token -> [Token] -> [Token]
        ensureSepAfter anchor next = case next of
            (t:_) | isSep t -> []
            _  -> [mkNL anchor]

        -- English comment:
        -- Decide what to use as "prev token" after emitting after-separator.
        lastAnchor :: Token -> [Token] -> Token
        lastAnchor rbrace after = case after of
            (t:_) -> t
            _  -> rbrace


        --------------------------------------------------------------------------------
        -- English comment:
        -- Paren depth tracking for () and [] only.
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
        -- English comment:
        -- Block handling (supports nested blocks).
        --------------------------------------------------------------------------------
        takeBlock :: [Token] -> ([Token], Token, [Token])
        takeBlock = loop 1 []
            where
                loop :: Int -> [Token] -> [Token] -> ([Token], Token, [Token])
                loop _ _ [] = error "Unterminated block: missing '}'"
                loop depth acc (x:xs)
                    | isLBrace x = loop (depth + 1) (x:acc) xs
                    | isRBrace x = if depth == 1
                        then (reverse acc, x, xs)
                        else loop (depth - 1) (x:acc) xs
                    | otherwise  = loop depth (x:acc) xs

        isLBrace :: Token -> Bool
        isLBrace (Symbol Lex.LBrace _) = True
        isLBrace _ = False

        isRBrace :: Token -> Bool
        isRBrace (Symbol Lex.RBrace _) = True
        isRBrace _ = False
