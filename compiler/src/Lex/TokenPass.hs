module Lex.TokenPass where

import Data.HashSet (HashSet)
import Lex.Token
import Util.Type

import qualified Data.HashSet as HashSet
import qualified Lex.Token as Lex


--------------------------------------------------------------------------------
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

    Lex.BitRShift, Lex.BitLShift, Lex.BitOr, Lex.BitXor, Lex.BitXnor, Lex.BitAnd, Lex.BitNot,

    Lex.Plus, Lex.Minus, Lex.Multiply, Lex.Divide, Lex.Modulo, Lex.Power,

    Lex.At, Lex.Dollar,

    Lex.LParen, Lex.LBracket, Lex.LBrace,

    Lex.Semicolon, Lex.Comma, Lex.Question, Lex.Colon, Lex.Arrow, Lex.FatArrow,
    Lex.QuestionArrow, Lex.Dot, Lex.DoubleDot]


banNextSet :: HashSet Lex.Symbol
banNextSet = HashSet.fromList [
    Lex.Assign, Lex.BitLShiftAssign, Lex.BitRShiftAssign, Lex.BitOrAssign, Lex.BitXorAssign,
    Lex.BitXnorAssign, Lex.PlusAssign, Lex.MinusAssign, Lex.MultiplyAssign, Lex.DivideAssign,
    Lex.ModuloAssign, Lex.PowerAssign,

    Lex.Equal, Lex.NotEqual, Lex.GreaterThan, Lex.LessThan, Lex.GreaterEqual, Lex.LessEqual,

    Lex.BitRShift, Lex.BitLShift, Lex.BitOr, Lex.BitXor, Lex.BitXnor, Lex.BitAnd,

    Lex.Multiply, Lex.Divide, Lex.Modulo, Lex.Power,

    Lex.LParen, Lex.LBracket, Lex.RBrace,

    Lex.Semicolon, Lex.Comma, Lex.Question, Lex.Colon, Lex.Arrow, Lex.FatArrow,
    Lex.QuestionArrow, Lex.Dot, Lex.DoubleDot]


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

insertTokenPass  :: [Token] -> [Token]
insertTokenPass  = dedupNL . go 0 Nothing
    where
        dedupNL :: [Token] -> [Token]
        dedupNL = subGo False
            where
                subGo _ [] = []
                subGo seenNL (t:ts)
                    | isNL t = if seenNL then subGo True ts else t : subGo True ts
                    | otherwise = t : subGo False ts

        go :: Int -> Maybe Token -> [Token] -> [Token]
        go _ _ [] = []
        go parenDepth prev (cursor:rest)
            | isEOF cursor = case prev of
                Just prevTok
                    | isSep prevTok -> [cursor]
                    | otherwise     -> [mkNLAtEOF cursor, cursor]
                Nothing -> [mkNLAtEOF cursor, cursor]

            | isLBrace cursor =
                let (inner, rbrace, rest') = takeBlock rest
                    inner' = insertTokenPass  inner
                                    -- Prefer an NL right before '}' unless the inner already ends with NL or ';'.
                    inner'' = ensureSepAtEnd rbrace inner'

                    -- Prefer an NL right after '}' unless the next token is already NL or ';'.
                    after   = ensureSepAfter rbrace rest'

                    out = concat [maybeInsertNL parenDepth prev cursor,
                            [cursor], inner'', [rbrace], after]
                in out ++ go parenDepth (Just (lastAnchor rbrace after)) rest'

            | otherwise =
                let out = maybeInsertNL parenDepth prev cursor ++ [cursor]
                    parenDepth' = updateParenDepth parenDepth cursor
                in out ++ go parenDepth' (Just cursor) rest

            where
                isEOF :: Token -> Bool
                isEOF (EOF _) = True
                isEOF _       = False

       
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


        mkNL :: Token -> Token
        mkNL cursor = let l = line (tokenPos cursor)
                          pos = makePosition l 0 0 in TokenPass  pos

        mkNLAtEOF :: Token -> Token
        mkNLAtEOF eofTok = TokenPass  (tokenPos eofTok)


        isNL :: Token -> Bool
        isNL (TokenPass  _) = True
        isNL _ = False


        isSemi :: Token -> Bool
        isSemi (Symbol Lex.Semicolon _) = True
        isSemi _ = False


        isSep :: Token -> Bool
        isSep t = isNL t || isSemi t


        ensureSepAtEnd :: Token -> [Token] -> [Token]
        ensureSepAtEnd anchor ts = case reverse ts of
            (t:_) | isSep t -> ts
            _               -> ts ++ [mkNL anchor]

       
        ensureSepAfter :: Token -> [Token] -> [Token]
        ensureSepAfter anchor next = case next of
            (t:_) | isSep t -> []
            _  -> [mkNL anchor]


        lastAnchor :: Token -> [Token] -> Token
        lastAnchor rbrace after = case after of
            (t:_) -> t
            _  -> rbrace


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


-- English comment:
-- Split '>>' into '>' '>' only inside generic-arg context: started by '::' '<' and ended by matching '>' depth.
splitShiftInGenerics :: [Token] -> [Token]
splitShiftInGenerics = go 0
    where
        go :: Int -> [Token] -> [Token]
        go _ [] = []
        go depth (t1:t2:ts)
            -- Enter or nest generic: '::' '<'
            | isDoubleColon t1 && isLessThan t2 = t1 : t2 : go (depth + 1) ts

                -- Inside generic: split '>>' into '>' '>'
            | depth > 0 && isBitRShift t1 =
                let p   = tokenPos t1
                    gt1 = Symbol Lex.GreaterThan (withLen 1 p)
                    gt2 = Symbol Lex.GreaterThan (withLen 1 (shiftCol 1 p))
                in gt1 : gt2 : go depth (t2:ts)

            -- Inside generic: close '>'
            | depth > 0 && isGreaterThan t1 = t1 : go (depth - 1) (t2:ts)
            | otherwise = t1 : go depth (t2:ts)

        go depth [t]
            | depth > 0 && isBitRShift t = let p = tokenPos t
                in [
                    Symbol Lex.GreaterThan (withLen 1 p),
                    Symbol Lex.GreaterThan (withLen 1 (shiftCol 1 p))]
            | otherwise = [t]

        isDoubleColon (Symbol Lex.DoubleColon _) = True
        isDoubleColon _ = False

        isLessThan (Symbol Lex.LessThan _) = True
        isLessThan _ = False

        isGreaterThan (Symbol Lex.GreaterThan _) = True
        isGreaterThan _ = False

        isBitRShift (Symbol Lex.BitRShift _) = True
        isBitRShift _ = False

        -- English comment: adjust only column; len is handled by withLen.
        shiftCol :: Int -> Position -> Position
        shiftCol k pos = pos { column = column pos + k }

        -- English comment: force token length.
        withLen :: Int -> Position -> Position
        withLen n pos = pos { len = n }

