module Lex.TokenPass where

import Data.HashSet (HashSet)
import Lex.Token (Token(EOF, TokenPass, Symbol), tokenPos)
import Util.Type (Position(line, len, column), makePosition)

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

    Lex.BitRShift, Lex.BitLShift, Lex.LogicalOr, Lex.LogicalAnd, Lex.LogicalNot, Lex.LogicalNand, Lex.LogicalNor, Lex.NotArrow, Lex.Arrow, Lex.IffArrow,

    Lex.Plus, Lex.Minus, Lex.Multiply, Lex.Divide, Lex.Modulo, Lex.Power,

    Lex.At, Lex.Dollar,

    Lex.LParen, Lex.LBracket, Lex.LBrace,

    Lex.Semicolon, Lex.Comma, Lex.Question, Lex.Colon, Lex.Arrow, Lex.IffArrow, Lex.FatArrow,
    Lex.QuestionArrow, Lex.Dot, Lex.DoubleDot]


banNextSet :: HashSet Lex.Symbol
banNextSet = HashSet.fromList [
    Lex.Assign, Lex.BitLShiftAssign, Lex.BitRShiftAssign, Lex.BitOrAssign, Lex.BitXorAssign,
    Lex.BitXnorAssign, Lex.PlusAssign, Lex.MinusAssign, Lex.MultiplyAssign, Lex.DivideAssign,
    Lex.ModuloAssign, Lex.PowerAssign,

    Lex.Equal, Lex.NotEqual, Lex.GreaterThan, Lex.LessThan, Lex.GreaterEqual, Lex.LessEqual,

    Lex.BitRShift, Lex.BitLShift, Lex.LogicalOr, Lex.LogicalAnd, Lex.LogicalNand, Lex.LogicalNor, Lex.NotArrow, Lex.Arrow, Lex.IffArrow,

    Lex.Multiply, Lex.Divide, Lex.Modulo, Lex.Power,

    Lex.LParen, Lex.LBracket, Lex.RBrace,

    Lex.Semicolon, Lex.Comma, Lex.Question, Lex.Colon, Lex.Arrow, Lex.IffArrow, Lex.FatArrow,
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
insertTokenPass  = dedupNL . go 0 (Nothing, Nothing, Nothing)
    where
        dedupNL :: [Token] -> [Token]
        dedupNL = subGo False
            where
                subGo _ [] = []
                subGo seenNL (t:ts)
                    | isNL t = if seenNL then subGo True ts else t : subGo True ts
                    | otherwise = t : subGo False ts

        go :: Int -> (Maybe Token, Maybe Token, Maybe Token) -> [Token] -> [Token]
        go _ _ [] = []
        go parenDepth ctx@(_, prev, _) (cursor:rest)
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

                    out = concat [maybeInsertNL parenDepth ctx cursor,
                            [cursor], inner'', [rbrace], after]
                    ctx' = advanceMany ctx out
                in out ++ go parenDepth ctx' rest'

            | otherwise =
                let out = maybeInsertNL parenDepth ctx cursor ++ [cursor]
                    parenDepth' = updateParenDepth parenDepth cursor
                    ctx' = advanceMany ctx out
                in out ++ go parenDepth' ctx' rest

            where
                isEOF :: Token -> Bool
                isEOF (EOF _) = True
                isEOF _       = False

       
        maybeInsertNL :: Int -> (Maybe Token, Maybe Token, Maybe Token) -> Token -> [Token]
        maybeInsertNL _ (_, Nothing, _) _ = []
        maybeInsertNL parenDepth (prevPrev, Just prevTok, lineHead) cursor =
            let lp      = line (tokenPos prevTok)
                lc      = line (tokenPos cursor)
                okLine  = lc > lp
                okDepth = parenDepth == 0
                okPrev  = not (banPrev prevTok) || isImportWildcardLineTail prevPrev prevTok lineHead
                okNext  = not (banNext cursor)
            in [mkNL cursor | okLine && okDepth && okPrev && okNext]

        isImportWildcardLineTail :: Maybe Token -> Token -> Maybe Token -> Bool
        isImportWildcardLineTail prevPrev prevTok lineHead =
            isMultiply prevTok
                && maybe False isDot prevPrev
                && maybe False isImportKw lineHead
                && maybe False (\h -> line (tokenPos h) == line (tokenPos prevTok)) lineHead

        isMultiply :: Token -> Bool
        isMultiply (Symbol Lex.Multiply _) = True
        isMultiply _ = False

        isDot :: Token -> Bool
        isDot (Symbol Lex.Dot _) = True
        isDot _ = False

        isImportKw :: Token -> Bool
        isImportKw (Lex.Ident "import" _) = True
        isImportKw _ = False

        advanceMany :: (Maybe Token, Maybe Token, Maybe Token) -> [Token] -> (Maybe Token, Maybe Token, Maybe Token)
        advanceMany = foldl advanceOne

        advanceOne :: (Maybe Token, Maybe Token, Maybe Token) -> Token -> (Maybe Token, Maybe Token, Maybe Token)
        advanceOne (_, Nothing, _) tok = (Nothing, Just tok, Just tok)
        advanceOne (_, Just prevTok, lineHead') tok
            | line (tokenPos prevTok) == line (tokenPos tok) = (Just prevTok, Just tok, lineHead')
            | otherwise = (Just prevTok, Just tok, Just tok)


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


-- English comment:
-- Rewrite concise template-call syntax:
--   add<int>(x)
-- into lexer-normalized form:
--   add::<int>(x)
--
-- This keeps grammar unambiguous (generic calls still parse via '::' '<...>'),
-- while allowing users to omit '::' at call site.
--
-- Heuristics to avoid rewriting relational expressions:
-- 1) callee and '<' must be adjacent (no whitespace gap)
-- 2) generic close and '(' must be adjacent
-- 3) content inside '<...>' must be type-like tokens only
--
-- Example rewritten:
--   foo<HashMap::<List::<Int>>>(h)
normalizeTemplateCallSyntax :: [Token] -> [Token]
normalizeTemplateCallSyntax = go Nothing
    where
        go :: Maybe Token -> [Token] -> [Token]
        go _ [] = []
        go prev (t:ts)
            | isLessThan t
            , Just prevTok <- prev
            , isIdent prevTok
            , isAdjacent prevTok t
            , Just (genericChunk, rest, closeTok) <- takeTypeLikeAngleChunk (t:ts)
            , (openParen:_) <- rest
            , isLParen openParen
            , isAdjacent closeTok openParen =
                let dc = mkInsertedDoubleColon t
                    emitted = dc : genericChunk
                    nextPrev = Just (last emitted)
                in emitted ++ go nextPrev rest
            | otherwise = t : go (Just t) ts

        takeTypeLikeAngleChunk :: [Token] -> Maybe ([Token], [Token], Token)
        takeTypeLikeAngleChunk [] = Nothing
        takeTypeLikeAngleChunk (lt:rest)
            | not (isLessThan lt) = Nothing
            | otherwise = scan 1 [lt] rest

        scan :: Int -> [Token] -> [Token] -> Maybe ([Token], [Token], Token)
        scan _ _ [] = Nothing
        scan depth revAcc (x:xs)
            | isLessThan x =
                scan (depth + 1) (x : revAcc) xs
            | isGreaterThan x =
                let depth' = depth - 1
                in if depth' == 0
                    then let chunk = reverse (x : revAcc)
                        in Just (chunk, xs, x)
                    else if depth' > 0
                        then scan depth' (x : revAcc) xs
                        else Nothing
            | isBitRShift x =
                if depth <= 1
                    then Nothing
                    else
                        let depth' = depth - 2
                        in if depth' == 0
                            then let chunk = reverse (x : revAcc)
                                in Just (chunk, xs, x)
                            else if depth' > 0
                                then scan depth' (x : revAcc) xs
                                else Nothing
            | isTypeLikeInner x =
                scan depth (x : revAcc) xs
            | otherwise = Nothing

        isTypeLikeInner :: Token -> Bool
        isTypeLikeInner (Lex.Ident _ _) = True
        isTypeLikeInner (Symbol Lex.Dot _) = True
        isTypeLikeInner (Symbol Lex.Comma _) = True
        isTypeLikeInner (Symbol Lex.DoubleColon _) = True
        isTypeLikeInner _ = False

        isIdent :: Token -> Bool
        isIdent (Lex.Ident _ _) = True
        isIdent _ = False

        isLessThan :: Token -> Bool
        isLessThan (Symbol Lex.LessThan _) = True
        isLessThan _ = False

        isGreaterThan :: Token -> Bool
        isGreaterThan (Symbol Lex.GreaterThan _) = True
        isGreaterThan _ = False

        isBitRShift :: Token -> Bool
        isBitRShift (Symbol Lex.BitRShift _) = True
        isBitRShift _ = False

        isLParen :: Token -> Bool
        isLParen (Symbol Lex.LParen _) = True
        isLParen _ = False

        isAdjacent :: Token -> Token -> Bool
        isAdjacent a b =
            let pa = tokenPos a
                pb = tokenPos b
            in line pa == line pb && (column pa + len pa == column pb)

        mkInsertedDoubleColon :: Token -> Token
        mkInsertedDoubleColon anchor =
            let p = tokenPos anchor
            in Symbol Lex.DoubleColon (p { len = 2 })


