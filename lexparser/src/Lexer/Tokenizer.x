{
module Lexer.Tokenizer where

import Data.List (partition)
import Numeric (readHex, readOct)
import Lexer.Token
import Util.Exception (ErrorKind, internalErrorMsg, unterminatedStrLiteralMsg)
import Util.Types (Position, Path)

import qualified Data.Char as DC
import qualified Util.Exception as UE
import qualified Util.Types as UTypes
import qualified Lexer.Token as LT
}

%wrapper "monadUserState"

$letter = [a-zA-Z]
$digit  = [0-9]
$underscore = [_]
$space = [\ \t\r\n\f\v]
$sign = [\-\+]
$hexdigit = [0-9a-fA-F]


-- intList = [+-]?[0-9]+
-- $hexIntLit = [+-]?0(x|X)[0-9a-fA-F]+
-- $hexLongLit = $hexIntLit(l/L)

-- $doubleLit = [+-]?((([0-9]+([.][0-9]*)|[.][0-9]+)([eE][+-]?[0-9]+)?)|([0-9]+([eE][+-]?[0-9]+)))
-- $floatLit = $doubleLit(f/F)
-- $floatLit = $doubleLit(l/L)

-- $numberLit = $intLit | $longLit | $hexIntLit | $hexLongLit | $doubleLit | $floatLit | $floatLit
@intLit = $sign? $digit+
@longLit = $sign? $digit+ [lL]

@hexPrefix = 0 [xX]
@hexIntLit = $sign? @hexPrefix $hexdigit+
@hexLongLit = @hexIntLit [lL]

@expPart = [eE] $sign? $digit+

@frac1 = $digit+ \. $digit*
@frac2 = \. $digit+
@decimal = (@frac1 | @frac2)

@doubleLit = $sign? ((@decimal @expPart?) | ($digit+ @expPart))

@floatLit = @doubleLit [fF]
@doubleLongLit = @doubleLit [lL]

@numberLit  = @hexLongLit | @hexIntLit | @longLit | @intLit | @floatLit | @doubleLongLit | @doubleLit


-- string pattern


-- rules
tokens :-

-- comment part

-- enter line comment states (only in default state)
<0> "//"                { beginLineComment }
<0> "#"                 { beginLineComment }

-- enter block comment state (only in default state)
<0> "/*"                { beginBlockComment }

-- comment: eat until newline, then exit
<icomment> \r?\n        { endLineComment } --  end of the line comment state end
<icomment> .            { skipTok }        -- other char just skip


-- comment: eat until */, allow newlines
<comment> "*/"          { endBlockComment } -- encounter */ the block comment end
<comment> .             { skipTok }         -- other anything just skip


-- normal tokens (default state!)

-- if match a number then eat as number
<0> @numberLit          { eatNumber }

-- if there is letter or underscore then eat as identity
<0> ($letter | $underscore) ($letter | $digit | $underscore)* { eatIdent }


-- string part

-- enter string literal
<0> \'                  { beginChar }
<0> \"                  { beginString }


-- char state rules



-- string state rules
<string> \"             { endString }
<string> \\\\ .         { skipTok }   -- escaped char: \" \\ \n etc.
<string> [^\"\\\n]+     { skipTok }   -- normal chars
<string> \n             { unterminatedString }


-- if  there is space col ++
<0> $space+             { skipTok }

-- other char will throw a error
<0> .                   { lexError }

{
-- ! create postion by using alexPosn
makePos :: AlexPosn -> Int -> Position
makePos (AlexPn _ line col) = UTypes.makePosition line col


-- | Removes the surrounding quotes from a string literal.
-- | Example: "\"hello\"" -> "hello"
unwrapString :: String -> String 
unwrapString = init . tail


-- | Converts a string representing a character literal into the actual character.
-- Handles escaped characters like '\n', '\t', '\xAB', '\u1234', etc.
-- | Converts a string representing a character literal into the actual character.
-- Returns Nothing if the literal is invalid or cannot be decoded.
unwrapChar :: String -> Maybe Char
unwrapChar s = case unwrapStringSafe s of
  Nothing -> Nothing
  Just body -> case body of
    [c]       -> Just c
    '\\':rest -> parseEscape rest
    _         -> Nothing
  where
    -- safer unwrap: require surrounding single quotes
    unwrapStringSafe :: String -> Maybe String
    unwrapStringSafe xs =
      case xs of
        ('\'':mid) | not (null mid) && last mid == '\'' -> Just (init mid)
        _                                               -> Nothing

    readHexInt :: String -> Maybe Int
    readHexInt str =
      case readHex str of
        [(n, "")] -> Just n
        _         -> Nothing

    readOctInt :: String -> Maybe Int
    readOctInt str =
      case readOct str of
        [(n, "")] -> Just n
        _         -> Nothing

    inRangeChar :: Int -> Maybe Char
    inRangeChar n
      | n >= 0 && n <= fromEnum (maxBound :: Char) = Just (DC.chr n)
      | otherwise                                 = Nothing

    parseEscape :: String -> Maybe Char
    parseEscape str = case str of
      "n"  -> Just '\n'
      "t"  -> Just '\t'
      "r"  -> Just '\r'
      "b"  -> Just '\b'
      "f"  -> Just '\f'
      "v"  -> Just '\v'
      "\\" -> Just '\\'
      "'"  -> Just '\''
      "\"" -> Just '\"'
      ('x':hex) | not (null hex) -> readHexInt hex >>= inRangeChar
      ('u':hex) | not (null hex) -> readHexInt hex >>= inRangeChar
      ('U':hex) | not (null hex) -> readHexInt hex >>= inRangeChar
      oct | not (null oct)
          && all (\c -> c >= '0' && c <= '7') oct
          && length oct <= 3     -> readOctInt oct >>= inRangeChar
      _ -> Nothing

-- util

-- skip 1 sapce
skipTok :: AlexInput -> Int -> Alex Token
skipTok _ _ = alexMonadScan

alexEOF :: Alex Token
alexEOF = do
    (p, _, _, _) <- alexGetInput
    return $ Error "<EOF>" (makePos p 0)


-- comment 
beginLineComment :: AlexInput -> Int -> Alex Token
beginLineComment _ _ = alexSetStartCode icomment >> alexMonadScan

endLineComment :: AlexInput -> Int -> Alex Token
endLineComment _ _ = alexSetStartCode 0 >> alexMonadScan

beginBlockComment :: AlexInput -> Int -> Alex Token
beginBlockComment _ _ = alexSetStartCode comment >> alexMonadScan

endBlockComment :: AlexInput -> Int -> Alex Token
endBlockComment _ _ = alexSetStartCode 0 >> alexMonadScan


-- char
beginChar :: AlexInput -> Int -> Alex Token
beginChar (p, _, _, s0) _ = do
    alexSetStartCode char
    alexSetUserState (Just (p, s0))
    alexMonadScan



-- string 
type AlexUserState = Maybe (AlexPosn, String)


alexInitUserState :: AlexUserState
alexInitUserState = Nothing


beginString :: AlexInput -> Int -> Alex Token
beginString (p, _, _, s0) _ = do
    alexSetStartCode string
    alexSetUserState (Just (p, s0))
    alexMonadScan


unterminatedString :: AlexInput -> Int -> Alex Token
unterminatedString (pNow, _, _, sNow) _ = do
    st <- alexGetUserState
    alexSetStartCode 0
    alexSetUserState Nothing
    case st of
        Just (pStart, sStart) -> do
            let consumed = length sStart - length sNow
            let len = max 0 (consumed - 1)
            return $ Error unterminatedStrLiteralMsg (makePos pStart len)
        Nothing -> return $ Error internalErrorMsg (makePos pNow 0)


endString :: AlexInput -> Int -> Alex Token
endString (_pEnd, _, _, sEnd) _ = do
    st <- alexGetUserState
    alexSetStartCode 0
    alexSetUserState Nothing
    case st of
        Just (pStart, sStart) -> do
            let consumed = length sStart - length sEnd
            let lexeme = take consumed sStart
            return $ StrConst (unwrapString lexeme) (makePos pStart consumed)
        Nothing -> return $ Error internalErrorMsg (makePos _pEnd 0)


eatNumber :: AlexInput -> Int -> Alex Token
eatNumber (p, _, _, s) len = let lexeme = take len s in return $ NumberConst lexeme (makePos p len)

eatIdent :: AlexInput -> Int -> Alex Token
eatIdent (p, _, _, s) len = let lexeme = take len s in return $ Ident lexeme (makePos p len)

lexError :: AlexInput -> Int -> Alex Token
lexError (p, _, _, s) len = let bad = take len s in return $ Error bad (makePos p len)



-- Helper to check for EOF sentinel
isEOF :: Token -> Bool
isEOF (Error "<EOF>" _) = True
isEOF _ = False




-- Scanner that collects all tokens, including the EOF token
alexScanTokens :: String -> Either String [Token]
alexScanTokens input = runAlex input collectTokens

collectTokens :: Alex [Token]
collectTokens = loop []
  where
    loop acc = do
      tok <- alexMonadScan
      if isEOF tok
        then return (reverse acc)
        else loop (tok : acc)


convertErrToken :: Path -> Token -> ErrorKind
convertErrToken p (Error why pos) = UE.Lexer $ UE.makeError p pos why
convertErrToken _ _ = error "what the hell is going on???"


tokenize :: Path -> String -> ([ErrorKind], [Token])
tokenize p str =
  case alexScanTokens str of
    Left _ -> ([UE.Unkown p], [])
    Right tokens ->
        let (errs, correct) = partition isErrToken tokens
            errs' = filter (not . isEOF) errs
        in (map (convertErrToken p) errs', correct)

debugTokenize :: String -> ([ErrorKind], [Token])
debugTokenize = tokenize "debug path"
}
