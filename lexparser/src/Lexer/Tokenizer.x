{
module Lexer.Tokenizer where

import Data.List (partition)
import Numeric (readHex, readOct)
import Lexer.Token
import Util.Exception (ErrorKind, internalErrorMsg, unclosedStrLiteralMsg, unclosedCharLiteralMsg, invalidCharLiteralMsg, invalidNumericLiteralMsg)
import Util.Types (Position, Path)

import qualified Data.Char as DC
import qualified Util.Exception as UE
import qualified Util.FileHelper as FileHelper
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
<comment> \r?\n         { skipTok } 
<comment> .             { skipTok }         -- other anything just skip


-- normal tokens (default state!)

-- if match a number then eat as number
<0> @numberLit / ($letter | $underscore)   { invalidNumber }
<0> @numberLit          { eatNumber }

-- if there is letter or underscore then eat as identity
<0> ($letter | $underscore) ($letter | $digit | $underscore)* { eatIdent }


-- string part

-- enter string literal
<0> \'                  { beginChar }
<0> \"                  { beginString }


-- char state rules
<char> \'               { endChar }
<char> \\ .             { skipTok }
<char> \r?\n            { unterminatedChar }
<char> .                { skipTok }


-- string state rules
<string> \"             { endString }
<string> \\ .           { skipTok }
<string> \r?\n          { unterminatedString }
<string> .              { skipTok }


-- if  there is space col ++
<0> $space+             { skipTok }


-- symbol 3 (5)
<0> "<<="               { eatSymbol BitLShiftAssign }
<0> ">>="               { eatSymbol BitRShiftAssign }
<0> "!^="               { eatSymbol BitXnorAssign }
<0> "**="               { eatSymbol PowerAssign }
<0> "?->"               { eatSymbol QuestionArrow }


-- symbol 2 (17)
<0> "|="                { eatSymbol BitOrAssign }
<0> "^="                { eatSymbol BitXorAssign }
<0> "+="                { eatSymbol PlusAssign }
<0> "-="                { eatSymbol MinusAssign }
<0> "*="                { eatSymbol MultiplyAssign }
<0> "/="                { eatSymbol DivideAssign }
<0> "%="                { eatSymbol ModuloAssign }

<0> "=="                { eatSymbol Equal }
<0> "!="                { eatSymbol NotEqual }
<0> ">="                { eatSymbol GreaterEqual }
<0> "<="                { eatSymbol LessEqual }

<0> ">>"                { eatSymbol BitRShift }
<0> "<<"                { eatSymbol BitLShift }
<0> "!^"                { eatSymbol BitXnor }
<0> "**"                { eatSymbol Power }
<0> "++"                { eatSymbol PlusPlus }
<0> "--"                { eatSymbol MinusMinus }
<0> ".."                { eatSymbol DoubleDot }


-- symbol 1 (28)
<0> "="                { eatSymbol Assign }

<0> ">"                { eatSymbol GreaterThan }
<0> "<"                { eatSymbol LessThan }

<0> "|"                { eatSymbol BitOr }
<0> "^"                { eatSymbol BitXor }
<0> "&"                { eatSymbol BitAnd }
<0> "!"                { eatSymbol BitReverse }

<0> "+"                { eatSymbol Plus }
<0> "-"                { eatSymbol Minus }
<0> "*"                { eatSymbol Multiply }
<0> "/"                { eatSymbol Divide }
<0> "%"                { eatSymbol Modulo }

<0> "@"                { eatSymbol At }
<0> "$"                { eatSymbol Dollar }
<0> "("                { eatSymbol LParen }
<0> ")"                { eatSymbol RParen }
<0> "["                { eatSymbol LBracket }
<0> "]"                { eatSymbol RBracket }
<0> "{"                { eatSymbol LBrace }
<0> "}"                { eatSymbol RBrace }

<0> ";"                { eatSymbol Semicolon }
<0> ","                { eatSymbol Comma }
<0> "?"                { eatSymbol Question }
<0> ":"                { eatSymbol Colon }
<0> "."                { eatSymbol Dot }
<0> "\\"               { eatSymbol Backslash }

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
        unwrapStringSafe xs = case xs of
            ('\'':mid) | not (null mid) && last mid == '\'' -> Just (init mid)
            _                                               -> Nothing

        readHexInt :: String -> Maybe Int
        readHexInt str = case readHex str of
            [(n, "")] -> Just n
            _         -> Nothing

        readOctInt :: String -> Maybe Int
        readOctInt str = case readOct str of
            [(n, "")] -> Just n
            _         -> Nothing

        inRangeChar :: Int -> Maybe Char
        inRangeChar n
            | n >= 0 && n <= fromEnum (maxBound :: Char) = Just (DC.chr n)
            | otherwise                                 = Nothing

        parseEscape :: String -> Maybe Char
        parseEscape str = case str of
            "0"  -> Just '\0'
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


-- skip 1 sapce
skipTok :: AlexInput -> Int -> Alex Token
skipTok _ _ = alexMonadScan


alexEOF :: Alex Token
alexEOF = do
    (pNow, _, _, sNow) <- alexGetInput
    st <- alexGetUserState
    alexSetStartCode 0
    alexSetUserState Nothing
    case st of
        Just (KString, pStart, sStart) -> do
            let consumed = length sStart - length sNow
            let len = max 0 (consumed - 1)
            return $ Error unclosedStrLiteralMsg (makePos pStart len)

        Just (KChar, pStart, sStart) -> do
            let consumed = length sStart - length sNow
            let len = max 0 (consumed - 1)
            return $ Error unclosedCharLiteralMsg (makePos pStart len)

        Just (KComment, pStart, sStart) -> do
            let consumed = length sStart - length sNow
            return $ Error UE.unclosedCommentMsg (makePos pStart consumed)

        Nothing ->
            return $ Error "<EOF>" (makePos pNow 0)



-- comment
data LitKind = KString | KChar | KComment
    deriving (Eq, Show)

type AlexUserState = Maybe (LitKind, AlexPosn, String)


-- | Enter line comment mode.
-- All characters are ignored until a newline.
beginLineComment :: AlexInput -> Int -> Alex Token
beginLineComment _ _ = alexSetStartCode icomment >> alexMonadScan


-- | Exit line comment mode.
-- Resets the lexer back to the default state.
endLineComment :: AlexInput -> Int -> Alex Token
endLineComment _ _ = alexSetStartCode 0 >> alexMonadScan


-- | Enter block comment mode.
-- Records the start position for error diagnostics.
beginBlockComment :: AlexInput -> Int -> Alex Token
beginBlockComment (p, _, _, s0) _ = do
    alexSetStartCode comment
    alexSetUserState (Just (KComment, p, s0))
    alexMonadScan


-- | Exit block comment mode.
-- Clears the stored comment state.
endBlockComment :: AlexInput -> Int -> Alex Token
endBlockComment _ _ = do
    alexSetStartCode 0
    alexSetUserState Nothing
    alexMonadScan


-- | Enter character literal mode.
-- Tracks the start position for validation and errors.
beginChar :: AlexInput -> Int -> Alex Token
beginChar (p, _, _, s0) _ = do
    alexSetStartCode char
    alexSetUserState (Just (KChar, p, s0))
    alexMonadScan


-- | Handle an unterminated character literal.
-- Emits an error token and resets lexer state.
unterminatedChar :: AlexInput -> Int -> Alex Token
unterminatedChar (pNow, _, _, sNow) _ = do
    st <- alexGetUserState
    alexSetStartCode 0
    alexSetUserState Nothing
    case st of
        Just (KChar, pStart, sStart) -> do
            let consumed = length sStart - length sNow
            return $ Error unclosedCharLiteralMsg (makePos pStart consumed)
        Just _ -> return $ Error internalErrorMsg (makePos pNow 0)
        Nothing -> return $ Error internalErrorMsg (makePos pNow 0)


-- | Finish a character literal.
-- Decodes the character or reports an invalid literal.
endChar :: AlexInput -> Int -> Alex Token
endChar (_pEnd, _, _, sEnd) len = do
    st <- alexGetUserState
    alexSetStartCode 0
    alexSetUserState Nothing
    case st of
        Just (KChar, pStart, sStart) -> do
            let consumed = (length sStart - length sEnd) + len
            case unwrapChar (take consumed sStart) of
                Just c -> return $ CharConst c (makePos pStart consumed)
                Nothing -> return $ Error invalidCharLiteralMsg (makePos pStart consumed)
        Just _ -> return $ Error internalErrorMsg (makePos _pEnd 0)
        Nothing -> return $ Error internalErrorMsg (makePos _pEnd 0)


-- string 
alexInitUserState :: AlexUserState
alexInitUserState = Nothing


-- | Enter string literal mode.
-- Stores the starting position and input snapshot.
beginString :: AlexInput -> Int -> Alex Token
beginString (p, _, _, s0) _ = do
    alexSetStartCode string
    alexSetUserState (Just (KString, p, s0))
    alexMonadScan


-- | Handle an unterminated string literal.
-- Reports a lexer error at the opening quote position.
unterminatedString :: AlexInput -> Int -> Alex Token
unterminatedString (pNow, _, _, sNow) _ = do
    st <- alexGetUserState
    alexSetStartCode 0
    alexSetUserState Nothing
    case st of
        Just (KString, pStart, sStart) -> do
            let consumed = length sStart - length sNow
            return $ Error unclosedStrLiteralMsg (makePos pStart consumed)
        Just _ -> return $ Error internalErrorMsg (makePos pNow 0)
        Nothing -> return $ Error internalErrorMsg (makePos pNow 0)


-- | Finish a string literal.
-- Produces a string constant token.
endString :: AlexInput -> Int -> Alex Token
endString (_pEnd, _, _, sEnd) len = do
    st <- alexGetUserState
    alexSetStartCode 0
    alexSetUserState Nothing
    case st of
        Just (KString, pStart, sStart) -> do
            let consumed = (length sStart - length sEnd) + len
            let lexeme = take consumed sStart
            return $ StrConst (unwrapString lexeme) (makePos pStart consumed)
        Just _ ->return $ Error internalErrorMsg (makePos _pEnd 0)
        Nothing -> return $ Error internalErrorMsg (makePos _pEnd 0)


-- | Consume a invalid numeric literal token.
-- The lexeme is kept as-is and interpreted later.
invalidNumber :: AlexInput -> Int -> Alex Token
invalidNumber (p, _, _, s) len = let lexeme = take len s in return $ Error invalidNumericLiteralMsg (makePos p len)


-- | Consume a numeric literal token.
-- The lexeme is kept as-is and interpreted later.
eatNumber :: AlexInput -> Int -> Alex Token
eatNumber (p, _, _, s) len = let lexeme = take len s in return $ NumberConst lexeme (makePos p len)


-- | Consume an identifier token.
-- Matches names consisting of letters, digits, and underscores.
eatIdent :: AlexInput -> Int -> Alex Token
eatIdent (p, _, _, s) len = let lexeme = take len s in return $ Ident lexeme (makePos p len)


-- | Consume a symbol token.
-- The symbol kind is provided explicitly by the lexer rule.
eatSymbol :: LT.Symbol -> AlexInput -> Int -> Alex Token
eatSymbol s (p, _, _, _) len = return $ LT.Symbol s (makePos p len) 


-- | Report a lexical error for an unexpected character.
-- The offending lexeme is included in the error token.
lexError :: AlexInput -> Int -> Alex Token
lexError (p, _, _, s) len = let bad = take len s in return $ Error bad (makePos p len)


-- | Check whether a token represents the EOF sentinel.
-- Used internally to terminate token collection.
isEOF :: Token -> Bool
isEOF (Error "<EOF>" _) = True
isEOF _ = False


-- | Scan the entire input into a list of tokens.
-- Returns either a lexer failure or all produced tokens.
alexScanTokens :: String -> Either String [Token]
alexScanTokens input = runAlex input collectTokens

-- | Internal loop that repeatedly scans tokens.
-- Stops when the EOF token is encountered.
collectTokens :: Alex [Token]
collectTokens = loop []
  where
    loop acc = do
      tok <- alexMonadScan
      if isEOF tok
        then return (reverse acc)
        else loop (tok : acc)


-- | Convert an error token into a structured 'ErrorKind'.
-- Assumes the token is an 'Error'.
convertErrToken :: Path -> Token -> ErrorKind
convertErrToken p (Error why pos) = UE.Lexer $ UE.makeError p pos why
convertErrToken _ _ = error "what the hell is going on???"


-- | Tokenize an input string and collect lexer errors.
-- Returns both errors and successfully parsed tokens.
tokenize :: Path -> String -> ([ErrorKind], [Token])
tokenize p str =
  case alexScanTokens str of
    Left _ -> ([UE.Unkown p], [])
    Right tokens ->
        let (errs, correct) = partition isErrToken tokens
            errs' = filter (not . isEOF) errs
        in (map (convertErrToken p) errs', correct)


-- | used for debug
debugTokenize :: String -> ([ErrorKind], [Token])
debugTokenize = tokenize "stdin"



fileTokenize :: Path -> IO ([ErrorKind], [Token])
fileTokenize p = do
    result <- FileHelper.readFile p

    case result of
        Left e -> return ([e], [])
        Right code -> return $ tokenize p code
}
