module Lexer.Tokenizer where

import Text.Regex.TDFA ((=~))

import Control.Monad.State.Strict (State, get, put, evalState)
import Data.Map.Strict (Map) 
import Numeric (readHex, readOct)

import qualified Data.Char as DC
import qualified Data.List as DL
import qualified Data.Trie as DT
import qualified Data.Map.Strict as Map

import qualified Util.FileHelper as UFH 
import qualified Util.Exception as UE
import qualified Util.Types as UTypes
import qualified Util.Basic as UB


-- | Symbol tokens for operators and delimiters in the language.
--
-- Each constructor corresponds to a string in 'symbols'.
data Symbol
    -- Assignment operators
    = Assign           -- "="
    | BitLShiftAssign  -- "<<="
    | BitRShiftAssign  -- ">>="
    | BitOrAssign      -- "|="
    | BitXorAssign     -- "^="
    | BitXnorAssign    -- "!^="
    | PlusAssign       -- "+="
    | MinusAssign      -- "-="
    | MultiplyAssign   -- "*="
    | DivideAssign     -- "/="
    | ModuloAssign     -- "%="
    | PowerAssign      -- "**="

    -- Equality / comparison
    | Equal            -- "=="
    | NotEqual         -- "!="
    | GreaterThan      -- ">"
    | LessThan         -- "<"
    | GreaterEqual     -- ">="
    | LessEqual        -- "<="

    -- Bitwise operators
    | BitRShift        -- ">>"
    | BitLShift        -- "<<"
    | BitOr            -- "|"
    | BitXor           -- "^"
    | BitXnor          -- "!^"
    | BitAnd           -- "&"
    | BitReverse       -- "!"

    -- Arithmetic operators
    | Plus             -- "+"
    | Minus            -- "-"
    | Multiply         -- "*"
    | Divide           -- "/"
    | Modulo           -- "%"
    | Power            -- "**"

    -- Increment / decrement
    | PlusPlus         -- "++"
    | MinusMinus       -- "--"

    -- Other symbols
    | At               -- "@"
    | Dollar           -- "$"  -- 注意这里 "%" 出现在 symbols 表里两次，你可能需要确认用途
    | LParen           -- "("
    | RParen           -- ")"
    | LBracket         -- "["
    | RBracket         -- "]"
    | LBrace           -- "{"
    | RBrace           -- "}"

    -- Functional / miscellaneous symbols
    | DoubleQuote      -- "\""
    | SingleQuote      -- "'"
    | Semicolon        -- ";"
    | Question         -- "?"
    | Colon            -- ":"
    | QuestionArrow    -- "?->"
    | Dot              -- "."
    | DoubleDot        -- ".."
    | Backslash        -- "\\"

    -- Comments / preprocessor
    | LineComment      -- "//"
    | Hash             -- "#"
    | BlockCommentStart -- "/*"
    | BlockCommentEnd   -- "*/"
    deriving (Eq, Show, Ord)


-- | A list of all symbols in the language along with their precedence and internal representation.
-- Each tuple contains: (symbol string, precedence level, corresponding Symbol constructor)
symbols :: [(String, Int, Symbol)]
symbols = [
    ("=", 0, Assign), ("<<=", 0, BitLShiftAssign), (">>=", 0, BitRShiftAssign), ("|=", 0, BitOrAssign), ("^=", 0, BitXorAssign),
    ("!^=", 0, BitXnorAssign), ("+=", 0, PlusAssign), ("-=", 0, MinusAssign), ("*=", 0, MultiplyAssign), ("/=", 0, DivideAssign),
    ("%=", 0, ModuloAssign), ("**=", 0, PowerAssign),

    ("==", 1, Equal), ("!=", 1, NotEqual),
    (">", 2, GreaterThan), ("<", 2, LessThan), (">=", 2, GreaterEqual), ("<=", 2, LessEqual),
    (">>", 3, BitRShift), ("<<", 3, BitLShift),
    ("|", 4, BitOr),
    ("^", 5, BitXor), ("!^", 5, BitXnor),
    ("&", 6, BitAnd),
    ("+", -1, Plus), ("-", -1, Minus),
    ("*", 8, Multiply), ("/", 8, Divide), ("%", 8, Modulo),
    ("**", 9, Power),
    ("!", 10, BitReverse),
    ("++", -1, PlusPlus), ("--", -1, MinusMinus),
    ("@", 13, At), ("$", 13, Dollar),
    ("(", 14, LParen), (")", 14, RParen), ("[", 14, LBracket), ("]", 14, RBracket), ("{", 14, LBrace),  ("}", 14, RBrace),

    -- functional symbols
    ("\"", -2, DoubleQuote), ("'", -2, SingleQuote), (";", -2, Semicolon),
    ("?", -2, Question), (":", -2, Colon), ("?->", -2, QuestionArrow), (".", -2, Dot), ("..", -2, DoubleDot), ("\\", -2, Backslash),
    ("//", -3, LineComment), ("#", -3, Hash), ("/*", -3, BlockCommentStart), ("*/", -3, BlockCommentEnd)]


-- | A map from symbol strings to their corresponding Symbol constructors for quick lookup.
symbolsMap :: Map String Symbol
symbolsMap = Map.fromList (map (\(a, _, b) -> (a, b)) symbols)


-- | A trie structure containing all symbol strings for efficient prefix-based searching.
symbolsTree :: DT.TrieTree Char
symbolsTree = DT.build $ map (\(a, _, _) -> a) symbols


-- | The 'Token' data type represents all possible tokens that can appear
-- in the source code after lexical analysis.
-- Each token carries a position from the source code for error reporting.
data Token = End
    | CharConst Char UTypes.Position -- contain ''
    | StrConst String UTypes.Position -- contain ""
    | NumberConst String UTypes.Position

    | Ident String UTypes.Position
    | Symbol Symbol UTypes.Position
    deriving (Eq)


-- | 'Show' instance for 'Token' provides a human-readable representation
-- of each token, including its value (if applicable) and its source position.
instance Show Token where
    show End = "End"
    show (CharConst c pos) = "Char@'" ++ [c] ++ "' " ++ show pos
    show (StrConst s pos) = "String@\"" ++ s ++ "\"  " ++ show pos
    show (NumberConst s pos) = "Number@" ++ s ++ " " ++ show pos
    show (Ident name pos) = "Ident@" ++ name ++ " " ++ show pos
    show (Symbol sym pos) = "Symbol@" ++ show sym ++ " " ++ show pos


-- | Extracts the source position of a token.
-- Throws an error if called on the 'End' token, which has no position.
getTokenPos :: Token -> UTypes.Position
getTokenPos tok = case tok of
    End -> error "End token has no position"
    CharConst _ pos -> pos
    StrConst _ pos -> pos
    NumberConst _ pos -> pos
    Ident _ pos -> pos
    Symbol _ pos -> pos


-- | Returns the matching closing bracket for a given opening bracket.
-- Returns 'Nothing' if the character is not an opening bracket.
matchBracket :: Char -> Maybe Char
matchBracket '(' = Just ')'
matchBracket '[' = Just ']'
matchBracket '{' = Just '}'
matchBracket _  = Nothing


-- | Represents the current state when processing comments and string/char literals.
data CommentState = Normal | InString | InChar | InBlockComment


-- | Removes all block comments (/* ... */) from a string.
-- Preserves newlines to maintain line numbers for error reporting.
-- Also correctly handles string literals and character literals, leaving them intact.
eatBlockComments :: String -> String
eatBlockComments = go Normal
  where
    go _ [] = []
    go Normal ('"':cs) = '"' : go InString cs
    go Normal ('\'':cs) = '\'' : go InChar cs
    go Normal ('/':'*':cs) = ' ' : ' ' : go InBlockComment cs
    go Normal (c:cs) = c : go Normal cs

    go InString ('\\':c:cs) = '\\' : c : go InString cs
    go InString ('"':cs) = '"' : go Normal cs
    go InString (c:cs) = c : go InString cs

    go InChar ('\\':c:cs) = '\\' : c : go InChar cs
    go InChar ('\'':cs) = '\'' : go Normal cs
    go InChar (c:cs) = c : go InChar cs

    go InBlockComment ('*':'/':cs) = ' ' : ' ' : go Normal cs
    go InBlockComment ('\n':cs) = '\n' : go InBlockComment cs
    go InBlockComment (_:cs) = ' ' : go InBlockComment cs


-- | Consumes leading whitespace from a string.
-- Tabs are counted as 4 spaces.
-- Returns a tuple of (number of spaces consumed, remaining string).
eatSpace :: String -> (Int, String)
eatSpace [] = (0, [])
eatSpace str@(x:xs)
    | x == '\t' = let (n, rest) = eatSpace xs in (n + 4, rest)
    | DC.isSpace x = let (n, rest) = eatSpace xs in (n + 1, rest)
    | otherwise = (0, str)


-- | Consumes the longest matching symbol from the beginning of a string.
-- Uses 'symbolsTree' for efficient prefix-based symbol matching.
-- Returns a tuple of (matched symbol, remaining string).
eatSymbol :: String -> (String, String)
eatSymbol = DT.eatLongest symbolsTree


-- | Consumes characters from the beginning of a string as long as they are valid identifier characters.
-- Returns a tuple of (matched identifier, remaining string).
eatIdentity :: String -> (String, String)
eatIdentity = span UB.isIdentChar


-- | Consumes the portion of a string matching a given regular expression.
-- Returns a tuple of (matched substring, remaining string).
eatByPattern :: UTypes.Regex -> String -> (String, String)
eatByPattern pat s = let (bef, mat, aft) = s =~ pat :: (String, String, String) in (mat, bef ++ aft)


-- | Consumes a numeric literal from the beginning of a string.
-- Returns a tuple of (number string, remaining string).
-- If no number is matched, returns ("", original string).
eatNumber :: String -> (String, String)
eatNumber s = case eatByPattern (UB.start UB.numberPattern) s of
    ("", _) -> ("", s)
    result -> result


-- | Eat string literal in code
-- first must be ", otherwise Nothing will be returned
eatStringLiteral :: String -> Maybe (String, String)
eatStringLiteral s =
    let (matched, rest) = eatByPattern (UB.start UB.stringLiteralPattern) s
    in if null matched
        then Nothing
        else Just (matched, rest)


-- | Attempts to consume a character literal from the beginning of a string.
-- Returns 'Nothing' if no valid character literal is found,
-- otherwise returns 'Just (matched literal string, remaining string)'.
eatCharLiteral :: String -> Maybe (String, String)
eatCharLiteral s =
    let (matched, rest) = eatByPattern (UB.start UB.charLiteralPattern) s
    in if null matched
        then Nothing
        else Just (matched, rest)


-- | Represents the state of the tokenizer (lexer) at any point.
-- Keeps track of the source file path, current position, and remaining lines of code.
data TokenizerState = TokenizerState {
    path :: UTypes.Path,
    position :: UTypes.Position,
    code :: [String]}


-- | Constructs a new 'TokenizerState' given a file path, position, and list of code lines.
makeState :: UTypes.Path -> UTypes.Position -> [String] -> TokenizerState
makeState p pos ls = TokenizerState {path = p, position = pos, code = ls}


-- | Removes the surrounding quotes from a string literal.
-- | Example: "\"hello\"" -> "hello"
unwrapString :: String -> String 
unwrapString = init . tail


-- | Converts a string representing a character literal into the actual character.
-- Handles escaped characters like '\n', '\t', '\xAB', '\u1234', etc.
unwrapChar :: String -> Char
unwrapChar s = case unwrapString s of
    [c] -> c
    '\\':rest -> parseEscape rest
    _ -> error "unreachable"
    where 
      readHexInt :: String -> Int
      readHexInt str = case readHex str of
        [(n, "")] -> n
        _ -> error "invalid hex"

      readOctInt :: String -> Int
      readOctInt str = case readOct str of
        [(n, "")] -> n
        _ -> error "invalid oct"

      parseEscape :: String -> Char
      parseEscape str = case str of
        "n"  -> '\n'
        "t"  -> '\t'
        "r"  -> '\r'
        "b"  -> '\b'
        "f"  -> '\f'
        "v"  -> '\v'
        "\\" -> '\\'
        "'"  -> '\''
        "\"" -> '\"'

        'x':hex -> DC.chr $ readHexInt hex
        'u':hex -> DC.chr $ readHexInt hex
        'U':hex -> DC.chr $ readHexInt hex
        oct -> DC.chr $ readOctInt oct


-- | Looks up a string in the symbol map and returns the corresponding 'Symbol' if it exists.
parseSymbol :: String -> Maybe Symbol
parseSymbol = flip Map.lookup symbolsMap


-- | Lexical analyzer (tokenizer) functions for source code.
-- Converts raw source code into a list of 'Token's, handling identifiers,
-- string and character literals, numbers, symbols, comments, and whitespace.

-- Consumes the next token from the tokenizer state.
-- Returns either a 'Token' or a syntax error wrapped in 'Either'.
-- Handles:
-- - Identifiers
-- - String literals
-- - Character literals
-- - Whitespace (skipped)
-- - Single-line comments ("//", "#")
-- - Symbols (operators, punctuation)
-- - Errors for invalid characters, unclosed comments, or invalid literals.
eat :: State TokenizerState (Either UE.ErrorKind Token)
eat = let comment = ["//", "#"] in do
    state <- get
    let filePath = path state
    let (ln, col, _) = UTypes.positionToTuple (position state)
    let strs = code state
    
    if null strs then return $ Right End
    else let
        line = head strs
        restLines = tail strs
        lineLength = length line
        in case line of
            [] -> do
                put $ makeState filePath (UTypes.makePosition (succ ln) 1 0) restLines
                eat
            (c:_) 
                -- eat number
               | let (number, remain) = eatNumber line, not $ null number -> do
                    let numberLength = length number
                    put $ makeState filePath (UTypes.makePosition ln (col + numberLength) 0) (remain : restLines)
                    return $ Right $ NumberConst number (UTypes.makePosition ln col numberLength)

                -- eat identifier
                | UB.isIdentChar c -> do
                    let (ident, remain) = eatIdentity line
                    let idLength = length ident
                    put $ makeState filePath (UTypes.makePosition ln (col + idLength) 0) (remain : restLines)
                    return $ Right $ Ident ident (UTypes.makePosition ln col idLength)

                -- eat String literal
                | c == '\"' -> let result = eatStringLiteral line in
                    case result of
                        Just (strLit, remain) -> do
                            let litLength = length strLit
                            put $ makeState filePath (UTypes.makePosition ln (col + litLength) 0) (remain : restLines)
                            return $ Right $ StrConst  (unwrapString strLit) (UTypes.makePosition ln col litLength)

                        Nothing -> return $ Left $ UE.SyntaxError $ UE.makeError filePath (UTypes.makePosition ln col lineLength) UE.invalidStringLiteralMsg

                -- eat Char literal
                | c == '\'' -> let result = eatCharLiteral line in
                    case result of
                        Just (charLit, remain) -> do
                            let litLength = length charLit
                            put $ makeState filePath (UTypes.makePosition ln (col + litLength) 0) (remain : restLines)
                            return $ Right $ CharConst (unwrapChar charLit) (UTypes.makePosition ln col litLength)

                        Nothing -> return $ Left $ UE.LexerError $ UE.makeError filePath (UTypes.makePosition ln col 3) UE.invalidCharLiteralMsg

                -- eat space
                | DC.isSpace c -> let (emptySize, remain) = eatSpace line in do
                    put $ makeState filePath (UTypes.makePosition ln (col + emptySize) 0) (remain : restLines)
                    eat 

                -- eat symbols
                | DT.hasPrefix symbolsTree [c] -> let (symbolStr, remain) = eatSymbol line in case symbolStr of
                    x | x `elem` comment -> do
                        put $ makeState filePath (UTypes.makePosition (succ ln) 1 0) restLines
                        eat
                    "/*" -> return $ Left $ UE.LexerError $ UE.makeError filePath (UTypes.makePosition ln col 2) UE.unclosedCommentMsg
                    "*/" -> return $ Left $ UE.LexerError $ UE.makeError filePath (UTypes.makePosition ln col 2) UE.unclosedCommentMsg
                    _ -> do
                        let symbolLength = length symbolStr
                        put $ makeState filePath (UTypes.makePosition ln (col + symbolLength) 0) (remain : restLines)
                        let foundSymbol = parseSymbol symbolStr

                        case foundSymbol of
                            Just symbol -> return $ Right $ Symbol symbol (UTypes.makePosition ln col symbolLength)
                            Nothing -> return $ Left $ UE.LexerError $ UE.makeError filePath (UTypes.makePosition ln col symbolLength) UE.invalidSymbolMsg

                | otherwise -> return $ Left $ UE.LexerError $ UE.makeError filePath (UTypes.makePosition ln col 1) UE.invalidCharMsg


-- | Recursively consumes all tokens from the tokenizer state until 'End' is reached.
-- Returns either a list of tokens or a syntax error.
eatAll :: State TokenizerState (Either UE.ErrorKind [Token])
eatAll = do 
    result <- eat
    case result of
        Right End  -> return (Right [End])
        Right tok  -> fmap (tok :) <$> eatAll
        Left err   -> return (Left err)



-- | Tokenizes a list of source lines from a given file path.
-- Returns either a list of 'Token's or a syntax error.
tokenize :: UTypes.Path -> [String] -> Either UE.ErrorKind [Token]
tokenize filePath xs = evalState eatAll (makeState filePath (UTypes.makePosition 1 1 0) xs)


-- | Tokenizes raw source code string, removing block comments first.
-- Processes the content using 'eatBlockComments' and any other preprocessing.
codeToTokens :: UTypes.Path -> String -> Either UE.ErrorKind [Token]
codeToTokens p = tokenize p . UFH.processContent . eatBlockComments 


-- | Pretty-prints a list of tokens for debugging or display.
-- Groups tokens by line and removes the 'End' token for clarity.
prettyTokens :: Either UE.ErrorKind [Token] -> String
prettyTokens (Left err) = show err
prettyTokens (Right ts) =
    let clean = filter (/= End) ts
        splited = DL.groupBy (\a b -> UTypes.line (getTokenPos a) == UTypes.line (getTokenPos b)) clean
    in DL.intercalate "\n" (map show splited)

-- | Debug
-- Debug function: reads a file, tokenizes it, and prints the tokens line by line.
-- Displays syntax errors if tokenization fails.
fileTokens :: UTypes.Path -> IO ()
fileTokens p = do
    rawContent <- UFH.readFile p
    case rawContent of
        Right content -> putStrLn $ prettyTokens $ codeToTokens p content
        Left err -> print err
