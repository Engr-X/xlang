{
module Parse.Parser where

import System.IO.Unsafe (unsafePerformIO)
import Data.List (find, intercalate)
import Data.Maybe (fromMaybe, listToMaybe)
import Lex.NewLine
import Lex.Token (Token, tokenPos, isLBracketToken, isRBracketToken, isBracketToken)
import Lex.Tokenizer (tokenize)
import Parse.SyntaxTree
import Util.Exception (ErrorKind, mismatchedBracket, makeError, expectedExpression)
import Util.Basic (isInt, isLong, isFloat, isDouble, isLongDouble)
import Util.Type

import qualified Lex.Token as Lex
import qualified Control.Exception as CE
import qualified Util.Exception as UE
}

%name parseExpr Expr
%name parseStmt Stmt
%name parseBlock Block
%name parseProg Program
%tokentype { Token }
%error { happyError }

%token
    KW_TRUE             { Lex.Ident "true" _ }
    KW_FALSE            { Lex.Ident "false" _ }

    KW_AS               { Lex.Ident "as" _ }
    KW_IF               { Lex.Ident "if" _ }
    KW_ELSE             { Lex.Ident "else" _ }

    NL                  { Lex.NewLine _ }   -- example: adjust to your actual NL token constructor
    identity            { Lex.Ident $$ _ }
    number              { Lex.NumberConst $$ _ }
    character           { Lex.CharConst $$ _ }
    string              { Lex.StrConst $$ _ }

    --operation symbols
    '.'                 { Lex.Symbol Lex.Dot _ }
    '+'                 { Lex.Symbol Lex.Plus _ }
    '-'                 { Lex.Symbol Lex.Minus _ }
    '*'                 { Lex.Symbol Lex.Multiply _ }
    '/'                 { Lex.Symbol Lex.Divide _ }

    '('                 { Lex.Symbol Lex.LParen _ }
    ')'                 { Lex.Symbol Lex.RParen _ }
    '['                 { Lex.Symbol Lex.LBracket _ }
    ']'                 { Lex.Symbol Lex.RBracket _ }
    '{'                 { Lex.Symbol Lex.LBrace _ }
    '}'                 { Lex.Symbol Lex.RBrace _ }
    ';'                 { Lex.Symbol Lex.Semicolon _ }

%left '='
%left '+' '-'
%left '*' '/'
%right UPLUS UMINUS

%%

Sep
    : ';' { () }
    | NL  { () }
    ;

OptSep
    : {- empty -} { () }       -- eat nothing
    | OptSep Sep  { () }       -- eat 1+ separators
    ;

--  for expression

QName
    : identity               { [$1] }
    | QName '.' identity     { $1 ++ [$3] }
    ;

Type
    : QName { toClass (intercalate "." $1) }
    ;

Atom
    : KW_TRUE   { BoolConst True }
    | KW_FALSE  { BoolConst False }

    | '+' Atom %prec UPLUS   { Unary UnaryPlus  $2 }
    | '+' error %prec UPLUS  { Error $1 (expectedExpression 1 "+") }

    | '-' Atom %prec UMINUS  { Unary UnaryMinus $2 }
    | '-' error %prec UMINUS { Error $1 (expectedExpression 1 "-") }

    | '(' Expr ')'           { $2 }

    | QName                   { Qualified $1 }
    | identity                { Variable $1 }

    | number                  { fromMaybe (error "classifyNumber failed") (classifyNumber $1) }
    | character               { CharConst $1 }
    | string                  { StringConst $1 }
    ;

Postfix
    : Atom                  { $1 }
    
    | Postfix KW_AS Type    { Cast $3 $1 }
    | Postfix KW_AS error   { Error $2 (expectedExpression 0 "as") }
    ;

Expr
    : Expr '+' Expr   { Binary Add $1 $3 }
    | Expr '+' error  { Error $2 (expectedExpression 1 "+") }

    | Expr '-' Expr   { Binary Sub $1 $3 }
    | Expr '-' error  { Error $2 (expectedExpression 1 "-") }

    | Expr '*' Expr   { Binary Mul $1 $3 }
    | Expr '*' error  { Error $2 (expectedExpression 1 "*") }

    | Expr '/' Expr   { Binary Div $1 $3 }
    | Expr '/' error  { Error $2 (expectedExpression 1 "/") }

    | Postfix         { $1 }
    ;

-- for statement

Stmt
    : Expr                          { Expr $1 }
    | Block                         { BlockStmt $1 }
    ;

StmtList
    : Stmt                           { [$1] }
    | StmtList Sep Stmt              { $1 ++ [$3] }
    ;


-- for block

Block
    : '{' OptSep StmtList OptSep '}'   { Multiple $3 }
    | '{' OptSep OptSep '}'            { Multiple [] }
    ;

-- for program

Program
    : OptSep StmtList OptSep   { ([], $2) }
    | OptSep OptSep            { ([], []) }
    ;


-- statement
{
-- | Report a parse error with the token nearest to the failure point.
--   If no tokens remain, the error is reported at EOF.
happyError :: [Token] -> a
happyError ts = error $ "Parse error near: " ++
    case ts of
        (t:_) -> show t
        [] -> "EOF"


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


-- | Parse a token stream into a program after performing bracket validation.
--   Reports a syntax error if mismatched brackets are detected.
parse :: Path -> [Token] -> Either [ErrorKind] Program
parse p ts = case checkBracket ts of
    Just t -> Left [UE.Syntax $ makeError p (tokenPos t) mismatchedBracket]
    Nothing -> let res = parseProg ts
                   errs = getErrorProgram res in case errs of
                        [] -> Right res
                        es -> Left $ map (toException p) es

    where
        toException :: Path -> Expression -> ErrorKind
        toException p (Error t why) = UE.Syntax $ UE.makeError p (tokenPos t) why
        toException _ _ = error "Is is really an error bro?"


-- combine two step: tokenizer and parser
lexparse :: Path -> String -> Either [ErrorKind] Program
lexparse p code = let (errs, tokens) = tokenize p code in case errs of
    [] -> parse p $ insertNewLine tokens
    _ -> Left errs

debugLexparse :: String -> Either [ErrorKind] Program
debugLexparse = lexparse "debug"
}
