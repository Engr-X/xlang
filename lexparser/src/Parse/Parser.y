{
module Parse.Parser where

import Data.List (find, intercalate)
import Data.Maybe (fromMaybe, listToMaybe)
import Lex.Token (Token, tokenPos, isLBracketToken, isRBracketToken, isBracketToken)
import Parse.SyntaxTree
import Util.Exception (ErrorKind, mismatchedBracket, makeError, expectedExpression)
import Util.Basic (isInt, isLong, isFloat, isDouble, isLongDouble, isBoolTrue, isBoolFalse)
import Util.Type

import qualified Lex.Token as Lex
import qualified Util.Exception as UE
}

%name parseExpr Expr
%tokentype { Token }
%error { happyError }

%token
    identity            { Lex.Ident $$ _ }
    number              { Lex.NumberConst $$ _ }
    character           { Lex.CharConst $$ _ }
    string              { Lex.StrConst $$ _ }

    KW_AS               { Lex.Ident "as" _ }   -- English: keyword 'as'


    --bitLShiftAssign     {}  
    '.'                 { Lex.Symbol Lex.Dot _ }
    '+'                 { Lex.Symbol Lex.Plus _ }
    '-'                 { Lex.Symbol Lex.Minus _ }
    '*'                 { Lex.Symbol Lex.Multiply _ }
    '/'                 { Lex.Symbol Lex.Divide _ }
    '('                 { Lex.Symbol Lex.LParen _ }
    ')'                 { Lex.Symbol Lex.RParen _ }

%left '='
%left '+' '-'
%left '*' '/'
%right UPLUS UMINUS

%%

-- like com.wangdi.util
QName
    : identity               { [$1] }
    | QName '.' identity     { $1 ++ [$3] }
    ;

Type
    : QName { toClass (intercalate "." $1) }
    ;

Atom
    : '+' Atom %prec UPLUS   { Unary UnaryPlus  $2 }
    | '+' error %prec UPLUS  { Error $1 (expectedExpression 1 "+") }

    | '-' Atom %prec UMINUS  { Unary UnaryMinus $2 }
    | '-' error %prec UMINUS { Error $1 (expectedExpression 1 "-") }

    | '(' Expr ')'           { $2 }

    | QName                   { Qualified $1 }
    | identity                { classifyId $1 }

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

{
happyError :: [Token] -> a
happyError ts = error $ "Parse error near: " ++
    case ts of
        (t:_) -> show t
        [] -> "EOF"


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


classifyNumber :: String -> Maybe Expression
classifyNumber s = let types = [(IntConst, isInt), (LongConst, isLong), (FloatConst, isFloat), (DoubleConst, isDouble), (LongDoubleConst, isLongDouble)] in
    fmap (\(f, _) -> f s) $ find (\(_, f) -> f s) types


classifyId :: String -> Expression
classifyId str
    | isBoolTrue str = BoolConst True
    | isBoolFalse str = BoolConst False
    | otherwise = Variable str


parse :: Path -> [Token] -> Either [ErrorKind] Program
parse p ts = case checkBracket ts of
    Just t -> Left [UE.Syntax $ makeError p (tokenPos t) mismatchedBracket]
}
