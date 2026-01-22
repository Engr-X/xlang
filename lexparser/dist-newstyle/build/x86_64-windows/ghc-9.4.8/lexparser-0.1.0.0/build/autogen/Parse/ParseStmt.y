{
module Parse.ParseStmt where
import Unsafe.Coerce (unsafeCoerce)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Lex.Token (Token)
import Parse.SyntaxTree
import Parse.ParserBasic
import Util.Exception (expectedExpression)
import qualified Lex.Token as Lex
}
%name parseStmt StmtEOF
%tokentype { Token }
%error { parseStmtError }
%token
    -- keywords (Ident)
    KW_TRUE { Lex.Ident "true" _ }
    KW_FALSE { Lex.Ident "false" _ }
    KW_AS { Lex.Ident "as" _ }
    -- literals / identifiers
    identity { Lex.Ident $$ _ }
    number { Lex.NumberConst $$ _ }
    character { Lex.CharConst $$ _ }
    string { Lex.StrConst $$ _ }
    -- newline / eof
    NL { Lex.NewLine _ }
    EOF { Lex.EOF _ }
    -- symbols (Expr)
    '.' { Lex.Symbol Lex.Dot _ }
    '+' { Lex.Symbol Lex.Plus _ }
    '-' { Lex.Symbol Lex.Minus _ }
    '*' { Lex.Symbol Lex.Multiply _ }
    '/' { Lex.Symbol Lex.Divide _ }
    '(' { Lex.Symbol Lex.LParen _ }
    ')' { Lex.Symbol Lex.RParen _ }
    -- symbols (Stmt/Block)
    '{' { Lex.Symbol Lex.LBrace _ }
    '}' { Lex.Symbol Lex.RBrace _ }
    ';' { Lex.Symbol Lex.Semicolon _ }
%left '+' '-'
%left '*' '/'
%right UPLUS UMINUS
%%
-- entry
StmtEOF :: { Statement }
  : OptTerms Stmt OptTerms EOF { $2 }
-- separators
Term :: { () }
  : ';' { () }
  | NL { () }
OptTerms :: { () }
    : {- empty -} { () }
    | OptTerms Term { () }
-- minimal statements
Stmt :: { Statement }
    : ExprStmt { Expr $1 }
    | Block { BlockStmt $1 }
    ;
ExprStmt :: { Expression }
    : Expr Term { $1 }
    ;
Block :: { Block }
    : '{' BlockBody '}' { Multiple $2 }
    ;
BlockBody :: { [Statement] }
    : {- empty -} { [] }
    | BlockBody Term { $1 }
    | BlockBody Stmt { $1 ++ [$2] }
    ;
-- shared expression grammar
-- ============================================================
-- Expression grammar (shared by Expr / Stmt / Block / Program)
-- ============================================================
QName
    : identity { [$1] }
    | QName '.' identity { $1 ++ [$3] }
    ;
Type
    : QName { toClass (intercalate "." $1) }
    ;
Atom
    : KW_TRUE { BoolConst True }
    | KW_FALSE { BoolConst False }
    -- unary operators
    | '+' Atom %prec UPLUS { Unary UnaryPlus $2 }
    | '+' error %prec UPLUS { Error $1 (expectedExpression 1 "+") }
    | '-' Atom %prec UMINUS { Unary UnaryMinus $2 }
    | '-' error %prec UMINUS { Error $1 (expectedExpression 1 "-") }
    -- parenthesized expression
    | '(' Expr ')' { $2 }
    -- identifiers
    | QName { Qualified $1 }
    | identity { Variable $1 }
    -- literals
    | number { fromMaybe (error "classifyNumber failed")
                                      (classifyNumber $1) }
    | character { CharConst $1 }
    | string { StringConst $1 }
    ;
Postfix
    : Atom { $1 }
    -- cast
    | Postfix KW_AS Type { Cast $3 $1 }
    | Postfix KW_AS error { Error $2 (expectedExpression 0 "as") }
    ;
Expr
    : Expr '+' Expr { Binary Add $1 $3 }
    | Expr '+' error { Error $2 (expectedExpression 1 "+") }
    | Expr '-' Expr { Binary Sub $1 $3 }
    | Expr '-' error { Error $2 (expectedExpression 1 "-") }
    | Expr '*' Expr { Binary Mul $1 $3 }
    | Expr '*' error { Error $2 (expectedExpression 1 "*") }
    | Expr '/' Expr { Binary Div $1 $3 }
    | Expr '/' error { Error $2 (expectedExpression 1 "/") }
    | Postfix { $1 }
    ;
{
parseStmtError :: [Token] -> a
parseStmtError toks = unsafeCoerce (Expr (mkHappyErrorExpr toks))
}
