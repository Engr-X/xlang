{
module Parse.ParseBlock where
import Unsafe.Coerce (unsafeCoerce)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Lex.Token (Token)
import Parse.SyntaxTree
import Parse.ParserBasic
import Util.Exception (expectedExpression)
import qualified Lex.Token as Lex
}
%name parseBlock BlockEOF
%tokentype { Token }
%error { parseBlockError }
%token
    -- keywords
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
    -- expression symbols
    '.' { Lex.Symbol Lex.Dot _ }
    '+' { Lex.Symbol Lex.Plus _ }
    '-' { Lex.Symbol Lex.Minus _ }
    '*' { Lex.Symbol Lex.Multiply _ }
    '/' { Lex.Symbol Lex.Divide _ }
    '(' { Lex.Symbol Lex.LParen _ }
    ')' { Lex.Symbol Lex.RParen _ }
    -- statement / block symbols
    '{' { Lex.Symbol Lex.LBrace _ }
    '}' { Lex.Symbol Lex.RBrace _ }
    ';' { Lex.Symbol Lex.Semicolon _ }
%left '+' '-'
%left '*' '/'
%right UPLUS UMINUS
%%
BlockEOF :: { Block } : OptTerms Block OptTerms EOF { $2 }
Term :: { () }
    : ';' { () }
    | NL { () }
    ;
OptTerms :: { () }
    : {- empty -} { () }
    | OptTerms Term { () }
    ;
Terms1 :: { () }
    : Term OptTerms { () }
    ;
Block :: { Block }
    : '{' OptTerms StmtSeqOpt OptTerms '}' { Multiple $3 }
    ;
StmtSeqOpt :: { [Statement] }
    : {- empty -} { [] }
    | StmtSeq { $1 }
    ;
StmtTerm :: { Statement }
    : Stmt Terms1 { $1 }
    ;
StmtSeq :: { [Statement] }
    : StmtTerm { [$1] }
    | StmtTerm StmtSeq { $1 : $2 }
    ;
Stmt :: { Statement }
    : Expr { Expr $1 }
    | Block { BlockStmt $1 }
    ;
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
    | QName { qnameToExpr $1 }
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
-- Happy 2.0 expects a polymorphic error handler.
-- We return a sentinel block containing a single error expression statement.
parseBlockError :: [Token] -> a
parseBlockError toks =
    let e = mkHappyErrorExpr toks
        bl = Multiple [Expr e] in unsafeCoerce bl
}
