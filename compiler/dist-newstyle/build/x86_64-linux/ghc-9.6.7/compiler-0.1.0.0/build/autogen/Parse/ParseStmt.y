{
module Parse.ParseStmt where
import Unsafe.Coerce (unsafeCoerce)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Lex.Token (Token, tokenPos)
import Lex.Tokenizer (tokenizeWithNL)
import Parse.ParserBasic
import Parse.SyntaxTree
import Parse.ParserBasic
import Util.Exception (ErrorKind, makeError, expectedExpression, assignErrorMsg)
import Util.Type (Path, defaultPosition)
import qualified Lex.Token as Lex
import qualified Util.Exception as UE
}
%name parseStmt StmtEOF
%tokentype { Token }
%error { parseStmtError }
%token
    EOF { t@(Lex.EOF _) }
    NL { t@(Lex.TokenPass _) }
    KW_TRUE { t@(Lex.Ident "true" _) }
    KW_FALSE { t@(Lex.Ident "false" _) }
    KW_AS { t@(Lex.Ident "as" _) }
    KW_IF { t@(Lex.Ident "if" _) }
    KW_ELSE { t@(Lex.Ident "else" _) }
    KW_WHILE { t@(Lex.Ident "while" _) }
    KW_BREAK { t@(Lex.Ident "break" _) }
    KW_CONTINUE { t@(Lex.Ident "continue" _) }
    KW_RETURN { t@(Lex.Ident "return" _) }
    --KW_FUNC { t@(Lex.Ident "fun" _) }
    -- literals / identifiers
    identity { t@(Lex.Ident _ _) }
    number { t@(Lex.NumberConst _ _) }
    character { t@(Lex.CharConst _ _) }
    string { t@(Lex.StrConst _ _) }
    '=' { t@(Lex.Symbol Lex.Assign _) }
    '>' { t@(Lex.Symbol Lex.GreaterThan _) }
    '<' { t@(Lex.Symbol Lex.LessThan _) }
    '==' { t@(Lex.Symbol Lex.Equal _) }
    '!=' { t@(Lex.Symbol Lex.NotEqual _) }
    '!' { t@(Lex.Symbol Lex.BitNot _) }
    '+' { t@(Lex.Symbol Lex.Plus _) }
    '-' { t@(Lex.Symbol Lex.Minus _) }
    '*' { t@(Lex.Symbol Lex.Multiply _) }
    '/' { t@(Lex.Symbol Lex.Divide _) }
    '(' { t@(Lex.Symbol Lex.LParen _) }
    ')' { t@(Lex.Symbol Lex.RParen _) }
    '{' { t@(Lex.Symbol Lex.LBrace _) }
    '}' { t@(Lex.Symbol Lex.RBrace _) }
    ';' { t@(Lex.Symbol Lex.Semicolon _) }
    ',' { t@(Lex.Symbol Lex.Comma _) }
    ':' { t@(Lex.Symbol Lex.Colon _) }
    '::' { t@(Lex.Symbol Lex.DoubleColon _) }
    --'=>' { t@(Lex.Symbol Lex.FatArrow _) }
    '.' { t@(Lex.Symbol Lex.Dot _) }
%right '='
%left '!=' '=='
%left '>' '<'
%left '+' '-'
%left '*' '/'
%right UPLUS UMINUS '!'
%nonassoc IFX
%nonassoc KW_ELSE
%%
Sep :: { () }
    : ';' { () }
    | NL { () }
    ;
-- this one is reversed
StmtSeq :: { [Statement] }
    : Stmt { [$1] }
    | StmtSeq Stmt { $2 : $1 }
    ;
StmtSeqOpt :: { [Statement] }
    : {- empty -} { [] }
    | StmtSeq { reverse $1 }
    ;
ElseBlock :: { (Maybe Block, Token) }
    : KW_ELSE ';' { (Nothing, $1) }
    | KW_ELSE BlockLike { (Just $2, $1) }
    ;
IfStmt :: { Statement }
    : KW_IF Expr ';' %prec IFX { If $2 Nothing Nothing ($1, Nothing) }
    | KW_IF Expr ';' ElseBlock { If $2 Nothing (fst $4) ($1, Just $ snd $4) }
    | KW_IF Expr BlockLike %prec IFX { If $2 (Just $3) Nothing ($1, Nothing) }
    | KW_IF Expr BlockLike ElseBlock { If $2 (Just $3) (fst $4) ($1, Just $ snd $4) }
    ;
-- while
WhileStmt :: { Statement }
    : KW_WHILE Expr ';' %prec IFX { While $2 Nothing Nothing ($1, Nothing) }
    | KW_WHILE Expr ';' ElseBlock { While $2 Nothing (fst $4) ($1, Just $ snd $4) }
    | KW_WHILE Expr BlockLike %prec IFX { While $2 (Just $3) Nothing ($1, Nothing) }
    | KW_WHILE Expr BlockLike ElseBlock { While $2 (Just $3) (fst $4) ($1, Just $ snd $4) }
    ;
-- this argument is reversed
-- only the first one is current varName token
TypedParams :: { [(Class, String, [Token])] }
    : Clazz identity { [(fst $1, identText $2, $2 : snd $1)] }
    | TypedParams ',' Clazz identity { (fst $3, identText $4, $4 : snd $3) : $1 }
    ;
FunctionParams :: { [(Class, String, [Token])] }
    : '(' ')' { [] }
    | '(' TypedParams ')' { reverse $2 }
    ;
FunctionBlock :: { Block }
    : BlockLike { $1 }
    | '=' Expr Sep { Multiple [Command (Return (Just $2)) $1] }
    ;
FunctionStmt :: { Statement }
    : Clazz QName FunctionParams FunctionBlock { Function $1 (qnameToExpr $2) $3 $4 }
    | Clazz QName GenericParamList FunctionParams FunctionBlock { FunctionT $1 (qnameToExpr $2) $3 $4 $5 }
    ;
-- assembled statement
Stmt :: { Statement }
    : KW_CONTINUE { Command Continue $1 }
    | KW_BREAK { Command Break $1 }
    | KW_RETURN ';' { Command (Return Nothing) $1 }
    | KW_RETURN Stmt { Command (Return (Just $ stmtToExpr $1 $2)) $1 }
    | Expr Sep { Expr $1 }
    | Block Sep { BlockStmt $ fromMaybe (Multiple []) $1 }
    | IfStmt { $1 }
    | WhileStmt { $1 }
    | FunctionStmt { $1 }
    ;
-- block
Block :: { Maybe Block }
    : '{' StmtSeqOpt '}' { Just $ Multiple $2 }
    | '{' Sep '}' { Nothing }
    ;
BlockLike :: { Block }
    : ':' Stmt { Multiple [$2] }
    | Block Sep { fromMaybe (Multiple []) $1 }
    | NL Block Sep { fromMaybe (Multiple []) $2 }
    ;
-- this qname is reversed
QName :: { ([String], [Token]) }
    : identity { ([identText $1], [$1]) }
    | QName '.' identity { let (xs, ts) = $1 in (identText $3 : xs, $3 : ts) }
    ;
-- this Clazzs is reversed
ClazzList :: { [(Class, [Token])] }
    : Clazz { [$1] }
    | ClazzList ',' Clazz { $3 : $1 }
    ;
Clazz :: { (Class, [Token]) }
    : QName { (normalizeClass $ Class (reverse (fst $1)) [] , snd $1) }
    | QName '::' '<' ClazzList '>' { let reversed = reverse $4 in
        (normalizeClass $ Class (reverse (fst $1)) (map fst reversed),
        concat [snd $1, [$2, $3], concatMap snd reversed, [$5]]) }
    ;
LValue :: { Expression }
    : QName { qnameToExpr $1 };
Atom :: { Expression }
    : KW_TRUE { BoolConst True $1 }
    | KW_FALSE { BoolConst False $1 }
    -- unary operators
    | '!' Atom %prec '!' { Unary BitNot $2 $1 }
    | '!' error %prec '!' { Error [$1] (expectedExpression 1 "!") }
    | '+' Atom %prec UPLUS { Unary UnaryPlus $2 $1}
    | '+' error %prec UPLUS { Error [$1] (expectedExpression 1 "+")}
    | '-' Atom %prec UMINUS { Unary UnaryMinus $2 $1}
    | '-' error %prec UMINUS { Error [$1] (expectedExpression 1 "-") }
    -- parenthesized expression
    | '(' Expr ')' { $2 }
    -- identifiers
    | QName { qnameToExpr $1 }
    -- literals
    | number { fromMaybe (Error [$1] "classifyNumber failed") (classifyNumber (numText $1) $1) }
    | character { CharConst (charVal $1) $1 }
    | string { StringConst (strVal $1) $1 }
    ;
-- this qname is reversed
Arguments :: { [Expression] }
    : Expr { [$1] }
    | Arguments ',' Expr { $3 : $1 }
    ;
ArgumentsOpt :: { [Expression] }
    : '(' ')' { [] }
    | '(' Arguments ')' { reverse $2 }
    ;
GenericParamList :: { [(Class, [Token])] }
    : '::' '<' '>' { [] }
    | '::' '<' ClazzList '>' { reverse $3 }
    ;
Postfix :: { Expression }
    : Atom { $1 }
    | Postfix KW_AS Clazz { Cast $3 $1 $2 }
    | Postfix KW_AS error { Error [$2] (expectedExpression 0 "as") }
    ;
Expr :: { Expression }
    : QName ArgumentsOpt { Call (qnameToExpr $1) $2 }
    | QName GenericParamList ArgumentsOpt { CallT (qnameToExpr $1) $2 $3 }
    | LValue '=' Expr { if isVariable $1 then Binary Assign $1 $3 $2 else Error [$2] assignErrorMsg }
    | LValue '=' error { Error [$2] (expectedExpression 1 "=") }
    --
    | Expr '==' Expr { Binary Equal $1 $3 $2}
    | Expr '==' error { Error [$2] (expectedExpression 1 "==") }
    | Expr '!=' Expr { Binary NotEqual $1 $3 $2}
    | Expr '!=' error { Error [$2] (expectedExpression 1 "!=") }
    | Expr '>' Expr { Binary GreaterThan $1 $3 $2}
    | Expr '>' error { Error [$2] (expectedExpression 1 ">") }
    | Expr '<' Expr { Binary LessThan $1 $3 $2}
    | Expr '<' error { Error [$2] (expectedExpression 1 "<") }
    --
    | Expr '+' Expr { Binary Add $1 $3 $2}
    | Expr '+' error { Error [$2] (expectedExpression 1 "+") }
    | Expr '-' Expr { Binary Sub $1 $3 $2}
    | Expr '-' error { Error [$2] (expectedExpression 1 "-") }
    | Expr '*' Expr { Binary Mul $1 $3 $2}
    | Expr '*' error { Error [$2] (expectedExpression 1 "*") }
    | Expr '/' Expr { Binary Div $1 $3 $2}
    | Expr '/' error { Error [$2] (expectedExpression 1 "/") }
    | Postfix { $1 }
    ;
StmtEOF :: { Statement } : Stmt EOF { $1 };
{
-- Happy 2.0 expects a polymorphic error handler.
-- We return a sentinel statement containing one error expression.
parseStmtError :: [Token] -> a
parseStmtError toks =
    let e = mkHappyErrorExpr toks
        st = Expr e :: Statement
    in unsafeCoerce st
-- used for parse statement for ast tree
lexparseStmt :: Path -> String -> Either [ErrorKind] Statement
lexparseStmt p str =
    let (errs, tokens) = tokenizeWithNL p str
    in case errs of
        [] -> let st = parseStmt tokens
            in case getErrorProgram ([], [st]) of
                [] -> Right st
                es -> Left $ map (toException p) es
        _ -> Left errs
-- | used for debug version
replLexparseStmt :: String -> Either [ErrorKind] Statement
replLexparseStmt = lexparseStmt "stdin"
}
