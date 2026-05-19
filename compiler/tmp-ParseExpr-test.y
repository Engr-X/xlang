{
module Parse.ParseExpr where
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
%name parseExpr Start
%tokentype { Token }
%error { parseExprError }
%token
%expect 1
    EOF { t@(Lex.EOF _) }
    NL { t@(Lex.TokenPass _) }
    KW_TRUE { t@(Lex.Ident "true" _) }
    KW_FALSE { t@(Lex.Ident "false" _) }
    KW_VAR { t@(Lex.Ident "var" _) }
    KW_VAL { t@(Lex.Ident "val" _) }
    KW_CONST { t@(Lex.Ident "const" _) }
    KW_FINAL { t@(Lex.Ident "final" _) }
    KW_MUT { t@(Lex.Ident "mut" _) }
    KW_INLINE { t@(Lex.Ident "inline" _) }
    KW_NATIVE { t@(Lex.Ident "native" _) }
    KW_POINTER { t@(Lex.Ident "pointer" _) }
    KW_BLOB { t@(Lex.Ident "blob" _) }
    KW_PUBLIC { t@(Lex.Ident "public" _) }
    KW_PROTECTED { t@(Lex.Ident "protected" _) }
    KW_PRIVATE { t@(Lex.Ident "private" _) }
    KW_AS { t@(Lex.Ident "as" _) }
    KW_SIZEOF { t@(Lex.Ident "sizeof" _) }
    KW_IF { t@(Lex.Ident "if" _) }
    KW_ELIF { t@(Lex.Ident "elif" _) }
    KW_ELSE { t@(Lex.Ident "else" _) }
    KW_DO { t@(Lex.Ident "do" _) }
    KW_WHILE { t@(Lex.Ident "while" _) }
    KW_UNTIL { t@(Lex.Ident "until" _) }
    KW_LOOP { t@(Lex.Ident "loop" _) }
    KW_FOR { t@(Lex.Ident "for" _) }
    KW_REPEAT { t@(Lex.Ident "repeat" _) }
    KW_BREAK { t@(Lex.Ident "break" _) }
    KW_CONTINUE { t@(Lex.Ident "continue" _) }
    KW_RETURN { t@(Lex.Ident "return" _) }
    KW_PASS { t@(Lex.Ident "pass" _) }
    KW_FUNC { t@(Lex.Ident "fun" _) }
    '++' { t@(Lex.Symbol Lex.PlusPlus _) }
    '--' { t@(Lex.Symbol Lex.MinusMinus _) }
    '!' { t@(Lex.Symbol Lex.LogicalNot _) }
    'inv' { t@(Lex.Ident "inv" _) }
    '**' { t@(Lex.Symbol Lex.Power _) }
    '*' { t@(Lex.Symbol Lex.Multiply _) }
    '/' { t@(Lex.Symbol Lex.Divide _) }
    '%' { t@(Lex.Symbol Lex.Modulo _) }
    '+' { t@(Lex.Symbol Lex.Plus _) }
    '-' { t@(Lex.Symbol Lex.Minus _) }
    'shl' { t@(Lex.Ident "shl" _) }
    'shr' { t@(Lex.Ident "shr" _) }
    'ushr' { t@(Lex.Ident "ushr" _) }
    '>' { t@(Lex.Symbol Lex.GreaterThan _) }
    '<' { t@(Lex.Symbol Lex.LessThan _) }
    '>=' { t@(Lex.Symbol Lex.GreaterEqual _) }
    '<=' { t@(Lex.Symbol Lex.LessEqual _) }
    '!=' { t@(Lex.Symbol Lex.NotEqual _) }
    '==' { t@(Lex.Symbol Lex.Equal _) }
    'and' { t@(Lex.Ident "and" _) }
    'nand' { t@(Lex.Ident "nand" _) }
    'xor' { t@(Lex.Ident "xor" _) }
    'xnor' { t@(Lex.Ident "xnor" _) }
    'or' { t@(Lex.Ident "or" _) }
    'nor' { t@(Lex.Ident "nor" _) }
    'implies' { t@(Lex.Ident "implies" _) }
    'nimplies' { t@(Lex.Ident "nimplies" _) }
    '&&' { t@(Lex.Symbol Lex.LogicalAnd _)}
    '!&&' { t@(Lex.Symbol Lex.LogicalNand _)}
    '||' { t@(Lex.Symbol Lex.LogicalOr _)}
    '!||' { t@(Lex.Symbol Lex.LogicalNor _)}
    '=' { t@(Lex.Symbol Lex.Assign _) }
    '+=' { t@(Lex.Symbol Lex.PlusAssign _) }
    '-=' { t@(Lex.Symbol Lex.MinusAssign _) }
    '*=' { t@(Lex.Symbol Lex.MultiplyAssign _) }
    '/=' { t@(Lex.Symbol Lex.DivideAssign _) }
    '%=' { t@(Lex.Symbol Lex.ModuloAssign _) }
    '**=' { t@(Lex.Symbol Lex.PowerAssign _) }
    identity { t@(Lex.Ident _ _) }
    annotation { t@(Lex.Annotation _ _ _) }
    number { t@(Lex.NumberConst _ _) }
    character { t@(Lex.CharConst _ _) }
    string { t@(Lex.StrConst _ _) }
    nativebody { t@(Lex.NativeBody _ _) }
    '(' { t@(Lex.Symbol Lex.LParen _) }
    ')' { t@(Lex.Symbol Lex.RParen _) }
    '[' { t@(Lex.Symbol Lex.LBracket _) }
    ']' { t@(Lex.Symbol Lex.RBracket _) }
    '{' { t@(Lex.Symbol Lex.LBrace _) }
    '}' { t@(Lex.Symbol Lex.RBrace _) }
    '@' { t@(Lex.Symbol Lex.At _) }
    ';' { t@(Lex.Symbol Lex.Semicolon _) }
    ',' { t@(Lex.Symbol Lex.Comma _) }
    ':' { t@(Lex.Symbol Lex.Colon _) }
    '.' { t@(Lex.Symbol Lex.Dot _) }
    '->' { t@(Lex.Symbol Lex.Arrow _) }
    '!->' { t@(Lex.Symbol Lex.NotArrow _) }
    '<->' { t@(Lex.Symbol Lex.IffArrow _) }
    '::' { t@(Lex.Symbol Lex.DoubleColon _) }
-- Happy precedence declarations are from low to high.
%nonassoc NL
%nonassoc IFX
%nonassoc '{'
-- 18: assignment
%right '=' '+=' '-=' '*=' '/=' '%=' '**='
-- 17: logical-iff (lower than implication)
%right '<->'
-- 18: logical-implication
%right '->' '!->'
-- 16: logical-or
%left '||' '!||'
-- 15: logical-and
%left '&&' '!&&'
-- 14: bit-implication
%right 'implies' 'nimplies'
-- 13: bit-or (reserved)
%left 'or' 'nor'
-- 12: bit-xor / bit-xnor (reserved)
%left 'xor' 'xnor'
-- 11: bit-and (reserved)
%left 'and' 'nand'
-- 10: equality
%left '!=' '=='
-- 9: relational
%left '>' '<' '>=' '<='
-- 8: shift (reserved)
%left 'shl' 'shr' 'ushr'
-- 7: additive
%left '+' '-'
-- 6: multiplicative
%left '*' '/' '%'
-- 5: power
%left '**'
-- 4: unary logical/bit not ('!' / 'inv')
%right '!' 'inv'
-- 3: unary plus/minus
%right UPLUS UMINUS
-- 2: postfix cast/index/inc/dec
%left KW_AS '[' '++' '--'
-- 2: prefix ++/-- (handled in Atom rules)
-- 1: postfix ++/-- (handled in Postfix rules)
-- 0: atom/call/cast
%nonassoc KW_ELIF KW_ELSE
%%
Start
    : Expr EOF { $1 }
    | Expr Sep EOF { $1 }
    ;
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
-- block
Block :: { Maybe Block }
    : '{' StmtSeqOpt '}' { Just $ Multiple $2 }
    | '{' Sep '}' { Nothing }
    ;
BlockLike :: { Block }
    : ':' Stmt { Multiple [$2] }
    | ':' Sep Stmt { Multiple [$3] }
    | Block Sep { fromMaybe (Multiple []) $1 }
    | NL Block Sep { fromMaybe (Multiple []) $2 }
    ;
ElseBlock :: { (Maybe Block, Token) }
    : KW_ELSE BlockLike { (Just $2, $1) }
    ;
-- loop
LoopStmt :: { Statement }
    : KW_LOOP ';' %prec IFX { Loop Nothing $1 }
    | KW_LOOP BlockLike { Loop (Just $2) $1 }
    ;
-- repeat
RepeatStmt :: { Statement }
    : KW_REPEAT Atom ';' %prec IFX { Repeat $2 Nothing Nothing ($1, Nothing) }
    | KW_REPEAT Atom ';' ElseBlock { Repeat $2 Nothing (fst $4) ($1, Just $ snd $4) }
    | KW_REPEAT Atom BlockLike %prec IFX { Repeat $2 (Just $3) Nothing ($1, Nothing) }
    | KW_REPEAT Atom BlockLike ElseBlock { Repeat $2 (Just $3) (fst $4) ($1, Just $ snd $4) }
    ;
-- while
WhileStmt :: { Statement }
    : KW_WHILE Expr ';' %prec IFX { While $2 Nothing Nothing ($1, Nothing) }
    | KW_WHILE Expr ';' ElseBlock { While $2 Nothing (fst $4) ($1, Just $ snd $4) }
    | KW_WHILE Expr BlockLike %prec IFX { While $2 (Just $3) Nothing ($1, Nothing) }
    | KW_WHILE Expr BlockLike ElseBlock { While $2 (Just $3) (fst $4) ($1, Just $ snd $4) }
    ;
-- until
UntilStmt :: { Statement }
    : KW_UNTIL Expr ';' %prec IFX { Until $2 Nothing Nothing ($1, Nothing) }
    | KW_UNTIL Expr ';' ElseBlock { Until $2 Nothing (fst $4) ($1, Just $ snd $4) }
    | KW_UNTIL Expr BlockLike %prec IFX { Until $2 (Just $3) Nothing ($1, Nothing) }
    | KW_UNTIL Expr BlockLike ElseBlock { Until $2 (Just $3) (fst $4) ($1, Just $ snd $4) }
    ;
-- do while
DoWhileStmt :: { Statement }
    : KW_DO NL KW_WHILE Expr Sep %prec IFX { DoWhile Nothing $4 Nothing ($1, $3, Nothing) }
    | KW_DO NL KW_WHILE Expr Sep ElseBlock { DoWhile Nothing $4 (fst $6) ($1, $3, Just $ snd $6) }
    | KW_DO BlockLike KW_WHILE Expr Sep %prec IFX { DoWhile (Just $2) $4 Nothing ($1, $3, Nothing) }
    | KW_DO BlockLike KW_WHILE Expr Sep ElseBlock { DoWhile (Just $2) $4 (fst $6) ($1, $3, Just $ snd $6) }
    ;
-- do until
DoUntilStmt :: { Statement }
    : KW_DO NL KW_UNTIL Expr Sep %prec IFX { DoUntil Nothing $4 Nothing ($1, $3, Nothing) }
    | KW_DO NL KW_UNTIL Expr Sep ElseBlock { DoUntil Nothing $4 (fst $6) ($1, $3, Just $ snd $6) }
    | KW_DO BlockLike KW_UNTIL Expr Sep %prec IFX { DoUntil (Just $2) $4 Nothing ($1, $3, Nothing) }
    | KW_DO BlockLike KW_UNTIL Expr Sep ElseBlock { DoUntil (Just $2) $4 (fst $6) ($1, $3, Just $ snd $6) }
    ;
MaybeExpr :: { Maybe Expression }
    : {- empty -} { Nothing }
    | Expr { (Just $1) }
    ;
ForExprList :: { [Expression] }
    : Expr ForExprListTail { $1 : $2 }
    ;
ForExprListTail :: { [Expression] }
    : {- empty -} { [] }
    | ',' Expr ForExprListTail { $2 : $3 }
    ;
ForVarInit :: { Statement }
    : VarDeclHead VarDeclItems
        { let
            (isConst, headTokens) = $1;
            mkDecl (names, mDeclType, mExpr, itemTokens) =
                (if isConst then DefConstField else DefField) names mDeclType mExpr (headTokens ++ itemTokens);
            decls = map mkDecl $2
        in case decls of
            [one] -> one;
            _ -> StmtGroup decls }
    ;
MaybeForInit :: { Maybe Statement }
    : {- empty -} { Nothing }
    | ForVarInit { Just $1 }
    | ForExprList { Just $ Exprs $1 }
    ;
MaybeForStep :: { Maybe Statement }
    : {- empty -} { Nothing }
    | ForExprList { Just $ Exprs $1 }
    ;
ForCondition :: { (Maybe Statement, Maybe Expression, Maybe Statement) }
    : '(' MaybeForInit ';' MaybeExpr ';' MaybeForStep ')' { ($2, $4, $6) }
    ;
ForStmt :: { Statement }
    : KW_FOR ForCondition ';' %prec IFX { For $2 Nothing Nothing ($1, Nothing) }
    | KW_FOR ForCondition ';' ElseBlock { For $2 Nothing (fst $4) ($1, Just $ snd $4) }
    | KW_FOR ForCondition BlockLike %prec IFX { For $2 (Just $3) Nothing ($1, Nothing) }
    | KW_FOR ForCondition BlockLike ElseBlock { For $2 (Just $3) (fst $4) ($1, Just $ snd $4) }
    ;
-- this argument is reversed
-- only the first one is current varName token
TypedParams :: { [(Class, String, [Token])] }
    : Clazz identity { [(fst $1, identText $2, $2 : snd $1)] }
    | identity ':' Clazz { [(fst $3, identText $1, [$1, $2] ++ snd $3)] }
    | KW_MUT identity ':' Clazz { [(fst $4, identText $2, [$1, $2, $3] ++ snd $4)] }
    | TypedParams ',' Clazz identity { (fst $3, identText $4, $4 : snd $3) : $1 }
    | TypedParams ',' identity ':' Clazz { (fst $5, identText $3, [$3, $4] ++ snd $5) : $1 }
    | TypedParams ',' KW_MUT identity ':' Clazz { (fst $6, identText $4, [$3, $4, $5] ++ snd $6) : $1 }
    ;
UntypedParam :: { (String, [Token]) }
    : identity { (identText $1, [$1]) }
    | KW_MUT identity { (identText $2, [$1, $2]) }
    ;
UntypedParams :: { [(String, [Token])] }
    : UntypedParam { [$1] }
    | UntypedParams ',' UntypedParam { $1 ++ [$3] }
    ;
FunctionParams :: { [(Class, String, [Token])] }
    : '(' ')' { [] }
    | '(' TypedParams ')' { reverse $2 }
    ;
UntypedFunctionParams :: { [(String, [Token])] }
    : '(' UntypedParams ')' { $2 }
    ;
-- generic parameters for function declarations:
-- support both legacy '::<T>' and simplified '<T>'
DeclGenericParamList :: { [(Class, [Token])] }
    : GenericParamList { $1 }
    | '<' '>' { [] }
    | '<' ClazzList '>' { reverse $2 }
    ;
FunctionBlock :: { Block }
    : BlockLike { $1 }
    | '=' Expr Sep { Multiple [Command (Return (Just $2)) $1] }
    ;
NativeSpecifier :: { ([Token], String) }
    : '@' KW_NATIVE '(' string ')' { ([$1, $2, $3, $4, $5], strVal $4) }
    ;
CFunctionBlock :: { String }
    : '{' nativebody '}' { nativeBodyVal $2 }
    | '{' nativebody Sep '}' { nativeBodyVal $2 }
    | '=' Expr Sep { "return (" ++ prettyExpr 0 (Just $2) ++ ")" }
    ;
AccessOpt :: { [Token] }
    : KW_PRIVATE { [$1] }
    | KW_PROTECTED { [$1] }
    | KW_PUBLIC { [$1] }
    ;
AccessOptOpt :: { [Token] }
    : {- empty -} { [] }
    | AccessOpt { $1 }
    ;
FunModifier :: { Token }
    : KW_PRIVATE { $1 }
    | KW_PROTECTED { $1 }
    | KW_PUBLIC { $1 }
    | KW_INLINE { $1 }
    | annotation { $1 }
    ;
FunModifierSeq :: { [Token] }
    : FunModifier { [$1] }
    | FunModifierSeq FunModifier { $1 ++ [$2] }
    | FunModifierSeq Sep FunModifier { $1 ++ [$3] }
    ;
FunModifierSeqOpt :: { [Token] }
    : {- empty -} { [] }
    | FunModifierSeq { $1 }
    | FunModifierSeq Sep { $1 }
    ;
NativeFunModifier :: { Token }
    : KW_PRIVATE { $1 }
    | KW_PROTECTED { $1 }
    | KW_PUBLIC { $1 }
    | KW_INLINE { $1 }
    | KW_NATIVE { $1 }
    | annotation { $1 }
    ;
NativeFunModifierSeq :: { [Token] }
    : NativeFunModifier { [$1] }
    | NativeFunModifierSeq NativeFunModifier { $1 ++ [$2] }
    | NativeFunModifierSeq Sep NativeFunModifier { $1 ++ [$3] }
    ;
NativeFunModifierSeqOpt :: { [Token] }
    : {- empty -} { [] }
    | NativeFunModifierSeq { $1 }
    | NativeFunModifierSeq Sep { $1 }
    ;
NativeSepOpt :: { () }
    : {- empty -} { () }
    | NativeSepOpt Sep { () }
    ;
VarKind :: { (Bool, Token) }
    : KW_VAR { (False, $1) }
    | KW_VAL { (True, $1) }
    ;
VarDeclHead :: { (Bool, [Token]) }
    : AccessOptOpt VarKind
        {
            let (isConst, kwTok) = $2
            in (isConst, $1 ++ [kwTok])
        }
    ;
ConstKw :: { Token }
    : KW_CONST { $1 }
    | KW_FINAL { $1 }
    ;
VarDeclItem :: { ([String], Maybe Class, Maybe Expression, [Token]) }
    : QName
        { (fst $1, Nothing, Nothing, snd $1) }
    | QName '=' Expr
        { (fst $1, Nothing, Just $3, snd $1 ++ [$2]) }
    | QName ':' Clazz
        { (fst $1, Just (fst $3), Nothing, snd $1 ++ [$2] ++ snd $3) }
    | QName ':' Clazz '=' Expr
        { (fst $1, Just (fst $3), Just $5, snd $1 ++ [$2] ++ snd $3 ++ [$4]) }
    ;
VarDeclItems :: { [([String], Maybe Class, Maybe Expression, [Token])] }
    : VarDeclItem { [$1] }
    | VarDeclItems ',' VarDeclItem { $1 ++ [$3] }
    ;
VarDeclStmt :: { Statement }
    : VarDeclHead VarDeclItems Sep
        { let
            (isConst, headTokens) = $1;
            mkDecl (names, mDeclType, mExpr, itemTokens) =
                (if isConst then DefConstField else DefField) names mDeclType mExpr (headTokens ++ itemTokens);
            decls = map mkDecl $2
        in case decls of
            [one] -> one;
            _ -> StmtGroup decls }
    ;
CDeclItem :: { ([String], Maybe Expression, [Token]) }
    : QName
        { (fst $1, Nothing, snd $1) }
    | QName '=' Expr
        { (fst $1, Just $3, snd $1 ++ [$2]) }
    ;
CDeclItems :: { [([String], Maybe Expression, [Token])] }
    : CDeclItem { [$1] }
    | CDeclItems ',' CDeclItem { $1 ++ [$3] }
    ;
FunctionStmt :: { Statement }
    : NativeSpecifier NativeSepOpt NativeFunModifierSeqOpt KW_FUNC QName FunctionParams '->' Clazz Sep
        { NativeMethod (fst $8, fst $1 ++ $3 ++ snd $8) (qnameToExpr $5) $6 (snd $1) }
    | NativeSpecifier NativeSepOpt NativeFunModifierSeqOpt KW_FUNC QName FunctionParams '->' Clazz CFunctionBlock
        { NativeMethod (fst $8, fst $1 ++ $3 ++ snd $8) (qnameToExpr $5) $6 (snd $1) }
    | FunModifierSeqOpt KW_FUNC QName UntypedFunctionParams FunctionBlock
        { mkUntypedSugarFunction ($1 ++ [$2]) $3 $4 $5 }
    | FunModifierSeqOpt KW_FUNC QName FunctionParams FunctionBlock
        { InstanceMethod (Void, $1 ++ [$2]) (qnameToExpr $3) $4 $5 }
    | FunModifierSeqOpt KW_FUNC QName DeclGenericParamList FunctionParams FunctionBlock
        { InstanceMethodT (Void, $1 ++ [$2]) (qnameToExpr $3) $4 $5 $6 }
    | FunModifierSeqOpt KW_FUNC QName FunctionParams '->' Clazz FunctionBlock
        { InstanceMethod (fst $6, $1 ++ snd $6) (qnameToExpr $3) $4 (normalizeFunctionBodyByRet (fst $6) $7) }
    | FunModifierSeqOpt KW_FUNC QName DeclGenericParamList FunctionParams '->' Clazz FunctionBlock
        { InstanceMethodT (fst $7, $1 ++ snd $7) (qnameToExpr $3) $4 $5 (normalizeFunctionBodyByRet (fst $7) $8) }
    ;
TypedLeadStmt :: { Statement }
    : Clazz QName FunctionParams FunctionBlock
        { InstanceMethod (fst $1, snd $1) (qnameToExpr $2) $3 (normalizeFunctionBodyByRet (fst $1) $4) }
    | Clazz QName DeclGenericParamList FunctionParams FunctionBlock
        { InstanceMethodT (fst $1, snd $1) (qnameToExpr $2) $3 $4 (normalizeFunctionBodyByRet (fst $1) $5) }
    | AccessOpt Clazz QName FunctionParams FunctionBlock
        { InstanceMethod (fst $2, $1 ++ snd $2) (qnameToExpr $3) $4 (normalizeFunctionBodyByRet (fst $2) $5) }
    | AccessOpt Clazz QName DeclGenericParamList FunctionParams FunctionBlock
        { InstanceMethodT (fst $2, $1 ++ snd $2) (qnameToExpr $3) $4 $5 (normalizeFunctionBodyByRet (fst $2) $6) }
    -- C-like declarations: int a; / int a = 10, b = 20;
    | Clazz CDeclItems Sep
        { let
              mkDecl (names, mExpr, itemTokens) = DefField names (Just $ fst $1) mExpr (snd $1 ++ itemTokens);
              decls = map mkDecl $2
          in case decls of
              [one] -> one;
              _ -> StmtGroup decls }
    | ConstKw Clazz CDeclItems Sep
        { let
              mkDecl (names, mExpr, itemTokens) = DefConstField names (Just $ fst $2) mExpr ([$1] ++ snd $2 ++ itemTokens);
              decls = map mkDecl $3
          in case decls of
              [one] -> one;
              _ -> StmtGroup decls }
    | AccessOpt Clazz CDeclItems Sep
        { let
              mkDecl (names, mExpr, itemTokens) = DefField names (Just $ fst $2) mExpr ($1 ++ snd $2 ++ itemTokens);
              decls = map mkDecl $3
          in case decls of
              [one] -> one;
              _ -> StmtGroup decls }
    | AccessOpt ConstKw Clazz CDeclItems Sep
        { let
              mkDecl (names, mExpr, itemTokens) = DefConstField names (Just $ fst $3) mExpr ($1 ++ [$2] ++ snd $3 ++ itemTokens);
              decls = map mkDecl $4
          in case decls of
              [one] -> one;
              _ -> StmtGroup decls }
    ;
StmtNoExpr :: { Statement }
    : KW_PASS Sep { Command Pass $1 }
    | KW_CONTINUE Sep { Command Continue $1 }
    | KW_BREAK Sep { Command Break $1 }
    | KW_RETURN Sep { Command (Return Nothing) $1 }
    | KW_RETURN Expr Sep { Command (Return (Just $2)) $1 }
    | VarDeclStmt { $1 }
    | TypedLeadStmt { $1 }
    | ForStmt { $1 }
    | LoopStmt { $1 }
    | RepeatStmt { $1 }
    | WhileStmt { $1 }
    | UntilStmt { $1 }
    | DoWhileStmt { $1 }
    | DoUntilStmt { $1 }
    | FunctionStmt { $1 }
    ;
-- assembled statement
Stmt :: { Statement }
    : StmtNoExpr { $1 }
    | Expr Sep
        { exprToStmt $1 }
    ;
-- this qname is reversed (value/name path)
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
    : '(' ')' '->' Clazz {
        let (retTy, retToks) = $4 in
        (normalizeClass $ FuncPtr retTy [], [$1, $2, $3] ++ retToks) }
    | '(' ClazzList ')' '->' Clazz {
        let (argPairs, (retTy, retToks)) = (reverse $2, $5) in
        (normalizeClass $ FuncPtr retTy (map fst argPairs), [$1] ++ concatMap snd argPairs ++ [$3, $4] ++ retToks) }
    | QName { (normalizeClass $ Class (reverse (fst $1)) [] , snd $1) }
    | QName '::' '<' ClazzList '>' { let reversed = reverse $4 in
        (normalizeClass $ Class (reverse (fst $1)) (map fst reversed),
        concat [snd $1, [$2, $3], concatMap snd reversed, [$5]]) }
    | KW_BLOB '[' BlobSizeExpr ']' {
        let (blobExpr, blobToks) = $3 in
        (Blob blobExpr, [$1, $2] ++ blobToks ++ [$4]) }
    | KW_POINTER '<' ClazzList '>' { let reversed = reverse $3 in
        (normalizeClass $ Class ["pointer"] (map fst reversed),
        [$1, $2] ++ concatMap snd reversed ++ [$4]) }
    | KW_POINTER '::' '<' ClazzList '>' { let reversed = reverse $4 in
        (normalizeClass $ Class ["pointer"] (map fst reversed),
        [$1, $2, $3] ++ concatMap snd reversed ++ [$5]) }
    | QName '::' '<' '*' '>' {
        (normalizeClass $ Class (reverse (fst $1)) [Class ["*"] []],
        concat [snd $1, [$2, $3, $4, $5]]) }
    | KW_POINTER '<' '*' '>' {
        (normalizeClass $ Class ["pointer"] [Class ["*"] []],
        [$1, $2, $3, $4]) }
    | KW_POINTER '::' '<' '*' '>' {
        (normalizeClass $ Class ["pointer"] [Class ["*"] []],
        [$1, $2, $3, $4, $5]) }
    ;
SizeofTypeExpr :: { (Class, [Token]) }
    : '(' ')' '->' Clazz {
        let (retTy, retToks) = $4 in
        (normalizeClass $ FuncPtr retTy [], [$1, $2, $3] ++ retToks) }
    | '(' ClazzList ')' '->' Clazz {
        let (argPairs, (retTy, retToks)) = (reverse $2, $5) in
        (normalizeClass $ FuncPtr retTy (map fst argPairs), [$1] ++ concatMap snd argPairs ++ [$3, $4] ++ retToks) }
    | QName '::' '<' ClazzList '>' { let reversed = reverse $4 in
        (normalizeClass $ Class (reverse (fst $1)) (map fst reversed),
        concat [snd $1, [$2, $3], concatMap snd reversed, [$5]]) }
    | KW_BLOB '[' BlobSizeExpr ']' {
        let (blobExpr, blobToks) = $3 in
        (Blob blobExpr, [$1, $2] ++ blobToks ++ [$4]) }
    | KW_POINTER '<' ClazzList '>' { let reversed = reverse $3 in
        (normalizeClass $ Class ["pointer"] (map fst reversed),
        [$1, $2] ++ concatMap snd reversed ++ [$4]) }
    | KW_POINTER '::' '<' ClazzList '>' { let reversed = reverse $4 in
        (normalizeClass $ Class ["pointer"] (map fst reversed),
        [$1, $2, $3] ++ concatMap snd reversed ++ [$5]) }
    | QName '::' '<' '*' '>' {
        (normalizeClass $ Class (reverse (fst $1)) [Class ["*"] []],
        concat [snd $1, [$2, $3, $4, $5]]) }
    | KW_POINTER '<' '*' '>' {
        (normalizeClass $ Class ["pointer"] [Class ["*"] []],
        [$1, $2, $3, $4]) }
    | KW_POINTER '::' '<' '*' '>' {
        (normalizeClass $ Class ["pointer"] [Class ["*"] []],
        [$1, $2, $3, $4, $5]) }
    ;
BlobSizeExpr :: { (Expression, [Token]) }
    : BlobSizeExpr '+' BlobSizeTerm { (Binary Add (fst $1) (fst $3) $2, snd $1 ++ [$2] ++ snd $3) }
    | BlobSizeExpr '-' BlobSizeTerm { (Binary Sub (fst $1) (fst $3) $2, snd $1 ++ [$2] ++ snd $3) }
    | BlobSizeTerm { $1 }
    ;
BlobSizeTerm :: { (Expression, [Token]) }
    : BlobSizeTerm '*' BlobSizeUnary { (Binary Mul (fst $1) (fst $3) $2, snd $1 ++ [$2] ++ snd $3) }
    | BlobSizeTerm '/' BlobSizeUnary { (Binary Div (fst $1) (fst $3) $2, snd $1 ++ [$2] ++ snd $3) }
    | BlobSizeTerm '%' BlobSizeUnary { (Binary Mod (fst $1) (fst $3) $2, snd $1 ++ [$2] ++ snd $3) }
    | BlobSizeUnary { $1 }
    ;
BlobSizeUnary :: { (Expression, [Token]) }
    : '+' BlobSizeUnary { (Unary UnaryPlus (fst $2) $1, [$1] ++ snd $2) }
    | '-' BlobSizeUnary { (Unary UnaryMinus (fst $2) $1, [$1] ++ snd $2) }
    | BlobSizeAtom { $1 }
    ;
BlobSizeAtom :: { (Expression, [Token]) }
    : number { (fromMaybe (Error [$1] "classifyNumber failed in blob size") (classifyNumber (numText $1) $1), [$1]) }
    | QName { (qnameToExpr $1, snd $1) }
    | KW_SIZEOF '(' SizeofTypeExpr ')' { (SizeOfType $3 $1, [$1, $2] ++ snd $3 ++ [$4]) }
    | KW_SIZEOF '(' QName ')' { (SizeOfExpr (qnameToExpr $3) $1, [$1, $2] ++ snd $3 ++ [$4]) }
    | '(' BlobSizeExpr ')' { (fst $2, [$1] ++ snd $2 ++ [$3]) }
    ;
LValue :: { Expression }
    : QName { qnameToExpr $1 }
    | '(' Expr ')' '.' identity { mkPointerSuffixExpr $2 $5 }
    | Postfix '[' Expr ']' { mkPointerIndexExpr $1 $3 $2 }
    | Postfix '[' Expr ']' '.' identity { mkPointerIndexSuffixExpr $1 $3 $2 $6 }
    ;
Atom :: { Expression }
    : KW_TRUE { BoolConst True $1 }
    | KW_FALSE { BoolConst False $1 }
    | KW_SIZEOF '(' Expr ')' { SizeOfExpr $3 $1 }
    | KW_SIZEOF '(' SizeofTypeExpr ')' { SizeOfType $3 $1 }
    -- unary operators
    | '!' Postfix %prec '!' { Unary LogicalNot $2 $1 }
    | '!' error %prec '!' { Error [$1] (expectedExpression 1 "!") }
    | 'inv' Postfix %prec '!' { Unary BitInv $2 $1 }
    | 'inv' error %prec '!' { Error [$1] (expectedExpression 1 "inv") }
    | '+' Atom %prec UPLUS { Unary UnaryPlus $2 $1}
    | '+' error %prec UPLUS { Error [$1] (expectedExpression 1 "+")}
    | '-' Atom %prec UMINUS { Unary UnaryMinus $2 $1}
    | '-' error %prec UMINUS { Error [$1] (expectedExpression 1 "-") }
    | '++' Atom %prec UPLUS { Unary IncSelf $2 $1 }
    | '++' error %prec UPLUS { Error [$1] (expectedExpression 1 "++") }
    | '--' Atom %prec UMINUS { Unary DecSelf $2 $1 }
    | '--' error %prec UMINUS { Error [$1] (expectedExpression 1 "--") }
    -- parenthesized expression
    | '(' Expr ')' { $2 }
    | ExprBlock { BlockExpr $ fromMaybe (Multiple []) $1 }
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
    -- calls (highest precedence, before cast/postfix ops)
    | QName ArgumentsOpt { Call (qnameToExpr $1) $2 }
    | QName GenericParamList ArgumentsOpt { CallT (qnameToExpr $1) $2 $3 }
    | Postfix '++' { Unary SelfInc $1 $2 }
    | Postfix '--' { Unary SelfDec $1 $2 }
    | Postfix '[' Expr ']' { mkPointerIndexExpr $1 $3 $2 }
    | Postfix '[' Expr ']' '.' identity { mkPointerIndexSuffixExpr $1 $3 $2 $6 }
    | Postfix KW_AS Clazz { Cast $3 $1 $2 }
    | Postfix KW_AS error { Error [$2] (expectedExpression 0 "as") }
    | '(' Expr ')' '.' identity { mkPointerSuffixExpr $2 $5 }
    ;
ExprBlock :: { Maybe Block }
    : Block { $1 }
    ;
IfExprBranch :: { Expression }
    : ':' Expr %prec IFX { $2 }
    | ':' Sep Expr %prec IFX { $3 }
    | ':' IfBranchStmt %prec IFX { BlockExpr (Multiple [$2]) }
    | ':' Sep IfBranchStmt %prec IFX { BlockExpr (Multiple [$3]) }
    | Block %prec IFX { BlockExpr (fromMaybe (Multiple []) $1) }
    | NL Block %prec IFX { BlockExpr (fromMaybe (Multiple []) $2) }
    ;
IfBranchStmt :: { Statement }
    : KW_PASS { Command Pass $1 }
    | KW_CONTINUE { Command Continue $1 }
    | KW_BREAK { Command Break $1 }
    | KW_RETURN %prec IFX { Command (Return Nothing) $1 }
    | KW_RETURN Expr %prec IFX { Command (Return (Just $2)) $1 }
    ;
IfExprTail :: { Maybe (Expression, Maybe Token) }
    : {- empty -} %prec IFX
        { Nothing }
    | KW_ELSE IfExprBranch
        { Just ($2, Just $1) }
    | KW_ELIF Expr IfExprBranch IfExprTail
        { Just (mkIfElifFromTail $1 $2 $3 $4) }
    ;
IfExprInline :: { Expression }
    : KW_IF Expr IfExprBranch IfExprTail
        { mkIfExprFromTail $1 $2 $3 $4 }
    ;
Expr :: { Expression }
    : AssignExpr %prec IFX { $1 }
    | Postfix { $1 }
    | IfExprInline %prec IFX { $1 }
    -- 5: power
    | Expr '**' Expr { Binary Pow $1 $3 $2 }
    | Expr '**' error { Error [$2] (expectedExpression 1 "**") }
    -- 6: multiplicative
    | Expr '*' Expr { Binary Mul $1 $3 $2 }
    | Expr '*' error { Error [$2] (expectedExpression 1 "*") }
    | Expr '/' Expr { Binary Div $1 $3 $2 }
    | Expr '/' error { Error [$2] (expectedExpression 1 "/") }
    | Expr '%' Expr { Binary Mod $1 $3 $2 }
    | Expr '%' error { Error [$2] (expectedExpression 1 "%") }
    -- 7: additive
    | Expr '+' Expr { Binary Add $1 $3 $2 }
    | Expr '+' error { Error [$2] (expectedExpression 1 "+") }
    | Expr '-' Expr { Binary Sub $1 $3 $2 }
    | Expr '-' error { Error [$2] (expectedExpression 1 "-") }
    -- 8: shift (reserved)
    | Expr 'shl' Expr { Binary BitLShift $1 $3 $2 }
    | Expr 'shl' error { Error [$2] (expectedExpression 1 "shl") }
    | Expr 'shr' Expr { Binary BitRShift $1 $3 $2 }
    | Expr 'shr' error { Error [$2] (expectedExpression 1 "shr") }
    | Expr 'ushr' Expr { Binary BitURShift $1 $3 $2 }
    | Expr 'ushr' error { Error [$2] (expectedExpression 1 "ushr") }
    -- 9: relational
    | Expr '>' Expr { Binary GreaterThan $1 $3 $2 }
    | Expr '>' error { Error [$2] (expectedExpression 1 ">") }
    | Expr '<' Expr { Binary LessThan $1 $3 $2 }
    | Expr '<' error { Error [$2] (expectedExpression 1 "<") }
    | Expr '>=' Expr { Binary GreaterEqual $1 $3 $2 }
    | Expr '>=' error { Error [$2] (expectedExpression 1 ">=") }
    | Expr '<=' Expr { Binary LessEqual $1 $3 $2 }
    | Expr '<=' error { Error [$2] (expectedExpression 1 "<=") }
    -- 10: equality
    | Expr '!=' Expr { Binary NotEqual $1 $3 $2 }
    | Expr '!=' error { Error [$2] (expectedExpression 1 "!=") }
    | Expr '==' Expr { Binary Equal $1 $3 $2 }
    | Expr '==' error { Error [$2] (expectedExpression 1 "==") }
    -- 11: bit-and (reserved)
    | Expr 'and' Expr { Binary BitAnd $1 $3 $2 }
    | Expr 'and' error { Error [$2] (expectedExpression 1 "and") }
    | Expr 'nand' Expr { Binary BitNand $1 $3 $2 }
    | Expr 'nand' error { Error [$2] (expectedExpression 1 "nand") }
    -- 12: bit-xor / bit-xnor (reserved)
    | Expr 'xor' Expr { Binary BitXor $1 $3 $2 }
    | Expr 'xor' error { Error [$2] (expectedExpression 1 "xor") }
    | Expr 'xnor' Expr { Binary BitXnor $1 $3 $2 }
    | Expr 'xnor' error { Error [$2] (expectedExpression 1 "xnor") }
    -- 13: bit-or, bit-nor (reserved)
    | Expr 'or' Expr { Binary BitOr $1 $3 $2 }
    | Expr 'or' error { Error [$2] (expectedExpression 1 "or") }
    | Expr 'nor' Expr { Binary BitNor $1 $3 $2 }
    | Expr 'nor' error { Error [$2] (expectedExpression 1 "nor") }
    -- 14: bit implication
    | Expr 'implies' Expr { Binary BitImply $1 $3 $2 }
    | Expr 'implies' error { Error [$2] (expectedExpression 1 "implies") }
    | Expr 'nimplies' Expr { Binary BitNimply $1 $3 $2 }
    | Expr 'nimplies' error { Error [$2] (expectedExpression 1 "nimplies") }
    -- 15: logical-and (reserved)
    | Expr '&&' Expr { Binary LogicalAnd $1 $3 $2 }
    | Expr '&&' error { Error [$2] (expectedExpression 1 "&&") }
    | Expr '!&&' Expr { Binary LogicalNand $1 $3 $2 }
    | Expr '!&&' error { Error [$2] (expectedExpression 1 "!&&") }
    -- 16: logical-or (reserved)
    | Expr '||' Expr { Binary LogicalOr $1 $3 $2 }
    | Expr '||' error { Error [$2] (expectedExpression 1 "||") }
    | Expr '!||' Expr { Binary LogicalNor $1 $3 $2 }
    | Expr '!||' error { Error [$2] (expectedExpression 1 "!||") }
    -- 17: logical-implication (reserved)
    | Expr '->' Expr { Binary LogicalImply $1 $3 $2 }
    | Expr '->' error { Error [$2] (expectedExpression 1 "->") }
    | Expr '!->' Expr { Binary LogicalNimply $1 $3 $2 }
    | Expr '!->' error { Error [$2] (expectedExpression 1 "!->") }
    | Expr '<->' Expr { Binary LogicalAnd (Binary LogicalImply $1 $3 $2) (Binary LogicalImply $3 $1 $2) $2 }
    | Expr '<->' error { Error [$2] (expectedExpression 1 "<->") }
    ;
AssignExpr :: { Expression }
    : LValue '=' Expr { mkAssignExpr $1 $3 $2 }
    | LValue '=' error { Error [$2] (expectedExpression 1 "=") }
    | LValue '+=' Expr { mkAugAssignExpr Add $1 $3 $2 }
    | LValue '+=' error { Error [$2] (expectedExpression 1 "+=") }
    | LValue '-=' Expr { mkAugAssignExpr Sub $1 $3 $2 }
    | LValue '-=' error { Error [$2] (expectedExpression 1 "-=") }
    | LValue '*=' Expr { mkAugAssignExpr Mul $1 $3 $2 }
    | LValue '*=' error { Error [$2] (expectedExpression 1 "*=") }
    | LValue '/=' Expr { mkAugAssignExpr Div $1 $3 $2 }
    | LValue '/=' error { Error [$2] (expectedExpression 1 "/=") }
    | LValue '%=' Expr { mkAugAssignExpr Mod $1 $3 $2 }
    | LValue '%=' error { Error [$2] (expectedExpression 1 "%=") }
    | LValue '**=' Expr { mkAugAssignExpr Pow $1 $3 $2 }
    | LValue '**=' error { Error [$2] (expectedExpression 1 "**=") }
    ;
{
-- | Happy-required error continuation for expression parsing.
--
-- Happy enforces a polymorphic error handler of type:
-- [Token] -> a
--
-- However, our parser produces a concrete 'Expression' as the error result.
-- To bridge this mismatch, we deliberately use 'unsafeCoerce' here to coerce
-- an 'Expression' into the expected polymorphic return type.
--
-- This function should ONLY be used as the 'parseError' hook generated by Happy.
-- Any misuse outside the parser error path is undefined behavior.
parseExprError :: [Token] -> a
parseExprError toks = unsafeCoerce (mkHappyErrorExpr toks)
-- | High-level entry point for lexing + parsing a single expression.
--
-- This function performs the following pipeline:
--
-- 1. Tokenize the input source string with newline inference.
-- 2. If lexical errors exist, return them immediately.
-- 3. Parse the token stream into an 'Expression'.
-- 4. Traverse the resulting AST to collect embedded parse errors.
-- 5. Convert all expression-level errors into unified 'ErrorKind's.
--
-- On success, returns:
-- Right Expression
--
-- On failure, returns:
-- Left [ErrorKind]
--
-- This design intentionally separates:
-- * lexing errors (returned directly)
-- * parsing errors (embedded in AST, then extracted)
lexparseExpr :: Path -> String -> Either [ErrorKind] Expression
lexparseExpr p str = let (errs, tokens) = tokenizeWithNL p str in case errs of
    [] -> let expr = parseExpr tokens in case getErrorProgram ([], [Expr expr]) of
        [] -> Right expr
        errs -> Left $ map (toException p) errs
    _ -> Left errs
-- | used for debug version
replLexparseExpr :: String -> Either [ErrorKind] Expression
replLexparseExpr = lexparseExpr "stdin"
}
