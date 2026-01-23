-- ============================================================
-- Expression grammar (shared by Expr / Stmt / Block / Program)
-- ============================================================

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

    -- unary operators
    | '+' Atom %prec UPLUS   { Unary UnaryPlus  $2 }
    | '+' error %prec UPLUS  { Error $1 (expectedExpression 1 "+") }

    | '-' Atom %prec UMINUS  { Unary UnaryMinus $2 }
    | '-' error %prec UMINUS { Error $1 (expectedExpression 1 "-") }

    -- parenthesized expression
    | '(' Expr ')'           { $2 }

    -- identifiers
    | QName                  { qnameToExpr $1 }

    -- literals
    | number                 { fromMaybe (error "classifyNumber failed")
                                      (classifyNumber $1) }
    | character              { CharConst $1 }
    | string                 { StringConst $1 }
    ;

Postfix
    : Atom                   { $1 }

    -- cast
    | Postfix KW_AS Type     { Cast $3 $1 }
    | Postfix KW_AS error    { Error $2 (expectedExpression 0 "as") }
    ;

Expr
    : Expr '+' Expr          { Binary Add $1 $3 }
    | Expr '+' error         { Error $2 (expectedExpression 1 "+") }

    | Expr '-' Expr          { Binary Sub $1 $3 }
    | Expr '-' error         { Error $2 (expectedExpression 1 "-") }

    | Expr '*' Expr          { Binary Mul $1 $3 }
    | Expr '*' error         { Error $2 (expectedExpression 1 "*") }

    | Expr '/' Expr          { Binary Div $1 $3 }
    | Expr '/' error         { Error $2 (expectedExpression 1 "/") }

    | Postfix                { $1 }
    ;
