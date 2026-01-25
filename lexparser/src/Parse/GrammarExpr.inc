QName
    : identity              { ([identText $1], [$1]) }
    | QName '.' identity    { let (xs, ts) = $1 in (xs ++ [identText $3], ts ++ [$3]) }
    ;

LValue : QName { qnameToExpr $1 };


Type : QName                { let (xs, _) = $1 in toClass (intercalate "." xs) };


Atom
    : KW_TRUE   { BoolConst True $1}
    | KW_FALSE  { BoolConst False $1}

    -- unary operators
    | '+' Atom %prec UPLUS   { Unary UnaryPlus $2 $1}
    | '+' error %prec UPLUS  { Error $1 (expectedExpression 1 "+")}

    | '-' Atom %prec UMINUS  { Unary UnaryMinus $2 $1}
    | '-' error %prec UMINUS { Error $1 (expectedExpression 1 "-") }

    -- parenthesized expression
    | '(' Expr ')'           { $2 }

    -- identifiers
    | QName                 { qnameToExpr $1 }

    -- literals
    | number                { fromMaybe (Error $1 "classifyNumber failed") (classifyNumber (numText $1) $1) }
    | character             { CharConst (charVal $1) $1 }
    | string                { StringConst (strVal $1) $1 }
    ;


Postfix
    : Atom                   { $1 }

    -- cast
    | Postfix KW_AS Type     { Cast $3 $1 $2 }
    | Postfix KW_AS error    { Error $2 (expectedExpression 0 "as") }
    ;


Expr
    : LValue '=' Expr        { if isVariable $1 then Binary Assign $1 $3 $2 else Error $2 assignErrorMsg }
    | LValue '=' error       { Error $2 (expectedExpression 1 "=") }

    | Expr '+' Expr          { Binary Add $1 $3 $2}
    | Expr '+' error         { Error $2 (expectedExpression 1 "+") }

    | Expr '-' Expr          { Binary Sub $1 $3 $2}
    | Expr '-' error         { Error $2 (expectedExpression 1 "-") }

    | Expr '*' Expr          { Binary Mul $1 $3 $2}
    | Expr '*' error         { Error $2 (expectedExpression 1 "*") }

    | Expr '/' Expr          { Binary Div $1 $3 $2}
    | Expr '/' error         { Error $2 (expectedExpression 1 "/") }

    | Postfix                { $1 }
    ;
