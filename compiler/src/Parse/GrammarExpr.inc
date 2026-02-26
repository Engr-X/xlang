-- this qname is reversed
QName :: { ([String], [Token]) }
    : identity              { ([identText $1], [$1]) }
    | QName '.' identity    { let (xs, ts) = $1 in (identText $3 : xs, $3 : ts) }
    ;

-- this Clazzs is reversed
ClazzList :: { [(Class, [Token])] }
    : Clazz                    { [$1] }
    | ClazzList ',' Clazz      { $3 : $1 }
    ;

Clazz :: { (Class, [Token]) }
    : QName                             { (normalizeClass $ Class (reverse (fst $1)) [] , snd $1) }
    | QName '::' '<' ClazzList '>'      { let reversed = reverse $4 in
        (normalizeClass $ Class (reverse (fst $1)) (map fst reversed),
        concat [snd $1, [$2, $3], concatMap snd reversed, [$5]]) }
    ;


LValue :: { Expression }
    : QName                 { qnameToExpr $1 };


Atom :: { Expression }
    : KW_TRUE   { BoolConst True $1 }
    | KW_FALSE  { BoolConst False $1 }

    -- unary operators
    | '!' Atom %prec '!'        { Unary BitNot $2 $1 }
    | '!' error %prec '!'       { Error [$1] (expectedExpression 1 "!") }

    | '+' Atom %prec UPLUS      { Unary UnaryPlus $2 $1}
    | '+' error %prec UPLUS     { Error [$1] (expectedExpression 1 "+")}

    | '-' Atom %prec UMINUS     { Unary UnaryMinus $2 $1}
    | '-' error %prec UMINUS    { Error [$1] (expectedExpression 1 "-") }

    -- parenthesized expression
    | '(' Expr ')'              { $2 }

    -- identifiers
    | QName                     { qnameToExpr $1 }

    -- literals
    | number                    { fromMaybe (Error [$1] "classifyNumber failed") (classifyNumber (numText $1) $1) }
    | character                 { CharConst (charVal $1) $1 }
    | string                    { StringConst (strVal $1) $1 }
    ;


-- this qname is reversed
Arguments :: { [Expression] }
    : Expr                      { [$1] }
    | Arguments ',' Expr        { $3 : $1 }
    ;

ArgumentsOpt :: { [Expression] }
    : '(' ')'                   { [] }
    | '(' Arguments ')'         { reverse $2 }
    ;

GenericParamList :: { [(Class, [Token])] }
    : '::' '<' '>'            { [] }
    | '::' '<' ClazzList '>'  { reverse $3 }
    ;


Postfix :: { Expression }
    : Atom                      { $1 }
    | Postfix KW_AS Clazz       { Cast $3 $1 $2 }
    | Postfix KW_AS error       { Error [$2] (expectedExpression 0 "as") }
    ;


Expr :: { Expression }
    : QName ArgumentsOpt                        { Call (qnameToExpr $1) $2 }
    | QName GenericParamList ArgumentsOpt       { CallT (qnameToExpr $1) $2 $3 }
    | LValue '=' Expr                           { if isVariable $1 then Binary Assign $1 $3 $2 else Error [$2] assignErrorMsg }
    | LValue '=' error                          { Error [$2] (expectedExpression 1 "=") }

    -- 
    | Expr '==' Expr            { Binary Equal $1 $3 $2}
    | Expr '==' error           { Error [$2] (expectedExpression 1 "==") }

    | Expr '!=' Expr            { Binary NotEqual $1 $3 $2}
    | Expr '!=' error           { Error [$2] (expectedExpression 1 "!=") }

    | Expr '>' Expr             { Binary GreaterThan $1 $3 $2}
    | Expr '>' error            { Error [$2] (expectedExpression 1 ">") }

    | Expr '<' Expr             { Binary LessThan $1 $3 $2}
    | Expr '<' error            { Error [$2] (expectedExpression 1 "<") }

    --
    | Expr '+' Expr             { Binary Add $1 $3 $2}
    | Expr '+' error            { Error [$2] (expectedExpression 1 "+") }

    | Expr '-' Expr             { Binary Sub $1 $3 $2}
    | Expr '-' error            { Error [$2] (expectedExpression 1 "-") }

    | Expr '*' Expr             { Binary Mul $1 $3 $2}
    | Expr '*' error            { Error [$2] (expectedExpression 1 "*") }

    | Expr '/' Expr             { Binary Div $1 $3 $2}
    | Expr '/' error            { Error [$2] (expectedExpression 1 "/") }

    | Postfix                   { $1 }
    ;
