Sep :: { () }
    : ';' { () }
    | NL  { () }
    ;

-- this one is reversed
StmtSeq :: { [Statement] }
    : Stmt             { [$1] }
    | StmtSeq Stmt     { $2 : $1 }
    ;


StmtSeqOpt :: { [Statement] }
    : {- empty -}       { [] }
    | StmtSeq           { reverse $1 }
    ;


ElseBlock :: { (Maybe Block, Token) }
    : KW_ELSE ';'                       { (Nothing, $1) }
    | KW_ELSE Stmt                      { (Just $ stmtToBlock $2, $1) }
    | KW_ELSE ':' Stmt                  { (Just $ stmtToBlock $3, $1) }
    ;


IfStmt :: { Statement }
    : KW_IF Expr ';'                      %prec IFX { If $2 Nothing Nothing ($1, Nothing) }
    | KW_IF Expr ';' ElseBlock                      { If $2 Nothing (fst $4) ($1, Just $ snd $4) }
    | KW_IF Expr ':' Stmt                 %prec IFX { If $2 (Just $ stmtToBlock $4) Nothing ($1, Nothing) }
    | KW_IF Expr ':' Stmt ElseBlock                 { If $2 (Just $ stmtToBlock $4) (fst $5) ($1, Just $ snd $5) }
    ;


-- while
WhileStmt :: { Statement }
    : KW_WHILE Expr ';'                   %prec IFX { While $2 Nothing Nothing ($1, Nothing) }
    | KW_WHILE Expr ';' ElseBlock                   { While $2 Nothing (fst $4) ($1, Just $ snd $4) }
    | KW_WHILE Expr ':' Stmt              %prec IFX { While $2 (Just $ stmtToBlock $4) Nothing ($1, Nothing) }
    | KW_WHILE Expr ':' Stmt ElseBlock              { While $2 (Just $ stmtToBlock $4) (fst $5) ($1, Just $ snd $5) }
    ;
    

-- this argument is reversed
TypedParams :: { [(Class, String, [Token])] }
    : Clazz identity                            { [(fst $1, identText $2, $2 : snd $1)] }
    | TypedParams ',' Clazz identity            { (fst $3, identText $4, $4 : snd $3) : $1 }
    ;

FunctionParams :: { [(Class, String, [Token])] }
    : '(' ')'                       { [] }
    | '(' TypedParams ')'           { reverse $2 }
    ;

FunctionBlock :: { Block }
    : Stmt                      { Multiple [$1] }
    | Sep Stmt                  { Multiple [$2] }
    | ':' Stmt                  { Multiple [$2] }
    ;

FunctionStmt :: { Statement }
    : Clazz QName FunctionParams FunctionBlock                      { Function $1 (qnameToExpr $2) $3 $4 }
    | Clazz QName GenericParamList FunctionParams FunctionBlock     { FunctionT $1 (qnameToExpr $2) $3 $4 $5 }
    ;


-- assembled statement
Stmt :: { Statement }
    : KW_CONTINUE           { Command Continue $1 }
    | KW_BREAK              { Command Break $1 }
    | KW_RETURN ';'         { Command (Return Nothing) $1 }
    | KW_RETURN Stmt        { Command (Return (Just $ stmtToExpr $1 $2)) $1 }

    | Expr Sep              { Expr $1 }
    | Block Sep             { BlockStmt $ fromMaybe (Multiple []) $1 }
    | IfStmt                { $1 }
    | WhileStmt             { $1 }
    | FunctionStmt          { $1 }
    ;


-- block
Block :: { Maybe Block }
    : '{' StmtSeqOpt '}'        { Just $ Multiple $2 }
    | '{' Sep '}'               { Nothing }
    ;
