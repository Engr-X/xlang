Sep :: { () }
    : ';' { () }
    | NL  { () }
    ;


StmtSeqOpt :: { [Statement] }
    : {- empty -} { [] }
    | StmtSeq     { $1 }
    ;


StmtSeq :: { [Statement] }
    : Stmt             { [$1] }
    | StmtSeq Stmt     { $1 ++ [$2] }
    ;


ElseBlock :: { Maybe Block }
    : KW_ELSE ';'                       { Nothing }
    | KW_ELSE ':' Stmt                  { Just $ stmtToBlock $3 }
    ;


IfStmt :: { Statement }
    : KW_IF Expr ';'                      %prec IFX { If $2 Nothing Nothing }
    | KW_IF Expr ';' ElseBlock                      { If $2 Nothing $4 }
    | KW_IF Expr ':' Stmt                 %prec IFX { If $2 (Just $ stmtToBlock $4) Nothing }
    | KW_IF Expr ':' Stmt ElseBlock                 { If $2 (Just $ stmtToBlock $4) $5 }
    ;


-- while
WhileStmt :: { Statement }
    : KW_WHILE Expr ';'                   %prec IFX { While $2 Nothing Nothing }
    | KW_WHILE Expr ';' ElseBlock                   { While $2 Nothing $4 }
    | KW_WHILE Expr ':' Stmt              %prec IFX { While $2 (Just $ stmtToBlock $4) Nothing }
    | KW_WHILE Expr ':' Stmt ElseBlock              { While $2 (Just $ stmtToBlock $4) $5 }
    ;


-- assembled statement
Stmt :: { Statement }
    : KW_CONTINUE           { Command Continue $1 }
    | KW_BREAK              { Command Break $1 }
    | KW_RETURN ';'         { Command (Return Nothing) $1}
    | KW_RETURN Stmt        { Command (Return (Just $ stmtToExpr $1 $2)) $1}

    | Expr Sep              { Expr $1 }
    | Block                 { BlockStmt $ fromMaybe (Multiple []) $1 }
    | IfStmt                { $1 }
    | WhileStmt             { $1 }
    ;


-- block
Block :: { Maybe Block }
    : '{' StmtSeqOpt '}' Sep    { Just $ Multiple $2 }
    | '{' Sep '}' Sep           { Nothing }
    ;
