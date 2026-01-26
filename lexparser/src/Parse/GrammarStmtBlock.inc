Sep :: { () }
    : ';' { () }
    | NL  { () }
    ;

Seps :: { () }
    : {- empty -} { () }
    | Seps Sep    { () }
    ;

Seps1 :: { () }
    : Sep         { () }
    | Seps1 Sep   { () }
    ;


StmtItem :: { Statement }
    : Stmt Seps1 { $1 }
    ;

StmtSeqOpt :: { [Statement] }
    : {- empty -} { [] }
    | StmtSeq     { $1 }
    ;

StmtSeq :: { [Statement] }
    : StmtItem             { [$1] }
    | StmtSeq StmtItem     { $1 ++ [$2] }
    ;

-- while
WhileStmt :: { Statement }
    : KW_WHILE Expr WhileBody   { While $2 $3 }
    ;

WhileBody :: { Maybe Block }
    : Block                     { Just $1 }                -- while e { ... }  or while e ;
    | NL Stmt                   { Just (stmtToBlock $2) }  -- while e \n stmt  (single stmt only)
    | ';'                       { Just (Multiple []) }     -- while e ;
    ;

-- assembled statement
Stmt :: { Statement }
    : WhileStmt     { $1 }
    | Expr          { Expr $1 }
    | Block         { BlockStmt $1 }
    ;


-- block
Block :: { Block }
    : '{' Seps StmtSeqOpt Seps '}' { Multiple $3 }
    | ';'                          { Multiple [] }
    ;

MaybeBlock :: { Maybe Block }
    : Block                          { Just $1 }
    | ';'                            { Just (Multiple []) }
    | Seps                           { Just (Multiple []) }
    ;
    

StmtEOF   :: { Statement } : Seps Stmt   Seps EOF { $2 };
BlockEOF  :: { Block }     : Seps Block  Seps EOF { $2 };
ProgramEOF :: { Program } : Seps StmtSeqOpt Seps EOF { ([], $2) };
