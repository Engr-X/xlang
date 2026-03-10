Sep :: { () }
    : ';'           { () }
    | NL            { () }
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
    | KW_ELSE BlockLike                 { (Just $2, $1) }
    ;


IfStmt :: { Statement }
    : KW_IF Expr ';'                      %prec IFX { If $2 Nothing Nothing ($1, Nothing) }
    | KW_IF Expr ';' ElseBlock                      { If $2 Nothing (fst $4) ($1, Just $ snd $4) }
    | KW_IF Expr BlockLike                %prec IFX { If $2 (Just $3) Nothing ($1, Nothing) }
    | KW_IF Expr BlockLike ElseBlock                { If $2 (Just $3) (fst $4) ($1, Just $ snd $4) }
    ;


-- while
WhileStmt :: { Statement }
    : KW_WHILE Expr ';'                   %prec IFX { While $2 Nothing Nothing ($1, Nothing) }
    | KW_WHILE Expr ';' ElseBlock                   { While $2 Nothing (fst $4) ($1, Just $ snd $4) }
    | KW_WHILE Expr BlockLike             %prec IFX { While $2 (Just $3) Nothing ($1, Nothing) }
    | KW_WHILE Expr BlockLike ElseBlock             { While $2 (Just $3) (fst $4) ($1, Just $ snd $4) }
    ;


-- this argument is reversed
-- only the first one is current varName token
TypedParams :: { [(Class, String, [Token])] }
    : Clazz identity                            { [(fst $1, identText $2, $2 : snd $1)] }
    | identity ':' Clazz                        { [(fst $3, identText $1, [$1, $2] ++ snd $3)] }

    | TypedParams ',' Clazz identity            { (fst $3, identText $4, $4 : snd $3) : $1 }
    | TypedParams ',' identity ':' Clazz        { (fst $5, identText $3, [$3, $4] ++ snd $5) : $1 }
    ;

FunctionParams :: { [(Class, String, [Token])] }
    : '(' ')'                       { [] }
    | '(' TypedParams ')'           { reverse $2 }
    ;

FunctionBlock :: { Block }
    : BlockLike                     { $1 }
    | '=' Expr Sep                  { Multiple [Command (Return (Just $2)) $1] }
    ;


AccessOpt :: { [Token] }
    : KW_PRIVATE                    { [$1] }
    | KW_PROTECTED                  { [$1] }
    | KW_PUBLIC                     { [$1] }
    ;


FunctionStmt :: { Statement }
    : Clazz QName FunctionParams FunctionBlock
        { StaticMethod (fst $1, snd $1) (qnameToExpr $2) $3 $4 }

    | KW_FUNC QName FunctionParams '->' Clazz FunctionBlock
        { StaticMethod (fst $5, snd $5) (qnameToExpr $2) $3 $6 }

    | Clazz QName GenericParamList FunctionParams FunctionBlock
        { StaticMethodT (fst $1, snd $1) (qnameToExpr $2) $3 $4 $5 }

    | KW_FUNC QName GenericParamList FunctionParams '->' Clazz FunctionBlock
        { StaticMethodT (fst $6, snd $6) (qnameToExpr $2) $3 $4 $7 }

    | AccessOpt Clazz QName FunctionParams FunctionBlock
        { StaticMethod (fst $2, $1 ++ snd $2) (qnameToExpr $3) $4 $5 }

    | AccessOpt KW_FUNC QName FunctionParams '->' Clazz FunctionBlock
        { StaticMethod (fst $6, $1 ++ snd $6) (qnameToExpr $3) $4 $7 }

    | AccessOpt Clazz QName GenericParamList FunctionParams FunctionBlock
        { StaticMethodT (fst $2, $1 ++ snd $2) (qnameToExpr $3) $4 $5 $6 }

    | AccessOpt KW_FUNC QName GenericParamList FunctionParams '->' Clazz FunctionBlock
        { StaticMethodT (fst $7, $1 ++ snd $7) (qnameToExpr $3) $4 $5 $8 }
    ;


-- assembled statement
Stmt :: { Statement }
    : KW_CONTINUE Sep           { Command Continue $1 }
    | KW_BREAK Sep              { Command Break $1 }
    | KW_RETURN Sep             { Command (Return Nothing) $1 }
    | KW_RETURN Stmt            { Command (Return (Just $ stmtToExpr $1 $2)) $1 }

    | Expr Sep                                  { Expr $1 }
    | KW_VAR QName '=' Expr Sep                 { DefVar (fst $2) (Just $4) ([$1, $3] ++ snd $2) }
    | KW_VAL QName '=' Expr Sep                 { DefConstVar (fst $2) (Just $4) ([$1, $3] ++ snd $2) }
    | AccessOpt KW_VAR QName '=' Expr Sep       { DefVar (fst $3) (Just $5) ($1 ++ [$2, $4] ++ snd $3) }
    | AccessOpt KW_VAL QName '=' Expr Sep       { DefConstVar (fst $3) (Just $5) ($1 ++ [$2, $4] ++ snd $3) }

    | Block Sep                     { BlockStmt $ fromMaybe (Multiple []) $1 }
    | IfStmt                        { $1 }
    | WhileStmt                     { $1 }
    | FunctionStmt                  { $1 }
    ;


-- block
Block :: { Maybe Block }
    : '{' StmtSeqOpt '}'        { Just $ Multiple $2 }
    | '{' Sep '}'               { Nothing }
    ;

BlockLike :: { Block }
    : ':' Stmt                      { Multiple [$2] }
    | Block Sep                     { fromMaybe (Multiple []) $1 }
    | NL Block Sep                  { fromMaybe (Multiple []) $2 }
    ;
