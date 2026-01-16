{
module Parse.Parser where

import Lex.Token
import Parse.SyntaxTree
}

%name parseExpr Expr
%tokentype { Token }
%error { happyError }

%token
  num   { NumberConst $$ _ }
  '+'   { Symbol Plus _ }
  '-'   { Symbol Minus _ }
  '*'   { Symbol Multiply _ }
  '/'   { Symbol Divide _ }
  '('   { Symbol LParen _ }
  ')'   { Symbol RParen _ }

%left '+' '-'
%left '*' '/'
%right UPLUS UMINUS

%%

Expr
  : Expr '+' Expr   { Binary Add $1 $3 }
  | Expr '-' Expr   { Binary Sub $1 $3 }
  | Expr '*' Expr   { Binary Mul $1 $3 }
  | Expr '/' Expr   { Binary Div $1 $3 }

  | '-' Expr %prec UMINUS { Unary UnaryMinus $2 }
  | '+' Expr %prec UPLUS  { Unary UnaryPlus $2 }

  | '(' Expr ')'     { $2 }
  | num              { IntConst $1 }
  ;

{
happyError :: [Token] -> a
happyError ts = error $ "Parse error near: " ++
    case ts of
        (t:_) -> show t
        []    -> "EOF"

checkBracket :: [Token] -> Bool
checkBracket = go []
  where
    go :: [Symbol] -> [Token] -> Bool
    go stk [] = null stk
    go stk (t:ts) =
        case t of
            Symbol sym _ -> case sym of
                LParen -> go (LParen:stk) ts
                LBracket -> go (LBracket:stk) ts
                LBrace -> go (LBrace:stk) ts

                RParen -> popMatch LParen stk ts
                RBracket -> popMatch LBracket stk ts
                RBrace -> popMatch LBrace stk ts

                _   -> go stk ts

            _ -> go stk ts

    popMatch :: Symbol -> [Symbol] -> [Token] -> Bool
    popMatch expected stk ts = case stk of
        (x:xs) | x == expected -> go xs ts
        _ -> False
}
