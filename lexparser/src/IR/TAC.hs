module IR.TAC where

import Parse.SyntaxTree (Operator, Expression, Statement, Block, Class, Program)

import Data.Maybe (fromMaybe)

import qualified Parse.SyntaxTree as AST


-- Expand an expression into: (prefix statements, residual expression).
-- English note: This lifts chained assignments out of expressions so that
-- `if (a = b = true)` becomes `b = true; a = b; if (a) ...`.
expandExpr :: Expression -> ([Statement], Expression)

-- Leaf nodes (return the same node via as-pattern)
expandExpr (AST.Error _ _) = error "what this error for parser occur in Ir part?"
expandExpr e@(AST.IntConst _ _) = ([], e)
expandExpr e@(AST.LongConst _ _) = ([], e)
expandExpr e@(AST.FloatConst _ _) = ([], e)
expandExpr e@(AST.DoubleConst _ _) = ([], e)
expandExpr e@(AST.LongDoubleConst _ _) = ([], e)
expandExpr e@(AST.CharConst _ _) = ([], e)
expandExpr e@(AST.StringConst _ _) = ([], e)
expandExpr e@(AST.BoolConst _ _) = ([], e)

expandExpr e@(AST.Variable _ _) = ([], e)
expandExpr e@(AST.Qualified _ _) = ([], e)

-- Cast
expandExpr (AST.Cast ct x tok) = let (ss, x') = expandExpr x in (ss, AST.Cast ct x' tok)

-- Unary
expandExpr (AST.Unary op x tok) = let (ss, x') = expandExpr x in (ss, AST.Unary op x' tok)

-- Assignment lifting: a = (b = rhs)  ==>  b = rhs; a = b; and residual becomes lhs
-- Replace `Assign` with your actual "=" operator constructor.
expandExpr (AST.Binary AST.Assign lhs rhs tok) =
    let (ssR, rhs') = expandExpr rhs
        assignE = AST.Binary AST.Assign lhs rhs' tok
        assignS = AST.Expr assignE
        residual = lhs
    in (ssR ++ [assignS], residual)

-- Other binary ops
expandExpr (AST.Binary op a b tok) =
    let (ss1, a') = expandExpr a
        (ss2, b') = expandExpr b
    in (ss1 ++ ss2, AST.Binary op a' b' tok)

-- Calls
expandExpr (AST.Call f args) =
    let (ssf, f') = expandExpr f
        (ssa, args') = expandExprList args
    in (ssf ++ ssa, AST.Call f' args')

expandExpr (AST.CallT f tys args) =
    let (ssf, f') = expandExpr f
        (ssa, args') = expandExprList args
    in (ssf ++ ssa, AST.CallT f' tys args')


expandExprList :: [Expression] -> ([Statement], [Expression])
expandExprList [] = ([], [])
expandExprList (x:xs) =
    let (ss1, x')  = expandExpr x
        (ss2, xs') = expandExprList xs
    in (ss1 ++ ss2, x' : xs')


-- Expand a command statement.
-- English note: `return <expr>` may contain assignment-exprs that must be lifted
-- before the return, e.g. `return a = 10` -> `a = 10; return a`.
expandStmt :: Statement -> [Statement]
expandStmt e@(AST.Command AST.Continue _) = [e]
expandStmt e@(AST.Command AST.Break _) = [e]
expandStmt e@(AST.Command (AST.Return Nothing) _) = [e]
expandStmt (AST.Command (AST.Return (Just e)) tok) = let (ss, e') = expandExpr e in ss ++ [AST.Command (AST.Return (Just e')) tok]

expandStmt (AST.Expr e) = let (ss, e') = expandExpr e in ss ++ [AST.Expr e']
expandStmt (AST.BlockStmt b) = [AST.BlockStmt $ expandBlock b]
expandStmt (AST.If cond th el toks) =
    let (ssC, cond') = expandExpr cond
        th' = fmap expandBlock th
        el' = fmap expandBlock el
    in ssC ++ [AST.If cond' th' el' toks]

expandStmt (AST.For (mi, mc, ms) body tokFor) =
    let
        (ssi, mi') = expandMaybeExpr mi
        (_, mc') = expandMaybeExpr mc
        (sss, ms') = expandMaybeExpr ms
        (AST.Multiple body') = fromMaybe (AST.Multiple []) (fmap expandBlock body)
        bodyFinal = Just $ AST.Multiple (body' ++ sss)

    in
        -- Execute lifted init statements once before the loop
        ssi ++ [AST.For (mi', mc', ms') bodyFinal tokFor]
    where
        expandMaybeExpr :: Maybe Expression -> ([Statement], Maybe Expression)
        expandMaybeExpr Nothing  = ([], Nothing)
        expandMaybeExpr (Just e) =
            let (ss, e') = expandExpr e
            in (ss, Just e')


expandBlock :: Block -> Block
expandBlock (AST.Multiple ss) = AST.Multiple $ concatMap expandStmt ss


data IRAtom
    = Var (String, Int)                           -- local variable
    | BoolC Bool
    | CharC Char
    | Int8C Int
    | Int16C Int 
    | Int32C Int
    | Int64C Int 
    | Float32C Double 
    | Float64C Double 
    | Float128C Rational 


data IRInstr
    = IAssign IRAtom IRAtom                         -- dst = src (move/copy)
    | IUnary IRAtom Operator IRAtom                 -- dst = op x
    | IBinary IRAtom Operator IRAtom IRAtom         -- dst = x op y

    -- for class
    | IGetField  IRAtom IRAtom [String]             -- dst = obj.f
    | IPutField  IRAtom [String] IRAtom             -- obj.f = v

    | IGetStatic IRAtom [String]                    -- dst = C.f
    | IPutStatic [String] IRAtom                    -- C.f = v
