@file.class("Parser")
package xlang.compiler.parser


import xlang.Operation
import xlang.compiler.Type
import xlang.compiler.lexer.Tokenizer
import xlang.compiler.parser.expression.Assignment
import xlang.compiler.parser.expression.Atom
import xlang.compiler.parser.expression.Expression
import xlang.compiler.parser.expression.ExpressionDesugar
import xlang.compiler.parser.expression.ExpressionTuple
import xlang.compiler.parser.expression.Expressions
import xlang.compiler.parser.expression.FieldAccess
import xlang.compiler.parser.expression.IndexAccess
import xlang.compiler.parser.expression.ListLiteral
import xlang.compiler.parser.expression.MethodCall
import xlang.compiler.parser.expression.NewFunction
import xlang.compiler.parser.expression.NewIdentifier
import xlang.compiler.parser.expression.TypeCast
import xlang.compiler.parser.statement.ExprListStatement
import xlang.compiler.parser.statement.ExprStatement
import xlang.compiler.parser.statement.ReturnStatement
import xlang.compiler.parser.statement.Statement
import xlang.compiler.parser.statement.Statements
import xlang.compiler.parser.statement.VariableDefine
import xlang.compiler.parser.statement.VariableDefines
import xlang.compiler.parser.stmtexpr.BlockExpr
import xlang.compiler.parser.stmtexpr.IfBranch
import xlang.compiler.parser.stmtexpr.IfElseBranch
import xlang.compiler.parser.stmtexpr.StatementExpression
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.parser.ParseContainer
import xlang.parser.PrattParser
import xlang.parser.util.ParserRef
import xlang.parser.util.ParserRefs
import xlang.parser.util.PatternList
import xlang.parser.util.Rule
import xlang.util.ArrayList


val TYPE_PARSER: pointer<ParserRef> = ParserRef.fromType(1000)


private val EXPRESSION_PARSER_ID: int = 1
private val ATOM_PARSER_ID: int = 2
private val EXPRESSIONS_PARSER_ID: int = 3
private val EXPRESSION_TUPLE_PARSER_ID: int = 4
private val LIST_LITERAL_PARSER_ID: int = 5
private val STATEMENT_PARSER_ID: int = 6
private val STATEMENTS_PARSER_ID: int = 7
private val EXPR_STATEMENT_PARSER_ID: int = 8
private val EXPR_LIST_STATEMENT_PARSER_ID: int = 9
private val VARIABLE_DEFINE_PARSER_ID: int = 10
private val VARIABLE_DEFINES_PARSER_ID: int = 11
private val RETURN_STATEMENT_PARSER_ID: int = 12
private val BLOCK_EXPR_PARSER_ID: int = 13
private val IF_BRANCH_PARSER_ID: int = 14
private val IF_ELSE_BRANCH_PARSER_ID: int = 15


private inline fun getContainerValue(results: pointer<ArrayList>, index: int, unwrapContainer: bool) -> pointer<*>
{
    val slot: pointer<pointer<*>> = results.get(index) as pointer<pointer<*>>
    val value: pointer<*> = slot.deref

    if !unwrapContainer:
        return value

    val container: pointer<ParseContainer> = value as pointer<ParseContainer>
    return container.getValue()
}

private inline fun getContainerValue(results: pointer<ArrayList>, index: int) -> pointer<*> =
    getContainerValue(results, index, true)

private fun makeExprFromPrefixWith(
    results: pointer<ArrayList>,
    build: (pointer<Operation>, pointer<Expression>) -> pointer<Expression>) -> pointer<*>
{
    val opToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val op: pointer<Operation> = toOperation(opToken, Operation.PREFIX_TYPE)

    if op == null:
        return null

    val expression: pointer<Expression> = getContainerValue(results, 1) as pointer<Expression>
    val result: pointer<Expression> = build(op, expression)

    if result == null:
        return null

    return result.addExtraToken(opToken)
}

private fun makeExprFromInfixWith(
    results: pointer<ArrayList>,
    build: (pointer<Operation>, pointer<Expression>, pointer<Expression>) -> pointer<Expression>) -> pointer<*>
{
    val opToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val op: pointer<Operation> = toOperation(opToken, Operation.INFIX_TYPE)

    if op == null:
        return null

    val left: pointer<Expression> = getContainerValue(results, 0) as pointer<Expression>
    val right: pointer<Expression> = getContainerValue(results, 2) as pointer<Expression>
    val result: pointer<Expression> = build(op, left, right)

    if result == null:
        return null

    return result.addExtraToken(opToken)
}

private fun makeExprFromAssignWith(results: pointer<ArrayList>, subOp: pointer<Operation>) -> pointer<*>
{
    val opToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val op: pointer<Operation> = toOperation(opToken, Operation.INFIX_TYPE)

    if op == null:
        return null

    val left: pointer<Expression> = getContainerValue(results, 0) as pointer<Expression>
    val right: pointer<Expression> = getContainerValue(results, 2) as pointer<Expression>
    val result: pointer<Expression> = ExpressionDesugar.fromAssignWith(op, left, right, subOp)

    if result == null:
        return null

    return result.addExtraToken(opToken)
}

private fun makeExprFromPostfixWith(
    results: pointer<ArrayList>,
    build: (pointer<Operation>, pointer<Expression>) -> pointer<Expression>) -> pointer<*>
{
    val opToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val op: pointer<Operation> = toOperation(opToken, Operation.POSTFIX_TYPE)

    if op == null:
        return null

    val expression: pointer<Expression> = getContainerValue(results, 0) as pointer<Expression>
    val result: pointer<Expression> = build(op, expression)

    if result == null:
        return null

    return result.addExtraToken(opToken)
}

private fun makeExprFromAssignWithBuilder(
    results: pointer<ArrayList>,
    build: (pointer<Operation>, pointer<Expression>, pointer<Expression>) -> pointer<Expression>) -> pointer<*>
{
    val opToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val op: pointer<Operation> = toOperation(opToken, Operation.INFIX_TYPE)

    if op == null:
        return null

    val left: pointer<Expression> = getContainerValue(results, 0) as pointer<Expression>
    val right: pointer<Expression> = getContainerValue(results, 2) as pointer<Expression>
    val value: pointer<Expression> = build(op, left, right)

    if value == null:
        return null

    val result: pointer<Expression> = ExpressionDesugar.fromAssignWith(op, left, value, null)

    if result == null:
        return null

    return result.addExtraToken(opToken)
}

private fun makeExprFromNewFunc(results: pointer<ArrayList>) -> pointer<*>
{
    val newToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val nameToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val tuple: pointer<ExpressionTuple> = getContainerValue(results, 2) as pointer<ExpressionTuple>

    val function: pointer<NewFunction> = new NewFunction(nameToken.text).setArguments(tuple)
    function.addExtraToken(newToken).addExtraToken(nameToken)

    return Expression.fromNewFunction(function)
}

private fun makeExprFromNewIdent(results: pointer<ArrayList>) -> pointer<*>
{
    val newToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val nameToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>

    val ident: pointer<NewIdentifier> = new NewIdentifier(nameToken.text)
    ident.addExtraToken(newToken).addExtraToken(nameToken)

    return Expression.fromNewIdentifier(ident)
}

private fun makeExprFromFuncCall(results: pointer<ArrayList>) -> pointer<*>
{
    val nameToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val tuple: pointer<ExpressionTuple> = getContainerValue(results, 1) as pointer<ExpressionTuple>

    val call: pointer<MethodCall> = new MethodCall(null, nameToken.text).setArguments(tuple)
    call.addExtraToken(nameToken)

    return Expression.fromMethodCall(call)
}

private fun makeExprFromAtom(results: pointer<ArrayList>) -> pointer<*>
{
    val atom: pointer<Atom> = getContainerValue(results, 0) as pointer<Atom>
    return Expression.fromAtom(atom)
}

private fun makeExprFromBlockExpr(results: pointer<ArrayList>) -> pointer<*>
{
    val block: pointer<BlockExpr> = getContainerValue(results, 0) as pointer<BlockExpr>
    return Expression.fromBlockExpr(block)
}

private fun makeExprFromIfBranch(results: pointer<ArrayList>) -> pointer<*>
{
    val branch: pointer<IfBranch> = getContainerValue(results, 0) as pointer<IfBranch>
    return Expression.fromIfBranch(branch)
}

private fun makeExprFromIfElseBranch(results: pointer<ArrayList>) -> pointer<*>
{
    val branch: pointer<IfElseBranch> = getContainerValue(results, 0) as pointer<IfElseBranch>
    return Expression.fromIfElseBranch(branch)
}

private fun makeExprFromParen(results: pointer<ArrayList>) -> pointer<*>
{
    val leftParen: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val rightParen: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val expression: pointer<Expression> = getContainerValue(results, 1) as pointer<Expression>
    val result: pointer<Expression> = expression.clone()

    return result.addExtraToken(leftParen).addExtraToken(rightParen)
}

private fun makeExprFromPrefix(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromPrefixWith(results, ExpressionDesugar.fromPrefix)


private fun makeExprFromIndexAccess(results: pointer<ArrayList>) -> pointer<*>
{
    val host: pointer<Expression> = getContainerValue(results, 0) as pointer<Expression>
    val list: pointer<ListLiteral> = getContainerValue(results, 1) as pointer<ListLiteral>

    return Expression.fromIndexAccess(host, list)
}

private fun makeExprFromMethodCall(results: pointer<ArrayList>) -> pointer<*>
{
    val host: pointer<Expression> = getContainerValue(results, 0) as pointer<Expression>
    val dotToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val nameToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val tuple: pointer<ExpressionTuple> = getContainerValue(results, 3) as pointer<ExpressionTuple>

    val call: pointer<MethodCall> = new MethodCall(host, nameToken.text).setArguments(tuple)
    call.addExtraToken(dotToken).addExtraToken(nameToken)

    return Expression.fromMethodCall(call)
}

private fun makeExprFromFieldAccess(results: pointer<ArrayList>) -> pointer<*>
{
    val host: pointer<Expression> = getContainerValue(results, 0) as pointer<Expression>
    val dotToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val fieldToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val expression: pointer<Expression> = Expression.fromFieldAccess(host, fieldToken.text)

    return expression.addExtraToken(dotToken).addExtraToken(fieldToken)
}

private fun makeExprFromPostfix(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromPostfixWith(results, ExpressionDesugar.fromPostfix)


private fun makeExprFromTypeCast(results: pointer<ArrayList>) -> pointer<*>
{
    val expression: pointer<Expression> = getContainerValue(results, 0) as pointer<Expression>
    val asToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val targetType: pointer<Type> = getContainerValue(results, 2) as pointer<Type>
    val result: pointer<Expression> = Expression.fromTypeCast(expression, targetType)

    return result.addExtraToken(asToken)
}

private fun makeExprFromInfix(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromInfixWith(results, ExpressionDesugar.fromInfix)


private fun makeExprFromCompare(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromInfixWith(results, ExpressionDesugar.fromCompare)


private fun makeExprFromNotRefEqual(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromInfixWith(results, ExpressionDesugar.makeNotRefEqual)


private fun makeExprFromNotEqual(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromInfixWith(results, ExpressionDesugar.makeNotEqual)


private fun makeExprFromBitwiseNand(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromInfixWith(results, ExpressionDesugar.makeBitwiseNand)


private fun makeExprFromBitwiseXor(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromInfixWith(results, ExpressionDesugar.makeBitwiseXor)


private fun makeExprFromBitwiseXnor(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromInfixWith(results, ExpressionDesugar.makeBitwiseXnor)


private fun makeExprFromBitwiseNor(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromInfixWith(results, ExpressionDesugar.makeBitwiseNor)


private fun makeExprFromImplies(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromInfixWith(results, ExpressionDesugar.makeImplies)


private fun makeExprFromNimplies(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromInfixWith(results, ExpressionDesugar.makeNimplies)


private fun makeExprFromIff(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromInfixWith(results, ExpressionDesugar.makeIff)


private fun makeExprFromNiff(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromInfixWith(results, ExpressionDesugar.makeNiff)


private fun makeExprFromLogicalNand(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromInfixWith(results, ExpressionDesugar.makeLogicalNand)


private fun makeExprFromLogicalXor(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromInfixWith(results, ExpressionDesugar.makeLogicalXor)


private fun makeExprFromLogicalXnor(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromInfixWith(results, ExpressionDesugar.makeLogicalXnor)


private fun makeExprFromLogicalNor(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromInfixWith(results, ExpressionDesugar.makeLogicalNor)


private fun makeExprFromLogicalImplies(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromInfixWith(results, ExpressionDesugar.makeLogicalImplies)


private fun makeExprFromLogicalNimplies(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromInfixWith(results, ExpressionDesugar.makeLogicalNimplies)


private fun makeExprFromLogicalIff(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromInfixWith(results, ExpressionDesugar.makeLogicalIff)


private fun makeExprFromLogicalNiff(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromInfixWith(results, ExpressionDesugar.makeLogicalNiff)


private fun makeExprFromAssign(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromAssignWith(results, null)


private fun makeExprFromPowAssign(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromAssignWith(results, OP_POW)


private fun makeExprFromTimesAssign(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromAssignWith(results, OP_TIMES)


private fun makeExprFromDivAssign(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromAssignWith(results, OP_DIV)


private fun makeExprFromRemAssign(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromAssignWith(results, OP_REM)


private fun makeExprFromPlusAssign(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromAssignWith(results, OP_PLUS)


private fun makeExprFromMinusAssign(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromAssignWith(results, OP_MINUS)


private fun makeExprFromShlAssign(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromAssignWith(results, OP_SHL)


private fun makeExprFromShrAssign(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromAssignWith(results, OP_SHR)


private fun makeExprFromUshlAssign(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromAssignWith(results, OP_SHL)


private fun makeExprFromUshrAssign(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromAssignWith(results, OP_USHR)


private fun makeExprFromBitwiseAndAssign(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromAssignWith(results, OP_BITWISE_AND)


private fun makeExprFromBitwiseNandAssign(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromAssignWithBuilder(results, ExpressionDesugar.makeBitwiseNand)


private fun makeExprFromBitwiseOrAssign(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromAssignWith(results, OP_BITWISE_OR)


private fun makeExprFromBitwiseNorAssign(results: pointer<ArrayList>) -> pointer<*> =
    makeExprFromAssignWithBuilder(results, ExpressionDesugar.makeBitwiseNor)


private fun makeAtom(results: pointer<ArrayList>) -> pointer<*>
{
    val token: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>

    if token.kind == Tokenizer.KW_NULL:
        return new Atom(Atom.NULL_IMM_KIND, results)
    elif token.kind == Tokenizer.KW_TRUE || token.kind == Tokenizer.KW_FALSE:
        return new Atom(Atom.BOOL_IMM_KIND, results)
    elif token.kind == Tokenizer.TK_CHAR:
        return new Atom(Atom.CHAR_IMM_KIND, results)
    elif token.kind == Tokenizer.TK_STRING:
        return new Atom(Atom.STRING_IMM_KIND, results)
    elif token.kind == Tokenizer.TK_INTEGER:
        return new Atom(Atom.INTEGER_IMM_KIND, results)
    elif token.kind == Tokenizer.TK_LONG:
        return new Atom(Atom.LONG_IMM_KIND, results)
    elif token.kind == Tokenizer.TK_FLOAT:
        return new Atom(Atom.FLOAT_IMM_KIND, results)
    elif token.kind == Tokenizer.TK_DOUBLE || token.kind == Tokenizer.TK_LONG_DOUBLE:
        return new Atom(Atom.DOUBLE_IMM_KIND, results)
    elif token.kind == Tokenizer.TK_IDENTIFIER:
        return new Atom(Atom.IDENTIFIER_KIND, results)
    else:
        return null
}

private inline fun makeExprsIt(results: pointer<ArrayList>) -> pointer<*>
{
    val expression: pointer<Expression> = getContainerValue(results, 0) as pointer<Expression>
    val commaToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val right: pointer<Expressions> = getContainerValue(results, 2) as pointer<Expressions>

    return new Expressions(expression).addExtraToken(commaToken).addExpressions(right)
}

private inline fun makeSingleExprs(results: pointer<ArrayList>) -> pointer<*>
{
    val expression: pointer<Expression> = getContainerValue(results, 0) as pointer<Expression>

    return new Expressions(expression)
}

private fun makeExpressionTuple0(results: pointer<ArrayList>) -> pointer<*>
{
    val leftParen: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val rightParen: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>

    return new ExpressionTuple().addExtraToken(leftParen).addExtraToken(rightParen)
}

private fun makeExpressionTuple1(results: pointer<ArrayList>) -> pointer<*>
{
    val leftParen: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val expressions: pointer<Expressions> = getContainerValue(results, 1) as pointer<Expressions>
    val rightParen: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>

    return new ExpressionTuple(expressions.getExpressions())
       .addExtraTokens(expressions.getExtraTokens())
       .addExtraToken(leftParen).addExtraToken(rightParen)
}

private fun makeExpressionTuple2(results: pointer<ArrayList>) -> pointer<*>
{
    val leftParen: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val expressions: pointer<Expressions> = getContainerValue(results, 1) as pointer<Expressions>
    val comma: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val rightParen: pointer<Token> = getContainerValue(results, 3, false) as pointer<Token>

    return new ExpressionTuple(expressions.getExpressions())
       .addExtraTokens(expressions.getExtraTokens())
       .addExtraToken(leftParen).addExtraToken(comma).addExtraToken(rightParen)
}

private fun makeListLiteral0(results: pointer<ArrayList>) -> pointer<*>
{
    val leftBracket: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val rightBracket: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>

    return new ListLiteral().addExtraToken(leftBracket).addExtraToken(rightBracket)
}

private fun makeListLiteral1(results: pointer<ArrayList>) -> pointer<*>
{
    val leftBracket: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val expressions: pointer<Expressions> = getContainerValue(results, 1) as pointer<Expressions>
    val rightBracket: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>

    return new ListLiteral(expressions.getExpressions())
       .addExtraTokens(expressions.getExtraTokens())
       .addExtraToken(leftBracket).addExtraToken(rightBracket)
}

private fun makeListLiteral2(results: pointer<ArrayList>) -> pointer<*>
{
    val leftBracket: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val expressions: pointer<Expressions> = getContainerValue(results, 1) as pointer<Expressions>
    val comma: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val rightBracket: pointer<Token> = getContainerValue(results, 3, false) as pointer<Token>

    return new ListLiteral(expressions.getExpressions())
       .addExtraTokens(expressions.getExtraTokens())
       .addExtraToken(leftBracket).addExtraToken(comma).addExtraToken(rightBracket)
}

private inline fun makeStmtFrom_ExprStatement(results: pointer<ArrayList>) -> pointer<*>
{
    val exprStatement: pointer<ExprStatement> = getContainerValue(results, 0) as pointer<ExprStatement>
    return Statement.fromExprStatement(exprStatement)
}

private inline fun makeStmtFrom_ExprListStatement(results: pointer<ArrayList>) -> pointer<*>
{
    val exprStatement: pointer<ExprListStatement> = getContainerValue(results, 0) as pointer<ExprListStatement>
    return Statement.fromExprListStatement(exprStatement)
}

private inline fun makeStmtFrom_VariableDefine(results: pointer<ArrayList>) -> pointer<*>
{
    val varToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val variableDefine: pointer<VariableDefine> = getContainerValue(results, 1) as pointer<VariableDefine>

    var statement: pointer<Statement> = if varToken.kind == Tokenizer.KW_VAL:
         Statement.fromVariableDefine(variableDefine.markAsConst())
    else:
         Statement.fromVariableDefine(variableDefine.markAsMut())

    return statement.addExtraToken(varToken)
}

private inline fun makeStmtFrom_VariableDefines(results: pointer<ArrayList>) -> pointer<*>
{
    val varToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val variableDefines: pointer<VariableDefines> = getContainerValue(results, 1) as pointer<VariableDefines>

    var statement: pointer<Statement> = if varToken.kind == Tokenizer.KW_VAL:
         Statement.fromVariableDefines(variableDefines.markAsConst())
    else:
         Statement.fromVariableDefines(variableDefines.markAsMut())

    return statement.addExtraToken(varToken)
}

private inline fun makeStmtFrom_ReturnStmt(results: pointer<ArrayList>) -> pointer<*>
{
    val returnStatement: pointer<ReturnStatement> = getContainerValue(results, 0) as pointer<ReturnStatement>
    return Statement.fromReturnStatement(returnStatement)
}

private inline fun makeStmtsIt(results: pointer<ArrayList>) -> pointer<*>
{
    val statement: pointer<Statement> = getContainerValue(results, 0) as pointer<Statement>
    val right: pointer<Statements> = getContainerValue(results, 1) as pointer<Statements>

    return new Statements(statement).addStatements(right)
}

private inline fun makeSingleStmts(results: pointer<ArrayList>) -> pointer<*>
{
    val statement: pointer<Statement> = getContainerValue(results, 0) as pointer<Statement>

    return new Statements(statement)
}

private inline fun makeExprStmt(results: pointer<ArrayList>) -> pointer<*>
{
    val expression: pointer<Expression> = getContainerValue(results, 0) as pointer<Expression>
    return new ExprStatement(expression)
}

private inline fun makeExprList(results: pointer<ArrayList>) -> pointer<*>
{
    val expression: pointer<Expression> = getContainerValue(results, 0) as pointer<Expression>
    val commaToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val exprList: pointer<ExprListStatement> = getContainerValue(results, 2) as pointer<ExprListStatement>

    return new ExprListStatement(expression).addExtraToken(commaToken).addExpressions(exprList)
}

private inline fun makeSingleExprList(results: pointer<ArrayList>) -> pointer<*>
{
    val expression: pointer<Expression> = getContainerValue(results, 0) as pointer<Expression>
    return new ExprListStatement(expression)
}

private inline fun makeVariableDefine(results: pointer<ArrayList>) -> pointer<*>
{
    val nameToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val equalToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val expression: pointer<Expression> = getContainerValue(results, 2) as pointer<Expression>

    return new VariableDefine(nameToken.text, expression).addExtraToken(nameToken).addExtraToken(equalToken)
}

private inline fun makeVarDefsIt(results: pointer<ArrayList>) -> pointer<*>
{
    val first: pointer<VariableDefine> = getContainerValue(results, 0) as pointer<VariableDefine>
    val commaToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val second: pointer<VariableDefines> = getContainerValue(results, 2) as pointer<VariableDefines>

    return new VariableDefines(first).addExtraToken(commaToken).addDefines(second)
}

private inline fun makeVariableDefines(results: pointer<ArrayList>) -> pointer<*>
{
    val variableDefine: pointer<VariableDefine> = getContainerValue(results, 0) as pointer<VariableDefine>

    return new VariableDefines(variableDefine)
}

private inline fun makeVariableDefineWithType(results: pointer<ArrayList>) -> pointer<*>
{
    val nameToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val colonToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val declaredType: pointer<Type> = getContainerValue(results, 2) as pointer<Type>
    val equalToken: pointer<Token> = getContainerValue(results, 3, false) as pointer<Token>
    val expression: pointer<Expression> = getContainerValue(results, 4) as pointer<Expression>

    return new VariableDefine(declaredType, nameToken.text, expression)
        .addExtraToken(nameToken)
        .addExtraToken(colonToken)
        .addExtraToken(equalToken)
}

private inline fun makeReturnStmt(results: pointer<ArrayList>) -> pointer<*>
{
    val returnToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    var result: pointer<ReturnStatement> = if results.length <= 2:
        new ReturnStatement()
    else:
    {
        val expression: pointer<Expression> = getContainerValue(results, 1) as pointer<Expression>
        new ReturnStatement(expression)
    }

    return result.addExtraToken(returnToken)
}

private fun makeEmptyBlockExpr(results: pointer<ArrayList>) -> pointer<*>
{
    val leftBrace: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val rightBrace: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>

    return new BlockExpr().addExtraToken(leftBrace).addExtraToken(rightBrace)
}

private fun makeBlockExpr(results: pointer<ArrayList>) -> pointer<*>
{
    val leftBrace: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val statements: pointer<Statements> = getContainerValue(results, 1) as pointer<Statements>
    val rightBrace: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>

    return new BlockExpr(statements.getStatements())
       .addExtraToken(leftBrace).addExtraToken(rightBrace)
}

private fun makeIfBranchFromStmts(results: pointer<ArrayList>) -> pointer<*>
{
    val ifToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val condition: pointer<Expression> = getContainerValue(results, 1) as pointer<Expression>
    val colonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val block: pointer<BlockExpr> = getContainerValue(results, 3) as pointer<BlockExpr>

    return new IfBranch(condition, block.getStatements())
       .addExtraToken(ifToken).addExtraToken(colonToken)
       .addExtraTokens(block.getExtraTokens())
}

private fun makeIfBranchFromStmt(results: pointer<ArrayList>) -> pointer<*>
{
    val ifToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val condition: pointer<Expression> = getContainerValue(results, 1) as pointer<Expression>
    val colonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val statement: pointer<Statement> = getContainerValue(results, 3) as pointer<Statement>
    val statements: pointer<ArrayList> = new ArrayList(sizeof(Statement))
    statements.push(statement)

    return new IfBranch(condition, statements)
       .addExtraToken(ifToken).addExtraToken(colonToken)
}

private fun makeIfElseBranchFromSS(results: pointer<ArrayList>) -> pointer<*>
{
    val ifToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val condition: pointer<Expression> = getContainerValue(results, 1) as pointer<Expression>
    val ifColonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val ifStatement: pointer<Statement> = getContainerValue(results, 3) as pointer<Statement>
    val elseToken: pointer<Token> = getContainerValue(results, 4, false) as pointer<Token>
    val elseColonToken: pointer<Token> = getContainerValue(results, 5, false) as pointer<Token>
    val elseStatement: pointer<Statement> = getContainerValue(results, 6) as pointer<Statement>

    return new IfElseBranch(condition, ifStatement)
       .setElseStatement(elseStatement)
       .addExtraToken(ifToken).addExtraToken(ifColonToken)
       .addExtraToken(elseToken).addExtraToken(elseColonToken)
}

private fun makeIfElseBranchFromBS(results: pointer<ArrayList>) -> pointer<*>
{
    val ifToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val condition: pointer<Expression> = getContainerValue(results, 1) as pointer<Expression>
    val ifColonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val block: pointer<BlockExpr> = getContainerValue(results, 3) as pointer<BlockExpr>
    val elseToken: pointer<Token> = getContainerValue(results, 5, false) as pointer<Token>
    val elseColonToken: pointer<Token> = getContainerValue(results, 6, false) as pointer<Token>
    val elseStatement: pointer<Statement> = getContainerValue(results, 7) as pointer<Statement>

    return new IfElseBranch(condition, block.getStatements())
       .setElseStatement(elseStatement)
       .addExtraToken(ifToken).addExtraToken(ifColonToken)
       .addExtraTokens(block.getExtraTokens())
       .addExtraToken(elseToken).addExtraToken(elseColonToken)
}

private fun makeIfElseBranchFromSB(results: pointer<ArrayList>) -> pointer<*>
{
    val ifToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val condition: pointer<Expression> = getContainerValue(results, 1) as pointer<Expression>
    val ifColonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val ifStatement: pointer<Statement> = getContainerValue(results, 3) as pointer<Statement>
    val elseToken: pointer<Token> = getContainerValue(results, 4, false) as pointer<Token>
    val elseColonToken: pointer<Token> = getContainerValue(results, 5, false) as pointer<Token>
    val block: pointer<BlockExpr> = getContainerValue(results, 6) as pointer<BlockExpr>

    return new IfElseBranch(condition, ifStatement)
       .setElseStatements(block.getStatements())
       .addExtraToken(ifToken).addExtraToken(ifColonToken)
       .addExtraToken(elseToken).addExtraToken(elseColonToken)
       .addExtraTokens(block.getExtraTokens())
}

private fun makeIfElseBranchFromBB(results: pointer<ArrayList>) -> pointer<*>
{
    val ifToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val condition: pointer<Expression> = getContainerValue(results, 1) as pointer<Expression>
    val ifColonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val ifBlock: pointer<BlockExpr> = getContainerValue(results, 3) as pointer<BlockExpr>
    val elseToken: pointer<Token> = getContainerValue(results, 5, false) as pointer<Token>
    val elseColonToken: pointer<Token> = getContainerValue(results, 6, false) as pointer<Token>
    val elseBlock: pointer<BlockExpr> = getContainerValue(results, 7) as pointer<BlockExpr>

    return new IfElseBranch(condition, ifBlock.getStatements())
       .setElseStatements(elseBlock.getStatements())
       .addExtraToken(ifToken).addExtraToken(ifColonToken)
       .addExtraTokens(ifBlock.getExtraTokens())
       .addExtraToken(elseToken).addExtraToken(elseColonToken)
       .addExtraTokens(elseBlock.getExtraTokens())
}

private fun prependLineTerminator(tokens: pointer<TokenList>) -> pointer<TokenList>
{
    if tokens == null || tokens.length() <= 0:
        return tokens

    val first: pointer<Token> = tokens.get(0)
    val terminator: pointer<Token> = new Token(Tokenizer.TK_LINE_TERMINATOR, first.pos, "\n")
    tokens.pushFront(terminator)

    return tokens
}


private val EXPRESSION_PARSER_SPECIFIC: pointer<PrattParser> = new PrattParser()
val EXPRESSION_PARSER: pointer<ParserRef> = ParserRef.fromPratt(EXPRESSION_PARSER_ID, EXPRESSION_PARSER_SPECIFIC)

val ATOM_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(ATOM_PARSER_ID)

val EXPRESSIONS_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(EXPRESSIONS_PARSER_ID)

val EXPRESSION_TUPLE_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(EXPRESSION_TUPLE_PARSER_ID)

val LIST_LITERAL_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(LIST_LITERAL_PARSER_ID)

val STATEMENT_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(STATEMENT_PARSER_ID)

val STATEMENTS_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(STATEMENTS_PARSER_ID)

val EXPR_STATEMENT_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(EXPR_STATEMENT_PARSER_ID)

val EXPR_LIST_STATEMENT_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(EXPR_LIST_STATEMENT_PARSER_ID)

val VARIABLE_DEFINE_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(VARIABLE_DEFINE_PARSER_ID)

val VARIABLE_DEFINES_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(VARIABLE_DEFINES_PARSER_ID)

val RETURN_STATEMENT_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(RETURN_STATEMENT_PARSER_ID)

val BLOCK_EXPR_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(BLOCK_EXPR_PARSER_ID)

val IF_BRANCH_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(IF_BRANCH_PARSER_ID)

val IF_ELSE_BRANCH_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(IF_ELSE_BRANCH_PARSER_ID)

val OP_PAREN: pointer<Operation> = new Operation(0, "$paren", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 220, null)
val OP_SUCC: pointer<Operation> = new Operation(1, "++", Operation.POSTFIX_TYPE, Operation.LEFT_ASSOC, 210, "succ")
val OP_PRED: pointer<Operation> = new Operation(2, "--", Operation.POSTFIX_TYPE, Operation.LEFT_ASSOC, 210, "pred")
val OP_INC: pointer<Operation> = new Operation(3, "++", Operation.PREFIX_TYPE, Operation.LEFT_ASSOC, 200, "inc")
val OP_DEC: pointer<Operation> = new Operation(4, "--", Operation.PREFIX_TYPE, Operation.LEFT_ASSOC, 200, "dec")
val OP_POS: pointer<Operation> = new Operation(5, "+", Operation.PREFIX_TYPE, Operation.RIGHT_ASSOC, 200, "pos")
val OP_NEG: pointer<Operation> = new Operation(6, "-", Operation.PREFIX_TYPE, Operation.RIGHT_ASSOC, 200, "neg")
val OP_POW: pointer<Operation> = new Operation(7, "**", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 190, "pow")
val OP_TIMES: pointer<Operation> = new Operation(8, "*", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 180, "times")
val OP_DIV: pointer<Operation> = new Operation(9, "/", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 180, "div")
val OP_REM: pointer<Operation> = new Operation(10, "%", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 180, "rem")
val OP_PLUS: pointer<Operation> = new Operation(11, "+", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 170, "plus")
val OP_MINUS: pointer<Operation> = new Operation(12, "-", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 170, "minus")
val OP_SHL: pointer<Operation> = new Operation(13, "shl", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 160, "shl")
val OP_SHR: pointer<Operation> = new Operation(14, "shr", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 160, "shr")
val OP_USHR: pointer<Operation> = new Operation(15, "ushr", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 160, "ushr")
val OP_GREATER: pointer<Operation> = new Operation(16, ">", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 150, "greater")
val OP_LESS: pointer<Operation> = new Operation(17, "<", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 150, "less")
val OP_GREATER_EQUAL: pointer<Operation> = new Operation(18, ">=", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 150, "greaterEqual")
val OP_LESS_EQUAL: pointer<Operation> = new Operation(19, "<=", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 150, "lessEqual")
val OP_REF_EQUALS: pointer<Operation> = new Operation(20, "===", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 140, "refEquals")
val OP_EQUALS: pointer<Operation> = new Operation(21, "==", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 140, "equals")
val OP_NOT_REF_EQUALS: pointer<Operation> = new Operation(22, "!==", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 140, "notRefEquals")
val OP_NOT_EQUALS: pointer<Operation> = new Operation(23, "!=", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 140, "notEquals")
val OP_INV: pointer<Operation> = new Operation(24, "inv", Operation.PREFIX_TYPE, Operation.RIGHT_ASSOC, 130, "inv")
val OP_BITWISE_AND: pointer<Operation> = new Operation(25, "and", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 120, "bitwiseAnd")
val OP_BITWISE_NAND: pointer<Operation> = new Operation(26, "nand", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 120, "bitwiseNand")
val OP_BITWISE_XOR: pointer<Operation> = new Operation(27, "xor", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 110, "bitwiseXor")
val OP_BITWISE_XNOR: pointer<Operation> = new Operation(28, "xnor", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 110, "bitwiseXnor")
val OP_BITWISE_OR: pointer<Operation> = new Operation(29, "or", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 100, "bitwiseOr")
val OP_BITWISE_NOR: pointer<Operation> = new Operation(30, "nor", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 100, "bitwiseNor")
val OP_IMPLIES: pointer<Operation> = new Operation(31, "implies", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 90, "implies")
val OP_NIMPLIES: pointer<Operation> = new Operation(32, "nimplies", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 90, "nimplies")
val OP_IFF: pointer<Operation> = new Operation(33, "iff", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 80, "iff")
val OP_NIFF: pointer<Operation> = new Operation(34, "niff", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 80, "niff")
val OP_NOT: pointer<Operation> = new Operation(35, "!", Operation.PREFIX_TYPE, Operation.RIGHT_ASSOC, 70, "not")
val OP_LOGICAL_AND: pointer<Operation> = new Operation(36, "&&", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 60, "logicalAnd")
val OP_LOGICAL_NAND: pointer<Operation> = new Operation(37, "!&&", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 60, "logicalNand")
val OP_LOGICAL_XOR: pointer<Operation> = new Operation(38, "^", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 50, "logicalXor")
val OP_LOGICAL_XNOR: pointer<Operation> = new Operation(39, "!^", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 50, "logicalXnor")
val OP_LOGICAL_OR: pointer<Operation> = new Operation(40, "||", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 40, "logicalOr")
val OP_LOGICAL_NOR: pointer<Operation> = new Operation(41, "!||", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 40, "logicalNor")
val OP_LOGICAL_IMPLIES: pointer<Operation> = new Operation(42, "->", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 30, "logicalImplies")
val OP_LOGICAL_NIMPLIES: pointer<Operation> = new Operation(43, "!->", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 30, "logicalNimplies")
val OP_LOGICAL_IFF: pointer<Operation> = new Operation(44, "<->", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 20, "logicalIff")
val OP_LOGICAL_NIFF: pointer<Operation> = new Operation(45, "!<->", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 20, "logicalNiff")
val OP_ASSIGN: pointer<Operation> = new Operation(46, "=", Operation.INFIX_TYPE, Operation.RIGHT_ASSOC, 10, "assign")
val OP_POW_ASSIGN: pointer<Operation> = new Operation(47, "**=", Operation.INFIX_TYPE, Operation.RIGHT_ASSOC, 10, "powAssign")
val OP_TIMES_ASSIGN: pointer<Operation> = new Operation(48, "*=", Operation.INFIX_TYPE, Operation.RIGHT_ASSOC, 10, "timesAssign")
val OP_DIV_ASSIGN: pointer<Operation> = new Operation(49, "/=", Operation.INFIX_TYPE, Operation.RIGHT_ASSOC, 10, "divAssign")
val OP_REM_ASSIGN: pointer<Operation> = new Operation(50, "%=", Operation.INFIX_TYPE, Operation.RIGHT_ASSOC, 10, "remAssign")
val OP_PLUS_ASSIGN: pointer<Operation> = new Operation(51, "+=", Operation.INFIX_TYPE, Operation.RIGHT_ASSOC, 10, "plusAssign")
val OP_MINUS_ASSIGN: pointer<Operation> = new Operation(52, "-=", Operation.INFIX_TYPE, Operation.RIGHT_ASSOC, 10, "minusAssign")
val OP_SHL_ASSIGN: pointer<Operation> = new Operation(53, "<<=", Operation.INFIX_TYPE, Operation.RIGHT_ASSOC, 10, "shlAssign")
val OP_SHR_ASSIGN: pointer<Operation> = new Operation(54, ">>=", Operation.INFIX_TYPE, Operation.RIGHT_ASSOC, 10, "shrAssign")
val OP_USHL_ASSIGN: pointer<Operation> = new Operation(55, "<<<=", Operation.INFIX_TYPE, Operation.RIGHT_ASSOC, 10, "ushlAssign")
val OP_USHR_ASSIGN: pointer<Operation> = new Operation(56, ">>>=", Operation.INFIX_TYPE, Operation.RIGHT_ASSOC, 10, "ushrAssign")
val OP_BITWISE_AND_ASSIGN: pointer<Operation> = new Operation(57, "&=", Operation.INFIX_TYPE, Operation.RIGHT_ASSOC, 10, "bitwiseAndAssign")
val OP_BITWISE_NAND_ASSIGN: pointer<Operation> = new Operation(58, "!&=", Operation.INFIX_TYPE, Operation.RIGHT_ASSOC, 10, "bitwiseNandAssign")
val OP_BITWISE_OR_ASSIGN: pointer<Operation> = new Operation(59, "|=", Operation.INFIX_TYPE, Operation.RIGHT_ASSOC, 10, "bitwiseOrAssign")
val OP_BITWISE_NOR_ASSIGN: pointer<Operation> = new Operation(60, "!|=", Operation.INFIX_TYPE, Operation.RIGHT_ASSOC, 10, "bitwiseNorAssign")

private fun toOperation(token: pointer<Token>, fixity: int) -> pointer<Operation>
{
    if token == null:
        return null

    if token.kind == Tokenizer.LEFT_PAREN && fixity == Operation.INFIX_TYPE:
        return OP_PAREN

    if token.kind == Tokenizer.PLUS && fixity == Operation.PREFIX_TYPE:
        return OP_POS

    if token.kind == Tokenizer.MINUS && fixity == Operation.PREFIX_TYPE:
        return OP_NEG

    if token.kind == Tokenizer.DOUBLE_PLUS && fixity == Operation.PREFIX_TYPE:
        return OP_INC

    if token.kind == Tokenizer.DOUBLE_MINUS && fixity == Operation.PREFIX_TYPE:
        return OP_DEC

    if token.kind == Tokenizer.KW_INV && fixity == Operation.PREFIX_TYPE:
        return OP_INV

    if token.kind == Tokenizer.BANG && fixity == Operation.PREFIX_TYPE:
        return OP_NOT

    if token.kind == Tokenizer.DOUBLE_PLUS && fixity == Operation.POSTFIX_TYPE:
        return OP_SUCC

    if token.kind == Tokenizer.DOUBLE_MINUS && fixity == Operation.POSTFIX_TYPE:
        return OP_PRED

    if token.kind == Tokenizer.DOUBLE_STAR && fixity == Operation.INFIX_TYPE:
        return OP_POW

    if token.kind == Tokenizer.STAR && fixity == Operation.INFIX_TYPE:
        return OP_TIMES

    if token.kind == Tokenizer.SLASH && fixity == Operation.INFIX_TYPE:
        return OP_DIV

    if token.kind == Tokenizer.PERCENT && fixity == Operation.INFIX_TYPE:
        return OP_REM

    if token.kind == Tokenizer.PLUS && fixity == Operation.INFIX_TYPE:
        return OP_PLUS

    if token.kind == Tokenizer.MINUS && fixity == Operation.INFIX_TYPE:
        return OP_MINUS

    if token.kind == Tokenizer.KW_SHL && fixity == Operation.INFIX_TYPE:
        return OP_SHL

    if token.kind == Tokenizer.KW_SHR && fixity == Operation.INFIX_TYPE:
        return OP_SHR

    if token.kind == Tokenizer.KW_USHR && fixity == Operation.INFIX_TYPE:
        return OP_USHR

    if token.kind == Tokenizer.GREATER && fixity == Operation.INFIX_TYPE:
        return OP_GREATER

    if token.kind == Tokenizer.LESS && fixity == Operation.INFIX_TYPE:
        return OP_LESS

    if token.kind == Tokenizer.GREATER_EQUAL && fixity == Operation.INFIX_TYPE:
        return OP_GREATER_EQUAL

    if token.kind == Tokenizer.LESS_EQUAL && fixity == Operation.INFIX_TYPE:
        return OP_LESS_EQUAL

    if token.kind == Tokenizer.TRIPLE_EQUAL && fixity == Operation.INFIX_TYPE:
        return OP_REF_EQUALS

    if token.kind == Tokenizer.DOUBLE_EQUAL && fixity == Operation.INFIX_TYPE:
        return OP_EQUALS

    if token.kind == Tokenizer.BANG_DOUBLE_EQUAL && fixity == Operation.INFIX_TYPE:
        return OP_REF_EQUALS

    if token.kind == Tokenizer.NOT_EQUAL && fixity == Operation.INFIX_TYPE:
        return OP_EQUALS

    if token.kind == Tokenizer.KW_AND && fixity == Operation.INFIX_TYPE:
        return OP_BITWISE_AND

    if token.kind == Tokenizer.KW_NAND && fixity == Operation.INFIX_TYPE:
        return OP_BITWISE_NAND

    if token.kind == Tokenizer.KW_XOR && fixity == Operation.INFIX_TYPE:
        return OP_BITWISE_XOR

    if token.kind == Tokenizer.KW_XNOR && fixity == Operation.INFIX_TYPE:
        return OP_BITWISE_XNOR

    if token.kind == Tokenizer.KW_OR && fixity == Operation.INFIX_TYPE:
        return OP_BITWISE_OR

    if token.kind == Tokenizer.KW_NOR && fixity == Operation.INFIX_TYPE:
        return OP_BITWISE_NOR

    if token.kind == Tokenizer.KW_IMPLIES && fixity == Operation.INFIX_TYPE:
        return OP_IMPLIES

    if token.kind == Tokenizer.KW_NIMPLIES && fixity == Operation.INFIX_TYPE:
        return OP_NIMPLIES

    if token.kind == Tokenizer.KW_IFF && fixity == Operation.INFIX_TYPE:
        return OP_IFF

    if token.kind == Tokenizer.KW_NIFF && fixity == Operation.INFIX_TYPE:
        return OP_NIFF

    if token.kind == Tokenizer.DOUBLE_AMPERSAND && fixity == Operation.INFIX_TYPE:
        return OP_LOGICAL_AND

    if token.kind == Tokenizer.BANG_DOUBLE_AMPERSAND && fixity == Operation.INFIX_TYPE:
        return OP_LOGICAL_NAND

    if token.kind == Tokenizer.CARET && fixity == Operation.INFIX_TYPE:
        return OP_LOGICAL_XOR

    if token.kind == Tokenizer.BANG_CARET && fixity == Operation.INFIX_TYPE:
        return OP_LOGICAL_XNOR

    if token.kind == Tokenizer.DOUBLE_PIPE && fixity == Operation.INFIX_TYPE:
        return OP_LOGICAL_OR

    if token.kind == Tokenizer.BANG_DOUBLE_PIPE && fixity == Operation.INFIX_TYPE:
        return OP_LOGICAL_NOR

    if token.kind == Tokenizer.ARROW && fixity == Operation.INFIX_TYPE:
        return OP_LOGICAL_IMPLIES

    if token.kind == Tokenizer.NOT_ARROW && fixity == Operation.INFIX_TYPE:
        return OP_LOGICAL_NIMPLIES

    if token.kind == Tokenizer.DOUBLE_ARROW && fixity == Operation.INFIX_TYPE:
        return OP_LOGICAL_IFF

    if token.kind == Tokenizer.BANG_DOUBLE_ARROW && fixity == Operation.INFIX_TYPE:
        return OP_LOGICAL_NIFF

    if token.kind == Tokenizer.EQUAL && fixity == Operation.INFIX_TYPE:
        return OP_ASSIGN

    if token.kind == Tokenizer.DOUBLE_STAR_EQUAL && fixity == Operation.INFIX_TYPE:
        return OP_POW_ASSIGN

    if token.kind == Tokenizer.STAR_EQUAL && fixity == Operation.INFIX_TYPE:
        return OP_TIMES_ASSIGN

    if token.kind == Tokenizer.SLASH_EQUAL && fixity == Operation.INFIX_TYPE:
        return OP_DIV_ASSIGN

    if token.kind == Tokenizer.PERCENT_EQUAL && fixity == Operation.INFIX_TYPE:
        return OP_REM_ASSIGN

    if token.kind == Tokenizer.PLUS_EQUAL && fixity == Operation.INFIX_TYPE:
        return OP_PLUS_ASSIGN

    if token.kind == Tokenizer.MINUS_EQUAL && fixity == Operation.INFIX_TYPE:
        return OP_MINUS_ASSIGN

    if token.kind == Tokenizer.DOUBLE_LESS_EQUAL && fixity == Operation.INFIX_TYPE:
        return OP_SHL_ASSIGN

    if token.kind == Tokenizer.DOUBLE_GREATER_EQUAL && fixity == Operation.INFIX_TYPE:
        return OP_SHR_ASSIGN

    if token.kind == Tokenizer.TRIPLE_LESS_EQUAL && fixity == Operation.INFIX_TYPE:
        return OP_USHL_ASSIGN

    if token.kind == Tokenizer.TRIPLE_GREATER_EQUAL && fixity == Operation.INFIX_TYPE:
        return OP_USHR_ASSIGN

    if token.kind == Tokenizer.AMPERSAND_EQUAL && fixity == Operation.INFIX_TYPE:
        return OP_BITWISE_AND_ASSIGN

    if token.kind == Tokenizer.BANG_AMPERSAND_EQUAL && fixity == Operation.INFIX_TYPE:
        return OP_BITWISE_NAND_ASSIGN

    if token.kind == Tokenizer.PIPE_EQUAL && fixity == Operation.INFIX_TYPE:
        return OP_BITWISE_OR_ASSIGN

    if token.kind == Tokenizer.BANG_PIPE_EQUAL && fixity == Operation.INFIX_TYPE:
        return OP_BITWISE_NOR_ASSIGN

    return null
}

private val EXPRESSION_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_NEW).pushRegex(Tokenizer.TK_IDENTIFIER).pushRef(EXPRESSION_TUPLE_PARSER), makeExprFromNewFunc, Rule.STARTER_ROLE, 250)
private val EXPRESSION_RULE1: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_NEW).pushRegex(Tokenizer.TK_IDENTIFIER), makeExprFromNewIdent, Rule.STARTER_ROLE, 240)
private val EXPRESSION_RULE2: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.TK_IDENTIFIER).pushRef(EXPRESSION_TUPLE_PARSER), makeExprFromFuncCall, Rule.STARTER_ROLE, 240)
private val EXPRESSION_RULE3: pointer<Rule> = new Rule(new PatternList().pushRef(IF_ELSE_BRANCH_PARSER), makeExprFromIfElseBranch, Rule.STARTER_ROLE, 230)
private val EXPRESSION_RULE4: pointer<Rule> = new Rule(new PatternList().pushRef(IF_BRANCH_PARSER), makeExprFromIfBranch, Rule.STARTER_ROLE, 230)
private val EXPRESSION_RULE5: pointer<Rule> = new Rule(new PatternList().pushRef(BLOCK_EXPR_PARSER), makeExprFromBlockExpr, Rule.STARTER_ROLE, 230)
private val EXPRESSION_RULE6: pointer<Rule> = new Rule(new PatternList().pushRef(ATOM_PARSER), makeExprFromAtom, Rule.STARTER_ROLE, 230)
private val EXPRESSION_RULE7: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_PAREN).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.RIGHT_PAREN), makeExprFromParen, Rule.STARTER_ROLE, OP_PAREN)
private val EXPRESSION_RULE8: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.PLUS).pushRef(EXPRESSION_PARSER), makeExprFromPrefix, Rule.STARTER_ROLE, OP_POS)
private val EXPRESSION_RULE9: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.MINUS).pushRef(EXPRESSION_PARSER), makeExprFromPrefix, Rule.STARTER_ROLE, OP_NEG)
private val EXPRESSION_RULE10: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.DOUBLE_PLUS).pushRef(EXPRESSION_PARSER), makeExprFromPrefix, Rule.STARTER_ROLE, OP_INC)
private val EXPRESSION_RULE11: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.DOUBLE_MINUS).pushRef(EXPRESSION_PARSER), makeExprFromPrefix, Rule.STARTER_ROLE, OP_DEC)
private val EXPRESSION_RULE12: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_INV).pushRef(EXPRESSION_PARSER), makeExprFromPrefix, Rule.STARTER_ROLE, OP_INV)
private val EXPRESSION_RULE13: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.BANG).pushRef(EXPRESSION_PARSER), makeExprFromPrefix, Rule.STARTER_ROLE, OP_NOT)
private val EXPRESSION_RULE14: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRef(LIST_LITERAL_PARSER), makeExprFromIndexAccess, Rule.CONTINUATION_ROLE, 230)
private val EXPRESSION_RULE15: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOT).pushRegex(Tokenizer.TK_IDENTIFIER).pushRef(EXPRESSION_TUPLE_PARSER), makeExprFromMethodCall, Rule.CONTINUATION_ROLE, 230)
private val EXPRESSION_RULE16: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOT).pushRegex(Tokenizer.TK_IDENTIFIER), makeExprFromFieldAccess, Rule.CONTINUATION_ROLE, 220)
private val EXPRESSION_RULE17: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOUBLE_PLUS), makeExprFromPostfix, Rule.CONTINUATION_ROLE, OP_SUCC)
private val EXPRESSION_RULE18: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOUBLE_MINUS), makeExprFromPostfix, Rule.CONTINUATION_ROLE, OP_PRED)
private val EXPRESSION_RULE19: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_AS).pushRef(TYPE_PARSER), makeExprFromTypeCast, Rule.CONTINUATION_ROLE, 200)
private val EXPRESSION_RULE20: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOUBLE_STAR).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_POW)
private val EXPRESSION_RULE21: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.STAR).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_TIMES)
private val EXPRESSION_RULE22: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.SLASH).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_DIV)
private val EXPRESSION_RULE23: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.PERCENT).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_REM)
private val EXPRESSION_RULE24: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.PLUS).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_PLUS)
private val EXPRESSION_RULE25: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.MINUS).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_MINUS)
private val EXPRESSION_RULE26: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_SHL).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_SHL)
private val EXPRESSION_RULE27: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_SHR).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_SHR)
private val EXPRESSION_RULE28: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_USHR).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_USHR)
private val EXPRESSION_RULE29: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.GREATER).pushRef(EXPRESSION_PARSER), makeExprFromCompare, Rule.CONTINUATION_ROLE, OP_GREATER)
private val EXPRESSION_RULE30: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.LESS).pushRef(EXPRESSION_PARSER), makeExprFromCompare, Rule.CONTINUATION_ROLE, OP_LESS)
private val EXPRESSION_RULE31: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.GREATER_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromCompare, Rule.CONTINUATION_ROLE, OP_GREATER_EQUAL)
private val EXPRESSION_RULE32: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.LESS_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromCompare, Rule.CONTINUATION_ROLE, OP_LESS_EQUAL)
private val EXPRESSION_RULE33: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.TRIPLE_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_REF_EQUALS)
private val EXPRESSION_RULE34: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOUBLE_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_EQUALS)
private val EXPRESSION_RULE35: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.BANG_DOUBLE_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromNotRefEqual, Rule.CONTINUATION_ROLE, OP_REF_EQUALS)
private val EXPRESSION_RULE36: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.NOT_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromNotEqual, Rule.CONTINUATION_ROLE, OP_EQUALS)
private val EXPRESSION_RULE37: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_AND).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_BITWISE_AND)
private val EXPRESSION_RULE38: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_NAND).pushRef(EXPRESSION_PARSER), makeExprFromBitwiseNand, Rule.CONTINUATION_ROLE, OP_BITWISE_NAND)
private val EXPRESSION_RULE39: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_XOR).pushRef(EXPRESSION_PARSER), makeExprFromBitwiseXor, Rule.CONTINUATION_ROLE, OP_BITWISE_XOR)
private val EXPRESSION_RULE40: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_XNOR).pushRef(EXPRESSION_PARSER), makeExprFromBitwiseXnor, Rule.CONTINUATION_ROLE, OP_BITWISE_XNOR)
private val EXPRESSION_RULE41: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_OR).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_BITWISE_OR)
private val EXPRESSION_RULE42: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_NOR).pushRef(EXPRESSION_PARSER), makeExprFromBitwiseNor, Rule.CONTINUATION_ROLE, OP_BITWISE_NOR)
private val EXPRESSION_RULE43: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_IMPLIES).pushRef(EXPRESSION_PARSER), makeExprFromImplies, Rule.CONTINUATION_ROLE, OP_IMPLIES)
private val EXPRESSION_RULE44: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_NIMPLIES).pushRef(EXPRESSION_PARSER), makeExprFromNimplies, Rule.CONTINUATION_ROLE, OP_NIMPLIES)
private val EXPRESSION_RULE45: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_IFF).pushRef(EXPRESSION_PARSER), makeExprFromIff, Rule.CONTINUATION_ROLE, OP_IFF)
private val EXPRESSION_RULE46: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_NIFF).pushRef(EXPRESSION_PARSER), makeExprFromNiff, Rule.CONTINUATION_ROLE, OP_NIFF)
private val EXPRESSION_RULE47: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOUBLE_AMPERSAND).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_LOGICAL_AND)
private val EXPRESSION_RULE48: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.BANG_DOUBLE_AMPERSAND).pushRef(EXPRESSION_PARSER), makeExprFromLogicalNand, Rule.CONTINUATION_ROLE, OP_LOGICAL_NAND)
private val EXPRESSION_RULE49: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.CARET).pushRef(EXPRESSION_PARSER), makeExprFromLogicalXor, Rule.CONTINUATION_ROLE, OP_LOGICAL_XOR)
private val EXPRESSION_RULE50: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.BANG_CARET).pushRef(EXPRESSION_PARSER), makeExprFromLogicalXnor, Rule.CONTINUATION_ROLE, OP_LOGICAL_XNOR)
private val EXPRESSION_RULE51: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOUBLE_PIPE).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_LOGICAL_OR)
private val EXPRESSION_RULE52: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.BANG_DOUBLE_PIPE).pushRef(EXPRESSION_PARSER), makeExprFromLogicalNor, Rule.CONTINUATION_ROLE, OP_LOGICAL_NOR)
private val EXPRESSION_RULE53: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.ARROW).pushRef(EXPRESSION_PARSER), makeExprFromLogicalImplies, Rule.CONTINUATION_ROLE, OP_LOGICAL_IMPLIES)
private val EXPRESSION_RULE54: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.NOT_ARROW).pushRef(EXPRESSION_PARSER), makeExprFromLogicalNimplies, Rule.CONTINUATION_ROLE, OP_LOGICAL_NIMPLIES)
private val EXPRESSION_RULE55: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOUBLE_ARROW).pushRef(EXPRESSION_PARSER), makeExprFromLogicalIff, Rule.CONTINUATION_ROLE, OP_LOGICAL_IFF)
private val EXPRESSION_RULE56: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.BANG_DOUBLE_ARROW).pushRef(EXPRESSION_PARSER), makeExprFromLogicalNiff, Rule.CONTINUATION_ROLE, OP_LOGICAL_NIFF)
private val EXPRESSION_RULE57: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromAssign, Rule.CONTINUATION_ROLE, OP_ASSIGN)
private val EXPRESSION_RULE58: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOUBLE_STAR_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromPowAssign, Rule.CONTINUATION_ROLE, OP_POW_ASSIGN)
private val EXPRESSION_RULE59: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.STAR_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromTimesAssign, Rule.CONTINUATION_ROLE, OP_TIMES_ASSIGN)
private val EXPRESSION_RULE60: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.SLASH_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromDivAssign, Rule.CONTINUATION_ROLE, OP_DIV_ASSIGN)
private val EXPRESSION_RULE61: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.PERCENT_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromRemAssign, Rule.CONTINUATION_ROLE, OP_REM_ASSIGN)
private val EXPRESSION_RULE62: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.PLUS_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromPlusAssign, Rule.CONTINUATION_ROLE, OP_PLUS_ASSIGN)
private val EXPRESSION_RULE63: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.MINUS_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromMinusAssign, Rule.CONTINUATION_ROLE, OP_MINUS_ASSIGN)
private val EXPRESSION_RULE64: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOUBLE_LESS_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromShlAssign, Rule.CONTINUATION_ROLE, OP_SHL_ASSIGN)
private val EXPRESSION_RULE65: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOUBLE_GREATER_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromShrAssign, Rule.CONTINUATION_ROLE, OP_SHR_ASSIGN)
private val EXPRESSION_RULE66: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.TRIPLE_LESS_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromUshlAssign, Rule.CONTINUATION_ROLE, OP_USHL_ASSIGN)
private val EXPRESSION_RULE67: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.TRIPLE_GREATER_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromUshrAssign, Rule.CONTINUATION_ROLE, OP_USHR_ASSIGN)
private val EXPRESSION_RULE68: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.AMPERSAND_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromBitwiseAndAssign, Rule.CONTINUATION_ROLE, OP_BITWISE_AND_ASSIGN)
private val EXPRESSION_RULE69: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.BANG_AMPERSAND_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromBitwiseNandAssign, Rule.CONTINUATION_ROLE, OP_BITWISE_NAND_ASSIGN)
private val EXPRESSION_RULE70: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.PIPE_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromBitwiseOrAssign, Rule.CONTINUATION_ROLE, OP_BITWISE_OR_ASSIGN)
private val EXPRESSION_RULE71: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.BANG_PIPE_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromBitwiseNorAssign, Rule.CONTINUATION_ROLE, OP_BITWISE_NOR_ASSIGN)

private val ATOM_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_NULL), makeAtom, Rule.STARTER_ROLE, 0)
private val ATOM_RULE1: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_TRUE), makeAtom, Rule.STARTER_ROLE, 0)
private val ATOM_RULE2: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_FALSE), makeAtom, Rule.STARTER_ROLE, 0)
private val ATOM_RULE3: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.TK_CHAR), makeAtom, Rule.STARTER_ROLE, 0)
private val ATOM_RULE4: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.TK_STRING), makeAtom, Rule.STARTER_ROLE, 0)
private val ATOM_RULE5: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.TK_INTEGER), makeAtom, Rule.STARTER_ROLE, 0)
private val ATOM_RULE6: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.TK_LONG), makeAtom, Rule.STARTER_ROLE, 0)
private val ATOM_RULE7: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.TK_FLOAT), makeAtom, Rule.STARTER_ROLE, 0)
private val ATOM_RULE8: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.TK_DOUBLE), makeAtom, Rule.STARTER_ROLE, 0)
private val ATOM_RULE9: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.TK_LONG_DOUBLE), makeAtom, Rule.STARTER_ROLE, 0)
private val ATOM_RULE10: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.TK_IDENTIFIER), makeAtom, Rule.STARTER_ROLE, 0)

private val EXPRESSIONS_RULE0: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.COMMA).pushRef(EXPRESSIONS_PARSER), makeExprsIt, Rule.STARTER_ROLE, 0)
private val EXPRESSIONS_RULE1: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER), makeSingleExprs, Rule.STARTER_ROLE, 0)

private val EXPRESSION_TUPLE_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_PAREN).pushRegex(Tokenizer.RIGHT_PAREN), makeExpressionTuple0, Rule.STARTER_ROLE, 0)
private val EXPRESSION_TUPLE_RULE1: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_PAREN).pushRef(EXPRESSIONS_PARSER).pushRegex(Tokenizer.RIGHT_PAREN), makeExpressionTuple1, Rule.STARTER_ROLE, 0)
private val EXPRESSION_TUPLE_RULE2: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_PAREN).pushRef(EXPRESSIONS_PARSER).pushRegex(Tokenizer.COMMA).pushRegex(Tokenizer.RIGHT_PAREN), makeExpressionTuple2, Rule.STARTER_ROLE, 0)

private val LIST_LITERAL_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_BRACKET).pushRegex(Tokenizer.RIGHT_BRACKET), makeListLiteral0, Rule.STARTER_ROLE, 0)
private val LIST_LITERAL_RULE1: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_BRACKET).pushRef(EXPRESSIONS_PARSER).pushRegex(Tokenizer.RIGHT_BRACKET), makeListLiteral1, Rule.STARTER_ROLE, 0)
private val LIST_LITERAL_RULE2: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_BRACKET).pushRef(EXPRESSIONS_PARSER).pushRegex(Tokenizer.COMMA).pushRegex(Tokenizer.RIGHT_BRACKET), makeListLiteral2, Rule.STARTER_ROLE, 0)

private val STATEMENT_RULE0: pointer<Rule> = new Rule(new PatternList().pushRef(EXPR_STATEMENT_PARSER), makeStmtFrom_ExprStatement, Rule.STARTER_ROLE, 0)
private val STATEMENT_RULE1: pointer<Rule> = new Rule(new PatternList().pushRef(EXPR_LIST_STATEMENT_PARSER), makeStmtFrom_ExprListStatement, Rule.STARTER_ROLE, 0)
private val STATEMENT_RULE2: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_VAR).pushRef(VARIABLE_DEFINE_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeStmtFrom_VariableDefine, Rule.STARTER_ROLE, 0)
private val STATEMENT_RULE3: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_VAL).pushRef(VARIABLE_DEFINE_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeStmtFrom_VariableDefine, Rule.STARTER_ROLE, 0)
private val STATEMENT_RULE4: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_VAR).pushRef(VARIABLE_DEFINES_PARSER), makeStmtFrom_VariableDefines, Rule.STARTER_ROLE, 0)
private val STATEMENT_RULE5: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_VAL).pushRef(VARIABLE_DEFINES_PARSER), makeStmtFrom_VariableDefines, Rule.STARTER_ROLE, 0)
private val STATEMENT_RULE6: pointer<Rule> = new Rule(new PatternList().pushRef(RETURN_STATEMENT_PARSER), makeStmtFrom_ReturnStmt, Rule.STARTER_ROLE, 0)

private val STATEMENTS_RULE0: pointer<Rule> = new Rule(new PatternList().pushRef(STATEMENT_PARSER).pushRef(STATEMENTS_PARSER), makeStmtsIt, Rule.STARTER_ROLE, 0)
private val STATEMENTS_RULE1: pointer<Rule> = new Rule(new PatternList().pushRef(STATEMENT_PARSER), makeSingleStmts, Rule.STARTER_ROLE, 0)

private val EXPR_STATEMENT_RULE0: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeExprStmt, Rule.STARTER_ROLE, 0)

private val EXPR_LIST_STATEMENT_RULE0: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.COMMA).pushRef(EXPR_LIST_STATEMENT_PARSER), makeExprList, Rule.STARTER_ROLE, 0)
private val EXPR_LIST_STATEMENT_RULE1: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeSingleExprList, Rule.STARTER_ROLE, 0)

private val VARIABLE_DEFINE_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.TK_IDENTIFIER).pushRegex(Tokenizer.EQUAL).pushRef(EXPRESSION_PARSER), makeVariableDefine, Rule.STARTER_ROLE, 0)
private val VARIABLE_DEFINE_RULE1: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.TK_IDENTIFIER).pushRegex(Tokenizer.COLON).pushRef(TYPE_PARSER).pushRegex(Tokenizer.EQUAL).pushRef(EXPRESSION_PARSER), makeVariableDefineWithType, Rule.STARTER_ROLE, 0)

private val VARIABLE_DEFINES_RULE0: pointer<Rule> = new Rule(new PatternList().pushRef(VARIABLE_DEFINE_PARSER).pushRegex(Tokenizer.COMMA).pushRef(VARIABLE_DEFINES_PARSER), makeVarDefsIt, Rule.STARTER_ROLE, 0)
private val VARIABLE_DEFINES_RULE1: pointer<Rule> = new Rule(new PatternList().pushRef(VARIABLE_DEFINE_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeVariableDefines, Rule.STARTER_ROLE, 0)

private val RETURN_STATEMENT_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_RETURN).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeReturnStmt, Rule.STARTER_ROLE, 0)
private val RETURN_STATEMENT_RULE1: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_RETURN).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeReturnStmt, Rule.STARTER_ROLE, 0)

private val BLOCK_EXPR_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_BRACE).pushRegex(Tokenizer.RIGHT_BRACE), makeEmptyBlockExpr, Rule.STARTER_ROLE, 0)
private val BLOCK_EXPR_RULE1: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_BRACE).pushRef(STATEMENTS_PARSER).pushRegex(Tokenizer.RIGHT_BRACE), makeBlockExpr, Rule.STARTER_ROLE, 0)

private val IF_BRANCH_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_IF).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.COLON).pushRef(BLOCK_EXPR_PARSER), makeIfBranchFromStmts, Rule.STARTER_ROLE, 0)
private val IF_BRANCH_RULE1: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_IF).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.COLON).pushRef(STATEMENT_PARSER), makeIfBranchFromStmt, Rule.STARTER_ROLE, 0).setAfterFun(prependLineTerminator)

private val IF_ELSE_BRANCH_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_IF).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.COLON).pushRef(STATEMENT_PARSER).pushRegex(Tokenizer.KW_ELSE).pushRegex(Tokenizer.COLON).pushRef(STATEMENT_PARSER), makeIfElseBranchFromSS, Rule.STARTER_ROLE, 0).setAfterFun(prependLineTerminator)
private val IF_ELSE_BRANCH_RULE1: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_IF).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.COLON).pushRef(BLOCK_EXPR_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR).pushRegex(Tokenizer.KW_ELSE).pushRegex(Tokenizer.COLON).pushRef(STATEMENT_PARSER), makeIfElseBranchFromBS, Rule.STARTER_ROLE, 0).setAfterFun(prependLineTerminator)
private val IF_ELSE_BRANCH_RULE2: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_IF).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.COLON).pushRef(STATEMENT_PARSER).pushRegex(Tokenizer.KW_ELSE).pushRegex(Tokenizer.COLON).pushRef(BLOCK_EXPR_PARSER), makeIfElseBranchFromSB, Rule.STARTER_ROLE, 0)
private val IF_ELSE_BRANCH_RULE3: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_IF).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.COLON).pushRef(BLOCK_EXPR_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR).pushRegex(Tokenizer.KW_ELSE).pushRegex(Tokenizer.COLON).pushRef(BLOCK_EXPR_PARSER), makeIfElseBranchFromBB, Rule.STARTER_ROLE, 0)

private val EXPRESSION_PARSER_SETUP: pointer<ParserRef> = EXPRESSION_PARSER.addRule(EXPRESSION_RULE0).addRule(EXPRESSION_RULE1).addRule(EXPRESSION_RULE2).addRule(EXPRESSION_RULE3).addRule(EXPRESSION_RULE4).addRule(EXPRESSION_RULE5).addRule(EXPRESSION_RULE6).addRule(EXPRESSION_RULE7).addRule(EXPRESSION_RULE8).addRule(EXPRESSION_RULE9).addRule(EXPRESSION_RULE10).addRule(EXPRESSION_RULE11).addRule(EXPRESSION_RULE12).addRule(EXPRESSION_RULE13).addRule(EXPRESSION_RULE14).addRule(EXPRESSION_RULE15).addRule(EXPRESSION_RULE16).addRule(EXPRESSION_RULE17).addRule(EXPRESSION_RULE18).addRule(EXPRESSION_RULE19).addRule(EXPRESSION_RULE20).addRule(EXPRESSION_RULE21).addRule(EXPRESSION_RULE22).addRule(EXPRESSION_RULE23).addRule(EXPRESSION_RULE24).addRule(EXPRESSION_RULE25).addRule(EXPRESSION_RULE26).addRule(EXPRESSION_RULE27).addRule(EXPRESSION_RULE28).addRule(EXPRESSION_RULE29).addRule(EXPRESSION_RULE30).addRule(EXPRESSION_RULE31).addRule(EXPRESSION_RULE32).addRule(EXPRESSION_RULE33).addRule(EXPRESSION_RULE34).addRule(EXPRESSION_RULE35).addRule(EXPRESSION_RULE36).addRule(EXPRESSION_RULE37).addRule(EXPRESSION_RULE38).addRule(EXPRESSION_RULE39).addRule(EXPRESSION_RULE40).addRule(EXPRESSION_RULE41).addRule(EXPRESSION_RULE42).addRule(EXPRESSION_RULE43).addRule(EXPRESSION_RULE44).addRule(EXPRESSION_RULE45).addRule(EXPRESSION_RULE46).addRule(EXPRESSION_RULE47).addRule(EXPRESSION_RULE48).addRule(EXPRESSION_RULE49).addRule(EXPRESSION_RULE50).addRule(EXPRESSION_RULE51).addRule(EXPRESSION_RULE52).addRule(EXPRESSION_RULE53).addRule(EXPRESSION_RULE54).addRule(EXPRESSION_RULE55).addRule(EXPRESSION_RULE56).addRule(EXPRESSION_RULE57).addRule(EXPRESSION_RULE58).addRule(EXPRESSION_RULE59).addRule(EXPRESSION_RULE60).addRule(EXPRESSION_RULE61).addRule(EXPRESSION_RULE62).addRule(EXPRESSION_RULE63).addRule(EXPRESSION_RULE64).addRule(EXPRESSION_RULE65).addRule(EXPRESSION_RULE66).addRule(EXPRESSION_RULE67).addRule(EXPRESSION_RULE68).addRule(EXPRESSION_RULE69).addRule(EXPRESSION_RULE70).addRule(EXPRESSION_RULE71)
private val ATOM_PARSER_SETUP: pointer<ParserRef> = ATOM_PARSER.addRule(ATOM_RULE0).addRule(ATOM_RULE1).addRule(ATOM_RULE2).addRule(ATOM_RULE3).addRule(ATOM_RULE4).addRule(ATOM_RULE5).addRule(ATOM_RULE6).addRule(ATOM_RULE7).addRule(ATOM_RULE8).addRule(ATOM_RULE9).addRule(ATOM_RULE10)
private val EXPRESSIONS_PARSER_SETUP: pointer<ParserRef> = EXPRESSIONS_PARSER.addRule(EXPRESSIONS_RULE0).addRule(EXPRESSIONS_RULE1)
private val EXPRESSION_TUPLE_PARSER_SETUP: pointer<ParserRef> = EXPRESSION_TUPLE_PARSER.addRule(EXPRESSION_TUPLE_RULE0).addRule(EXPRESSION_TUPLE_RULE1).addRule(EXPRESSION_TUPLE_RULE2)
private val LIST_LITERAL_PARSER_SETUP: pointer<ParserRef> = LIST_LITERAL_PARSER.addRule(LIST_LITERAL_RULE0).addRule(LIST_LITERAL_RULE1).addRule(LIST_LITERAL_RULE2)
private val STATEMENT_PARSER_SETUP: pointer<ParserRef> = STATEMENT_PARSER.addRule(STATEMENT_RULE0).addRule(STATEMENT_RULE1).addRule(STATEMENT_RULE2).addRule(STATEMENT_RULE3).addRule(STATEMENT_RULE4).addRule(STATEMENT_RULE5).addRule(STATEMENT_RULE6)
private val STATEMENTS_PARSER_SETUP: pointer<ParserRef> = STATEMENTS_PARSER.addRule(STATEMENTS_RULE0).addRule(STATEMENTS_RULE1)
private val EXPR_STATEMENT_PARSER_SETUP: pointer<ParserRef> = EXPR_STATEMENT_PARSER.addRule(EXPR_STATEMENT_RULE0)
private val EXPR_LIST_STATEMENT_PARSER_SETUP: pointer<ParserRef> = EXPR_LIST_STATEMENT_PARSER.addRule(EXPR_LIST_STATEMENT_RULE0).addRule(EXPR_LIST_STATEMENT_RULE1)
private val VARIABLE_DEFINE_PARSER_SETUP: pointer<ParserRef> = VARIABLE_DEFINE_PARSER.addRule(VARIABLE_DEFINE_RULE0).addRule(VARIABLE_DEFINE_RULE1)
private val VARIABLE_DEFINES_PARSER_SETUP: pointer<ParserRef> = VARIABLE_DEFINES_PARSER.addRule(VARIABLE_DEFINES_RULE0).addRule(VARIABLE_DEFINES_RULE1)
private val RETURN_STATEMENT_PARSER_SETUP: pointer<ParserRef> = RETURN_STATEMENT_PARSER.addRule(RETURN_STATEMENT_RULE0).addRule(RETURN_STATEMENT_RULE1)
private val BLOCK_EXPR_PARSER_SETUP: pointer<ParserRef> = BLOCK_EXPR_PARSER.addRule(BLOCK_EXPR_RULE0).addRule(BLOCK_EXPR_RULE1)
private val IF_BRANCH_PARSER_SETUP: pointer<ParserRef> = IF_BRANCH_PARSER.addRule(IF_BRANCH_RULE0).addRule(IF_BRANCH_RULE1)
private val IF_ELSE_BRANCH_PARSER_SETUP: pointer<ParserRef> = IF_ELSE_BRANCH_PARSER.addRule(IF_ELSE_BRANCH_RULE0).addRule(IF_ELSE_BRANCH_RULE1).addRule(IF_ELSE_BRANCH_RULE2).addRule(IF_ELSE_BRANCH_RULE3)


fun parseExpression(input: pointer<TokenList>) -> pointer<Expression>
{
    if input == null:
        return null

    if EXPRESSION_PARSER_SETUP.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = EXPRESSION_PARSER_SETUP.getResult()

    if result == null || result.isKind(EXPRESSION_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<Expression>
}

fun parseAtom(input: pointer<TokenList>) -> pointer<Atom>
{
    if input == null:
        return null

    if ATOM_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = ATOM_PARSER.getResult()

    if result == null || result.isKind(ATOM_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<Atom>
}

fun parseExpressions(input: pointer<TokenList>) -> pointer<Expressions>
{
    if input == null:
        return null

    if EXPRESSIONS_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = EXPRESSIONS_PARSER.getResult()

    if result == null || result.isKind(EXPRESSIONS_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<Expressions>
}

fun parseExpressionTuple(input: pointer<TokenList>) -> pointer<ExpressionTuple>
{
    if input == null:
        return null

    if EXPRESSION_TUPLE_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = EXPRESSION_TUPLE_PARSER.getResult()

    if result == null || result.isKind(EXPRESSION_TUPLE_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<ExpressionTuple>
}

fun parseListLiteral(input: pointer<TokenList>) -> pointer<ListLiteral>
{
    if input == null:
        return null

    if LIST_LITERAL_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = LIST_LITERAL_PARSER.getResult()

    if result == null || result.isKind(LIST_LITERAL_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<ListLiteral>
}

fun parseStatement(input: pointer<TokenList>) -> pointer<Statement>
{
    if input == null:
        return null

    if STATEMENT_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = STATEMENT_PARSER.getResult()

    if result == null || result.isKind(STATEMENT_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<Statement>
}

fun parseStatements(input: pointer<TokenList>) -> pointer<Statements>
{
    if input == null:
        return null

    if STATEMENTS_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = STATEMENTS_PARSER.getResult()

    if result == null || result.isKind(STATEMENTS_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<Statements>
}

fun parseExprStatement(input: pointer<TokenList>) -> pointer<ExprStatement>
{
    if input == null:
        return null

    if EXPR_STATEMENT_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = EXPR_STATEMENT_PARSER.getResult()

    if result == null || result.isKind(EXPR_STATEMENT_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<ExprStatement>
}

fun parseExprListStatement(input: pointer<TokenList>) -> pointer<ExprListStatement>
{
    if input == null:
        return null

    if EXPR_LIST_STATEMENT_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = EXPR_LIST_STATEMENT_PARSER.getResult()

    if result == null || result.isKind(EXPR_LIST_STATEMENT_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<ExprListStatement>
}

fun parseVariableDefine(input: pointer<TokenList>) -> pointer<VariableDefine>
{
    if input == null:
        return null

    if VARIABLE_DEFINE_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = VARIABLE_DEFINE_PARSER.getResult()

    if result == null || result.isKind(VARIABLE_DEFINE_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<VariableDefine>
}

fun parseVariableDefines(input: pointer<TokenList>) -> pointer<VariableDefines>
{
    if input == null:
        return null

    if VARIABLE_DEFINES_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = VARIABLE_DEFINES_PARSER.getResult()

    if result == null || result.isKind(VARIABLE_DEFINES_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<VariableDefines>
}

fun parseReturnStatement(input: pointer<TokenList>) -> pointer<ReturnStatement>
{
    if input == null:
        return null

    if RETURN_STATEMENT_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = RETURN_STATEMENT_PARSER.getResult()

    if result == null || result.isKind(RETURN_STATEMENT_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<ReturnStatement>
}

fun parseBlockExpr(input: pointer<TokenList>) -> pointer<BlockExpr>
{
    if input == null:
        return null

    if BLOCK_EXPR_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = BLOCK_EXPR_PARSER.getResult()

    if result == null || result.isKind(BLOCK_EXPR_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<BlockExpr>
}

fun parseIfBranch(input: pointer<TokenList>) -> pointer<IfBranch>
{
    if input == null:
        return null

    if IF_BRANCH_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = IF_BRANCH_PARSER.getResult()

    if result == null || result.isKind(IF_BRANCH_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<IfBranch>
}

fun parseIfElseBranch(input: pointer<TokenList>) -> pointer<IfElseBranch>
{
    if input == null:
        return null

    if IF_ELSE_BRANCH_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = IF_ELSE_BRANCH_PARSER.getResult()

    if result == null || result.isKind(IF_ELSE_BRANCH_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<IfElseBranch>
}


