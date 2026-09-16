#file.outerClass("Parser")
package xlang.compiler.parser


import xlang.Operation
import xlang.compiler.Type
import xlang.compiler.lexer.Tokenizer
import xlang.compiler.parser.expression.Assignment
import xlang.compiler.parser.expression.Atom
import xlang.compiler.parser.expression.Expression
import xlang.compiler.parser.expression.ExpressionDesugar
import xlang.compiler.parser.expression.ExpressionTuple
import xlang.compiler.parser.expression.FieldAccess
import xlang.compiler.parser.expression.IndexAccess
import xlang.compiler.parser.expression.ListLiteral
import xlang.compiler.parser.expression.MethodCall
import xlang.compiler.parser.expression.NewFunction
import xlang.compiler.parser.expression.NewIdentifier
import xlang.compiler.parser.expression.TypeCast
import xlang.compiler.parser.expression.intrisic.SizeOf
import xlang.compiler.parser.program.Annotation
import xlang.compiler.parser.program.AnnotationsMaybe
import xlang.compiler.parser.program.Field
import xlang.compiler.parser.program.Function
import xlang.compiler.parser.program.FunctionParam
import xlang.compiler.parser.program.FunctionParams
import xlang.compiler.parser.program.FunctionParamsMaybe
import xlang.compiler.parser.program.ImportDeclaration
import xlang.compiler.parser.program.ImportDeclarationsMaybe
import xlang.compiler.parser.program.Member
import xlang.compiler.parser.program.Modifier
import xlang.compiler.parser.program.ModifierListMaybe
import xlang.compiler.parser.program.NamespaceImport
import xlang.compiler.parser.program.PackageDeclaration
import xlang.compiler.parser.program.PackageDeclarationMaybe
import xlang.compiler.parser.program.PreprocessSetting
import xlang.compiler.parser.program.PreprocessSettingsMaybe
import xlang.compiler.parser.program.Program
import xlang.compiler.parser.program.QualifiedName
import xlang.compiler.parser.program.Struct
import xlang.compiler.parser.program.StructConstructor
import xlang.compiler.parser.statement.BreakStatement
import xlang.compiler.parser.statement.ContinueStatement
import xlang.compiler.parser.statement.ElseStatement
import xlang.compiler.parser.statement.ExprListStatement
import xlang.compiler.parser.statement.ExprStatement
import xlang.compiler.parser.statement.ForHeader
import xlang.compiler.parser.statement.ForStatement
import xlang.compiler.parser.statement.PassStatement
import xlang.compiler.parser.statement.ReturnStatement
import xlang.compiler.parser.statement.Statement
import xlang.compiler.parser.statement.VariableDefine
import xlang.compiler.parser.statement.VariableDefines
import xlang.compiler.parser.statement.WhileStatement
import xlang.compiler.parser.stmtexpr.Block
import xlang.compiler.parser.stmtexpr.IfElseExpression
import xlang.compiler.parser.stmtexpr.IfExpression
import xlang.compiler.parser.stmtexpr.StatementExpression
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.lexer.TokenPosition
import xlang.parser.ParseContainer
import xlang.parser.PrattParser
import xlang.parser.util.ParserRef
import xlang.parser.util.ParserRefs
import xlang.parser.util.PatternAtom
import xlang.parser.util.PatternList
import xlang.parser.util.Rule
import xlang.util.ArrayList


val TYPE_PARSER: pointer<ParserRef> = ParserRef.fromType(1000)


private val EXPRESSION_PARSER_ID: int = 1
private val ATOM_PARSER_ID: int = 2
private val SIZE_OF_PARSER_ID: int = 3
private val EXPRESSION_TUPLE_PARSER_ID: int = 4
private val LIST_LITERAL_PARSER_ID: int = 5
private val STATEMENT_PARSER_ID: int = 6
private val ELSE_STATEMENT_PARSER_ID: int = 7
private val WHILE_STATEMENT_PARSER_ID: int = 8
private val FOR_HEADER_PARSER_ID: int = 9
private val FOR_STATEMENT_PARSER_ID: int = 10
private val VARIABLE_DEFINE_PARSER_ID: int = 11
private val RETURN_STATEMENT_PARSER_ID: int = 12
private val BLOCK_PARSER_ID: int = 13
private val IF_EXPRESSION_PARSER_ID: int = 14
private val IF_ELSE_EXPRESSION_PARSER_ID: int = 15
private val MODIFIER_PARSER_ID: int = 16
private val MODIFIER_LIST_MAYBE_PARSER_ID: int = 17
private val PREPROCESS_SETTING_PARSER_ID: int = 18
private val PREPROCESS_SETTINGS_MAYBE_PARSER_ID: int = 19
private val ANNOTATION_PARSER_ID: int = 20
private val ANNOTATIONS_MAYBE_PARSER_ID: int = 21
private val QUALIFIED_NAME_PARSER_ID: int = 22
private val PACKAGE_DECLARATION_MAYBE_PARSER_ID: int = 23
private val NAMESPACE_IMPORT_PARSER_ID: int = 24
private val IMPORT_DECLARATION_PARSER_ID: int = 25
private val IMPORT_DECLARATIONS_MAYBE_PARSER_ID: int = 26
private val FUNCTION_PARAM_PARSER_ID: int = 27
private val FUNCTION_PARAMS_PARSER_ID: int = 28
private val FUNCTION_PARAMS_MAYBE_PARSER_ID: int = 29
private val FIELD_PARSER_ID: int = 30
private val FUNCTION_PARSER_ID: int = 31
private val STRUCT_CONSTRUCTOR_PARSER_ID: int = 32
private val MEMBER_PARSER_ID: int = 33
private val STRUCT_PARSER_ID: int = 34
private val PROGRAM_PARSER_ID: int = 35


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

private fun makeExprFromSizeOf(results: pointer<ArrayList>) -> pointer<*>
{
    val sizeOf: pointer<SizeOf> = getContainerValue(results, 0) as pointer<SizeOf>
    return sizeOf.toExpression()
}

private fun makeExprFromBlockExpr(results: pointer<ArrayList>) -> pointer<*>
{
    val block: pointer<Block> = getContainerValue(results, 0) as pointer<Block>
    return Expression.fromBlockExpr(block)
}

private fun makeExprFromIfExpr(results: pointer<ArrayList>) -> pointer<*>
{
    val branch: pointer<IfExpression> = getContainerValue(results, 0) as pointer<IfExpression>
    return Expression.fromIfElseExpr(branch.toIfElseExpr())
}

private fun makeExprFromIfElseExpr(results: pointer<ArrayList>) -> pointer<*>
{
    val branch: pointer<IfElseExpression> = getContainerValue(results, 0) as pointer<IfElseExpression>
    return Expression.fromIfElseExpr(branch)
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

    if targetType != null && targetType.isPrimary():
    {
        val functionName: pointer<char> = targetType.getFunction()

        if functionName != null:
        {
            val call: pointer<MethodCall> = new MethodCall(null, functionName).addArgument(expression)
            val result: pointer<Expression> = Expression.fromMethodCall(call).addExtraToken(asToken)
            val typeTokens: pointer<ArrayList> = targetType.getAllTokens()

            if typeTokens != null:
            {
                for (var i = 0; i < typeTokens.length; i++):
                    result.addExtraToken(typeTokens.get(i) as pointer<Token>)
            }

            return result
        }
    }

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

private fun makeSizeOf(results: pointer<ArrayList>) -> pointer<*>
{
    val sizeOfToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val leftParen: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val targetType: pointer<Type> = getContainerValue(results, 2) as pointer<Type>
    val rightParen: pointer<Token> = getContainerValue(results, 3, false) as pointer<Token>

    return new SizeOf(targetType)
        .addExtraToken(sizeOfToken)
        .addExtraToken(leftParen)
        .addExtraToken(rightParen)
}

private fun unwrapExpressionList(results: pointer<ArrayList>, index: int) -> pointer<ArrayList>
{
    val parsedExpressions: pointer<ArrayList> = getContainerValue(results, index) as pointer<ArrayList>
    val expressions: pointer<ArrayList> = new ArrayList(sizeof(pointer<Expression>))

    if parsedExpressions == null:
        return expressions

    for (var i = 0; i < parsedExpressions.length; i++):
    {
        val expression: pointer<Expression> = getContainerValue(parsedExpressions, i) as pointer<Expression>

        if expression != null:
            expressions.push(expression.ref)
    }

    return expressions
}

private fun makeExpressionTuple(results: pointer<ArrayList>) -> pointer<*>
{
    val leftParen: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val expressions: pointer<ArrayList> = unwrapExpressionList(results, 1)
    val rightParen: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>

    return new ExpressionTuple(expressions)
        .addExtraToken(leftParen)
        .addExtraToken(rightParen)
}

private fun makeListLiteral(results: pointer<ArrayList>) -> pointer<*>
{
    val leftBracket: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val expressions: pointer<ArrayList> = unwrapExpressionList(results, 1)
    val rightBracket: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>

    return new ListLiteral(expressions)
        .addExtraToken(leftBracket)
        .addExtraToken(rightBracket)
}

private inline fun makeStmtFrom_ExprStatement(results: pointer<ArrayList>) -> pointer<*>
{
    val expression: pointer<Expression> = getContainerValue(results, 0) as pointer<Expression>
    return Statement.fromExprStatement(new ExprStatement(expression))
}

private inline fun makeStmtFrom_ExprListStatement(results: pointer<ArrayList>) -> pointer<*>
{
    val parsedExpressions: pointer<ArrayList> = getContainerValue(results, 0) as pointer<ArrayList>

    if parsedExpressions == null || parsedExpressions.length <= 0:
        return null

    val first: pointer<Expression> = getContainerValue(parsedExpressions, 0) as pointer<Expression>
    val exprStatement: pointer<ExprListStatement> = new ExprListStatement(first)

    for (var i = 1; i < parsedExpressions.length; i++):
    {
        val expression: pointer<Expression> =
            getContainerValue(parsedExpressions, i) as pointer<Expression>

        exprStatement.addExpression(expression)
    }

    return Statement.fromExprListStatement(exprStatement)
}

private fun unwrapVariableDefines(results: pointer<ArrayList>, index: int) -> pointer<VariableDefines>
{
    val parsedDefines: pointer<ArrayList> = getContainerValue(results, index) as pointer<ArrayList>

    if parsedDefines == null || parsedDefines.length <= 0:
        return null

    val first: pointer<VariableDefine> = getContainerValue(parsedDefines, 0) as pointer<VariableDefine>
    val variableDefines: pointer<VariableDefines> = new VariableDefines(first)

    for (var i = 1; i < parsedDefines.length; i++):
    {
        val variableDefine: pointer<VariableDefine> =
            getContainerValue(parsedDefines, i) as pointer<VariableDefine>

        variableDefines.addDefine(variableDefine)
    }

    return variableDefines
}

private inline fun makeStmtFrom_VariableDefines(results: pointer<ArrayList>) -> pointer<*>
{
    val varToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val variableDefines: pointer<VariableDefines> = unwrapVariableDefines(results, 1)

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

private inline fun makeStmtFrom_WhileStmt(results: pointer<ArrayList>) -> pointer<*>
{
    val whileStatement: pointer<WhileStatement> = getContainerValue(results, 0) as pointer<WhileStatement>
    return Statement.fromWhileStatement(whileStatement)
}

private inline fun makeStmtFrom_ForStmt(results: pointer<ArrayList>) -> pointer<*>
{
    val forStatement: pointer<ForStatement> = getContainerValue(results, 0) as pointer<ForStatement>
    return Statement.fromForStatement(forStatement)
}

private inline fun makeStmtFrom_BreakStmt(results: pointer<ArrayList>) -> pointer<*>
{
    val breakToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    return Statement.fromBreakStatement(new BreakStatement().addExtraToken(breakToken))
}

private inline fun makeStmtFrom_ContinueStmt(results: pointer<ArrayList>) -> pointer<*>
{
    val continueToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    return Statement.fromContinueStatement(new ContinueStatement().addExtraToken(continueToken))
}

private inline fun makeStmtFrom_PassStmt(results: pointer<ArrayList>) -> pointer<*>
{
    val passToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    return Statement.fromPassStatement(new PassStatement().addExtraToken(passToken))
}

private inline fun makeElseStmtFromStmt(results: pointer<ArrayList>) -> pointer<*>
{
    val elseToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val colonToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val statement: pointer<Statement> = getContainerValue(results, 2) as pointer<Statement>

    return new ElseStatement(statement)
       .addExtraToken(elseToken).addExtraToken(colonToken)
}

private inline fun makeElseStmtFromBlock(results: pointer<ArrayList>) -> pointer<*>
{
    val elseToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val colonToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val block: pointer<Block> = getContainerValue(results, 2) as pointer<Block>
    val result: pointer<ElseStatement> = new ElseStatement(block.getStatements())
       .addExtraToken(elseToken).addExtraToken(colonToken)
    val extraTokens: pointer<ArrayList> = block.getExtraTokens()

    if extraTokens != null:
    {
        for (var i = 0; i < extraTokens.length; i++):
        {
            val token: pointer<Token> = extraTokens.get(i) as pointer<Token>
            result.addExtraToken(token)
        }
    }

    return result
}

private inline fun makeLoopCondition() -> pointer<Expression>
{
    val position: pointer<TokenPosition> = TokenPosition.autoGenPos()
    val token: pointer<Token> = new Token(Tokenizer.KW_TRUE, position, "true")
    val tokens: pointer<ArrayList> = new ArrayList(sizeof(pointer<*>))
    val resultItem: pointer<*> = token as pointer<*>

    tokens.push(resultItem.ref)
    return Expression.fromAtom(new Atom(Atom.BOOL_IMM_KIND, tokens))
}

private inline fun buildForHeader(
    initStmt: pointer<Statement>,
    condition: pointer<Expression>,
    stepStmt: pointer<Statement>,
    leftParenToken: pointer<Token>,
    firstSemicolonToken: pointer<Token>,
    secondSemicolonToken: pointer<Token>,
    rightParenToken: pointer<Token>) -> pointer<ForHeader>
{
    return new ForHeader(initStmt, condition, stepStmt)
       .addExtraToken(leftParenToken)
       .addExtraToken(firstSemicolonToken)
       .addExtraToken(secondSemicolonToken)
       .addExtraToken(rightParenToken)
}

private inline fun makeForHeaderICS(results: pointer<ArrayList>) -> pointer<*>
{
    val leftParenToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val initStmt: pointer<Statement> = getContainerValue(results, 1) as pointer<Statement>
    val firstSemicolonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val condition: pointer<Expression> = getContainerValue(results, 3) as pointer<Expression>
    val secondSemicolonToken: pointer<Token> = getContainerValue(results, 4, false) as pointer<Token>
    val stepStmt: pointer<Statement> = getContainerValue(results, 5) as pointer<Statement>
    val rightParenToken: pointer<Token> = getContainerValue(results, 6, false) as pointer<Token>

    return buildForHeader(initStmt, condition, stepStmt, leftParenToken, firstSemicolonToken, secondSemicolonToken, rightParenToken)
}

private inline fun makeForHeaderIC(results: pointer<ArrayList>) -> pointer<*>
{
    val leftParenToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val initStmt: pointer<Statement> = getContainerValue(results, 1) as pointer<Statement>
    val firstSemicolonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val condition: pointer<Expression> = getContainerValue(results, 3) as pointer<Expression>
    val secondSemicolonToken: pointer<Token> = getContainerValue(results, 4, false) as pointer<Token>
    val rightParenToken: pointer<Token> = getContainerValue(results, 5, false) as pointer<Token>

    return buildForHeader(initStmt, condition, null, leftParenToken, firstSemicolonToken, secondSemicolonToken, rightParenToken)
}

private inline fun makeForHeaderIS(results: pointer<ArrayList>) -> pointer<*>
{
    val leftParenToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val initStmt: pointer<Statement> = getContainerValue(results, 1) as pointer<Statement>
    val firstSemicolonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val secondSemicolonToken: pointer<Token> = getContainerValue(results, 3, false) as pointer<Token>
    val stepStmt: pointer<Statement> = getContainerValue(results, 4) as pointer<Statement>
    val rightParenToken: pointer<Token> = getContainerValue(results, 5, false) as pointer<Token>

    return buildForHeader(initStmt, null, stepStmt, leftParenToken, firstSemicolonToken, secondSemicolonToken, rightParenToken)
}

private inline fun makeForHeaderCS(results: pointer<ArrayList>) -> pointer<*>
{
    val leftParenToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val firstSemicolonToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val condition: pointer<Expression> = getContainerValue(results, 2) as pointer<Expression>
    val secondSemicolonToken: pointer<Token> = getContainerValue(results, 3, false) as pointer<Token>
    val stepStmt: pointer<Statement> = getContainerValue(results, 4) as pointer<Statement>
    val rightParenToken: pointer<Token> = getContainerValue(results, 5, false) as pointer<Token>

    return buildForHeader(null, condition, stepStmt, leftParenToken, firstSemicolonToken, secondSemicolonToken, rightParenToken)
}

private inline fun makeForHeaderI(results: pointer<ArrayList>) -> pointer<*>
{
    val leftParenToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val initStmt: pointer<Statement> = getContainerValue(results, 1) as pointer<Statement>
    val firstSemicolonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val secondSemicolonToken: pointer<Token> = getContainerValue(results, 3, false) as pointer<Token>
    val rightParenToken: pointer<Token> = getContainerValue(results, 4, false) as pointer<Token>

    return buildForHeader(initStmt, null, null, leftParenToken, firstSemicolonToken, secondSemicolonToken, rightParenToken)
}

private inline fun makeForHeaderC(results: pointer<ArrayList>) -> pointer<*>
{
    val leftParenToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val firstSemicolonToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val condition: pointer<Expression> = getContainerValue(results, 2) as pointer<Expression>
    val secondSemicolonToken: pointer<Token> = getContainerValue(results, 3, false) as pointer<Token>
    val rightParenToken: pointer<Token> = getContainerValue(results, 4, false) as pointer<Token>

    return buildForHeader(null, condition, null, leftParenToken, firstSemicolonToken, secondSemicolonToken, rightParenToken)
}

private inline fun makeForHeaderS(results: pointer<ArrayList>) -> pointer<*>
{
    val leftParenToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val firstSemicolonToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val secondSemicolonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val stepStmt: pointer<Statement> = getContainerValue(results, 3) as pointer<Statement>
    val rightParenToken: pointer<Token> = getContainerValue(results, 4, false) as pointer<Token>

    return buildForHeader(null, null, stepStmt, leftParenToken, firstSemicolonToken, secondSemicolonToken, rightParenToken)
}

private inline fun makeEmptyForHeader(results: pointer<ArrayList>) -> pointer<*>
{
    val leftParenToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val firstSemicolonToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val secondSemicolonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val rightParenToken: pointer<Token> = getContainerValue(results, 3, false) as pointer<Token>

    return buildForHeader(null, null, null, leftParenToken, firstSemicolonToken, secondSemicolonToken, rightParenToken)
}

private inline fun makeForStmtFromStmt(results: pointer<ArrayList>) -> pointer<*>
{
    val forToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val header: pointer<ForHeader> = getContainerValue(results, 1) as pointer<ForHeader>
    val colonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val statement: pointer<Statement> = getContainerValue(results, 3) as pointer<Statement>
    val statements: pointer<ArrayList> = new ArrayList(sizeof(Statement))
    statements.push(statement)
    val extraTokens: pointer<ArrayList> = new ArrayList(sizeof(Token))
    extraTokens.push(forToken)
    extraTokens.push(colonToken)

    return new ForStatement(header, statements).addExtraTokens(extraTokens)
}

private inline fun makeForStmtFromBlock(results: pointer<ArrayList>) -> pointer<*>
{
    val forToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val header: pointer<ForHeader> = getContainerValue(results, 1) as pointer<ForHeader>
    val colonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val block: pointer<Block> = getContainerValue(results, 3) as pointer<Block>
    val extraTokens: pointer<ArrayList> = new ArrayList(sizeof(Token))
    extraTokens.push(forToken)
    extraTokens.push(colonToken)

    return new ForStatement(header, block.getStatements())
       .addExtraTokens(extraTokens)
       .addExtraTokens(block.getExtraTokens())
}

private inline fun makeForStmtFromStmtElse(results: pointer<ArrayList>) -> pointer<*>
{
    val forToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val header: pointer<ForHeader> = getContainerValue(results, 1) as pointer<ForHeader>
    val colonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val statement: pointer<Statement> = getContainerValue(results, 3) as pointer<Statement>
    val elseStatement: pointer<ElseStatement> = getContainerValue(results, 4) as pointer<ElseStatement>
    val statements: pointer<ArrayList> = new ArrayList(sizeof(Statement))
    statements.push(statement)
    val extraTokens: pointer<ArrayList> = new ArrayList(sizeof(Token))
    extraTokens.push(forToken)
    extraTokens.push(colonToken)

    return new ForStatement(header, statements, elseStatement.getStatements())
       .addExtraTokens(extraTokens)
       .addExtraTokens(elseStatement.getExtraTokens())
}

private inline fun makeForStmtFromBlockElse(results: pointer<ArrayList>) -> pointer<*>
{
    val forToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val header: pointer<ForHeader> = getContainerValue(results, 1) as pointer<ForHeader>
    val colonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val block: pointer<Block> = getContainerValue(results, 3) as pointer<Block>
    val elseStatement: pointer<ElseStatement> = getContainerValue(results, 5) as pointer<ElseStatement>
    val extraTokens: pointer<ArrayList> = new ArrayList(sizeof(Token))
    extraTokens.push(forToken)
    extraTokens.push(colonToken)

    return new ForStatement(header, block.getStatements(), elseStatement.getStatements())
       .addExtraTokens(extraTokens)
       .addExtraTokens(block.getExtraTokens())
       .addExtraTokens(elseStatement.getExtraTokens())
}

private inline fun makeForStmtFromEmpty(results: pointer<ArrayList>) -> pointer<*>
{
    val forToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val header: pointer<ForHeader> = getContainerValue(results, 1) as pointer<ForHeader>
    val block: pointer<Block> = new Block()

    return new ForStatement(header, block.getStatements()).addExtraToken(forToken)
}

private inline fun makeWhileStmtFromStmt(results: pointer<ArrayList>) -> pointer<*>
{
    val whileToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val condition: pointer<Expression> = getContainerValue(results, 1) as pointer<Expression>
    val colonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val statement: pointer<Statement> = getContainerValue(results, 3) as pointer<Statement>
    val statements: pointer<ArrayList> = new ArrayList(sizeof(Statement))
    statements.push(statement)
    val extraTokens: pointer<ArrayList> = new ArrayList(sizeof(Token))
    extraTokens.push(whileToken)
    extraTokens.push(colonToken)

    return new WhileStatement(condition, statements).addExtraTokens(extraTokens)
}

private inline fun makeWhileStmtFromBlock(results: pointer<ArrayList>) -> pointer<*>
{
    val whileToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val condition: pointer<Expression> = getContainerValue(results, 1) as pointer<Expression>
    val colonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val block: pointer<Block> = getContainerValue(results, 3) as pointer<Block>
    val extraTokens: pointer<ArrayList> = new ArrayList(sizeof(Token))
    extraTokens.push(whileToken)
    extraTokens.push(colonToken)

    return new WhileStatement(condition, block.getStatements())
       .addExtraTokens(extraTokens)
       .addExtraTokens(block.getExtraTokens())
}

private inline fun makeLoopStmtFromStmt(results: pointer<ArrayList>) -> pointer<*>
{
    val loopToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val colonToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val statement: pointer<Statement> = getContainerValue(results, 2) as pointer<Statement>
    val statements: pointer<ArrayList> = new ArrayList(sizeof(Statement))
    statements.push(statement)
    val extraTokens: pointer<ArrayList> = new ArrayList(sizeof(Token))
    extraTokens.push(loopToken)
    extraTokens.push(colonToken)

    return new WhileStatement(makeLoopCondition(), statements).addExtraTokens(extraTokens)
}

private inline fun makeLoopStmtFromBlock(results: pointer<ArrayList>) -> pointer<*>
{
    val loopToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val colonToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val block: pointer<Block> = getContainerValue(results, 2) as pointer<Block>
    val extraTokens: pointer<ArrayList> = new ArrayList(sizeof(Token))
    extraTokens.push(loopToken)
    extraTokens.push(colonToken)

    return new WhileStatement(makeLoopCondition(), block.getStatements())
       .addExtraTokens(extraTokens)
       .addExtraTokens(block.getExtraTokens())
}

private inline fun makeLoopStmtFromStmtElse(results: pointer<ArrayList>) -> pointer<*>
{
    val loopToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val colonToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val statement: pointer<Statement> = getContainerValue(results, 2) as pointer<Statement>
    val elseStatement: pointer<ElseStatement> = getContainerValue(results, 3) as pointer<ElseStatement>
    val statements: pointer<ArrayList> = new ArrayList(sizeof(Statement))
    statements.push(statement)
    val extraTokens: pointer<ArrayList> = new ArrayList(sizeof(Token))
    extraTokens.push(loopToken)
    extraTokens.push(colonToken)

    return new WhileStatement(makeLoopCondition(), statements, elseStatement.getStatements())
       .addExtraTokens(extraTokens)
       .addExtraTokens(elseStatement.getExtraTokens())
}

private inline fun makeLoopStmtFromBlockElse(results: pointer<ArrayList>) -> pointer<*>
{
    val loopToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val colonToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val block: pointer<Block> = getContainerValue(results, 2) as pointer<Block>
    val elseStatement: pointer<ElseStatement> = getContainerValue(results, 4) as pointer<ElseStatement>
    val extraTokens: pointer<ArrayList> = new ArrayList(sizeof(Token))
    extraTokens.push(loopToken)
    extraTokens.push(colonToken)

    return new WhileStatement(makeLoopCondition(), block.getStatements(), elseStatement.getStatements())
       .addExtraTokens(extraTokens)
       .addExtraTokens(block.getExtraTokens())
       .addExtraTokens(elseStatement.getExtraTokens())
}

private inline fun makeWhileStmtFromStmtElse(results: pointer<ArrayList>) -> pointer<*>
{
    val whileToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val condition: pointer<Expression> = getContainerValue(results, 1) as pointer<Expression>
    val colonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val statement: pointer<Statement> = getContainerValue(results, 3) as pointer<Statement>
    val elseStatement: pointer<ElseStatement> = getContainerValue(results, 4) as pointer<ElseStatement>
    val statements: pointer<ArrayList> = new ArrayList(sizeof(Statement))
    statements.push(statement)
    val extraTokens: pointer<ArrayList> = new ArrayList(sizeof(Token))
    extraTokens.push(whileToken)
    extraTokens.push(colonToken)

    return new WhileStatement(condition, statements, elseStatement.getStatements())
       .addExtraTokens(extraTokens)
       .addExtraTokens(elseStatement.getExtraTokens())
}

private inline fun makeWhileStmtFromBlockElse(results: pointer<ArrayList>) -> pointer<*>
{
    val whileToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val condition: pointer<Expression> = getContainerValue(results, 1) as pointer<Expression>
    val colonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val block: pointer<Block> = getContainerValue(results, 3) as pointer<Block>
    val elseStatement: pointer<ElseStatement> = getContainerValue(results, 5) as pointer<ElseStatement>
    val extraTokens: pointer<ArrayList> = new ArrayList(sizeof(Token))
    extraTokens.push(whileToken)
    extraTokens.push(colonToken)

    return new WhileStatement(condition, block.getStatements(), elseStatement.getStatements())
       .addExtraTokens(extraTokens)
       .addExtraTokens(block.getExtraTokens())
       .addExtraTokens(elseStatement.getExtraTokens())
}

private inline fun makeVariableDefine(results: pointer<ArrayList>) -> pointer<*>
{
    val nameToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val equalToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val expression: pointer<Expression> = getContainerValue(results, 2) as pointer<Expression>

    return new VariableDefine(nameToken.text, expression).addExtraToken(nameToken).addExtraToken(equalToken)
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

private inline fun makeVarDefWithoutInitValue(results: pointer<ArrayList>) -> pointer<*>
{
    val nameToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>

    return new VariableDefine(nameToken.text, null).addExtraToken(nameToken)
}

private inline fun makeVarDefWithTypeWithoutInitValue(results: pointer<ArrayList>) -> pointer<*>
{
    val nameToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val colonToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val declaredType: pointer<Type> = getContainerValue(results, 2) as pointer<Type>

    return new VariableDefine(declaredType, nameToken.text, null)
        .addExtraToken(nameToken)
        .addExtraToken(colonToken)
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

    return new Block().addExtraToken(leftBrace).addExtraToken(rightBrace)
}

private fun makeBlockExpr(results: pointer<ArrayList>) -> pointer<*>
{
    val leftBrace: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val statements: pointer<ArrayList> = getContainerValue(results, 1) as pointer<ArrayList>
    val rightBrace: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>

    return new Block(statements)
       .addExtraToken(leftBrace).addExtraToken(rightBrace)
}

private fun makeIfExprFromStmts(results: pointer<ArrayList>) -> pointer<*>
{
    val ifToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val condition: pointer<Expression> = getContainerValue(results, 1) as pointer<Expression>
    val colonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val block: pointer<Block> = getContainerValue(results, 3) as pointer<Block>

    return new IfExpression(condition, block.getStatements())
       .addExtraToken(ifToken).addExtraToken(colonToken)
       .addExtraTokens(block.getExtraTokens())
}

private fun makeIfExprFromStmt(results: pointer<ArrayList>) -> pointer<*>
{
    val ifToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val condition: pointer<Expression> = getContainerValue(results, 1) as pointer<Expression>
    val colonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val statement: pointer<Statement> = getContainerValue(results, 3) as pointer<Statement>
    val statements: pointer<ArrayList> = new ArrayList(sizeof(Statement))
    statements.push(statement)

    return new IfExpression(condition, statements)
       .addExtraToken(ifToken).addExtraToken(colonToken)
}

private fun makeIfElseExprFinalB(results: pointer<ArrayList>) -> pointer<*>
{
    val ifToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val condition: pointer<Expression> = getContainerValue(results, 1) as pointer<Expression>
    val colonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val block: pointer<Block> = getContainerValue(results, 3) as pointer<Block>
    val instance1: pointer<IfElseExpression> = getContainerValue(results, 5) as pointer<IfElseExpression>

    return instance1.setCondition(condition)
       .addIfStatements(block.getStatements())
       .addExtraToken(ifToken).addExtraToken(colonToken)
       .addExtraTokens(block.getExtraTokens())
}

private fun makeIfElseExprFinalS(results: pointer<ArrayList>) -> pointer<*>
{
    val ifToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val condition: pointer<Expression> = getContainerValue(results, 1) as pointer<Expression>
    val colonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val statement: pointer<Statement> = getContainerValue(results, 3) as pointer<Statement>
    val instance1: pointer<IfElseExpression> = getContainerValue(results, 4) as pointer<IfElseExpression>

    return instance1.setCondition(condition)
       .addIfStatement(statement)
       .addExtraToken(ifToken).addExtraToken(colonToken)
}

private fun makeIfElseExprFinalE(results: pointer<ArrayList>) -> pointer<*>
{
    val ifToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val condition: pointer<Expression> = getContainerValue(results, 1) as pointer<Expression>
    val colonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val expression: pointer<Expression> = getContainerValue(results, 3) as pointer<Expression>
    val instance1: pointer<IfElseExpression> = getContainerValue(results, 4) as pointer<IfElseExpression>
    val statement: pointer<Statement> = Statement.fromExprStatement(new ExprStatement(expression))

    return instance1.setCondition(condition)
       .addIfStatement(statement)
       .addExtraToken(ifToken).addExtraToken(colonToken)
}

private fun makeIfElseExprFromElifB(results: pointer<ArrayList>) -> pointer<*>
{
    val elifToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val condition: pointer<Expression> = getContainerValue(results, 1) as pointer<Expression>
    val colonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val block: pointer<Block> = getContainerValue(results, 3) as pointer<Block>
    val instance1: pointer<IfElseExpression> = new IfElseExpression()
       .setCondition(condition)
       .addIfStatements(block.getStatements())
       .addExtraTokens(block.getExtraTokens())

    val expr: pointer<Expression> = Expression.fromIfElseExpr(instance1)
    val stmt: pointer<Statement> = Statement.fromExprStatement(new ExprStatement(expr))

    return new IfElseExpression(stmt)
       .addExtraToken(elifToken).addExtraToken(colonToken)
}

private fun makeIfElseExprItB(results: pointer<ArrayList>) -> pointer<*>
{
    val elifToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val condition: pointer<Expression> = getContainerValue(results, 1) as pointer<Expression>
    val colonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val block: pointer<Block> = getContainerValue(results, 3) as pointer<Block>
    val instance1: pointer<IfElseExpression> = getContainerValue(results, 5) as pointer<IfElseExpression>

    instance1.setCondition(condition)
       .addIfStatements(block.getStatements())
       .addExtraTokens(block.getExtraTokens())
    val expr: pointer<Expression> = Expression.fromIfElseExpr(instance1)
    val stmt: pointer<Statement> = Statement.fromExprStatement(new ExprStatement(expr))

    return new IfElseExpression(stmt)
       .addExtraToken(elifToken).addExtraToken(colonToken)
}

private fun makeIfElseExprFromElifS(results: pointer<ArrayList>) -> pointer<*>
{
    val elifToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val condition: pointer<Expression> = getContainerValue(results, 1) as pointer<Expression>
    val colonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val statement: pointer<Statement> = getContainerValue(results, 3) as pointer<Statement>
    val instance1: pointer<IfElseExpression> = new IfElseExpression()
       .setCondition(condition)
       .addIfStatement(statement)
    val expr: pointer<Expression> = Expression.fromIfElseExpr(instance1)
    val stmt: pointer<Statement> = Statement.fromExprStatement(new ExprStatement(expr))

    return new IfElseExpression(stmt)
       .addExtraToken(elifToken).addExtraToken(colonToken)
}

private fun makeIfElseExprFromElifE(results: pointer<ArrayList>) -> pointer<*>
{
    val elifToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val condition: pointer<Expression> = getContainerValue(results, 1) as pointer<Expression>
    val colonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val expression: pointer<Expression> = getContainerValue(results, 3) as pointer<Expression>
    val statement: pointer<Statement> = Statement.fromExprStatement(new ExprStatement(expression))
    val instance1: pointer<IfElseExpression> = new IfElseExpression()
       .setCondition(condition)
       .addIfStatement(statement)
    val expr: pointer<Expression> = Expression.fromIfElseExpr(instance1)
    val stmt: pointer<Statement> = Statement.fromExprStatement(new ExprStatement(expr))

    return new IfElseExpression(stmt)
       .addExtraToken(elifToken).addExtraToken(colonToken)
}

private fun makeIfElseExprItE(results: pointer<ArrayList>) -> pointer<*>
{
    val elifToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val condition: pointer<Expression> = getContainerValue(results, 1) as pointer<Expression>
    val colonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val expression: pointer<Expression> = getContainerValue(results, 3) as pointer<Expression>
    val instance1: pointer<IfElseExpression> = getContainerValue(results, 4) as pointer<IfElseExpression>
    val statement: pointer<Statement> = Statement.fromExprStatement(new ExprStatement(expression))

    instance1.setCondition(condition)
       .addIfStatement(statement)
    val expr: pointer<Expression> = Expression.fromIfElseExpr(instance1)
    val stmt: pointer<Statement> = Statement.fromExprStatement(new ExprStatement(expr))

    return new IfElseExpression(stmt)
       .addExtraToken(elifToken).addExtraToken(colonToken)
}

private fun makeIfElseExprItS(results: pointer<ArrayList>) -> pointer<*>
{
    val elifToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val condition: pointer<Expression> = getContainerValue(results, 1) as pointer<Expression>
    val colonToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val statement: pointer<Statement> = getContainerValue(results, 3) as pointer<Statement>
    val instance1: pointer<IfElseExpression> = getContainerValue(results, 4) as pointer<IfElseExpression>

    instance1.setCondition(condition)
       .addIfStatement(statement)
    val expr: pointer<Expression> = Expression.fromIfElseExpr(instance1)
    val stmt: pointer<Statement> = Statement.fromExprStatement(new ExprStatement(expr))

    return new IfElseExpression(stmt)
       .addExtraToken(elifToken).addExtraToken(colonToken)
}

private fun makeIfElseExprFromElseB(results: pointer<ArrayList>) -> pointer<*>
{
    val elseToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val colonToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val block: pointer<Block> = getContainerValue(results, 2) as pointer<Block>

    return new IfElseExpression(block.getStatements())
       .addExtraToken(elseToken).addExtraToken(colonToken)
       .addExtraTokens(block.getExtraTokens())
}

private fun makeIfElseExprFromElseS(results: pointer<ArrayList>) -> pointer<*>
{
    val elseToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val colonToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val statement: pointer<Statement> = getContainerValue(results, 2) as pointer<Statement>

    return new IfElseExpression(statement)
       .addExtraToken(elseToken).addExtraToken(colonToken)
}

private fun makeElseStmtFromExpr(results: pointer<ArrayList>) -> pointer<*>
{
    val elseToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val colonToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val expression: pointer<Expression> = getContainerValue(results, 2) as pointer<Expression>
    val statement: pointer<Statement> = Statement.fromExprStatement(new ExprStatement(expression))

    return new IfElseExpression(statement)
       .addExtraToken(elseToken).addExtraToken(colonToken)
}

private fun prependLineTerminator(tokens: pointer<TokenList>, index: int) -> pointer<TokenList>
{
    if tokens == null || index < 0 || index > tokens.length():
        return tokens

    if index < tokens.length():
    {
        val current: pointer<Token> = tokens.get(index)

        if current != null && current.kind == Tokenizer.TK_LINE_TERMINATOR:
            return tokens
    }

    val terminator: pointer<Token> = new Token(Tokenizer.TK_LINE_TERMINATOR, TokenPosition.autoGenPos(), "\n")
    tokens.add(index, terminator)

    return tokens
}

private inline fun makeModifier(results: pointer<ArrayList>) -> pointer<*>
{
    val token: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    return new Modifier(token.text).addExtraToken(token)
}

private inline fun makeModifierListMaybe(results: pointer<ArrayList>) -> pointer<*>
{
    val parsedModifiers: pointer<ArrayList> = getContainerValue(results, 0) as pointer<ArrayList>
    val modifiers: pointer<ArrayList> = new ArrayList(sizeof(Modifier))

    for (var i = 0; i < parsedModifiers.length; i++):
        modifiers.push(getContainerValue(parsedModifiers, i) as pointer<Modifier>)

    return new ModifierListMaybe(modifiers)
}

private inline fun makeEmptyModifierListMaybe(results: pointer<ArrayList>) -> pointer<*> = new ModifierListMaybe()

private inline fun makePreprocessSetting(results: pointer<ArrayList>) -> pointer<*>
{
    val hashToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val qualifiedName: pointer<QualifiedName> = getContainerValue(results, 1) as pointer<QualifiedName>
    val leftParen: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val value: pointer<ArrayList> = getContainerValue(results, 3) as pointer<ArrayList>
    val rightParen: pointer<Token> = getContainerValue(results, 4, false) as pointer<Token>

    return new PreprocessSetting(qualifiedName, value)
        .addExtraToken(hashToken)
        .addExtraToken(leftParen)
        .addExtraToken(rightParen)
}

private inline fun makePreprocessSettingsMaybe(results: pointer<ArrayList>) -> pointer<*>
{
    val parsedSettings: pointer<ArrayList> = getContainerValue(results, 0) as pointer<ArrayList>
    val settings: pointer<ArrayList> = new ArrayList(sizeof(PreprocessSetting))

    for (var i = 0; i < parsedSettings.length; i++):
        settings.push(getContainerValue(parsedSettings, i) as pointer<PreprocessSetting>)

    return new PreprocessSettingsMaybe(settings)
}

private inline fun makeEmptyPreprocessSettingsMaybe(results: pointer<ArrayList>) -> pointer<*> = new PreprocessSettingsMaybe()

private inline fun makeAnnotation(results: pointer<ArrayList>) -> pointer<*>
{
    val atToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val qualifiedName: pointer<QualifiedName> = getContainerValue(results, 1) as pointer<QualifiedName>

    return new Annotation(qualifiedName).addExtraToken(atToken)
}

private inline fun makeAnnotationWithValues(results: pointer<ArrayList>) -> pointer<*>
{
    val atToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val qualifiedName: pointer<QualifiedName> = getContainerValue(results, 1) as pointer<QualifiedName>
    val leftParen: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val value: pointer<ArrayList> = getContainerValue(results, 3) as pointer<ArrayList>
    val rightParen: pointer<Token> = getContainerValue(results, 4, false) as pointer<Token>

    return new Annotation(qualifiedName, value)
        .addExtraToken(atToken)
        .addExtraToken(leftParen)
        .addExtraToken(rightParen)
}

private inline fun makeAnnotationsMaybe(results: pointer<ArrayList>) -> pointer<*>
{
    val parsedAnnotations: pointer<ArrayList> = getContainerValue(results, 0) as pointer<ArrayList>
    val annotations: pointer<ArrayList> = new ArrayList(sizeof(Annotation))

    for (var i = 0; i < parsedAnnotations.length; i++):
        annotations.push(getContainerValue(parsedAnnotations, i) as pointer<Annotation>)

    return new AnnotationsMaybe(annotations)
}

private inline fun makeEmptyAnnotationsMaybe(results: pointer<ArrayList>) -> pointer<*> = new AnnotationsMaybe()

private inline fun makeQualifiedNameIt(results: pointer<ArrayList>) -> pointer<*>
{
    val nameToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val dotToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val qualifiedName: pointer<QualifiedName> = getContainerValue(results, 2) as pointer<QualifiedName>

    return qualifiedName.pushFront(nameToken.text)
        .addExtraToken(nameToken)
        .addExtraToken(dotToken)
}

private inline fun makeQualifiedName(results: pointer<ArrayList>) -> pointer<*>
{
    val nameToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>

    return new QualifiedName(nameToken.text).addExtraToken(nameToken)
}

private inline fun makePackageDeclarationMaybe(results: pointer<ArrayList>) -> pointer<*>
{
    val packageToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val qualifiedName: pointer<QualifiedName> = getContainerValue(results, 1) as pointer<QualifiedName>

    val packageDeclaration: pointer<PackageDeclaration> = qualifiedName.toPackageDecl().addExtraToken(packageToken)
    return new PackageDeclarationMaybe(packageDeclaration)
}

private inline fun makeEmptyPackageDeclarationMaybe(results: pointer<ArrayList>) -> pointer<*> = new PackageDeclarationMaybe()

private inline fun makeNamespaceImport(results: pointer<ArrayList>) -> pointer<*>
{
    val importToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val qualifiedName: pointer<QualifiedName> = getContainerValue(results, 1) as pointer<QualifiedName>

    return new NamespaceImport(qualifiedName).addExtraToken(importToken)
}

private inline fun makeImportFromNamespace(results: pointer<ArrayList>) -> pointer<*> =
    ImportDeclaration.fromNamespace(getContainerValue(results, 0) as pointer<NamespaceImport>)

private inline fun makeImportDeclarationsMaybe(results: pointer<ArrayList>) -> pointer<*>
{
    val parsedImports: pointer<ArrayList> = getContainerValue(results, 0) as pointer<ArrayList>
    val imports: pointer<ArrayList> = new ArrayList(sizeof(ImportDeclaration))

    for (var i = 0; i < parsedImports.length; i++):
        imports.push(getContainerValue(parsedImports, i) as pointer<ImportDeclaration>)

    return new ImportDeclarationsMaybe(imports)
}

private inline fun makeFunctionParam(results: pointer<ArrayList>) -> pointer<*>
{
    val nameToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val colonToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val paramType: pointer<Type> = getContainerValue(results, 2) as pointer<Type>

    return new FunctionParam(nameToken.text, paramType)
        .addExtraToken(nameToken)
        .addExtraToken(colonToken)
}

private inline fun makeMutFunctionParam(results: pointer<ArrayList>) -> pointer<*>
{
    val mutToken: pointer<Token> = getContainerValue(results, 0, false) as pointer<Token>
    val paramResults: pointer<ArrayList> = new ArrayList(sizeof(pointer<*>))

    paramResults.push(results.get(1))
    paramResults.push(results.get(2))
    paramResults.push(results.get(3))

    val result: pointer<FunctionParam> = makeFunctionParam(paramResults) as pointer<FunctionParam>

    return result
        .markAsMut()
        .addExtraToken(mutToken)
}

private inline fun makeFunctionParamsIt(results: pointer<ArrayList>) -> pointer<*>
{
    val param: pointer<FunctionParam> = getContainerValue(results, 0) as pointer<FunctionParam>
    val commaToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val params: pointer<FunctionParams> = getContainerValue(results, 2) as pointer<FunctionParams>

    return new FunctionParams(param).addExtraToken(commaToken).pushAll(params)
}

private inline fun makeFunctionParams(results: pointer<ArrayList>) -> pointer<*>
{
    val param: pointer<FunctionParam> = getContainerValue(results, 0) as pointer<FunctionParam>
    return new FunctionParams(param)
}

private inline fun makeFunctionParamsMaybe(results: pointer<ArrayList>) -> pointer<*>
{
    val params: pointer<FunctionParams> = getContainerValue(results, 0) as pointer<FunctionParams>
    return new FunctionParamsMaybe(params)
}

private inline fun makeEmptyFunctionParamsMaybe(results: pointer<ArrayList>) -> pointer<*> = new FunctionParamsMaybe(null)

private inline fun buildField(results: pointer<ArrayList>) -> pointer<Field>
{
    val annotations: pointer<AnnotationsMaybe> = getContainerValue(results, 0) as pointer<AnnotationsMaybe>
    val modifiers: pointer<ModifierListMaybe> = getContainerValue(results, 1) as pointer<ModifierListMaybe>
    val varToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val nameToken: pointer<Token> = getContainerValue(results, 3, false) as pointer<Token>
    val colonToken: pointer<Token> = getContainerValue(results, 4, false) as pointer<Token>
    val fieldType: pointer<Type> = getContainerValue(results, 5) as pointer<Type>

    var result: pointer<Field> = if varToken.kind == Tokenizer.KW_VAL:
        new Field(nameToken.text, fieldType).markAsConst()
    else:
        new Field(nameToken.text, fieldType).markAsMut()

    return result
        .setAnnotations(annotations.toAnnotations())
        .setModifiers(modifiers.toModifierList())
        .addExtraToken(varToken)
        .addExtraToken(nameToken)
        .addExtraToken(colonToken)
}

private inline fun makeField(results: pointer<ArrayList>) -> pointer<*> =
    buildField(results)


private inline fun makeFieldWithInitialValue(results: pointer<ArrayList>) -> pointer<*>
{
    val equalToken: pointer<Token> = getContainerValue(results, 6, false) as pointer<Token>
    val initialValue: pointer<Expression> = getContainerValue(results, 7) as pointer<Expression>

    return buildField(results)
        .setInitialValue(initialValue)
        .addExtraToken(equalToken)
}

private inline fun buildFunction(results: pointer<ArrayList>, returnType: pointer<Type>, bodyExpr: pointer<Expression>) -> pointer<Function>
{
    val annotations: pointer<AnnotationsMaybe> = getContainerValue(results, 0) as pointer<AnnotationsMaybe>
    val modifiers: pointer<ModifierListMaybe> = getContainerValue(results, 1) as pointer<ModifierListMaybe>
    val functionNameToken: pointer<Token> = getContainerValue(results, 3, false) as pointer<Token>
    val params: pointer<FunctionParamsMaybe> = getContainerValue(results, 5) as pointer<FunctionParamsMaybe>

    return new Function(functionNameToken.text, params.toFunctionParams(), bodyExpr)
        .setAnnotations(annotations.toAnnotations())
        .setModifiers(modifiers.toModifierList())
        .setReturnType(returnType)
}

private inline fun addFunctionPrefixTokens(function: pointer<Function>, results: pointer<ArrayList>) -> pointer<Function>
{
    val funToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val functionNameToken: pointer<Token> = getContainerValue(results, 3, false) as pointer<Token>
    val leftParen: pointer<Token> = getContainerValue(results, 4, false) as pointer<Token>
    val rightParen: pointer<Token> = getContainerValue(results, 6, false) as pointer<Token>

    return function
        .addExtraToken(funToken)
        .addExtraToken(functionNameToken)
        .addExtraToken(leftParen)
        .addExtraToken(rightParen)
}

private inline fun makeFunction(results: pointer<ArrayList>) -> pointer<*>
{
    val arrowToken: pointer<Token> = getContainerValue(results, 7, false) as pointer<Token>
    val returnType: pointer<Type> = getContainerValue(results, 8) as pointer<Type>
    val separatorToken: pointer<Token> = getContainerValue(results, 9, false) as pointer<Token>
    val bodyExpr: pointer<Expression> = getContainerValue(results, 10) as pointer<Expression>

    return addFunctionPrefixTokens(buildFunction(results, returnType, bodyExpr), results)
        .addExtraToken(arrowToken)
        .addExtraToken(separatorToken)
}

private inline fun makeFunctionFromBlock(results: pointer<ArrayList>) -> pointer<*>
{
    val arrowToken: pointer<Token> = getContainerValue(results, 7, false) as pointer<Token>
    val returnType: pointer<Type> = getContainerValue(results, 8) as pointer<Type>
    val block: pointer<Block> = getContainerValue(results, 9) as pointer<Block>
    val bodyExpr: pointer<Expression> = Expression.fromBlockExpr(block)

    return addFunctionPrefixTokens(buildFunction(results, returnType, bodyExpr), results)
        .addExtraToken(arrowToken)
}

private inline fun makeFunctionFromEmptyBlock(results: pointer<ArrayList>) -> pointer<*>
{
    val arrowToken: pointer<Token> = getContainerValue(results, 7, false) as pointer<Token>
    val returnType: pointer<Type> = getContainerValue(results, 8) as pointer<Type>

    return addFunctionPrefixTokens(buildFunction(results, returnType, null), results)
        .addExtraToken(arrowToken)
}

private inline fun makeVoidFunction(results: pointer<ArrayList>) -> pointer<*>
{
    val colonToken: pointer<Token> = getContainerValue(results, 7, false) as pointer<Token>
    val bodyExpr: pointer<Expression> = getContainerValue(results, 8) as pointer<Expression>

    return addFunctionPrefixTokens(buildFunction(results, Type.voidType(), bodyExpr), results)
        .addExtraToken(colonToken)
}

private inline fun makeVoidFunctionFromBlock(results: pointer<ArrayList>) -> pointer<*>
{
    val block: pointer<Block> = getContainerValue(results, 7) as pointer<Block>
    val bodyExpr: pointer<Expression> = Expression.fromBlockExpr(block)

    return addFunctionPrefixTokens(buildFunction(results, Type.voidType(), bodyExpr), results)
}

private inline fun makeVoidFunctionFromEmptyBlock(results: pointer<ArrayList>) -> pointer<*> =
    addFunctionPrefixTokens(buildFunction(results, Type.voidType(), null), results)

private inline fun makeStructConstructor(results: pointer<ArrayList>) -> pointer<*>
{
    val modifiers: pointer<ModifierListMaybe> = getContainerValue(results, 0) as pointer<ModifierListMaybe>
    val constructorToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val leftParen: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val params: pointer<FunctionParamsMaybe> = getContainerValue(results, 3) as pointer<FunctionParamsMaybe>
    val rightParen: pointer<Token> = getContainerValue(results, 4, false) as pointer<Token>
    val colonToken: pointer<Token> = getContainerValue(results, 5, false) as pointer<Token>
    val bodyExpr: pointer<Expression> = getContainerValue(results, 6) as pointer<Expression>

    return new StructConstructor(params.toFunctionParams(), bodyExpr)
        .setModifiers(modifiers.toModifierList())
        .addExtraToken(constructorToken)
        .addExtraToken(leftParen)
        .addExtraToken(rightParen)
        .addExtraToken(colonToken)
}

private inline fun makeStructConstructorFromBlock(results: pointer<ArrayList>) -> pointer<*>
{
    val modifiers: pointer<ModifierListMaybe> = getContainerValue(results, 0) as pointer<ModifierListMaybe>
    val constructorToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val leftParen: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val params: pointer<FunctionParamsMaybe> = getContainerValue(results, 3) as pointer<FunctionParamsMaybe>
    val rightParen: pointer<Token> = getContainerValue(results, 4, false) as pointer<Token>
    val block: pointer<Block> = getContainerValue(results, 5) as pointer<Block>
    val bodyExpr: pointer<Expression> = Expression.fromBlockExpr(block)

    return new StructConstructor(params.toFunctionParams(), bodyExpr)
        .setModifiers(modifiers.toModifierList())
        .addExtraToken(constructorToken)
        .addExtraToken(leftParen)
        .addExtraToken(rightParen)
}

private inline fun makeMemberFromField(results: pointer<ArrayList>) -> pointer<*>
{
    val field: pointer<Field> = getContainerValue(results, 0) as pointer<Field>
    return Member.fromField(field)
}

private inline fun makeMemberFromFunction(results: pointer<ArrayList>) -> pointer<*>
{
    val function: pointer<Function> = getContainerValue(results, 0) as pointer<Function>
    return Member.fromFunction(function)
}

private inline fun makeMemberFromStructConstructor(results: pointer<ArrayList>) -> pointer<*>
{
    val structConstructor: pointer<StructConstructor> = getContainerValue(results, 0) as pointer<StructConstructor>
    return Member.fromStructConstructor(structConstructor)
}

private inline fun makeMemberFromStruct(results: pointer<ArrayList>) -> pointer<*>
{
    val structDecl: pointer<Struct> = getContainerValue(results, 0) as pointer<Struct>
    return Member.fromStruct(structDecl)
}

private inline fun makeStruct(results: pointer<ArrayList>) -> pointer<*>
{
    val modifiers: pointer<ModifierListMaybe> = getContainerValue(results, 0) as pointer<ModifierListMaybe>
    val structToken: pointer<Token> = getContainerValue(results, 1, false) as pointer<Token>
    val nameToken: pointer<Token> = getContainerValue(results, 2, false) as pointer<Token>
    val leftBrace: pointer<Token> = getContainerValue(results, 3, false) as pointer<Token>
    val members: pointer<ArrayList> = getContainerValue(results, 4) as pointer<ArrayList>
    val rightBrace: pointer<Token> = getContainerValue(results, 5, false) as pointer<Token>

    return new Struct(nameToken.text, members)
        .setModifiers(modifiers.toModifierList())
        .addExtraToken(structToken)
        .addExtraToken(nameToken)
        .addExtraToken(leftBrace)
        .addExtraToken(rightBrace)
}

private inline fun makeProgram(results: pointer<ArrayList>) -> pointer<*>
{
    val settings: pointer<PreprocessSettingsMaybe> = getContainerValue(results, 0) as pointer<PreprocessSettingsMaybe>
    val packageDeclaration: pointer<PackageDeclarationMaybe> = getContainerValue(results, 1) as pointer<PackageDeclarationMaybe>
    val imports: pointer<ImportDeclarationsMaybe> = getContainerValue(results, 2) as pointer<ImportDeclarationsMaybe>
    val members: pointer<ArrayList> = getContainerValue(results, 3) as pointer<ArrayList>

    return new Program(members)
        .setPreprocessSettings(settings.toPreprocessSettings())
        .setPackageDeclaration(packageDeclaration.toPackageDeclaration())
        .setImportDeclarations(imports.toImportDeclarations())
}


private val EXPRESSION_PARSER_SPECIFIC: pointer<PrattParser> = new PrattParser()
val EXPRESSION_PARSER: pointer<ParserRef> = ParserRef.fromPratt(EXPRESSION_PARSER_ID, EXPRESSION_PARSER_SPECIFIC)

val ATOM_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(ATOM_PARSER_ID)

val SIZE_OF_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(SIZE_OF_PARSER_ID)

val EXPRESSION_TUPLE_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(EXPRESSION_TUPLE_PARSER_ID)

val LIST_LITERAL_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(LIST_LITERAL_PARSER_ID)

val STATEMENT_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(STATEMENT_PARSER_ID)

val ELSE_STATEMENT_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(ELSE_STATEMENT_PARSER_ID)

val WHILE_STATEMENT_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(WHILE_STATEMENT_PARSER_ID)

val FOR_HEADER_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(FOR_HEADER_PARSER_ID)

val FOR_STATEMENT_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(FOR_STATEMENT_PARSER_ID)

val VARIABLE_DEFINE_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(VARIABLE_DEFINE_PARSER_ID)

val RETURN_STATEMENT_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(RETURN_STATEMENT_PARSER_ID)

val BLOCK_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(BLOCK_PARSER_ID)

val IF_EXPRESSION_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(IF_EXPRESSION_PARSER_ID)

val IF_ELSE_EXPRESSION_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(IF_ELSE_EXPRESSION_PARSER_ID)

val MODIFIER_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(MODIFIER_PARSER_ID)

val MODIFIER_LIST_MAYBE_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(MODIFIER_LIST_MAYBE_PARSER_ID)

val PREPROCESS_SETTING_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(PREPROCESS_SETTING_PARSER_ID)

val PREPROCESS_SETTINGS_MAYBE_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(PREPROCESS_SETTINGS_MAYBE_PARSER_ID)

val ANNOTATION_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(ANNOTATION_PARSER_ID)

val ANNOTATIONS_MAYBE_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(ANNOTATIONS_MAYBE_PARSER_ID)

val QUALIFIED_NAME_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(QUALIFIED_NAME_PARSER_ID)

val PACKAGE_DECLARATION_MAYBE_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(PACKAGE_DECLARATION_MAYBE_PARSER_ID)

val NAMESPACE_IMPORT_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(NAMESPACE_IMPORT_PARSER_ID)

val IMPORT_DECLARATION_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(IMPORT_DECLARATION_PARSER_ID)

val IMPORT_DECLARATIONS_MAYBE_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(IMPORT_DECLARATIONS_MAYBE_PARSER_ID)

val FUNCTION_PARAM_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(FUNCTION_PARAM_PARSER_ID)

val FUNCTION_PARAMS_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(FUNCTION_PARAMS_PARSER_ID)

val FUNCTION_PARAMS_MAYBE_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(FUNCTION_PARAMS_MAYBE_PARSER_ID)

val FIELD_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(FIELD_PARSER_ID)

val FUNCTION_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(FUNCTION_PARSER_ID)

val STRUCT_CONSTRUCTOR_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(STRUCT_CONSTRUCTOR_PARSER_ID)

val MEMBER_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(MEMBER_PARSER_ID)

val STRUCT_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(STRUCT_PARSER_ID)

val PROGRAM_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(PROGRAM_PARSER_ID)

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
private val EXPRESSION_RULE2: pointer<Rule> = new Rule(new PatternList().pushRef(SIZE_OF_PARSER), makeExprFromSizeOf, Rule.STARTER_ROLE, 240)
private val EXPRESSION_RULE3: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.TK_IDENTIFIER).pushRef(EXPRESSION_TUPLE_PARSER), makeExprFromFuncCall, Rule.STARTER_ROLE, 240)
private val EXPRESSION_RULE4: pointer<Rule> = new Rule(new PatternList().pushRef(IF_ELSE_EXPRESSION_PARSER), makeExprFromIfElseExpr, Rule.STARTER_ROLE, 240)
private val EXPRESSION_RULE5: pointer<Rule> = new Rule(new PatternList().pushRef(IF_EXPRESSION_PARSER), makeExprFromIfExpr, Rule.STARTER_ROLE, 230)
private val EXPRESSION_RULE6: pointer<Rule> = new Rule(new PatternList().pushRef(BLOCK_PARSER), makeExprFromBlockExpr, Rule.STARTER_ROLE, 230)
private val EXPRESSION_RULE7: pointer<Rule> = new Rule(new PatternList().pushRef(ATOM_PARSER), makeExprFromAtom, Rule.STARTER_ROLE, 230)
private val EXPRESSION_RULE8: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_PAREN).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.RIGHT_PAREN), makeExprFromParen, Rule.STARTER_ROLE, OP_PAREN)
private val EXPRESSION_RULE9: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.PLUS).pushRef(EXPRESSION_PARSER), makeExprFromPrefix, Rule.STARTER_ROLE, OP_POS)
private val EXPRESSION_RULE10: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.MINUS).pushRef(EXPRESSION_PARSER), makeExprFromPrefix, Rule.STARTER_ROLE, OP_NEG)
private val EXPRESSION_RULE11: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.DOUBLE_PLUS).pushRef(EXPRESSION_PARSER), makeExprFromPrefix, Rule.STARTER_ROLE, OP_INC)
private val EXPRESSION_RULE12: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.DOUBLE_MINUS).pushRef(EXPRESSION_PARSER), makeExprFromPrefix, Rule.STARTER_ROLE, OP_DEC)
private val EXPRESSION_RULE13: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_INV).pushRef(EXPRESSION_PARSER), makeExprFromPrefix, Rule.STARTER_ROLE, OP_INV)
private val EXPRESSION_RULE14: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.BANG).pushRef(EXPRESSION_PARSER), makeExprFromPrefix, Rule.STARTER_ROLE, OP_NOT)
private val EXPRESSION_RULE15: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRef(LIST_LITERAL_PARSER), makeExprFromIndexAccess, Rule.CONTINUATION_ROLE, 230)
private val EXPRESSION_RULE16: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOT).pushRegex(Tokenizer.TK_IDENTIFIER).pushRef(EXPRESSION_TUPLE_PARSER), makeExprFromMethodCall, Rule.CONTINUATION_ROLE, 230)
private val EXPRESSION_RULE17: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOT).pushRegex(Tokenizer.TK_IDENTIFIER), makeExprFromFieldAccess, Rule.CONTINUATION_ROLE, 220)
private val EXPRESSION_RULE18: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOUBLE_PLUS), makeExprFromPostfix, Rule.CONTINUATION_ROLE, OP_SUCC)
private val EXPRESSION_RULE19: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOUBLE_MINUS), makeExprFromPostfix, Rule.CONTINUATION_ROLE, OP_PRED)
private val EXPRESSION_RULE20: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_AS).pushRef(TYPE_PARSER), makeExprFromTypeCast, Rule.CONTINUATION_ROLE, 200)
private val EXPRESSION_RULE21: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOUBLE_STAR).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_POW)
private val EXPRESSION_RULE22: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.STAR).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_TIMES)
private val EXPRESSION_RULE23: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.SLASH).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_DIV)
private val EXPRESSION_RULE24: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.PERCENT).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_REM)
private val EXPRESSION_RULE25: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.PLUS).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_PLUS)
private val EXPRESSION_RULE26: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.MINUS).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_MINUS)
private val EXPRESSION_RULE27: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_SHL).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_SHL)
private val EXPRESSION_RULE28: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_SHR).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_SHR)
private val EXPRESSION_RULE29: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_USHR).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_USHR)
private val EXPRESSION_RULE30: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.GREATER).pushRef(EXPRESSION_PARSER), makeExprFromCompare, Rule.CONTINUATION_ROLE, OP_GREATER)
private val EXPRESSION_RULE31: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.LESS).pushRef(EXPRESSION_PARSER), makeExprFromCompare, Rule.CONTINUATION_ROLE, OP_LESS)
private val EXPRESSION_RULE32: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.GREATER_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromCompare, Rule.CONTINUATION_ROLE, OP_GREATER_EQUAL)
private val EXPRESSION_RULE33: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.LESS_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromCompare, Rule.CONTINUATION_ROLE, OP_LESS_EQUAL)
private val EXPRESSION_RULE34: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.TRIPLE_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_REF_EQUALS)
private val EXPRESSION_RULE35: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOUBLE_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_EQUALS)
private val EXPRESSION_RULE36: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.BANG_DOUBLE_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromNotRefEqual, Rule.CONTINUATION_ROLE, OP_REF_EQUALS)
private val EXPRESSION_RULE37: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.NOT_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromNotEqual, Rule.CONTINUATION_ROLE, OP_EQUALS)
private val EXPRESSION_RULE38: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_AND).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_BITWISE_AND)
private val EXPRESSION_RULE39: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_NAND).pushRef(EXPRESSION_PARSER), makeExprFromBitwiseNand, Rule.CONTINUATION_ROLE, OP_BITWISE_NAND)
private val EXPRESSION_RULE40: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_XOR).pushRef(EXPRESSION_PARSER), makeExprFromBitwiseXor, Rule.CONTINUATION_ROLE, OP_BITWISE_XOR)
private val EXPRESSION_RULE41: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_XNOR).pushRef(EXPRESSION_PARSER), makeExprFromBitwiseXnor, Rule.CONTINUATION_ROLE, OP_BITWISE_XNOR)
private val EXPRESSION_RULE42: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_OR).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_BITWISE_OR)
private val EXPRESSION_RULE43: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_NOR).pushRef(EXPRESSION_PARSER), makeExprFromBitwiseNor, Rule.CONTINUATION_ROLE, OP_BITWISE_NOR)
private val EXPRESSION_RULE44: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_IMPLIES).pushRef(EXPRESSION_PARSER), makeExprFromImplies, Rule.CONTINUATION_ROLE, OP_IMPLIES)
private val EXPRESSION_RULE45: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_NIMPLIES).pushRef(EXPRESSION_PARSER), makeExprFromNimplies, Rule.CONTINUATION_ROLE, OP_NIMPLIES)
private val EXPRESSION_RULE46: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_IFF).pushRef(EXPRESSION_PARSER), makeExprFromIff, Rule.CONTINUATION_ROLE, OP_IFF)
private val EXPRESSION_RULE47: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_NIFF).pushRef(EXPRESSION_PARSER), makeExprFromNiff, Rule.CONTINUATION_ROLE, OP_NIFF)
private val EXPRESSION_RULE48: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOUBLE_AMPERSAND).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_LOGICAL_AND)
private val EXPRESSION_RULE49: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.BANG_DOUBLE_AMPERSAND).pushRef(EXPRESSION_PARSER), makeExprFromLogicalNand, Rule.CONTINUATION_ROLE, OP_LOGICAL_NAND)
private val EXPRESSION_RULE50: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.CARET).pushRef(EXPRESSION_PARSER), makeExprFromLogicalXor, Rule.CONTINUATION_ROLE, OP_LOGICAL_XOR)
private val EXPRESSION_RULE51: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.BANG_CARET).pushRef(EXPRESSION_PARSER), makeExprFromLogicalXnor, Rule.CONTINUATION_ROLE, OP_LOGICAL_XNOR)
private val EXPRESSION_RULE52: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOUBLE_PIPE).pushRef(EXPRESSION_PARSER), makeExprFromInfix, Rule.CONTINUATION_ROLE, OP_LOGICAL_OR)
private val EXPRESSION_RULE53: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.BANG_DOUBLE_PIPE).pushRef(EXPRESSION_PARSER), makeExprFromLogicalNor, Rule.CONTINUATION_ROLE, OP_LOGICAL_NOR)
private val EXPRESSION_RULE54: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.ARROW).pushRef(EXPRESSION_PARSER), makeExprFromLogicalImplies, Rule.CONTINUATION_ROLE, OP_LOGICAL_IMPLIES)
private val EXPRESSION_RULE55: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.NOT_ARROW).pushRef(EXPRESSION_PARSER), makeExprFromLogicalNimplies, Rule.CONTINUATION_ROLE, OP_LOGICAL_NIMPLIES)
private val EXPRESSION_RULE56: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOUBLE_ARROW).pushRef(EXPRESSION_PARSER), makeExprFromLogicalIff, Rule.CONTINUATION_ROLE, OP_LOGICAL_IFF)
private val EXPRESSION_RULE57: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.BANG_DOUBLE_ARROW).pushRef(EXPRESSION_PARSER), makeExprFromLogicalNiff, Rule.CONTINUATION_ROLE, OP_LOGICAL_NIFF)
private val EXPRESSION_RULE58: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromAssign, Rule.CONTINUATION_ROLE, OP_ASSIGN)
private val EXPRESSION_RULE59: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOUBLE_STAR_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromPowAssign, Rule.CONTINUATION_ROLE, OP_POW_ASSIGN)
private val EXPRESSION_RULE60: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.STAR_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromTimesAssign, Rule.CONTINUATION_ROLE, OP_TIMES_ASSIGN)
private val EXPRESSION_RULE61: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.SLASH_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromDivAssign, Rule.CONTINUATION_ROLE, OP_DIV_ASSIGN)
private val EXPRESSION_RULE62: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.PERCENT_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromRemAssign, Rule.CONTINUATION_ROLE, OP_REM_ASSIGN)
private val EXPRESSION_RULE63: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.PLUS_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromPlusAssign, Rule.CONTINUATION_ROLE, OP_PLUS_ASSIGN)
private val EXPRESSION_RULE64: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.MINUS_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromMinusAssign, Rule.CONTINUATION_ROLE, OP_MINUS_ASSIGN)
private val EXPRESSION_RULE65: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOUBLE_LESS_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromShlAssign, Rule.CONTINUATION_ROLE, OP_SHL_ASSIGN)
private val EXPRESSION_RULE66: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOUBLE_GREATER_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromShrAssign, Rule.CONTINUATION_ROLE, OP_SHR_ASSIGN)
private val EXPRESSION_RULE67: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.TRIPLE_LESS_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromUshlAssign, Rule.CONTINUATION_ROLE, OP_USHL_ASSIGN)
private val EXPRESSION_RULE68: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.TRIPLE_GREATER_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromUshrAssign, Rule.CONTINUATION_ROLE, OP_USHR_ASSIGN)
private val EXPRESSION_RULE69: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.AMPERSAND_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromBitwiseAndAssign, Rule.CONTINUATION_ROLE, OP_BITWISE_AND_ASSIGN)
private val EXPRESSION_RULE70: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.BANG_AMPERSAND_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromBitwiseNandAssign, Rule.CONTINUATION_ROLE, OP_BITWISE_NAND_ASSIGN)
private val EXPRESSION_RULE71: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.PIPE_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromBitwiseOrAssign, Rule.CONTINUATION_ROLE, OP_BITWISE_OR_ASSIGN)
private val EXPRESSION_RULE72: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.BANG_PIPE_EQUAL).pushRef(EXPRESSION_PARSER), makeExprFromBitwiseNorAssign, Rule.CONTINUATION_ROLE, OP_BITWISE_NOR_ASSIGN)

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

private val SIZE_OF_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_SIZEOF).pushRegex(Tokenizer.LEFT_PAREN).pushRef(TYPE_PARSER).pushRegex(Tokenizer.RIGHT_PAREN), makeSizeOf, Rule.STARTER_ROLE, 0)

private val EXPRESSION_TUPLE_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_PAREN).pushRefs(new ParserRefs(EXPRESSION_PARSER, new PatternAtom(Tokenizer.COMMA, null))).pushRegex(Tokenizer.RIGHT_PAREN), makeExpressionTuple, Rule.STARTER_ROLE, 0)

private val LIST_LITERAL_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_BRACKET).pushRefs(new ParserRefs(EXPRESSION_PARSER, new PatternAtom(Tokenizer.COMMA, null))).pushRegex(Tokenizer.RIGHT_BRACKET), makeListLiteral, Rule.STARTER_ROLE, 0)

private val STATEMENT_RULE0: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeStmtFrom_ExprStatement, Rule.STARTER_ROLE, 0)
private val STATEMENT_RULE1: pointer<Rule> = new Rule(new PatternList().pushRefs(new ParserRefs(EXPRESSION_PARSER, new PatternAtom(Tokenizer.COMMA, null))).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeStmtFrom_ExprListStatement, Rule.STARTER_ROLE, 0)
private val STATEMENT_RULE2: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_VAR).pushRefs(new ParserRefs(VARIABLE_DEFINE_PARSER, new PatternAtom(Tokenizer.COMMA, null), false)).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeStmtFrom_VariableDefines, Rule.STARTER_ROLE, 0)
private val STATEMENT_RULE3: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_VAL).pushRefs(new ParserRefs(VARIABLE_DEFINE_PARSER, new PatternAtom(Tokenizer.COMMA, null), false)).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeStmtFrom_VariableDefines, Rule.STARTER_ROLE, 0)
private val STATEMENT_RULE4: pointer<Rule> = new Rule(new PatternList().pushRef(WHILE_STATEMENT_PARSER), makeStmtFrom_WhileStmt, Rule.STARTER_ROLE, 0)
private val STATEMENT_RULE5: pointer<Rule> = new Rule(new PatternList().pushRef(FOR_STATEMENT_PARSER), makeStmtFrom_ForStmt, Rule.STARTER_ROLE, 0)
private val STATEMENT_RULE6: pointer<Rule> = new Rule(new PatternList().pushRef(RETURN_STATEMENT_PARSER), makeStmtFrom_ReturnStmt, Rule.STARTER_ROLE, 0)
private val STATEMENT_RULE7: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_BREAK).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeStmtFrom_BreakStmt, Rule.STARTER_ROLE, 0)
private val STATEMENT_RULE8: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_CONTINUE).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeStmtFrom_ContinueStmt, Rule.STARTER_ROLE, 0)
private val STATEMENT_RULE9: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_PASS).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeStmtFrom_PassStmt, Rule.STARTER_ROLE, 0)

private val ELSE_STATEMENT_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_ELSE).pushRegex(Tokenizer.COLON).pushRef(BLOCK_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeElseStmtFromBlock, Rule.STARTER_ROLE, 0)
private val ELSE_STATEMENT_RULE1: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_ELSE).pushRegex(Tokenizer.COLON).pushRef(STATEMENT_PARSER), makeElseStmtFromStmt, Rule.STARTER_ROLE, 0)
private val ELSE_STATEMENT_RULE2: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_ELSE).pushRegex(Tokenizer.COLON).pushRef(EXPRESSION_PARSER), makeElseStmtFromExpr, Rule.STARTER_ROLE, 0)

private val WHILE_STATEMENT_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_LOOP).pushRegex(Tokenizer.COLON).pushRef(BLOCK_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR).pushRef(ELSE_STATEMENT_PARSER), makeLoopStmtFromBlockElse, Rule.STARTER_ROLE, 0)
private val WHILE_STATEMENT_RULE1: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_LOOP).pushRegex(Tokenizer.COLON).pushRef(STATEMENT_PARSER).pushRef(ELSE_STATEMENT_PARSER), makeLoopStmtFromStmtElse, Rule.STARTER_ROLE, 0)
private val WHILE_STATEMENT_RULE2: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_LOOP).pushRegex(Tokenizer.COLON).pushRef(BLOCK_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeLoopStmtFromBlock, Rule.STARTER_ROLE, 0)
private val WHILE_STATEMENT_RULE3: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_LOOP).pushRegex(Tokenizer.COLON).pushRef(STATEMENT_PARSER), makeLoopStmtFromStmt, Rule.STARTER_ROLE, 0)
private val WHILE_STATEMENT_RULE4: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_WHILE).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.COLON).pushRef(BLOCK_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR).pushRef(ELSE_STATEMENT_PARSER), makeWhileStmtFromBlockElse, Rule.STARTER_ROLE, 0)
private val WHILE_STATEMENT_RULE5: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_WHILE).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.COLON).pushRef(STATEMENT_PARSER).pushRef(ELSE_STATEMENT_PARSER), makeWhileStmtFromStmtElse, Rule.STARTER_ROLE, 0)
private val WHILE_STATEMENT_RULE6: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_WHILE).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.COLON).pushRef(BLOCK_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeWhileStmtFromBlock, Rule.STARTER_ROLE, 0)
private val WHILE_STATEMENT_RULE7: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_WHILE).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.COLON).pushRef(STATEMENT_PARSER), makeWhileStmtFromStmt, Rule.STARTER_ROLE, 0)

private val FOR_HEADER_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_PAREN).pushRegex(Tokenizer.SEMICOLON).pushRegex(Tokenizer.SEMICOLON).pushRegex(Tokenizer.RIGHT_PAREN), makeEmptyForHeader, Rule.STARTER_ROLE, 0)
private val FOR_HEADER_RULE1: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_PAREN).pushRegex(Tokenizer.SEMICOLON).pushRegex(Tokenizer.SEMICOLON).pushRefs(new ParserRefs(EXPRESSION_PARSER, new PatternAtom(Tokenizer.COMMA, null))).pushRegex(Tokenizer.RIGHT_PAREN), makeForHeaderS, Rule.STARTER_ROLE, 0)
private val FOR_HEADER_RULE2: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_PAREN).pushRegex(Tokenizer.SEMICOLON).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.SEMICOLON).pushRegex(Tokenizer.RIGHT_PAREN), makeForHeaderC, Rule.STARTER_ROLE, 0)
private val FOR_HEADER_RULE3: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_PAREN).pushRegex(Tokenizer.SEMICOLON).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.SEMICOLON).pushRefs(new ParserRefs(EXPRESSION_PARSER, new PatternAtom(Tokenizer.COMMA, null))).pushRegex(Tokenizer.RIGHT_PAREN), makeForHeaderCS, Rule.STARTER_ROLE, 0)
private val FOR_HEADER_RULE4: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_PAREN).pushRegex(Tokenizer.KW_VAR).pushRefs(new ParserRefs(VARIABLE_DEFINE_PARSER, new PatternAtom(Tokenizer.COMMA, null), false)).pushRegex(Tokenizer.SEMICOLON).pushRegex(Tokenizer.SEMICOLON).pushRegex(Tokenizer.RIGHT_PAREN), makeForHeaderI, Rule.STARTER_ROLE, 0)
private val FOR_HEADER_RULE5: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_PAREN).pushRegex(Tokenizer.KW_VAR).pushRefs(new ParserRefs(VARIABLE_DEFINE_PARSER, new PatternAtom(Tokenizer.COMMA, null), false)).pushRegex(Tokenizer.SEMICOLON).pushRegex(Tokenizer.SEMICOLON).pushRefs(new ParserRefs(EXPRESSION_PARSER, new PatternAtom(Tokenizer.COMMA, null))).pushRegex(Tokenizer.RIGHT_PAREN), makeForHeaderIS, Rule.STARTER_ROLE, 0)
private val FOR_HEADER_RULE6: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_PAREN).pushRegex(Tokenizer.KW_VAR).pushRefs(new ParserRefs(VARIABLE_DEFINE_PARSER, new PatternAtom(Tokenizer.COMMA, null), false)).pushRegex(Tokenizer.SEMICOLON).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.SEMICOLON).pushRegex(Tokenizer.RIGHT_PAREN), makeForHeaderIC, Rule.STARTER_ROLE, 0)
private val FOR_HEADER_RULE7: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_PAREN).pushRegex(Tokenizer.KW_VAR).pushRefs(new ParserRefs(VARIABLE_DEFINE_PARSER, new PatternAtom(Tokenizer.COMMA, null), false)).pushRegex(Tokenizer.SEMICOLON).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.SEMICOLON).pushRefs(new ParserRefs(EXPRESSION_PARSER, new PatternAtom(Tokenizer.COMMA, null))).pushRegex(Tokenizer.RIGHT_PAREN), makeForHeaderICS, Rule.STARTER_ROLE, 0)

private val FOR_STATEMENT_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_FOR).pushRef(FOR_HEADER_PARSER).pushRegex(Tokenizer.COLON).pushRef(BLOCK_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR).pushRef(ELSE_STATEMENT_PARSER), makeForStmtFromBlockElse, Rule.STARTER_ROLE, 0)
private val FOR_STATEMENT_RULE1: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_FOR).pushRef(FOR_HEADER_PARSER).pushRegex(Tokenizer.COLON).pushRef(STATEMENT_PARSER).pushRef(ELSE_STATEMENT_PARSER), makeForStmtFromStmtElse, Rule.STARTER_ROLE, 0)
private val FOR_STATEMENT_RULE2: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_FOR).pushRef(FOR_HEADER_PARSER).pushRegex(Tokenizer.COLON).pushRef(BLOCK_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeForStmtFromBlock, Rule.STARTER_ROLE, 0)
private val FOR_STATEMENT_RULE3: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_FOR).pushRef(FOR_HEADER_PARSER).pushRegex(Tokenizer.COLON).pushRef(STATEMENT_PARSER), makeForStmtFromStmt, Rule.STARTER_ROLE, 0)
private val FOR_STATEMENT_RULE4: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_FOR).pushRef(FOR_HEADER_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeForStmtFromEmpty, Rule.STARTER_ROLE, 0)

private val VARIABLE_DEFINE_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.TK_IDENTIFIER).pushRegex(Tokenizer.EQUAL).pushRef(EXPRESSION_PARSER), makeVariableDefine, Rule.STARTER_ROLE, 0)
private val VARIABLE_DEFINE_RULE1: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.TK_IDENTIFIER).pushRegex(Tokenizer.COLON).pushRef(TYPE_PARSER).pushRegex(Tokenizer.EQUAL).pushRef(EXPRESSION_PARSER), makeVariableDefineWithType, Rule.STARTER_ROLE, 0)
private val VARIABLE_DEFINE_RULE2: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.TK_IDENTIFIER).pushRegex(Tokenizer.COLON).pushRef(TYPE_PARSER), makeVarDefWithTypeWithoutInitValue, Rule.STARTER_ROLE, 0)
private val VARIABLE_DEFINE_RULE3: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.TK_IDENTIFIER), makeVarDefWithoutInitValue, Rule.STARTER_ROLE, 0)

private val RETURN_STATEMENT_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_RETURN).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeReturnStmt, Rule.STARTER_ROLE, 0)
private val RETURN_STATEMENT_RULE1: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_RETURN).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeReturnStmt, Rule.STARTER_ROLE, 0)

private val BLOCK_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_BRACE).pushRegex(Tokenizer.RIGHT_BRACE), makeEmptyBlockExpr, Rule.STARTER_ROLE, 0)
private val BLOCK_RULE1: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_BRACE).pushRefs(new ParserRefs(STATEMENT_PARSER)).pushRegex(Tokenizer.RIGHT_BRACE), makeBlockExpr, Rule.STARTER_ROLE, 0)

private val IF_EXPRESSION_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_IF).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.COLON).pushRef(BLOCK_PARSER), makeIfExprFromStmts, Rule.STARTER_ROLE, 0)
private val IF_EXPRESSION_RULE1: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_IF).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.COLON).pushRef(STATEMENT_PARSER), makeIfExprFromStmt, Rule.STARTER_ROLE, 0).setAfterFun(prependLineTerminator)

private val IF_ELSE_EXPRESSION_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_IF).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.COLON).pushRef(BLOCK_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR).pushRef(IF_ELSE_EXPRESSION_PARSER), makeIfElseExprFinalB, Rule.STARTER_ROLE, 0)
private val IF_ELSE_EXPRESSION_RULE1: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_IF).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.COLON).pushRef(STATEMENT_PARSER).pushRef(IF_ELSE_EXPRESSION_PARSER), makeIfElseExprFinalS, Rule.STARTER_ROLE, 0)
private val IF_ELSE_EXPRESSION_RULE2: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_IF).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.COLON).pushRef(EXPRESSION_PARSER).pushRef(IF_ELSE_EXPRESSION_PARSER), makeIfElseExprFinalE, Rule.STARTER_ROLE, 0)
private val IF_ELSE_EXPRESSION_RULE3: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_ELIF).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.COLON).pushRef(BLOCK_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR).pushRef(IF_ELSE_EXPRESSION_PARSER), makeIfElseExprItB, Rule.STARTER_ROLE, 0)
private val IF_ELSE_EXPRESSION_RULE4: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_ELIF).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.COLON).pushRef(STATEMENT_PARSER).pushRef(IF_ELSE_EXPRESSION_PARSER), makeIfElseExprItS, Rule.STARTER_ROLE, 0)
private val IF_ELSE_EXPRESSION_RULE5: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_ELIF).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.COLON).pushRef(EXPRESSION_PARSER).pushRef(IF_ELSE_EXPRESSION_PARSER), makeIfElseExprItE, Rule.STARTER_ROLE, 0)
private val IF_ELSE_EXPRESSION_RULE6: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_ELSE).pushRegex(Tokenizer.COLON).pushRef(BLOCK_PARSER), makeIfElseExprFromElseB, Rule.STARTER_ROLE, 0)
private val IF_ELSE_EXPRESSION_RULE7: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_ELSE).pushRegex(Tokenizer.COLON).pushRef(STATEMENT_PARSER), makeIfElseExprFromElseS, Rule.STARTER_ROLE, 0).setAfterFun(prependLineTerminator)
private val IF_ELSE_EXPRESSION_RULE8: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_ELSE).pushRegex(Tokenizer.COLON).pushRef(EXPRESSION_PARSER), makeElseStmtFromExpr, Rule.STARTER_ROLE, 0)
private val IF_ELSE_EXPRESSION_RULE9: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_ELIF).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.COLON).pushRef(BLOCK_PARSER), makeIfElseExprFromElifB, Rule.STARTER_ROLE, 0)
private val IF_ELSE_EXPRESSION_RULE10: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_ELIF).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.COLON).pushRef(STATEMENT_PARSER), makeIfElseExprFromElifS, Rule.STARTER_ROLE, 0).setAfterFun(prependLineTerminator)
private val IF_ELSE_EXPRESSION_RULE11: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_ELIF).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.COLON).pushRef(EXPRESSION_PARSER), makeIfElseExprFromElifE, Rule.STARTER_ROLE, 0)

private val MODIFIER_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_PRIVATE), makeModifier, Rule.STARTER_ROLE, 0)
private val MODIFIER_RULE1: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_PROTECTED), makeModifier, Rule.STARTER_ROLE, 0)
private val MODIFIER_RULE2: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_PUBLIC), makeModifier, Rule.STARTER_ROLE, 0)
private val MODIFIER_RULE3: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_STATIC), makeModifier, Rule.STARTER_ROLE, 0)
private val MODIFIER_RULE4: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_FINAL), makeModifier, Rule.STARTER_ROLE, 0)
private val MODIFIER_RULE5: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_INLINE), makeModifier, Rule.STARTER_ROLE, 0)
private val MODIFIER_RULE6: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_NATIVE), makeModifier, Rule.STARTER_ROLE, 0)
private val MODIFIER_RULE7: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_INTRINSIC), makeModifier, Rule.STARTER_ROLE, 0)

private val MODIFIER_LIST_MAYBE_RULE0: pointer<Rule> = new Rule(new PatternList().pushRefs(new ParserRefs(MODIFIER_PARSER)), makeModifierListMaybe, Rule.STARTER_ROLE, 0)
private val MODIFIER_LIST_MAYBE_RULE1: pointer<Rule> = new Rule(new PatternList(), makeEmptyModifierListMaybe, Rule.STARTER_ROLE, 0)

private val PREPROCESS_SETTING_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.HASH).pushRef(QUALIFIED_NAME_PARSER).pushRegex(Tokenizer.LEFT_PAREN).pushRefs(new ParserRefs(ATOM_PARSER, new PatternAtom(Tokenizer.COMMA, null))).pushRegex(Tokenizer.RIGHT_PAREN).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makePreprocessSetting, Rule.STARTER_ROLE, 0)

private val PREPROCESS_SETTINGS_MAYBE_RULE0: pointer<Rule> = new Rule(new PatternList().pushRefs(new ParserRefs(PREPROCESS_SETTING_PARSER)), makePreprocessSettingsMaybe, Rule.STARTER_ROLE, 0)
private val PREPROCESS_SETTINGS_MAYBE_RULE1: pointer<Rule> = new Rule(new PatternList(), makeEmptyPreprocessSettingsMaybe, Rule.STARTER_ROLE, 0)

private val ANNOTATION_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.AT).pushRef(QUALIFIED_NAME_PARSER).pushRegex(Tokenizer.LEFT_PAREN).pushRefs(new ParserRefs(ATOM_PARSER, new PatternAtom(Tokenizer.COMMA, null))).pushRegex(Tokenizer.RIGHT_PAREN), makeAnnotationWithValues, Rule.STARTER_ROLE, 0)
private val ANNOTATION_RULE1: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.AT).pushRef(QUALIFIED_NAME_PARSER), makeAnnotation, Rule.STARTER_ROLE, 0)

private val ANNOTATIONS_MAYBE_RULE0: pointer<Rule> = new Rule(new PatternList().pushRefs(new ParserRefs(ANNOTATION_PARSER)).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeAnnotationsMaybe, Rule.STARTER_ROLE, 0)
private val ANNOTATIONS_MAYBE_RULE1: pointer<Rule> = new Rule(new PatternList(), makeEmptyAnnotationsMaybe, Rule.STARTER_ROLE, 0)

private val QUALIFIED_NAME_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.TK_IDENTIFIER).pushRegex(Tokenizer.DOT).pushRef(QUALIFIED_NAME_PARSER), makeQualifiedNameIt, Rule.STARTER_ROLE, 0)
private val QUALIFIED_NAME_RULE1: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.TK_IDENTIFIER), makeQualifiedName, Rule.STARTER_ROLE, 0)

private val PACKAGE_DECLARATION_MAYBE_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_PACKAGE).pushRef(QUALIFIED_NAME_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makePackageDeclarationMaybe, Rule.STARTER_ROLE, 0)
private val PACKAGE_DECLARATION_MAYBE_RULE1: pointer<Rule> = new Rule(new PatternList(), makeEmptyPackageDeclarationMaybe, Rule.STARTER_ROLE, 0)

private val NAMESPACE_IMPORT_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_IMPORT).pushRef(QUALIFIED_NAME_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeNamespaceImport, Rule.STARTER_ROLE, 0)

private val IMPORT_DECLARATION_RULE0: pointer<Rule> = new Rule(new PatternList().pushRef(NAMESPACE_IMPORT_PARSER), makeImportFromNamespace, Rule.STARTER_ROLE, 0)

private val IMPORT_DECLARATIONS_MAYBE_RULE0: pointer<Rule> = new Rule(new PatternList().pushRefs(new ParserRefs(IMPORT_DECLARATION_PARSER)), makeImportDeclarationsMaybe, Rule.STARTER_ROLE, 0)

private val FUNCTION_PARAM_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.TK_IDENTIFIER).pushRegex(Tokenizer.COLON).pushRef(TYPE_PARSER), makeFunctionParam, Rule.STARTER_ROLE, 0)
private val FUNCTION_PARAM_RULE1: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.KW_MUT).pushRegex(Tokenizer.TK_IDENTIFIER).pushRegex(Tokenizer.COLON).pushRef(TYPE_PARSER), makeMutFunctionParam, Rule.STARTER_ROLE, 0)

private val FUNCTION_PARAMS_RULE0: pointer<Rule> = new Rule(new PatternList().pushRef(FUNCTION_PARAM_PARSER).pushRegex(Tokenizer.COMMA).pushRef(FUNCTION_PARAMS_PARSER), makeFunctionParamsIt, Rule.STARTER_ROLE, 0)
private val FUNCTION_PARAMS_RULE1: pointer<Rule> = new Rule(new PatternList().pushRef(FUNCTION_PARAM_PARSER), makeFunctionParams, Rule.STARTER_ROLE, 0)

private val FUNCTION_PARAMS_MAYBE_RULE0: pointer<Rule> = new Rule(new PatternList().pushRef(FUNCTION_PARAMS_PARSER), makeFunctionParamsMaybe, Rule.STARTER_ROLE, 0)
private val FUNCTION_PARAMS_MAYBE_RULE1: pointer<Rule> = new Rule(new PatternList(), makeEmptyFunctionParamsMaybe, Rule.STARTER_ROLE, 0)

private val FIELD_RULE0: pointer<Rule> = new Rule(new PatternList().pushRef(ANNOTATIONS_MAYBE_PARSER).pushRef(MODIFIER_LIST_MAYBE_PARSER).pushRegex(Tokenizer.KW_VAR).pushRegex(Tokenizer.TK_IDENTIFIER).pushRegex(Tokenizer.COLON).pushRef(TYPE_PARSER).pushRegex(Tokenizer.EQUAL).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeFieldWithInitialValue, Rule.STARTER_ROLE, 0)
private val FIELD_RULE1: pointer<Rule> = new Rule(new PatternList().pushRef(ANNOTATIONS_MAYBE_PARSER).pushRef(MODIFIER_LIST_MAYBE_PARSER).pushRegex(Tokenizer.KW_VAL).pushRegex(Tokenizer.TK_IDENTIFIER).pushRegex(Tokenizer.COLON).pushRef(TYPE_PARSER).pushRegex(Tokenizer.EQUAL).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeFieldWithInitialValue, Rule.STARTER_ROLE, 0)
private val FIELD_RULE2: pointer<Rule> = new Rule(new PatternList().pushRef(ANNOTATIONS_MAYBE_PARSER).pushRef(MODIFIER_LIST_MAYBE_PARSER).pushRegex(Tokenizer.KW_VAR).pushRegex(Tokenizer.TK_IDENTIFIER).pushRegex(Tokenizer.COLON).pushRef(TYPE_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeField, Rule.STARTER_ROLE, 0)
private val FIELD_RULE3: pointer<Rule> = new Rule(new PatternList().pushRef(ANNOTATIONS_MAYBE_PARSER).pushRef(MODIFIER_LIST_MAYBE_PARSER).pushRegex(Tokenizer.KW_VAL).pushRegex(Tokenizer.TK_IDENTIFIER).pushRegex(Tokenizer.COLON).pushRef(TYPE_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeField, Rule.STARTER_ROLE, 0)

private val FUNCTION_RULE0: pointer<Rule> = new Rule(new PatternList().pushRef(ANNOTATIONS_MAYBE_PARSER).pushRef(MODIFIER_LIST_MAYBE_PARSER).pushRegex(Tokenizer.KW_FUN).pushRegex(Tokenizer.TK_IDENTIFIER).pushRegex(Tokenizer.LEFT_PAREN).pushRef(FUNCTION_PARAMS_MAYBE_PARSER).pushRegex(Tokenizer.RIGHT_PAREN).pushRegex(Tokenizer.ARROW).pushRef(TYPE_PARSER).pushRegex(Tokenizer.EQUAL).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeFunction, Rule.STARTER_ROLE, 0)
private val FUNCTION_RULE1: pointer<Rule> = new Rule(new PatternList().pushRef(ANNOTATIONS_MAYBE_PARSER).pushRef(MODIFIER_LIST_MAYBE_PARSER).pushRegex(Tokenizer.KW_FUN).pushRegex(Tokenizer.TK_IDENTIFIER).pushRegex(Tokenizer.LEFT_PAREN).pushRef(FUNCTION_PARAMS_MAYBE_PARSER).pushRegex(Tokenizer.RIGHT_PAREN).pushRegex(Tokenizer.ARROW).pushRef(TYPE_PARSER).pushRegex(Tokenizer.COLON).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeFunction, Rule.STARTER_ROLE, 0)
private val FUNCTION_RULE2: pointer<Rule> = new Rule(new PatternList().pushRef(ANNOTATIONS_MAYBE_PARSER).pushRef(MODIFIER_LIST_MAYBE_PARSER).pushRegex(Tokenizer.KW_FUN).pushRegex(Tokenizer.TK_IDENTIFIER).pushRegex(Tokenizer.LEFT_PAREN).pushRef(FUNCTION_PARAMS_MAYBE_PARSER).pushRegex(Tokenizer.RIGHT_PAREN).pushRegex(Tokenizer.ARROW).pushRef(TYPE_PARSER).pushRef(BLOCK_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeFunctionFromBlock, Rule.STARTER_ROLE, 0)
private val FUNCTION_RULE3: pointer<Rule> = new Rule(new PatternList().pushRef(ANNOTATIONS_MAYBE_PARSER).pushRef(MODIFIER_LIST_MAYBE_PARSER).pushRegex(Tokenizer.KW_FUN).pushRegex(Tokenizer.TK_IDENTIFIER).pushRegex(Tokenizer.LEFT_PAREN).pushRef(FUNCTION_PARAMS_MAYBE_PARSER).pushRegex(Tokenizer.RIGHT_PAREN).pushRegex(Tokenizer.ARROW).pushRef(TYPE_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeFunctionFromEmptyBlock, Rule.STARTER_ROLE, 0)
private val FUNCTION_RULE4: pointer<Rule> = new Rule(new PatternList().pushRef(ANNOTATIONS_MAYBE_PARSER).pushRef(MODIFIER_LIST_MAYBE_PARSER).pushRegex(Tokenizer.KW_FUN).pushRegex(Tokenizer.TK_IDENTIFIER).pushRegex(Tokenizer.LEFT_PAREN).pushRef(FUNCTION_PARAMS_MAYBE_PARSER).pushRegex(Tokenizer.RIGHT_PAREN).pushRegex(Tokenizer.COLON).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeVoidFunction, Rule.STARTER_ROLE, 0)
private val FUNCTION_RULE5: pointer<Rule> = new Rule(new PatternList().pushRef(ANNOTATIONS_MAYBE_PARSER).pushRef(MODIFIER_LIST_MAYBE_PARSER).pushRegex(Tokenizer.KW_FUN).pushRegex(Tokenizer.TK_IDENTIFIER).pushRegex(Tokenizer.LEFT_PAREN).pushRef(FUNCTION_PARAMS_MAYBE_PARSER).pushRegex(Tokenizer.RIGHT_PAREN).pushRef(BLOCK_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeVoidFunctionFromBlock, Rule.STARTER_ROLE, 0)
private val FUNCTION_RULE6: pointer<Rule> = new Rule(new PatternList().pushRef(ANNOTATIONS_MAYBE_PARSER).pushRef(MODIFIER_LIST_MAYBE_PARSER).pushRegex(Tokenizer.KW_FUN).pushRegex(Tokenizer.TK_IDENTIFIER).pushRegex(Tokenizer.LEFT_PAREN).pushRef(FUNCTION_PARAMS_MAYBE_PARSER).pushRegex(Tokenizer.RIGHT_PAREN).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeVoidFunctionFromEmptyBlock, Rule.STARTER_ROLE, 0)

private val STRUCT_CONSTRUCTOR_RULE0: pointer<Rule> = new Rule(new PatternList().pushRef(MODIFIER_LIST_MAYBE_PARSER).pushRegex(Tokenizer.KW_CONSTRUCTOR).pushRegex(Tokenizer.LEFT_PAREN).pushRef(FUNCTION_PARAMS_MAYBE_PARSER).pushRegex(Tokenizer.RIGHT_PAREN).pushRegex(Tokenizer.COLON).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeStructConstructor, Rule.STARTER_ROLE, 0)
private val STRUCT_CONSTRUCTOR_RULE1: pointer<Rule> = new Rule(new PatternList().pushRef(MODIFIER_LIST_MAYBE_PARSER).pushRegex(Tokenizer.KW_CONSTRUCTOR).pushRegex(Tokenizer.LEFT_PAREN).pushRef(FUNCTION_PARAMS_MAYBE_PARSER).pushRegex(Tokenizer.RIGHT_PAREN).pushRef(BLOCK_PARSER).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeStructConstructorFromBlock, Rule.STARTER_ROLE, 0)

private val MEMBER_RULE0: pointer<Rule> = new Rule(new PatternList().pushRef(FIELD_PARSER), makeMemberFromField, Rule.STARTER_ROLE, 0)
private val MEMBER_RULE1: pointer<Rule> = new Rule(new PatternList().pushRef(FUNCTION_PARSER), makeMemberFromFunction, Rule.STARTER_ROLE, 0)
private val MEMBER_RULE2: pointer<Rule> = new Rule(new PatternList().pushRef(STRUCT_CONSTRUCTOR_PARSER), makeMemberFromStructConstructor, Rule.STARTER_ROLE, 0)
private val MEMBER_RULE3: pointer<Rule> = new Rule(new PatternList().pushRef(STRUCT_PARSER), makeMemberFromStruct, Rule.STARTER_ROLE, 0)

private val STRUCT_RULE0: pointer<Rule> = new Rule(new PatternList().pushRef(MODIFIER_LIST_MAYBE_PARSER).pushRegex(Tokenizer.KW_STRUCT).pushRegex(Tokenizer.TK_IDENTIFIER).pushRegex(Tokenizer.LEFT_BRACE).pushRefs(new ParserRefs(MEMBER_PARSER)).pushRegex(Tokenizer.RIGHT_BRACE).pushRegex(Tokenizer.TK_LINE_TERMINATOR), makeStruct, Rule.STARTER_ROLE, 0)

private val PROGRAM_RULE0: pointer<Rule> = new Rule(new PatternList().pushRef(PREPROCESS_SETTINGS_MAYBE_PARSER).pushRef(PACKAGE_DECLARATION_MAYBE_PARSER).pushRef(IMPORT_DECLARATIONS_MAYBE_PARSER).pushRefs(new ParserRefs(MEMBER_PARSER)), makeProgram, Rule.STARTER_ROLE, 0)

private val EXPRESSION_PARSER_SETUP: pointer<ParserRef> = EXPRESSION_PARSER.addRule(EXPRESSION_RULE0).addRule(EXPRESSION_RULE1).addRule(EXPRESSION_RULE2).addRule(EXPRESSION_RULE3).addRule(EXPRESSION_RULE4).addRule(EXPRESSION_RULE5).addRule(EXPRESSION_RULE6).addRule(EXPRESSION_RULE7).addRule(EXPRESSION_RULE8).addRule(EXPRESSION_RULE9).addRule(EXPRESSION_RULE10).addRule(EXPRESSION_RULE11).addRule(EXPRESSION_RULE12).addRule(EXPRESSION_RULE13).addRule(EXPRESSION_RULE14).addRule(EXPRESSION_RULE15).addRule(EXPRESSION_RULE16).addRule(EXPRESSION_RULE17).addRule(EXPRESSION_RULE18).addRule(EXPRESSION_RULE19).addRule(EXPRESSION_RULE20).addRule(EXPRESSION_RULE21).addRule(EXPRESSION_RULE22).addRule(EXPRESSION_RULE23).addRule(EXPRESSION_RULE24).addRule(EXPRESSION_RULE25).addRule(EXPRESSION_RULE26).addRule(EXPRESSION_RULE27).addRule(EXPRESSION_RULE28).addRule(EXPRESSION_RULE29).addRule(EXPRESSION_RULE30).addRule(EXPRESSION_RULE31).addRule(EXPRESSION_RULE32).addRule(EXPRESSION_RULE33).addRule(EXPRESSION_RULE34).addRule(EXPRESSION_RULE35).addRule(EXPRESSION_RULE36).addRule(EXPRESSION_RULE37).addRule(EXPRESSION_RULE38).addRule(EXPRESSION_RULE39).addRule(EXPRESSION_RULE40).addRule(EXPRESSION_RULE41).addRule(EXPRESSION_RULE42).addRule(EXPRESSION_RULE43).addRule(EXPRESSION_RULE44).addRule(EXPRESSION_RULE45).addRule(EXPRESSION_RULE46).addRule(EXPRESSION_RULE47).addRule(EXPRESSION_RULE48).addRule(EXPRESSION_RULE49).addRule(EXPRESSION_RULE50).addRule(EXPRESSION_RULE51).addRule(EXPRESSION_RULE52).addRule(EXPRESSION_RULE53).addRule(EXPRESSION_RULE54).addRule(EXPRESSION_RULE55).addRule(EXPRESSION_RULE56).addRule(EXPRESSION_RULE57).addRule(EXPRESSION_RULE58).addRule(EXPRESSION_RULE59).addRule(EXPRESSION_RULE60).addRule(EXPRESSION_RULE61).addRule(EXPRESSION_RULE62).addRule(EXPRESSION_RULE63).addRule(EXPRESSION_RULE64).addRule(EXPRESSION_RULE65).addRule(EXPRESSION_RULE66).addRule(EXPRESSION_RULE67).addRule(EXPRESSION_RULE68).addRule(EXPRESSION_RULE69).addRule(EXPRESSION_RULE70).addRule(EXPRESSION_RULE71).addRule(EXPRESSION_RULE72)
private val ATOM_PARSER_SETUP: pointer<ParserRef> = ATOM_PARSER.addRule(ATOM_RULE0).addRule(ATOM_RULE1).addRule(ATOM_RULE2).addRule(ATOM_RULE3).addRule(ATOM_RULE4).addRule(ATOM_RULE5).addRule(ATOM_RULE6).addRule(ATOM_RULE7).addRule(ATOM_RULE8).addRule(ATOM_RULE9).addRule(ATOM_RULE10)
private val SIZE_OF_PARSER_SETUP: pointer<ParserRef> = SIZE_OF_PARSER.addRule(SIZE_OF_RULE0)
private val EXPRESSION_TUPLE_PARSER_SETUP: pointer<ParserRef> = EXPRESSION_TUPLE_PARSER.addRule(EXPRESSION_TUPLE_RULE0)
private val LIST_LITERAL_PARSER_SETUP: pointer<ParserRef> = LIST_LITERAL_PARSER.addRule(LIST_LITERAL_RULE0)
private val STATEMENT_PARSER_SETUP: pointer<ParserRef> = STATEMENT_PARSER.addRule(STATEMENT_RULE0).addRule(STATEMENT_RULE1).addRule(STATEMENT_RULE2).addRule(STATEMENT_RULE3).addRule(STATEMENT_RULE4).addRule(STATEMENT_RULE5).addRule(STATEMENT_RULE6).addRule(STATEMENT_RULE7).addRule(STATEMENT_RULE8).addRule(STATEMENT_RULE9)
private val ELSE_STATEMENT_PARSER_SETUP: pointer<ParserRef> = ELSE_STATEMENT_PARSER.addRule(ELSE_STATEMENT_RULE0).addRule(ELSE_STATEMENT_RULE1).addRule(ELSE_STATEMENT_RULE2)
private val WHILE_STATEMENT_PARSER_SETUP: pointer<ParserRef> = WHILE_STATEMENT_PARSER.addRule(WHILE_STATEMENT_RULE0).addRule(WHILE_STATEMENT_RULE1).addRule(WHILE_STATEMENT_RULE2).addRule(WHILE_STATEMENT_RULE3).addRule(WHILE_STATEMENT_RULE4).addRule(WHILE_STATEMENT_RULE5).addRule(WHILE_STATEMENT_RULE6).addRule(WHILE_STATEMENT_RULE7)
private val FOR_HEADER_PARSER_SETUP: pointer<ParserRef> = FOR_HEADER_PARSER.addRule(FOR_HEADER_RULE0).addRule(FOR_HEADER_RULE1).addRule(FOR_HEADER_RULE2).addRule(FOR_HEADER_RULE3).addRule(FOR_HEADER_RULE4).addRule(FOR_HEADER_RULE5).addRule(FOR_HEADER_RULE6).addRule(FOR_HEADER_RULE7)
private val FOR_STATEMENT_PARSER_SETUP: pointer<ParserRef> = FOR_STATEMENT_PARSER.addRule(FOR_STATEMENT_RULE0).addRule(FOR_STATEMENT_RULE1).addRule(FOR_STATEMENT_RULE2).addRule(FOR_STATEMENT_RULE3).addRule(FOR_STATEMENT_RULE4)
private val VARIABLE_DEFINE_PARSER_SETUP: pointer<ParserRef> = VARIABLE_DEFINE_PARSER.addRule(VARIABLE_DEFINE_RULE0).addRule(VARIABLE_DEFINE_RULE1).addRule(VARIABLE_DEFINE_RULE2).addRule(VARIABLE_DEFINE_RULE3)
private val RETURN_STATEMENT_PARSER_SETUP: pointer<ParserRef> = RETURN_STATEMENT_PARSER.addRule(RETURN_STATEMENT_RULE0).addRule(RETURN_STATEMENT_RULE1)
private val BLOCK_PARSER_SETUP: pointer<ParserRef> = BLOCK_PARSER.addRule(BLOCK_RULE0).addRule(BLOCK_RULE1)
private val IF_EXPRESSION_PARSER_SETUP: pointer<ParserRef> = IF_EXPRESSION_PARSER.addRule(IF_EXPRESSION_RULE0).addRule(IF_EXPRESSION_RULE1)
private val IF_ELSE_EXPRESSION_PARSER_SETUP: pointer<ParserRef> = IF_ELSE_EXPRESSION_PARSER.addRule(IF_ELSE_EXPRESSION_RULE0).addRule(IF_ELSE_EXPRESSION_RULE1).addRule(IF_ELSE_EXPRESSION_RULE2).addRule(IF_ELSE_EXPRESSION_RULE3).addRule(IF_ELSE_EXPRESSION_RULE4).addRule(IF_ELSE_EXPRESSION_RULE5).addRule(IF_ELSE_EXPRESSION_RULE6).addRule(IF_ELSE_EXPRESSION_RULE7).addRule(IF_ELSE_EXPRESSION_RULE8).addRule(IF_ELSE_EXPRESSION_RULE9).addRule(IF_ELSE_EXPRESSION_RULE10).addRule(IF_ELSE_EXPRESSION_RULE11)
private val MODIFIER_PARSER_SETUP: pointer<ParserRef> = MODIFIER_PARSER.addRule(MODIFIER_RULE0).addRule(MODIFIER_RULE1).addRule(MODIFIER_RULE2).addRule(MODIFIER_RULE3).addRule(MODIFIER_RULE4).addRule(MODIFIER_RULE5).addRule(MODIFIER_RULE6).addRule(MODIFIER_RULE7)
private val MODIFIER_LIST_MAYBE_PARSER_SETUP: pointer<ParserRef> = MODIFIER_LIST_MAYBE_PARSER.addRule(MODIFIER_LIST_MAYBE_RULE0).addRule(MODIFIER_LIST_MAYBE_RULE1)
private val PREPROCESS_SETTING_PARSER_SETUP: pointer<ParserRef> = PREPROCESS_SETTING_PARSER.addRule(PREPROCESS_SETTING_RULE0)
private val PREPROCESS_SETTINGS_MAYBE_PARSER_SETUP: pointer<ParserRef> = PREPROCESS_SETTINGS_MAYBE_PARSER.addRule(PREPROCESS_SETTINGS_MAYBE_RULE0).addRule(PREPROCESS_SETTINGS_MAYBE_RULE1)
private val ANNOTATION_PARSER_SETUP: pointer<ParserRef> = ANNOTATION_PARSER.addRule(ANNOTATION_RULE0).addRule(ANNOTATION_RULE1)
private val ANNOTATIONS_MAYBE_PARSER_SETUP: pointer<ParserRef> = ANNOTATIONS_MAYBE_PARSER.addRule(ANNOTATIONS_MAYBE_RULE0).addRule(ANNOTATIONS_MAYBE_RULE1)
private val QUALIFIED_NAME_PARSER_SETUP: pointer<ParserRef> = QUALIFIED_NAME_PARSER.addRule(QUALIFIED_NAME_RULE0).addRule(QUALIFIED_NAME_RULE1)
private val PACKAGE_DECLARATION_MAYBE_PARSER_SETUP: pointer<ParserRef> = PACKAGE_DECLARATION_MAYBE_PARSER.addRule(PACKAGE_DECLARATION_MAYBE_RULE0).addRule(PACKAGE_DECLARATION_MAYBE_RULE1)
private val NAMESPACE_IMPORT_PARSER_SETUP: pointer<ParserRef> = NAMESPACE_IMPORT_PARSER.addRule(NAMESPACE_IMPORT_RULE0)
private val IMPORT_DECLARATION_PARSER_SETUP: pointer<ParserRef> = IMPORT_DECLARATION_PARSER.addRule(IMPORT_DECLARATION_RULE0)
private val IMPORT_DECLARATIONS_MAYBE_PARSER_SETUP: pointer<ParserRef> = IMPORT_DECLARATIONS_MAYBE_PARSER.addRule(IMPORT_DECLARATIONS_MAYBE_RULE0)
private val FUNCTION_PARAM_PARSER_SETUP: pointer<ParserRef> = FUNCTION_PARAM_PARSER.addRule(FUNCTION_PARAM_RULE0).addRule(FUNCTION_PARAM_RULE1)
private val FUNCTION_PARAMS_PARSER_SETUP: pointer<ParserRef> = FUNCTION_PARAMS_PARSER.addRule(FUNCTION_PARAMS_RULE0).addRule(FUNCTION_PARAMS_RULE1)
private val FUNCTION_PARAMS_MAYBE_PARSER_SETUP: pointer<ParserRef> = FUNCTION_PARAMS_MAYBE_PARSER.addRule(FUNCTION_PARAMS_MAYBE_RULE0).addRule(FUNCTION_PARAMS_MAYBE_RULE1)
private val FIELD_PARSER_SETUP: pointer<ParserRef> = FIELD_PARSER.addRule(FIELD_RULE0).addRule(FIELD_RULE1).addRule(FIELD_RULE2).addRule(FIELD_RULE3)
private val FUNCTION_PARSER_SETUP: pointer<ParserRef> = FUNCTION_PARSER.addRule(FUNCTION_RULE0).addRule(FUNCTION_RULE1).addRule(FUNCTION_RULE2).addRule(FUNCTION_RULE3).addRule(FUNCTION_RULE4).addRule(FUNCTION_RULE5).addRule(FUNCTION_RULE6)
private val STRUCT_CONSTRUCTOR_PARSER_SETUP: pointer<ParserRef> = STRUCT_CONSTRUCTOR_PARSER.addRule(STRUCT_CONSTRUCTOR_RULE0).addRule(STRUCT_CONSTRUCTOR_RULE1)
private val MEMBER_PARSER_SETUP: pointer<ParserRef> = MEMBER_PARSER.addRule(MEMBER_RULE0).addRule(MEMBER_RULE1).addRule(MEMBER_RULE2).addRule(MEMBER_RULE3)
private val STRUCT_PARSER_SETUP: pointer<ParserRef> = STRUCT_PARSER.addRule(STRUCT_RULE0)
private val PROGRAM_PARSER_SETUP: pointer<ParserRef> = PROGRAM_PARSER.addRule(PROGRAM_RULE0)


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

fun parseSizeOf(input: pointer<TokenList>) -> pointer<SizeOf>
{
    if input == null:
        return null

    if SIZE_OF_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = SIZE_OF_PARSER.getResult()

    if result == null || result.isKind(SIZE_OF_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<SizeOf>
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

fun parseElseStatement(input: pointer<TokenList>) -> pointer<ElseStatement>
{
    if input == null:
        return null

    if ELSE_STATEMENT_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = ELSE_STATEMENT_PARSER.getResult()

    if result == null || result.isKind(ELSE_STATEMENT_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<ElseStatement>
}

fun parseWhileStatement(input: pointer<TokenList>) -> pointer<WhileStatement>
{
    if input == null:
        return null

    if WHILE_STATEMENT_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = WHILE_STATEMENT_PARSER.getResult()

    if result == null || result.isKind(WHILE_STATEMENT_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<WhileStatement>
}

fun parseForHeader(input: pointer<TokenList>) -> pointer<ForHeader>
{
    if input == null:
        return null

    if FOR_HEADER_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = FOR_HEADER_PARSER.getResult()

    if result == null || result.isKind(FOR_HEADER_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<ForHeader>
}

fun parseForStatement(input: pointer<TokenList>) -> pointer<ForStatement>
{
    if input == null:
        return null

    if FOR_STATEMENT_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = FOR_STATEMENT_PARSER.getResult()

    if result == null || result.isKind(FOR_STATEMENT_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<ForStatement>
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

fun parseBlock(input: pointer<TokenList>) -> pointer<Block>
{
    if input == null:
        return null

    if BLOCK_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = BLOCK_PARSER.getResult()

    if result == null || result.isKind(BLOCK_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<Block>
}

fun parseIfExpression(input: pointer<TokenList>) -> pointer<IfExpression>
{
    if input == null:
        return null

    if IF_EXPRESSION_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = IF_EXPRESSION_PARSER.getResult()

    if result == null || result.isKind(IF_EXPRESSION_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<IfExpression>
}

fun parseIfElseExpression(input: pointer<TokenList>) -> pointer<IfElseExpression>
{
    if input == null:
        return null

    if IF_ELSE_EXPRESSION_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = IF_ELSE_EXPRESSION_PARSER.getResult()

    if result == null || result.isKind(IF_ELSE_EXPRESSION_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<IfElseExpression>
}

fun parseModifier(input: pointer<TokenList>) -> pointer<Modifier>
{
    if input == null:
        return null

    if MODIFIER_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = MODIFIER_PARSER.getResult()

    if result == null || result.isKind(MODIFIER_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<Modifier>
}

fun parseModifierListMaybe(input: pointer<TokenList>) -> pointer<ModifierListMaybe>
{
    if input == null:
        return null

    if MODIFIER_LIST_MAYBE_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = MODIFIER_LIST_MAYBE_PARSER.getResult()

    if result == null || result.isKind(MODIFIER_LIST_MAYBE_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<ModifierListMaybe>
}

fun parsePreprocessSetting(input: pointer<TokenList>) -> pointer<PreprocessSetting>
{
    if input == null:
        return null

    if PREPROCESS_SETTING_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = PREPROCESS_SETTING_PARSER.getResult()

    if result == null || result.isKind(PREPROCESS_SETTING_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<PreprocessSetting>
}

fun parsePreprocessSettingsMaybe(input: pointer<TokenList>) -> pointer<PreprocessSettingsMaybe>
{
    if input == null:
        return null

    if PREPROCESS_SETTINGS_MAYBE_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = PREPROCESS_SETTINGS_MAYBE_PARSER.getResult()

    if result == null || result.isKind(PREPROCESS_SETTINGS_MAYBE_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<PreprocessSettingsMaybe>
}

fun parseAnnotation(input: pointer<TokenList>) -> pointer<Annotation>
{
    if input == null:
        return null

    if ANNOTATION_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = ANNOTATION_PARSER.getResult()

    if result == null || result.isKind(ANNOTATION_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<Annotation>
}

fun parseAnnotationsMaybe(input: pointer<TokenList>) -> pointer<AnnotationsMaybe>
{
    if input == null:
        return null

    if ANNOTATIONS_MAYBE_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = ANNOTATIONS_MAYBE_PARSER.getResult()

    if result == null || result.isKind(ANNOTATIONS_MAYBE_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<AnnotationsMaybe>
}

fun parseQualifiedName(input: pointer<TokenList>) -> pointer<QualifiedName>
{
    if input == null:
        return null

    if QUALIFIED_NAME_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = QUALIFIED_NAME_PARSER.getResult()

    if result == null || result.isKind(QUALIFIED_NAME_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<QualifiedName>
}

fun parsePackageDeclarationMaybe(input: pointer<TokenList>) -> pointer<PackageDeclarationMaybe>
{
    if input == null:
        return null

    if PACKAGE_DECLARATION_MAYBE_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = PACKAGE_DECLARATION_MAYBE_PARSER.getResult()

    if result == null || result.isKind(PACKAGE_DECLARATION_MAYBE_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<PackageDeclarationMaybe>
}

fun parseNamespaceImport(input: pointer<TokenList>) -> pointer<NamespaceImport>
{
    if input == null:
        return null

    if NAMESPACE_IMPORT_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = NAMESPACE_IMPORT_PARSER.getResult()

    if result == null || result.isKind(NAMESPACE_IMPORT_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<NamespaceImport>
}

fun parseImportDeclaration(input: pointer<TokenList>) -> pointer<ImportDeclaration>
{
    if input == null:
        return null

    if IMPORT_DECLARATION_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = IMPORT_DECLARATION_PARSER.getResult()

    if result == null || result.isKind(IMPORT_DECLARATION_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<ImportDeclaration>
}

fun parseImportDeclarationsMaybe(input: pointer<TokenList>) -> pointer<ImportDeclarationsMaybe>
{
    if input == null:
        return null

    if IMPORT_DECLARATIONS_MAYBE_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = IMPORT_DECLARATIONS_MAYBE_PARSER.getResult()

    if result == null || result.isKind(IMPORT_DECLARATIONS_MAYBE_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<ImportDeclarationsMaybe>
}

fun parseFunctionParam(input: pointer<TokenList>) -> pointer<FunctionParam>
{
    if input == null:
        return null

    if FUNCTION_PARAM_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = FUNCTION_PARAM_PARSER.getResult()

    if result == null || result.isKind(FUNCTION_PARAM_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<FunctionParam>
}

fun parseFunctionParams(input: pointer<TokenList>) -> pointer<FunctionParams>
{
    if input == null:
        return null

    if FUNCTION_PARAMS_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = FUNCTION_PARAMS_PARSER.getResult()

    if result == null || result.isKind(FUNCTION_PARAMS_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<FunctionParams>
}

fun parseFunctionParamsMaybe(input: pointer<TokenList>) -> pointer<FunctionParamsMaybe>
{
    if input == null:
        return null

    if FUNCTION_PARAMS_MAYBE_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = FUNCTION_PARAMS_MAYBE_PARSER.getResult()

    if result == null || result.isKind(FUNCTION_PARAMS_MAYBE_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<FunctionParamsMaybe>
}

fun parseField(input: pointer<TokenList>) -> pointer<Field>
{
    if input == null:
        return null

    if FIELD_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = FIELD_PARSER.getResult()

    if result == null || result.isKind(FIELD_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<Field>
}

fun parseFunction(input: pointer<TokenList>) -> pointer<Function>
{
    if input == null:
        return null

    if FUNCTION_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = FUNCTION_PARSER.getResult()

    if result == null || result.isKind(FUNCTION_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<Function>
}

fun parseStructConstructor(input: pointer<TokenList>) -> pointer<StructConstructor>
{
    if input == null:
        return null

    if STRUCT_CONSTRUCTOR_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = STRUCT_CONSTRUCTOR_PARSER.getResult()

    if result == null || result.isKind(STRUCT_CONSTRUCTOR_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<StructConstructor>
}

fun parseMember(input: pointer<TokenList>) -> pointer<Member>
{
    if input == null:
        return null

    if MEMBER_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = MEMBER_PARSER.getResult()

    if result == null || result.isKind(MEMBER_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<Member>
}

fun parseStruct(input: pointer<TokenList>) -> pointer<Struct>
{
    if input == null:
        return null

    if STRUCT_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = STRUCT_PARSER.getResult()

    if result == null || result.isKind(STRUCT_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<Struct>
}

fun parseProgram(input: pointer<TokenList>) -> pointer<Program>
{
    if input == null:
        return null

    if PROGRAM_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = PROGRAM_PARSER.getResult()

    if result == null || result.isKind(PROGRAM_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<Program>
}


