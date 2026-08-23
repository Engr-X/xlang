@file.class("Parser")
package xlang.compiler.parser


import xlang.Operation
import xlang.compiler.Type
import xlang.compiler.lexer.Tokenizer
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


private val ATOM_PARSER_ID: int = 1
private val EXPRESSION_PARSER_ID: int = 2
private val SEXPRESSION_PARSER_ID: int = 3
private val EXPRESSION_TUPLE_PARSER_ID: int = 4
private val LIST_LITERAL_PARSER_ID: int = 5


private fun makeAtom(results: pointer<ArrayList>) -> pointer<*>
{
    val slot: pointer<pointer<*>> = results.get(0) as pointer<pointer<*>>
    val token: pointer<Token> = slot.deref as pointer<Token>

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

private fun makeSExpression(results: pointer<ArrayList>) -> pointer<*>
{
    val commaSlot: pointer<pointer<*>> = results.get(0) as pointer<pointer<*>>
    val commaToken: pointer<Token> = commaSlot.deref as pointer<Token>
    val expressionSlot: pointer<pointer<*>> = results.get(1) as pointer<pointer<*>>
    val container: pointer<ParseContainer> = expressionSlot.deref as pointer<ParseContainer>
    val expression: pointer<Expression> = container.getValue() as pointer<Expression>

    return new SExpression(expression).addExtraToken(commaToken)
}

private fun makeExpressionTuple0(results: pointer<ArrayList>) -> pointer<*>
{
    val leftParenSlot: pointer<pointer<*>> = results.get(0) as pointer<pointer<*>>
    val rightParenSlot: pointer<pointer<*>> = results.get(1) as pointer<pointer<*>>
    val leftParen: pointer<Token> = leftParenSlot.deref as pointer<Token>
    val rightParen: pointer<Token> = rightParenSlot.deref as pointer<Token>

    return new ExpressionTuple().addExtraToken(leftParen).addExtraToken(rightParen)
}

private fun makeExpressionTuple(results: pointer<ArrayList>, rightParenIndex: int) -> pointer<ExpressionTuple>
{
    val leftParenSlot: pointer<pointer<*>> = results.get(0) as pointer<pointer<*>>
    val leftParen: pointer<Token> = leftParenSlot.deref as pointer<Token>
    val expressionSlot: pointer<pointer<*>> = results.get(1) as pointer<pointer<*>>
    val expressionContainer: pointer<ParseContainer> = expressionSlot.deref as pointer<ParseContainer>
    val firstExpression: pointer<Expression> = expressionContainer.getValue() as pointer<Expression>
    val sExpressionsSlot: pointer<pointer<*>> = results.get(2) as pointer<pointer<*>>
    val sExpressionsContainer: pointer<ParseContainer> = sExpressionsSlot.deref as pointer<ParseContainer>
    val sExpressions: pointer<ArrayList> = sExpressionsContainer.getValue() as pointer<ArrayList>
    val rightParenSlot: pointer<pointer<*>> = results.get(rightParenIndex) as pointer<pointer<*>>
    val rightParen: pointer<Token> = rightParenSlot.deref as pointer<Token>
    val expressions: pointer<ArrayList> = SExpression.unwrap(sExpressions)
    val extraTokens: pointer<ArrayList> = SExpression.unwrapExtraTokens(sExpressions)

    expressions.pushFront(firstExpression.ref)

    return new ExpressionTuple(expressions).addExtraTokens(extraTokens).addExtraToken(leftParen).addExtraToken(rightParen)
}

private fun makeExpressionTuple1(results: pointer<ArrayList>) -> pointer<*> = makeExpressionTuple(results, 3)

private fun makeExpressionTuple2(results: pointer<ArrayList>) -> pointer<*>
{
    val tuple: pointer<ExpressionTuple> = makeExpressionTuple(results, 4)
    val commaSlot: pointer<pointer<*>> = results.get(3) as pointer<pointer<*>>
    val commaToken: pointer<Token> = commaSlot.deref as pointer<Token>

    return tuple.addExtraToken(commaToken)
}

private fun makeListLiteral0(results: pointer<ArrayList>) -> pointer<*>
{
    val leftBracketSlot: pointer<pointer<*>> = results.get(0) as pointer<pointer<*>>
    val rightBracketSlot: pointer<pointer<*>> = results.get(1) as pointer<pointer<*>>
    val leftBracket: pointer<Token> = leftBracketSlot.deref as pointer<Token>
    val rightBracket: pointer<Token> = rightBracketSlot.deref as pointer<Token>

    return new ListLiteral().addExtraToken(leftBracket).addExtraToken(rightBracket)
}

private fun makeListLiteral(results: pointer<ArrayList>, rightBracketIndex: int) -> pointer<ListLiteral>
{
    val leftBracketSlot: pointer<pointer<*>> = results.get(0) as pointer<pointer<*>>
    val leftBracket: pointer<Token> = leftBracketSlot.deref as pointer<Token>
    val expressionSlot: pointer<pointer<*>> = results.get(1) as pointer<pointer<*>>
    val expressionContainer: pointer<ParseContainer> = expressionSlot.deref as pointer<ParseContainer>
    val firstExpression: pointer<Expression> = expressionContainer.getValue() as pointer<Expression>
    val sExpressionsSlot: pointer<pointer<*>> = results.get(2) as pointer<pointer<*>>
    val sExpressionsContainer: pointer<ParseContainer> = sExpressionsSlot.deref as pointer<ParseContainer>
    val sExpressions: pointer<ArrayList> = sExpressionsContainer.getValue() as pointer<ArrayList>
    val rightBracketSlot: pointer<pointer<*>> = results.get(rightBracketIndex) as pointer<pointer<*>>
    val rightBracket: pointer<Token> = rightBracketSlot.deref as pointer<Token>
    val expressions: pointer<ArrayList> = SExpression.unwrap(sExpressions)
    val extraTokens: pointer<ArrayList> = SExpression.unwrapExtraTokens(sExpressions)

    expressions.pushFront(firstExpression.ref)

    return new ListLiteral(expressions).addExtraTokens(extraTokens).addExtraToken(leftBracket).addExtraToken(rightBracket)
}

private fun makeListLiteral1(results: pointer<ArrayList>) -> pointer<*> = makeListLiteral(results, 3)

private fun makeListLiteral2(results: pointer<ArrayList>) -> pointer<*>
{
    val list: pointer<ListLiteral> = makeListLiteral(results, 4)
    val commaSlot: pointer<pointer<*>> = results.get(3) as pointer<pointer<*>>
    val commaToken: pointer<Token> = commaSlot.deref as pointer<Token>

    return list.addExtraToken(commaToken)
}

private fun makeExpressionFromAtom(results: pointer<ArrayList>) -> pointer<*>
{
    val slot: pointer<pointer<*>> = results.get(0) as pointer<pointer<*>>
    val container: pointer<ParseContainer> = slot.deref as pointer<ParseContainer>
    val atom: pointer<Atom> = container.getValue() as pointer<Atom>

    return Expression.fromAtom(atom)
}

private fun makeExpressionFromFunctionCall(results: pointer<ArrayList>) -> pointer<*>
{
    val nameSlot: pointer<pointer<*>> = results.get(0) as pointer<pointer<*>>
    val nameToken: pointer<Token> = nameSlot.deref as pointer<Token>
    val tupleSlot: pointer<pointer<*>> = results.get(1) as pointer<pointer<*>>
    val tupleContainer: pointer<ParseContainer> = tupleSlot.deref as pointer<ParseContainer>
    val tuple: pointer<ExpressionTuple> = tupleContainer.getValue() as pointer<ExpressionTuple>

    val call: pointer<MethodCall> = new MethodCall(null, nameToken.text).setArguments(tuple)
    call.addExtraToken(nameToken)

    return Expression.fromMethodCall(call)
}

private fun makeExpressionFromMethodCall(results: pointer<ArrayList>) -> pointer<*>
{
    val hostSlot: pointer<pointer<*>> = results.get(0) as pointer<pointer<*>>
    val hostContainer: pointer<ParseContainer> = hostSlot.deref as pointer<ParseContainer>
    val host: pointer<Expression> = hostContainer.getValue() as pointer<Expression>
    val dotSlot: pointer<pointer<*>> = results.get(1) as pointer<pointer<*>>
    val dotToken: pointer<Token> = dotSlot.deref as pointer<Token>
    val nameSlot: pointer<pointer<*>> = results.get(2) as pointer<pointer<*>>
    val nameToken: pointer<Token> = nameSlot.deref as pointer<Token>
    val tupleSlot: pointer<pointer<*>> = results.get(3) as pointer<pointer<*>>
    val tupleContainer: pointer<ParseContainer> = tupleSlot.deref as pointer<ParseContainer>
    val tuple: pointer<ExpressionTuple> = tupleContainer.getValue() as pointer<ExpressionTuple>

    val call: pointer<MethodCall> = new MethodCall(host, nameToken.text).setArguments(tuple)
    call.addExtraToken(dotToken).addExtraToken(nameToken)

    return Expression.fromMethodCall(call)
}

private fun makeExpressionFromIndexAccess(results: pointer<ArrayList>) -> pointer<*>
{
    val hostSlot: pointer<pointer<*>> = results.get(0) as pointer<pointer<*>>
    val hostContainer: pointer<ParseContainer> = hostSlot.deref as pointer<ParseContainer>
    val host: pointer<Expression> = hostContainer.getValue() as pointer<Expression>
    val listSlot: pointer<pointer<*>> = results.get(1) as pointer<pointer<*>>
    val listContainer: pointer<ParseContainer> = listSlot.deref as pointer<ParseContainer>
    val list: pointer<ListLiteral> = listContainer.getValue() as pointer<ListLiteral>

    val access: pointer<IndexAccess> = new IndexAccess(host, list)

    return Expression.fromIndexAccess(access)
}

private fun makeExpressionFromFieldAccess(results: pointer<ArrayList>) -> pointer<*>
{
    val hostSlot: pointer<pointer<*>> = results.get(0) as pointer<pointer<*>>
    val hostContainer: pointer<ParseContainer> = hostSlot.deref as pointer<ParseContainer>
    val host: pointer<Expression> = hostContainer.getValue() as pointer<Expression>
    val dotSlot: pointer<pointer<*>> = results.get(1) as pointer<pointer<*>>
    val dotToken: pointer<Token> = dotSlot.deref as pointer<Token>
    val fieldSlot: pointer<pointer<*>> = results.get(2) as pointer<pointer<*>>
    val fieldToken: pointer<Token> = fieldSlot.deref as pointer<Token>
    val expression: pointer<Expression> = Expression.fromFieldAccess(host, fieldToken.text)

    return expression.addExtraToken(dotToken).addExtraToken(fieldToken)
}

private fun makeExpressionFromTypeCast(results: pointer<ArrayList>) -> pointer<*>
{
    val expressionSlot: pointer<pointer<*>> = results.get(0) as pointer<pointer<*>>
    val expressionContainer: pointer<ParseContainer> = expressionSlot.deref as pointer<ParseContainer>
    val asSlot: pointer<pointer<*>> = results.get(1) as pointer<pointer<*>>
    val asToken: pointer<Token> = asSlot.deref as pointer<Token>
    val typeSlot: pointer<pointer<*>> = results.get(2) as pointer<pointer<*>>
    val typeContainer: pointer<ParseContainer> = typeSlot.deref as pointer<ParseContainer>

    val expression: pointer<Expression> = expressionContainer.getValue() as pointer<Expression>
    val targetType: pointer<Type> = typeContainer.getValue() as pointer<Type>
    val cast: pointer<TypeCast> = new TypeCast(expression, targetType)
    val result: pointer<Expression> = Expression.fromTypeCast(cast)

    return result.addExtraToken(asToken)
}

private fun makeExpressionFromParenthesis(results: pointer<ArrayList>) -> pointer<*>
{
    val leftParenSlot: pointer<pointer<*>> = results.get(0) as pointer<pointer<*>>
    val expressionSlot: pointer<pointer<*>> = results.get(1) as pointer<pointer<*>>
    val rightParenSlot: pointer<pointer<*>> = results.get(2) as pointer<pointer<*>>
    val leftParen: pointer<Token> = leftParenSlot.deref as pointer<Token>
    val container: pointer<ParseContainer> = expressionSlot.deref as pointer<ParseContainer>
    val rightParen: pointer<Token> = rightParenSlot.deref as pointer<Token>
    val expression: pointer<Expression> = container.getValue() as pointer<Expression>
    val result: pointer<Expression> = expression.clone()

    return result.addExtraToken(leftParen).addExtraToken(rightParen)
}

private fun makeExpressionFromPrefix(results: pointer<ArrayList>) -> pointer<*>
{
    val opSlot: pointer<pointer<*>> = results.get(0) as pointer<pointer<*>>
    val expressionSlot: pointer<pointer<*>> = results.get(1) as pointer<pointer<*>>
    val opToken: pointer<Token> = opSlot.deref as pointer<Token>
    val op: pointer<Operation> = toOperation(opToken, Operation.PREFIX_TYPE)
    val container: pointer<ParseContainer> = expressionSlot.deref as pointer<ParseContainer>

    val expression: pointer<Expression> = container.getValue() as pointer<Expression>
    val result: pointer<Expression> = Expression.fromPrefix(op, expression)

    return result.addExtraToken(opToken)
}

private fun makeExpressionFromInfix(results: pointer<ArrayList>) -> pointer<*>
{
    val leftSlot: pointer<pointer<*>> = results.get(0) as pointer<pointer<*>>
    val opSlot: pointer<pointer<*>> = results.get(1) as pointer<pointer<*>>
    val rightSlot: pointer<pointer<*>> = results.get(2) as pointer<pointer<*>>

    val leftContainer: pointer<ParseContainer> = leftSlot.deref as pointer<ParseContainer>
    val opToken: pointer<Token> = opSlot.deref as pointer<Token>
    val op: pointer<Operation> = toOperation(opToken, Operation.INFIX_TYPE)
    val rightContainer: pointer<ParseContainer> = rightSlot.deref as pointer<ParseContainer>

    val left: pointer<Expression> = leftContainer.getValue() as pointer<Expression>
    val right: pointer<Expression> = rightContainer.getValue() as pointer<Expression>
    val result: pointer<Expression> = Expression.fromBinary(op, left, right)

    return result.addExtraToken(opToken)
}

private fun makeExpressionFromPostfix(results: pointer<ArrayList>) -> pointer<*>
{
    val expressionSlot: pointer<pointer<*>> = results.get(0) as pointer<pointer<*>>
    val opSlot: pointer<pointer<*>> = results.get(1) as pointer<pointer<*>>
    val opToken: pointer<Token> = opSlot.deref as pointer<Token>
    val op: pointer<Operation> = toOperation(opToken, Operation.POSTFIX_TYPE)
    val container: pointer<ParseContainer> = expressionSlot.deref as pointer<ParseContainer>

    val expression: pointer<Expression> = container.getValue() as pointer<Expression>
    val result: pointer<Expression> = Expression.fromPostfix(op, expression)

    return result.addExtraToken(opToken)
}


val ATOM_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(ATOM_PARSER_ID)

private val EXPRESSION_PARSER_SPECIFIC: pointer<PrattParser> = new PrattParser()
val EXPRESSION_PARSER: pointer<ParserRef> = ParserRef.fromPratt(EXPRESSION_PARSER_ID, EXPRESSION_PARSER_SPECIFIC)

val SEXPRESSION_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(SEXPRESSION_PARSER_ID)

val EXPRESSION_TUPLE_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(EXPRESSION_TUPLE_PARSER_ID)

val LIST_LITERAL_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(LIST_LITERAL_PARSER_ID)

val EXPRESSION_OPERATION0: pointer<Operation> = new Operation(0, "$paren", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 220, null)
val EXPRESSION_OPERATION1: pointer<Operation> = new Operation(1, "++", Operation.POSTFIX_TYPE, Operation.LEFT_ASSOC, 210, "succ")
val EXPRESSION_OPERATION2: pointer<Operation> = new Operation(2, "--", Operation.POSTFIX_TYPE, Operation.LEFT_ASSOC, 210, "pred")
val EXPRESSION_OPERATION3: pointer<Operation> = new Operation(3, "++", Operation.PREFIX_TYPE, Operation.LEFT_ASSOC, 200, "inc")
val EXPRESSION_OPERATION4: pointer<Operation> = new Operation(4, "--", Operation.PREFIX_TYPE, Operation.LEFT_ASSOC, 200, "dec")
val EXPRESSION_OPERATION5: pointer<Operation> = new Operation(5, "+", Operation.PREFIX_TYPE, Operation.RIGHT_ASSOC, 200, "pos")
val EXPRESSION_OPERATION6: pointer<Operation> = new Operation(6, "-", Operation.PREFIX_TYPE, Operation.RIGHT_ASSOC, 200, "neg")
val EXPRESSION_OPERATION7: pointer<Operation> = new Operation(7, "**", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 190, "pow")
val EXPRESSION_OPERATION8: pointer<Operation> = new Operation(8, "*", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 180, "times")
val EXPRESSION_OPERATION9: pointer<Operation> = new Operation(9, "/", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 180, "div")
val EXPRESSION_OPERATION10: pointer<Operation> = new Operation(10, "%", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 180, "rem")
val EXPRESSION_OPERATION11: pointer<Operation> = new Operation(11, "+", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 170, "plus")
val EXPRESSION_OPERATION12: pointer<Operation> = new Operation(12, "-", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 170, "minus")
val EXPRESSION_OPERATION13: pointer<Operation> = new Operation(13, "shl", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 160, "shl")
val EXPRESSION_OPERATION14: pointer<Operation> = new Operation(14, "shr", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 160, "shr")
val EXPRESSION_OPERATION15: pointer<Operation> = new Operation(15, "ushr", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 160, "ushr")
val EXPRESSION_OPERATION16: pointer<Operation> = new Operation(16, ">", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 150, "greater")
val EXPRESSION_OPERATION17: pointer<Operation> = new Operation(17, "<", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 150, "less")
val EXPRESSION_OPERATION18: pointer<Operation> = new Operation(18, ">=", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 150, "greaterEqual")
val EXPRESSION_OPERATION19: pointer<Operation> = new Operation(19, "<=", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 150, "lessEqual")

private fun toOperation(token: pointer<Token>, fixity: int) -> pointer<Operation>
{
    if token == null:
        return null

    if token.kind == Tokenizer.LEFT_PAREN && fixity == Operation.INFIX_TYPE:
        return EXPRESSION_OPERATION0

    if token.kind == Tokenizer.PLUS && fixity == Operation.PREFIX_TYPE:
        return EXPRESSION_OPERATION5

    if token.kind == Tokenizer.MINUS && fixity == Operation.PREFIX_TYPE:
        return EXPRESSION_OPERATION6

    if token.kind == Tokenizer.DOUBLE_PLUS && fixity == Operation.PREFIX_TYPE:
        return EXPRESSION_OPERATION3

    if token.kind == Tokenizer.DOUBLE_MINUS && fixity == Operation.PREFIX_TYPE:
        return EXPRESSION_OPERATION4

    if token.kind == Tokenizer.DOUBLE_PLUS && fixity == Operation.POSTFIX_TYPE:
        return EXPRESSION_OPERATION1

    if token.kind == Tokenizer.DOUBLE_MINUS && fixity == Operation.POSTFIX_TYPE:
        return EXPRESSION_OPERATION2

    if token.kind == Tokenizer.DOUBLE_STAR && fixity == Operation.INFIX_TYPE:
        return EXPRESSION_OPERATION7

    if token.kind == Tokenizer.STAR && fixity == Operation.INFIX_TYPE:
        return EXPRESSION_OPERATION8

    if token.kind == Tokenizer.SLASH && fixity == Operation.INFIX_TYPE:
        return EXPRESSION_OPERATION9

    if token.kind == Tokenizer.PERCENT && fixity == Operation.INFIX_TYPE:
        return EXPRESSION_OPERATION10

    if token.kind == Tokenizer.PLUS && fixity == Operation.INFIX_TYPE:
        return EXPRESSION_OPERATION11

    if token.kind == Tokenizer.MINUS && fixity == Operation.INFIX_TYPE:
        return EXPRESSION_OPERATION12

    if token.kind == Tokenizer.KW_SHL && fixity == Operation.INFIX_TYPE:
        return EXPRESSION_OPERATION13

    if token.kind == Tokenizer.KW_SHR && fixity == Operation.INFIX_TYPE:
        return EXPRESSION_OPERATION14

    if token.kind == Tokenizer.KW_USHR && fixity == Operation.INFIX_TYPE:
        return EXPRESSION_OPERATION15

    if token.kind == Tokenizer.GREATER && fixity == Operation.INFIX_TYPE:
        return EXPRESSION_OPERATION16

    if token.kind == Tokenizer.LESS && fixity == Operation.INFIX_TYPE:
        return EXPRESSION_OPERATION17

    return null
}

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

private val EXPRESSION_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.TK_IDENTIFIER).pushRef(EXPRESSION_TUPLE_PARSER), makeExpressionFromFunctionCall, Rule.STARTER_ROLE, 240)
private val EXPRESSION_RULE1: pointer<Rule> = new Rule(new PatternList().pushRef(ATOM_PARSER), makeExpressionFromAtom, Rule.STARTER_ROLE, 230)
private val EXPRESSION_RULE2: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_PAREN).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.RIGHT_PAREN), makeExpressionFromParenthesis, Rule.STARTER_ROLE, EXPRESSION_OPERATION0)
private val EXPRESSION_RULE3: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.PLUS).pushRef(EXPRESSION_PARSER), makeExpressionFromPrefix, Rule.STARTER_ROLE, EXPRESSION_OPERATION5)
private val EXPRESSION_RULE4: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.MINUS).pushRef(EXPRESSION_PARSER), makeExpressionFromPrefix, Rule.STARTER_ROLE, EXPRESSION_OPERATION6)
private val EXPRESSION_RULE5: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.DOUBLE_PLUS).pushRef(EXPRESSION_PARSER), makeExpressionFromPrefix, Rule.STARTER_ROLE, EXPRESSION_OPERATION3)
private val EXPRESSION_RULE6: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.DOUBLE_MINUS).pushRef(EXPRESSION_PARSER), makeExpressionFromPrefix, Rule.STARTER_ROLE, EXPRESSION_OPERATION4)
private val EXPRESSION_RULE7: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRef(LIST_LITERAL_PARSER), makeExpressionFromIndexAccess, Rule.CONTINUATION_ROLE, 230)
private val EXPRESSION_RULE8: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOT).pushRegex(Tokenizer.TK_IDENTIFIER).pushRef(EXPRESSION_TUPLE_PARSER), makeExpressionFromMethodCall, Rule.CONTINUATION_ROLE, 230)
private val EXPRESSION_RULE9: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOT).pushRegex(Tokenizer.TK_IDENTIFIER), makeExpressionFromFieldAccess, Rule.CONTINUATION_ROLE, 220)
private val EXPRESSION_RULE10: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOUBLE_PLUS), makeExpressionFromPostfix, Rule.CONTINUATION_ROLE, EXPRESSION_OPERATION1)
private val EXPRESSION_RULE11: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOUBLE_MINUS), makeExpressionFromPostfix, Rule.CONTINUATION_ROLE, EXPRESSION_OPERATION2)
private val EXPRESSION_RULE12: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_AS).pushRef(TYPE_PARSER), makeExpressionFromTypeCast, Rule.CONTINUATION_ROLE, 200)
private val EXPRESSION_RULE13: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOUBLE_STAR).pushRef(EXPRESSION_PARSER), makeExpressionFromInfix, Rule.CONTINUATION_ROLE, EXPRESSION_OPERATION7)
private val EXPRESSION_RULE14: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.STAR).pushRef(EXPRESSION_PARSER), makeExpressionFromInfix, Rule.CONTINUATION_ROLE, EXPRESSION_OPERATION8)
private val EXPRESSION_RULE15: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.SLASH).pushRef(EXPRESSION_PARSER), makeExpressionFromInfix, Rule.CONTINUATION_ROLE, EXPRESSION_OPERATION9)
private val EXPRESSION_RULE16: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.PERCENT).pushRef(EXPRESSION_PARSER), makeExpressionFromInfix, Rule.CONTINUATION_ROLE, EXPRESSION_OPERATION10)
private val EXPRESSION_RULE17: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.PLUS).pushRef(EXPRESSION_PARSER), makeExpressionFromInfix, Rule.CONTINUATION_ROLE, EXPRESSION_OPERATION11)
private val EXPRESSION_RULE18: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.MINUS).pushRef(EXPRESSION_PARSER), makeExpressionFromInfix, Rule.CONTINUATION_ROLE, EXPRESSION_OPERATION12)
private val EXPRESSION_RULE19: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_SHL).pushRef(EXPRESSION_PARSER), makeExpressionFromInfix, Rule.CONTINUATION_ROLE, EXPRESSION_OPERATION13)
private val EXPRESSION_RULE20: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_SHR).pushRef(EXPRESSION_PARSER), makeExpressionFromInfix, Rule.CONTINUATION_ROLE, EXPRESSION_OPERATION14)
private val EXPRESSION_RULE21: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.KW_USHR).pushRef(EXPRESSION_PARSER), makeExpressionFromInfix, Rule.CONTINUATION_ROLE, EXPRESSION_OPERATION15)
private val EXPRESSION_RULE22: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.GREATER).pushRef(EXPRESSION_PARSER), makeExpressionFromInfix, Rule.CONTINUATION_ROLE, EXPRESSION_OPERATION16)
private val EXPRESSION_RULE23: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.LESS).pushRef(EXPRESSION_PARSER), makeExpressionFromInfix, Rule.CONTINUATION_ROLE, EXPRESSION_OPERATION17)

private val SEXPRESSION_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.COMMA).pushRef(EXPRESSION_PARSER), makeSExpression, Rule.STARTER_ROLE, 0)

private val EXPRESSION_TUPLE_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_PAREN).pushRegex(Tokenizer.RIGHT_PAREN), makeExpressionTuple0, Rule.STARTER_ROLE, 0)
private val EXPRESSION_TUPLE_RULE1: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_PAREN).pushRef(EXPRESSION_PARSER).pushRefs(new ParserRefs(SEXPRESSION_PARSER)).pushRegex(Tokenizer.RIGHT_PAREN), makeExpressionTuple1, Rule.STARTER_ROLE, 0)
private val EXPRESSION_TUPLE_RULE2: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_PAREN).pushRef(EXPRESSION_PARSER).pushRefs(new ParserRefs(SEXPRESSION_PARSER)).pushRegex(Tokenizer.COMMA).pushRegex(Tokenizer.RIGHT_PAREN), makeExpressionTuple2, Rule.STARTER_ROLE, 0)

private val LIST_LITERAL_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_BRACKET).pushRegex(Tokenizer.RIGHT_BRACKET), makeListLiteral0, Rule.STARTER_ROLE, 0)
private val LIST_LITERAL_RULE1: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_BRACKET).pushRef(EXPRESSION_PARSER).pushRefs(new ParserRefs(SEXPRESSION_PARSER)).pushRegex(Tokenizer.RIGHT_BRACKET), makeListLiteral1, Rule.STARTER_ROLE, 0)
private val LIST_LITERAL_RULE2: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_BRACKET).pushRef(EXPRESSION_PARSER).pushRefs(new ParserRefs(SEXPRESSION_PARSER)).pushRegex(Tokenizer.COMMA).pushRegex(Tokenizer.RIGHT_BRACKET), makeListLiteral2, Rule.STARTER_ROLE, 0)

private val ATOM_PARSER_SETUP: pointer<ParserRef> = ATOM_PARSER.addRule(ATOM_RULE0).addRule(ATOM_RULE1).addRule(ATOM_RULE2).addRule(ATOM_RULE3).addRule(ATOM_RULE4).addRule(ATOM_RULE5).addRule(ATOM_RULE6).addRule(ATOM_RULE7).addRule(ATOM_RULE8).addRule(ATOM_RULE9).addRule(ATOM_RULE10)
private val EXPRESSION_PARSER_SETUP: pointer<ParserRef> = EXPRESSION_PARSER.addRule(EXPRESSION_RULE0).addRule(EXPRESSION_RULE1).addRule(EXPRESSION_RULE2).addRule(EXPRESSION_RULE3).addRule(EXPRESSION_RULE4).addRule(EXPRESSION_RULE5).addRule(EXPRESSION_RULE6).addRule(EXPRESSION_RULE7).addRule(EXPRESSION_RULE8).addRule(EXPRESSION_RULE9).addRule(EXPRESSION_RULE10).addRule(EXPRESSION_RULE11).addRule(EXPRESSION_RULE12).addRule(EXPRESSION_RULE13).addRule(EXPRESSION_RULE14).addRule(EXPRESSION_RULE15).addRule(EXPRESSION_RULE16).addRule(EXPRESSION_RULE17).addRule(EXPRESSION_RULE18).addRule(EXPRESSION_RULE19).addRule(EXPRESSION_RULE20).addRule(EXPRESSION_RULE21).addRule(EXPRESSION_RULE22).addRule(EXPRESSION_RULE23)
private val SEXPRESSION_PARSER_SETUP: pointer<ParserRef> = SEXPRESSION_PARSER.addRule(SEXPRESSION_RULE0)
private val EXPRESSION_TUPLE_PARSER_SETUP: pointer<ParserRef> = EXPRESSION_TUPLE_PARSER.addRule(EXPRESSION_TUPLE_RULE0).addRule(EXPRESSION_TUPLE_RULE1).addRule(EXPRESSION_TUPLE_RULE2)
private val LIST_LITERAL_PARSER_SETUP: pointer<ParserRef> = LIST_LITERAL_PARSER.addRule(LIST_LITERAL_RULE0).addRule(LIST_LITERAL_RULE1).addRule(LIST_LITERAL_RULE2)


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

fun parseSExpression(input: pointer<TokenList>) -> pointer<SExpression>
{
    if input == null:
        return null

    if SEXPRESSION_PARSER.doParse(input) < 0:
        return null

    val result: pointer<ParseContainer> = SEXPRESSION_PARSER.getResult()

    if result == null || result.isKind(SEXPRESSION_PARSER_ID) == false:
        return null

    return result.getValue() as pointer<SExpression>
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


