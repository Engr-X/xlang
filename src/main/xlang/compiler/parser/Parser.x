@file.class("Parser")
package xlang.compiler.parser


import xlang.Operation
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


private val ATOM_PARSER_ID: int = 1
private val EXPRESSION_PARSER_ID: int = 2
private val SEXPRESSION_PARSER_ID: int = 3
private val EXPRESSION_TUPLE_PARSER_ID: int = 4


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
    val expressionSlot: pointer<pointer<*>> = results.get(1) as pointer<pointer<*>>
    val container: pointer<ParseContainer> = expressionSlot.deref as pointer<ParseContainer>
    val expression: pointer<Expression> = container.getValue() as pointer<Expression>

    return new SExpression(expression)
}

private fun makeExpressionTuple0(results: pointer<ArrayList>) -> pointer<*> = new ExpressionTuple()

private fun makeExpressionTuple1(results: pointer<ArrayList>) -> pointer<*>
{
    val expressionSlot: pointer<pointer<*>> = results.get(1) as pointer<pointer<*>>
    val expressionContainer: pointer<ParseContainer> = expressionSlot.deref as pointer<ParseContainer>
    val firstExpression: pointer<Expression> = expressionContainer.getValue() as pointer<Expression>
    val sExpressionsSlot: pointer<pointer<*>> = results.get(2) as pointer<pointer<*>>
    val sExpressionsContainer: pointer<ParseContainer> = sExpressionsSlot.deref as pointer<ParseContainer>
    val sExpressions: pointer<ArrayList> = sExpressionsContainer.getValue() as pointer<ArrayList>
    val expressions: pointer<ArrayList> = SExpression.unwrap(sExpressions)

    expressions.pushFront(firstExpression.ref)

    return new ExpressionTuple(expressions)
}

private fun makeExpressionFromAtom(results: pointer<ArrayList>) -> pointer<*>
{
    val slot: pointer<pointer<*>> = results.get(0) as pointer<pointer<*>>
    val container: pointer<ParseContainer> = slot.deref as pointer<ParseContainer>
    val atom: pointer<Atom> = container.getValue() as pointer<Atom>

    return Expression.fromAtom(atom)
}

private fun makeFunction(results: pointer<ArrayList>) -> pointer<*>
{
    val nameSlot: pointer<pointer<*>> = results.get(0) as pointer<pointer<*>>
    val nameToken: pointer<Token> = nameSlot.deref as pointer<Token>
    val tupleSlot: pointer<pointer<*>> = results.get(1) as pointer<pointer<*>>
    val tupleContainer: pointer<ParseContainer> = tupleSlot.deref as pointer<ParseContainer>
    val tuple: pointer<ExpressionTuple> = tupleContainer.getValue() as pointer<ExpressionTuple>

    val call: pointer<MethodCall> = new MethodCall(null, nameToken.text).setArguments(tuple)

    return Expression.fromMethodCall(call)
}

private fun makeExpressionFromParenthesis(results: pointer<ArrayList>) -> pointer<*>
{
    val slot: pointer<pointer<*>> = results.get(1) as pointer<pointer<*>>
    val container: pointer<ParseContainer> = slot.deref as pointer<ParseContainer>

    return container.getValue()
}

private fun makeExpressionFromPrefix(results: pointer<ArrayList>) -> pointer<*>
{
    val opSlot: pointer<pointer<*>> = results.get(0) as pointer<pointer<*>>
    val expressionSlot: pointer<pointer<*>> = results.get(2) as pointer<pointer<*>>

    val op: pointer<Operation> = opSlot.deref as pointer<Operation>
    val container: pointer<ParseContainer> = expressionSlot.deref as pointer<ParseContainer>

    val expression: pointer<Expression> = container.getValue() as pointer<Expression>

    return Expression.fromPrefix(op, expression)
}

private fun toOperation(token: pointer<Token>) -> pointer<Operation> = if token == null:
        null
    elif token.kind == Tokenizer.DOUBLE_STAR:
        EXPRESSION_OPERATION3
    elif token.kind == Tokenizer.STAR:
        EXPRESSION_OPERATION4
    elif token.kind == Tokenizer.SLASH:
        EXPRESSION_OPERATION5
    elif token.kind == Tokenizer.PERCENT:
        EXPRESSION_OPERATION6
    elif token.kind == Tokenizer.PLUS:
        EXPRESSION_OPERATION7
    elif token.kind == Tokenizer.MINUS:
        EXPRESSION_OPERATION8
    else:
        null

private fun makeExpressionFromBinary(results: pointer<ArrayList>) -> pointer<*>
{
    val opSlot: pointer<pointer<*>> = results.get(1) as pointer<pointer<*>>
    val leftSlot: pointer<pointer<*>> = results.get(0) as pointer<pointer<*>>
    val rightSlot: pointer<pointer<*>> = results.get(2) as pointer<pointer<*>>

    val leftContainer: pointer<ParseContainer> = leftSlot.deref as pointer<ParseContainer>
    val opToken: pointer<Token> = opSlot.deref as pointer<Token>
    val op: pointer<Operation> = toOperation(opToken)
    val rightContainer: pointer<ParseContainer> = rightSlot.deref as pointer<ParseContainer>

    if op == null:
        return null

    val left: pointer<Expression> = leftContainer.getValue() as pointer<Expression>
    val right: pointer<Expression> = rightContainer.getValue() as pointer<Expression>

    return Expression.fromBinary(op, left, right)
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

val ATOM_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(ATOM_PARSER_ID).addRule(ATOM_RULE0).addRule(ATOM_RULE1).addRule(ATOM_RULE2).addRule(ATOM_RULE3).addRule(ATOM_RULE4).addRule(ATOM_RULE5).addRule(ATOM_RULE6).addRule(ATOM_RULE7).addRule(ATOM_RULE8).addRule(ATOM_RULE9).addRule(ATOM_RULE10)

val EXPRESSION_OPERATION0: pointer<Operation> = new Operation(0, "$paren", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 220, null)
val EXPRESSION_OPERATION1: pointer<Operation> = new Operation(1, "+", Operation.PREFIX_TYPE, Operation.RIGHT_ASSOC, 200, "pos")
val EXPRESSION_OPERATION2: pointer<Operation> = new Operation(2, "-", Operation.PREFIX_TYPE, Operation.RIGHT_ASSOC, 200, "neg")
val EXPRESSION_OPERATION3: pointer<Operation> = new Operation(3, "**", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 190, "pow")
val EXPRESSION_OPERATION4: pointer<Operation> = new Operation(4, "*", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 180, "times")
val EXPRESSION_OPERATION5: pointer<Operation> = new Operation(5, "/", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 180, "div")
val EXPRESSION_OPERATION6: pointer<Operation> = new Operation(6, "%", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 180, "rem")
val EXPRESSION_OPERATION7: pointer<Operation> = new Operation(7, "+", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 170, "plus")
val EXPRESSION_OPERATION8: pointer<Operation> = new Operation(8, "-", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 170, "minus")
private val EXPRESSION_PARSER_SPECIFIC: pointer<PrattParser> = new PrattParser()
val EXPRESSION_PARSER: pointer<ParserRef> = ParserRef.fromPratt(EXPRESSION_PARSER_ID, EXPRESSION_PARSER_SPECIFIC)

private val EXPRESSION_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.TK_IDENTIFIER).pushRef(EXPRESSION_TUPLE_PARSER), makeFunction, Rule.STARTER_ROLE, 240)
private val EXPRESSION_RULE1: pointer<Rule> = new Rule(new PatternList().pushRef(ATOM_PARSER), makeExpressionFromAtom, Rule.STARTER_ROLE, 230)
private val EXPRESSION_RULE2: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_PAREN).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.RIGHT_PAREN), makeExpressionFromParenthesis, Rule.STARTER_ROLE, EXPRESSION_OPERATION0)
private val EXPRESSION_RULE3: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.PLUS).pushRef(EXPRESSION_PARSER), makeExpressionFromPrefix, Rule.STARTER_ROLE, EXPRESSION_OPERATION1)
private val EXPRESSION_RULE4: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.MINUS).pushRef(EXPRESSION_PARSER), makeExpressionFromPrefix, Rule.STARTER_ROLE, EXPRESSION_OPERATION2)
private val EXPRESSION_RULE5: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOUBLE_STAR).pushRef(EXPRESSION_PARSER), makeExpressionFromBinary, Rule.CONTINUATION_ROLE, EXPRESSION_OPERATION3)
private val EXPRESSION_RULE6: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.STAR).pushRef(EXPRESSION_PARSER), makeExpressionFromBinary, Rule.CONTINUATION_ROLE, EXPRESSION_OPERATION4)
private val EXPRESSION_RULE7: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.SLASH).pushRef(EXPRESSION_PARSER), makeExpressionFromBinary, Rule.CONTINUATION_ROLE, EXPRESSION_OPERATION5)
private val EXPRESSION_RULE8: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.PERCENT).pushRef(EXPRESSION_PARSER), makeExpressionFromBinary, Rule.CONTINUATION_ROLE, EXPRESSION_OPERATION6)
private val EXPRESSION_RULE9: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.PLUS).pushRef(EXPRESSION_PARSER), makeExpressionFromBinary, Rule.CONTINUATION_ROLE, EXPRESSION_OPERATION7)
private val EXPRESSION_RULE10: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.MINUS).pushRef(EXPRESSION_PARSER), makeExpressionFromBinary, Rule.CONTINUATION_ROLE, EXPRESSION_OPERATION8)
private val EXPRESSION_PARSER_SETUP: pointer<ParserRef> = EXPRESSION_PARSER.addRule(EXPRESSION_RULE0).addRule(EXPRESSION_RULE1).addRule(EXPRESSION_RULE2).addRule(EXPRESSION_RULE3).addRule(EXPRESSION_RULE4).addRule(EXPRESSION_RULE5).addRule(EXPRESSION_RULE6).addRule(EXPRESSION_RULE7).addRule(EXPRESSION_RULE8).addRule(EXPRESSION_RULE9).addRule(EXPRESSION_RULE10)

private val SEXPRESSION_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.COMMA).pushRef(EXPRESSION_PARSER), makeSExpression, Rule.STARTER_ROLE, 0)

val SEXPRESSION_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(SEXPRESSION_PARSER_ID).addRule(SEXPRESSION_RULE0)

private val EXPRESSION_TUPLE_RULE0: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_PAREN).pushRegex(Tokenizer.RIGHT_PAREN), makeExpressionTuple0, Rule.STARTER_ROLE, 0)
private val EXPRESSION_TUPLE_RULE1: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_PAREN).pushRef(EXPRESSION_PARSER).pushRefs(new ParserRefs(SEXPRESSION_PARSER)).pushRegex(Tokenizer.RIGHT_PAREN), makeExpressionTuple1, Rule.STARTER_ROLE, 0)
private val EXPRESSION_TUPLE_RULE2: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_PAREN).pushRef(EXPRESSION_PARSER).pushRefs(new ParserRefs(SEXPRESSION_PARSER)).pushRegex(Tokenizer.COMMA).pushRegex(Tokenizer.RIGHT_PAREN), makeExpressionTuple1, Rule.STARTER_ROLE, 0)

val EXPRESSION_TUPLE_PARSER: pointer<ParserRef> = ParserRef.fromRecursiveDown(EXPRESSION_TUPLE_PARSER_ID).addRule(EXPRESSION_TUPLE_RULE0).addRule(EXPRESSION_TUPLE_RULE1).addRule(EXPRESSION_TUPLE_RULE2)


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


