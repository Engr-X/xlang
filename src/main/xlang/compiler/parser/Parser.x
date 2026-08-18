@file.class("Parser")
package xlang.compiler.parser


import xlang.Operation
import xlang.compiler.lexer.Tokenizer
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.parser.ParseContainer
import xlang.parser.PrattParser
import xlang.parser.util.Parser
import xlang.parser.util.PatternList
import xlang.parser.util.Rule
import xlang.util.ArrayList


private val ATOM_PARSER_ID: int = 1
private val EXPRESSION_PARSER_ID: int = 2


private fun makeAtom(results: pointer<ArrayList>) -> pointer<*>
{
    val slot: pointer<pointer<*>> = results.get(0) as pointer<pointer<*>>
    val token: pointer<Token> = slot.deref as pointer<Token>

    if token.kind == Tokenizer.KW_NULL:
        return new Atom(Atom.NULL_IMM_KIND, results)

    if token.kind == Tokenizer.KW_TRUE || token.kind == Tokenizer.KW_FALSE:
        return new Atom(Atom.BOOL_IMM_KIND, results)

    if token.kind == Tokenizer.TK_CHAR:
        return new Atom(Atom.CHAR_IMM_KIND, results)

    if token.kind == Tokenizer.TK_STRING:
        return new Atom(Atom.STRING_IMM_KIND, results)

    if token.kind == Tokenizer.TK_INTEGER:
        return new Atom(Atom.INTEGER_IMM_KIND, results)

    if token.kind == Tokenizer.TK_LONG:
        return new Atom(Atom.LONG_IMM_KIND, results)

    if token.kind == Tokenizer.TK_FLOAT:
        return new Atom(Atom.FLOAT_IMM_KIND, results)

    if token.kind == Tokenizer.TK_DOUBLE || token.kind == Tokenizer.TK_LONG_DOUBLE:
        return new Atom(Atom.DOUBLE_IMM_KIND, results)

    if token.kind == Tokenizer.TK_IDENTIFIER:
        return new Atom(Atom.IDENTIFIER_KIND, results)
    return null
}

private fun makeExpressionFromAtom(results: pointer<ArrayList>) -> pointer<*>
{
    val slot: pointer<pointer<*>> = results.get(0) as pointer<pointer<*>>
    val container: pointer<ParseContainer> = slot.deref as pointer<ParseContainer>

    if container == null || container.isKind(ATOM_PARSER_ID) == false:
        return null

    val atom: pointer<Atom> = container.getValue() as pointer<Atom>

    return Expression.fromAtom(atom)
}

private fun makeExpressionFromParenthesis(results: pointer<ArrayList>) -> pointer<*>
{
    val slot: pointer<pointer<*>> = results.get(1) as pointer<pointer<*>>
    val container: pointer<ParseContainer> = slot.deref as pointer<ParseContainer>

    if container == null || container.isKind(EXPRESSION_PARSER_ID) == false:
        return null

    return container.getValue()
}

private fun makeExpressionFromPrefix(results: pointer<ArrayList>) -> pointer<*>
{
    val opSlot: pointer<pointer<*>> = results.get(0) as pointer<pointer<*>>
    val expressionSlot: pointer<pointer<*>> = results.get(1) as pointer<pointer<*>>

    val opToken: pointer<Token> = opSlot.deref as pointer<Token>
    val container: pointer<ParseContainer> = expressionSlot.deref as pointer<ParseContainer>

    val expression: pointer<Expression> = container.getValue() as pointer<Expression>
    val op: pointer<Operation> = operationFromToken(opToken)

    if op == null:
        return null

    return Expression.fromPrefix(op, expression)
}

private fun operationFromToken(token: pointer<Token>) -> pointer<Operation>
{
    if token == null:
        return null

    if token.kind == Tokenizer.PLUS:
        return EXPRESSION_OPERATION1

    if token.kind == Tokenizer.MINUS:
        return EXPRESSION_OPERATION2

    if token.kind == Tokenizer.STAR:
        return EXPRESSION_OPERATION3

    if token.kind == Tokenizer.SLASH:
        return EXPRESSION_OPERATION4

    if token.kind == Tokenizer.PERCENT:
        return EXPRESSION_OPERATION5

    return null
}

private fun makeExpressionFromBinary(results: pointer<ArrayList>) -> pointer<*>
{
    val leftSlot: pointer<pointer<*>> = results.get(0) as pointer<pointer<*>>
    val opSlot: pointer<pointer<*>> = results.get(1) as pointer<pointer<*>>
    val rightSlot: pointer<pointer<*>> = results.get(2) as pointer<pointer<*>>

    val leftContainer: pointer<ParseContainer> = leftSlot.deref as pointer<ParseContainer>
    val opToken: pointer<Token> = opSlot.deref as pointer<Token>
    val rightContainer: pointer<ParseContainer> = rightSlot.deref as pointer<ParseContainer>

    val left: pointer<Expression> = leftContainer.getValue() as pointer<Expression>
    val right: pointer<Expression> = rightContainer.getValue() as pointer<Expression>
    val op: pointer<Operation> = operationFromToken(opToken)

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

val ATOM_PARSER: pointer<Parser> = Parser.fromRecursiveDown(ATOM_PARSER_ID).addRule(ATOM_RULE0).addRule(ATOM_RULE1).addRule(ATOM_RULE2).addRule(ATOM_RULE3).addRule(ATOM_RULE4).addRule(ATOM_RULE5).addRule(ATOM_RULE6).addRule(ATOM_RULE7).addRule(ATOM_RULE8).addRule(ATOM_RULE9).addRule(ATOM_RULE10)

val EXPRESSION_OPERATION0: pointer<Operation> = new Operation(0, "$paren", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 220, null)
val EXPRESSION_OPERATION1: pointer<Operation> = new Operation(1, "**", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 190, "pow")
val EXPRESSION_OPERATION2: pointer<Operation> = new Operation(2, "*", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 180, "times")
val EXPRESSION_OPERATION3: pointer<Operation> = new Operation(3, "/", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 180, "div")
val EXPRESSION_OPERATION4: pointer<Operation> = new Operation(4, "%", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 180, "mod")
val EXPRESSION_OPERATION5: pointer<Operation> = new Operation(5, "+", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 170, "plus")
val EXPRESSION_OPERATION6: pointer<Operation> = new Operation(6, "-", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 170, "minus")
private val EXPRESSION_PARSER_SPECIFIC: pointer<PrattParser> = new PrattParser()
val EXPRESSION_PARSER: pointer<Parser> = Parser.fromPratt(EXPRESSION_PARSER_ID, EXPRESSION_PARSER_SPECIFIC)

private val EXPRESSION_RULE0: pointer<Rule> = new Rule(new PatternList().pushRef(ATOM_PARSER), makeExpressionFromAtom, Rule.STARTER_ROLE, 230)
private val EXPRESSION_RULE1: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.LEFT_PAREN).pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.RIGHT_PAREN), makeExpressionFromParenthesis, Rule.STARTER_ROLE, 220)
private val EXPRESSION_RULE2: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.PLUS).pushRef(EXPRESSION_PARSER), makeExpressionFromPrefix, Rule.STARTER_ROLE, 200)
private val EXPRESSION_RULE3: pointer<Rule> = new Rule(new PatternList().pushRegex(Tokenizer.MINUS).pushRef(EXPRESSION_PARSER), makeExpressionFromPrefix, Rule.STARTER_ROLE, 200)
private val EXPRESSION_RULE4: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.DOUBLE_STAR).pushRef(EXPRESSION_PARSER), makeExpressionFromBinary, Rule.CONTINUATION_ROLE, EXPRESSION_OPERATION1)
private val EXPRESSION_RULE5: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.STAR).pushRef(EXPRESSION_PARSER), makeExpressionFromBinary, Rule.CONTINUATION_ROLE, EXPRESSION_OPERATION2)
private val EXPRESSION_RULE6: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.SLASH).pushRef(EXPRESSION_PARSER), makeExpressionFromBinary, Rule.CONTINUATION_ROLE, EXPRESSION_OPERATION3)
private val EXPRESSION_RULE7: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.PERCENT).pushRef(EXPRESSION_PARSER), makeExpressionFromBinary, Rule.CONTINUATION_ROLE, EXPRESSION_OPERATION4)
private val EXPRESSION_RULE8: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.PLUS).pushRef(EXPRESSION_PARSER), makeExpressionFromBinary, Rule.CONTINUATION_ROLE, EXPRESSION_OPERATION5)
private val EXPRESSION_RULE9: pointer<Rule> = new Rule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.MINUS).pushRef(EXPRESSION_PARSER), makeExpressionFromBinary, Rule.CONTINUATION_ROLE, EXPRESSION_OPERATION6)
private val EXPRESSION_PARSER_SETUP: pointer<Parser> = EXPRESSION_PARSER.addRule(EXPRESSION_RULE0).addRule(EXPRESSION_RULE1).addRule(EXPRESSION_RULE2).addRule(EXPRESSION_RULE3).addRule(EXPRESSION_RULE4).addRule(EXPRESSION_RULE5).addRule(EXPRESSION_RULE6).addRule(EXPRESSION_RULE7).addRule(EXPRESSION_RULE8).addRule(EXPRESSION_RULE9)


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


