@file.class("Parser")
package xlang.compiler.parser


import xlang.compiler.Operation
import xlang.compiler.lexer.Tokenizer
import xlang.lexer.PatternList
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.parser.Parser
import xlang.parser.RecursiveParser
import xlang.util.ArrayList


fun atomParserParse(input: pointer<TokenList>, index: int) -> int = ATOM_PARSER.parse(input, index)

fun atomParserLastTrySuccess() -> bool = ATOM_PARSER.lastTrySuccess()

fun atomParserGetResult() -> pointer<*> = ATOM_PARSER.getResult()

fun atomParserDoParse(input: pointer<TokenList>) -> int = ATOM_PARSER.doParse(input)

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

    return null
}

private fun makeExpressionFromAtom(results: pointer<ArrayList>) -> pointer<*>
{
    val slot: pointer<pointer<*>> = results.get(0) as pointer<pointer<*>>
    val atom: pointer<Atom> = slot.deref as pointer<Atom>

    return Expression.fromAtom(atom)
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

    val left: pointer<Expression> = leftSlot.deref as pointer<Expression>
    val opToken: pointer<Token> = opSlot.deref as pointer<Token>
    val right: pointer<Expression> = rightSlot.deref as pointer<Expression>
    val op: pointer<Operation> = operationFromToken(opToken)

    if op == null:
        return null

    return Expression.fromBinary(op, left, right)
}


private val ATOM_RULE0: pointer<PatternList> = new PatternList().pushRegex(Tokenizer.KW_NULL)
private val ATOM_RULE1: pointer<PatternList> = new PatternList().pushRegex(Tokenizer.KW_TRUE)
private val ATOM_RULE2: pointer<PatternList> = new PatternList().pushRegex(Tokenizer.KW_FALSE)
private val ATOM_RULE3: pointer<PatternList> = new PatternList().pushRegex(Tokenizer.TK_CHAR)
private val ATOM_RULE4: pointer<PatternList> = new PatternList().pushRegex(Tokenizer.TK_STRING)
private val ATOM_RULE5: pointer<PatternList> = new PatternList().pushRegex(Tokenizer.TK_INTEGER)
private val ATOM_RULE6: pointer<PatternList> = new PatternList().pushRegex(Tokenizer.TK_LONG)
private val ATOM_RULE7: pointer<PatternList> = new PatternList().pushRegex(Tokenizer.TK_FLOAT)
private val ATOM_RULE8: pointer<PatternList> = new PatternList().pushRegex(Tokenizer.TK_DOUBLE)
private val ATOM_RULE9: pointer<PatternList> = new PatternList().pushRegex(Tokenizer.TK_LONG_DOUBLE)

private val ATOM_PARSER_SPECIFIC: pointer<RecursiveParser> = new RecursiveParser(makeAtom).addRule(ATOM_RULE0).addRule(ATOM_RULE1).addRule(ATOM_RULE2).addRule(ATOM_RULE3).addRule(ATOM_RULE4).addRule(ATOM_RULE5).addRule(ATOM_RULE6).addRule(ATOM_RULE7).addRule(ATOM_RULE8).addRule(ATOM_RULE9)
val ATOM_PARSER: pointer<Parser> = new Parser(Parser.RECURSIVE_DOWN, ATOM_PARSER_SPECIFIC)

val EXPRESSION_OPERATION0: pointer<Operation> = new Operation(0, "$paren", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, -1, null)
val EXPRESSION_OPERATION1: pointer<Operation> = new Operation(1, "+", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 10, "plus")
val EXPRESSION_OPERATION2: pointer<Operation> = new Operation(2, "-", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 10, "minus")
val EXPRESSION_OPERATION3: pointer<Operation> = new Operation(3, "*", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 20, "times")
val EXPRESSION_OPERATION4: pointer<Operation> = new Operation(4, "/", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 20, "div")
val EXPRESSION_OPERATION5: pointer<Operation> = new Operation(5, "%", Operation.INFIX_TYPE, Operation.LEFT_ASSOC, 20, "mod")
// private val EXPRESSION_RULE0: pointer<PrattRule> = new PrattRule(new PatternList().pushRef(ATOM_PARSER), null, makeExpressionFromAtom)
// private val EXPRESSION_RULE1: pointer<PrattRule> = new PrattRule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.PLUS).pushRef(EXPRESSION_PARSER), EXPRESSION_OPERATION1, makeExpressionFromBinary)
// private val EXPRESSION_RULE2: pointer<PrattRule> = new PrattRule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.MINUS).pushRef(EXPRESSION_PARSER), EXPRESSION_OPERATION2, makeExpressionFromBinary)
// private val EXPRESSION_RULE3: pointer<PrattRule> = new PrattRule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.STAR).pushRef(EXPRESSION_PARSER), EXPRESSION_OPERATION3, makeExpressionFromBinary)
// private val EXPRESSION_RULE4: pointer<PrattRule> = new PrattRule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.SLASH).pushRef(EXPRESSION_PARSER), EXPRESSION_OPERATION4, makeExpressionFromBinary)
// private val EXPRESSION_RULE5: pointer<PrattRule> = new PrattRule(new PatternList().pushRef(EXPRESSION_PARSER).pushRegex(Tokenizer.PERCENT).pushRef(EXPRESSION_PARSER), EXPRESSION_OPERATION5, makeExpressionFromBinary)

// private val EXPRESSION_PARSER_SPECIFIC: pointer<PrattParser> = new PrattParser().addRule(EXPRESSION_RULE0).addRule(EXPRESSION_RULE1).addRule(EXPRESSION_RULE2).addRule(EXPRESSION_RULE3).addRule(EXPRESSION_RULE4).addRule(EXPRESSION_RULE5)
// val EXPRESSION_PARSER: pointer<Parser> = new Parser(Parser.PRATT, EXPRESSION_PARSER_SPECIFIC)


fun parseAtom(input: pointer<TokenList>) -> pointer<Atom>
{
    if input == null:
        return null

    if ATOM_PARSER.doParse(input) < 0:
        return null

    return ATOM_PARSER.getResult() as pointer<Atom>
}

// fun parseExpression(input: pointer<TokenList>) -> pointer<Expression>
// {
//     if input == null:
//         return null

//     if EXPRESSION_PARSER.doParse(input) < 0:
//         return null

//     return EXPRESSION_PARSER.getResult() as pointer<Expression>
// }


