@file.class("Parser")
package xlang.compiler.parser


import xlang.compiler.lexer.Tokenizer
import xlang.lexer.PatternList
import xlang.lexer.Token
import xlang.parser.ParsedObject
import xlang.util.ArrayList


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


val ATOM_RULE0: pointer<PatternList> = new PatternList().pushRegex(Tokenizer.KW_NULL)
val ATOM_RULE1: pointer<PatternList> = new PatternList().pushRegex(Tokenizer.KW_TRUE)
val ATOM_RULE2: pointer<PatternList> = new PatternList().pushRegex(Tokenizer.KW_FALSE)
val ATOM_RULE3: pointer<PatternList> = new PatternList().pushRegex(Tokenizer.TK_CHAR)
val ATOM_RULE4: pointer<PatternList> = new PatternList().pushRegex(Tokenizer.TK_STRING)
val ATOM_RULE5: pointer<PatternList> = new PatternList().pushRegex(Tokenizer.TK_INTEGER)
val ATOM_RULE6: pointer<PatternList> = new PatternList().pushRegex(Tokenizer.TK_LONG)
val ATOM_RULE7: pointer<PatternList> = new PatternList().pushRegex(Tokenizer.TK_FLOAT)
val ATOM_RULE8: pointer<PatternList> = new PatternList().pushRegex(Tokenizer.TK_DOUBLE)
val ATOM_RULE9: pointer<PatternList> = new PatternList().pushRegex(Tokenizer.TK_LONG_DOUBLE)
val ATOM_PARSER: pointer<ParsedObject> = new ParsedObject(makeAtom).addRule(ATOM_RULE0).addRule(ATOM_RULE1).addRule(ATOM_RULE2).addRule(ATOM_RULE3).addRule(ATOM_RULE4).addRule(ATOM_RULE5).addRule(ATOM_RULE6).addRule(ATOM_RULE7).addRule(ATOM_RULE8).addRule(ATOM_RULE9)


