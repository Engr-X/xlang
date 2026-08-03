@file.class("Parser")
package xlang.compiler.parser


import xlang.compiler.lexer.Tokenizer
import xlang.lexer.PatternList
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.parser.ParsedObject
import xlang.util.ArrayList


private fun parseAtom(results: pointer<ArrayList>) -> pointer<Atom>
{
    if results == null || results.length != 1:
        return null

    val slot: pointer<pointer<*>> = results.get(0) as pointer<pointer<*>>

    if slot == null:
        return null

    val token: pointer<Token> = slot.deref as pointer<Token>

    if token == null:
        return null

    if token.kind == Tokenizer.KW_NULL:
        return new Atom(Atom.NULL_IMM_KIND, results)

    if token.kind == Tokenizer.KW_TRUE || token.kind == Tokenizer.KW_FALSE:
        return new Atom(Atom.BOOL_IMM_KIND, results)

    if token.kind == Tokenizer.TK_CHAR:
        return new Atom(Atom.CHAR_IMM_KIND, results)

    return null
}


private fun parserResultConstructor0(results: pointer<ArrayList>) -> pointer<*> =
    parseAtom(results) as pointer<*>


var parserIsInit: bool = false
val ATOM_PARSER: pointer<ParsedObject> = new ParsedObject(parserResultConstructor0)


private fun parserInit()
{
    val parser0: pointer<ParsedObject> = ATOM_PARSER
    parser0.addRule(new PatternList().pushRegex(Tokenizer.KW_NULL))
    parser0.addRule(new PatternList().pushRegex(Tokenizer.KW_TRUE))
    parser0.addRule(new PatternList().pushRegex(Tokenizer.KW_FALSE))
    parser0.addRule(new PatternList().pushRegex(Tokenizer.TK_CHAR))
    parserIsInit = true
}


fun parseAtom(tokens: pointer<TokenList>) -> pointer<Atom>
{
    if !parserIsInit:
        parserInit()

    if tokens == null:
        return null

    val parser: pointer<ParsedObject> = ATOM_PARSER

    if parser.parse(tokens, 0) <= 0:
        return null

    return parser.getResult() as pointer<Atom>
}


