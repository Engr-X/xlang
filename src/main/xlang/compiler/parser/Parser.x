@file.class("Parser")
package xlang.compiler.parser


import xlang.compiler.lexer.Tokenizer
import xlang.lexer.PatternList
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.parser.ParsedObject


private fun parseBoolAtom(tokens: pointer<TokenList>) -> pointer<Atom>
{
    if tokens == null || tokens.length() != 1:
        return null

    val token: pointer<Token> = tokens.get(0)

    if token.kind != Tokenizer.KW_TRUE && token.kind != Tokenizer.KW_FALSE:
        return null

    return new Atom(Atom.BOOL_IMM_KIND, tokens.toArray())
}


private fun parserResultConstructor0(tokens: pointer<TokenList>) -> pointer<*> =
    parseBoolAtom(tokens) as pointer<*>


var parserIsInit: bool = false
val ATOM_PARSER: pointer<ParsedObject> = new ParsedObject(parserResultConstructor0)


private fun parserInit()
{
    val parser0: pointer<ParsedObject> = ATOM_PARSER
    parser0.addRule(new PatternList().push(Tokenizer.KW_TRUE))
    parser0.addRule(new PatternList().push(Tokenizer.KW_FALSE))
    parserIsInit = true
}


fun parseAtom(tokens: pointer<TokenList>) -> pointer<Atom>
{
    if !parserIsInit:
        parserInit()

    if tokens == null:
        return null

    val parser: pointer<ParsedObject> = ATOM_PARSER

    if parser.doParse(tokens) <= 0:
        return null

    return parser.getResult() as pointer<Atom>
}


