/*
 * Copyright (c) 2026 Di Wang
 * SPDX-License-Identifier: MIT
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 */

@file.class("TypeParser")
package xlang.parser

import xlang.Diagnostic
import xlang.SourceLocation
import xlang.compiler.Type
import xlang.compiler.lexer.Tokenizer
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.TypeConvert


struct TypeParser
{
    private var id: int

    private var error: pointer<Diagnostic>

    private var result: pointer<ParseContainer>

    private var depth: int


    fun __init__(id: int)
    {
        this.id = id
        this.error = null
        this.result = null
    }


    fun getId() -> int = this.id


    fun parse(tokens: pointer<TokenList>, index: int) -> int
    {
        this.reset()

        if index < 0 || index >= tokens.length():
        {
            this.error = Diagnostic.makeInternalError(
                Diagnostic.EMPTY_INPUT,
                new ArrayList(sizeof(SourceLocation)),
                Diagnostic.EMPTY_INPUT_MSG)
            return -1
        }

        val token: pointer<Token> = tokens.get(index)

        if token.kind == Tokenizer.KW_BLOB:
            return this.parseBlob(tokens, index)

        if token.kind != Tokenizer.TK_IDENTIFIER &&
            token.kind != Tokenizer.KW_BOOL &&
            token.kind != Tokenizer.KW_BYTE &&
            token.kind != Tokenizer.KW_CHAR &&
            token.kind != Tokenizer.KW_DOUBLE &&
            token.kind != Tokenizer.KW_FLOAT &&
            token.kind != Tokenizer.KW_INT &&
            token.kind != Tokenizer.KW_LONG &&
            token.kind != Tokenizer.KW_POINTER &&
            token.kind != Tokenizer.KW_SHORT &&
            token.kind != Tokenizer.KW_VOID:
        {
            val locations: pointer<ArrayList> = new ArrayList(sizeof(SourceLocation))
            val location: pointer<SourceLocation> = new SourceLocation(
                tokens.filePath,
                token.pos.offset,
                token.pos.line,
                token.pos.column,
                token.pos.length)

            locations.push(location)
            this.error = Diagnostic.makeError(
                Diagnostic.CANNOT_PARSE_TYPE,
                locations,
                Diagnostic.CANNOT_PARSE_TYPE_MSG)
            return -1
        }



        var nextToken: pointer<Token> = null

        if index + 1 < tokens.length():
            nextToken = tokens.get(index + 1)

        if nextToken == null || nextToken.kind != Tokenizer.LESS:
        {
            val parsedType: pointer<Type> = new Type(null, token.text, 0)
            this.result = new ParseContainer(this.id, parsedType)
            return 1
        }


        val parsedType: pointer<Type> = new Type(null, token.text, 0)
        var consumed: int = 2

        while index + consumed < tokens.length():
        {
            val argumentParser: pointer<TypeParser> = this.clone()
            val argumentLength: int = argumentParser.parse(tokens, index + consumed)

            if argumentParser.haveError(argumentLength):
            {
                this.error = argumentParser.getError()
                return -1
            }

            val argumentResult: pointer<ParseContainer> = argumentParser.getResult()
            val typeArgument: pointer<Type> = argumentResult.getValue() as pointer<Type>

            parsedType.addTypeArgument(typeArgument)
            consumed += argumentLength

            if index + consumed >= tokens.length():
                break

            var delimiter: pointer<Token> = tokens.get(index + consumed)

            // eat >, >=, >>=, >>>=
            if delimiter.kind == Tokenizer.TRIPLE_GREATER_EQUAL ||
                delimiter.kind == Tokenizer.DOUBLE_GREATER_EQUAL ||
                delimiter.kind == Tokenizer.GREATER_EQUAL:
            {
                this.doComposite(tokens, index + consumed)
                delimiter = tokens.get(index + consumed)
            }

            if delimiter.kind == Tokenizer.GREATER:
            {
                consumed++
                this.result = new ParseContainer(this.id, parsedType)
                return consumed
            }

            if delimiter.kind != Tokenizer.COMMA:
                break

            consumed++
        }

        var errorIndex: int = index + consumed

        if errorIndex >= tokens.length():
            errorIndex = tokens.length() - 1

        val locations: pointer<ArrayList> = new ArrayList(sizeof(SourceLocation))
        val errorToken: pointer<Token> = tokens.get(errorIndex)
        val location: pointer<SourceLocation> = new SourceLocation(
            tokens.filePath,
            errorToken.pos.offset,
            errorToken.pos.line,
            errorToken.pos.column,
            errorToken.pos.length)

        locations.push(location)
        this.error = Diagnostic.makeError(
            Diagnostic.CANNOT_PARSE_TYPE,
            locations,
            Diagnostic.CANNOT_PARSE_TYPE_MSG)

        return -1
    }


    private fun parseBlob(tokens: pointer<TokenList>, index: int) -> int
    {
        var errorIndex: int = index

        if index + 3 < tokens.length():
        {
            val blobToken: pointer<Token> = tokens.get(index)
            val leftBracket: pointer<Token> = tokens.get(index + 1)
            val sizeToken: pointer<Token> = tokens.get(index + 2)
            val rightBracket: pointer<Token> = tokens.get(index + 3)

            if leftBracket.kind != Tokenizer.LEFT_BRACKET:
                errorIndex = index + 1
            elif sizeToken.kind != Tokenizer.TK_INTEGER:
                errorIndex = index + 2
            elif rightBracket.kind != Tokenizer.RIGHT_BRACKET:
                errorIndex = index + 3
            else:
            {
                val size: long = TypeConvert.stringToLong(sizeToken.text)
                val memSize: int = size as int

                if (memSize as long) == size:
                {
                    val parsedType: pointer<Type> = new Type(null, blobToken.text, memSize)
                    this.result = new ParseContainer(this.id, parsedType)
                    return 4
                }

                errorIndex = index + 2
            }
        }
        else:
            errorIndex = tokens.length() - 1

        val errorToken: pointer<Token> = tokens.get(errorIndex)
        val locations: pointer<ArrayList> = new ArrayList(sizeof(SourceLocation))
        val location: pointer<SourceLocation> = new SourceLocation(
            tokens.filePath,
            errorToken.pos.offset,
            errorToken.pos.line,
            errorToken.pos.column,
            errorToken.pos.length)

        locations.push(location)
        this.error = Diagnostic.makeError(
            Diagnostic.CANNOT_PARSE_TYPE,
            locations,
            Diagnostic.CANNOT_PARSE_TYPE_MSG)

        return -1
    }


    private fun decomposeToken(token: pointer<Token>) -> pointer<ArrayList>
    {
        val list: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if token == null:
            return list

        var greaterCount: int = 0

        if token.kind == Tokenizer.TRIPLE_GREATER_EQUAL:
            greaterCount = 3
        elif token.kind == Tokenizer.DOUBLE_GREATER_EQUAL:
            greaterCount = 2
        elif token.kind == Tokenizer.GREATER_EQUAL:
            greaterCount = 1
        else:
        {
            list.push(token.copy())
            return list
        }

        for (var i = 0; i < greaterCount; i++):
        {
            val position: pointer<TokenPosition> = new TokenPosition(
                token.pos.offset + i,
                token.pos.line,
                token.pos.column + i,
                1)
            val greater: pointer<Token> = new Token(Tokenizer.GREATER, position, ">")

            list.push(greater)
        }

        val equalPosition: pointer<TokenPosition> = new TokenPosition(
            token.pos.offset + greaterCount,
            token.pos.line,
            token.pos.column + greaterCount,
            1)
        val equal: pointer<Token> = new Token(Tokenizer.EQUAL, equalPosition, "=")

        list.push(equal)

        return list
    }


    private fun doComposite(tokens: pointer<TokenList>, index: int)
    {
        if tokens == null || index < 0 || index >= tokens.length():
            return

        val decomposed: pointer<ArrayList> = this.decomposeToken(tokens.get(index))

        if decomposed.length <= 1:
            return

        tokens.remove(index, index + 1)
        tokens.addAll(index, decomposed)
    }


    fun doParse(input: pointer<TokenList>) -> int
    {
        if input == null:
        {
            this.reset()
            this.error = Diagnostic.makeInternalError(
                Diagnostic.NULL_INPUT,
                new ArrayList(sizeof(SourceLocation)),
                Diagnostic.NULL_INPUT_MSG)
            return -1
        }

        val consumed: int = this.parse(input, 0)

        if this.haveError(consumed):
            return -1

        if consumed > input.length():
        {
            this.error = Diagnostic.makeInternalError(
                0,
                new ArrayList(sizeof(SourceLocation)),
                "internal error: parser consumed more tokens than input length")
            return -1
        }

        input.remove(0, consumed)
        return consumed
    }


    fun haveError(eaten: int) -> bool = this.error != null || eaten <= 0


    fun getResult() -> pointer<ParseContainer> = this.result


    fun getError() -> pointer<Diagnostic> = this.error


    fun reset() -> pointer<TypeParser>
    {
        this.error = null
        this.result = null
        return this
    }


    fun clone() -> pointer<TypeParser> = new TypeParser(this.id)
}
