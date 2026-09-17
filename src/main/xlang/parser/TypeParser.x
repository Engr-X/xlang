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
 */

#file.outerClass("TypeParser")
package xlang.parser

import xlang.Diagnostic
import xlang.SourceLocation
import xlang.compiler.BlobType
import xlang.compiler.FunctionType
import xlang.compiler.NormalType
import xlang.compiler.Type
import xlang.compiler.lexer.Tokenizer
import xlang.compiler.parser.Parser
import xlang.compiler.parser.expression.Expression
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.lexer.TokenPosition
import xlang.parser.util.ParserRefs
import xlang.parser.util.PatternAtom
import xlang.util.ArrayList


struct TypeParser
{
    private var id: int

    private var error: pointer<Diagnostic>

    private var result: pointer<ParseContainer>

    private var depth: int


    constructor(id: int)
    {
        this.id = id
        this.error = null
        this.result = null
        this.depth = 0
    }


    fun getId() -> int = this.id


    fun parse(tokens: pointer<TokenList>, index: int) -> int
    {
        this.reset()
        val consumed: int = this.parseType(tokens, index)

        if !this.haveError(consumed):
            return consumed

        if tokens == null || index < 0 || index >= tokens.length():
            return -1

        val leftParen: pointer<Token> = tokens.get(index)

        if leftParen.kind != Tokenizer.LEFT_PAREN:
            return -1

        this.reset()
        val innerConsumed: int = this.parse(tokens, index + 1)

        if this.haveError(innerConsumed):
            return -1

        val rightParenIndex: int = index + innerConsumed + 1

        if rightParenIndex >= tokens.length():
            return this.failCannotParseType(tokens, tokens.length() - 1)

        val rightParen: pointer<Token> = tokens.get(rightParenIndex)

        if rightParen.kind != Tokenizer.RIGHT_PAREN:
            return this.failCannotParseType(tokens, rightParenIndex)

        return innerConsumed + 2
    }


    private fun parseType(tokens: pointer<TokenList>, index: int) -> int
    {
        if index >= 0 && index < tokens.length():
        {
            val token: pointer<Token> = tokens.get(index)

            if token.kind == Tokenizer.LEFT_PAREN:
            {
                val consumed: int = this.parseFunction(tokens, index)

                if this.haveError(consumed):
                    return -1

                val functionType: pointer<FunctionType> = this.result.getValue() as pointer<FunctionType>

                this.result = new ParseContainer(this.id, Type.fromFunction(functionType))
                return consumed
            }

            if token.kind == Tokenizer.KW_BLOB:
            {
                val consumed: int = this.parseBlob(tokens, index)

                if this.haveError(consumed):
                    return -1

                val blobType: pointer<BlobType> = this.result.getValue() as pointer<BlobType>

                this.result = new ParseContainer(this.id, Type.fromBlob(blobType))
                return consumed
            }
        }

        val consumed: int = this.parseNormal(tokens, index)

        if this.haveError(consumed):
            return -1

        val normalType: pointer<NormalType> = this.result.getValue() as pointer<NormalType>

        this.result = new ParseContainer(this.id, Type.fromNormal(normalType))
        return consumed
    }


    private fun parseFunction(tokens: pointer<TokenList>, index: int) -> int
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

        val leftParen: pointer<Token> = tokens.get(index)

        if leftParen.kind != Tokenizer.LEFT_PAREN:
            return this.failCannotParseType(tokens, index)

        val parameters: pointer<ArrayList> = new ArrayList(sizeof(Type))
        val extraTokens: pointer<ArrayList> = new ArrayList(sizeof(Token))
        extraTokens.push(leftParen)

        var consumed: int = 1

        if index + consumed >= tokens.length():
            return this.failCannotParseType(tokens, tokens.length() - 1)

        val firstAfterLeftParen: pointer<Token> = tokens.get(index + consumed)

        if firstAfterLeftParen.kind != Tokenizer.RIGHT_PAREN:
        {
            val parameterRefs: pointer<ParserRefs> = new ParserRefs(
                Parser.TYPE_PARSER.clone(),
                new PatternAtom(Tokenizer.COMMA, null),
                false)
            val parameterLength: int = parameterRefs.parse(tokens, index + consumed)

            if parameterLength <= 0:
                return this.failCannotParseType(tokens, index + consumed)

            val parameterResult: pointer<ParseContainer> = parameterRefs.getResult()
            val parameterValues: pointer<ArrayList> = parameterResult.getValue() as pointer<ArrayList>

            for (var i = 0; i < parameterValues.length; i++):
            {
                val slot: pointer<pointer<*>> = parameterValues.get(i) as pointer<pointer<*>>
                val container: pointer<ParseContainer> = slot.deref as pointer<ParseContainer>
                val parameterType: pointer<Type> = container.getValue() as pointer<Type>

                parameters.push(parameterType)
            }

            extraTokens.pushAll(parameterRefs.getExtraTokens())
            consumed += parameterLength

            if index + consumed >= tokens.length():
                return this.failCannotParseType(tokens, tokens.length() - 1)
        }

        val rightParen: pointer<Token> = tokens.get(index + consumed)

        if rightParen.kind != Tokenizer.RIGHT_PAREN:
            return this.failCannotParseType(tokens, index + consumed)

        extraTokens.push(rightParen)
        consumed++

        if index + consumed >= tokens.length():
            return this.failCannotParseType(tokens, index + consumed - 1)

        val arrow: pointer<Token> = tokens.get(index + consumed)

        if arrow.kind != Tokenizer.ARROW:
            return this.failCannotParseType(tokens, index + consumed)

        extraTokens.push(arrow)
        consumed++

        if index + consumed >= tokens.length():
            return this.failCannotParseType(tokens, index + consumed - 1)

        val returnParser: pointer<TypeParser> = this.clone()
        val returnLength: int = returnParser.parse(tokens, index + consumed)

        if returnParser.haveError(returnLength):
        {
            this.error = returnParser.getError()
            return -1
        }

        val returnResult: pointer<ParseContainer> = returnParser.getResult()
        val returnType: pointer<Type> = returnResult.getValue() as pointer<Type>

        consumed += returnLength

        val parsedType: pointer<FunctionType> = new FunctionType(parameters, returnType)

        for (var i = 0; i < extraTokens.length; i++):
        {
            val token: pointer<Token> = extraTokens.get(i) as pointer<Token>
            parsedType.addExtraToken(token)
        }

        this.result = new ParseContainer(this.id, parsedType)
        return consumed
    }


    private fun failCannotParseType(tokens: pointer<TokenList>, index: int) -> int
    {
        var errorIndex: int = index

        if errorIndex < 0:
            errorIndex = 0

        if errorIndex >= tokens.length():
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


    private fun parseNormal(tokens: pointer<TokenList>, index: int) -> int
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

        if token.kind == Tokenizer.STAR:
        {
            if this.depth >= 1:
            {
                val parsedType: pointer<NormalType> = NormalType.voidType().addToken(token)
                this.result = new ParseContainer(this.id, parsedType)
                return 1
            }

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
            val parsedType: pointer<NormalType> = new NormalType(null, token.text, 0).addToken(token)
            this.result = new ParseContainer(this.id, parsedType)
            return 1
        }


        val parsedType: pointer<NormalType> = new NormalType(null, token.text, 0).addToken(token).addToken(nextToken)
        this.depth++
        var consumed: int = 2

        while index + consumed < tokens.length():
        {
            val argumentParser: pointer<TypeParser> = this.clone()
            argumentParser.depth = this.depth
            val argumentLength: int = argumentParser.parse(tokens, index + consumed)

            if argumentParser.haveError(argumentLength):
            {
                this.depth--
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
                this.depth--
                consumed++
                parsedType.addToken(delimiter)
                this.result = new ParseContainer(this.id, parsedType)
                return consumed
            }

            if delimiter.kind != Tokenizer.COMMA:
                break

            parsedType.addToken(delimiter)
            consumed++
        }

        this.depth--
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

        if index + 2 < tokens.length():
        {
            val blobToken: pointer<Token> = tokens.get(index)
            val leftBracket: pointer<Token> = tokens.get(index + 1)

            if blobToken.kind == Tokenizer.KW_BLOB && leftBracket.kind == Tokenizer.LEFT_BRACKET:
            {
                val expressionLength: int = Parser.EXPRESSION_PARSER.parse(tokens, index + 2)
                val expressionResult: pointer<ParseContainer> = Parser.EXPRESSION_PARSER.getResult()
                val rightBracketIndex: int = index + 2 + expressionLength

                if expressionLength > 0 && expressionResult != null && rightBracketIndex < tokens.length():
                {
                    val rightBracket: pointer<Token> = tokens.get(rightBracketIndex)

                    if rightBracket.kind == Tokenizer.RIGHT_BRACKET:
                    {
                        val blobSize: pointer<Expression> = expressionResult.getValue() as pointer<Expression>

                        val parsedType: pointer<BlobType> = new BlobType(blobSize, 0)
                            .addExtraToken(blobToken)
                            .addExtraToken(leftBracket)
                            .addExtraToken(rightBracket)

                        this.result = new ParseContainer(this.id, parsedType)
                        return expressionLength + 3
                    }

                    errorIndex = rightBracketIndex
                }
                else:
                    errorIndex = index + 2
            }
            else:
                errorIndex = index + 1
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
            list.push(token.clone())
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
