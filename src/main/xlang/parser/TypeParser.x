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

package xlang.parser

import xlang.Diagnostic
import xlang.SourceLocation
import xlang.compiler.type.BlobType
import xlang.compiler.type.FunctionType
import xlang.compiler.type.NormalType
import xlang.compiler.type.Type
import xlang.compiler.lexer.Tokenizer
import xlang.compiler.parser.Parser
import xlang.compiler.parser.expression.Expression
import xlang.lexer.Token
import xlang.lexer.TokenList
import xlang.lexer.TokenPosition
import xlang.parser.util.ParserRefs
import xlang.parser.util.PatternAtom
import xlang.util.ArrayList


/**
 * Parses type expressions from a token stream.
 *
 * <p>A {@code TypeParser} recognizes normal types, generic types, function
 * types, blob types, and parenthesized type expressions.
 *
 * <p>The parser stores the most recent parse result and diagnostic. Successful
 * type parsing produces a {@code Type} instance wrapped in a
 * {@code ParseContainer} whose kind is the identifier assigned to this parser.
 *
 * <p>Generic type parsing keeps track of the current nesting depth and may
 * decompose composite greater-than tokens such as {@code >=}, {@code >>=},
 * and {@code >>>=} when they occur at the end of nested generic type
 * arguments.
 */
struct TypeParser
{
    /**
     * The identifier assigned to parse results produced by this parser.
     */
    private var id: int

    /**
     * The diagnostic produced by the current parse operation.
     *
     * <p>This value is {@code null} when no parsing error has been recorded.
     */
    private var error: pointer<Diagnostic>

    /**
     * The most recent type-parsing result.
     *
     * <p>This value is {@code null} when no successful parse result is
     * currently available.
     */
    private var result: pointer<ParseContainer>

    /**
     * The current generic-type nesting depth.
     *
     * <p>This value is used while parsing nested type arguments and affects
     * how certain tokens, such as {@code *}, are interpreted.
     */
    private var depth: int

    /**
     * Creates a type parser with the specified result identifier.
     *
     * <p>No parse result or diagnostic is initially available, and the generic
     * nesting depth is initialized to {@code 0}.
     *
     * @param id                the identifier assigned to parse results produced by this parser
     */
    constructor(id: int)
    {
        this.id = id
        this.error = null
        this.result = null
        this.depth = 0
    }


    /**
     * Returns the identifier assigned to this parser.
     *
     * @return                  the parser identifier
     */
    fun getId() -> int = this.id


    /**
     * Parses a type expression beginning at the specified token index.
     *
     * <p>The parser first attempts to parse the input as a regular type
     * expression. If that fails and the current token is a left parenthesis,
     * the parser attempts to interpret the contents as a parenthesized type.
     *
     * <p>A parenthesized type must contain a successfully parsed type followed
     * by a matching right parenthesis.
     *
     * @param tokens            a pointer to the token list to parse
     * @param index             the token index at which parsing begins
     *
     * @return                  the number of consumed tokens, or {@code -1} if parsing fails
     */
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


    /**
     * Parses a type according to the token at the specified position.
     *
     * <p>If the type begins with {@code '('}, the parser attempts to parse a
     * function type. If it begins with the {@code blob} keyword, a blob type
     * is parsed. All other valid type forms are handled as normal types.
     *
     * <p>The concrete parsed representation is converted into a general
     * {@code Type} instance before being stored in {@code result}.
     *
     * @param tokens            a pointer to the token list to parse
     * @param index             the token index at which type parsing begins
     *
     * @return                  the number of consumed tokens, or {@code -1} if parsing fails
     */
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


    /**
     * Parses a function type.
     *
     * <p>A function type has the form:
     *
     * <pre>
     * (parameterType, parameterType, ...) -> returnType
     * </pre>
     *
     * <p>The parameter list may be empty. Parameter types are parsed using
     * {@code Parser.TYPE_PARSER} and are separated by commas. A trailing comma
     * is not accepted.
     *
     * <p>The return type is parsed recursively using a cloned
     * {@code TypeParser}.
     *
     * <p>Structural tokens such as parentheses, commas, and the arrow token are
     * retained as extra tokens of the resulting {@code FunctionType}.
     *
     * @param tokens            a pointer to the token list to parse
     * @param index             the index of the opening parenthesis
     *
     * @return                  the number of consumed tokens, or {@code -1} if the function type
     *                          cannot be parsed
     */
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


    /**
     * Records a diagnostic indicating that a type could not be parsed.
     *
     * <p>The supplied index is clamped to the valid token range and the token
     * at that position is used to construct the diagnostic source location.
     *
     * <p>The resulting diagnostic uses
     * {@code Diagnostic.CANNOT_PARSE_TYPE}.
     *
     * @param tokens            a pointer to the token list being parsed
     * @param index             the preferred token index at which the parsing failure
     *                          occurred
     *
     * @return                  {@code -1}
     */
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
            null,
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


    /**
     * Parses a normal or generic type.
     *
     * <p>Normal types may begin with an identifier or one of the supported
     * built-in type keywords.
     *
     * <p>If the type name is not followed by {@code '<'}, a non-generic
     * {@code NormalType} is produced immediately.
     *
     * <p>If {@code '<'} follows the type name, one or more type arguments are
     * parsed recursively. Type arguments are separated by commas and the list
     * must terminate with a matching {@code '>'}.
     *
     * <p>Nested generic parsing may decompose composite greater-than operators
     * such as {@code >=}, {@code >>=}, and {@code >>>=} so that individual
     * {@code '>'} tokens can terminate nested generic argument lists.
     *
     * <p>The {@code '*'} token is accepted as a special type only when parsing
     * within a nested type context.
     *
     * @param tokens            a pointer to the token list to parse
     * @param index             the token index at which the type begins
     *
     * @return                  the number of consumed tokens, or {@code -1} if parsing fails
     */
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
                null,
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
                null,
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
            null,
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


    /**
     * Parses a blob type.
     *
     * <p>A blob type has the form:
     *
     * <pre>
     * blob[expression]
     * </pre>
     *
     * <p>The expression between the brackets is parsed using
     * {@code Parser.EXPRESSION_PARSER} and represents the blob size.
     *
     * <p>The {@code blob} keyword and both bracket tokens are retained as extra
     * tokens of the resulting {@code BlobType}.
     *
     * @param tokens            a pointer to the token list to parse
     * @param index             the index of the {@code blob} keyword
     *
     * @return                  the number of consumed tokens, or {@code -1} if the blob type
     *                          cannot be parsed
     */
    private fun parseBlob(tokens: pointer<TokenList>, index: int) -> int
    {
        if index < 0 || index >= tokens.length():
            return this.failCannotParseType(tokens, index)

        val blobToken: pointer<Token> = tokens.get(index)

        if blobToken.kind != Tokenizer.KW_BLOB:
            return this.failCannotParseType(tokens, index)

        if index + 1 >= tokens.length():
            return this.failCannotParseType(tokens, index)

        val leftBracket: pointer<Token> = tokens.get(index + 1)

        if leftBracket.kind != Tokenizer.LEFT_BRACKET:
            return this.failCannotParseType(tokens, index + 1)

        if index + 2 >= tokens.length():
            return this.failCannotParseType(tokens, index + 1)

        val expressionLength: int = Parser.EXPRESSION_PARSER.parse(tokens, index + 2)

        if Parser.EXPRESSION_PARSER.haveError(expressionLength) || expressionLength <= 0:
            return this.failCannotParseType(tokens, index + 2)

        val expressionResult: pointer<ParseContainer> = Parser.EXPRESSION_PARSER.getResult()

        if expressionResult == null:
            return this.failCannotParseType(tokens, index + 2)

        val rightBracketIndex: int = index + 2 + expressionLength

        if rightBracketIndex >= tokens.length():
            return this.failCannotParseType(tokens, tokens.length() - 1)

        val rightBracket: pointer<Token> = tokens.get(rightBracketIndex)

        if rightBracket.kind != Tokenizer.RIGHT_BRACKET:
            return this.failCannotParseType(tokens, rightBracketIndex)

        val blobSize: pointer<Expression> = expressionResult.getValue() as pointer<Expression>

        if blobSize == null:
            return this.failCannotParseType(tokens, index + 2)

        val parsedType: pointer<BlobType> = new BlobType(blobSize, 0)
            .addExtraToken(blobToken)
            .addExtraToken(leftBracket)
            .addExtraToken(rightBracket)

        this.result = new ParseContainer(this.id, parsedType)
        return expressionLength + 3
    }


    /**
     * Decomposes a composite greater-than assignment token into individual
     * tokens.
     *
     * <p>The following tokens are decomposed:
     *
     * <ul>
     *     <li>{@code >=} into {@code '>'} and {@code '='}</li>
     *     <li>{@code >>=} into {@code '>'}, {@code '>'}, and {@code '='}</li>
     *     <li>{@code >>>=} into three {@code '>'} tokens followed by
     *         {@code '='}</li>
     * </ul>
     *
     * <p>Each generated token receives a source position derived from the
     * corresponding character position within the original token.
     *
     * <p>If the supplied token is not one of the supported composite tokens,
     * a clone of the original token is returned as the only list element.
     *
     * @param token             a pointer to the token to decompose
     *
     * @return                  a list containing the decomposed tokens
     */
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


    /**
     * Replaces a composite greater-than token in the token list with its
     * decomposed token sequence.
     *
     * <p>If {@code tokens} is {@code null}, {@code index} is invalid, or the
     * selected token does not require decomposition, this method performs no
     * modification.
     *
     * <p>The original token is removed and the generated tokens are inserted at
     * the same position.
     *
     * @param tokens            a pointer to the token list to modify
     * @param index             the index of the token to decompose
     */
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


    /**
     * Parses a type from the beginning of the supplied token list and removes
     * the successfully consumed tokens.
     *
     * <p>If {@code input} is {@code null}, a
     * {@code Diagnostic.NULL_INPUT} internal diagnostic is generated.
     *
     * <p>If parsing fails or produces an error, this method returns
     * {@code -1} and leaves the successfully unconsumed input available to the
     * caller.
     *
     * <p>If the parser reports consuming more tokens than are present in the
     * input, an internal diagnostic is generated.
     *
     * <p>On success, the consumed prefix is removed from {@code input}.
     *
     * @param input             a pointer to the token list to parse and consume
     *
     * @return                  the number of consumed tokens, or {@code -1} if parsing fails
     */
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


    /**
     * Checks whether the supplied parse result represents a failure.
     *
     * <p>A parse is considered erroneous if a diagnostic has been recorded or
     * if the number of consumed tokens is not positive.
     *
     * @param eaten             the number of tokens reported as consumed
     *
     * @return                  {@code true} if parsing failed; {@code false} otherwise
     */
    fun haveError(eaten: int) -> bool = this.error != null || eaten <= 0


    /**
     * Returns the result produced by the most recent successful parse.
     *
     * @return                  a pointer to the current parse result, or {@code null} if no
     *                          successful result is available
     */
    fun getResult() -> pointer<ParseContainer> = this.result


    /**
     * Returns the diagnostic produced by the current parse operation.
     *
     * @return                  a pointer to the current diagnostic, or {@code null} if no error
     *                          has been recorded
     */
    fun getError() -> pointer<Diagnostic> = this.error


    /**
     * Resets the transient state of this parser.
     *
     * <p>The current diagnostic and parse result are cleared. The parser
     * identifier and generic nesting depth are preserved.
     *
     * @return                  this {@code TypeParser} instance
     */
    fun reset() -> pointer<TypeParser>
    {
        this.error = null
        this.result = null
        return this
    }


    /**
     * Creates a new type parser with the same result identifier.
     *
     * <p>Transient state such as the current diagnostic, parse result, and
     * generic nesting depth is not copied.
     *
     * @return                  a pointer to the cloned {@code TypeParser}
     */
    fun clone() -> pointer<TypeParser> = new TypeParser(this.id)
}
