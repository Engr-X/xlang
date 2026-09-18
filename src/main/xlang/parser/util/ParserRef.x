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

package xlang.parser.util

import xlang.Diagnostic
import xlang.lexer.TokenList
import xlang.parser.ParseContainer
import xlang.parser.PrattParser
import xlang.parser.RecursiveParser
import xlang.parser.TypeParser


/**
 * Provides a unified reference to one of the parser implementations used by
 * the parser subsystem.
 *
 * <p>A {@code ParserRef} stores a parser identifier, the concrete parser type,
 * and a generic pointer to the underlying parser instance. Operations such as
 * rule registration, parsing, error checking, result retrieval, and cloning are
 * dispatched to the concrete parser according to the stored parser type.
 *
 * <p>The following parser implementations are currently supported:
 * <ul>
 *     <li>{@code RECURSIVE_DOWN} - {@link RecursiveParser}</li>
 *     <li>{@code PRATT} - {@link PrattParser}</li>
 *     <li>{@code TYPE_PARSER} - {@link TypeParser}</li>
 * </ul>
 */
struct ParserRef
{
    /**
     * Identifies a recursive-descent parser.
     */
    static val RECURSIVE_DOWN: int = 0

    /**
     * Identifies a Pratt parser.
     */
    static val PRATT: int = 1

    /**
     * Identifies a type parser.
     */
    static val TYPE_PARSER: int = 2


    /**
     * The identifier associated with this parser reference.
     */
    private var id: int

    /**
     * The type identifier of the underlying parser.
     */
    private var type: int

    /**
     * A pointer to the underlying parser instance.
     *
     * <p>The concrete pointer type is determined by {@code type}.
     */
    private var host: pointer<*>


    /**
     * Creates a parser reference backed by a new recursive-descent parser.
     *
     * @param id                the identifier assigned to the parser
     *
     * @return a                new parser reference containing a {@code RecursiveParser}
     */
    static fun fromRecursiveDown(id: int) -> pointer<ParserRef> =
        new ParserRef(id, RECURSIVE_DOWN, new RecursiveParser(id))


    /**
     * Creates a parser reference backed by the specified Pratt parser.
     *
     * <p>The supplied parser is assigned the specified identifier before being
     * stored in the returned parser reference.
     *
     * @param id                the identifier assigned to the parser
     * @param host              a pointer to the Pratt parser to reference
     *
     * @return                  a new parser reference containing the supplied {@code PrattParser}
     */
    static fun fromPratt(id: int, host: pointer<PrattParser>) -> pointer<ParserRef> =
        new ParserRef(id, PRATT, host.setId(id))


    /**
     * Creates a parser reference backed by a new type parser.
     *
     * @param id                the identifier assigned to the parser
     *
     * @return                  a new parser reference containing a {@code TypeParser}
     */
    static fun fromType(id: int) -> pointer<ParserRef> =
        new ParserRef(id, TYPE_PARSER, new TypeParser(id))


    /**
     * Creates a parser reference for the specified parser implementation.
     *
     * @param id                the identifier assigned to the parser
     * @param type              the type identifier of the underlying parser
     * @param host              a pointer to the underlying parser instance
     */
    private constructor(id: int, type: int, host: pointer<*>)
    {
        this.id = id
        this.type = type
        this.host = host
    }

    /**
     * Returns the identifier of this parser reference.
     *
     * @return                  the parser identifier
     */
    fun getId() -> int = this.id


    /**
     * Adds a parsing rule to the underlying parser.
     *
     * <p>For a recursive-descent parser, the rule is added directly to its rule
     * collection.
     *
     * <p>For a Pratt parser, starter rules are registered as starter rules while
     * all other rules are registered as continuation rules.
     *
     * <p>Type parsers currently do not accept rules through this method.
     *
     * @param rule              a pointer to the rule to add
     *
     * @return                  this {@code ParserRef} instance
     */
    fun addRule(rule: pointer<Rule>) -> pointer<ParserRef>
    {
        if this.type == RECURSIVE_DOWN:
        {
            val parser: pointer<RecursiveParser> = this.host as pointer<RecursiveParser>
            parser.addRule(rule)
        }

        if this.type == PRATT:
        {
            val parser: pointer<PrattParser> = this.host as pointer<PrattParser>

            if rule.role == Rule.STARTER_ROLE:
                parser.addStarterRule(rule)
            else:
                parser.addContinuationRule(rule)
        }

        return this
    }


    /**
     * Parses the token stream starting at the specified index.
     *
     * <p>The parse operation is delegated to the concrete parser referenced by
     * this instance.
     *
     * @param tokens            a pointer to the token list to parse
     * @param index             the token index at which parsing should begin
     *
     * @return                  the number of tokens consumed or the parser-specific parse result;
     *                          {@code -1} if the parser type is invalid
     */
    fun parse(tokens: pointer<TokenList>, index: int) -> int
    {
        if this.type == RECURSIVE_DOWN:
        {
            val parser: pointer<RecursiveParser> = this.host as pointer<RecursiveParser>
            return parser.parse(tokens, index)
        }

        if this.type == PRATT:
        {
            val parser: pointer<PrattParser> = this.host as pointer<PrattParser>
            return parser.parse(tokens, index)
        }

        if this.type == TYPE_PARSER:
        {
            val parser: pointer<TypeParser> = this.host as pointer<TypeParser>
            return parser.parse(tokens, index)
        }

        return -1
    }


    /**
     * Parses tokens from the beginning of the supplied token list and removes
     * the successfully consumed tokens.
     *
     * <p>If {@code input} is {@code null}, parsing fails immediately.
     *
     * <p>After parsing, the result is validated using {@code haveError}. Parsing
     * also fails if the reported number of consumed tokens exceeds the number
     * of available tokens.
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
            return -1

        val consumed: int = this.parse(input, 0)

        if this.haveError(consumed) || consumed > input.length():
            return -1

        input.remove(0, consumed)
        return consumed
    }


    /**
     * Checks whether a parse result represents an error.
     *
     * <p>The check is delegated to the underlying parser implementation.
     *
     * @param eaten             the parser-specific number of consumed tokens or parse result
     *
     * @return                  {@code true} if the parse result represents an error;
     *                          {@code false} otherwise
     */
    fun haveError(eaten: int) -> bool
    {
        if this.type == RECURSIVE_DOWN:
        {
            val parser: pointer<RecursiveParser> = this.host as pointer<RecursiveParser>
            return parser.haveError(eaten)
        }

        if this.type == PRATT:
        {
            val parser: pointer<PrattParser> = this.host as pointer<PrattParser>
            return parser.haveError(eaten)
        }

        if this.type == TYPE_PARSER:
        {
            val parser: pointer<TypeParser> = this.host as pointer<TypeParser>
            return parser.haveError(eaten)
        }

        return true
    }


    /**
     * Returns the parse result produced by the underlying parser.
     *
     * <p>The result is normalized to a {@code ParseContainer} pointer regardless
     * of the concrete parser implementation.
     *
     * @return                  a pointer to the current parse result, or {@code null} if the
     *                          parser type is invalid or no result is available
     */
    fun getResult() -> pointer<ParseContainer>
    {
        if this.type == RECURSIVE_DOWN:
        {
            val parser: pointer<RecursiveParser> = this.host as pointer<RecursiveParser>
            return parser.getResult()
        }

        if this.type == PRATT:
        {
            val parser: pointer<PrattParser> = this.host as pointer<PrattParser>
            return parser.getResult() as pointer<ParseContainer>
        }

        if this.type == TYPE_PARSER:
        {
            val parser: pointer<TypeParser> = this.host as pointer<TypeParser>
            return parser.getResult()
        }

        return null
    }


    /**
     * Returns the most relevant diagnostic produced by the underlying parser.
     *
     * <p>For recursive-descent and type parsers, this method returns their current
     * error diagnostic. For Pratt parsers, the most recent error diagnostic is
     * returned.
     *
     * @return                  a pointer to the parser diagnostic, or {@code null} if no error is
     *                          available or the parser type is invalid
     */
    fun getError() -> pointer<Diagnostic>
    {
        if this.type == RECURSIVE_DOWN:
        {
            val parser: pointer<RecursiveParser> = this.host as pointer<RecursiveParser>
            return parser.getError()
        }

        if this.type == PRATT:
        {
            val parser: pointer<PrattParser> = this.host as pointer<PrattParser>
            return parser.getLastError()
        }

        if this.type == TYPE_PARSER:
        {
            val parser: pointer<TypeParser> = this.host as pointer<TypeParser>
            return parser.getError()
        }

        return null
    }


    /**
     * Returns the underlying parser instance.
     *
     * <p>The returned pointer is untyped. Its concrete type is determined by the
     * parser type stored in this reference.
     *
     * @return                  a pointer to the underlying parser
     */
    fun getHost() -> pointer<*> = this.host


    /**
     * Creates a copy of this parser reference and its underlying parser.
     *
     * <p>The concrete parser is cloned using its own {@code clone} implementation.
     * The returned parser reference preserves the same parser identifier and type.
     *
     * @return                  a pointer to the cloned parser reference, or {@code null} if the
     *                          parser type is invalid
     */
    fun clone() -> pointer<ParserRef>
    {
        if this.type == RECURSIVE_DOWN:
        {
            val parser: pointer<RecursiveParser> = this.host as pointer<RecursiveParser>
            return new ParserRef(this.id, this.type, parser.clone())
        }

        if this.type == PRATT:
        {
            val parser: pointer<PrattParser> = this.host as pointer<PrattParser>
            return new ParserRef(this.id, this.type, parser.clone())
        }

        if this.type == TYPE_PARSER:
        {
            val parser: pointer<TypeParser> = this.host as pointer<TypeParser>
            return new ParserRef(this.id, this.type, parser.clone())
        }

        return null
    }
}
