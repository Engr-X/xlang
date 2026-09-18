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
 *
 *
 */

package xlang.lexer

import xlang.Diagnostic
import xlang.SourceLocation
import xlang.util.ArrayList
import xlang.util.File
import xlang.util.string.String


/**
 * Represents a tokenized source file.
 *
 * <p>A {@code TokenizedFile} associates a source file path with the list of
 * tokens produced from that file.
 *
 * <p>The file path is duplicated and owned by this structure, while the
 * token list is stored by reference. If a token list is provided, its source
 * path is updated to match the path of this token file.
 */
struct TokenizedFile
{
    /**
     * The duplicated path of the source file.
     */
    private var path: pointer<char>

    /**
     * The complete textual content of the source file.
     *
     * <p>This field stores a pointer to the null-terminated character sequence
     * containing the original source text associated with this token file.
     *
     * <p>The content may be used to recover source fragments, generate diagnostic
     * messages, or perform operations that require access to the original file
     * text in addition to the token stream.
     *
     * <p>The ownership and lifetime of the referenced character buffer depend on
     * how the value is assigned to this field.
     */
    private var content: pointer<char>

    /**
     * The list of tokens associated with this source file.
     *
     * <p>This value may be {@code null} if no tokens are available.
     */
    private var tokens: pointer<TokenList>


    /**
     * Creates a token file from the specified source path, source content,
     * and token list.
     *
     * <p>The supplied file path is duplicated using {@code String.strdup}, so the
     * stored path does not depend on the lifetime of the original path string.
     *
     * <p>The source content is stored by reference and is not duplicated. The
     * referenced character buffer must therefore remain valid for as long as this
     * token file requires access to it.
     *
     * <p>If {@code tokens} is not {@code null}, the duplicated file path is also
     * assigned to the token list as its source path.
     *
     * @param path a pointer to the null-terminated source file path
     * @param content a pointer to the null-terminated source file content
     * @param tokens a pointer to the token list associated with the file,
     *               or {@code null} if no token list is available
     */
    constructor(path: pointer<char>, content: pointer<char>, tokens: pointer<TokenList>)
    {
        this.path = String.strdup(path)
        this.content = content
        this.tokens = tokens
    }

    /**
     * Creates a token file from the specified file.
     *
     * <p>If {@code file} is not {@code null}, its path is duplicated using
     * {@code String.strdup}, while its content pointer is stored by reference.
     * No token list is assigned during construction.
     *
     * <p>If {@code file} is {@code null}, both the path and content are initialized
     * to {@code null}.
     *
     * @param file a pointer to the source file to initialize from,
     *             or {@code null} to create an empty token file
     */
    constructor(file: pointer<File>)
    {
        this.path = if file == null:
                null
            else:
                String.strdup(file.getPath())

        this.content = if file == null:
                null
            else:
                file.getContent()

        this.tokens = null
    }


    /**
     * Returns the lexical error associated with this token file, if one exists.
     *
     * <p>The lexer represents a lexical failure by appending an error token to the
     * end of the token list. An error token is identified by a negative token kind.
     *
     * <p>If the token list is {@code null}, empty, or does not end with an error
     * token, this method returns {@code null}.
     *
     * <p>When an error token is present, a diagnostic is created using the token's
     * source position and error information. If the token does not contain source
     * position information, a default source location at the beginning of the file
     * is used instead.
     *
     * <p>The returned list currently contains a single
     * {@code Diagnostic.UNEXPECTED_TOKEN} error diagnostic.
     *
     * @return a list containing the lexical error diagnostic, or {@code null}
     *         if no lexical error is present
     */
    private fun getError() -> pointer<ArrayList>
    {
        if this.tokens == null || this.tokens.length() <= 0:
            return null

        val token: pointer<Token> = this.tokens.get(this.tokens.length() - 1)

        if token == null || token.kind >= 0:
            return null

        val locations: pointer<ArrayList> = new ArrayList(sizeof(SourceLocation))
        val result: pointer<ArrayList> = new ArrayList(sizeof(Diagnostic))

        if token.pos == null:
            locations.push(new SourceLocation(this.path, 0, 0, 0, 0))
        else:
            locations.push(new SourceLocation(
                this.path,
                token.pos.offset,
                token.pos.line,
                token.pos.column,
                token.pos.length))

        result.push(Diagnostic.makeError(
            Diagnostic.UNEXPECTED_TOKEN,
            locations,
            token.errorInfo))

        return result
    }


    /**
     * Checks whether this token file contains a lexical error.
     *
     * <p>This method determines the result by calling {@code getError()} and
     * checking whether an error diagnostic is available.
     *
     * @return                  {@code true} if a lexical error is present;
     *                          {@code false} otherwise
     */
    fun haveError() -> bool = this.getError() != null


    /**
     * Returns the lexical warnings associated with this token file.
     *
     * <p>Lexical warnings are not currently produced, so this method always
     * returns {@code null}.
     *
     * @return                  {@code null}
     */
    fun getWarning() -> pointer<ArrayList> = null


    fun throw()
    {
    }


    /**
     * Returns the path of the source file.
     *
     * @return                  a pointer to the null-terminated source file path
     */
    fun getPath() -> pointer<char> = this.path


    /**
     * Returns the original textual content of the source file.
     *
     * <p>The returned pointer refers to the null-terminated character buffer
     * associated with this token file. The content is returned by reference and
     * is not copied.
     *
     * @return                  a pointer to the null-terminated source file content
     */
    fun getContent() -> pointer<char> = this.content

    /**
     * Returns the token list associated with this source file.
     *
     * @return                  a pointer to the token list, or {@code null} if no token list
     *                          is available
     */
    fun getTokens() -> pointer<TokenList> = this.tokens


    /**
     * Returns the number of tokens contained in this token file.
     *
     * <p>If no token list is associated with this file, {@code 0} is returned.
     *
     * @return                  the number of tokens in the associated token list
     */
    fun length() -> int = if this.tokens == null: 0 else: this.tokens.length()
}
