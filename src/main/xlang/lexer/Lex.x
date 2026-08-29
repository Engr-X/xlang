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

import xlang.System
import xlang.TokenPosition


/**
 * Represents a position in the source text.
 *
 * A lexical position contains an absolute character offset and
 * human-readable line and column numbers.
 *
 * The offset is zero-based, while the line and column numbers
 * are expected to be one-based.
 *
 * Code that modifies a LexPosition is responsible for keeping
 * offset, line and column consistent with each other.
 */
struct LexPosition
{
    /**
     * Provides the initial position of a source text.
     *
     * The position starts at offset 0, line 1 and column 1.
     *
     * This value points to a shared mutable LexPosition object.
     *
     * Callers should allocate an independent position before modifying it.
     *
     * @warning                 Modifying this object directly changes the shared start position.
     */
    static val START_POSITION: pointer<LexPosition> = new LexPosition(0, 1, 1)

    /**
     * Stores the absolute character offset in the source text.
     *
     * The first character normally has an offset of 0.
     */
    var offset: int

    /**
     * Stores the current line number.
     *
     * Line numbers normally start at 1.
     */
    var line: int

    /**
     * Stores the current column number.
     *
     * Column numbers normally start at 1.
     */
    var column: int


    /**
     * Initializes a lexical position.
     *
     * The given values are stored directly without validation
     * or normalization.
     *
     * Negative offsets or non-positive line and column numbers are
     * accepted by this constructor, although they may not represent
     * a valid source position.
     *
     * The caller is responsible for providing values that match the
     * position rules used by the lexer.
     *
     * @param offset            the absolute character offset in the source text
     * @param line              the line number, normally starting at 1
     * @param column            the column number, normally starting at 1
     */
    fun __init__(offset: int, line: int, column: int)
    {
        this.offset = offset
        this.line = line
        this.column = column
    }


    /**
     * Creates a token position beginning at this lexical position.
     *
     * The current offset, line and column are copied into a newly
     * allocated TokenPosition, and the given value becomes its length.
     *
     * A zero-length token is allowed if it is meaningful to the caller.
     * Negative lengths are not rejected by this function.
     *
     * The caller is responsible for providing a valid token length and
     * managing the lifetime of the returned object.
     *
     * @param length            the length of the token in characters
     *
     * @return                  a newly allocated TokenPosition
     *
     * @note                    This function does not modify the current LexPosition.
     */
    inline fun toTokenPosition(length: int) -> pointer<TokenPosition> = 
        new TokenPosition(this.offset, this.line, this.column, length)
}


/**
 * Stores the current input state of the lexer.
 *
 * The input state contains the source text, its length, the current
 * lexical position and the previously processed character.
 *
 * This structure does not clone or own the source text or position object.
 *
 * The caller must ensure that the referenced text and position remain
 * valid for as long as the LexInput object uses them.
 */
struct LexInput
{
    /**
     * Points to the current position in the source text.
     *
     * The lexer updates this object as characters are consumed.
     *
     * The caller must provide a valid LexPosition pointer.
     */
    var pos: pointer<LexPosition>

    /**
     * Stores the previously processed character.
     *
     * Its initial value is defined by the lexer and may be a null
     * character when no previous character exists.
     */
    var prevChar: char

    /**
     * Points to the source text being processed.
     *
     * The text is referenced directly and is not copied by LexInput.
     *
     * The caller must keep the memory valid while lexical analysis
     * is in progress.
     */
    var text: pointer<char>

    /**
     * Stores the number of characters available in the source text.
     *
     * This value is used to prevent reads beyond the input buffer.
     *
     * It must match the accessible length of the buffer referenced
     * by text.
     */
    var textLength: int


    /**
     * Initializes the input state of the lexer.
     *
     * The given position and text pointers are stored directly without
     * copying their contents.
     *
     * A zero-length input is allowed. The function does not verify that
     * textLength matches the actual size of the source buffer.
     *
     * The caller must provide valid pointers and keep the referenced
     * objects alive while this LexInput is being used.
     *
     * @param pos               the current lexical position
     * @param prevChar          the previously processed character
     * @param text              the source text buffer
     * @param textLength        the accessible length of the source text
     *
     * @warning                 An incorrect text length may cause out-of-bounds memory access.
     */
    fun __init__(pos: pointer<LexPosition>, prevChar: char, text: pointer<char>, textLength: int)
    {
        this.pos = pos
        this.prevChar = prevChar
        this.text = text
        this.textLength = textLength
    }


    /**
     * Writes the current lexical position into an existing token position.
     *
     * The current offset, line and column are copied into dest, and
     * the given value is written as the token length.
     *
     * The destination object is modified in place. No new object is
     * allocated, and a zero or negative length is not rejected.
     *
     * The caller must provide a valid destination pointer and ensure
     * that the given length correctly describes the token.
     *
     * @param dest              the token position to update
     * @param length            the length of the token in characters
     *
     * @note                    This function does not modify the current LexPosition.
     *
     * @warning                 Passing an invalid or null destination pointer causes undefined behavior.
     */
    fun toTokenPosition(dest: pointer<TokenPosition>, length: int)
    {
        dest.line = this.pos.line
        dest.offset = this.pos.offset
        dest.column = this.pos.column
        dest.length = length
    }
}
