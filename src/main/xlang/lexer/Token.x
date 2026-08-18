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
import xlang.parser.util.PatternList
import xlang.util.ArrayList
import xlang.util.string.String
import xlang.util.string.StringBuilder


/**
 * Describes the location and length of a token in the source text.
 *
 * A token position contains both an absolute one-dimensional offset
 * and human-readable line and column numbers.
 *
 * The offset is normally zero-based, while line and column numbers
 * are normally one-based. The length may be zero for synthetic or
 * empty tokens.
 *
 * The caller is responsible for keeping all position values consistent
 * with the source text and the lexer position rules.
 */
struct TokenPosition
{
    /**
     * Stores the absolute one-dimensional offset in the source text.
     *
     * The first character of the source text normally has offset 0.
     */
    var offset: int

    /**
     * Stores the line number at which the token begins.
     *
     * Line numbers normally start at 1.
     */
    var line: int

    /**
     * Stores the column number at which the token begins.
     *
     * Column numbers normally start at 1.
     */
    var column: int

    /**
     * Stores the length of the token in characters.
     *
     * This value describes the token range beginning at offset.
     */
    var length: int


    /**
     * Initializes a token position.
     *
     * The supplied values are stored directly without validation
     * or normalization.
     *
     * Zero-length positions are allowed. Negative values are not
     * rejected, although they may not represent a valid source range.
     *
     * The caller is responsible for providing values that correctly
     * describe the token in the source text.
     *
     * @param offset            the absolute offset at which the token begins
     * @param line              the line number at which the token begins
     * @param column            the column number at which the token begins
     * @param length            the length of the token in characters
     */
    fun __init__(offset: int, line: int, column: int, length: int)
    {
        this.offset = offset
        this.line = line
        this.column = column
        this.length = length
    }   
}


/**
 * Represents a token produced by the lexer.
 *
 * A token contains a kind identifier, a source position, its textual
 * representation and optional error information.
 *
 * Normal token text and error information are duplicated when the token
 * is created. EOF tokens always use the shared EOF_STRING value instead
 * of duplicating the supplied text.
 *
 * The Token object does not copy or own the TokenPosition object.
 * The caller must keep the position valid while the token uses it.
 */
struct Token
{
    /**
     * Identifies the end-of-file token kind.
     */
    static val EOF_KIND: int = 0

    /**
     * Represents a wildcard token kind that may match any concrete kind.
     *
     * This value is the minimum representable 32-bit signed integer.
     *
     * @note                    This constant is intended for matching and lookup operations,
     * not for ordinary lexer output.
     */
    static val AnyKind: int = -2147483647 - 1

    /**
     * Provides the shared textual representation of an EOF token.
     *
     * EOF tokens reference this string directly.
     *
     * @warning                 This shared string must not be modified or released by callers.
     */
    static val EOF_STRING: pointer<char> = "<EOF>"

    /**
     * Stores the token kind identifier.
     */
    var kind: int

    /**
     * Points to the source position occupied by the token.
     *
     * The position object is referenced directly and is not copied.
     */
    var pos: pointer<TokenPosition>

    /**
     * Points to the textual representation of the token.
     *
     * For ordinary tokens, this is an independent copy of the supplied
     * text. For EOF tokens, this points to the shared EOF_STRING value.
     */
    var text: pointer<char>

    /**
     * Points to optional diagnostic information associated with the token.
     *
     * This field is normally null when the token does not contain an error.
     */
    var errorInfo: pointer<char>


    /**
     * Initializes a token without error information.
     *
     * For ordinary tokens, the supplied text is duplicated and stored
     * in the token. The error information is set to null.
     *
     * For an EOF token, the supplied text is ignored and the token uses
     * the shared EOF_STRING value.
     *
     * The caller must provide a valid position pointer and valid
     * null-terminated text for ordinary tokens.
     *
     * @param kind              the token kind identifier
     * @param pos               the source position of the token
     * @param text              the null-terminated token text
     *
     * @note                    The position object is not copied.
     *
     * @warning                 Passing an invalid text pointer to a non-EOF token may cause undefined behavior.
     */
    fun __init__(kind: int, pos: pointer<TokenPosition>, text: pointer<char>)
    {
        this.kind = kind
        this.pos = pos

        if kind == EOF_KIND:
        {
            this.text = EOF_STRING
            return
        }

        this.text = String.strdup(text)
        this.errorInfo = null
    }


    /**
     * Initializes a token with diagnostic information.
     *
     * For ordinary tokens, both the supplied text and error information
     * are duplicated and stored in the token.
     *
     * For an EOF token, the supplied text and error information are
     * ignored and the token uses the shared EOF_STRING value.
     *
     * The caller must provide a valid position pointer and valid
     * null-terminated strings for ordinary tokens.
     *
     * @param kind              the token kind identifier
     * @param pos               the source position of the token
     * @param text              the null-terminated token text
     * @param errorInfo         the null-terminated diagnostic message
     *
     * @note                    The position object is not copied.
     *
     * @warning                 Passing an invalid string pointer to a non-EOF token may cause undefined behavior.
     */
    fun __init__(kind: int, pos: pointer<TokenPosition>, text: pointer<char>, errorInfo: pointer<char>)
    {
        this.kind = kind
        this.pos = pos

        if kind == EOF_KIND:
        {
            this.text = EOF_STRING
            return
        }

        this.text = String.strdup(text)
        this.errorInfo = String.strdup(errorInfo)
    }


    fun copy() -> pointer<Token>
    {
        if this.errorInfo == null:
            return new Token(this.kind, this.pos, this.text)

        return new Token(this.kind, this.pos, this.text, this.errorInfo)
    }


    /**
     * Tests whether this token represents the end of the input.
     *
     * The token is considered an EOF token when its kind equals EOF_KIND.
     *
     * @return                  true if this token is an EOF token; otherwise false
     */
    fun isEOF() -> bool = this.kind == EOF_KIND
}


/**
 * Stores an ordered collection of lexer tokens.
 *
 * Tokens are kept in the same order in which they are appended.
 * The collection provides direct access, copying and source-like
 * string reconstruction operations.
 *
 * The internal ArrayList remains mutable. Methods that expose it directly
 * allow callers to modify the contents of this TokenList.
 */
struct TokenList
{
    /**
     * Points to the source file path associated with this token list.
     *
     * The path is optional. A null path means the token list is not tied to
     * a named source file, such as code from a REPL or an in-memory test.
     */
    var filePath: pointer<char>

    /**
     * Stores the internal token collection.
     *
     * This list is owned and managed by the TokenList instance.
     */
    private var tokens: pointer<ArrayList>


    /**
     * Initializes an empty token list.
     *
     * A new internal ArrayList is created using the size of Token
     * as its element size.
     */
    fun __init__()
    {
        this.filePath = null
        this.tokens = new ArrayList(sizeof(Token))
    }


    /**
     * Initializes an empty token list associated with a source file path.
     *
     * The supplied file path is duplicated. A null path remains null.
     *
     * @param filePath          the source file path for this token list
     */
    fun __init__(filePath: pointer<char>)
    {
        this.filePath = String.strdup(filePath)
        this.tokens = new ArrayList(sizeof(Token))
    }


    /**
     * Updates the source path associated with this token list.
     *
     * The supplied path is duplicated. A null path clears the path.
     *
     * @param path              the new source path
     *
     * @return                  this token list for chained calls
     */
    fun setPath(path: pointer<char>) -> pointer<TokenList>
    {
        this.filePath = String.strdup(path)
        return this
    }


    /**
     * Updates the source file path associated with this token list.
     *
     * This is the explicit-name alias of setPath.
     *
     * @param filePath          the new source file path
     *
     * @return                  this token list for chained calls
     */
    fun setFilePath(filePath: pointer<char>) -> pointer<TokenList> =
        this.setPath(filePath)


    /**
     * Appends a token to the end of the list.
     *
     * Existing tokens keep their original order, and the new token
     * becomes the final element.
     *
     * The caller must provide a valid token pointer.
     *
     * @param token             the token to append
     *
     * @warning                 Passing an invalid or null token pointer may cause undefined behavior.
     */
    fun push(token: pointer<Token>):
        this.tokens.push(token)


    /**
     * Returns the number of tokens currently stored in the list.
     *
     * @return                  the number of stored tokens
     */
    fun length() -> int = this.tokens.length


    /**
     * Returns the token stored at the specified index.
     *
     * Token indices are zero-based. The valid index range is from
     * 0 through length() - 1.
     *
     * The returned pointer refers to an element managed by the internal
     * ArrayList and may become invalid if the list is modified or released.
     *
     * The caller is responsible for ensuring that the index is valid.
     *
     * @param index             the zero-based token index
     *
     * @return                  a pointer to the token at the specified index
     *
     * @warning                 An out-of-range index may cause undefined behavior.
     */
    fun get(index: int) -> pointer<Token> = this.tokens.get(index) as pointer<Token>


    /**
     * Tests whether a complete PatternList matches at an index.
     *
     * @param index             the zero-based token index at which matching begins
     * @param patternList       the pattern sequence to match
     *
     * @return                  true if every pattern atom matched; otherwise false
     */
    fun canMatch(index: int, patternList: pointer<PatternList>) -> bool =
        patternList.regMatch(this, index)


    /**
     * Tests whether a complete PatternList matches at the beginning.
     *
     * This overload uses index zero.
     *
     * @param patternList       the pattern sequence to match
     *
     * @return                  true if every pattern atom matched; otherwise false
     */
    fun canMatch(patternList: pointer<PatternList>) -> bool =
        this.canMatch(0, patternList)


    /**
     * Returns the internal token array directly.
     *
     * The returned ArrayList is the same object used internally by
     * this TokenList. Modifications made through it affect this list.
     *
     * The caller must not release the returned ArrayList while this
     * TokenList is still using it.
     *
     * @return                  the internal mutable ArrayList
     *
     * @note                    Use toArray when an independent collection is required.
     *
     * @warning                 Direct modification may break TokenList invariants.
     */
    fun array() -> pointer<ArrayList> = this.tokens


    /**
     * Creates a copy of the internal token array.
     *
     * The returned ArrayList can be modified independently from the
     * original list structure.
     *
     * Whether token contents themselves are deeply copied depends on
     * the clone behavior of ArrayList.
     *
     * The caller is responsible for managing the lifetime of the
     * returned ArrayList.
     *
     * @return                  a cloned ArrayList containing the current tokens
     *
     * @note                    This method guarantees a separate list object, but it may
     *                          not create independent copies of referenced token data.
     */
    fun toArray() -> pointer<ArrayList> = this.tokens.clone()


    /**
     * Copies a half-open token range into a new TokenList.
     *
     * The range follows Java-style bounds:
     * - from is inclusive.
     * - to is exclusive.
     *
     * Token values are copied into a new backing ArrayList. Token text,
     * position and error-info pointers inside each Token value are not
     * deeply copied.
     *
     * Invalid ranges return null.
     *
     * @param from              inclusive start index
     * @param to                exclusive end index
     *
     * @return                  copied token list, or null for an invalid range
     */
    fun subToken(from: int, to: int) -> pointer<TokenList>
    {
        val copiedTokens: pointer<ArrayList> = this.tokens.sublist(from, to)

        if copiedTokens == null:
            return null

        val result: pointer<TokenList> = new TokenList(this.filePath)
        result.tokens = copiedTokens

        return result
    }


    /**
     * Removes a half-open token range from this list.
     *
     * The range follows Java-style bounds:
     * - from is inclusive.
     * - to is exclusive.
     *
     * Invalid ranges are ignored. Removing an empty range is a no-op.
     *
     * @param from              inclusive start index
     * @param to                exclusive end index
     */
    fun removeIndex(from: int, to: int)
    {
        if from < 0 || to < from || to > this.tokens.length:
            return

        var i: int = to

        while i > from:
        {
            i--
            this.tokens.removeAt(i)
        }
    }


    /**
     * Removes a half-open token range from this list.
     *
     * This is the short-name alias of removeIndex.
     *
     * @param from              inclusive start index
     * @param to                exclusive end index
     */
    fun remove(from: int, to: int):
        this.removeIndex(from, to)


    /**
     * Compatibility alias for removeIndex.
     *
     * @param from              inclusive start index
     * @param to                exclusive end index
     */
    fun romoveIndex(from: int, to: int):
        this.removeIndex(from, to)


    /**
     * Converts the token list into a readable source-like string.
     *
     * Tokens whose kind equals newlineKind are converted into newline
     * characters. EOF tokens are represented by EOF_STRING. Other tokens
     * with non-null text are appended followed by a single space.
     *
     * Tokens with null text are skipped. A space may remain before a
     * newline or the EOF marker because ordinary token text always receives
     * a trailing space.
     *
     * The caller must provide the token kind used to represent source
     * newlines and is responsible for managing the returned StringBuilder.
     *
     * @param newlineKind       the token kind that should produce a newline
     *
     * @return                  a newly allocated StringBuilder containing the formatted tokens
     *
     * @note                    This method creates a readable representation and does not
     *                          necessarily reproduce the original source text exactly.
     */
    fun toString(newlineKind: int) -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        for (var i: int = 0; i < this.tokens.length; i++):
        {
            val token: pointer<Token> = this.get(i)

            if token.kind == newlineKind:
                sb.append("\n")
            elif token.isEOF():
                sb.append(Token.EOF_STRING)
            elif token.text != null:
            {
                sb.append(token.text)
                sb.append(' ')
            }
        }

        return sb
    }
}
