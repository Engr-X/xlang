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

package xlang.lexer

import xlang.util.ArrayList
import xlang.util.string.String


/**
 * Describes one token-matching condition.
 *
 * A pattern atom may restrict a token by its kind, its textual content,
 * or both. Token.AnyKind disables kind filtering, while a null regex
 * disables text filtering.
 *
 * When both conditions are enabled, the token must satisfy both of them.
 * The kind condition is checked before the regular-expression condition.
 *
 * The regular-expression string is duplicated during construction, so the
 * PatternAtom keeps an independent copy of the supplied pattern.
 *
 * The caller is responsible for eventually releasing the copied regex if
 * the runtime does not manage allocated string memory automatically.
 */
struct PatternAtom
{
    /**
     * Stores the required token kind.
     *
     * Token.AnyKind means that tokens of every kind are accepted.
     */
    private var kind: int

    /**
     * Stores the optional token-text regular expression.
     *
     * A null value means that token text is not checked.
     */
    private var regex: pointer<char>


    /**
     * Initializes a token pattern atom.
     *
     * The supplied token kind is stored directly. The regular-expression
     * string is duplicated with String.strdup so later changes to the
     * original string do not affect this pattern.
     *
     * Token.AnyKind may be used to create a regex-only pattern. A null regex
     * may be used to create a kind-only pattern. Combining Token.AnyKind with
     * a null regex creates a pattern that accepts every non-null token.
     *
     * The caller must provide a valid null-terminated regex when regex is
     * non-null.
     *
     * @param kind              the required token kind, or Token.AnyKind.
     * @param regex             the optional token-text regular expression.
     *
     * @note                    The constructor creates an independent copy of regex.
     *
     * @warning                 If String.strdup does not accept null, passing a null regex
     *                          causes undefined behavior.
     */
    fun __init__(kind: int, regex: pointer<char>)
    {
        this.kind = kind
        this.regex = String.strdup(regex)
    }


    /**
     * Tests whether a token has an accepted kind.
     *
     * The token matches when this atom uses Token.AnyKind or when the token's
     * kind exactly equals the stored kind.
     *
     * The caller must provide a valid, non-null token pointer.
     *
     * @param token             the token whose kind should be tested.
     *
     * @return                  true if the token kind is accepted; otherwise false.
     *
     * @warning                 Passing a null or invalid token pointer causes undefined
     *                          behavior.
     */
    private inline fun matchKind(token: pointer<Token>) -> bool =
        this.kind == Token.AnyKind || this.kind == token.kind


    /**
     * Tests whether a token satisfies this pattern atom.
     *
     * A null token never matches. The token kind is checked first. If the
     * kind does not match, the function returns false without inspecting
     * the token text.
     *
     * If regex is null, a successful kind match is sufficient. Otherwise,
     * token.text must be non-null and String.strRegMatch must return a
     * positive match length.
     *
     * A zero-length regular-expression match is currently treated as a
     * failed match because the result must be greater than zero.
     *
     * @param token             the token to test.
     *
     * @return                  true if the token satisfies all enabled conditions;
     *                          otherwise false.
     *
     * @note                    Token.AnyKind combined with a null regex accepts every
     *                          non-null token.
     */
    fun match(token: pointer<Token>) -> bool
    {
        if token == null:
            return false

        if this.kind != Token.AnyKind && this.kind != token.kind:
            return false

        if this.regex == null:
            return true

        if token.text == null:
            return false

        return String.strRegMatch(this.regex, token.text) > 0
    }
}


/**
 * Stores an ordered sequence of token pattern atoms.
 *
 * A PatternList describes a contiguous token window. Pattern atom zero
 * matches the first token in the window, pattern atom one matches the next
 * token, and so on.
 *
 * PatternAtom values are copied into an internal ArrayList in insertion
 * order. The list may contain kind-only, regex-only or combined conditions.
 *
 * The internal pattern storage is managed by this object. Pointers returned
 * by get may become invalid if the backing ArrayList is modified or released.
 */
struct PatternList
{
    /**
     * Stores PatternAtom values in matching order.
     */
    private var patterns: pointer<ArrayList>


    /**
     * Initializes an empty pattern list.
     *
     * A new ArrayList is allocated with PatternAtom as its element type.
     */
    fun __init__():
        this.patterns = new ArrayList(sizeof(PatternAtom))


    /**
     * Appends a kind-only pattern atom.
     *
     * The appended atom accepts tokens of the specified kind without
     * checking their text.
     *
     * Token.AnyKind may be supplied to create a pattern that accepts every
     * non-null token.
     *
     * @param kind              the required token kind, or Token.AnyKind.
     *
     * @return                  this PatternList for chained calls.
     */
    inline fun push(kind: int) -> pointer<PatternList> =
        this.push(kind, null)


    /**
     * Appends a regex-only pattern atom.
     *
     * The appended atom accepts tokens of any kind whose non-null text
     * matches the supplied regular expression.
     *
     * The regular-expression string is duplicated by PatternAtom.
     *
     * The caller must provide a valid null-terminated regex.
     *
     * @param regex             the regular expression matched against token.text.
     *
     * @return                  this PatternList for chained calls.
     *
     * @warning                 Passing null may create an unrestricted pattern or cause
     *                          undefined behavior, depending on String.strdup.
     */
    inline fun push(regex: pointer<char>) -> pointer<PatternList> =
        this.push(Token.AnyKind, regex)


    /**
     * Appends a pattern atom with kind and optional text conditions.
     *
     * The new atom is added to the end of the sequence. The PatternAtom
     * constructor duplicates the supplied regular-expression string before
     * the atom is copied into the backing ArrayList.
     *
     * Token.AnyKind disables kind filtering. A null regex disables text
     * filtering.
     *
     * The caller must provide a valid null-terminated regex when regex is
     * non-null.
     *
     * @param kind              the required token kind, or Token.AnyKind.
     * @param regex             the optional regular expression matched against token.text.
     *
     * @return                  this PatternList for chained calls.
     */
    inline fun push(kind: int, regex: pointer<char>) -> pointer<PatternList>
    {
        val pattern: PatternAtom = PatternAtom(kind, regex)
        this.patterns.push(pattern.ref)
        return this
    }


    /**
     * Returns the number of pattern atoms in this list.
     *
     * @return                  the current pattern count.
     */
    fun length() -> int = this.patterns.length


    /**
     * Returns the pattern atom stored at an index.
     *
     * Indices are zero-based. The valid range is from zero through
     * length() minus one.
     *
     * The returned pointer refers to storage owned by the internal
     * ArrayList. It may become invalid after the list is modified,
     * reallocated or released.
     *
     * The behavior for an invalid index depends on ArrayList.get.
     *
     * @param index             the zero-based pattern index.
     *
     * @return                  a pointer to the pattern atom, or null if ArrayList.get
     *                          returns null for the index.
     *
     * @warning                 An invalid index may cause undefined behavior if ArrayList.get
     *                          does not perform bounds checking.
     */
    fun get(index: int) -> pointer<PatternAtom> =
        this.patterns.get(index) as pointer<PatternAtom>


    /**
     * Returns the longest matched prefix length at a token-list position.
     *
     * Matching begins at index. Pattern atom zero is compared with
     * tokens[index], atom one with tokens[index + 1], and so on. The function
     * stops at the first failed pattern, null PatternAtom, invalid token-list
     * range or end of input.
     *
     * The returned value is the number of pattern atoms that matched
     * consecutively. A complete match is therefore:
     *     maxMatchLength(tokens, index) == length()
     *
     * When the value is smaller than length(), the value also identifies the
     * first failing pattern position, which is useful for precise parser or
     * normalizer diagnostics.
     *
     * @param tokens            the token list to inspect.
     * @param index             the zero-based token index at which matching begins.
     *
     * @return                  the number of consecutive pattern atoms that matched.
     */
    fun maxMatchLength(tokens: pointer<TokenList>, index: int) -> int
    {
        var count: int = 0

        if tokens == null:
            return 0

        if index < 0:
            return 0

        while count < this.patterns.length:
        {
            val tokenIndex: int = index + count

            if tokenIndex >= tokens.length():
                break

            val pattern: pointer<PatternAtom> = this.get(count)
            val token: pointer<Token> = tokens.get(tokenIndex)

            if pattern == null:
                break

            if !pattern.match(token):
                break

            count++
        }

        return count
    }


    /**
     * Tests whether the full pattern sequence matches at a token-list index.
     *
     * This is the boolean form of maxMatchLength. It returns true only when
     * every pattern atom in this list matches consecutively.
     *
     * @param tokens            the token list to inspect.
     * @param index             the zero-based token index at which matching begins.
     *
     * @return                  true if the complete pattern sequence matches.
     */
    inline fun canMatch(tokens: pointer<TokenList>, index: int) -> bool = 
        (this.maxMatchLength(tokens, index) == this.length())
}
