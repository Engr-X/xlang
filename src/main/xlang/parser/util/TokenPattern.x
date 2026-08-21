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

import xlang.lexer.Token
import xlang.lexer.TokenList
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


    var refParser: pointer<ParserRef>

    var refsParser: pointer<ParserRefs>


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
        this.refParser = null
        this.refsParser = null
    }


    fun __init__(refParser: pointer<ParserRef>)
    {
        this.kind = Token.AnyKind
        this.regex = null
        this.refParser = refParser
        this.refsParser = null
    }


    fun __init__(refsParser: pointer<ParserRefs>)
    {
        this.kind = Token.AnyKind
        this.regex = null
        this.refParser = null
        this.refsParser = refsParser
    }


    fun matchRegex(tokens: pointer<TokenList>, index: int) -> int
    {
        if !this.isRegex() || tokens == null || index < 0 || index >= tokens.length():
            return -1

        val token: pointer<Token> = tokens.get(index)

        if this.kind != Token.AnyKind && this.kind != token.kind:
            return -1

        if this.regex == null:
            return 1

        if token.text == null:
            return -1

        return if String.strRegMatch(this.regex, token.text) > 0:
                1
            else:
                -1
    }


    inline fun isRegex() -> bool = this.refParser == null && this.refsParser == null

    inline fun isRef() -> bool = this.refParser != null

    inline fun isRefs() -> bool = this.refsParser != null

    inline fun getRefParser() -> pointer<ParserRef> = this.refParser

    inline fun getRefsParser() -> pointer<ParserRefs> = this.refsParser
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
    inline fun pushRegex(kind: int) -> pointer<PatternList> =
        this.pushRegex(kind, null)


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
    inline fun pushRegex(regex: pointer<char>) -> pointer<PatternList> =
        this.pushRegex(Token.AnyKind, regex)


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
    inline fun pushRegex(kind: int, regex: pointer<char>) -> pointer<PatternList>
    {
        val pattern: PatternAtom = PatternAtom(kind, regex)
        this.patterns.push(pattern.ref)
        return this
    }


    inline fun pushRef(refParser: pointer<ParserRef>) -> pointer<PatternList>
    {
        val pattern: PatternAtom = PatternAtom(refParser)
        this.patterns.push(pattern.ref)
        return this
    }


    inline fun pushRefs(refsParser: pointer<ParserRefs>) -> pointer<PatternList>
    {
        val pattern: PatternAtom = PatternAtom(refsParser)
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
     * Tests whether the full pattern sequence matches at a token-list index.
     *
     * This is the boolean form of match. It returns true only when every
     * pattern atom in this list matches consecutively.
     *
     * @param tokens            the token list to inspect.
     * @param index             the zero-based token index at which matching begins.
     *
     * @return                  true if the complete pattern sequence matches.
     */
    fun regMatch(tokens: pointer<TokenList>, index: int) -> bool
    {
        var consumed: int = 0

        if tokens == null || index < 0 || index >= tokens.length():
            return false

        for (var patternIndex = 0; patternIndex < this.length(); patternIndex++):
        {
            val pattern: pointer<PatternAtom> = this.get(patternIndex)

            if pattern == null || !pattern.isRegex():
                return false

            val length: int = pattern.matchRegex(tokens, index + consumed)

            if length < 0:
                return false

            consumed += length
        }

        return true
    }
}
