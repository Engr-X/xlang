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
import xlang.util.string.Regex
import xlang.System


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
 * PatternAtom keeps an independent clone of the supplied pattern.
 *
 * PatternAtom owns both the copied regex and its compiled representation.
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
    * Stores the compiled representation of {@link #regex}.
    *
    * A null value means that no regular expression has been compiled.
    */
    private var compiledRegex: pointer<*>

    /**
    * References another parser that may be invoked when this atom is matched.
    *
    * A null value means that this atom does not delegate matching to a
    * single referenced parser.
    */
    var refParser: pointer<ParserRef>

    /**
    * References a collection of parsers that may be invoked recursively
    * while matching this atom.
    *
    * A null value means that this atom does not reference multiple parsers.
    */
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
     * @note                    The constructor creates an independent clone of regex.
     */
    constructor(kind: int, regex: pointer<char>)
    {
        this.kind = kind
        this.regex = String.strdup(regex)
        this.compiledRegex = null

        if this.regex != null:
        {
            this.compiledRegex = System.allocMemory(Regex.compileSize())
            Regex.compile(this.regex, this.compiledRegex)
        }

        this.refParser = null
        this.refsParser = null
    }


    /**
     * Creates a pattern atom backed by a single parser reference.
     *
     * <p>This pattern atom does not perform token-kind or regular-expression
     * matching. Instead, matching is delegated to the supplied
     * {@code ParserRef}.
     *
     * @param refParser          a pointer to the parser reference used by this pattern atom
     */
    constructor(refParser: pointer<ParserRef>)
    {
        this.kind = Token.AnyKind
        this.regex = null
        this.compiledRegex = null
        this.refParser = refParser
        this.refsParser = null
    }


    /**
     * Creates a pattern atom backed by a repeated parser reference.
     *
     * <p>This pattern atom does not perform token-kind or regular-expression
     * matching. Instead, matching is delegated to the supplied
     * {@code ParserRefs}.
     *
     * @param refsParser        a pointer to the repeated parser reference used by this
     *                          pattern atom
     */
    constructor(refsParser: pointer<ParserRefs>)
    {
        this.kind = Token.AnyKind
        this.regex = null
        this.compiledRegex = null
        this.refParser = null
        this.refsParser = refsParser
    }


    /**
     * Attempts to match the token at the specified index against the token-kind
     * and regular-expression constraints of this pattern atom.
     *
     * <p>This method may only be used for regular token pattern atoms. If this
     * pattern atom contains a parser reference, the match fails immediately.
     *
     * <p>If a specific token kind is configured, the token must have the same
     * kind. {@code Token.AnyKind} disables token-kind filtering.
     *
     * <p>If no regular expression is configured, a matching token kind is
     * sufficient for a successful match.
     *
     * <p>If a regular expression is configured, the token text is matched against
     * either the compiled regular expression, when available, or the original
     * regular-expression string otherwise.
     *
     * @param tokens            a pointer to the token list to inspect
     * @param index             the index of the token to match
     * @return                  {@code 1} if the token matches this pattern atom;
     *                          {@code -1} otherwise
     */
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

        return if this.compiledRegex == null:
            (if String.strRegMatch(this.regex, token.text) > 0: 1 else: -1)
            else: 
            (if Regex.regexMatch(this.compiledRegex, token.text) > 0: 1 else: -1)
    }


    /**
     * Checks whether this pattern atom represents a regular token pattern.
     *
     * <p>A pattern atom is considered a regular token pattern when it contains
     * neither a single parser reference nor a repeated parser reference.
     *
     * @return                  {@code true} if this pattern atom performs token or regular-expression
     *                          matching; {@code false} otherwise
     */
    inline fun isRegex() -> bool = this.refParser == null && this.refsParser == null

    /**
     * Checks whether this pattern atom contains a single parser reference.
     *
     * @return                  {@code true} if a {@code ParserRef} is associated with this pattern
     *                          atom; {@code false} otherwise
     */
    inline fun isRef() -> bool = this.refParser != null


    /**
     * Checks whether this pattern atom contains a repeated parser reference.
     *
     * @return                  {@code true} if a {@code ParserRefs} instance is associated with this
     *                          pattern atom; {@code false} otherwise
     */
    inline fun isRefs() -> bool = this.refsParser != null


    /**
     * Returns the single parser reference associated with this pattern atom.
     *
     * @return                  a pointer to the associated {@code ParserRef}, or {@code null} if
     *                          this pattern atom does not contain one
     */
    inline fun getRefParser() -> pointer<ParserRef> = this.refParser


    /**
     * Returns the repeated parser reference associated with this pattern atom.
     *
     * @return                  a pointer to the associated {@code ParserRefs}, or {@code null} if
     *                          this pattern atom does not contain one
     */
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
    constructor():
        this.patterns = new ArrayList(sizeof(PatternAtom))


    constructor(pattern: pointer<PatternAtom>)
    {
        this.patterns = new ArrayList(sizeof(PatternAtom))

        if pattern != null:
            this.patterns.push(pattern)
    }


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
