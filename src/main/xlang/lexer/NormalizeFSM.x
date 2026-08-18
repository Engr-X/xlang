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

import xlang.System
import xlang.parser.util.PatternList
import xlang.util.ArrayList


/**
 * Performs no normalization action.
 *
 * This function is used as the default action of a NormalizeRule.
 * It does not inspect or modify the FSM, token window or receiver.
 *
 * @param fsm               the normalization state machine
 * @param tokens            the token window matched by the rule
 *
 * @return                  always false
 *
 * @note                    Returning false indicates that the rule did not perform
 *                          a successful normalization action.
 */
private fun noopNormalizeAction(fsm: pointer<NormalizeFSM>, tokens: pointer<ArrayList>) -> bool =
    false


/**
 * Collects token insertions and deletions during normalization.
 *
 * The receiver stores one deletion flag and one optional inserted token
 * for every token in the original TokenList. Changes are recorded first
 * and applied later to produce a new TokenList.
 *
 * An inserted token is emitted immediately before the original token at
 * the same index. If the original token is also marked for deletion, only
 * the inserted token is emitted.
 *
 * Each index can hold at most one inserted token. Inserting another token
 * at the same index replaces the previously recorded token.
 *
 * The receiver does not copy Token objects. The caller must ensure that
 * all referenced tokens remain valid while the result list uses them.
 */
struct NormalizeReceiver
{
    /**
     * Stores the number of positions managed by this receiver.
     *
     * This value must match the length of the TokenList passed to apply.
     */
    var length: int

    /**
     * Stores the deletion flag for each original token.
     *
     * A true value means that the token at the corresponding index
     * will not be copied into the result.
     */
    private var deleted: pointer<bool>

    /**
     * Stores an optional token to insert before each original token.
     *
     * A null entry means that no token is inserted at that index.
     */
    private var addedTokens: pointer<pointer<Token>>


    /**
     * Initializes a normalization receiver for a fixed token count.
     *
     * For a positive length, this constructor allocates and initializes
     * the deletion and insertion arrays. Every token initially remains
     * undeleted and every insertion slot initially contains null.
     *
     * If length is zero or negative, the receiver is initialized as empty
     * and no memory is allocated.
     *
     * The caller is responsible for eventually releasing allocated memory
     * if the runtime does not manage it automatically.
     *
     * @param length            the number of original token positions to manage
     *
     * @warning                 A very large length may overflow the allocation-size
     *                          calculation or cause memory allocation to fail.
     */
    fun __init__(length: int)
    {
        if length <= 0:
        {
            this.length = 0
            this.deleted = null
            this.addedTokens = null
            return
        }

        this.length = length
        this.deleted = System.allocMemory(length * sizeof(bool)) as pointer<bool>
        this.addedTokens = System.allocMemory(length * sizeof(pointer<Token>)) as pointer<pointer<Token>>

        for (var i = 0; i < length; i++):
        {
            this.deleted[i] = false
            this.addedTokens[i] = null
        }
    }


    /**
     * Tests whether an index belongs to this receiver.
     *
     * Valid indices begin at zero and end at length minus one.
     *
     * @param index             the index to validate
     *
     * @return                  true if the index is valid; otherwise false
     */
    private inline fun checkIndex(index: int) -> bool =
        0 <= index && index < this.length


    /**
     * Marks an original token for deletion.
     *
     * The token at the given index will be omitted when apply is called.
     * Calling this function repeatedly for the same index has no additional
     * effect.
     *
     * An invalid index is ignored.
     *
     * @param index             the zero-based index of the token to delete
     *
     * @return                  this receiver for chained calls
     *
     * @note                    This function records the deletion but does not immediately
     *                          modify the original TokenList.
     */
    fun deleteAt(index: int) -> pointer<NormalizeReceiver>
    {
        if !this.checkIndex(index):
            return this

        this.deleted[index] = true
        return this
    }


    /**
     * Records a token insertion at the specified position.
     *
     * The token is inserted immediately before the original token at the
     * same index when apply is called.
     *
     * If an insertion already exists at this index, it is replaced.
     * An invalid index is ignored.
     *
     * The token pointer is stored directly and the Token object is not copied.
     * The caller must keep the token valid while it is used by the result.
     *
     * @param index             the zero-based insertion position
     * @param token             the token to insert before the original token
     *
     * @return                  this receiver for chained calls
     *
     * @note                    Passing null clears the effective insertion at the index.
     */
    fun insertAt(index: int, token: pointer<Token>) -> pointer<NormalizeReceiver>
    {
        if !this.checkIndex(index):
            return this

        this.addedTokens[index] = token
        return this
    }


    /**
     * Applies all recorded changes to a token list.
     *
     * The function traverses the original list in order. At every index,
     * the recorded inserted token is appended first, followed by the
     * original token unless it is marked for deletion.
     *
     * The original TokenList is not modified. A new TokenList is returned.
     *
     * If list is null or its length does not match this receiver's length,
     * the operation fails and null is returned.
     *
     * The caller is responsible for managing the returned TokenList and
     * ensuring that the original and inserted Token objects remain valid.
     *
     * @param list              the original token list
     *
     * @return                  a newly allocated normalized TokenList, or null if the input list is invalid or has a different length
     *
     * @note                    Token objects are reused rather than deeply copied.
     */
    fun apply(list: pointer<TokenList>) -> pointer<TokenList>
    {
        if list == null || list.length() != this.length:
            return null

        val result: pointer<TokenList> = new TokenList(list.filePath)
        var deleteCursor: pointer<bool> = this.deleted
        var addCursor: pointer<pointer<Token>> = this.addedTokens

        for (var i = 0; i < this.length; i++):
        {
            val current: pointer<Token> = list.get(i)

            if addCursor.deref != null:
                result.push(addCursor.deref)

            if !deleteCursor.deref:
                result.push(current)

            deleteCursor++
            addCursor++
        }

        return result
    }
}


/**
 * Defines a token sequence and action used by the normalization FSM.
 *
 * A rule consists of an ordered PatternList, one selected
 * pivot pattern, a required FSM state and an action function.
 *
 * During matching, currentIndex identifies the token corresponding to the
 * pivot. The remaining patterns are matched before and after that token
 * according to their positions relative to the pivot.
 *
 * A rule cannot match until a valid pivot has been selected with setPivot.
 *
 * Pattern strings are owned by the internal PatternList. Action functions
 * are referenced directly, so the caller must keep them valid while the
 * rule is used.
 */
struct NormalizeRule
{
    /**
     * Stores the application-defined rule identifier.
     *
     * The normalization engine does not interpret this value directly.
     */
    var id: int

    /**
     * Stores the FSM state in which this rule is active.
     */
    var state: int

    /**
     * Stores the function executed after this rule matches.
     *
     * The action receives the FSM and the matched token window. Its return
     * value indicates whether the normalization action was successful.
     */
    var action: (pointer<NormalizeFSM>, pointer<ArrayList>) -> bool
    
    /**
     * Stores the pivot's zero-based index within the pattern sequence.
     *
     * A value of -1 means that no valid pivot is currently selected.
     */
    private var pivotIndex: int
    
    /**
     * Stores the ordered token pattern sequence used by this rule.
     */
    private var patterns: pointer<PatternList>


    /**
     * Initializes a normalization rule with a no-op action.
     *
     * The rule begins with an empty pattern sequence and no selected pivot.
     * noopNormalizeAction is used until another action is assigned.
     *
     * @param id                the application-defined rule identifier
     * @param state             the FSM state in which the rule may match
     */
    fun __init__(id: int, state: int):
    {
        this.id = id
        this.state = state
        this.action = noopNormalizeAction
        this.pivotIndex = -1
        this.patterns = new PatternList()
    }


    /**
     * Initializes a normalization rule with a custom action.
     *
     * The rule begins with an empty pattern sequence and no selected pivot.
     * The action pointer is stored directly without validation.
     *
     * The caller must provide a valid callable action and keep it available
     * while the rule is used.
     *
     * @param id                the application-defined rule identifier
     * @param state             the FSM state in which the rule may match
     * @param action            the function to execute after a successful match
     *
     * @warning                 A null or invalid action causes undefined behavior when
     *                          the rule is applied.
     */
    fun __init__(id: int, state: int, action: (pointer<NormalizeFSM>, pointer<ArrayList>) -> bool):
    {
        this.id = id
        this.state = state
        this.action = action
        this.pivotIndex = -1
        this.patterns = new PatternList()
    }


    /**
     * Appends a token-kind pattern without a text requirement.
     *
     * The new pattern accepts tokens of the specified kind. Token.AnyKind
     * may be used to accept any kind.
     *
     * @param kind              the required token kind or Token.AnyKind
     *
     * @return                  this rule for chained calls
     */
    fun addPattern(kind: int) -> pointer<NormalizeRule> =
        this.addPattern(kind, null)


    /**
     * Appends a token pattern to the rule.
     *
     * The pattern is added to the end of the matching sequence. PatternList
     * owns the concrete pattern storage.
     *
     * PatternList creates the concrete pattern atom and owns any copied
     * regex string used for matching.
     *
     * @param kind              the required token kind or Token.AnyKind
     * @param regex             the optional regular expression
     *
     * @return                  this rule for chained calls
     *
     * @note                    It is safest to call setPivot after all patterns have been added.
     */
    fun addPattern(kind: int, regex: pointer<char>) -> pointer<NormalizeRule>
    {
        this.patterns.pushRegex(kind, regex)
        return this
    }


    /**
     * Selects one pattern as the rule's pivot.
     *
     * The pivot determines which pattern is aligned with currentIndex during
     * matching. Patterns before and after it are matched at relative indices.
     *
     * If the supplied index is invalid, the current pivot is cleared and
     * pivotIndex becomes -1.
     *
     * @param pivot             the zero-based index of the pivot pattern
     *
     * @return                  this rule for chained calls
     *
     * @note                    A rule without a valid pivot cannot match.
     */
    fun setPivot(pivot: int) -> pointer<NormalizeRule>
    {
        if pivot < 0 || pivot >= this.patterns.length():
        {
            this.pivotIndex = -1
            return this
        }

        this.pivotIndex = pivot
        return this
    }


    /**
     * Tests whether this rule matches around a current token index.
     *
     * currentIndex is aligned with the rule's pivot. The function calculates
     * the start of the complete pattern sequence and verifies that the whole
     * sequence fits inside the token list.
     *
     * The complete sequence is matched by TokenList.canMatch.
     *
     * The rule's state is not checked by this function. NormalizeFSM performs
     * that check before calling match.
     *
     * @param tokens            the token list to inspect
     * @param currentIndex      the index aligned with the rule's pivot
     *
     * @return                  true if the complete pattern sequence matches; otherwise false
     *
     * @note                    A rule with no valid pivot always returns false.
     *
     * @warning                 The token list must contain valid Token elements.
     */
    fun match(tokens: pointer<TokenList>, currentIndex: int) -> bool
    {
        if tokens == null || this.pivotIndex < 0:
            return false

        val start: int = currentIndex - this.pivotIndex

        return tokens.canMatch(start, this.patterns)
    }


    /**
     * Executes this rule's normalization action.
     *
     * The FSM and matched token window are passed directly to the configured
     * action function.
     *
     * @param fsm               the active normalization state machine
     * @param tokens            the token window matched by this rule
     *
     * @return                  the result returned by the action function
     *
     * @warning                 The action function and both pointer arguments must be valid.
     */
    fun apply(fsm: pointer<NormalizeFSM>, tokens: pointer<ArrayList>) -> bool =
        this.action(fsm, tokens)


    /**
     * Returns the relative index of the pivot pattern.
     *
     * @return                  the zero-based pivot index, or -1 if no valid pivot is set
     */
    fun getPivotIndex() -> int = this.pivotIndex


    /**
     * Returns the number of patterns in this rule.
     *
     * @return                  the pattern sequence length
     */
    fun getPatternLength() -> int = this.patterns.length()
}


/**
 * Applies state-dependent normalization rules to a token list.
 *
 * The FSM scans the original TokenList from left to right. At each token,
 * it tests active rules in their supplied order. When a rule matches, the
 * rule receives a window containing the complete matched sequence.
 *
 * Rule actions may record token insertions and deletions through this FSM.
 * These changes are collected by NormalizeReceiver and applied after the
 * scan, so the original token list remains unchanged during matching.
 *
 * The FSM also stores parenthesis and bracket counters that actions may use
 * to track whether scanning is currently inside an unmatched pair.
 *
 * The TokenList pointer is stored directly. The caller must keep the list
 * and its tokens valid while the FSM is used.
 */
struct NormalizeFSM
{
    /**
     * Identifies the default normalization state.
     */
    static val DEFAULT: int = 1

    /**
     * Stores the current FSM state.
     *
     * Only rules with the same state are considered during scanning.
     */
    var state: int

    /**
     * Stores the index currently being examined.
     */
    private var currentIndex: int

    /**
     * Stores the number of rule actions that returned true.
     *
     * This value is reset before each normalization scan.
     */
    private var ptr: int

    /**
     * Points to the original token list being normalized.
     */
    private var list: pointer<TokenList>

    /**
     * Stores token insertions and deletions recorded by rule actions.
     */
    private var receiver: pointer<NormalizeReceiver>

    /**
     * Stores the current parenthesis balance.
     *
     * Rule actions are responsible for updating this value.
     */
    private var parenthesis: int

    /**
     * Stores the current bracket balance.
     *
     * Rule actions are responsible for updating this value.
     */
    private var bracket: int


    /**
     * Initializes a normalization FSM for a token list.
     *
     * The FSM starts in DEFAULT state at token index zero. A receiver is
     * created with one modification slot for every token in the list.
     * Parenthesis and bracket balances begin at zero.
     *
     * The TokenList is referenced directly rather than copied.
     *
     * The caller must provide a valid, non-null TokenList and keep it alive
     * while this FSM is used.
     *
     * @param list              the token list to normalize
     *
     * @warning                 Passing a null or invalid list pointer causes undefined
     *                          behavior because its length is read immediately.
     */
    fun __init__(list: pointer<TokenList>)
    {
        this.state = DEFAULT
        this.currentIndex = 0
        this.ptr = 0
        this.list = list
        this.receiver = new NormalizeReceiver(list.length())

        this.parenthesis = 0
        this.bracket = 0
    }


    /**
     * Changes the current FSM state.
     *
     * The new state takes effect immediately and affects which rules are
     * considered during the remainder of the scan.
     *
     * @param state             the new FSM state identifier
     */
    fun setState(state: int):
        this.state = state


    /**
     * Returns the current FSM state.
     *
     * @return                  the active state identifier
     */
    fun getState() -> int = this.state


    /**
     * Returns the token index currently being examined.
     *
     * During a rule action, this value identifies the token aligned with
     * that rule's pivot.
     *
     * @return                  the current zero-based token index
     */
    fun getCurrentIndex() -> int = this.currentIndex


    /**
     * Returns the number of successful rule actions.
     *
     * The counter is incremented whenever a matched rule action returns true.
     *
     * @return                  the successful-action count for the current or most recent scan
     */
    fun getPtr() -> int = this.ptr


    /**
     * Tests whether an unmatched parenthesis or bracket is currently open.
     *
     * A pair is considered open when either internal balance is greater
     * than zero. Negative balances are not considered open.
     *
     * @return                  true if the parenthesis or bracket balance is positive; otherwise false
     *
     * @note                    This function does not detect mismatched closing delimiters.
     */
    inline fun hasOpenPair() -> bool =
        this.parenthesis > 0 || this.bracket > 0


    /**
     * Extracts the complete token window aligned with a matched rule.
     *
     * currentIndex is treated as the position of the rule's pivot. The
     * returned sublist begins at currentIndex minus the pivot index and
     * contains exactly the rule's pattern length.
     *
     * This function only checks whether rule and tokens are non-null.
     * It does not verify the calculated range.
     *
     * The caller should normally call this function only after rule.match
     * has returned true for the same token list and current index.
     *
     * @param rule              the matched normalization rule
     * @param tokens            the complete token list
     * @param currentIndex      the token index aligned with the rule's pivot
     *
     * @return                  the matched token array, or null if rule or tokens is null
     *
     * @warning                 An invalid pivot, current index or pattern length may produce
     *                          an out-of-range sublist request.
     */
    static fun window(rule: pointer<NormalizeRule>, tokens: pointer<TokenList>, currentIndex: int) -> pointer<ArrayList>
    {
        if rule == null || tokens == null:
            return null

        val start: int = currentIndex - rule.getPivotIndex()
        val tokenWindow: pointer<TokenList> = tokens.subToken(start, start + rule.getPatternLength())

        if tokenWindow == null:
            return null

        return tokenWindow.array()
    }


    /**
     * Scans the token list and records normalization changes.
     *
     * Rules are tested from the first supplied rule to the last at each token
     * index. Null rules, rules in another state and non-matching rules are
     * skipped.
     *
     * When a rule matches, its complete token window is passed to its action.
     * If the action returns true, the success counter is incremented and no
     * later rule is tested at the same current index.
     *
     * If the action returns false, scanning continues with the remaining
     * rules at that same index.
     *
     * Rules and rule actions may change the FSM state, delimiter balances
     * and receiver contents while scanning.
     *
     * If rules is null or rulesLength is zero or negative, this function
     * performs no scan.
     *
     * The caller must ensure that the rule array contains at least
     * rulesLength accessible entries.
     *
     * @param rules             the array of normalization-rule pointers
     * @param rulesLength       the number of entries in the rule array
     *
     * @warning                 An incorrect rulesLength may cause out-of-bounds memory access.
     */
    private fun initReceiver(rules: pointer<pointer<NormalizeRule>>, rulesLength: int)
    {
        if rules == null || rulesLength <= 0:
            return

        val tokenLength: int = this.list.length()

        this.currentIndex = 0
        this.ptr = 0

        while this.currentIndex < tokenLength:
        {
            for (var j = 0; j < rulesLength; j++):
            {
                val rule: pointer<NormalizeRule> = rules[j]

                if rule == null:
                    continue

                if rule.state != this.state:
                    continue

                if !rule.match(this.list, this.currentIndex):
                    continue

                val tokens: pointer<ArrayList> = NormalizeFSM.window(rule, this.list, this.currentIndex)

                if rule.apply(this, tokens):
                {
                    this.ptr++
                    break
                }
            }

            this.currentIndex++
        }
    }


    /**
     * Applies normalization rules and builds the resulting token list.
     *
     * The original list is scanned first and rule actions record their
     * requested insertions and deletions. The receiver then applies those
     * recorded changes and returns a new TokenList.
     *
     * The original TokenList is not modified.
     *
     * If no valid rules are provided, the receiver still produces a new list
     * using its currently recorded modifications.
     *
     * The caller must provide a valid rule array and manage the returned
     * TokenList.
     *
     * @param rules             the array of normalization-rule pointers
     * @param rulesLength       the number of entries in the rule array
     *
     * @return                  a newly allocated normalized TokenList, or null if receiver application fails
     *
     * @note                    Calling apply multiple times on the same FSM reuses the existing
     *                          receiver, so modifications recorded by earlier calls remain active.
     */
    fun apply(rules: pointer<pointer<NormalizeRule>>, rulesLength: int) -> pointer<TokenList>
    {
        this.initReceiver(rules, rulesLength)
        return this.receiver.apply(this.list)
    }


    /**
     * Marks the current token for deletion.
     *
     * The deletion is recorded in the receiver and takes effect when the
     * final TokenList is produced.
     *
     * If the current index is outside the receiver range, the request is
     * silently ignored.
     *
     * @note                    This function is intended to be called from a rule action.
     */
    fun deleteToken()
    {
        this.receiver.deleteAt(this.currentIndex)
    }

    
    /**
     * Inserts a token before the current token.
     *
     * The insertion is recorded in the receiver and takes effect when the
     * final TokenList is produced.
     *
     * If another insertion already exists at the current index, it is
     * replaced. An invalid current index is silently ignored.
     *
     * The token is referenced directly and is not copied.
     *
     * @param token             the token to insert
     *
     * @note                    This function is intended to be called from a rule action.
     */
    fun insertToken(token: pointer<Token>)
    {
        this.receiver.insertAt(this.currentIndex, token)
    }


    /**
     * Inserts a token at an index relative to the current token.
     *
     * The target index is calculated as currentIndex plus offset. The token
     * is emitted before the original token at that target index.
     *
     * If the resulting index is invalid, the request is silently ignored.
     * If another insertion already exists at the target index, it is replaced.
     *
     * The token is referenced directly and is not copied.
     *
     * @param offset            the index offset relative to the current token
     * @param token             the token to insert
     *
     * @note                    An offset of zero is equivalent to insertToken(token).
     */
    fun insertToken(offset: int, token: pointer<Token>)
    {
        this.receiver.insertAt(this.currentIndex + offset, token)
    }


    /**
     * Adjusts the current parenthesis balance.
     *
     * Positive values increase the balance and negative values decrease it.
     * No validation prevents the balance from becoming negative.
     *
     * Rule actions are responsible for applying the correct offset when
     * opening or closing parentheses are encountered.
     *
     * @param offset            the amount to add to the parenthesis balance
     *
     * @warning                 An incorrect offset may leave the FSM with an invalid
     *                          delimiter state.
     */
    inline fun changeParenthesis(offset: int):
        this.parenthesis += offset


    /**
     * Adjusts the current bracket balance.
     *
     * Positive values increase the balance and negative values decrease it.
     * No validation prevents the balance from becoming negative.
     *
     * Rule actions are responsible for applying the correct offset when
     * opening or closing brackets are encountered.
     *
     * @param offset            the amount to add to the bracket balance
     *
     * @warning                 An incorrect offset may leave the FSM with an invalid
     *                          delimiter state.
     */
    inline fun changeBracket(offset: int):
        this.bracket += offset
}
