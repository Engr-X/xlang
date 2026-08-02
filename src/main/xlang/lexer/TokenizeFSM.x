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
import xlang.util.string.String
import xlang.util.string.StringBuilder


/**
 * Defines one pattern-matching rule used by the tokenizer.
 *
 * A tokenize rule contains an identifier, an FSM state, a regular-expression
 * pattern and an action function. The rule is eligible only when its state
 * matches the current state of the TokenizeFSM.
 *
 * When the pattern matches the source text at the current cursor position,
 * the action receives information about the matched text and may create a
 * Token, update the FSM state, advance the cursor or accumulate additional
 * text.
 *
 * The pattern and action are stored directly. The caller must keep them valid
 * for as long as the rule may be used.
 */
struct TokenizeRule
{
    /**
     * Stores the application-defined identifier of this rule.
     *
     * The tokenizer does not interpret this value directly. It may be used
     * by callers for diagnostics, debugging or rule classification.
     */
    var id: int

    /**
     * Stores the FSM state in which this rule is active.
     */
    var state: int

    /**
     * Points to the regular-expression pattern used by this rule.
     *
     * Matching begins at the current tokenizer cursor. The pattern string is
     * referenced directly and is not copied.
     */
    var pattern: pointer<char>

    /**
     * Stores the action executed after this rule matches.
     *
     * The action receives the reusable LexInput describing the matched text
     * and the active TokenizeFSM. It may return a token or null when the
     * matched input should not directly produce a token.
     */
    var action: (pointer<LexInput>, pointer<TokenizeFSM>) -> pointer<Token>


    /**
     * Initializes a tokenize rule.
     *
     * All supplied values are stored directly without validation or copying.
     * The pattern must be a valid null-terminated regular-expression string,
     * and the action must be a valid callable function.
     *
     * The caller is responsible for keeping the pattern and action valid
     * while this rule is used.
     *
     * @param id                the application-defined rule identifier
     * @param state             the FSM state in which the rule is active
     * @param pattern           the regular-expression pattern matched at the cursor
     * @param action            the function executed after a successful match
     *
     * @warning                 A null or invalid pattern or action may cause undefined
     *                          behavior when the rule is evaluated.
     */
    fun __init__(id: int, state: int, pattern: pointer<char>, action: (pointer<LexInput>, pointer<TokenizeFSM>) -> pointer<Token>)
    {
        this.id = id
        this.state = state
        this.pattern = pattern
        this.action = action
    }
}


/**
 * Controls tokenization state and matching over a source-text buffer.
 *
 * The FSM stores the source code, current state, cursor position, reusable
 * LexInput object and a StringBuilder that token actions may use to accumulate
 * text across multiple matches.
 *
 * Rules are evaluated in their supplied order. At most one rule action is
 * executed by each call to apply: the first rule in the current state whose
 * pattern successfully matches at the cursor.
 *
 * Rule actions are responsible for advancing the cursor. The apply function
 * itself does not consume matched characters or change the source position.
 *
 * The source-code pointer is stored directly and is not copied. The caller
 * must keep the source buffer valid for the lifetime of this FSM.
 */
struct TokenizeFSM
{
    /**
     * Identifies the default tokenizer state.
     */
    static val DEFAULT: int = 1

    /**
     * Points to the source-code buffer being tokenized.
     *
     * The buffer is referenced directly and is expected to remain valid and
     * accessible while tokenization is in progress.
     */
    var code: pointer<char>

    /**
     * Stores the current tokenizer state.
     *
     * Only rules whose state equals this value are considered by apply.
     */
    var state: int

    /**
     * Stores text accumulated by tokenize-rule actions.
     *
     * Actions may append characters or strings through the append methods.
     * This FSM does not automatically clear the accumulator between matches.
     */
    var accumulator: pointer<StringBuilder>

    /**
     * Stores the current position in the source-code buffer.
     *
     * The offset is normally zero-based, while line and column numbers
     * normally begin at one.
     */
    var cursorPos: pointer<LexPosition>

    /**
     * Stores the starting position of the most recent rule match.
     *
     * This object is reused and exposed to rule actions through input.
     */
    private var inputPos: pointer<LexPosition>

    /**
     * Stores reusable information about the most recent matched text.
     *
     * The same LexInput object is updated before every action invocation.
     * Actions must not assume that its contents remain unchanged after apply
     * is called again.
     */
    private var input: pointer<LexInput>


    /**
     * Initializes a tokenizer FSM for a source-code buffer.
     *
     * The FSM begins in DEFAULT state at offset 0, line 1 and column 1.
     * An empty accumulator and reusable lexical-input objects are allocated.
     *
     * The source pointer is stored directly without copying or validating
     * the source text.
     *
     * The caller must provide a valid source buffer and keep it alive while
     * the FSM is used.
     *
     * @param code              the null-terminated source-code buffer to tokenize
     *
     * @warning                 Passing a null or invalid source pointer may cause undefined
     *                          behavior when apply reads from the buffer.
     */
    fun __init__(code: pointer<char>)
    {
        this.code = code
        this.state = DEFAULT
        this.accumulator = new StringBuilder()
        this.cursorPos = new LexPosition(0, 1, 1)
        this.inputPos = new LexPosition(0, 1, 1)
        this.input = new LexInput(this.inputPos, 0 as char, null as pointer<char>, 0)
    }


    /**
     * Changes the current tokenizer state.
     *
     * The new state takes effect immediately. Subsequent calls to apply
     * consider only rules associated with this state.
     *
     * @param state             the new tokenizer state identifier
     */
    fun setState(state: int):
        this.state = state


    /**
     * Returns the current tokenizer state.
     *
     * @return                  the active tokenizer state identifier
     */
    fun getState(): int = this.state


    /**
     * Appends one character to the tokenizer accumulator.
     *
     * The accumulator is not automatically cleared before or after this
     * operation.
     *
     * @param c                 the character to append
     */
    fun append(c: char):
        this.accumulator.append(c)


    /**
     * Appends a null-terminated string to the tokenizer accumulator.
     *
     * The supplied string is passed directly to StringBuilder.append.
     * The accumulator is not automatically cleared before or after this
     * operation.
     *
     * The caller must provide a valid null-terminated string.
     *
     * @param str               the string to append
     *
     * @warning                 Passing a null or invalid string pointer may cause undefined
     *                          behavior, depending on StringBuilder.append.
     */
    fun append(str: pointer<char>):
        this.accumulator.append(str)


    /**
     * Returns the mutable tokenizer cursor position.
     *
     * The returned pointer refers to the same LexPosition object used
     * internally by the FSM. Modifying it directly immediately changes
     * where the tokenizer reads.
     *
     * The caller must keep offset, line and column consistent with the
     * source-code buffer.
     *
     * @return                  the internal mutable cursor-position pointer
     *
     * @warning                 Incorrect direct modifications may cause invalid memory access
     *                          or incorrect token positions.
     */
    fun getCursorPtr() -> pointer<LexPosition> = this.cursorPos


    /**
     * Advances the cursor horizontally by a specified amount.
     *
     * Both the absolute offset and column number are increased by n.
     * The line number is not changed.
     *
     * Negative values move the cursor backward. No bounds checking prevents
     * the offset or column from becoming invalid.
     *
     * This function is intended for matched text that does not contain
     * newline characters. Newline-aware actions must update line and column
     * separately through the cursor pointer.
     *
     * @param n                 the number of characters by which to move the cursor
     *
     * @note                    This function does not verify the source-buffer boundaries.
     *
     * @warning                 Using this function for text containing newlines produces
     *                          incorrect line and column information.
     */
    fun updateCursor(n: int)
    {
        this.cursorPos.offset += n
        this.cursorPos.column += n
    }


    /**
     * Applies the first matching rule for the current state and cursor.
     *
     * Rules are examined in the order in which they appear in the supplied
     * rule table. Rules belonging to another FSM state are skipped.
     *
     * For each active rule, its pattern is matched against the source text
     * beginning at the current cursor offset. A non-negative match length is
     * treated as a successful match.
     *
     * After a successful match, the matched characters are copied into a
     * newly allocated null-terminated buffer. The reusable LexInput is then
     * updated with the match position, previous character, matched text and
     * match length before the rule action is invoked.
     *
     * The action result is returned immediately, even when it is null.
     * Therefore, a null result may mean either that no rule matched or that
     * the first matching rule intentionally skipped the input.
     *
     * This function does not advance the cursor. The matched rule action must
     * consume input by updating the FSM cursor. A zero-length match is allowed
     * by this implementation and may cause repeated matching at the same
     * position unless the action changes the cursor or state.
     *
     * The temporary matched-text buffer becomes input.text. The action or
     * surrounding memory-management system is responsible for its ownership
     * and lifetime.
     *
     * @param rules             the ordered array of tokenize-rule pointers
     * @param rulesLength       the number of accessible entries in the rule array
     *
     * @return                  the value returned by the first matching rule action, or null if no active rule matches
     *
     * @note                    This function does not automatically create an EOF token when
     *                          the cursor reaches the end of the source text.
     * @warning                 The rule array, each accessed rule, its pattern and its action
     *                          must all be valid.
     * @warning                 An incorrect rulesLength may cause out-of-bounds memory access.
     * @warning                 If the cursor offset is outside the source buffer, calculating
     *                          currentPtr or matching a pattern may cause undefined behavior.
     */
    fun apply(rules: pointer<pointer<TokenizeRule>>, rulesLength: int) -> pointer<Token>
    {
        val currentPtr: pointer<char> = this.code + this.cursorPos.offset

        for (var i: int = 0; i < rulesLength; i++):
        {
            val rule: pointer<TokenizeRule> = rules[i]

            if rule.state == this.state:
            {
                val matchLength: int = String.strRegMatch(rule.pattern, currentPtr)

                if matchLength >= 0:
                {
                    val preChar: char = if this.cursorPos.offset == 0: -1 else: this.code[this.cursorPos.offset - 1] as int
                    val token: pointer<char> = System.allocMemory((matchLength + 1) * sizeof(char)) as pointer<char>
                    String.strncpy(token, currentPtr, matchLength)

                    this.inputPos.offset = this.cursorPos.offset
                    this.inputPos.line = this.cursorPos.line
                    this.inputPos.column = this.cursorPos.column

                    this.input.prevChar = preChar
                    this.input.text = token
                    this.input.textLength = matchLength

                    val result: pointer<Token> = rule.action(this.input, this)
                    return result
                }
            }
        }

        return null
    }
}
