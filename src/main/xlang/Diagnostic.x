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

package xlang

import xlang.util.ArrayList
import xlang.util.string.String


/**
 * Identifies a source range inside one file.
 *
 * The location stores the file path together with the absolute offset,
 * line number, column number and character length of the range.
 *
 * The offset is normally zero-based, while line and column numbers normally
 * start at 1. The constructor stores numeric values directly without
 * validation.
 */
struct SourceLocation
{
    /**
     * Points to the file path that owns this source range.
     *
     * The constructor duplicates the supplied path, so this field is owned
     * independently from the caller's input string.
     */
    var filePath: pointer<char>

    /**
     * Stores the absolute character offset in the source file.
     */
    var offset: int

    /**
     * Stores the line number at which the range begins.
     */
    var line: int

    /**
     * Stores the column number at which the range begins.
     */
    var column: int

    /**
     * Stores the length of the source range in characters.
     */
    var length: int


    /**
     * Initializes a source location.
     *
     * The file path is duplicated. A null file path remains null.
     *
     * @param filePath          the source file path
     * @param offset            the absolute character offset
     * @param line              the starting line number
     * @param column            the starting column number
     * @param length            the range length in characters
     */
    fun __init__(filePath: pointer<char>, offset: int, line: int, column: int, length: int)
    {
        this.filePath = String.strdup(filePath)
        this.offset = offset
        this.line = line
        this.column = column
        this.length = length
    }
}


/**
 * Represents one compiler diagnostic.
 *
 * A diagnostic can be an error or warning and carries a machine-readable code,
 * a human-readable message and an optional source location.
 *
 * The message string is duplicated by the constructor. The location pointer is
 * stored directly because locations are already explicit value objects.
 */
struct Diagnostic
{
    // Diagnostic severity for normal messages.
    static val NORMAL_LEVEL: int = 0

    // Diagnostic severity for errors.
    static val ERROR_LEVEL: int = 1

    // Diagnostic severity for warnings.
    static val WARNING_LEVEL: int = 2

    // Diagnostic severity for compiler internal errors.
    static val INTERNAL_ERROR_LEVEL: int = 3



    // Diagnostic code for an invalid bracket pair table.
    static val INVALID_BRACKET_PAIR_TABLE: int = 1

    // Diagnostic code for a closing bracket without a matching opener.
    static val UNEXPECTED_CLOSE_BRACKET: int = 2

    // Diagnostic code for a closing bracket that does not match the current opener.
    static val MISMATCHED_CLOSE_BRACKET: int = 3

    // Diagnostic code for an opening bracket that is never closed.
    static val UNCLOSED_OPEN_BRACKET: int = 4

    // Diagnostic code for a token that does not match the expected parser rule.
    static val UNEXPECTED_TOKEN: int = 5

    // Diagnostic code for a null parser input.
    static val NULL_INPUT: int = 6

    // Diagnostic code for an empty parser input.
    static val EMPTY_INPUT: int = 7

    // Diagnostic code for a parser result-construction failure.
    static val CANNOT_CONSTRUCT_AST: int = 8

    // Diagnostic code for an invalid parser pattern atom.
    static val INVALID_PATTERN_ATOM: int = 9

    // Diagnostic code for multiple parser rules matching at the highest priority.
    static val AMBIGUOUS_PARSER_RULE: int = 10

    // Diagnostic code for a parser rule that cannot be parsed.
    static val CANNOT_PARSE: int = 11

    // Diagnostic code for a token sequence that cannot start a type.
    static val CANNOT_PARSE_TYPE: int = 12
    

    // Message for an invalid bracket pair table.
    static val INVALID_BRACKET_PAIR_TABLE_MSG: pointer<char> = "invalid bracket pair table"

    // Message for a closing bracket without a matching opener.
    static val UNEXPECTED_CLOSE_BRACKET_MSG: pointer<char> = "unexpected closing bracket"

    // Message for a closing bracket that does not match the current opener.
    static val MISMATCHED_CLOSE_BRACKET_MSG: pointer<char> = "mismatched closing bracket"

    // Message for an opening bracket that is never closed.
    static val UNCLOSED_OPEN_BRACKET_MSG: pointer<char> = "unclosed opening bracket"

    // Message for a token that does not match the expected parser rule.
    static val UNEXPECTED_TOKEN_MSG: pointer<char> = "unexpected token"

    // Message for a null parser input.
    static val NULL_INPUT_MSG: pointer<char> = "internal error: null input"

    // Message for an empty parser input.
    static val EMPTY_INPUT_MSG: pointer<char> = "internal error: empty input"

    // Message for a parser result-construction failure.
    static val CANNOT_CONSTRUCT_AST_MSG: pointer<char> = "internal error: cannot construct AST"

    // Message for an invalid parser pattern atom.
    static val INVALID_PATTERN_ATOM_MSG: pointer<char> = "internal error: invalid pattern atom"

    // Message for multiple parser rules matching at the highest priority.
    static val AMBIGUOUS_PARSER_RULE_MSG: pointer<char> = "internal error: ambiguous parser rule"

    // Message for a parser rule that cannot be parsed.
    static val CANNOT_PARSE_MSG: pointer<char> = "internal error: cannot parse"

    // Message for a token sequence that cannot be parsed as a type.
    static val CANNOT_PARSE_TYPE_MSG: pointer<char> = "cannot parse type"


    /**
    * Creates a normal diagnostic result.
    *
    * The returned Diagnostic uses NORMAL_LEVEL as its severity level and does
    * not contain an error code, source location or diagnostic message.
    *
    * This function may be used when an operation completes successfully but
    * still requires a Diagnostic object as its result.
    *
    * The returned Diagnostic is newly allocated, and the caller is responsible
    * for managing its lifetime.
    *
    * @return a newly allocated normal diagnostic.
    *
    * @note The returned diagnostic uses code 0, a null source-location list
    * and an empty message.
    * @note The returned diagnostic always has NORMAL_LEVEL severity.
    */
    static fun makeNormal() -> pointer<Diagnostic> =
        new Diagnostic(NORMAL_LEVEL, 0, new ArrayList(sizeof(SourceLocation)), "")
        

    /**
    * Creates an error diagnostic with an existing source location.
    *
    * The returned Diagnostic uses ERROR_LEVEL as its severity level.
    * The supplied diagnostic code, source location and message are passed
    * directly to the Diagnostic constructor.
    *
    * location may be null when the error cannot be associated with a specific
    * source range. This function does not copy or validate the location list.
    *
    * The caller must provide a valid null-terminated message and is responsible
    * for managing the lifetime of the returned Diagnostic.
    *
    * @param code               the machine-readable diagnostic code.
    * @param location           the optional source location list, or null if unavailable.
    * @param message            the human-readable diagnostic message.
    *
    * @return                   a newly allocated error diagnostic.
    *
    * @note                     The returned diagnostic always has ERROR_LEVEL severity.
    *
    * @warning                  Passing an invalid message or location pointer may cause
    *                           undefined behavior, depending on the Diagnostic constructor.
    */
    static fun makeError(code: int, location: pointer<ArrayList>, message: pointer<char>) -> pointer<Diagnostic> =
        new Diagnostic(ERROR_LEVEL, code, location, message)


    /**
     * Creates a warning diagnostic with an existing source location.
     *
     * @param code              the machine-readable diagnostic code
     * @param location          the optional source location list
     * @param message           the human-readable diagnostic message
     *
     * @return                  a newly allocated warning diagnostic
     */
    static fun makeWarning(code: int, location: pointer<ArrayList>, message: pointer<char>) -> pointer<Diagnostic> =
        new Diagnostic(WARNING_LEVEL, code, location, message)


    /**
     * Creates an internal-error diagnostic with an existing source location.
     *
     * The returned Diagnostic uses INTERNAL_ERROR_LEVEL as its severity level.
     * Internal errors normally represent compiler failures, invalid internal
     * states or conditions that should not be caused by ordinary source code.
     *
     * The supplied diagnostic code, source location and message are passed
     * directly to the Diagnostic constructor.
     *
     * location may be null when the internal error cannot be associated with a
     * specific source range. This function does not copy or validate the
     * location list.
     *
     * The caller must provide a valid null-terminated message and is responsible
     * for managing the lifetime of the returned Diagnostic.
     *
     * @param code              the machine-readable diagnostic code.
     * @param location          the optional source location list, or null if unavailable.
     * @param message           the human-readable internal-error message.
     *
     * @return                  a newly allocated internal-error diagnostic.
     *
     * @note                    The returned diagnostic always has INTERNAL_ERROR_LEVEL severity.
     *
     * @warning                 Internal errors should not be used for invalid user input that
     *                          can be reported as a normal error diagnostic.
     * @warning                 Passing an invalid message or location pointer may cause
     *                          undefined behavior, depending on the Diagnostic constructor.
     */
    static fun makeInternalError(code: int, location: pointer<ArrayList>, message: pointer<char>) -> pointer<Diagnostic> =
        new Diagnostic(INTERNAL_ERROR_LEVEL, code, location, message)
        

    /**
     * Stores the diagnostic severity.
     */
    var level: int

    /**
     * Stores the machine-readable diagnostic code.
     */
    var code: int

    /**
     * Points to the source locations associated with this diagnostic.
     *
     * The ArrayList stores SourceLocation values. An empty list means the
     * diagnostic is not tied to a specific file range.
     */
    var location: pointer<ArrayList>

    /**
     * Points to the human-readable diagnostic message.
     *
     * The constructor duplicates the supplied message, so this field is owned
     * independently from the caller's input string.
     */
    var message: pointer<char>


    /**
     * Initializes a diagnostic with an existing source location list.
     *
     * The severity level and diagnostic code are stored directly without
     * validation. The location list pointer is also stored directly and is not
     * copied.
     *
     * The message is duplicated with String.strdup, so later modifications to
     * the original message do not affect this Diagnostic.
     *
     * location may be null when the diagnostic cannot be associated with a
     * specific source range.
     *
     * The caller must provide a valid null-terminated message and keep a non-null
     * SourceLocation list valid for as long as this Diagnostic uses it.
     *
     * @param level             the diagnostic severity level.
     * @param code              the machine-readable diagnostic code.
     * @param location          the optional source location list, or null if unavailable.
     * @param message           the human-readable diagnostic message.
     *
     * @note                    This constructor does not verify that level is one of the defined
     *                          diagnostic-level constants.
     *
     * @warning                 Passing a null or invalid message pointer may cause undefined
     *                          behavior in String.strdup.
     */
    fun __init__(level: int, code: int, location: pointer<ArrayList>, message: pointer<char>)
    {
        this.level = level
        this.code = code
        this.location = location
        this.message = String.strdup(message)
    }

    /**
     * Tests whether this diagnostic is a normal informational message.
     *
     * The diagnostic is considered normal only when its level exactly equals
     * NORMAL_LEVEL.
     *
     * @return                  true if the diagnostic level is NORMAL_LEVEL; otherwise false.
     */
    fun isNormal() -> bool = this.level == NORMAL_LEVEL


    /**
     * Tests whether this diagnostic is an error.
     *
     * The diagnostic is considered an error only when its level exactly equals
     * ERROR_LEVEL. Internal errors are not included by this check.
     *
     * @return                  true if the diagnostic level is ERROR_LEVEL; otherwise false.
     */
    fun isError() -> bool = this.level == ERROR_LEVEL


    /**
     * Tests whether this diagnostic is a warning.
     *
     * The diagnostic is considered a warning only when its level exactly equals
     * WARNING_LEVEL.
     *
     * @return                  true if the diagnostic level is WARNING_LEVEL; otherwise false.
     */
    fun isWarning() -> bool = this.level == WARNING_LEVEL


    /**
     * Tests whether this diagnostic represents an internal compiler error.
     *
     * Internal errors normally indicate an invalid compiler state, a violated
     * internal invariant or another failure that should not result from ordinary
     * user input.
     *
     * The diagnostic is considered an internal error only when its level exactly
     * equals INTERNAL_ERROR_LEVEL.
     *
     * @return                  true if the diagnostic level is INTERNAL_ERROR_LEVEL;
     *                          otherwise false.
     *
     * @note                    Internal errors are intentionally distinct from normal source-code
     *                          errors reported by isError.
     */
    fun isInternalError() -> bool = this.level == INTERNAL_ERROR_LEVEL
}
