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
    /**
     * Diagnostic severity for errors.
     */
    static val ERROR_LEVEL: int = 1

    /**
     * Diagnostic severity for warnings.
     */
    static val WARNING_LEVEL: int = 2

    /**
     * Diagnostic code for an invalid bracket pair table.
     */
    static val DIAG_INVALID_BRACKET_PAIR_TABLE: int = 1

    /**
     * Diagnostic code for a closing bracket without a matching opener.
     */
    static val DIAG_UNEXPECTED_CLOSE_BRACKET: int = 2

    /**
     * Diagnostic code for a closing bracket that does not match the current opener.
     */
    static val DIAG_MISMATCHED_CLOSE_BRACKET: int = 3

    /**
     * Diagnostic code for an opening bracket that is never closed.
     */
    static val DIAG_UNCLOSED_OPEN_BRACKET: int = 4

    /**
     * Message for an invalid bracket pair table.
     */
    static val INVALID_BRACKET_PAIR_TABLE_MSG: pointer<char> = "invalid bracket pair table"

    /**
     * Message for a closing bracket without a matching opener.
     */
    static val UNEXPECTED_CLOSE_BRACKET_MSG: pointer<char> = "unexpected closing bracket"

    /**
     * Message for a closing bracket that does not match the current opener.
     */
    static val MISMATCHED_CLOSE_BRACKET_MSG: pointer<char> = "mismatched closing bracket"

    /**
     * Message for an opening bracket that is never closed.
     */
    static val UNCLOSED_OPEN_BRACKET_MSG: pointer<char> = "unclosed opening bracket"

    /**
     * Creates an error diagnostic with an existing source location.
     *
     * @param code              the machine-readable diagnostic code
     * @param location          the optional source location
     * @param message           the human-readable diagnostic message
     * @return                  a newly allocated error diagnostic
     */
    static fun makeError(code: int, location: pointer<SourceLocation>, message: pointer<char>) -> pointer<Diagnostic> =
        new Diagnostic(ERROR_LEVEL, code, location, message)


    /**
     * Creates a warning diagnostic with an existing source location.
     *
     * @param code              the machine-readable diagnostic code
     * @param location          the optional source location
     * @param message           the human-readable diagnostic message
     * @return                  a newly allocated warning diagnostic
     */
    static fun makeWarning(code: int, location: pointer<SourceLocation>, message: pointer<char>) -> pointer<Diagnostic> =
        new Diagnostic(WARNING_LEVEL, code, location, message)


    /**
     * Creates an error diagnostic and constructs its source location inline.
     *
     * @param code              the machine-readable diagnostic code
     * @param filePath          the source file path
     * @param offset            the absolute character offset
     * @param line              the starting line number
     * @param column            the starting column number
     * @param length            the source range length in characters
     * @param message           the human-readable diagnostic message
     * @return                  a newly allocated error diagnostic
     */
    static fun makeError(
        code: int,
        filePath: pointer<char>,
        offset: int,
        line: int,
        column: int,
        length: int,
        message: pointer<char>) -> pointer<Diagnostic> =
        new Diagnostic(ERROR_LEVEL, code, filePath, offset, line, column, length, message)


    /**
     * Creates a warning diagnostic and constructs its source location inline.
     *
     * @param code              the machine-readable diagnostic code
     * @param filePath          the source file path
     * @param offset            the absolute character offset
     * @param line              the starting line number
     * @param column            the starting column number
     * @param length            the source range length in characters
     * @param message           the human-readable diagnostic message
     * @return                  a newly allocated warning diagnostic
     */
    static fun makeWarning(
        code: int,
        filePath: pointer<char>,
        offset: int,
        line: int,
        column: int,
        length: int,
        message: pointer<char>) -> pointer<Diagnostic> =
        new Diagnostic(WARNING_LEVEL, code, filePath, offset, line, column, length, message)


    /**
     * Stores the diagnostic severity.
     */
    var level: int

    /**
     * Stores the machine-readable diagnostic code.
     */
    var code: int

    /**
     * Points to the source location associated with this diagnostic.
     *
     * A null location means the diagnostic is not tied to a specific file
     * range.
     */
    var location: pointer<SourceLocation>

    /**
     * Points to the human-readable diagnostic message.
     *
     * The constructor duplicates the supplied message, so this field is owned
     * independently from the caller's input string.
     */
    var message: pointer<char>


    /**
     * Initializes a diagnostic with an existing source location.
     *
     * The location pointer is stored directly. The message is duplicated.
     *
     * @param level             the diagnostic severity
     * @param code              the machine-readable diagnostic code
     * @param location          the optional source location
     * @param message           the human-readable diagnostic message
     */
    fun __init__(level: int, code: int, location: pointer<SourceLocation>, message: pointer<char>)
    {
        this.level = level
        this.code = code
        this.location = location
        this.message = String.strdup(message)
    }


    /**
     * Initializes a diagnostic and creates its source location inline.
     *
     * The file path and message are duplicated.
     *
     * @param level             the diagnostic severity
     * @param code              the machine-readable diagnostic code
     * @param filePath          the source file path
     * @param offset            the absolute character offset
     * @param line              the starting line number
     * @param column            the starting column number
     * @param length            the source range length in characters
     * @param message           the human-readable diagnostic message
     */
    fun __init__(
        level: int,
        code: int,
        filePath: pointer<char>,
        offset: int,
        line: int,
        column: int,
        length: int,
        message: pointer<char>)
    {
        this.level = level
        this.code = code
        this.location = new SourceLocation(filePath, offset, line, column, length)
        this.message = String.strdup(message)
    }


    /**
     * Tests whether this diagnostic is an error.
     *
     * @return                  true if level is ERROR_LEVEL; otherwise false
     */
    fun isError() -> bool = this.level == ERROR_LEVEL


    /**
     * Tests whether this diagnostic is a warning.
     *
     * @return                  true if level is WARNING_LEVEL; otherwise false
     */
    fun isWarning() -> bool = this.level == WARNING_LEVEL
}
