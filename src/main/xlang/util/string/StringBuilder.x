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

package xlang.util.string

import xlang.System


/**
 * Mutable builder for null-terminated strings.
 *
 * StringBuilder owns an internal heap-allocated character buffer and grows it
 * when more space is required. The internal buffer is always kept
 * null-terminated, so it can be used with C-style string utilities.
 *
 * Invariant:
 * - length is the number of valid characters, excluding the null terminator.
 * - capacity is the number of allocated character slots.
 * - list[length] is always String.NULL_CHAR.
 */
struct StringBuilder
{
    // Initial number of character slots allocated for a new builder.
    private static val INIT_CAPACITY: int = 16

    /**
     * Buffer load threshold used before attempting to grow the buffer.
     *
     * When the next append would make the used size reach this ratio, resize()
     * is called to make sure the buffer can hold the new content.
     */
    private static val LOAD_FACTOR: double = 0.75

    /**
     * Number of characters currently stored in the builder.
     *
     * This value does not include the null terminator.
     */
    var length: int

    /**
     * Number of character slots currently allocated in the internal buffer.
     */
    private var capacity: int

    /**
     * Internal heap-allocated character buffer.
     *
     * The buffer is owned by this builder and stores a null-terminated string.
     */
    private var list: pointer<char>


    /**
     * Creates an empty StringBuilder.
     *
     * Allocates the initial buffer and writes the null terminator at index 0.
     * After construction, the builder represents an empty string.
     */
    fun __init__()
    {
        this.length = 0
        this.capacity = INIT_CAPACITY
        this.list = System.allocMemory(this.capacity * sizeof(char))
        this.list.deref = '\0'
    }


    /**
     * Creates a StringBuilder initialized with a null-terminated string.
     *
     * The source string is copied into an owned heap buffer. The builder length
     * is initialized to the source string length, excluding the null terminator.
     * Extra capacity is reserved so appending after construction does not
     * immediately require a reallocation for small strings.
     *
     * @param string            initial null-terminated string content
     */
    fun __init__(string: pointer<char>)
    {
        val strLength: int = String.strlen(string)

        this.length = strLength
        this.capacity = if strLength * 2 > INIT_CAPACITY: strLength * 2 else: INIT_CAPACITY
        this.list = System.allocMemory(this.capacity * sizeof(char))
        String.strcpy(this.list, string)
    }


    /**
     * Clears the current content without releasing the internal buffer.
     *
     * After clearing, the builder represents an empty null-terminated string.
     * The allocated capacity is kept so the builder can be reused without
     * allocating again for small appends.
     */
    fun clear()
    {
        this.length = 0
        this.list.deref = '\0'
    }


    /**
     * Ensures that the internal buffer can hold an upcoming insertion.
     *
     * The required capacity is:
     * current length + inserted length + null terminator.
     *
     * If the current capacity is too small, it is doubled until it can hold the
     * required number of character slots. The internal buffer is then reallocated.
     *
     * @param insertLength      number of characters that will be inserted
     */
    private fun resize(insertLength: int)
    {
        val requiredCapacity: int = insertLength + this.length + 1

        while this.capacity < requiredCapacity:
            this.capacity *= 2

        this.list = System.reallocMemory(this.list, this.capacity * sizeof(char))
    }


    /**
     * Appends one character code to the builder.
     *
     * The builder checks the load threshold before writing the character.
     * After the character is appended, the internal buffer is terminated with
     * String.NULL_CHAR.
     *
     * @param ch                character code to append
     */
    fun append(ch: char)
    {
        if this.length + 1 >= ((this.capacity as double) * LOAD_FACTOR) as int:
            this.resize(1)
        
        this.list[this.length++] = ch
        this.list[this.length] = '\0'
    }


    /**
     * Appends a null-terminated string to the builder.
     *
     * The source string is copied into the internal buffer after the current
     * content. The copied result remains null-terminated.
     *
     * The source pointer must point to a valid null-terminated string.
     *
     * @param str               pointer to the null-terminated string to append
     */
    fun append(str: pointer<char>)
    {
        val appendLength: int = String.strlen(str)

        if this.length + appendLength >= ((this.capacity as double) * LOAD_FACTOR) as int:
            this.resize(appendLength)

        String.strcpy(this.list + this.length, str)
        this.length += appendLength
    }


    /**
     * Appends another StringBuilder's current content.
     *
     * The source builder is copied by value into this builder. The source
     * object is not modified.
     *
     * @param sb                builder whose content should be appended
     */
    fun append(sb: pointer<StringBuilder>)
    {
        if sb == null:
            return

        this.append(sb.list)
    }


    /**
     * Appends a line feed character to the builder.
     */
    fun newline():
        this.append(String.LINE_FEED)


    /**
     * Returns the character at the given content index.
     *
     * Valid indexes are in the range [0, length). The null terminator is not
     * considered part of the readable content. If the index is outside the
     * valid range, this function returns -1 cast to char.
     *
     * @param index             zero-based character index to read
     *
     * @return                  character at index, or -1 as char if invalid
     */
    fun get(index: int) -> char
    {
        if index < 0 || index >= this.length:
            return (-1) as char

        return this.list[index]
    }


    /**
     * Copies the builder content into a destination buffer.
     *
     * The copied string is null-terminated. The destination buffer must have
     * enough capacity to store length + 1 characters.
     *
     * @param dest              pointer to the destination character buffer
     */
    fun toString(dest: pointer<char>):
        String.strcpy(dest, this.list)
}
