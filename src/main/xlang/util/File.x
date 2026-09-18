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

package xlang.util

import xlang.util.string.String
import xlang.util.string.StringBuilder


/**
 * Represents a source file loaded into memory.
 *
 * <p>A {@code File} stores the path of a file together with its textual
 * content. The file path is duplicated when the instance is created, while
 * the file content is loaded using {@code IO.readFile}.
 *
 * <p>If the file cannot be read, the content may be {@code null}.
 */
struct File
{
    /**
     * The duplicated path of the file.
     */
    private var path: pointer<char>

    /**
     * The null-terminated textual content of the file.
     *
     * <p>This value may be {@code null} if the file could not be read.
     */
    private var content: pointer<char>


    /**
     * Creates a file instance for the specified path and loads its content
     * into memory.
     *
     * <p>The supplied path is duplicated using {@code String.strdup}, so the
     * stored path does not depend on the lifetime of the original character
     * string.
     *
     * <p>The file content is loaded using {@code IO.readFile}. If the file
     * cannot be opened or read successfully, the stored content may be
     * {@code null}.
     *
     * @param path              a pointer to the null-terminated path of the file to load
     */
    constructor(path: pointer<char>)
    {
        this.path = String.strdup(path)
        this.content = IO.readFile(path)
    }


    /**
     * Returns the path of this file.
     *
     * <p>The returned pointer refers to the duplicated path stored by this
     * file instance.
     *
     * @return                  a pointer to the null-terminated file path
     */
    fun getPath() -> pointer<char> = this.path


    /**
     * Returns the textual content of this file.
     *
     * <p>The returned pointer refers directly to the loaded content buffer
     * and is not copied.
     *
     * @return                  a pointer to the null-terminated file content, or {@code null}
     *                          if the file content is unavailable
     */
    fun getContent() -> pointer<char> = this.content


    /**
     * Checks whether this file has valid loaded content.
     *
     * @return                  {@code true} if the content pointer is not {@code null};
     *                          {@code false} otherwise
     */
    fun hasContent() -> bool = this.content != null
}
