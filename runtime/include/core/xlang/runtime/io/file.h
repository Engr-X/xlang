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

#ifndef _XLANG_RUNTIME_FILE_H_
#define _XLANG_RUNTIME_FILE_H_

/**
 * 
 */

int file_size(const char* const path, int* const result);


/**
 * Reads an entire file into a caller-provided buffer.
 *
 * The file specified by `path` is opened in binary read mode. The function
 * determines the file size, seeks back to the beginning, and reads exactly
 * that many bytes into `dest`. * * After the file contents, the function writes a null terminator to
 * `dest[file_size]`. The destination therefore contains the original file * bytes followed by `'\0'`. The terminator is added for convenience and is
 * not included in the returned byte count. 
 * File contents are copied without character encoding conversion or
 * text-mode newline translation. The file may contain arbitrary binary data,
 * including embedded null bytes; therefore, the resulting buffer is only a
 * conventional C string when the file itself contains no embedded null bytes.
 *
 * The caller is responsible for ensuring that `dest` has space for at least
 * `file_size + 1` bytes. This function cannot verify the capacity of the
 * supplied buffer.
 * The file is closed before the function returns after it has been
 * successfully opened. If an error occurs after reading has started, `dest`
 * may contain partially read data and is not guaranteed to be * null-terminated. 
 * This function relies on `fseek` and `ftell`, so it may not work for
 * non-seekable streams or for files whose sizes cannot be represented by `long`.
 *
 * @param path                  pointer to a valid null-terminated file path
 * @param dest                  pointer to the buffer that receives the file contents and trailing null terminator 
 * 
 * @return                      the number of file bytes written to `dest` on success, excluding
 *                              the trailing null terminator.
 * @return                      `-1` if `path` or `dest` is null.
 * @return                      `-2` if the file cannot be opened.
 * @return                      `-3` if seeking to the end of the file fails.
 * @return                      `-4` if obtaining the file position fails.
 * @return                      `-5` if the file is too large for the return type or for the * trailing null terminator.
 * @return                      `-6` if seeking back to the beginning of the file fails.
 * @return                      `-7` if the complete file contents cannot be read. 
 *
 * @warning                     `path` must point to a valid null-terminated string.
 * @warning                     `dest` must point to writable memory containing at least `file_size + 1` bytes.
 * @warning                     The function does not receive the capacity of `dest`; passing an * undersized buffer causes undefined behavior.
 */
int read_file(const char* const path, char* const dest);

#endif
