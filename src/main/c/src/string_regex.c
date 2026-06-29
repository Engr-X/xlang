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

#include "string_regex.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "regex.h"


#define REGEX_PATTERN_BUFFER_SIZE 4096


static char regex_pattern_buffer[REGEX_PATTERN_BUFFER_SIZE];


static int regex_match_raw(const char* const pattern, const char* const str)
{
    int match_index;
    int match_length;

    match_index = re_match(pattern, str, &match_length);

    if (match_index != 0)
        return -1;

    return match_length;
}


static bool is_eof_pattern_range(const char* const pattern, const size_t begin, const size_t end)
{
    return end - begin == 2 && pattern[begin] == '\\' && pattern[begin + 1] == '0';
}


static size_t copy_normalized_pattern_range(
    char* const dest,
    const char* const pattern,
    const size_t begin,
    const size_t end)
{
    size_t dest_index = 0;

    for (size_t i = begin; i < end; i++)
    {
        if (pattern[i] != '\\' || i + 1 >= end)
        {
            dest[dest_index++] = pattern[i];
            continue;
        }

        const char escaped = pattern[++i];

        switch (escaped)
        {
            case 'n':
            {
                dest[dest_index++] = '\n';
                break;
            }
            case 'r':
            {
                dest[dest_index++] = '\r';
                break;
            }
            case 't':
            {
                dest[dest_index++] = '\t';
                break;
            }
            case 'f':
            {
                dest[dest_index++] = '\f';
                break;
            }
            case 'v':
            {
                dest[dest_index++] = '\v';
                break;
            }
            default:
            {
                dest[dest_index++] = '\\';
                dest[dest_index++] = escaped;
                break;
            }
        }
    }

    dest[dest_index] = '\0';

    return dest_index;
}


static bool is_wrapped_by_parentheses(const char* const pattern, const size_t begin, const size_t end)
{
    if (end - begin < 2 || pattern[begin] != '(' || pattern[end - 1] != ')')
        return false;

    int depth = 0;
    bool escaped = false;
    bool in_class = false;

    for (size_t i = begin; i < end; i++)
    {
        const char ch = pattern[i];

        if (escaped)
        {
            escaped = false;
            continue;
        }

        if (ch == '\\')
        {
            escaped = true;
            continue;
        }

        if (in_class)
        {
            if (ch == ']')
                in_class = false;

            continue;
        }

        if (ch == '[')
        {
            in_class = true;
            continue;
        }

        if (ch == '(')
        {
            depth++;
            continue;
        }

        if (ch == ')')
        {
            depth--;

            if (depth == 0 && i != end - 1)
                return false;

            if (depth < 0)
                return false;
        }
    }

    return depth == 0;
}


static int regex_match_range(const char* const pattern, size_t begin, size_t end, const char* const str)
{
    while (is_wrapped_by_parentheses(pattern, begin, end))
    {
        begin++;
        end--;
    }

    size_t branch_begin = begin;
    int depth = 0;
    bool escaped = false;
    bool in_class = false;
    bool has_branch = false;
    int best_match_length = -1;

    for (size_t i = begin; i < end; i++)
    {
        bool split_branch = false;
        const char ch = pattern[i];

        if (escaped)
        {
            escaped = false;
            continue;
        }

        if (ch == '\\')
        {
            escaped = true;
            continue;
        }

        if (in_class)
        {
            if (ch == ']')
                in_class = false;

            continue;
        }

        if (ch == '[')
        {
            in_class = true;
            continue;
        }

        if (ch == '(')
        {
            depth++;
            continue;
        }

        if (ch == ')')
        {
            if (depth > 0)
                depth--;

            continue;
        }

        split_branch = ch == '|' && depth == 0;

        if (!split_branch)
            continue;

        if (i > branch_begin)
        {
            const int match_length = regex_match_range(pattern, branch_begin, i, str);

            if (match_length > best_match_length)
                best_match_length = match_length;
        }

        if (i < end)
            has_branch = true;

        branch_begin = i + 1;
    }

    if (has_branch)
    {
        if (end > branch_begin)
        {
            const int match_length = regex_match_range(pattern, branch_begin, end, str);

            if (match_length > best_match_length)
                best_match_length = match_length;
        }

        return best_match_length;
    }

    const size_t length = end - begin;

    if (length == 0)
        return -1;

    if (is_eof_pattern_range(pattern, begin, end))
        return str[0] == '\0' ? 0 : -1;

    char* match_pattern = regex_pattern_buffer;
    bool heap_allocated = false;

    if (length + 1 > REGEX_PATTERN_BUFFER_SIZE)
    {
        match_pattern = malloc(length + 1);

        if (match_pattern == NULL)
            return -1;

        heap_allocated = true;
    }

    copy_normalized_pattern_range(match_pattern, pattern, begin, end);

    const int match_length = regex_match_raw(match_pattern, str);

    if (heap_allocated)
        free(match_pattern);

    return match_length;
}


int regex_match(const char* const pattern, const char* const str)
{
    if (pattern == NULL || str == NULL)
        return -1;

    return regex_match_range(pattern, 0, strlen(pattern), str);
}
