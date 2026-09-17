#include "xlang/xtypedef.h"
#include "util.h"

#include <errno.h>
#include <stdlib.h>
#include <string.h>


static int is_integer_suffix(const char value)
{
    return value == 'l' || value == 'L';
}


static int is_float_suffix(const char value)
{
    return value == 'f' || value == 'F';
}


static void trim_suffix(char* const text, int (*predicate)(char))
{
    const size_t length = text == NULL ? 0 : strlen(text);

    if (length > 0 && predicate(text[length - 1]))
        text[length - 1] = '\0';
}


static char* new_narrow_text(const x_char* const text)
{
    char* const result = text == NULL ? NULL : malloc((xchar_strlen(text) + 1) * sizeof(char));

    if (result != NULL)
        narrow_xchar_string(text, result);

    return result;
}


x_i32 stringToInt(const x_char* const text)
{
    char* const narrow_text = new_narrow_text(text);
    char* end = NULL;
    x_i32 result;

    if (narrow_text == NULL)
        return 0;

    trim_suffix(narrow_text, is_integer_suffix);
    errno = 0;
    result = strtol(narrow_text, &end, 0);

    if (errno != 0 || end == narrow_text || *end != '\0')
        result = 0;

    free(narrow_text);
    return result;
}


x_i64 stringToLong(const x_char* const text)
{
    char* const narrow_text = new_narrow_text(text);
    char* end = NULL;
    x_i64 result;

    if (narrow_text == NULL)
        return 0;

    trim_suffix(narrow_text, is_integer_suffix);
    errno = 0;
    result = strtoll(narrow_text, &end, 0);

    if (errno != 0 || end == narrow_text || *end != '\0')
        result = 0;

    free(narrow_text);
    return result;
}


x_f32 stringToFloat(const x_char* const text)
{
    char* const narrow_text = new_narrow_text(text);
    char* end = NULL;
    x_f32 result;

    if (narrow_text == NULL)
        return 0.0f;

    trim_suffix(narrow_text, is_float_suffix);
    errno = 0;
    result = strtof(narrow_text, &end);

    if (errno != 0 || end == narrow_text || *end != '\0')
        result = 0.0f;

    free(narrow_text);
    return result;
}


x_f64 stringToDouble(const x_char* const text)
{
    char* const narrow_text = new_narrow_text(text);
    char* end = NULL;
    x_f64 result;

    if (narrow_text == NULL)
        return 0.0;

    errno = 0;
    result = strtod(narrow_text, &end);

    if (errno != 0 || end == narrow_text || *end != '\0')
        result = 0.0;

    free(narrow_text);
    return result;
}
