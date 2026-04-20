#include "xlang/io/console.h"

#include <stdio.h>


void xlang_put_bool(const bool value)
{
    printf("%s", value ? "true" : "false");
}

void xlang_put_char(const char value)
{
    putchar(value);
}

void xlang_put_i8(const int8_t value)
{
    printf("%d", value);
}

void xlang_put_i16(const int16_t value)
{
    printf("%d", value);
}

void xlang_put_i32(const int32_t value)
{
    printf("%d", value);
}

void xlang_put_i64(const int64_t value)
{
    printf("%lld", value);
}

void xlang_put_f32(const float value)
{
    printf("%f", value);
}

void xlang_put_f64(const double value)
{
    printf("%lf", value);
}

void xlang_put_str(const char* value)
{
    printf("%s", value);
}


void xlang_putln_bool(const bool value)
{
    puts(value ? "true" : "false");
}

void xlang_putln_char(const char value)
{
    putchar(value);
    putchar('\n');
}

void xlang_putln_i8(const int8_t value)
{
    printf("%d\n", value);
}

void xlang_putln_i16(const int16_t value)
{
    printf("%d\n", value);
}

void xlang_putln_i32(const int32_t value)
{
    printf("%d\n", value);
}

void xlang_putln_i64(const int64_t value)
{
    printf("%lld\n", value);
}

void xlang_putln_f32(const float value)
{
    printf("%f\n", value);
}

void xlang_putln_f64(const double value)
{
    printf("%lf\n", value);
}

void xlang_putln_str(const char* value)
{
    puts(value);
}
