#include "xlang/io/console.h"

#include <stdio.h>


void __xlang_put_bool__(const bool value)
{
    printf("%s", value ? "true" : "false");
}

void __xlang_putchar__(const char value)
{
    putchar(value);
}

void __xlang_put_i8__(const int8_t value)
{
    printf("%d", value);
}

void __xlang_put_i16__(const int16_t value)
{
    printf("%d", value);
}

void __xlang_put_i32__(const int32_t value)
{
    printf("%d", value);
}

void __xlang_put_i64__(const int64_t value)
{
    printf("%lld", value);
}

void __xlang_put_f32__(const float value)
{
    printf("%f", value);
}

void __xlang_put_f64__(const double value)
{
    printf("%lf", value);
}

void __xlang_put_str__(const char* value)
{
    printf("%s", value);
}


void __xlang_putln_bool__(const bool value)
{
    puts(value ? "true" : "false");
}

void __xlang_putln_char__(const char value)
{
    putchar(value);
    putchar('\n');
}

void __xlang_putln_i8__(const int8_t value)
{
    printf("%d\n", value);
}

void __xlang_putln_i16__(const int16_t value)
{
    printf("%d\n", value);
}

void __xlang_putln_i32__(const int32_t value)
{
    printf("%d\n", value);
}

void __xlang_putln_i64__(const int64_t value)
{
    printf("%lld\n", value);
}

void __xlang_putln_f32__(const float value)
{
    printf("%f\n", value);
}

void __xlang_putln_f64__(const double value)
{
    printf("%lf\n", value);
}

void __xlang_putln_str__(const char* value)
{
    puts(value);
}
