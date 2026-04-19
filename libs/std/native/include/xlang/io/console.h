#ifndef __XLANG_IO_CONSOLE_H__
#define __XLANG_IO_CONSOLE_H__

#include <stdbool.h>
#include <stdint.h>


extern void __xlang_put_bool__(const bool value);
extern void __xlang_putchar__(const char value);
extern void __xlang_put_i8__(const int8_t value);
extern void __xlang_put_i16__(const int16_t value);
extern void __xlang_put_i32__(const int32_t value);
extern void __xlang_put_i64__(const int64_t value);
extern void __xlang_put_f32__(const float value);
extern void __xlang_put_f64__(const double value);
extern void __xlang_put_str__(const char* value);

extern void __xlang_putln_bool__(const bool value);
extern void __xlang_putln_char__(const char value);
extern void __xlang_putln_i8__(const int8_t value);
extern void __xlang_putln_i16__(const int16_t value);
extern void __xlang_putln_i32__(const int32_t value);
extern void __xlang_putln_i64__(const int64_t value);
extern void __xlang_putln_f32__(const float value);
extern void __xlang_putln_f64__(const double value);
extern void __xlang_putln_str__(const char* value);


#endif