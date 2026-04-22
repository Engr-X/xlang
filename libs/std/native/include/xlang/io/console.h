#ifndef _XLANG_IO_CONSOLE_H_
#define _XLANG_IO_CONSOLE_H_

#include <stdbool.h>
#include <stdint.h>


extern void xlang_put_bool(const bool value);
extern void xlang_put_char(const char value);
extern void xlang_put_i8(const int8_t value);
extern void xlang_put_i16(const int16_t value);
extern void xlang_put_i32(const int32_t value);
extern void xlang_put_i64(const int64_t value);
extern void xlang_put_f32(const float value);
extern void xlang_put_f64(const double value);
extern void xlang_put_str(const char* value);

extern void xlang_putln_bool(const bool value);
extern void xlang_putln_char(const char value);
extern void xlang_putln_i8(const int8_t value);
extern void xlang_putln_i16(const int16_t value);
extern void xlang_putln_i32(const int32_t value);
extern void xlang_putln_i64(const int64_t value);
extern void xlang_putln_f32(const float value);
extern void xlang_putln_f64(const double value);
extern void xlang_putln_str(const char* value);


#endif