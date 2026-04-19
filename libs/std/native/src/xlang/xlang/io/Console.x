@class("Console")
package xlang.io


@native("__xlang_put_bool__")
native inline fun put(value: bool) -> void

@native("__xlang_putchar__")
native inline fun put(value: char) -> void

@native("__xlang_put_i8__")
native inline fun put(value: byte) -> void

@native("__xlang_put_i16__")
native inline fun put(value: short) -> void

@native("__xlang_put_i32__")
native inline fun put(value: int) -> void

@native("__xlang_put_i64__")
native inline fun put(value: long) -> void

@native("__xlang_put_f32__")
native inline fun put(value: float) -> void

@native("__xlang_put_f64__")
native inline fun put(value: double) -> void

@native("__xlang_putln_bool__")
native inline fun putln(value: bool) -> void

@native("__xlang_putln_char__")
native inline fun putln(value: char) -> void


@native("__xlang_putln_i8__")
native inline fun putln(value: byte) -> void

@native("__xlang_putln_i16__")
native inline fun putln(value: short) -> void

@native("__xlang_putln_i32__")
native inline fun putln(value: int) -> void

@native("__xlang_putln_i64__")
native inline fun putln(value: long) -> void

@native("__xlang_putln_f32__")
native inline fun putln(value: float) -> void

@native("__xlang_putln_f64__")
native inline fun putln(value: double) -> void
