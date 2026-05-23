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

#ifndef _XLANG_RUNTIME_MATH_H_
#define _XLANG_RUNTIME_MATH_H_

#include "xlang/xtypedef.h"

/**
 * xlang.Math native C API for the xlang standard library.
 *
 * <p>For native targets, x_f32ing-point operations are routed through
 * xlang-prefixed wrappers that delegate to C standard math functions in
 * <math.h>. This keeps behavior aligned with the platform math library
 * while exposing a stable ABI for xlang runtime calls.
 */


/**
 * Returns the trigonometric sine of an angle.
 *
 * @param x     an angle, in radians
 * @return      the sine of {@code x}
 */
extern x_f64 xlang_sin(x_f64 x);


/**
 * Returns the trigonometric cosine of an angle.
 *
 * @param x     an angle, in radians
 * @return      the cosine of {@code x}
 */
extern x_f64 xlang_cos(x_f64 x);


/**
 * Returns the trigonometric tangent of an angle.
 *
 * @param x     an angle, in radians
 * @return      the tangent of {@code x}
 */
extern x_f64 xlang_tan(x_f64 x);


/**
 * Returns the arc sine of a value.
 *
 * @param x     a value (typically in [-1, 1])
 * @return      the arc sine of {@code x}, in radians
 */
extern x_f64 xlang_asin(x_f64 x);


/**
 * Returns the arc cosine of a value.
 *
 * @param x     a value (typically in [-1, 1])
 * @return      the arc cosine of {@code x}, in radians
 */
extern x_f64 xlang_acos(x_f64 x);


/**
 * Returns the arc tangent of a value.
 *
 * @param x     a value
 * @return      the arc tangent of {@code x}, in radians
 */
extern x_f64 xlang_atan(x_f64 x);


/**
 * Returns Euler's number e raised to the power of a value.
 *
 * @param x     the exponent
 * @return      e raised to the power {@code x}
 */
extern x_f64 xlang_exp(x_f64 x);


/**
 * Returns the natural logarithm (base e) of a value.
 *
 * @param x     a value
 * @return      the natural logarithm of {@code x}
 */
extern x_f64 xlang_ln(x_f64 x);


/**
 * Returns the base-10 logarithm of a value.
 *
 * @param x     a value
 * @return      the base-10 logarithm of {@code x}
 */
extern x_f64 xlang_log10(x_f64 x);


/**
 * Returns the correctly rounded positive square root of a value.
 *
 * @param x     a value
 * @return      the square root of {@code x}
 */
extern x_f64 xlang_sqrt(x_f64 x);


/**
 * Returns the cube root of a value.
 *
 * @param x     a value
 * @return      the cube root of {@code x}
 */
extern x_f64 xlang_cbrt(x_f64 x);


/**
 * Returns the IEEE 754-style remainder of two values.
 *
 * @param x the dividend
 * @param y the divisor
 * @return the IEEE 754-style remainder of {@code x / y}
 */
extern x_f64 xlang_IEEEremainder(x_f64 x, x_f64 y);


#endif
