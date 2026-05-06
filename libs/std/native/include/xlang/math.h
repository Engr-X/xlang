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

#ifndef _XLANG_MATH_H_
#define _XLANG_MATH_H_

/**
 * xlang.Math native C API for the xlang standard library.
 *
 * <p>For native targets, floating-point operations are routed through
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
extern double xlang_sin(double x);


/**
 * Returns the trigonometric cosine of an angle.
 *
 * @param x     an angle, in radians
 * @return      the cosine of {@code x}
 */
extern double xlang_cos(double x);


/**
 * Returns the trigonometric tangent of an angle.
 *
 * @param x     an angle, in radians
 * @return      the tangent of {@code x}
 */
extern double xlang_tan(double x);


/**
 * Returns the arc sine of a value.
 *
 * @param x     a value (typically in [-1, 1])
 * @return      the arc sine of {@code x}, in radians
 */
extern double xlang_asin(double x);


/**
 * Returns the arc cosine of a value.
 *
 * @param x     a value (typically in [-1, 1])
 * @return      the arc cosine of {@code x}, in radians
 */
extern double xlang_acos(double x);


/**
 * Returns the arc tangent of a value.
 *
 * @param x     a value
 * @return      the arc tangent of {@code x}, in radians
 */
extern double xlang_atan(double x);


/**
 * Returns Euler's number e raised to the power of a value.
 *
 * @param x     the exponent
 * @return      e raised to the power {@code x}
 */
extern double xlang_exp(double x);


/**
 * Returns the natural logarithm (base e) of a value.
 *
 * @param x     a value
 * @return      the natural logarithm of {@code x}
 */
extern double xlang_ln(double x);


/**
 * Returns the base-10 logarithm of a value.
 *
 * @param x     a value
 * @return      the base-10 logarithm of {@code x}
 */
extern double xlang_log10(double x);


/**
 * Returns the correctly rounded positive square root of a value.
 *
 * @param x     a value
 * @return      the square root of {@code x}
 */
extern double xlang_sqrt(double x);


/**
 * Returns the cube root of a value.
 *
 * @param x     a value
 * @return      the cube root of {@code x}
 */
extern double xlang_cbrt(double x);


/**
 * Returns the IEEE 754-style remainder of two values.
 *
 * @param x the dividend
 * @param y the divisor
 * @return the IEEE 754-style remainder of {@code x / y}
 */
extern double xlang_IEEEremainder(double x, double y);


#endif
