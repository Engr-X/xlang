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

/**
 * Native mathematical function declarations for the xlang runtime.
 *
 * This header defines the low-level C ABI used by the xlang standard library
 * Math module. The functions declared here expose xlang-prefixed wrappers for
 * common floating-point mathematical operations.
 *
 * On native targets, these functions are normally implemented by delegating to
 * the host C math library, such as the functions provided by <math.h>. This
 * keeps the implementation close to the platform's floating-point behavior
 * while giving xlang a stable runtime symbol namespace.
 *
 * All functions in this header currently operate on x_f64 values. Higher-level
 * overloads, integer conversions, generic math dispatch, compile-time constant
 * folding, and x_f32-specific variants should be implemented above this native
 * ABI layer or added explicitly as separate runtime entry points.
 *
 * Unless otherwise documented by the implementation, special floating-point
 * values such as NaN, positive infinity, and negative infinity follow the
 * behavior of the underlying platform math library.
 * 
 * @author Di Wang
 * @since alpha-1.1.0
 */

#include "xlang/xtypedef.h"


/**
 * Returns the trigonometric sine of an angle.
 *
 * The input angle is interpreted in radians. The result is the sine of the
 * given angle, using the behavior of the underlying native math backend.
 *
 * @param x                     the input angle, in radians
 * @return                      the sine of x
 */
extern inline x_f64 xlang_sin(x_f64 x);


/**
 * Returns the trigonometric cosine of an angle.
 *
 * The input angle is interpreted in radians. The result is the cosine of the
 * given angle, using the behavior of the underlying native math backend.
 *
 * @param x                     the input angle, in radians
 * @return                      the cosine of x
 */
extern inline x_f64 xlang_cos(x_f64 x);


/**
 * Returns the trigonometric tangent of an angle.
 *
 * The input angle is interpreted in radians. The result is the tangent of the
 * given angle, using the behavior of the underlying native math backend.
 *
 * @param x                     the input angle, in radians
 * @return                      the tangent of x
 */
extern inline x_f64 xlang_tan(x_f64 x);


/**
 * Returns the arc sine of a value.
 *
 * The result is expressed in radians. For real-valued results, the input is
 * normally expected to be in the range [-1, 1]. Inputs outside that domain
 * follow the behavior of the underlying native math backend, usually producing
 * NaN for standard IEEE-style implementations.
 *
 * @param x                     the input value
 * @return                      the arc sine of x, in radians
 */
extern inline x_f64 xlang_asin(x_f64 x);


/**
 * Returns the arc cosine of a value.
 *
 * The result is expressed in radians. For real-valued results, the input is
 * normally expected to be in the range [-1, 1]. Inputs outside that domain
 * follow the behavior of the underlying native math backend, usually producing
 * NaN for standard IEEE-style implementations.
 *
 * @param x                     the input value
 * @return                      the arc cosine of x, in radians
 */
extern inline x_f64 xlang_acos(x_f64 x);


/**
 * Returns the arc tangent of a value.
 *
 * The result is expressed in radians. The returned angle is determined by the
 * behavior of the underlying native math backend.
 *
 * @param x                     the input value
 * @return                      the arc tangent of x, in radians
 */
extern inline x_f64 xlang_atan(x_f64 x);


/**
 * Returns Euler's number raised to the power of a value.
 *
 * This function computes e^x, where e is Euler's number. Overflow, underflow,
 * NaN, and infinity behavior follow the underlying native math backend.
 *
 * @param x                     the exponent
 * @return                      e raised to the power of x
 */
extern inline x_f64 xlang_exp(x_f64 x);


/**
 * Returns the natural logarithm of a value.
 *
 * This function computes the base-e logarithm of x. For real-valued results,
 * the input is normally expected to be greater than zero. Domain errors,
 * negative inputs, zero, NaN, and infinity follow the behavior of the
 * underlying native math backend.
 *
 * @param x                     the input value
 * @return                      the natural logarithm of x
 */
extern inline x_f64 xlang_ln(x_f64 x);


/**
 * Returns the base-10 logarithm of a value.
 *
 * This function computes the decimal logarithm of x. For real-valued results,
 * the input is normally expected to be greater than zero. Domain errors,
 * negative inputs, zero, NaN, and infinity follow the behavior of the
 * underlying native math backend.
 *
 * @param x                     the input value
 * @return                      the base-10 logarithm of x
 */
extern inline x_f64 xlang_log10(x_f64 x);


/**
 * Returns the square root of a value.
 *
 * For real-valued results, the input is normally expected to be greater than
 * or equal to zero. Negative inputs, NaN, and infinity follow the behavior of
 * the underlying native math backend.
 *
 * @param x                     the input value
 * @return                      the square root of x
 */
extern inline x_f64 xlang_sqrt(x_f64 x);


/**
 * Returns the cube root of a value.
 *
 * This function computes the real cube root of x. Unlike square root, cube root
 * is defined for negative real values as well as positive real values.
 *
 * @param x                     the input value
 * @return                      the cube root of x
 */
extern inline x_f64 xlang_cbrt(x_f64 x);


/**
 * Returns the IEEE 754-style floating-point remainder of two values.
 *
 * This function computes the floating-point remainder of x divided by y using
 * the platform math backend's IEEE-style remainder behavior. The result is not
 * the same as a simple modulo operation for all inputs.
 *
 * Division by zero, NaN, infinity, and signed-zero behavior follow the
 * underlying native math backend.
 *
 * @param x                     the dividend
 * @param y                     the divisor
 * @return                      the IEEE-style floating-point remainder of x divided by y
 */
extern inline x_f64 xlang_IEEEremainder(x_f64 x, x_f64 y);

#endif
