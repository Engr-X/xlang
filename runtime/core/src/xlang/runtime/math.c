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

#include "xlang/runtime/math.h"

#include <math.h>

/**
 * xlang.Math native implementation for the xlang standard library.
 *
 * This file implements the xlang-prefixed C wrappers declared in math.h and
 * delegates each operation to the corresponding C standard math function.
 */


/**
 * Returns the trigonometric sine of an angle.
 *
 * @param x     an angle, in radians
 * @return      the sine of {@code x}
 */
double xlang_sin(double x)
{
    return sin(x);
}


/**
 * Returns the trigonometric cosine of an angle.
 *
 * @param x     an angle, in radians
 * @return      the cosine of {@code x}
 */
double xlang_cos(double x)
{
    return cos(x);
}


/**
 * Returns the trigonometric tangent of an angle.
 *
 * @param x     an angle, in radians
 * @return      the tangent of {@code x}
 */
double xlang_tan(double x)
{
    return tan(x);
}


/**
 * Returns the arc sine of a value.
 *
 * @param x     a value (typically in [-1, 1])
 * @return      the arc sine of {@code x}, in radians
 */
double xlang_asin(double x)
{
    return asin(x);
}


/**
 * Returns the arc cosine of a value.
 *
 * @param x     a value (typically in [-1, 1])
 * @return      the arc cosine of {@code x}, in radians
 */
double xlang_acos(double x)
{
    return acos(x);
}


/**
 * Returns the arc tangent of a value.
 *
 * @param x     a value
 * @return      the arc tangent of {@code x}, in radians
 */
double xlang_atan(double x)
{
    return atan(x);
}


/**
 * Returns Euler's number e raised to the power of a value.
 *
 * @param x     the exponent
 * @return      e raised to the power {@code x}
 */
double xlang_exp(double x)
{
    return exp(x);
}


/**
 * Returns the natural logarithm (base e) of a value.
 *
 * @param x     a value
 * @return      the natural logarithm of {@code x}
 */
double xlang_ln(double x)
{
    return log(x);
}


/**
 * Returns the base-10 logarithm of a value.
 *
 * @param x     a value
 * @return      the base-10 logarithm of {@code x}
 */
double xlang_log10(double x)
{
    return log10(x);
}


/**
 * Returns the correctly rounded positive square root of a value.
 *
 * @param x     a value
 * @return      the square root of {@code x}
 */
double xlang_sqrt(double x)
{
    return sqrt(x);
}


/**
 * Returns the cube root of a value.
 *
 * @param x     a value
 * @return      the cube root of {@code x}
 */
double xlang_cbrt(double x)
{
    return cbrt(x);
}


/**
 * Returns the IEEE 754-style remainder of two values.
 *
 * @param x the dividend
 * @param y the divisor
 * @return the IEEE 754-style remainder of {@code x / y}
 */
double xlang_IEEEremainder(double x, double y)
{
    return remainder(x, y);
}
