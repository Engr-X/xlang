@class("Math")
package xlang


/**
 * The {@code double} value that is nearest to the mathematical constant
 * pi, defined as the ratio of a circle's circumference to its diameter.
 * Use this constant for geometry and trigonometric calculations in radians.
 */
val PI: double = 3.14159265358979323846


/**
 * The {@code double} value that is nearest to Euler's number e.
 * This is the base of the natural logarithm and appears in exponential
 * growth, continuous compounding, and many calculus formulas.
 */
val E: double = 2.7182818284590452354


/**
 * The {@code double} value that is nearest to the golden ratio φ.
 * This constant appears in geometry, continued fractions, and some
 * growth patterns in mathematics and nature.
 */
val PHI: double = 1.6180339887498948482


/**
 * The {@code double} value that is nearest to τ, the ratio of a circle's
 * circumference to its radius. This is equal to {@code 2 * PI} and
 * represents one full turn in radians.
 */
val TAU: double = 6.2831853071795864769


/**
 * The base-2 logarithm of {@code E}.
 * Equivalent to {@code log2(e)} and useful when converting
 * between natural-log and binary-log expressions.
 */
val LOG2E: double = 1.4426950408889634074


/**
 * The base-10 logarithm of {@code E}.
 * Equivalent to {@code log10(e)} and useful when converting
 * between natural-log and common-log expressions.
 */
val LOG10E: double = 0.43429448190325182765


/**
 * The natural logarithm of 2.
 * Equivalent to {@code ln(2)} and frequently used in
 * information theory and complexity formulas.
 */
val LN2: double = 0.69314718055994530942


/**
 * The natural logarithm of 10.
 * Equivalent to {@code ln(10)} and useful when converting
 * between natural logarithms and base-10 logarithms.
 */
val LN10: double = 2.30258509299404568402


/**
 * One half of {@code PI}.
 * Equivalent to {@code PI / 2}, commonly used in angle identities.
 */
val PI_2: double = 1.57079632679489661923


/**
 * One quarter of {@code PI}.
 * Equivalent to {@code PI / 4}, commonly used for 45-degree angles.
 */
val PI_4: double = 0.78539816339744830962


/**
 * Reciprocal of {@code PI}.
 * Equivalent to {@code 1 / PI}.
 */
val ONE_PI: double = 0.31830988618379067154


/**
 * Twice the reciprocal of {@code PI}.
 * Equivalent to {@code 2 / PI}.
 */
val TWO_PI: double = 0.63661977236758134308


/**
 * Two divided by the square root of {@code PI}.
 * Equivalent to {@code 2 / sqrt(PI)}.
 */
val TWO_SQRTPI: double = 1.12837916709551257390


/**
 * Square root of 2.
 * Equivalent to {@code sqrt(2)}.
 */
val SQRT2: double = 1.41421356237309504880


/**
 * Square root of one half.
 * Equivalent to {@code sqrt(1/2)} and also {@code 1 / sqrt(2)}.
 */
val SQRT1_2: double = 0.70710678118654752440


/**
 * Constant by which to multiply an angular value in degrees to obtain an
 * angular value in radians.
 */
private val DEGREES_TO_RADIANS: double = 0.017453292519943295


/**
 * Constant by which to multiply an angular value in radians to obtain an
 * angular value in degrees.
 */
private val RADIANS_TO_DEGREES: double = 57.29577951308232


/**
 * Returns the successor of the given {@code int} value ({@code x + 1}).
 * This method delegates to {@code Math.addExact}, so an overflow raises
 * an arithmetic exception instead of silently wrapping around.
 *
 * @param x input integer
 * @return {@code x + 1} when no overflow occurs
 * @since beta-1.0.0
 */
// fun succ(x: int) -> int = Math.addExact(x, 1)


/**
 * Returns the successor of the given {@code long} value ({@code x + 1}).
 * This method delegates to {@code Math.addExact} and keeps overflow
 * behavior explicit by throwing on out-of-range results.
 *
 * @param x input long integer
 * @return {@code x + 1} when no overflow occurs
 * @since beta-1.0.0
 */
// fun succ(x: long) -> long = Math.addExact(x, 1L)


/**
 * Returns the predecessor of the given {@code int} value ({@code x - 1}).
 * This method delegates to {@code Math.subtractExact}, so overflow is
 * reported as an arithmetic exception rather than wrapped.
 *
 * @param x input integer
 * @return {@code x - 1} when no overflow occurs
 * @since beta-1.0.0
 */
// fun pred(x: int) -> int = Math.subtractExact(x, 1)


/**
 * Returns the predecessor of the given {@code long} value ({@code x - 1}).
 * This method delegates to {@code Math.subtractExact} and throws on
 * overflow to avoid implicit two's-complement wraparound.
 *
 * @param x input long integer
 * @return {@code x - 1} when no overflow occurs
 * @since beta-1.0.0
 */
// fun pred(x: long) -> long = Math.subtractExact(x, 1L)


/**
 * Returns the trigonometric sine of an angle in radians.
 * The implementation delegates to {@code StrictMath.sin}, so behavior
 * is consistent with strict floating-point rules on the JVM.
 *
 * Special cases:
 * <ul>
 *   <li>If {@code x} is NaN, the result is NaN.</li>
 *   <li>If {@code x} is +Infinity or -Infinity, the result is NaN.</li>
 *   <li>If {@code x} is +0.0 or -0.0, the result keeps the same signed zero.</li>
 * </ul>
 *
 * The returned value is a floating-point approximation of the exact
 * mathematical sine, with normal IEEE-754 rounding behavior.
 *
 * @param x an angle, in radians
 * @return the sine of {@code x}
 * @since beta-1.0.0
 */
@native("sin")
native inline fun sin(x: double) -> double


/**
 * Returns the trigonometric cosine of an angle in radians.
 * The implementation delegates to {@code StrictMath.cos} to keep
 * deterministic floating-point behavior across platforms.
 *
 * Special cases:
 * <ul>
 *   <li>If {@code x} is NaN, the result is NaN.</li>
 *   <li>If {@code x} is +Infinity or -Infinity, the result is NaN.</li>
 *   <li>If {@code x} is +0.0 or -0.0, the result is exactly {@code 1.0}.</li>
 * </ul>
 *
 * The returned value is a floating-point approximation of the exact
 * mathematical cosine, subject to IEEE-754 rounding behavior.
 *
 * @param x an angle, in radians
 * @return the cosine of {@code x}
 * @since beta-1.0.0
 */
@native("cos")
native inline fun cos(x: double) -> double


/**
 * Returns the trigonometric tangent of an angle in radians.
 * The implementation delegates to {@code StrictMath.tan}.
 *
 * Special cases:
 * <ul>
 *   <li>If {@code x} is NaN, the result is NaN.</li>
 *   <li>If {@code x} is +Infinity or -Infinity, the result is NaN.</li>
 *   <li>If {@code x} is +0.0 or -0.0, the result keeps the same signed zero.</li>
 * </ul>
 *
 * Tangent grows quickly near odd multiples of {@code PI / 2}, so
 * large-magnitude outputs are expected in those regions.
 *
 * @param x an angle, in radians
 * @return the tangent of {@code x}
 * @since beta-1.0.0
 */
@native("tan") 
native inline fun tan(x: double) -> double


/**
 * Returns the trigonometric secant of an angle in radians.
 * Secant is defined as {@code 1 / cos(x)}.
 *
 * Special cases follow reciprocal floating-point behavior:
 * <ul>
 *   <li>If {@code x} is NaN, the result is NaN.</li>
 *   <li>If {@code cos(x)} is +0.0 or -0.0, the result is signed infinity.</li>
 *   <li>Near zeros of cosine, magnitude may become very large.</li>
 * </ul>
 *
 * @param x an angle, in radians
 * @return the secant of {@code x}
 * @since beta-1.0.0
 */
inline fun sec(x: double) -> double = 1.0 / cos(x)


/**
 * Returns the trigonometric cosecant of an angle in radians.
 * Cosecant is defined as {@code 1 / sin(x)}.
 *
 * Special cases follow reciprocal floating-point behavior:
 * <ul>
 *   <li>If {@code x} is NaN, the result is NaN.</li>
 *   <li>If {@code sin(x)} is +0.0 or -0.0, the result is signed infinity.</li>
 *   <li>Near zeros of sine, magnitude may become very large.</li>
 * </ul>
 *
 * @param x an angle, in radians
 * @return the cosecant of {@code x}
 * @since beta-1.0.0
 */
inline fun csc(x: double) -> double = 1.0 / sin(x)


/**
 * Returns the trigonometric cotangent of an angle in radians.
 * Cotangent is defined as {@code 1 / tan(x)}.
 *
 * Special cases follow reciprocal floating-point behavior:
 * <ul>
 *   <li>If {@code x} is NaN, the result is NaN.</li>
 *   <li>If {@code tan(x)} is +0.0 or -0.0, the result is signed infinity.</li>
 *   <li>Near zeros of tangent, magnitude may become very large.</li>
 * </ul>
 *
 * @param x an angle, in radians
 * @return the cotangent of {@code x}
 * @since beta-1.0.0
 */
inline fun cot(x: double) -> double = 1.0 / tan(x)


/**
 * Returns the inverse sine (arcsin) of a value.
 * The returned angle is in the interval {@code [-PI/2, PI/2]}.
 *
 * Special cases:
 * <ul>
 *   <li>If {@code x} is NaN, the result is NaN.</li>
 *   <li>If {@code abs(x) > 1}, the result is NaN.</li>
 *   <li>If {@code x} is +0.0 or -0.0, the result keeps the same signed zero.</li>
 * </ul>
 *
 * The computation delegates to {@code StrictMath.asin} and returns
 * a floating-point approximation consistent with IEEE-754 behavior.
 *
 * @param x value whose arcsine is requested
 * @return angle y such that {@code sin(y) = x}, within {@code [-PI/2, PI/2]}
 * @since beta-1.0.0
*/
@native("asin")
native inline fun asin(x: double) -> double = asin(x)


/**
 * Returns the inverse cosine (arccos) of a value.
 * The returned angle is in the interval {@code [0, PI]}.
 *
 * Special cases:
 * <ul>
 *   <li>If {@code x} is NaN, the result is NaN.</li>
 *   <li>If {@code abs(x) > 1}, the result is NaN.</li>
 * </ul>
 *
 * The computation delegates to {@code StrictMath.acos} and returns
 * a floating-point approximation consistent with IEEE-754 behavior.
 *
 * @param x value whose arccosine is requested
 * @return angle y such that {@code cos(y) = x}, within {@code [0, PI]}
 * @since beta-1.0.0
 */
@native("acos")
native inline fun acos(x: double) -> double = acos(x)


/**
 * Returns the inverse tangent (arctan) of a value.
 * The returned angle is in the interval {@code [-PI/2, PI/2]}.
 *
 * Special cases:
 * <ul>
 *   <li>If {@code x} is NaN, the result is NaN.</li>
 *   <li>If {@code x} is +0.0 or -0.0, the result keeps the same signed zero.</li>
 *   <li>If {@code x} is +Infinity, the result approaches {@code +PI/2}.</li>
 *   <li>If {@code x} is -Infinity, the result approaches {@code -PI/2}.</li>
 * </ul>
 *
 * The computation delegates to {@code StrictMath.atan}.
 *
 * @param x value whose arctangent is requested
 * @return angle y such that {@code tan(y) = x}, within {@code [-PI/2, PI/2]}
 * @since beta-1.0.0
 */
@native("atan")
native inline fun atan(x: double) -> double


/**
 * Returns the inverse secant (arcsec) of a value.
 * This implementation is defined as {@code acos(1 / x)}.
 *
 * Domain note:
 * <ul>
 *   <li>Real-valued results are expected for {@code abs(x) >= 1}.</li>
 *   <li>For {@code abs(x) < 1}, reciprocal mapping leads to NaN via {@code acos} domain rules.</li>
 * </ul>
 *
 * The principal-value range follows {@code acos}, i.e. {@code [0, PI]}.
 *
 * @param x value whose arcsecant is requested
 * @return arcsecant of {@code x}, computed as {@code acos(1 / x)}
 * @since beta-1.0.0
 */
inline fun asec(x: double) -> double = acos(1.0 / x)


/**
 * Returns the inverse cosecant (arccsc) of a value.
 * This implementation is defined as {@code asin(1 / x)}.
 *
 * Domain note:
 * <ul>
 *   <li>Real-valued results are expected for {@code abs(x) >= 1}.</li>
 *   <li>For {@code abs(x) < 1}, reciprocal mapping leads to NaN via {@code asin} domain rules.</li>
 * </ul>
 *
 * The principal-value range follows {@code asin}, i.e. {@code [-PI/2, PI/2]}.
 *
 * @param x value whose arccosecant is requested
 * @return arccosecant of {@code x}, computed as {@code asin(1 / x)}
 * @since beta-1.0.0
 */
inline fun acsc(x: double) -> double = asin(1.0 / x)


/**
 * Returns the inverse cotangent (arccot) of a value.
 * This implementation is defined as {@code atan(1 / x)}.
 *
 * Behavior note:
 * <ul>
 *   <li>{@code x = +0.0} maps to {@code +PI/2} through reciprocal infinity.</li>
 *   <li>{@code x = -0.0} maps to {@code -PI/2} through reciprocal infinity.</li>
 *   <li>Very large {@code abs(x)} maps close to zero.</li>
 * </ul>
 *
 * The principal-value range follows {@code atan}, i.e. {@code [-PI/2, PI/2]}.
 *
 * @param x value whose arccotangent is requested
 * @return arccotangent of {@code x}, computed as {@code atan(1 / x)}
 * @since beta-1.0.0
 */
inline fun acot(x: double) -> double = atan(1.0 / x)


/**
 * Converts an angle measured in degrees to radians.
 *
 * <p>This uses the identity {@code radians = degrees * (PI / 180)}.
 *
 * @param angdeg angle value in degrees
 * @return the equivalent angle in radians
 * @since beta-1.0.0
 */
inline fun toRadians(angdeg: double) -> double = angdeg * DEGREES_TO_RADIANS;


/**
 * Converts an angle measured in radians to degrees.
 *
 * <p>This uses the identity {@code degrees = radians * (180 / PI)}.
 *
 * @param angrad angle value in radians
 * @return the equivalent angle in degrees
 * @since beta-1.0.0
 */
inline fun toDegrees(angrad: double) -> double = angrad * RADIANS_TO_DEGREES;


/**
 * Returns Euler's number {@code e} raised to the power of {@code x}.
 *
 * <p>This delegates to {@code StrictMath.exp}.
 *
 * @param x exponent value
 * @return {@code e^x}
 * @since beta-1.0.0
 */
@native("exp")
native inline fun exp(x: double) -> double


/**
 * Returns the natural logarithm (base {@code e}) of a value.
 *
 * <p>This delegates to {@code StrictMath.log}.
 *
 * @param x input value
 * @return {@code ln(x)}
 * @since beta-1.0.0
 */
@native("log")
native inline fun ln(x: double) -> double


/**
 * Returns the logarithm of a value in the specified base.
 *
 * <p>This is computed as {@code ln(x) / ln(base)} using natural logarithms.
 *
 * @param base the logarithmic base
 * @param x input value
 * @return {@code log_base(x)}
 * @since beta-1.0.0
 */
inline fun log(base: double, x: double) -> double = ln(x) / ln(base)


/**
 * Returns the base-2 logarithm of a value.
 *
 * <p>This is computed as {@code log(x) / log(2)} using natural logarithms.
 *
 * @param x input value
 * @return {@code log2(x)}
 * @since beta-1.0.0
 */
fun log2(x: double) -> double = ln(x) / LN2;


/**
 * Returns the base-10 logarithm of a value.
 *
 * <p>This delegates to {@code StrictMath.log10}.
 *
 * @param x input value
 * @return {@code log10(x)}
 * @since beta-1.0.0
 */
@native("log10")
native inline fun log10(x: double) -> double


/**
 * Returns the correctly rounded positive square root of a value.
 *
 * <p>This delegates to {@code StrictMath.sqrt}.
 *
 * @param x input value
 * @return {@code sqrt(x)}
 * @since beta-1.0.0
 */
@native("sqrt")
native inline fun sqrt(x: double) -> double


/**
 * Returns the cube root of a value.
 *
 * <p>This delegates to {@code StrictMath.cbrt}.
 *
 * @param x input value
 * @return {@code cbrt(x)}
 * @since beta-1.0.0
 */
@native("cbrt")
native inline fun cbrt(x: double) -> double


/**
 * Returns whether the given 32-bit integer is a prime number.
 *
 * <p>A value is prime if it is greater than 1 and has no positive divisors
 * other than 1 and itself. This method checks divisibility from 2 up to
 * floor(sqrt(x)).
 *
 * @param x the value to test.
 * @return true if x is prime; otherwise false.
 * @since beta-1.0.0
 */
fun isPrime(x: int) -> bool
{
    if x <= 1:
        return false
    elif x == 2:
        return true
    else
    {
        val half: int = sqrt(x as double) as int

        for (var i: int = 2; i <= half; i++)
        {
            if x % i == 0:
                return false
        }

        return true
    }
}


/**
 * Returns whether the given 64-bit integer is a prime number.
 *
 * <p>A value is prime if it is greater than 1 and has no positive divisors
 * other than 1 and itself. This method checks divisibility from 2 up to
 * floor(sqrt(x)).
 *
 * @param x the value to test.
 * @return true if x is prime; otherwise false.
 * @since beta-1.0.0
 */
fun isPrime(x: long) -> bool
{
    if x <= 1L:
        return false
    elif x == 2L:
        return true
    else
    {
        val half: long = sqrt(x as double) as long

        for (var i: long = 2L; i <= half; i++)
        {
            if x % i == 0L:
                return false
        }

        return true
    }
}


/**
 * Returns whether the given 32-bit integer is even.
 *
 * <p>This uses a bit test: the least-significant bit must be 0.
 *
 * @param x the value to test.
 * @return true if x is even; otherwise false.
 * @since beta-1.0.0
 */
inline fun isEven(x: int) -> bool = (x and 1) == 0


/**
 * Returns whether the given 64-bit integer is even.
 *
 * <p>This uses a bit test: the least-significant bit must be 0.
 *
 * @param x the value to test.
 * @return true if x is even; otherwise false.
 * @since beta-1.0.0
 */
inline fun isEven(x: long) -> bool = (x and 1L) == 0L


/**
 * Returns whether the given 32-bit integer is odd.
 *
 * <p>This uses a bit test: the least-significant bit must be 1.
 *
 * @param x the value to test.
 * @return true if x is odd; otherwise false.
 * @since beta-1.0.0
 */
inline fun isOdd(x: int) -> bool = (x and 1) == 1


/**
 * Returns whether the given 64-bit integer is odd.
 *
 * <p>This uses a bit test: the least-significant bit must be 1.
 *
 * @param x the value to test.
 * @return true if x is odd; otherwise false.
 * @since beta-1.0.0
 */
inline fun isOdd(x: long) -> bool = (x and 1L) == 1L
