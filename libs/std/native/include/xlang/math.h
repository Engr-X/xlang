#ifndef __XLANG_MATH_H__
#define __XLANG_MATH_H__


/**
 * This lib need to implement some math functions,
 * however, these functions are already implemented in the C standard library, 
*/

double xlang_sin(double x);
double xlang_cos(double x);
double xlang_tan(double x);
double xlang_asin(double x);
double xlang_acos(double x);
double xlang_atan(double x);

double xlang_exp(double x);
double xlang_ln(double x);
double xlang_log10(double x);

double xlang_sqrt(double x);
double xlang_cbrt(double x);

double xlang_IEEEremainder(double x, double y);


#endif