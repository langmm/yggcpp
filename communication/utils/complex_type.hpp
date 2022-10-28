#pragma once
#include <limits>

#ifdef USE_OSR_YGG
#ifdef __cplusplus
extern "C" {
#endif

struct complex_float{
  float re;
  float im;
};
struct complex_double{
  double re;
  double im;
};
struct complex_long_double{
  long double re;
  long double im;
};
typedef struct complex_float complex_float;
typedef struct complex_double complex_double;
typedef struct complex_long_double complex_long_double;
#define creal(x) x.re
#define crealf(x) x.re
#define creall(x) x.re
#define cimag(x) x.im
#define cimagf(x) x.im
#define cimagl(x) x.im

#ifdef __cplusplus
}
#endif

#else /*USE_YGG_OSR*/
#ifdef _MSC_VER
#ifdef __cplusplus
#include <complex>
typedef std::complex<float> complex_float;
typedef std::complex<double> complex_double;
typedef std::complex<long double> complex_long_double;
#ifndef creal
#define creal(x) x.real()
#define crealf(x) x.real()
#define creall(x) x.real()
#define cimag(x) x.imag()
#define cimagf(x) x.imag()
#define cimagl(x) x.imag()
#endif /*creal*/
#endif /*__cplusplus*/

#else // Unix

#ifdef __cplusplus
#include <complex>
typedef std::complex<float> complex_float;
typedef std::complex<double> complex_double;
typedef std::complex<long double> complex_long_double;
#ifndef creal
#define creal(x) x.real()
#define crealf(x) x.real()
#define creall(x) x.real()
#define cimag(x) x.imag()
#define cimagf(x) x.imag()
#define cimagl(x) x.imag()
#endif /*creal*/
#endif /*__cplusplus*/
#endif /*_MSC_VER*/
#endif /*USE_OSR_YGG*/
#ifndef print_complex
#define print_complex(x) printf("%lf+%lfj\n", (double)creal(x), (double)cimag(x))
#endif

#ifdef __cplusplus
extern "C" {
#endif
/*! @brief Wrapper for a complex number with float components. */
typedef struct complex_float_t {
    float re; //!< Real component
    float im; //!< Imaginary component
} complex_float_t;
/*! @brief Wrapper for a complex number with double components. */
typedef struct complex_double_t {
    double re; //!< Real component
    double im; //!< Imaginary component
} complex_double_t;
/*! @brief Wrapper for a complex number with long double components. */
typedef struct complex_long_double_t {
    long double re; //!< Real component
    long double im; //!< Imaginary component
} complex_long_double_t;


#ifdef __cplusplus
}

template<typename T>
struct is_complex {
    static const bool value = false;
};
template<>
struct is_complex<complex_float_t> {
    static const bool value = true;
};
template<>
struct is_complex<complex_double_t> {
    static const bool value = true;
};
template<>
struct is_complex<complex_long_double_t> {
    static const bool value = true;
};


template<typename Type, typename ReType = void>
using EnableForComplex = typename std::enable_if<is_complex<Type>::value, ReType>::type;

template<typename T>
auto operator<<(std::ostream& out, const T& x) -> EnableForComplex<T, std::ostream&> {
    if (sizeof(x.re) == sizeof(float)) {
        out.precision(std::numeric_limits<float>::digits10);
    } else if (sizeof(x.re) == sizeof(double)) {
        out.precision(std::numeric_limits<double>::digits10);
    } else {
        out.precision(std::numeric_limits<long double>::digits10);
    }
    return out << '(' << x.re << ',' << x.im << ')';
}

template<typename T>
auto operator>>(std::istream& in, T& x) ->EnableForComplex<T, std::istream&> {
    char c;
    return in >> c >> x.re >> c >> x.im >> c;
}

#endif