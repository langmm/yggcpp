#pragma once
#ifdef __cplusplus
#include <limits>
#include <vector>
#include "rapidjson/internal/meta.h"
#endif // __cplusplus

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
#ifdef YGGDRASIL_LONG_DOUBLE_AVAILABLE
struct complex_long_double{
  long double re;
  long double im;
};
#endif // YGGDRASIL_LONG_DOUBLE_AVAILABLE
typedef struct complex_float complex_float;
typedef struct complex_double complex_double;
#ifdef YGGDRASIL_LONG_DOUBLE_AVAILABLE
typedef struct complex_long_double complex_long_double;
#endif // YGGDRASIL_LONG_DOUBLE_AVAILABLE
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
#ifdef YGGDRASIL_LONG_DOUBLE_AVAILABLE
typedef std::complex<long double> complex_long_double;
#endif // YGGDRASIL_LONG_DOUBLE_AVAILABLE
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
#ifdef YGGDRASIL_LONG_DOUBLE_AVAILABLE
typedef std::complex<long double> complex_long_double;
#endif // YGGDRASIL_LONG_DOUBLE_AVAILABLE
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
#ifdef YGGDRASIL_LONG_DOUBLE_AVAILABLE
/*! @brief Wrapper for a complex number with long double components. */
typedef struct complex_long_double_t {
    long double re; //!< Real component
    long double im; //!< Imaginary component
} complex_long_double_t;
#endif // YGGDRASIL_LONG_DOUBLE_AVAILABLE

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
#ifdef YGGDRASIL_LONG_DOUBLE_AVAILABLE
template<>
struct is_complex<complex_long_double_t> {
    static const bool value = true;
};
#endif // YGGDRASIL_LONG_DOUBLE_AVAILABLE
template<>
struct is_complex<std::complex<float> > {
    static const bool value = true;
};
template<>
struct is_complex<std::complex<double> > {
    static const bool value = true;
};
#ifdef YGGDRASIL_LONG_DOUBLE_AVAILABLE
template<>
struct is_complex<std::complex<long double> > {
    static const bool value = true;
};
#endif // YGGDRASIL_LONG_DOUBLE_AVAILABLE


template<typename Type, typename ReType = void>
using EnableForComplex = typename std::enable_if<is_complex<Type>::value, ReType>::type;

#ifdef YGGDRASIL_LONG_DOUBLE_AVAILABLE
#define YGGDRASIL_IS_COMPLEX_TYPE_(T)                                    \
  rapidjson::internal::OrExpr<rapidjson::internal::IsSame<T,complex_float_t >, \
    rapidjson::internal::OrExpr<rapidjson::internal::IsSame<T,complex_double_t >, \
    rapidjson::internal::OrExpr<rapidjson::internal::IsSame<T,complex_long_double_t >,            \
    rapidjson::internal::OrExpr<rapidjson::internal::IsSame<T,std::complex<double> >, \
    rapidjson::internal::OrExpr<rapidjson::internal::IsSame<T,std::complex<double> >, \
    rapidjson::internal::IsSame<T,std::complex<long double> > > > > > >
#else // YGGDRASIL_LONG_DOUBLE_AVAILABLE
#define YGGDRASIL_IS_COMPLEX_TYPE_(T)                                    \
  rapidjson::internal::OrExpr<rapidjson::internal::IsSame<T,complex_float_t >, \
    rapidjson::internal::OrExpr<rapidjson::internal::IsSame<T,complex_double_t >, \
    rapidjson::internal::OrExpr<rapidjson::internal::IsSame<T,std::complex<float> >,            \
    rapidjson::internal::IsSame<T,std::complex<double> > > > >
#endif // YGGDRASIL_LONG_DOUBLE_AVAILABLE

template<typename T>
RAPIDJSON_ENABLEIF_RETURN((YGGDRASIL_IS_COMPLEX_TYPE_(T)), (std::ostream&))
  operator<<(std::ostream& out, const T& x) {
    out.setf(std::ios::fixed);
    if (sizeof(x.re) == sizeof(float)) {
        out.precision(std::numeric_limits<float>::digits10);
    } else if (sizeof(x.re) == sizeof(double)) {
        out.precision(std::numeric_limits<double>::digits10);
#ifdef YGGDRASIL_LONG_DOUBLE_AVAILABLE
    } else {
        out.precision(std::numeric_limits<long double>::digits10);
#endif // YGGDRASIL_LONG_DOUBLE_AVAILABLE
    }
    return out << '(' << x.re << ',' << x.im << ')';
}

template<typename T>
RAPIDJSON_ENABLEIF_RETURN((YGGDRASIL_IS_COMPLEX_TYPE_(T)), (std::istream&))
  operator>>(std::istream& in, T& x) {
    char c;
    return in >> c >> x.re >> c >> x.im >> c;
}

template<typename T>
RAPIDJSON_ENABLEIF_RETURN((YGGDRASIL_IS_COMPLEX_TYPE_(T)), (T))
  operator+(const T& a, const T& b) {
    T c;
    c.re = a.re + b.re;
    c.im = a.im + b.im;
    return c;
}

template<typename T, typename F>
RAPIDJSON_ENABLEIF_RETURN((YGGDRASIL_IS_COMPLEX_TYPE_(T)), (T))
  operator+(const T&a, const F v) {
    T c;
    c.re = a.re + v;
    c.im = a.im;
    return c;
}

template<typename T>
RAPIDJSON_ENABLEIF_RETURN((YGGDRASIL_IS_COMPLEX_TYPE_(T)), (T))
  operator-(const T& a, const T& b) {
    T c;
    c.re = a.re - b.re;
    c.im = a.im - b.im;
    return c;
}

template<typename T, typename F>
RAPIDJSON_ENABLEIF_RETURN((YGGDRASIL_IS_COMPLEX_TYPE_(T)), (T))
  operator-(const T&a, const F v) {
    T c;
    c.re = a.re - v;
    c.im = a.im;
    return c;
}
template<typename T>
RAPIDJSON_ENABLEIF_RETURN((YGGDRASIL_IS_COMPLEX_TYPE_(T)), (T))
  operator*(const T& a, const T& b) {
    T c;
    c.re = (a.re * b.re - a.im * b.im);
    c.im = (a.re * b.im + a.im * b.re);
    return c;
}

template<typename T, typename F>
RAPIDJSON_ENABLEIF_RETURN((YGGDRASIL_IS_COMPLEX_TYPE_(T)), (T))
  operator*(const T&a, const F v) {
    T c;
    c.re = a.re * v;
    c.im = a.im * v;
    return c;
}
template<typename T>
RAPIDJSON_ENABLEIF_RETURN((YGGDRASIL_IS_COMPLEX_TYPE_(T)), (T))
  operator/(const T& a, const T& b) {
    T c;
#ifdef YGGDRASIL_LONG_DOUBLE_AVAILABLE
    long double xy = b.re * b.re + b.im * b.im;
#else // YGGDRASIL_LONG_DOUBLE_AVAILABLE
    double xy = b.re * b.re + b.im * b.im;
#endif // YGGDRASIL_LONG_DOUBLE_AVAILABLE
    c.re = (a.re * b.re + a.im * b.im)/xy;
    c.im = (a.im * b.re - a.re * b.im)/xy;
    return c;
}

template<typename T, typename F>
RAPIDJSON_ENABLEIF_RETURN((YGGDRASIL_IS_COMPLEX_TYPE_(T)), (T))
  operator/(const T&a, const F v) {
    T c;
    c.re = a.re / v;
    c.im = a.im / v;
    return c;
}


template<typename T>
RAPIDJSON_ENABLEIF_RETURN((rapidjson::internal::IsSame<T, complex_float_t>), (bool))
operator==(const T& a, const T& b) {
  return (std::abs(a.re - b.re) < pow(10, -(std::numeric_limits<float>::digits10 - 1))) &&
    (std::abs(a.im - b.im) < pow(10, -(std::numeric_limits<float>::digits10 - 1)));
}

template<typename T>
RAPIDJSON_ENABLEIF_RETURN((rapidjson::internal::IsSame<T, complex_double_t>), (bool))
operator==(const T& a, const T& b) {
  return (std::abs(a.re - b.re) < pow(10, -(std::numeric_limits<double>::digits10 - 1))) &&
    (std::abs(a.im - b.im) < pow(10, -(std::numeric_limits<double>::digits10 - 1)));
}

#ifdef YGGDRASIL_LONG_DOUBLE_AVAILABLE
template<typename T>
RAPIDJSON_ENABLEIF_RETURN((rapidjson::internal::IsSame<T, complex_long_double_t>), (bool))
operator==(const T& a, const T& b) {
    return (abs(a.re - b.re) < pow(10, -(std::numeric_limits<long double>::digits10 - 1))) &&
           (abs(a.im - b.im) < pow(10, -(std::numeric_limits<long double>::digits10 - 1)));
}
#endif // YGGDRASIL_LONG_DOUBLE_AVAILABLE

template<typename T>
RAPIDJSON_ENABLEIF_RETURN((YGGDRASIL_IS_COMPLEX_TYPE_(T)), (bool))
  operator!=(const T& a, const T& b) {
    return !(a==b);
}

template<typename T>
RAPIDJSON_ENABLEIF_RETURN((YGGDRASIL_IS_COMPLEX_TYPE_(T)), (complex_float_t))
  complex_f(const T& a) {
    complex_float_t f = {static_cast<float>(a.re), static_cast<float>(a.im)};
    return f;
}

template<typename T>
RAPIDJSON_ENABLEIF_RETURN((YGGDRASIL_IS_COMPLEX_TYPE_(T)), (complex_double_t))
  complex_d(const T& a) {
    complex_double_t f = {static_cast<double>(a.re), static_cast<double>(a.im)};
    return f;
}

#ifdef YGGDRASIL_LONG_DOUBLE_AVAILABLE
template<typename T>
RAPIDJSON_ENABLEIF_RETURN((YGGDRASIL_IS_COMPLEX_TYPE_(T)), (complex_long_double_t))
  complex_ld(const T& a) {
    complex_long_double_t f = {static_cast<long double>(a.re), static_cast<long double>(a.im)};
    return f;
}
#endif // YGGDRASIL_LONG_DOUBLE_AVAILABLE

#undef YGGDRASIL_IS_COMPLEX_TYPE_

#endif
