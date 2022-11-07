#pragma once

#ifdef __cplusplus
#include <string>
#include "enums.hpp"
#include "complex_type.hpp"

template<typename T>
struct is_string {
    static const bool value = false;
};

template<>
struct is_string<std::string> {
    static const bool value = true;
};

template<typename Type, typename ReType = void>
using EnableForString = typename std::enable_if<is_string<Type>::value, ReType>::type;

template<typename Type, typename ReType = void>
using EnableForNumeric = typename std::enable_if<std::is_floating_point<Type>::value ||
                                                 std::is_same<Type, int8_t>::value ||
                                                 std::is_same<Type, int16_t>::value ||
                                                 std::is_same<Type, int32_t>::value ||
                                                 std::is_same<Type, int64_t>::value ||
                                                 std::is_same<Type, bool>::value ||
                                                 std::is_same<Type, uint8_t>::value ||
                                                 std::is_same<Type, uint16_t>::value ||
                                                 std::is_same<Type, uint32_t>::value ||
                                                 std::is_same<Type, uint64_t>::value, ReType>::type;

template<typename Type, typename ReType = void>
using EnableForNotString = typename std::enable_if<std::is_floating_point<Type>::value ||
                                                   std::is_same<Type, int8_t>::value ||
                                                   std::is_same<Type, int16_t>::value ||
                                                   std::is_same<Type, int32_t>::value ||
                                                   std::is_same<Type, int64_t>::value ||
                                                   std::is_same<Type, bool>::value ||
                                                   std::is_same<Type, uint8_t>::value ||
                                                   std::is_same<Type, uint16_t>::value ||
                                                   std::is_same<Type, uint32_t>::value ||
                                                   std::is_same<Type, uint64_t>::value ||
                                                   is_complex<Type>::value, ReType>::type;

template<typename Type, typename ReType = void>
using EnableForAll = typename std::enable_if<std::is_floating_point<Type>::value ||
                                             std::is_same<Type, int8_t>::value ||
                                             std::is_same<Type, int16_t>::value ||
                                             std::is_same<Type, int32_t>::value ||
                                             std::is_same<Type, int64_t>::value ||
                                             std::is_same<Type, bool>::value ||
                                             std::is_same<Type, uint8_t>::value ||
                                             std::is_same<Type, uint16_t>::value ||
                                             std::is_same<Type, uint32_t>::value ||
                                             std::is_same<Type, uint64_t>::value ||
                                             is_complex<Type>::value ||
                                             is_string<Type>::value, ReType>::type;

template<typename Type, typename ReType = void>
using EnableWithEnum = typename std::enable_if<std::is_floating_point<Type>::value ||
                                               std::is_same<Type, int8_t>::value ||
                                               std::is_same<Type, int16_t>::value ||
                                               std::is_same<Type, int32_t>::value ||
                                               std::is_same<Type, int64_t>::value ||
                                               std::is_same<Type, bool>::value ||
                                               std::is_same<Type, uint8_t>::value ||
                                               std::is_same<Type, uint16_t>::value ||
                                               std::is_same<Type, uint32_t>::value ||
                                               std::is_same<Type, uint64_t>::value ||
                                               is_complex<Type>::value ||
                                               std::is_same<Type, SUBTYPE>::value ||
                                               std::is_same<Type, VTYPE>::value, ReType>::type;

#define ENABLE_TYPECHECK template<typename T, std::enable_if_t<std::is_arithmetic<T>::value || \
                                                               is_complex<T>::value ||         \
                                                               is_string<T>::value, bool> = true>

template<typename T, std::enable_if_t<!std::is_floating_point<T>::value &&
                                      !is_complex<T>::value, bool> = true>
bool COMPARE(const T &a, const T &b) {return a == b;}

template<typename T, std::enable_if_t<std::is_same<T, float>::value, bool> = true>
bool COMPARE(const T &a, const T &b) {return abs(a - b) < 1e-6;}

template<typename T, std::enable_if_t<std::is_same<T, double>::value, bool> = true>
bool COMPARE(const T &a, const T &b) {return abs(a-b) < 1e-13;}

template<typename T, std::enable_if_t<std::is_same<T, long double>::value, bool> = true>
bool COMPARE(const T &a, const T &b) {return abs(a-b) < 1e-15;}

template<typename T, std::enable_if_t<is_complex<T>::value, bool> = true>
bool COMPARE(const T &a, const T &b) {
    return COMPARE(a.re, b.re) && COMPARE(a.im, b.im);
}

template<typename T, std::enable_if_t<std::is_same<T, int8_t>::value ||
                                      std::is_same<T, uint8_t>::value ||
                                      std::is_same<T, double>::value ||
                                      std::is_same<T, complex_double_t>::value, bool> = true>
uint8_t PRECISION() {return 8;}

template<typename T, std::enable_if_t<is_string<T>::value||
                                      std::is_same<T, bool>::value, bool> = true>
uint8_t PRECISION() {return 0;}

template<typename T, std::enable_if_t<std::is_same<T, float>::value ||
                                      std::is_same<T, complex_float_t>::value, bool> = true>
uint8_t PRECISION() {return 4;}

template<typename T, std::enable_if_t<std::is_same<T, int16_t>::value ||
                                      std::is_same<T, uint16_t>::value, bool> = true>
uint8_t PRECISION() {return 16;}

template<typename T, std::enable_if_t<std::is_same<T, int32_t>::value ||
                                      std::is_same<T, uint32_t>::value, bool> = true>
uint8_t PRECISION() {return 32;}

template<typename T, std::enable_if_t<std::is_same<T, int64_t>::value ||
                                      std::is_same<T, uint64_t>::value, bool> = true>
uint8_t PRECISION() {return 64;}

template<typename T, std::enable_if_t<std::is_same<T, long double>::value ||
                                      std::is_same<T, complex_long_double_t>::value, bool> = true>
uint8_t PRECISION() {return 12;}

template<typename T, std::enable_if_t<std::is_same<T, int8_t>::value ||
                                      std::is_same<T, int16_t>::value ||
                                      std::is_same<T, int32_t>::value ||
                                      std::is_same<T, int64_t>::value, bool> = true>
SUBTYPE GET_ST() {return T_INT;}

template<typename T, std::enable_if_t<std::is_same<T, float>::value ||
                                      std::is_same<T, double>::value ||
                                      std::is_same<T, long double>::value, bool> = true>
SUBTYPE GET_ST() {return T_FLOAT;}

template<typename T, std::enable_if_t<std::is_same<T, uint8_t>::value ||
                                      std::is_same<T, uint16_t>::value ||
                                      std::is_same<T, uint32_t>::value ||
                                      std::is_same<T, uint64_t>::value, bool> = true>
SUBTYPE GET_ST() {return T_UINT;}

template<typename T, std::enable_if_t<is_complex<T>::value, bool> = true>
SUBTYPE GET_ST() {return T_COMPLEX;}

template<typename T, std::enable_if_t<std::is_same<T, bool>::value, bool> = true>
SUBTYPE GET_ST() {return T_BOOLEAN;}

template<typename T, std::enable_if_t<is_string<T>::value, bool> = true>
SUBTYPE GET_ST() {return T_STRING;}


#endif