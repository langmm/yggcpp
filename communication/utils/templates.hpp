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

#define ENABLE_TYPECHECK template<typename T, std::enable_if_t<std::is_arithmetic<T>::value || is_complex<T>::value || is_string<T>::value, bool> = true>

#endif