#pragma once
#ifdef __cplusplus
#include <vector>
#include <algorithm>
#include "templates.hpp"
#include "complex_type.hpp"
#include "constants.hpp"

#ifdef _MSC_VER
#ifndef _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_WARNINGS 1
#endif
#endif

#ifdef _OPENMP
#include <omp.h>
#endif

#include <string>
#include <cstdio>
#include <cstdlib>
#include <cstdarg>
#include <cerrno>
#include <ctime>

#ifndef print_complex
#define print_complex(x) printf("%lf+%lfj\n", (double)creal(x), (double)cimag(x))
#endif

#include <cmath> // Required to prevent error when using mingw on windows

//#ifdef _DEBUG
//#undef _DEBUG
//#include <Python.h>
//#include <numpy/arrayobject.h>
//#include <numpy/ndarrayobject.h>
//#include <numpy/npy_common.h>
//#define _DEBUG
//#else

//#include <Python.h>
//#include <numpy/arrayobject.h>
//#include <numpy/ndarrayobject.h>
//#include <numpy/npy_common.h>

//#endif

#endif
#ifdef _MSC_VER
// Prevent windows.h from including winsock.h
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>
#include <process.h>
#define ygg_getpid _getpid
#define sleep(tsec) Sleep(1000*tsec)
#define usleep(usec) Sleep(usec/1000)
#else

#include <cstdint>
#include <unistd.h>

#define ygg_getpid getpid
#endif

#define STRBUFF 100

/*! @brief Define old style names for compatibility. */
#ifdef PSI_DEBUG
#define YGG_DEBUG PSI_DEBUG
#endif
#include "complex_type.hpp"

#define DELIMITER ','


#define YGGCPP_BEGIN_VAR_ARGS(name, first_arg, nargs, realloc)	\
  rapidjson::VarArgList name(nargs, realloc);			\
  va_start(name.va, first_arg)
#define YGGCPP_END_VAR_ARGS(name)		\
  if (name.get_nargs() != 0)			\
    ygglog_error << name.get_nargs() << " arguments unused" << std::endl
#define YGGC_BEGIN_VAR_ARGS(name, first_arg, nargs, realloc)	\
  rapidjson::VarArgList name(&nargs, realloc, true);		\
  va_start(name.va, first_arg)
#define YGGC_END_VAR_ARGS(name)		\
  if (name.get_nargs() != 0)			\
    ygglog_error << name.get_nargs() << " arguments unused" << std::endl


#include "logging.hpp"

namespace communication {
namespace utils {

template<typename T, std::enable_if_t<!std::is_floating_point<T>::value &&
                                      !std::is_same<T, uint8_t>::value &&
                                      !std::is_same<T, int8_t>::value, bool> = true>
void _join(const std::vector<T>& values, std::ostream& out, const char delim=DELIMITER) {
    for (size_t i = 0; i < values.size()-1; i++)
        out << values[i] << delim;
    out << values[values.size()-1];
}

template<typename T, std::enable_if_t<std::is_same<T, uint8_t>::value ||
                                      std::is_same<T, int8_t>::value, bool> = true>
auto _join(const std::vector<T>& values, std::ostream& out, const char delim=DELIMITER){
    for (size_t i = 0; i < values.size()-1; i++) {
        out << +values[i] << delim;
    }
    out << +values[values.size()-1];
}


template<typename T, std::enable_if_t<std::is_floating_point<T>::value, bool> = true>
auto _join(const std::vector<T>& values, std::ostream& out, const char delim=DELIMITER) {
    out.setf(std::ios::fixed);
    out.precision(std::numeric_limits<T>::digits10);
    for (size_t i = 0; i < values.size()-1; i++)
        out << values[i] << delim;
    out << values[values.size()-1];
}

template<typename T>
inline
auto join(const std::vector<T>& values, std::ostream& out) -> EnableWithEnum<T> {
    _join(values, out);
}

inline
size_t find_spacer(const std::vector<std::string> &values, const size_t start=0) {
    size_t i;
    size_t count;
    for (i = 0; i < REPLACE_SPACE.size(); i++) {
        count = 0;
        for (auto word : values) {
	    if (word.find(REPLACE_SPACE[i], start) == std::string::npos) {
                count ++;
            }
        }
        if (count == values.size())
            return i;
    }
    return i + 1;
}

template<typename T>
inline
auto join(const std::vector<T>& values, std::ostream& out) -> EnableForString<T> {
    size_t spacer = find_spacer(values);
    std::vector<std::string> temp(values);
    for (auto &word : temp) {
        std::replace(word.begin(), word.end(), ' ', REPLACE_SPACE[spacer]);
    }
    out << REPLACE_SPACE[spacer] << " ";
    spacer = find_spacer(temp, spacer + 1);
    out << REPLACE_SPACE[spacer] << " ";
    _join(temp, out, REPLACE_SPACE[spacer]);
}


template<typename T, std::enable_if_t<!std::is_same<T, uint8_t>::value &&
                                      !std::is_same<T, int8_t>::value, bool> = true>
auto parse(std::vector<T>& values, const size_t count, std::istream &input) -> EnableWithEnum<T> {
    values.clear();
    values.reserve(count);
    input >> std::ws;
    char c;
    T v;
    for (size_t i = 0; i < count - 1; i++) {
        if (input.eof())
            throw std::length_error("Invalid length found, expected " + std::to_string(count) + " but found " + std::to_string(i));
        input >> v;
        values.push_back(v);
        input >> c;
    }
    if (input.eof())
        throw std::length_error("Invalid length found, expected " + std::to_string(count) + " but found " + std::to_string(count - 1));
    input >> v;
    values.push_back(v);
    if (input.eof())
        return;
    if (!std::isspace(input.peek())) {
        throw std::length_error("Invalid length found, expected " + std::to_string(count) + " but found more");
    }
}

template<typename T, std::enable_if_t<std::is_same<T, uint8_t>::value ||
                                      std::is_same<T, int8_t>::value, bool> = true>
auto parse(std::vector<T>& values, const size_t count, std::istream &input) {
    values.clear();
    values.reserve(count);
    input >> std::ws;
    char c;
    int v;
    for (size_t i = 0; i < count - 1; i++) {
        if (input.eof())
            throw std::length_error("Invalid length found, expected " + std::to_string(count) + " but found " + std::to_string(i));
        input >> v;
        values.push_back(static_cast<T>(v));
        input >> c;
    }
    if (input.eof())
        throw std::length_error("Invalid length found, expected " + std::to_string(count) + " but found " + std::to_string(count - 1));
    input >> v;
    values.push_back(static_cast<T>(v));
    if (input.eof())
        return;
    if (!std::isspace(input.peek())) {
        throw std::length_error("Invalid length found, expected " + std::to_string(count) + " but found more");
    }
}

template<typename T>
auto parse(std::vector<T>& values, const size_t count, std::istream &input) -> EnableForString<T> {
    values.clear();
    char temp[1000];
    values.reserve(count);
    char spacer1, spacer2;
    input >> spacer1;
    input >> spacer2;
    input >> std::ws;
    for (size_t i = 0; i < count - 1; i++) {
        if (input.eof())
            throw std::length_error("Invalid length found, expected " + std::to_string(count) + " but found " + std::to_string(i));
        input.getline(temp, 1000, spacer2);
        values.push_back(temp);
    }
    if (input.eof())
        throw std::length_error("Invalid length found, expected " + std::to_string(count) + " but found " + std::to_string(count - 1));
    input.getline(temp, 1000);
    values.push_back(temp);
    for (auto &word : values)
        std::replace(word.begin(), word.end(), spacer1, ' ');
}

/*! @brief Define macros to allow counts of variables. */
// https://codecraft.co/2014/11/25/variadic-macros-tricks/
#ifdef _MSC_VER
// https://stackoverflow.com/questions/48710758/how-to-fix-variadic-macro-related-issues-with-macro-overloading-in-msvc-mic
#define MSVC_BUG(MACRO, ARGS) MACRO ARGS  // name to remind that bug fix is due to MSVC :-)
#define _GET_NTH_ARG_2(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, N, ...) N
#define _GET_NTH_ARG(...) MSVC_BUG(_GET_NTH_ARG_2, (__VA_ARGS__))
#define COUNT_VARARGS(...) _GET_NTH_ARG("ignored", ##__VA_ARGS__, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
#define VA_MACRO(MACRO, ...) MSVC_BUG(CONCATE, (MACRO, COUNT_VARARGS(__VA_ARGS__)))(__VA_ARGS__)
#else
#define _GET_NTH_ARG(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, N, ...) N
#define COUNT_VARARGS(...) _GET_NTH_ARG("ignored", ##__VA_ARGS__, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
#endif
#define UNUSED(arg) ((void)&(arg))

/*! @brief Memory to allow thread association to be set via macro. */
static int global_thread_id = -1;
#define ASSOCIATED_WITH_THREAD(COMM, THREAD) global_thread_id = THREAD; COMM; global_thread_id = -1;
#ifdef _OPENMP
#pragma omp threadprivate(global_thread_id)
#endif

/*!
  @brief Get an unsigned long seed from the least significant 32bits of a pointer.
  @param[in] ptr Pointer that should be turned into a seed.
  @return Unsigned long seed.
 */
static inline
unsigned long ptr2seed(void *ptr) {
    auto v = (uint64_t) ptr;
    auto seed = (unsigned long) (v & 0xFFFFFFFFLL);
    return seed;
}

/*!
  @brief Get the ID for the current thread (if inside one).
  @returns int Thread ID.
 */
static inline
int get_thread_id() {
    int out = 0;
    if (global_thread_id >= 0)
        return global_thread_id;
#ifdef _OPENMP
    if (omp_in_parallel())
        out = omp_get_thread_num();
/* #elif defined pthread_self */
/*   // TODO: Finalize/test support for pthread */
/*   out = pthread_self(); */
#endif
    return out;
}





/*!
  @brief Get the length (in bytes) of a character array containing 4 byte
  unicode characters.
  @param[in] strarg char* Pointer to character array.
  @returns size_t Length of strarg in bytes.
 */
static inline
size_t strlen4(char *strarg) {
    if (!strarg)
        return 0; //strarg is NULL pointer
    char *str = strarg;
    for (; *str; str += 4); // empty body
    return (size_t)(str - strarg);
}

/*!
  @brief Called snprintf and realloc buffer if the formatted string is
  larger than the provided buffer.
  @param[in] dst char** Pointer to buffer where formatted message
  should be stored.
  @param[in,out] max_len size_t* Pointer to maximum size of buffer
  that will be modified when the buffer is reallocated.
  @param[in,out] offset size_t* Pointer to offset in buffer where the
  formatted message should be stored. This will be updated to the end
  of the updated message.
  @param[in] format_str const char* Format string that should be used.
  @param[in] ... Additional arguments are passed to snprintf as
  parameters for formatting.
  @returns int -1 if there is an error, otherwise the number of new
  characters written to the buffer.
 */
/*static inline
int snprintf_realloc(char **dst, size_t *max_len, size_t *offset,
                     const char *format_str, ...) {
    va_list arglist;
    va_start(arglist, format_str);
    int fmt_len;
    while (true) {
        va_list arglist_copy;
        va_copy(arglist_copy, arglist);
        fmt_len = vsnprintf(dst[0] + offset[0],
                            max_len[0] - offset[0],
                            format_str, arglist_copy);
        if (fmt_len > (int) (max_len[0] - offset[0])) {
            max_len[0] = max_len[0] + fmt_len + 1;
            char *temp = (char *) realloc(dst[0], max_len[0]);
            if (temp == nullptr) {
                ygglog_error("snprintf_realloc: Error reallocating buffer.");
                fmt_len = -1;
                break;
            }
            dst[0] = temp;
        } else {
            offset[0] = offset[0] + fmt_len;
            break;
        }
    }
    va_end(arglist);
    return fmt_len;
}*/

/*!
  @brief Check if a character array matches a message and is non-zero length.
  @param[in] pattern constant character pointer to string that should be checked.
  @param[in] buf constant character pointer to string that should be checked.
  @returns int 1 if buf matches pattern, 0 otherwise.
 */
static inline
bool not_empty_match(const std::string &pattern, const std::string &buf) {
    return buf == pattern;
}

/*!
  @brief Check if a character array matches the internal EOF message.
  @param[in] buf constant character pointer to string that should be checked.
  @returns int 1 if buf is the EOF message, 0 otherwise.
 */
static inline
int is_eof(const char *buf) {
    return not_empty_match(YGG_MSG_EOF, buf);
}

}
}
