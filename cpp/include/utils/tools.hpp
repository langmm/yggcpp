#pragma once

#define YGG_API
// #if defined(_WINDOWS)
// #if defined(YggInterface_EXPORTS) || defined(YggInterface_py_EXPORTS)
// #   define YGG_API __declspec(dllexport)
// #else
// #   define YGG_API __declspec(dllimport)
// #endif
// #else
// #   define YGG_API
// #endif
#define UNUSED(arg) ((void)&(arg))
#define STRINGIFY(x) #x


#ifdef __cplusplus
#include <vector>
#include <algorithm>
#include "utils/complex_type.hpp"
#include "utils/constants.hpp"

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
#ifdef THREADSINSTALLED
#include <thread>
#include <chrono>
#include <mutex>
#endif // THREADSINSTALLED

#ifndef print_complex
#define print_complex(x) printf("%lf+%lfj\n", (double)creal(x), (double)cimag(x))
#endif

#include <cmath> // Required to prevent error when using mingw on windows

#if defined(RESTINSTALLED) && defined(__MINGW32__)
// Ensure winsock2.h is included before windows.h included by curl
#include <winsock2.h>
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
#ifndef PATH_MAX
#define PATH_MAX MAX_PATH
#endif
#else

#include <cstdint>
#include <unistd.h>

#define ygg_getpid getpid
#endif

#endif

#define STRBUFF 100

/*! @brief Define old style names for compatibility. */
#ifdef PSI_DEBUG
#define YGG_DEBUG PSI_DEBUG
#endif
#include "utils/complex_type.hpp"

#define DELIMITER ','

#define _BEGIN_CPP				\
  try
#define _END_CPP(name, ret)						\
  catch (...) {								\
    YggLogError << #name << ": C++ exception thrown." << std::endl;	\
    return ret;								\
  }
#define _END_CPP_CLEANUP(name, ret, cleanup)	\
  catch (...) {								\
    YggLogError << #name << ": C++ exception thrown." << std::endl;	\
    cleanup;								\
    return ret;								\
  }

#define YGGCPP_BEGIN_VAR_ARGS(name, first_arg, nargs, realloc)	\
  rapidjson::VarArgList name(nargs, realloc);			\
  va_start(name.va, first_arg)
#define YGGCPP_END_VAR_ARGS(name)		\
  if (name.get_nargs() != 0)			\
    YggLogError << name.get_nargs() << " arguments unused" << std::endl
#define YGGC_BEGIN_VAR_ARGS(name, first_arg, nargs, realloc)	\
  rapidjson::VarArgList name(&nargs, realloc, true);		\
  va_start(name.va, first_arg)
#define YGGC_END_VAR_ARGS(name)		\
  if (name.get_nargs() != 0)			\
    YggLogError << name.get_nargs() << " arguments unused" << std::endl


#include "utils/logging.hpp"

#ifdef _WIN32  // _MSC_VER
static inline
int setenv(const char *name, const char *value, int overwrite) {
  if (overwrite || getenv(name) == NULL) {
    size_t len = strlen(name) + strlen(value) + 2;
    char* tmp = (char*)malloc(len * sizeof(char));
    if (tmp == NULL)
      return -1;
    tmp[0] = '\0';
    strcat(tmp, name);
    strcat(tmp, "=");
    strcat(tmp, value);
    int out = _putenv(tmp);
    // free(tmp);
    return out;
  }
  return 0;
}
static inline
int unsetenv(const char *name) {
  return setenv(name, "", true);
}
#endif // _WIN32 _MSC_VER

namespace YggInterface {
namespace utils {

  /**
   * @brief Generate a random alpha numeric string of a given length
   * @param[in] length Size of string to generate
   * @return Generated string
   */
  std::string random_string(std::string::size_type length);
  
// template<typename T, std::enable_if_t<!std::is_floating_point<T>::value &&
//                                       !std::is_same<T, uint8_t>::value &&
//                                       !std::is_same<T, int8_t>::value, bool> = true>
// void _join(const std::vector<T>& values, std::ostream& out, const char delim=DELIMITER) {
//     for (size_t i = 0; i < values.size()-1; i++)
//         out << values[i] << delim;
//     out << values[values.size()-1];
// }

// template<typename T, std::enable_if_t<std::is_same<T, uint8_t>::value ||
//                                       std::is_same<T, int8_t>::value, bool> = true>
// auto _join(const std::vector<T>& values, std::ostream& out, const char delim=DELIMITER){
//     for (size_t i = 0; i < values.size()-1; i++) {
//         out << +values[i] << delim;
//     }
//     out << +values[values.size()-1];
// }


// template<typename T, std::enable_if_t<std::is_floating_point<T>::value, bool> = true>
// auto _join(const std::vector<T>& values, std::ostream& out, const char delim=DELIMITER) {
//     out.setf(std::ios::fixed);
//     out.precision(std::numeric_limits<T>::digits10);
//     for (size_t i = 0; i < values.size()-1; i++)
//         out << values[i] << delim;
//     out << values[values.size()-1];
// }

// template<typename T>
// inline
// auto join(const std::vector<T>& values, std::ostream& out) -> EnableWithEnum<T> {
//     _join(values, out);
// }

// inline
// size_t find_spacer(const std::vector<std::string> &values, const size_t start=0) {
//     size_t i;
//     size_t count;
//     for (i = 0; i < REPLACE_SPACE.size(); i++) {
//         count = 0;
//         for (auto word : values) {
// 	    if (word.find(REPLACE_SPACE[i], start) == std::string::npos) {
//                 count ++;
//             }
//         }
//         if (count == values.size())
//             return i;
//     }
//     return i + 1;
// }

// template<typename T>
// inline
// auto join(const std::vector<T>& values, std::ostream& out) -> EnableForString<T> {
//     size_t spacer = find_spacer(values);
//     std::vector<std::string> temp(values);
//     for (auto &word : temp) {
//         std::replace(word.begin(), word.end(), ' ', REPLACE_SPACE[spacer]);
//     }
//     out << REPLACE_SPACE[spacer] << " ";
//     spacer = find_spacer(temp, spacer + 1);
//     out << REPLACE_SPACE[spacer] << " ";
//     _join(temp, out, REPLACE_SPACE[spacer]);
// }


// template<typename T, std::enable_if_t<!std::is_same<T, uint8_t>::value &&
//                                       !std::is_same<T, int8_t>::value, bool> = true>
// auto parse(std::vector<T>& values, const size_t count, std::istream &input) -> EnableWithEnum<T> {
//     values.clear();
//     values.reserve(count);
//     input >> std::ws;
//     char c;
//     T v;
//     for (size_t i = 0; i < count - 1; i++) {
//         if (input.eof())
//             throw std::length_error("Invalid length found, expected " + std::to_string(count) + " but found " + std::to_string(i));
//         input >> v;
//         values.push_back(v);
//         input >> c;
//     }
//     if (input.eof())
//         throw std::length_error("Invalid length found, expected " + std::to_string(count) + " but found " + std::to_string(count - 1));
//     input >> v;
//     values.push_back(v);
//     if (input.eof())
//         return;
//     if (!std::isspace(input.peek())) {
//         throw std::length_error("Invalid length found, expected " + std::to_string(count) + " but found more");
//     }
// }

// template<typename T, std::enable_if_t<std::is_same<T, uint8_t>::value ||
//                                       std::is_same<T, int8_t>::value, bool> = true>
// auto parse(std::vector<T>& values, const size_t count, std::istream &input) {
//     values.clear();
//     values.reserve(count);
//     input >> std::ws;
//     char c;
//     int v;
//     for (size_t i = 0; i < count - 1; i++) {
//         if (input.eof())
//             throw std::length_error("Invalid length found, expected " + std::to_string(count) + " but found " + std::to_string(i));
//         input >> v;
//         values.push_back(static_cast<T>(v));
//         input >> c;
//     }
//     if (input.eof())
//         throw std::length_error("Invalid length found, expected " + std::to_string(count) + " but found " + std::to_string(count - 1));
//     input >> v;
//     values.push_back(static_cast<T>(v));
//     if (input.eof())
//         return;
//     if (!std::isspace(input.peek())) {
//         throw std::length_error("Invalid length found, expected " + std::to_string(count) + " but found more");
//     }
// }

// template<typename T>
// auto parse(std::vector<T>& values, const size_t count, std::istream &input) -> EnableForString<T> {
//     values.clear();
//     char temp[1000];
//     values.reserve(count);
//     char spacer1, spacer2;
//     input >> spacer1;
//     input >> spacer2;
//     input >> std::ws;
//     for (size_t i = 0; i < count - 1; i++) {
//         if (input.eof())
//             throw std::length_error("Invalid length found, expected " + std::to_string(count) + " but found " + std::to_string(i));
//         input.getline(temp, 1000, spacer2);
//         values.push_back(temp);
//     }
//     if (input.eof())
//         throw std::length_error("Invalid length found, expected " + std::to_string(count) + " but found " + std::to_string(count - 1));
//     input.getline(temp, 1000);
//     values.push_back(temp);
//     for (auto &word : values)
//         std::replace(word.begin(), word.end(), spacer1, ' ');
// }

#ifdef THREADSINSTALLED
#define THREAD_USLEEP(x) std::this_thread::sleep_for(std::chrono::microseconds(x))
#else // THREADSINSTALLED
#define THREAD_USLEEP(x) usleep(x)
#endif // THREADSINSTALLED
  /*
#define TIMEOUT_LOOP(TOUT, TSTEP)					\
  for (int istep = 0; (TOUT < 0 || istep < ((TOUT / TSTEP) + 1)); istep++)
  */
#define TIMEOUT_LOOP(TOUT, TSTEP)		\
  std::chrono::time_point<std::chrono::system_clock> timeout_start = std::chrono::system_clock::now(); \
  for (int istep = 0; istep >= 0 && (TOUT < 0 || (std::chrono::system_clock::now() - timeout_start) <= std::chrono::microseconds(TOUT)); istep++)
#define AFTER_TIMEOUT_LOOP(TSTEP)		\
  THREAD_USLEEP(TSTEP)

/*! @brief Memory to allow thread association to be set via macro. */
static int global_thread_id = -1;
#define ASSOCIATED_WITH_THREAD(COMM, THREAD) global_thread_id = THREAD; COMM; global_thread_id = -1;
#ifdef _OPENMP
#pragma omp threadprivate(global_thread_id)
#endif

/*! @brief Macro to define global vars to be used across thread */
#if defined(_MSC_VER) && defined(_OPENMP)
#define YGG_THREAD_GLOBAL_VAR(T, name, args) \
  extern __declspec(thread) T name args;
#else // _MSC_VER
#ifdef _OPENMP
#define YGG_THREAD_GLOBAL_VAR(T, name, args) \
  extern T name args;			     \
  _Pragma(STRINGIFY(omp threadprivate(name)))
#else // _OPENMP
#define YGG_THREAD_GLOBAL_VAR(T, name, args) \
  extern T name args;
#endif // _OPENMP
#endif // _MSC_VER

/*! @brief Macro to open section that should be synchronized across
  threads, typically where thread global variables are acessed. */
#ifdef THREADSINSTALLED
#ifdef _OPENMP
#define YGG_THREADING_ACTIVE 1
#define YGG_THREAD_SAFE_BEGIN(name)		\
  _Pragma(STRINGIFY(omp critical (name))) {	\
    const std::lock_guard<std::mutex> name ## _lock(global_context->name ## _mutex);
#define YGG_THREAD_SAFE_BEGIN_LOCAL(name)	\
  _Pragma(STRINGIFY(omp critical (name))) {	\
    const std::lock_guard<std::mutex> name ## _lock(name ## _mutex);
#else // _OPENMP
#define YGG_THREAD_SAFE_BEGIN(name)		\
  {						\
    const std::lock_guard<std::mutex> name ## _lock(global_context->name ## _mutex);
#define YGG_THREAD_SAFE_BEGIN_LOCAL(name)	\
  {						\
    const std::lock_guard<std::mutex> name ## _lock(name ## _mutex);
#endif // _OPENMP
#else // THREADSINSTALLED
#ifdef _OPENMP
#define YGG_THREADING_ACTIVE 1
#define YGG_THREAD_SAFE_BEGIN(name)		\
  _Pragma(STRINGIFY(omp critical (name))) {
#define YGG_THREAD_SAFE_BEGIN_LOCAL(name)	\
  YGG_THREAD_SAFE_BEGIN(name)
#else // _OPENMP
#define YGG_THREAD_SAFE_BEGIN(name)		\
  {
#define YGG_THREAD_SAFE_BEGIN_LOCAL(name)	\
  YGG_THREAD_SAFE_BEGIN(name)
#endif // _OPENMP
#endif // THREADSINSTALLED
#define YGG_THREAD_SAFE_END			\
  }

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
std::string get_thread_id() {
  std::string out;
  if (global_thread_id >= 0)
    return std::to_string(global_thread_id);
#ifdef _OPENMP
  if (omp_in_parallel())
    return std::to_string(omp_get_thread_num());
#endif
#ifdef RAPIDJSON_YGGDRASIL_PYTHON
  // TODO: Check for Python thread
#endif // RAPIDJSON_YGGDRASIL_PYTHON
#ifdef THREADSINSTALLED
  std::stringstream ss;
  ss << std::this_thread::get_id();
  out = ss.str();
#endif // THREADSINSTALLED
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

//
//  @brief Called snprintf and realloc buffer if the formatted string is
//  larger than the provided buffer.
//  @param[in] dst char** Pointer to buffer where formatted message
//  should be stored.
//  @param[in,out] max_len size_t* Pointer to maximum size of buffer
//  that will be modified when the buffer is reallocated.
//  @param[in,out] offset size_t* Pointer to offset in buffer where the
//  formatted message should be stored. This will be updated to the end
//  of the updated message.
//  @param[in] format_str const char* Format string that should be used.
//  @param[in] ... Additional arguments are passed to snprintf as
//  parameters for formatting.
//  @returns int -1 if there is an error, otherwise the number of new
//  characters written to the buffer.
// */
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
                YggLogError("snprintf_realloc: Error reallocating buffer.");
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
