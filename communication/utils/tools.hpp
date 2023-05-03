#pragma once
#ifdef __cplusplus
#include <vector>
#include <algorithm>
#include "complex_type.hpp"

#ifdef _MSC_VER
#ifndef _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_WARNINGS 1
#endif
#endif

#ifdef HAVE_OPENMP

#include <omp.h>
#endif

#include <string>
#include <cstdio>
#include <cstdlib>
#include <cstdarg>
#include <cerrno>
#include <ctime>

#define COMMBUFFSIZ 2000

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

// Platform specific
#ifdef _WIN32
#include "regex/regex_win32.h"
#include "getline_win32.h"
#else

//#include "regex_posix.h"

#endif
#ifdef _MSC_VER
#include "windows_stdint.h"  // Use local copy for MSVC support
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

/*! @brief Maximum message size. */
#ifdef IPCDEF
#define YGG_MSG_MAX 2048
#else
#define YGG_MSG_MAX 1048576
#endif
/*! @brief End of file message. */
#define YGG_MSG_EOF "EOF!!!"
/*! @brief End of client message. */
#define YGG_CLIENT_EOF "YGG_END_CLIENT"
/*! @brief Resonable size for buffer. */
#define YGG_MSG_BUF 2048
/*! @brief Sleep time in micro-seconds */
#define YGG_SLEEP_TIME ((int)250000)
/*! @brief Size for buffers to contain names of Python objects. */
#define PYTHON_NAME_SIZE 1000

/*! @brief Define old style names for compatibility. */
#define PSI_MSG_MAX YGG_MSG_MAX
#define PSI_MSG_BUF YGG_MSG_BUF
#define PSI_MSG_EOF YGG_MSG_EOF
#ifdef PSI_DEBUG
#define YGG_DEBUG PSI_DEBUG
#endif
#include "complex_type.hpp"

#define DELIMITER ','

#include "logging.hpp"

namespace communication {
namespace utils {


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
#ifdef HAVE_OPENMP
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
#ifdef HAVE_OPENMP
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
    return (str - strarg);
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

#endif