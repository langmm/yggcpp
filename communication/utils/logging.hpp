#pragma once
#include <string>

namespace communication {
namespace utils {
//==============================================================================
/*!
  Logging

  Alliases are set at compile-time based on the value of YGG_CLIENT_DEBUG. If
  set to INFO, only messages logged with info or error alias are printed. If
  set to DEBUG, messages logged with error, info or debug aliases are printed.
  Otherwise, only error messages are printed. If the YGG_CLIENT_DEBUG is
  changed, any code including this header must be recompiled for the change to
  take effect.

*/
//==============================================================================

class Logger {
public:
    Logger() = delete;
    ~Logger() = delete;
/*!
  @brief Print a log message.
  Prints a formatted message, prepending it with the process id and appending
  it with a newline.
  @param[in] prefix a constant character pointer to the prefix that should
  preceed the message and process id.
  @param[in] fmt a constant character pointer to a format string.
  @param[in] ap va_list of arguments to be formatted in the format string.
 */
    static void yggLog(const char *prefix, const char *fmt, va_list ap);

    static void yggLog(const std::string &prefix, const std::string &fmt, va_list ap);

/*!
  @brief Print an info log message.
  Prints a formatted message, prepending it with INFO and the process id. A
  newline character is added to the end of the message.
  @param[in] fmt a constant character pointer to a format string.
  @param[in] ... arguments to be formatted in the format string.
 */
    static void yggInfo(const char *fmt, ...);

/*!
  @brief Print an debug log message.
  Prints a formatted message, prepending it with DEBUG and the process id. A
  newline character is added to the end of the message.
  @param[in] fmt a constant character pointer to a format string.
  @param[in] ... arguments to be formatted in the format string.
 */
    static void yggDebug(const char *fmt, ...);

/*!
  @brief Print an error log message from a variable argument list.
  Prints a formatted message, prepending it with ERROR and the process id. A
  newline character is added to the end of the message.
  @param[in] fmt a constant character pointer to a format string.
  @param[in] ap va_list Variable argument list.
  @param[in] ... arguments to be formatted in the format string.
 */
    static void yggError_va(const char *fmt, va_list ap);

    static void yggError_va(const std::string &fmt, va_list &ap);

/*!
  @brief Print an error log message.
  Prints a formatted message, prepending it with ERROR and the process id. A
  newline character is added to the end of the message.
  @param[in] fmt a constant character pointer to a format string.
  @param[in] ... arguments to be formatted in the format string.
 */
    static void yggError(const char *fmt, ...);

/*!
  @brief Throw an error and long it.
  @param[in] fmt char* Format string.
  @param[in] ... Parameters that should be formated using the format string.
 */
    static void ygglogThrowError(const char *fmt, ...);

    static int _ygg_error_flag;
private:
#define ygglog_throw_error Logger::ygglogThrowError
#define YGG_DEBUG 10
#ifndef DOXYGEN_SHOULD_SKIP_THIS
#ifdef YGG_DEBUG
#if YGG_DEBUG <= 10
#define ygglog_error Logger::yggError
#define ygglog_info Logger::yggInfo
#define ygglog_debug Logger::yggDebug
#elif YGG_DEBUG <= 20
#define ygglog_error Logger::yggError
#define ygglog_info Logger::yggInfo
#define ygglog_debug while (0) Logger::yggDebug
#elif YGG_DEBUG <= 40
#define ygglog_error Logger::yggError
#define ygglog_info while (0) Logger::yggInfo
#define ygglog_debug while (0) Logger::yggDebug
#else
#define ygglog_error while (0) Logger::yggError
#define ygglog_info while (0) Logger::yggInfo
#define ygglog_debug while (0) Logger::yggDebug
#endif
#else
#define ygglog_error yggError
#define ygglog_info while (0) yggInfo
#define ygglog_debug while (0) yggDebug
#endif
#endif // DOXYGEN_SHOULD_SKIP_THIS

};
}
}