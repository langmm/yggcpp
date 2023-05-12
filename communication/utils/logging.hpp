#pragma once
#include <string>

#ifdef YGG_TEST
#include <iostream>
#else
#include <boost/log/trivial.hpp>
#endif

namespace communication {
namespace utils {

/**
 * Get the prefix string for log messages
 * @return
 */
std::string _getLogPretex();

/**
 * Reports an error to the log, the raises the error as an exception
 * @param msg
 */
void ygglog_throw_error(const std::string& msg);

/**
 * Initialize the logger
 */
void init_logger();
void _set_error();
extern int _ygg_error_flag;
extern bool _loginit;

#ifdef YGG_TEST
#define ygglog_error std::cout << "ERROR: " << communication::utils::_getLogPretex()
#define ygglog_info std::cout << "INFO: " << communication::utils::_getLogPretex()
#define ygglog_debug std::cout << "DEBUG: " << communication::utils::_getLogPretex()
#define loggingLevel boost::log::trivial::debug

#else

#define ygglog_error BOOST_LOG_TRIVIAL(error) << communication::utils::_getLogPretex()
#define ygglog_info BOOST_LOG_TRIVIAL(info) << communication::utils::_getLogPretex()
#define ygglog_debug BOOST_LOG_TRIVIAL(debug) << communication::utils::_getLogPretex()

#ifndef YGG_DEBUG
#define YGG_DEBUG 10
#endif
#ifndef DOXYGEN_SHOULD_SKIP_THIS
#ifdef YGG_DEBUG
#if YGG_DEBUG <= 10
#define loggingLevel boost::log::trivial::debug
#elif YGG_DEBUG <= 20
#define loggingLevel boost::log::trivial::info
#elif YGG_DEBUG <= 40
#define loggingLevel boost::log::trivial::error
#else
#define loggingLevel boost::log::trivial::fatal
#endif
#else
#define loggingLevel boost::log::trivial::error
#endif
#endif // DOXYGEN_SHOULD_SKIP_THIS

#endif // YGG_TEST

std::string string_format(const std::string fmt, ...);
  
#define ygglog_error_c(...) ygglog_error << string_format(__VA_ARGS__) << std::endl
#define ygglog_debug_c(...) ygglog_debug << string_format(__VA_ARGS__) << std::endl
#define ygglog_info_c(...) ygglog_info << string_format(__VA_ARGS__) << std::endl
#define ygglog_throw_error_c(...) ygglog_throw_error(string_format(__VA_ARGS__))
  
}
}

