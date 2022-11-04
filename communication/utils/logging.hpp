#pragma once
#include <string>

#ifdef YGG_TEST
#include <iostream>
#else
#include <boost/log/trivial.hpp>
#endif

namespace communication {
namespace utils {
std::string _getLogPretex();
void ygglog_throw_error(const std::string& msg);
void init_logger();
static int _ygg_error_flag = 0;
static bool _loginit = false;

#ifdef YGG_TEST
#define ygglog_error std::cout << "ERROR: " << communication::utils::_getLogPretex()
#define ygglog_info std::cout << "INFO: " << communication::utils::_getLogPretex()
#define ygglog_debug std::cout << "DEBUG: " << communication::utils::_getLogPretex()
#define loggingLevel boost::log::trivial::debug

#else

#define ygglog_error BOOST_LOG_TRIVIAL(error) << communication::utils::_getLogPretex()
#define ygglog_info BOOST_LOG_TRIVIAL(info) << communication::utils::_getLogPretex()
#define ygglog_debug BOOST_LOG_TRIVIAL(debug) << communication::utils::_getLogPretex()

#define YGG_DEBUG 10
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

}
}