#pragma once
#include <string>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <chrono>
#ifdef _WIN32
#define localtime_r(_Time, _Tm) localtime_s(_Tm, _Time)
#endif

#ifdef YGG_TEST
#ifndef YGG_DEBUG
#define YGG_DEBUG 10
#endif
#endif

namespace communication {
namespace utils {

  class YggdrasilLogger {
  public:
    YggdrasilLogger(std::string nme, size_t lvl, bool is_err=false);
    YggdrasilLogger(YggdrasilLogger const & rhs);
    ~YggdrasilLogger();
    std::string name;
    size_t level;
    bool is_error;
    std::stringstream ss;
    std::chrono::system_clock::time_point t;
    template<typename T>
    YggdrasilLogger& operator << (const T& x) {
      ss << x;
      return *this;
    }
    YggdrasilLogger& operator << (std::ostream& (*x)(std::ostream&)) {
      ss << x;
      return *this;
    }
    bool eval();
    static int _ygg_error_flag;

  private:
    /**
     * Get the prefix string for log messages
     * @return
     */
    std::string _getLogPretex();
  };
  
  /**
   * Reports an error to the log, the raises the error as an exception
   * @param msg
   */
  void ygglog_throw_error(const std::string& msg);
  std::string string_format(const std::string fmt, ...);
  std::string string_format_va(const std::string fmt, va_list op);
  
}
}

#define ygglog_param_error ("ERROR", 40, true)
#define ygglog_param_info ("INFO", 20)
// #define ygglog_param_debug ("DEBUG", 10)
#define ygglog_param_debug ("DEBUG", 40)

#define ygglog_error communication::utils::YggdrasilLogger ygglog_param_error
#define ygglog_info communication::utils::YggdrasilLogger ygglog_param_info
#define ygglog_debug communication::utils::YggdrasilLogger ygglog_param_debug

#define ygglog_throw_error_c(...) communication::utils::ygglog_throw_error(communication::utils::string_format(__VA_ARGS__))
