#pragma once
#include <string>
#include <iostream>
#include <sstream>

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
    YggdrasilLogger(const YggdrasilLogger& other);
    ~YggdrasilLogger();
    std::string name;
    size_t level;
    bool is_error;
    std::stringstream ss;
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
    std::string _getLogPretex();
  };
  
  void ygglog_throw_error(const std::string& msg);
  std::string string_format(const std::string fmt, ...);
  
}
}

#define ygglog_error YggdrasilLogger("ERROR", 40, true)
#define ygglog_info YggdrasilLogger("INFO", 20)
#define ygglog_debug YggdrasilLogger("DEBUG", 10)

#define ygglog_error_c(...) ygglog_error << string_format(__VA_ARGS__) << std::endl
#define ygglog_debug_c(...) ygglog_debug << string_format(__VA_ARGS__) << std::endl
#define ygglog_info_c(...) ygglog_info << string_format(__VA_ARGS__) << std::endl
#define ygglog_throw_error_c(...) ygglog_throw_error(string_format(__VA_ARGS__))
