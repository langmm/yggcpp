#pragma once
#include <string>
#include <iostream>
#include <sstream>

#ifdef YGG_TEST
#ifndef YGG_DEBUG
#define YGG_DEBUG 10
#endif
#endif

namespace YggInterface {
namespace utils {
  /*!
   * @brief Class for logging
   */
  class YggdrasilLogger {
  public:
    /*!
     * @brief Construnctor
     * @param[in] nme The name of the logger
     * @param[in] lvl The level to log at (i.e. any messages at or above this level will be logged)
     * @param[in] is_err
     */
    YggdrasilLogger(std::string nme, size_t lvl, bool is_err=false);
    ~YggdrasilLogger();
    std::string name;  /**! The logger name */
    size_t level;      /**! The minimum logging level */
    bool is_error;     /**!  */
    std::stringstream ss;  /**! internal use */
    /*!
     * @brief Templated streaming operator
     * @tparam T The type of item to write
     * @param x The item to write
     * @return
     */
    template<typename T>
    YggdrasilLogger& operator << (const T& x) {
      ss << x;
      return *this;
    }
    /*!
     * @brief Streaming operator
     * @param x The item to write
     * @return
     */
    YggdrasilLogger& operator << (std::ostream& (*x)(std::ostream&)) {
      ss << x;
      return *this;
    }
    bool eval();
    static int _ygg_error_flag;

  private:
    /**
     * @brief Get the prefix string for log messages
     * @return
     */
    static std::string _getLogPretex();
  };
  
  /**
   * @brief Reports an error to the log, the raises the error as an exception
   * @param msg
   */
  void ygglog_throw_error(const std::string& msg);
  std::string string_format(const std::string fmt, ...);
  
}
}

#define ygglog_error YggInterface::utils::YggdrasilLogger("ERROR", 40, true)
#define ygglog_info YggInterface::utils::YggdrasilLogger("INFO", 20)
// #define ygglog_debug YggInterface::utils::YggdrasilLogger("DEBUG", 10)
#define ygglog_debug YggInterface::utils::YggdrasilLogger("DEBUG", 40)

#define ygglog_error_c(...) ygglog_error << YggInterface::utils::string_format(__VA_ARGS__) << std::endl
#define ygglog_debug_c(...) ygglog_debug << YggInterface::utils::string_format(__VA_ARGS__) << std::endl
#define ygglog_info_c(...) ygglog_info << YggInterface::utils::string_format(__VA_ARGS__) << std::endl
#define ygglog_throw_error_c(...) YggInterface::utils::ygglog_throw_error(YggInterface::utils::string_format(__VA_ARGS__))
