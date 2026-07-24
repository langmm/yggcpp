#pragma once
#include "YggInterface_export.h"
#include <string>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <chrono>
#include <stdexcept>
#ifdef _WIN32
#define localtime_r(_Time, _Tm) localtime_s(_Tm, _Time)
#endif
#include "utils/enums.hpp"

#define ygglog_param_error (LOG_ERROR, "ERROR")
#define ygglog_param_info (LOG_INFO, "INFO")
#define ygglog_param_debug (LOG_DEBUG, "DEBUG")
#define ygglog_param_verbose (LOG_VERBOSE, "VERBOSE")

namespace YggInterface {
  
  /**
   * @brief Class for yggdrasil specific runtime errors.
   * @param[in] message Error message.
   */
  class YggdrasilRuntimeError : public std::runtime_error {
  public:
    /**
     * @brief Constructor from a C-style string
     * @param[in] message Error message.
     */
    YggdrasilRuntimeError(char const* const message) throw();
    /**
     * @brief Constructor from a C++ string.
     * @param[in] message Error message.
     */
    YggdrasilRuntimeError(const std::string& message) throw();
  };
  
namespace utils {
  /*!
   * @brief Class for logging
   */
  class YggdrasilLogger {
  private:
    YggdrasilLogger(const YggdrasilLogger&) = delete;
    YggdrasilLogger& operator=(const YggdrasilLogger&) = delete;
  public:
    /*!
     * @brief Construnctor
     * @param[in] name The name of the logger
     * @param[in] emit If true, emit messages from the logger.
     * @param[in] is_err True if the logger is for an error message.
     */
    YGG_API explicit YggdrasilLogger(std::string name, bool emit, bool is_err=false);
    /*!
     * @brief Construnctor
     * @param[in] level The level to log at (i.e. any messages at or above this level will be logged)
     * @param[in] name The name of the logger. If not provided, the name
     *   will be set based on the level.
     */
    YGG_API explicit YggdrasilLogger(int level, std::string name = "");
    /*!
     * @brief Move constructor
     * @param[in] rhs Logger to move
     */
    YGG_API YggdrasilLogger(YggdrasilLogger&& rhs);
    /*!
     * @brief Destructor
     */
    YGG_API ~YggdrasilLogger();
    std::string name;  /**< The logger name */
    std::ostringstream* ss;  /**< internal use */
    std::chrono::system_clock::time_point t; /**< Time for message */
    /*!
     * @brief Templated streaming operator
     * @tparam T The type of item to write
     * @param x The item to write
     * @return Logger
     */
    template<typename T>
    YggdrasilLogger& operator << (const T& x) {
      if (ss != nullptr)
        (*ss) << x;
      return *this;
    }
    /*!
     * @brief Streaming operator
     * @param x The item to write
     * @return Logger
     */
    YggdrasilLogger& operator << (std::ostream& (*x)(std::ostream&)) {
      if (ss != nullptr)
        (*ss) << x;
      return *this;
    }
    static int _ygg_error_flag; //!< 1 if the logger is for errors
    static int _ygg_default_level; //!< Default logging level
    
    /**
     * @brief Convert a log level into a string by rounding down to the
     *   nearest named level.
     * @param[in] level Log level.
     * @returns Level name.
     */
    YGG_API static std::string level2str(const int level);
    /**
     * @brief Convert a string to a log level, checking for named levels.
     * @param[in] level Level string.
     * @returns Level value.
     */
    YGG_API static int str2level(const std::string& level);
    /**
     * @brief Set the default log level.
     * @param[in] level Log level.
     */
    YGG_API static void set_default_level(const int level);

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
  YGG_API void YggLogThrowError(const std::string& msg);
  /**
   * @brief Format a string based on a C-style format string
   * @param[in] fmt C-Style format string
   */
  YGG_API std::string string_format(const std::string fmt, ...);
  /**
   * @brief Format a string based on a C-style format string
   * @param[in] fmt C-Style format string
   * @param[in] op Variable argument list to use if formatting
   */
  YGG_API std::string string_format_va(const std::string fmt, va_list op);

  /**
   * @brief Base class for loggers
   */
  class LogBase {
  public:
    /** @brief Constructor */
    LogBase();
    /** @brief Destructor */
    virtual ~LogBase();
    /**
     * @brief A string describing the class
     * @return Description
     */
    virtual std::string logClass() const;
    /**
     * @brief A string describing the instance
     * @return Description
     */
    virtual std::string logInst() const;
    /**
     * @brief A string describing the class and instance
     * @return Description
     */
    std::string logStr() const;
    /**
       @brief Throw a C++ exception.
       @param[in] msg Exception description
    */
    void throw_error(const std::string msg) const;
    /**
     * @brief Get a logger prefixed with class information.
     * @param[in] level Level of logger to create.
     * @param[in] name Name of the log level to prefix log messages with.
     *   If not provided, the name will be set based on the level.
     * @return Logger that messages can be added to. They will only be
     *   flushed to the buffer when the logger is destroyed.
     */
    YggdrasilLogger log(int level, std::string name = "") const;
    /**
     * @brief Get an error logger prefixed with class information.
     * @return logger
     */
    YggdrasilLogger log_error() const;
    /**
     * @brief Get an info logger prefixed with class information.
     * @return logger
     */
    YggdrasilLogger log_info() const;
    /**
     * @brief Get an debug logger prefixed with class information.
     * @return logger
     */
    YggdrasilLogger log_debug() const;
    /**
     * @brief Get an verbose logger prefixed with class information.
     * @return logger
     */
    YggdrasilLogger log_verbose() const;
    
    int loglevel;  /**< The level of log messages enabled for this instance */
  };
  
}
}

//! @brief Logger for error messages
#define YggLogError YggInterface::utils::YggdrasilLogger ygglog_param_error
//! @brief Logger for info messages
#define YggLogInfo YggInterface::utils::YggdrasilLogger ygglog_param_info
//! @brief Logger for debug messages
#define YggLogDebug YggInterface::utils::YggdrasilLogger ygglog_param_debug
//! @brief Logger to use in destructors
#define YggLogDestructor YggLogDebug

//! @brief Throw an error message
//! @param[in] ... Arguments to string_format for error message
#define ygglog_throw_error(...) YggInterface::utils::YggLogThrowError(YggInterface::utils::string_format(__VA_ARGS__))
