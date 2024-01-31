#pragma once
#include "YggInterface_export.h"
#include <string>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <chrono>
#ifdef _WIN32
#define localtime_r(_Time, _Tm) localtime_s(_Tm, _Time)
#endif

#define ygglog_param_error ("ERROR", 40, true)
#define ygglog_param_info ("INFO", 20)
#define ygglog_param_debug ("DEBUG", 10)
#define ygglog_param_verbose ("DEBUG", 5)

#ifndef YGG_DEBUG
#define YGG_DEBUG 20
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
    YGG_API YggdrasilLogger(std::string nme, size_t lvl, bool is_err=false);
    /*!
     * @brief Copy constructor
     * @param[in] rhs Logger to copy
     */
    YGG_API YggdrasilLogger(YggdrasilLogger const & rhs);
    /*!
     * @brief Destructor
     */
    YGG_API ~YggdrasilLogger();
    std::string name;  /**< The logger name */
    size_t level;      /**< The minimum logging level */
    bool is_error;     /**< Is the logger used for errors */
    std::stringstream ss;  /**< internal use */
    std::chrono::system_clock::time_point t; /**< Time for message */
    /*!
     * @brief Templated streaming operator
     * @tparam T The type of item to write
     * @param x The item to write
     * @return Logger
     */
    template<typename T>
    YggdrasilLogger& operator << (const T& x) {
      ss << x;
      return *this;
    }
    /*!
     * @brief Streaming operator
     * @param x The item to write
     * @return Logger
     */
    YggdrasilLogger& operator << (std::ostream& (*x)(std::ostream&)) {
      ss << x;
      return *this;
    }
    /*!
     * @brief Evaluate the logger state to determine if the log should
     *   be displayed
     * @return true if the message should be logged, false otherwise
     */
    bool eval();
    static int _ygg_error_flag; //!< 1 if the logger is for errors

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
    LogBase() {}
    /** @brief Destructor */
    virtual ~LogBase() {}
    /**
     * @brief A string describing the class
     * @return Description
     */
    virtual std::string logClass() const { return ""; }
    /**
     * @brief A string describing the instance
     * @return Description
     */
    virtual std::string logInst() const { return ""; }
    /**
     * @brief A string describing the class and instance
     * @return Description
     */
    std::string logStr() const {
      std::string out = logClass();
      std::string out_inst = logInst();
      if (!out_inst.empty())
	out += "(" + out_inst + ")";
      return out;
    }
    /**
       @brief Throw a C++ exception.
       @param[in] msg Exception description
    */
    void throw_error(const std::string msg) const {
      utils::YggLogThrowError(logStr() + "::" + msg);
    }
    /**
     * @brief Get a logger prefixed with class information.
     * @param[in] nme Name of the log level to prefix log messages with.
     * @param[in] lvl Level of logger to create.
     * @param[in] is_err If true, messages sent to the logger are
     *   considered error messages and an error code will be returned
     *   on exit.
     * @return Logger that messages can be added to. They will only be
     *   flushed to the buffer when the logger is destroyed.
     */
    YggdrasilLogger log(std::string nme, size_t lvl,
			bool is_err=false) const {
      YggdrasilLogger out(nme, lvl, is_err);
      if (out.eval())
	out << logStr() << "::";
      return out;
    }
    /**
     * @brief Get an error logger prefixed with class information.
     * @return logger
     */
    YggdrasilLogger log_error() const { return log ygglog_param_error; }
    /**
     * @brief Get an info logger prefixed with class information.
     * @return logger
     */
    YggdrasilLogger log_info() const { return log ygglog_param_info; }
    /**
     * @brief Get an debug logger prefixed with class information.
     * @return logger
     */
    YggdrasilLogger log_debug() const { return log ygglog_param_debug; }
    /**
     * @brief Get an verbose logger prefixed with class information.
     * @return logger
     */
    YggdrasilLogger log_verbose() const { return log ygglog_param_verbose; }
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
