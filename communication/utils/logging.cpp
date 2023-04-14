#include "logging.hpp"
#include "tools.hpp"
#ifndef YGG_TEST
#include <boost/log/core.hpp>
#include <boost/log/expressions.hpp>
#endif
std::string communication::utils::_getLogPretex() {
    if (!communication::utils::_loginit)
        communication::utils::init_logger();
    std::string out = std::to_string(ygg_getpid()) + ":" + std::to_string(get_thread_id()) + " ";
    char *model_name = getenv("YGG_MODEL_NAME");
    if (model_name != nullptr) {
        out += model_name;
        char *model_copy = getenv("YGG_MODEL_COPY");
        if (model_copy != nullptr) {
            out += "_copy" + std::string(model_copy);
        }
        out += " ";
    }
    return out;
}

void communication::utils::init_logger() {
#ifndef YGG_TEST
    boost::log::core::get()->set_filter(boost::log::trivial::severity >= loggingLevel);
#endif
    communication::utils::_loginit = true;
}

void communication::utils::_set_error() {
  communication::utils::_ygg_error_flag = 1;
}

void communication::utils::ygglog_throw_error(const std::string& msg) {
    ygglog_error << msg << std::endl;
    throw std::exception();
}

std::string communication::utils::string_format(const std::string fmt, ...) {
  int size = ((int)fmt.size()) * 2 + 50;   // Use a rubric appropriate for your code
  std::string str;
  va_list ap;
  while (1) {     // Maximum two passes on a POSIX system...
    str.resize(size);
    va_start(ap, fmt);
    int n = vsnprintf((char *)str.data(), size, fmt.c_str(), ap);
    va_end(ap);
    if (n > -1 && n < size) {  // Everything worked
      str.resize(n);
      return str;
    }
    if (n > -1)  // Needed size returned
      size = n + 1;   // For null char
    else
      size *= 2;      // Guess at a larger size (OS specific)
  }
  return str;
}
