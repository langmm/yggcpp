#include "logging.hpp"

#include <utility>
#include "tools.hpp"

namespace YggInterface {
namespace utils {
  
int YggdrasilLogger::_ygg_error_flag = 0;
YggdrasilLogger::YggdrasilLogger(std::string nme, size_t lvl, bool is_err) :
  name(std::move(nme)), level(lvl), is_error(is_err), ss() {
}
YggdrasilLogger::~YggdrasilLogger() {
  std::string out = ss.str();
  if (eval() && !out.empty()) {
    std::cout << name << ": " << _getLogPretex() << out;
  }
}
bool YggdrasilLogger::eval() {
  bool out = is_error;
  if (is_error)
    YggdrasilLogger::_ygg_error_flag = 1;
#ifdef YGG_DEBUG
  if (YGG_DEBUG <= level)
    out = true;
#endif
  return out;
}
std::string YggdrasilLogger::_getLogPretex() {
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

void ygglog_throw_error(const std::string& msg) {
  ygglog_error << msg << std::endl;
  throw std::exception();
}

std::string string_format(const std::string fmt, ...) {
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

}}
