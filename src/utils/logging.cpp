#include "logging.hpp"

#include <utility>
#include "tools.hpp"

namespace YggInterface {
namespace utils {
  
int YggdrasilLogger::_ygg_error_flag = 0;
YggdrasilLogger::YggdrasilLogger(std::string nme, size_t lvl, bool is_err) :
  name(nme), level(lvl), is_error(is_err), ss(), t(std::chrono::system_clock::now()) {
}
YggdrasilLogger::YggdrasilLogger(YggdrasilLogger const & rhs) :
  name(rhs.name), level(rhs.level), is_error(rhs.is_error), ss(rhs.ss.str()), t(rhs.t) {
  level = 0;
}
YggdrasilLogger::~YggdrasilLogger() {
  std::string out = ss.str();
  if (eval() && !out.empty()) {
    time_t     now = std::chrono::system_clock::to_time_t(t); // time(0);
    struct tm  tstruct;
    char       buf[80];
    localtime_r(&now, &tstruct);
    // tstruct = *localtime(&now);
    strftime(buf, sizeof(buf), "%X", &tstruct);
    const std::chrono::duration<double> tse = t.time_since_epoch();
    std::chrono::seconds::rep milliseconds = std::chrono::duration_cast<std::chrono::milliseconds>(tse).count() % 1000;
    // TODO: Revert to using std::cout after debugging done
    std::cerr << buf << "." << std::setfill('0') << std::setw(3) <<
      milliseconds << " " << name << ": " << _getLogPretex() << out;
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
  std::string out = std::to_string(ygg_getpid()) + ":" + get_thread_id() + " ";
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

void YggLogThrowError(const std::string& msg) {
  YggLogError << msg << std::endl;
  throw std::exception();
}

std::string string_format(const std::string fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  std::string str = string_format_va(fmt, ap);
  va_end(ap);
  return str;
}

std::string string_format_va(const std::string fmt, va_list op) {
  int size = ((int)fmt.size()) * 2 + 50;   // Use a rubric appropriate for your code
  std::string str;
  va_list ap;
  while (1) {     // Maximum two passes on a POSIX system...
    str.resize(size);
    va_copy(ap, op);
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
