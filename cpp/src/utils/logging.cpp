#include "utils/tools.hpp"
#include "utils/logging.hpp"
#include "utils/enums_utils.hpp"

#include <utility>

namespace YggInterface {

  YggdrasilRuntimeError::YggdrasilRuntimeError(char const* const message) throw()
    : std::runtime_error(message) {}
  YggdrasilRuntimeError::YggdrasilRuntimeError(const std::string& message) throw()
    : YggdrasilRuntimeError(message.c_str()) {}
  
namespace utils {

/*! @brief Define old style names for compatibility. */
#ifdef PSI_DEBUG
#define YGG_DEBUG PSI_DEBUG
#endif

int YggdrasilLogger::_ygg_error_flag = 0;
#ifdef YGG_DEBUG
int YggdrasilLogger::_ygg_default_level = YGG_DEBUG;
#else // YGG_DEBUG
int YggdrasilLogger::_ygg_default_level = static_cast<int>(LOG_INFO);
#endif // YGG_DEBUG
YggdrasilLogger::YggdrasilLogger(std::string nme, bool emit, bool is_err) :
  name(nme), ss(nullptr), t(std::chrono::system_clock::now()) {
  if (is_err) {
    emit = true;
    YggdrasilLogger::_ygg_error_flag = 1;
  }
  if (emit)
    ss = new std::ostringstream();
}
YggdrasilLogger::YggdrasilLogger(int lvl, std::string nme) :
  YggdrasilLogger(nme,
                  (lvl >= YggdrasilLogger::_ygg_default_level),
                  (lvl >= static_cast<int>(LOG_ERROR))) {
  if (name.empty())
    name = YggdrasilLogger::level2str(lvl);
}
YggdrasilLogger::YggdrasilLogger(YggdrasilLogger&& rhs) :
  name(rhs.name), ss(rhs.ss), t(rhs.t) {
  rhs.ss = nullptr;
}
YggdrasilLogger::~YggdrasilLogger() {
  if (ss != nullptr) {
    std::string out = ss->str();
    time_t     now = std::chrono::system_clock::to_time_t(t); // time(0);
    struct tm  tstruct;
    char       buf[80];
    localtime_r(&now, &tstruct);
    // tstruct = *localtime(&now);
    strftime(buf, sizeof(buf), "%X", &tstruct);
    const std::chrono::duration<double> tse = t.time_since_epoch();
    std::chrono::seconds::rep milliseconds = std::chrono::duration_cast<std::chrono::milliseconds>(tse).count() % 1000;
    ss->str("");
    (*ss) << buf << "." << std::setfill('0') << std::setw(3) <<
      milliseconds << " " << name << ": " << _getLogPretex() << out;
    // TODO: Revert to using std::cout after debugging done
    std::cerr << ss->str() << std::flush;
    delete ss;
    ss = nullptr;
  }
}

std::string YggdrasilLogger::level2str(const int level) {
  std::string outStr;
  if (!YggInterface::utils::enum_key_search_round(YggInterface::utils::LOGGING_LEVEL_map(),
                                                  level, outStr))
    outStr = "USER";
  return outStr;
}
int YggdrasilLogger::str2level(const std::string& level) {
  if (level.empty())
    return -1;
  if (std::all_of(level.begin(), level.end(), [](unsigned char c) {
    return std::isdigit(c);
  })) {
    return std::stoi(level);
  }
  LOGGING_LEVEL key = LOG_ALL;
  if (YggInterface::utils::enum_value_search(YggInterface::utils::LOGGING_LEVEL_map(),
                                             level, key, true))
    return static_cast<int>(key);
  return -1;
}
void YggdrasilLogger::set_default_level(const int level) {
  _ygg_default_level = level;
}

std::string YggdrasilLogger::_getLogPretex() {
  std::string out = std::to_string(ygg_getpid()) + ":" + get_thread_id() + " ";
  char *model_name = std::getenv("YGG_MODEL_NAME");
  if (model_name != nullptr) {
    out += model_name;
    char *model_copy = std::getenv("YGG_MODEL_COPY");
    if (model_copy != nullptr) {
      out += "_copy" + std::string(model_copy);
    }
    out += " ";
  }
  return out;
}

void YggLogThrowError(const std::string& msg) {
  YggLogError << msg << std::endl;
  throw YggdrasilRuntimeError(msg);
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

LogBase::LogBase() : loglevel(YggdrasilLogger::_ygg_default_level) {
  // TODO: Check environment variable?
}
LogBase::~LogBase() {}
std::string LogBase::logClass() const { return ""; }
std::string LogBase::logInst() const { return ""; }
std::string LogBase::logStr() const {
  std::string out = logClass();
  std::string out_inst = logInst();
  if (!out_inst.empty())
    out += "(" + out_inst + ")";
  return out;
}
void LogBase::throw_error(const std::string msg) const {
  utils::YggLogThrowError(logStr() + "::" + msg);
}
YggdrasilLogger LogBase::log(int lvl, std::string nme) const {
  if (nme.empty())
    nme = YggdrasilLogger::level2str(lvl);
  YggdrasilLogger out(nme, (lvl >= loglevel), (lvl >= LOG_ERROR));
  if (out.ss)
    out << logStr() << "::";
  return out;
}
YggdrasilLogger LogBase::log_error() const {
  return log ygglog_param_error;
}
YggdrasilLogger LogBase::log_info() const {
  return log ygglog_param_info;
}
YggdrasilLogger LogBase::log_debug() const {
  return log ygglog_param_debug;
}
YggdrasilLogger LogBase::log_verbose() const {
  return log ygglog_param_verbose;
}

}}
