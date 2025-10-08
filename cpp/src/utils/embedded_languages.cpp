#include "utils/tools.hpp"
#include "utils/embedded_languages.hpp"


using namespace YggInterface::utils;


EmbeddedLanguageBase::EmbeddedLanguageBase(const LANGUAGE& lang,
					   const std::string& ext0) :
  LogBase(), language(lang), ext(ext0), thread_id(get_thread_id()),
  initialized(false) {
}
std::string EmbeddedLanguageBase::logInst() const {
  return LANGUAGE_map().find(language)->second;
}
bool EmbeddedLanguageBase::is_enabled() const { return false; }
std::string EmbeddedLanguageBase::env_dont_use() const {
  std::string language_str = str_toupper(logInst());
  std::string out;
  if (!language_str.empty())
    out = "YGG_DISABLE_EMBEDDED_" + language_str;
  return out;
}
bool EmbeddedLanguageBase::onParentThread() const {
  return (thread_id == get_thread_id());
}
int EmbeddedLanguageBase::functionFlags() const {
  int out = FUNCTION_EMBEDDED;
  if (!onParentThread())
    out |= FUNCTION_ON_ASYNC;
  return out;
}
int64_t EmbeddedLanguageBase::commFlags() const { return 0; }
bool EmbeddedLanguageBase::disable() const {
  bool out = is_enabled();
  std::string envvar = env_dont_use();
  if (!envvar.empty())
    setenv(envvar.c_str(), "1", 1);
  return out;
}
bool EmbeddedLanguageBase::enable() const {
  bool out = is_enabled();
  std::string envvar = env_dont_use();
  if (!envvar.empty())
    setenv(envvar.c_str(), "0", 1);
  return out;
}
int EmbeddedLanguageBase::init_count(int action) {
  YGG_THREAD_LOCAL std::map<LANGUAGE, int> languages = {};
  int value = 0;
  if (languages.find(language) != languages.end()) {
    value = languages[language];
  }
  value = value + action;
  if (value < 0)
    throw_error("init_count: Cannot have a negative function count");
  log_debug() << "init_count[" << action << "]: " << value << std::endl;
  languages[language] = value;
  return value;
}
bool EmbeddedLanguageBase::initialize(bool on_thread) {
  bool out = true, inc = 1;
  log_debug() << "initialize: on_thread = " << on_thread << std::endl;
  if (init_count() == 0) {
    if (on_thread) {
      out = initialize_thread();
    } else {
      out = initialize_main();
      initialized = true;
    }
  } else {
    // Only allow a single initialization on the main thread
    if (!on_thread)
      inc = 0;
  }
  if (out && inc) {
    init_count(inc);
  }
  log_debug() << "initialize: on_thread = " << on_thread <<
    " [complete]" << std::endl;
  return out;
}
bool EmbeddedLanguageBase::finalize(bool on_thread) {
  bool out = true;
  log_debug() << "finalize: on_thread = " << on_thread << std::endl;
  if (on_thread) {
    if (init_count() == 1) {
      out = finalize_thread();
    }
    if (out)
      init_count(-1);
  } else {
    // Avoid use of init_count in global CommContext destructor
    //   due to unpredictable teardown order of static variables
    if (initialized)
      out = finalize_main();
  }
  log_debug() << "finalize: on_thread = " << on_thread <<
    " [complete]" << std::endl;
  return out;
}
bool EmbeddedLanguageBase::initialize_thread() { return true; }
bool EmbeddedLanguageBase::finalize_thread() { return true; }
bool EmbeddedLanguageBase::check_error(const std::string& context) const {
  std::string error_msg;
  if (get_error(error_msg)) {
    log_error() << context << ": " << logInst() << " error - " <<
      error_msg << std::endl;
    return true;
  }
  return false;
}
