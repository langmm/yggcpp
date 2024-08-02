#include "communicators/comms.hpp"
#include "utils/tools.hpp"
#include "utils/rapidjson_wrapper.hpp"
#include "utils/embedded_python.hpp"
#include "utils/embedded_julia.hpp"
#include "utils/embedded_r.hpp"

#ifndef DOXYGEN_SHOULD_SKIP_THIS
int YggInterface::communicator::global_scope_comm = 0;
#ifdef RAPIDJSON_YGGDRASIL_PYTHON
std::shared_ptr<YggInterface::communicator::CommContext> YggInterface::communicator::global_context(NULL);
#else // RAPIDJSON_YGGDRASIL_PYTHON
std::shared_ptr<YggInterface::communicator::CommContext> YggInterface::communicator::global_context(new YggInterface::communicator::CommContext());
#endif // RAPIDJSON_YGGDRASIL_PYTHON
#endif // DOXYGEN_SHOULD_SKIP_THIS

using namespace YggInterface::communicator;

CommContext::CommContext(bool for_testing) :
  LogBase(), registry_(), thread_id(utils::get_thread_id()),
  for_testing_(for_testing), cleanup_mode_(), zmq_ctx(NULL), rng() {
  log_debug() << "CommContext: New context" << std::endl;
#ifdef YGG_ZMQ_PRELOAD
  hzmqDLL = LoadLibrary(TEXT("zmq"));
  if (!hzmqDLL) {
    throw_error("Failed to load zmq library");
  }
#endif // YGG_ZMQ_PRELOAD
  init(for_testing);
}
CommContext::~CommContext() {
  log_debug() << "~CommContext: Begin destructor" << std::endl;
  cleanup(CLEANUP_ATEXIT);
#ifdef YGG_ZMQ_PRELOAD
  if (hzmqDLL)
    FreeLibrary(hzmqDLL);
#endif // YGG_ZMQ_PRELOAD
}
int CommContext::init(bool for_testing) {
  log_debug() << "init: Begin" << std::endl;
  for_testing_ = for_testing;
#define ADD_EMBEDDED(cls)						\
  {									\
    utils::cls* iembed = new utils::cls();				\
    LANGUAGE iembed_language = iembed->language;			\
    std::map<LANGUAGE, utils::EmbeddedLanguageBase*>::iterator it = embed_registry_.find(iembed_language); \
    if (it == embed_registry_.end() || !(it->second)) {			\
      embed_registry_[iembed_language] = iembed;			\
    } else {								\
      delete iembed;							\
      iembed = dynamic_cast<utils::cls*>(it->second);			\
    }									\
    if (iembed->is_enabled()) {						\
      iembed->initialize();						\
    }									\
  }
  YGG_THREAD_SAFE_BEGIN_LOCAL(embed) {
    ADD_EMBEDDED(EmbeddedPython);
    ADD_EMBEDDED(EmbeddedJulia);
  } YGG_THREAD_SAFE_END;
#undef ADD_EMBEDDED
#ifdef ZMQINSTALLED
  log_debug() << "init: begin zmq initialization" << std::endl;
  YGG_THREAD_SAFE_BEGIN_LOCAL(zmq) {
    if (zmq_ctx == NULL)
      zmq_ctx = zmq_ctx_new();
  } YGG_THREAD_SAFE_END;
  log_debug() << "init: finished zmq initialization" << std::endl;
#endif // ZMQINSTALLED
#ifdef RESTINSTALLED
  log_debug() << "init: begin curl initialization" << std::endl;
  curl_global_init(CURL_GLOBAL_ALL);
  log_debug() << "init: finished curl initialization" << std::endl;
#endif // RESTINSTALLED
  log_debug() << "init: End" << std::endl;
  rng.seed((uint64_t)(this));
  std::srand(ptr2seed(this));
  return 0;
}
void CommContext::cleanup(CLEANUP_MODE mode) {
  log_debug() << "cleanup: mode = " << mode << std::endl;
  YGG_THREAD_SAFE_BEGIN_LOCAL(clean) {
    CLEANUP_MODE prev_mode = cleanup_mode_;
    cleanup_mode_ = mode;
    log_debug() << "cleanup: Begin cleanup of " << registry_.size() << " communicators (mode = " << mode << ")" << std::endl;
    for (size_t i = 0; i < registry_.size(); i++) {
      if (registry_[i]) {
	if (registry_[i]->getFlags() & COMM_FLAG_DELETE) {
	  delete registry_[i];
	}
      }
    }
    YGG_THREAD_SAFE_BEGIN_LOCAL(functions) {
      for (std::map<std::string, FunctionWrapper*>::iterator it = func_registry_.begin();
	   it != func_registry_.end(); it++) {
	if (it->second) {
	  delete it->second;
	  it->second = nullptr;
	}
      }
      func_registry_.clear();
    } YGG_THREAD_SAFE_END;
    YGG_THREAD_SAFE_BEGIN_LOCAL(comms) {
      registry_.clear();
      if (mode != CLEANUP_COMMS) {
#ifdef ZMQINSTALLED
	YGG_THREAD_SAFE_BEGIN_LOCAL(zmq) {
	  if (zmq_ctx != NULL) {
#ifdef YGG_ZMQ_PRESERVE_CONTEXT
	    if (mode != CLEANUP_ATEXIT) {
#endif // YGG_ZMQ_PRESERVE_CONTEXT
#ifdef YGG_ZMQ_CATCH_ERROR_POST_UNLOAD
	    __try
	      {
#endif // YGG_ZMQ_CATCH_ERROR_POST_UNLOAD
	    zmq_ctx_shutdown(zmq_ctx);
	    if (zmq_ctx_term(zmq_ctx) != 0) {
	      log_error() << "cleanup: Error terminating ZeroMQ context via zmq_ctx_term" << std::endl;
	    }
#ifdef YGG_ZMQ_CATCH_ERROR_POST_UNLOAD
	      }
	    __except(_HandleWSAStartupError(GetExceptionCode(),
					    GetExceptionInformation()))
	      {
		log_debug() << "cleanup: Caught error due to DLL unloading" << std::endl;
	      }
#endif // YGG_ZMQ_CATCH_ERROR_POST_UNLOAD
#ifdef YGG_ZMQ_PRESERVE_CONTEXT
	    }
#endif // YGG_ZMQ_PRESERVE_CONTEXT
	    zmq_ctx = NULL;
	  }
	} YGG_THREAD_SAFE_END;
#endif // ZMQINSTALLED
	YGG_THREAD_SAFE_BEGIN_LOCAL(embed) {
	  for (std::map<LANGUAGE, utils::EmbeddedLanguageBase*>::iterator it = embed_registry_.begin();
	       it != embed_registry_.end(); it++) {
	    if (it->second) {
	      if (it->second->is_enabled())
		it->second->finalize();
	      delete it->second;
	      it->second = nullptr;
	    }
	  }
	} YGG_THREAD_SAFE_END;
      }
    } YGG_THREAD_SAFE_END;
#ifdef RESTINSTALLED
    curl_global_cleanup();
#endif // RESTINSTALLED
    log_debug() << "cleanup: Cleanup complete" << std::endl;
    cleanup_mode_ = prev_mode;
  } YGG_THREAD_SAFE_END;
#ifndef RAPIDJSON_YGGDRASIL_PYTHON
  if ((!for_testing_) && utils::YggdrasilLogger::_ygg_error_flag) {
    log_debug() << "cleanup: Error code set" << std::endl;
    _exit(utils::YggdrasilLogger::_ygg_error_flag);
  }
#endif // RAPIDJSON_YGGDRASIL_PYTHON
}

std::map<LANGUAGE, bool> CommContext::disable_embedded_languages(const std::map<LANGUAGE, bool>& languages) const {
  std::map<LANGUAGE, bool> out;
  for (std::map<LANGUAGE, utils::EmbeddedLanguageBase*>::const_iterator it = embed_registry_.begin();
       it != embed_registry_.end(); it++) {
    out[it->first] = it->second->is_enabled();
    std::map<LANGUAGE, bool>::const_iterator it_bool = languages.find(it->first);
    if ((it_bool == languages.end()) || it_bool->second)
      it->second->disable();
  }
  return out;
}

std::map<LANGUAGE, bool> CommContext::enable_embedded_languages(const std::map<LANGUAGE, bool>& languages) const {
  std::map<LANGUAGE, bool> out;
  for (std::map<LANGUAGE, utils::EmbeddedLanguageBase*>::const_iterator it = embed_registry_.begin();
       it != embed_registry_.end(); it++) {
    out[it->first] = it->second->is_enabled();
    std::map<LANGUAGE, bool>::const_iterator it_bool = languages.find(it->first);
    if ((it_bool == languages.end()) || it_bool->second)
      it->second->enable();
  }
  return out;
}

void CommContext::register_comm(Comm_t* x) {
  if (x->getFlags() & COMM_FLAG_ASYNC_WRAPPED)
    return;
  log_debug() << "register_comm: Registering " << x->name << ", " << x->address.address() << std::endl;
  YGG_THREAD_SAFE_BEGIN_LOCAL(comms) {
    x->index_in_register = registry_.size();
    registry_.push_back(x);
  } YGG_THREAD_SAFE_END;
  log_debug() << "register_comm: Registered " << x->name << ", " <<
    x->address.address() << " (idx = " << x->index_in_register <<
    ", global = " << (x->flags & COMM_FLAG_GLOBAL) << ")" << std::endl;
}

Comm_t* CommContext::find_registered_comm(const std::string& name,
					  const DIRECTION dir,
					  const COMM_TYPE type) {
  Comm_t* out = NULL;
  assert(!name.empty());
  log_debug() << "find_registered_comm: global_scope_comm = " <<
    global_scope_comm << std::endl;
  YGG_THREAD_SAFE_BEGIN_LOCAL(comms) {
    if (global_scope_comm) {
      log_debug() << "find_registered_comm: Checking for match to (" <<
	name << ", " << dir << ", " << type << ") amongst " <<
	registry_.size() << " registered comms" << std::endl;
      for (std::vector<Comm_t*>::iterator it = registry_.begin();
	   it != registry_.end(); it++) {
	if (*it && (*it)->global() &&
	    ((*it)->name == name) &&
	    ((*it)->direction == dir) &&
	    ((*it)->type == type)) {
	  log_debug() << "find_registered_comm: Found match for (" <<
	    name << ", " << dir << ", " << type << ")" << std::endl;
	  out = *it;
	  break;
	}
	if (*it) {
	  log_debug() << "find_registered_comm: No match for (" <<
	    name << ", " << dir << ", " << type << ") against (" <<
	    (*it)->name << ", " << (*it)->direction << ", " <<
	    (*it)->type << ") global = " << (*it)->global() << std::endl;
	} else {
	  log_debug() << "find_registered_comm: No match for (" <<
	    name << ", " << dir << ", " << type << ") against (null)" <<
	    std::endl;
	}
      }
    }
  } YGG_THREAD_SAFE_END;
  return out;
}

void CommContext::register_function(FunctionWrapper* x) {
  log_debug() << "register_function: Registering " << x->address << std::endl;
  if (x && (x->flags & FUNCTION_WEAK_REF)) {
    log_debug() << "register_function: Skipping registration of weakref function" << std::endl;
    return;
  }
  YGG_THREAD_SAFE_BEGIN_LOCAL(functions) {
    if (func_registry_.find(x->address) == func_registry_.end()) {
      func_registry_[x->address] = x;
    }
  } YGG_THREAD_SAFE_END;
  log_debug() << "register_function: Registered " << x->address <<
    " (idx = " << func_registry_.size() << ")" << std::endl;
}

FunctionWrapper* CommContext::find_registered_function(const std::string& name) {
  FunctionWrapper* out = NULL;
  log_debug() << "find_registered_function: " << name << std::endl;
  YGG_THREAD_SAFE_BEGIN_LOCAL(functions) {
    log_debug() << "find_registered_function: Checking for match to " <<
      name << " amongst " << func_registry_.size() <<
      " registered functions" << std::endl;
    std::map<std::string, FunctionWrapper*>::iterator it = func_registry_.end();
    if (!name.empty()) {
      it = func_registry_.find(name);
    } else if (!func_registry_.empty()) {
      log_debug() << "find_registered_function: Returning most " <<
	"recently registered function" << std::endl;
      it = func_registry_.end();
      it--;
    }
    if (it != func_registry_.end()) {
      out = it->second;
      log_debug() << "find_registered_function: Found match for " <<
	name << std::endl;
    } else {
      log_debug() << "find_registered_function: No match for " <<
	name << std::endl;
    }
  } YGG_THREAD_SAFE_END;
  return out;
}

FunctionWrapper* CommContext::create_registered_function(const std::string& name,
							 const LANGUAGE calling_lang,
							 int flags) {
  FunctionWrapper* handle = find_registered_function(name);
  if (handle)
    return handle;
  if (name.empty())
    throw_error("create_registered_function: Failed to get function wrapper given empty address");
  log_debug() << "create_registered_function: Creating function " <<
    "wrapper for " << name << std::endl;
  handle = new FunctionWrapper(name, false, calling_lang, flags);
  register_function(handle);
  return handle;
}

uint64_t CommContext::uuid() {
  uint64_t out = 0;
  YGG_THREAD_SAFE_BEGIN_LOCAL(uuid) {
    out = rng();
  } YGG_THREAD_SAFE_END;
  return out;
}

// https://stackoverflow.com/questions/440133/how-do-i-create-a-random-alpha-numeric-string-in-c
std::string CommContext::uuid_str(const size_t& length) {
  static size_t N = 62;
  static const char* chrs = "0123456789"
    "abcdefghijklmnopqrstuvwxyz"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  thread_local static std::uniform_int_distribution<std::string::size_type> pick(0, N - 2);
  std::string s;
  s.reserve(length);
  size_t len = length;
  YGG_THREAD_SAFE_BEGIN_LOCAL(uuid) {
    while(len--)
      s += chrs[pick(rng)];
  } YGG_THREAD_SAFE_END;
  return s;
  
}

#ifdef YGG_ZMQ_CATCH_ERROR_POST_UNLOAD
DWORD CommContext::_HandleWSAStartupError(unsigned int code,
					  struct _EXCEPTION_POINTERS *ep) {
  if (code != 0x40000015)
    return EXCEPTION_CONTINUE_SEARCH;
  const char* msg = (const char*)(ep->ExceptionRecord->ExceptionInformation[0]);
  const char* msg_handle = "Assertion failed: Successful WSASTARTUP not yet performed";
  if (strncmp(msg, msg_handle, strlen(msg_handle)) != 0) {
    std::cerr << "Error message did not match: \"" << msg << "\"" << std::endl;
    return EXCEPTION_CONTINUE_SEARCH;
  }
  return EXCEPTION_EXECUTE_HANDLER;
}
#endif

// void _cleanup_wrapper() {
//   global_context->cleanup(CLEANUP_ATEXIT);
// }

int YggInterface::communicator::get_global_scope_comm() {
  return global_scope_comm;
}
void YggInterface::communicator::set_global_scope_comm(int new_value) {
  global_scope_comm = new_value;
}
void YggInterface::communicator::global_scope_comm_on() {
  set_global_scope_comm(1);
}
void YggInterface::communicator::global_scope_comm_off() {
  // #ifndef _OPENMP
  set_global_scope_comm(0);
  // #endif
}
int YggInterface::communicator::ygg_init(bool for_testing) {
  if (!global_context) {
    global_context.reset(new CommContext());
    return 0;
  }
  return global_context->init(for_testing);
}
void YggInterface::communicator::ygg_cleanup(CLEANUP_MODE mode) {
  global_context->cleanup(mode);
}
void YggInterface::communicator::ygg_exit() {
  global_context->cleanup();
}
