#include "comms.hpp"
#include "utils/tools.hpp"
#include "utils/rapidjson_wrapper.hpp"

using namespace communication::communicator;

CommContext::CommContext(bool for_testing) :
  LogBase(), registry_(), thread_id(utils::get_thread_id()),
  for_testing_(for_testing), cleanup_mode_(), zmq_ctx(NULL) {
  log_debug() << "CommContext: New context" << std::endl;
  init(for_testing);
}
CommContext::~CommContext() {
  cleanup(CLEANUP_ATEXIT);
}
int CommContext::init(bool for_testing) {
  log_debug() << "init: Begin" << std::endl;
  for_testing_ = for_testing;
  utils::initialize_python("CommContext::init");
#ifdef ZMQINSTALLED
  YGG_THREAD_SAFE_BEGIN_LOCAL(zmq) {
    if (zmq_ctx == NULL)
      zmq_ctx = zmq_ctx_new();
  } YGG_THREAD_SAFE_END;
#endif // ZMQINSTALLED
  log_debug() << "init: End" << std::endl;
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
    YGG_THREAD_SAFE_BEGIN_LOCAL(comms) {
      registry_.clear();
      if (mode != CLEANUP_COMMS) {
	YGG_THREAD_SAFE_BEGIN_LOCAL(zmq) {
	  if (zmq_ctx != NULL) {
	    zmq_ctx_shutdown(zmq_ctx);
	    if (zmq_ctx_term(zmq_ctx) != 0) {
	      log_error() << "cleanup: Error terminating ZeroMQ context via zmq_ctx_term" << std::endl;
	    }
	    zmq_ctx = NULL;
	  }
	} YGG_THREAD_SAFE_END;
	utils::finalize_python("CommContext::cleanup");
      }
    } YGG_THREAD_SAFE_END;
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

void CommContext::register_comm(Comm_t* x) {
  if (x->getFlags() & COMM_FLAG_ASYNC_WRAPPED)
    return;
  YGG_THREAD_SAFE_BEGIN_LOCAL(comms) {
    x->index_in_register = registry_.size();
    registry_.push_back(x);
  } YGG_THREAD_SAFE_END;
}

Comm_t* CommContext::find_registered_comm(const std::string& name,
					  const DIRECTION dir,
					  const COMM_TYPE type) {
  Comm_t* out = NULL;
  assert(!name.empty());
  YGG_THREAD_SAFE_BEGIN_LOCAL(comms) {
    if (global_scope_comm) {
      for (std::vector<Comm_t*>::iterator it = registry_.begin();
	   it != registry_.end(); it++) {
	if (*it && (*it)->global() &&
	    ((*it)->name == name) &&
	    ((*it)->direction == dir) &&
	    ((*it)->type == type)) {
	  out = *it;
	  break;
	}
      }
    }
  } YGG_THREAD_SAFE_END;
  return out;
}
