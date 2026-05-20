#include "communicators/StateInterface.hpp"
#include "utils/yggdrasil_rapidjson_wrapper.hpp"

using namespace YggInterface::communicator;


StateInterface::StateFunctionPtr
StateInterface::_ensure_ptr(typename StateInterface::StateFunctionPtr ptr) {
  return ptr;
}
StateInterface::StateFunctionPtr
StateInterface::_ensure_ptr(typename StateInterface::StateFunctionRefPtr ptr) {
  if(!ptr)
    return nullptr;
  return new StateInterface::StateFunction(*ptr);
}
StateInterface::StateFunctionPtr
StateInterface::_ensure_ptr(typename StateInterface::StateFunction& ptr) {
  return new StateInterface::StateFunction(ptr);
}

StateInterface::StateInterface(typename StateInterface::StateFunctionPtr fget,
                               typename StateInterface::StateFunctionPtr fset,
                               typename StateInterface::StateFunctionPtr fact,
                               const std::string& name,
                               FLAG_TYPE flags,
                               const COMM_TYPE request_commtype,
                               const COMM_TYPE reply_commtype) :
  LogBase(),
  comm(name, flags, SERVER_COMM, 0, request_commtype, reply_commtype),
  _created(false),
  _get(fget), _set(fset), _act(fact),
  _get_c(nullptr), _set_c(nullptr), _act_c(nullptr) {
  comm.addSchema("{\"type\": \"any\"}", false, SEND);
  comm.addSchema("{\"type\": \"any\"}", false, RECV);
}

StateInterface::StateInterface(typename StateInterface::StateFunction& fget,
                               typename StateInterface::StateFunction& fset,
                               typename StateInterface::StateFunction& fact,
                               const std::string& name,
                               FLAG_TYPE flags,
                               const COMM_TYPE request_commtype,
                               const COMM_TYPE reply_commtype) :
  StateInterface(StateInterface::_ensure_ptr(fget),
                 StateInterface::_ensure_ptr(fset),
                 StateInterface::_ensure_ptr(fact),
                 name, flags, request_commtype, reply_commtype) {}

StateInterface::StateInterface(typename StateInterface::StateFunctionRefPtr fget,
                               typename StateInterface::StateFunctionRefPtr fset,
                               typename StateInterface::StateFunctionRefPtr fact,
                               const std::string& name,
                               FLAG_TYPE flags,
                               const COMM_TYPE request_commtype,
                               const COMM_TYPE reply_commtype) :
  StateInterface(StateInterface::_ensure_ptr(fget),
                 StateInterface::_ensure_ptr(fset),
                 StateInterface::_ensure_ptr(fact),
                 name, flags, request_commtype, reply_commtype) {}

StateInterface::StateInterface(typename StateInterface::CStateFunctionPtr fget,
                               typename StateInterface::CStateFunctionPtr fset,
                               typename StateInterface::CStateFunctionPtr fact,
                               const std::string& name,
                               FLAG_TYPE flags,
                               const COMM_TYPE request_commtype,
                               const COMM_TYPE reply_commtype) :
  StateInterface((StateInterface::StateFunctionPtr)nullptr,
                 (StateInterface::StateFunctionPtr)nullptr,
                 (StateInterface::StateFunctionPtr)nullptr,
                 name, flags, request_commtype, reply_commtype) {
  _get_c = fget;
  _set_c = fset;
  _act_c = fact;
}

StateInterface::~StateInterface() {
  if (_created) {
#define CLEAR_FUNC(name)                        \
    if (name) {                                 \
      delete name;                              \
      name = nullptr;                           \
    }
    CLEAR_FUNC(_get)
    CLEAR_FUNC(_set)
    CLEAR_FUNC(_act)
#undef CLEAR_FUNC
  }
}

std::string StateInterface::logInst() const {
  return comm.getName();
}

bool StateInterface::reply_to_requests() {
  bool flag = true;
  yggdrasil_rapidjson::Document request, reply;
  std::string name, command;
  yggdrasil_rapidjson::Document::AllocatorType& allocator = reply.GetAllocator();
  while (flag) {
    flag = comm.recv(request);
    if (!flag) {
      log_error() << "Failed to receive a state request" << std::endl;
      goto cleanup;
    }
    log_debug() << "Received request: " << request << std::endl;
    if (!(request.IsArray() && request.Size() >= 1
          && request[0].IsString())) {
      log_info() << "State requests must be an array where the " <<
        "first element is the request type string. Request: " <<
        request << std::endl;
      flag = false;
      goto do_reply;
    }
    command = request[0].GetString();
    if (command == "resume") {
      log_debug() << "Resuming" << std::endl;
      break;
    }
    if (!(request.Size() >= 2 && request[1].IsString())) {
      log_info() << "The second element of \"" << command <<
        "\" state requests must be the name of the variable or " <<
        "action. Request: " << request << std::endl;
      flag = false;
      goto do_reply;
    }
    name = request[1].GetString();
    if (request.Size() == 3) {
      reply.CopyFrom(request[2], allocator, true);
    } else if (request.Size() > 3) {
      log_info() << "State requests must not have more than 3 " <<
        "elements. Request: " << request << std::endl;
      flag = false;
      goto do_reply;
    }
    if (command == "get") {
      flag = get(name, reply);
    } else if (command == "set") {
      flag = set(name, reply);
    } else if (command == "act") {
      flag = act(name, reply);
    } else {
      log_info() << "State requests must be one of \"resume\", " <<
        "\"get\", \"set\", or \"act\", but \"" << command <<
        "\" was received. Request: " << request << std::endl;
      reply.SetNull();
      flag = false;
      goto do_reply;
    }
    if (!flag) {
      log_info() << "Error in \"" << command <<
        "\" request. Request: " << request << std::endl;
      reply.SetNull();
    }
  do_reply:
    if (reply.Empty()) {
      if (flag)
        reply.SetString("ok", allocator);
      else
        reply.SetString("error", allocator);
    }
    if (comm.send(reply) < 0) {
      log_error() << "Failed to send reply to request: " <<
        request << std::endl;
      flag = false;
      goto cleanup;
    }
    reply.SetNull();
    flag = true;
  }
  if (reply.Empty()) {
    if (flag)
      reply.SetString("ok", allocator);
    else
      reply.SetString("error", allocator);
  }
  flag = (comm.send(reply) >= 0);
 cleanup:
  if (!flag) {
    ygglog_throw_error("Error in state interface");
  }
  return flag;
}

bool StateInterface::_call_c(typename StateInterface::CStateFunctionPtr func,
                             const std::string& name,
                             yggdrasil_rapidjson::Document& data) {
  const char* name_c = name.c_str();
  generic_t data_c;
  data_c.obj = (void*)(&data);
  return (func(name_c, data_c) > 0);
}

bool StateInterface::get(const std::string& name,
                         yggdrasil_rapidjson::Document& data) {
  try {
    if (_get) return (*_get)(name, data);
    if (_get_c) return _call_c(_get_c, name, data);
  } catch (...) {
    log_info() << "Error in call to user provided get function" << std::endl;
    return false;
  }
  log_info() << "No function defined for getting state variables" << std::endl;
  return false;
}

bool StateInterface::set(const std::string& name,
                         yggdrasil_rapidjson::Document& data) {
  try {
    if (_set) return (*_set)(name, data);
    if (_set_c) return _call_c(_set_c, name, data);
  } catch (...) {
    log_info() << "Error in call to user provided set function" << std::endl;
    return false;
  }
  log_info() << "No function defined for setting state variables" << std::endl;
  return false;
}

bool StateInterface::act(const std::string& name,
                         yggdrasil_rapidjson::Document& data) {
  try {
    if (_act) return (*_act)(name, data);
    if (_act_c) return _call_c(_act_c, name, data);
  } catch (...) {
    log_info() << "Error in call to user provided act function" << std::endl;
    return false;
  }
  log_info() << "No function defined for performing actions" << std::endl;
  return false;
}
