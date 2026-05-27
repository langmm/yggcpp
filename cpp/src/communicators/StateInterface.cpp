#include "communicators/StateInterface.hpp"
#include "utils/yggdrasil_rapidjson_wrapper.hpp"
#include "communicators/FunctionComm.hpp"

using namespace YggInterface::communicator;

StateFunction::StateFunction() {}
StateFunction::~StateFunction() {}
bool StateFunction::operator()(const std::string&,
                               yggdrasil_rapidjson::Document&) {
  return false;
}
StateFunction* StateFunction::copy() const {
  return new StateFunction();
}

CXXStateFunction::CXXStateFunction(typename CXXStateFunction::FunctionTypePtr ptr) :
  StateFunction(), _created(false), _ptr(ptr) {}
CXXStateFunction::CXXStateFunction(typename CXXStateFunction::FunctionType& func) :
  StateFunction(), _created(true),
  _ptr(new CXXStateFunction::FunctionType(func)) {}
CXXStateFunction::CXXStateFunction(typename CXXStateFunction::FunctionPtr ptr) :
  StateFunction(), _created(true), _ptr(nullptr) {
  if (ptr)
    _ptr = new CXXStateFunction::FunctionType(*ptr);
}
CXXStateFunction::~CXXStateFunction() {
  if (_created && _ptr)
    delete _ptr;
  _ptr = nullptr;
}
bool CXXStateFunction::operator()(const std::string& name,
                                  yggdrasil_rapidjson::Document& data) {
  if (!_ptr) return false;
  return (*_ptr)(name, data);
}
StateFunction* CXXStateFunction::copy() const {
  if (!_ptr) return new CXXStateFunction();
  return new CXXStateFunction(*_ptr);
}

CStateFunction::CStateFunction(typename CStateFunction::FunctionPtr ptr) :
  StateFunction(), _ptr(ptr) {}
CStateFunction::~CStateFunction() {}
bool CStateFunction::operator()(const std::string& name,
                                yggdrasil_rapidjson::Document& data) {
  if (!_ptr) return false;
  const char* name_c = name.c_str();
  generic_t data_c;
  data_c.obj = (void*)(&data);
  return (_ptr(name_c, data_c) > 0);
}
StateFunction* CStateFunction::copy() const {
  return new CStateFunction(_ptr);
}

EmbeddedStateFunction::EmbeddedStateFunction(void* ptr,
                                             const LANGUAGE& language) :
  StateFunction(), _ptr(nullptr) {
  if (ptr)
    _ptr = new FunctionWrapper("", ptr, language);
}
EmbeddedStateFunction::EmbeddedStateFunction(FunctionWrapper& func) :
  StateFunction(), _ptr(new FunctionWrapper(func)) {}
EmbeddedStateFunction::EmbeddedStateFunction(FunctionWrapper* ptr) :
  StateFunction(), _ptr(nullptr) {
  if (ptr)
    _ptr = new FunctionWrapper(*ptr);
}
EmbeddedStateFunction::~EmbeddedStateFunction() {
  if (_ptr) {
    delete _ptr;
    _ptr = nullptr;
  }
}
bool EmbeddedStateFunction::operator()(const std::string& name,
                                       yggdrasil_rapidjson::Document& data) {
  if (_ptr) return false;
  yggdrasil_rapidjson::Document data_send, data_recv;
  yggdrasil_rapidjson::Document::AllocatorType& allocator = data_send.GetAllocator();
  data_send.SetArray();
  data_send.PushBack(yggdrasil_rapidjson::Value(name.c_str(),
                                                name.size(),
                                                allocator).Move(),
                     allocator);
  data_send.PushBack(yggdrasil_rapidjson::Value(data, allocator, true).Move(),
                     allocator);
  bool out = _ptr->operator()(data_send, data_recv);
  if (out) {
    data.SetNull();
    data.CopyFrom(data_recv, data.GetAllocator(), true);
  }
  return out;
}
StateFunction* EmbeddedStateFunction::copy() const {
  return new EmbeddedStateFunction(_ptr);
}

StateFunction* StateInterface::_wrap_func(StateFunction* func) {
  if (!func) return new CXXStateFunction();
  return func->copy();
}
StateFunction* StateInterface::_wrap_func(typename CXXStateFunction::FunctionType& func) {
  return new CXXStateFunction(func);
}
StateFunction* StateInterface::_wrap_func(typename CXXStateFunction::FunctionTypePtr func) {
  return new CXXStateFunction(func);
}
StateFunction* StateInterface::_wrap_func(typename CXXStateFunction::FunctionPtr func) {
  return new CXXStateFunction(func);
}
StateFunction* StateInterface::_wrap_func(typename CStateFunction::FunctionPtr func) {
  return new CStateFunction(func);
}
StateFunction* StateInterface::_wrap_func(FunctionWrapper& func) {
  return new EmbeddedStateFunction(func);
}
StateFunction* StateInterface::_wrap_func(typename EmbeddedStateFunction::FunctionPtr func) {
  return new EmbeddedStateFunction(func);
}
StateFunction* StateInterface::_wrap_func(void* ptr, const LANGUAGE& language) {
  return new EmbeddedStateFunction(ptr, language);
}

StateInterface::~StateInterface() {
#define CLEAR_FUNC(name)                        \
  if (name) {                                   \
    delete name;                                \
    name = nullptr;                             \
  }
  CLEAR_FUNC(_get)
  CLEAR_FUNC(_set)
  CLEAR_FUNC(_act)
#undef CLEAR_FUNC
}

std::string StateInterface::logInst() const {
  return comm.getName();
}

bool StateInterface::reply_to_requests() {
  if (_complete) return true;
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
    } else if (command == "terminate" || command == "error") {
      log_debug() << "Terminating" << std::endl;
      flag = false;
      break;
    } else if (command == "complete") {
      log_debug() << "Continuing without pause" << std::endl;
      _complete = true;
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

// TODO: Add error to data?
#define CALL_FUNC(func)                         \
  if (!func) return false;                      \
  try {                                         \
    return func->operator()(name, data);        \
  } catch (...) {                               \
    return false;                               \
  }

bool StateInterface::get(const std::string& name,
                         yggdrasil_rapidjson::Document& data) {
  CALL_FUNC(_get)
}

bool StateInterface::set(const std::string& name,
                         yggdrasil_rapidjson::Document& data) {
  CALL_FUNC(_set)
}

bool StateInterface::act(const std::string& name,
                         yggdrasil_rapidjson::Document& data) {
  CALL_FUNC(_act)
}

#undef CALL_FUNC
