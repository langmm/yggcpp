#include "communicators/FunctionComm.hpp"
#include "utils/enums_utils.hpp"

using namespace YggInterface::communicator;
using namespace YggInterface::utils;

/////////////////////////////////////////////////////////
// FunctionWrapper
/////////////////////////////////////////////////////////

FunctionWrapper::FunctionWrapper(const std::string& f,
				 bool pointer_provided) :
  LogBase(), address(f), language(NO_LANGUAGE), func(nullptr),
  recv_backlog() {
  std::vector<std::string> parts = split(address, "::", 1);
  if (parts.size() != 2)
    throw_error("FunctionWrapper: Error parsing function address \""
		+ address + "\"");
  if (!enum_value_search(LANGUAGE_map, parts[0], language, true)) {
    throw_error("FunctionWrapper: Could not find language in "
		+ address + " (language part = " + parts[0] + ")");
  }
  switch (language) {
  case CXX_LANGUAGE:
  case C_LANGUAGE:
  case FORTRAN_LANGUAGE: {
    if (!pointer_provided)
      throw_error("FunctionWrapper: Pointer not provided to function");
    break;
  }
#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
  case PYTHON_LANGUAGE: {
    func = (void*)utils::import_python_object(parts[1].c_str());
    if (func == NULL)
      throw_error("FunctionWrapper: Error importing python function: "
		  + parts[1]);
    break;
  }
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
  // case MATLAB_LANGUAGE: {
  //   break;
  // }
  // case R_LANGUAGE: {
  // }
  // case JULIA_LANGUAGE: {
  // }
  // case JAVA_LANGUAGE: {
  // }
  default: {
    throw_error("FunctionWrapper: Unsupported language \""
		+ LANGUAGE_map.find(language)->second + "\"");
  }
  }
}

FunctionWrapper::FunctionWrapper(const std::string& name,
				 cxx_function& f) :
  FunctionWrapper(name, true) {
  func = (void*)(new cxx_function(f));
}

FunctionWrapper::FunctionWrapper(const std::string& name,
				 c_function* f) :
  FunctionWrapper(name, true) {
  func = (void*)(f);
}

FunctionWrapper::~FunctionWrapper() {
  switch (language) {
  case C_LANGUAGE:
  case FORTRAN_LANGUAGE: {
    func = nullptr;
    break;
  }
  case CXX_LANGUAGE: {
    cxx_function* c_func = (cxx_function*)func;
    func = nullptr;
    delete c_func;
    break;
  }
#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
  case PYTHON_LANGUAGE: {
    if (func) {
      PyObject* py_func = (PyObject*)func;
      func = nullptr;
      Py_CLEAR(py_func);
    }
    break;
  }
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
  default: {
    throw_error("~FunctionWrapper: Unsupported language \""
		+ LANGUAGE_map.find(language)->second + "\"");
  }
  }
}

bool FunctionWrapper::_call(const rapidjson::Document& data_send,
			    rapidjson::Document& data_recv) {
  if (!func) {
    log_error() << "_call: Function not initialized" << std::endl;
    return false;
  }
  switch (language) {
  case C_LANGUAGE:
  case FORTRAN_LANGUAGE: {
    c_function* f = (c_function*)func;
    generic_t c_data_send, c_data_recv;
    c_data_send.obj = (void*)(&data_send);
    c_data_recv.obj = (void*)(&data_recv);
    return (*f)(c_data_send, c_data_recv);
  }
  case CXX_LANGUAGE: {
    cxx_function* f = (cxx_function*)func;
    return (*f)(data_send, data_recv);
  }
#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
  case PYTHON_LANGUAGE: {
    PyObject* py_args = NULL;
    PyObject* py_args_orig = data_send.GetPythonObjectRaw();
    if (py_args_orig == NULL) {
      log_error() << "_call: Error converting arguments from C++ to Python: " <<
	data_send << std::endl;
      return false;
    }
    if (PyList_Check(py_args_orig)) {
      py_args = PyList_AsTuple(py_args_orig);
      Py_CLEAR(py_args_orig);
      if (py_args == NULL) {
	log_error() << "_call: Error converting arguments from Python list to Python tuple" << std::endl;
	return false;
      }
    } else {
      py_args = PyTuple_Pack(1, py_args_orig);
      Py_CLEAR(py_args_orig);
      if (py_args == NULL) {
	log_error() << "_call: Error packing arguments into Python tuple" << std::endl;
	return false;
      }
    }
    PyObject* py_result = PyObject_Call((PyObject*)func, py_args, NULL);
    Py_CLEAR(py_args);
    if (py_result == NULL) {
      log_error() << "_call: Python call failed" << std::endl;
      return false;
    }
    if (!data_recv.SetPythonObjectRaw(py_result, data_recv.GetAllocator())) {
      Py_CLEAR(py_result);
      log_error() << "_call: Error converting result from Python to C++" << std::endl;
      return false;
    }
    Py_CLEAR(py_result);
    return true;
  }
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
  default: {
    throw_error("_call: Unsupported language \""
		+ LANGUAGE_map.find(language)->second + "\"");
  }
  }
  return false;
}

bool FunctionWrapper::send(const rapidjson::Document& data) {
  recv_backlog.resize(recv_backlog.size() + 1);
  return _call(data, recv_backlog[recv_backlog.size() - 1]);
}

bool FunctionWrapper::recv(rapidjson::Document& data) {
  if (recv_backlog.empty())
    return false;
  data.Swap(recv_backlog[recv_backlog.size() - 1]);
  recv_backlog.resize(recv_backlog.size() - 1);
  return true;
}

int FunctionWrapper::nmsg() const {
  return static_cast<int>(recv_backlog.size());
}

void FunctionWrapper::clear() {
  recv_backlog.clear();
}

/////////////////////////////////////////////////////////
// FunctionComm
/////////////////////////////////////////////////////////

COMM_CONSTRUCTOR_CORE_DEF(FunctionComm, COMM_FLAG_DONT_SERIALIZE)

void FunctionComm::_open(bool call_base) {
  BEFORE_OPEN_DEF;
  updateMaxMsgSize(0);
  bool created = ((!address.valid()) || address.address().empty());
  if (created) {
    YGG_THREAD_SAFE_BEGIN(functions) {
      if (!global_context->func_registry_.empty()) {
	std::map<std::string, FunctionWrapper*>::iterator it = global_context->func_registry_.begin();
	address.address(it->first);
      }
    } YGG_THREAD_SAFE_END;
    if ((!address.valid()) || address.address().empty()) {
      throw std::runtime_error("FuntionComm::_open: Failed to find registered function");
    }
  }
  Comm_t::_init_name();
  handle = FunctionWrapper::create_function_wrapper(this->address.address());
  if (!handle)
    throw std::runtime_error("FuntionComm::_open: Failed to get function wrapper for \"" + this->address.address() + "\"");
  AFTER_OPEN_DEF;
}

void FunctionComm::_close(bool call_base) {
  BEFORE_CLOSE_DEF;
  if (handle && !global_comm) {
    handle->clear();
  }
  // Prevent function wrapper from being cleaned up so it can be reused.
  // It will be cleaned up by the communicator context.
  handle = nullptr;
  AFTER_CLOSE_DEF;
}

int FunctionComm::nmsg(DIRECTION dir) const {
  if (dir == NONE)
    dir = direction;
  if (dir == RECV && handle)
    return handle->nmsg();
  return 0;
}
      
int FunctionComm::send_single(utils::Header& header) {
  if (header.on_send() < 0)
    return -1;
  bool is_eof = (header.flags & HEAD_FLAG_EOF);
  rapidjson::Document doc;
  if (is_eof)
    return 1;
  if (!(header.flags & HEAD_FLAG_DOC_SET)) {
    log_error() << "send_single: Document not set" << std::endl;
    return -1;
  }
  if (!(handle && handle->send(header.doc)))
    return -1;
  return 1;
}

long FunctionComm::recv_single(utils::Header& header) {
  if (!(handle && handle->recv(header.doc))) {
    log_error() << "recv_single: Error receiving message" << std::endl;
    return -1;
  }
  header.flags |= HEAD_FLAG_DOC_SET;
  return 1;
}
