#include "communicators/FunctionComm.hpp"
#include "utils/enums_utils.hpp"
#include "utils/tools.hpp"
#include "utils/multiprocessing.hpp"
#ifdef _WIN32
#include <windows.h>
#include <system_error>
#else
#include <dlfcn.h>
#endif

using namespace YggInterface::communicator;
using namespace YggInterface::utils;

#define CALL_ALIASED_FUNCTION_WRAPPER_NORET(method)	\
  if ((flags & FUNCTION_WEAK_REF) && func) {		\
    ((FunctionWrapper*)func)->method;			\
  } else
#define CALL_ALIASED_FUNCTION_WRAPPER(method, ret)	\
  if ((flags & FUNCTION_WEAK_REF) && func) {		\
    ret = ((FunctionWrapper*)func)->method;		\
  } else


/////////////////////////////////////////////////////////
// DynamicLibrary
/////////////////////////////////////////////////////////

DynamicLibrary::DynamicLibrary(LANGUAGE lang, const std::string& name,
			       LANGUAGE calling_lang) :
  LogBase(), language(lang), address(name), library(nullptr),
  calling_language(calling_lang) {
  std::vector<std::string> parts = split(address, ".", 1, true);
  if (parts.size() == 1 || (parts.size() == 2 && parts[0].size() == 0)) {
#ifdef _WIN32
    address += ".dll";
#elif __APPLE__
    address += ".dylib";
#else
    address += ".so";
#endif
  }
  setenv("YGG_PREVENT_PYTHON_INITIALIZATION", "1", 1);
  std::vector<std::string> to_try;
  to_try.push_back(address);
  std::string address_forward, address_dir, address_base, address_base_alt;
// This is primarily for ease in testing
  if (address.find("/") == std::string::npos &&
      address.find("\\") == std::string::npos) {
    char* dynamic_dir_str = getenv("YGGTEST_DYNAMIC_DIR");
    if (dynamic_dir_str) {
      std::string dynamic_dir(dynamic_dir_str);
      address = dynamic_dir + "/" + address;
    }
  }
#ifdef _WIN32
  if (address.find("/") != std::string::npos) {
    address_forward = address;
    to_try.push_back(address);
    regex_replace(to_try[to_try.size() - 1], "/", "\\");
  } else if (address.find("\\") != std::string::npos) {
    regex_replace(address_forward, "\\", "/");
    to_try.push_back(address_forward);
  }
#else
  if (address.find("/") != std::string::npos) {
    address_forward = address;
  }
#endif
  if (!address_forward.empty()) {
    std::vector<std::string> path_parts = split(address, "/", 1, true);
    address_dir = path_parts[0];
    address_base = path_parts[1];
  } else {
    address_base = address;
  }
  if (address_base.find("lib") == 0) {
    address_base_alt = address_base.substr(3);
  } else {
    address_base_alt = "lib" + address_base;
  }
  if (!address_dir.empty())
    address_base_alt = address_dir + "/" + address_base_alt;
  to_try.push_back(address_base_alt);
#ifdef _WIN32
  if (!address_forward.empty()) {
    to_try.push_back(address_base_alt);
    regex_replace(to_try[to_try.size() - 1], "/", "\\");
  }
#endif
  for (std::vector<std::string>::iterator it = to_try.begin();
       it != to_try.end(); it++) {
    if (load(*it)) {
      address = *it;
      log_debug() << "DynamicLibrary: Loaded library \"" << address <<
	"\"" << std::endl;
      break;
    }
  }
  unsetenv("YGG_PREVENT_PYTHON_INITIALIZATION");
  if (!library)
    throw_error("DynamicLibrary: Failed to load library: " + address);
}

bool DynamicLibrary::load(const std::string& name) {
  if (library) {
    log_error() << "load: Library already loaded" << std::endl;
    return false;
  }
  std::string error_msg;
#ifdef _WIN32
  library = (void*)LoadLibraryA(name.c_str());
  if (!library)
    error_msg = Win32Base::error();
#else
  library = dlopen(name.c_str(), RTLD_LAZY | RTLD_GLOBAL);
  if (!library)
    error_msg = SysVBase::error();
#endif
  if (!library) {
    log_info() << "load: Failed to load library: " << name << " = " <<
      error_msg << std::endl;
    return false;
  }
  return true;
}

DynamicLibrary::~DynamicLibrary() {
  if (library) {
#ifdef _WIN32
    if (!FreeLibrary((HMODULE)library))
#else
    if (dlclose(library) != 0)
#endif
      throw_error("DynamicLibrary: Error unloading library: " + address);
    library = nullptr;
  }
}

void* DynamicLibrary::function(const std::string& name) {
  void* out = NULL;
  if (library) {
#ifdef _WIN32
    out = (void*)GetProcAddress((HMODULE)library, name.c_str());
#else
    out = dlsym(library, name.c_str());
#endif
  }
  return out;
}


/////////////////////////////////////////////////////////
// FunctionWrapper
/////////////////////////////////////////////////////////

FunctionWrapper::FunctionWrapper(const std::string& f,
				 bool pointer_provided,
				 const LANGUAGE calling_lang,
				 int flags0) :
  LogBase(), address(f), language(NO_LANGUAGE),
  calling_language(calling_lang), flags(flags0), library(nullptr),
  func(nullptr), recv_backlog() {
  std::vector<std::string> parts = split(address, "::", 1);
  if (parts.size() != 2)
    throw_error("FunctionWrapper: Error parsing function address \""
		+ address + "\"");
  if (!enum_value_search(LANGUAGE_map(), parts[0], language, true)) {
    throw_error("FunctionWrapper: Could not find language in "
		+ address + " (language part = " + parts[0] + ")");
  }
  switch (language) {
  case CXX_LANGUAGE:
  case C_LANGUAGE:
  case FORTRAN_LANGUAGE: {
    if (!pointer_provided) {
      std::vector<std::string> libparts = split(parts[1], "::", 1, true);
      if (libparts.size() != 2)
	throw_error("FunctionWrapper: Error parsing function address for library name \""
		    + address + "\"");
      library = new DynamicLibrary(language, libparts[0],
				   calling_language);
      func = library->function(libparts[1]);
      if (!func)
	throw_error("FunctionWrapper: Error locating function \""
		    + libparts[1] + "\"");
    }
    break;
  }
#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
  case PYTHON_LANGUAGE: {
    YGGDRASIL_PYGIL_BEGIN;
    func = (void*)utils::import_python_object(parts[1].c_str());
    YGGDRASIL_PYGIL_END;
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
		+ LANGUAGE_map().find(language)->second + "\"");
  }
  }
  if ((!func) && (!pointer_provided))
    throw_error("FunctionWrapper: Failed to initialize "
		+ LANGUAGE_map().find(language)->second + " function: "
		+ parts[1]);
}

FunctionWrapper::FunctionWrapper(const std::string& name,
				 cxx_function& f, int flags) :
  FunctionWrapper(name, true, NO_LANGUAGE, flags) {
  func = (void*)(new cxx_function(f));
}

FunctionWrapper::FunctionWrapper(const std::string& name,
				 c_function& f, int flags) :
  FunctionWrapper(name, true, NO_LANGUAGE, flags) {
  func = (void*)(f);
}

FunctionWrapper::FunctionWrapper(const FunctionWrapper& rhs,
				 const LANGUAGE calling_lang,
				 int flags0) :
  LogBase(), address(rhs.address), language(rhs.language),
  calling_language(calling_lang), flags(flags0), library(nullptr),
  func((void*)(&rhs)), recv_backlog() {}

FunctionWrapper::~FunctionWrapper() {
  if (flags & FUNCTION_WEAK_REF) {
    func = nullptr;
    return;
  }
  switch (language) {
  case C_LANGUAGE:
  case FORTRAN_LANGUAGE: {
    func = nullptr;
    if (library)
      delete library;
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
      YGGDRASIL_PYGIL_BEGIN;
      PyObject* py_func = (PyObject*)func;
      func = nullptr;
      Py_CLEAR(py_func);
      YGGDRASIL_PYGIL_END;
    }
    break;
  }
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
  default: {
    throw_error("~FunctionWrapper: Unsupported language \""
		+ LANGUAGE_map().find(language)->second + "\"");
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
    generic_t c_data_send, c_data_recv;
    c_data_send.obj = (void*)(&data_send);
    c_data_recv.obj = (void*)(&data_recv);
    return _call_pointer(func, c_data_send, c_data_recv);
  }
  case CXX_LANGUAGE: {
    cxx_function* f = (cxx_function*)func;
    return (*f)(data_send, data_recv);
  }
#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
  case PYTHON_LANGUAGE: {
    bool py_out = false;
    PyObject *py_args = NULL, *py_args_orig = NULL, *py_result = NULL;
    YGGDRASIL_PYGIL_BEGIN;
    py_args_orig = data_send.GetPythonObjectRaw();
    if (py_args_orig == NULL) {
      log_error() << "_call: Error converting arguments from C++ to Python: " <<
	data_send << std::endl;
      goto cleanup;
    }
    if (PyList_Check(py_args_orig)) {
      py_args = PyList_AsTuple(py_args_orig);
      Py_CLEAR(py_args_orig);
      if (py_args == NULL) {
	log_error() << "_call: Error converting arguments from Python list to Python tuple" << std::endl;
	goto cleanup;
      }
    } else {
      py_args = PyTuple_Pack(1, py_args_orig);
      Py_CLEAR(py_args_orig);
      if (py_args == NULL) {
	log_error() << "_call: Error packing arguments into Python tuple" << std::endl;
	goto cleanup;
      }
    }
    py_result = PyObject_Call((PyObject*)func, py_args, NULL);
    Py_CLEAR(py_args);
    if (py_result == NULL) {
      log_error() << "_call: Python call failed" << std::endl;
      goto cleanup;
    }
    if (!data_recv.SetPythonObjectRaw(py_result, data_recv.GetAllocator())) {
      Py_CLEAR(py_result);
      log_error() << "_call: Error converting result from Python to C++" << std::endl;
      goto cleanup;
    }
    py_out = true;
    cleanup:
    Py_CLEAR(py_args);
    Py_CLEAR(py_args_orig);
    Py_CLEAR(py_result);
    YGGDRASIL_PYGIL_END;
    return py_out;
  }
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
  default: {
    throw_error("_call: Unsupported language \""
		+ LANGUAGE_map().find(language)->second + "\"");
  }
  }
  return false;
}

bool FunctionWrapper::send(const rapidjson::Document& data, bool is_eof) {
  bool out = false;
  CALL_ALIASED_FUNCTION_WRAPPER(send(data, is_eof), out) {
    YGG_THREAD_SAFE_BEGIN(functions) {
      recv_backlog.resize(recv_backlog.size() + 1);
      if (is_eof) {
	recv_backlog[recv_backlog.size() - 1].SetString(YGG_MSG_EOF, YGG_MSG_EOF_LEN,
							recv_backlog[recv_backlog.size() - 1].GetAllocator());
	out = true;
      } else {
	out = _call(data, recv_backlog[recv_backlog.size() - 1]);
      }
    } YGG_THREAD_SAFE_END;
  }
  return out;
}

bool FunctionWrapper::recv(rapidjson::Document& data) {
  bool out = false;
  CALL_ALIASED_FUNCTION_WRAPPER(recv(data), out) {
    YGG_THREAD_SAFE_BEGIN(functions) {
      if (!recv_backlog.empty()) {
	data.Swap(recv_backlog[0]);
	recv_backlog.erase(recv_backlog.begin());
	out = true;
      }
    } YGG_THREAD_SAFE_END;
  }
  return out;
}

int FunctionWrapper::nmsg() const {
  int out = -1;
  CALL_ALIASED_FUNCTION_WRAPPER(nmsg(), out) {
    YGG_THREAD_SAFE_BEGIN(functions) {
      out = static_cast<int>(recv_backlog.size());
    } YGG_THREAD_SAFE_END;
  }
  return out;
}

void FunctionWrapper::clear() {
  CALL_ALIASED_FUNCTION_WRAPPER_NORET(clear()) {
    YGG_THREAD_SAFE_BEGIN(functions) {
      recv_backlog.clear();
    } YGG_THREAD_SAFE_END;
  }
}

/////////////////////////////////////////////////////////
// FunctionComm
/////////////////////////////////////////////////////////

COMM_CONSTRUCTOR_CORE_DEF(FunctionComm, COMM_FLAG_DONT_SERIALIZE)

void FunctionComm::_open(bool call_base) {
  BEFORE_OPEN_DEF;
  updateMaxMsgSize(0);
  bool created = ((!address.valid()) || address.address().empty());
  bool is_async = (getFlags() & COMM_FLAG_ASYNC_WRAPPED);
  int func_flags = 0;
  if (is_async)
    func_flags |= FUNCTION_ON_ASYNC;
  handle = ctx->find_registered_function(this->address.address());
  if (created && handle)
    address.address(handle->address);
  Comm_t::_init_name();
  if (!handle) {
    if (created) {
      throw std::runtime_error("FunctionComm::_open: Failed to get function wrapper for \"" + this->address.address() + "\"");
    } else {
      handle = new FunctionWrapper(address.address(), false,
				   language, func_flags);
      ctx->register_function(handle);
      
    }
  }
  if (handle && is_async)
    handle = new FunctionWrapper(*handle, language,
				 func_flags | FUNCTION_WEAK_REF);
  if (handle && handle->language == PYTHON_LANGUAGE)
    getFlags() |= COMM_FLAG_REQUIRES_PYGIL;
  AFTER_OPEN_DEF;
}

void FunctionComm::_close(bool call_base) {
  BEFORE_CLOSE_DEF;
  if ((direction == RECV) && this->is_open() &&
      (!global_comm) && (!(flags & COMM_FLAG_EOF_RECV)) &&
      (utils::YggdrasilLogger::_ygg_error_flag == 0)) {
    handle->clear();
  }
  // Prevent function wrapper from being cleaned up so it can be reused.
  // It will be cleaned up by the communicator context.
  if (handle && (handle->flags & FUNCTION_WEAK_REF)) {
    delete handle;
  }
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
  // TODO: Copy full message with header?
  // if (is_eof) {
  //   doc.SetString(YGG_MSG_EOF, YGG_MSG_EOF_LEN, doc.GetAllocator());
  //   if (!(handle && handle->send(doc, is_eof)))
  //     return -1;
  // } else {
  if (!(is_eof || (header.flags & HEAD_FLAG_DOC_SET))) {
    log_error() << "send_single: Document not set" << std::endl;
    return -1;
  }
  if (!(handle && handle->send(header.doc, is_eof)))
    return -1;
  return 1;
}

long FunctionComm::recv_single(utils::Header& header) {
  if (!(handle && handle->recv(header.doc))) {
    log_error() << "recv_single: Error receiving message" << std::endl;
    return -1;
  }
  if (header.doc.IsString()) 
    header.setMessageFlags(header.doc.GetString(),
			   static_cast<size_t>(header.doc.GetStringLength()));
  header.flags |= HEAD_FLAG_DOC_SET;
  return 1;
}

void YggInterface::communicator::register_function(const std::string& name,
						   cxx_function& func,
						   bool no_prefix) {
  std::string new_name = name;
  if (!no_prefix)
    new_name = "cxx::" + new_name;
  if (!global_context->find_registered_function(new_name)) {
    FunctionWrapper* created = new FunctionWrapper(new_name, func);
    global_context->register_function(created);
  }
}

void YggInterface::communicator::register_function(const std::string& name,
						   cxx_function_alt& func,
						   bool no_prefix) {
  cxx_function cxx_func = func;
  register_function(name, cxx_func, no_prefix);
}

void YggInterface::communicator::register_function(const std::string& name,
						   c_function& func,
						   bool no_prefix) {
  std::string new_name = name;
  if (!no_prefix)
    new_name = "c::" + new_name;
  if (!global_context->find_registered_function(new_name)) {
    FunctionWrapper* created = new FunctionWrapper(new_name, func);
    global_context->register_function(created);
  }
}
