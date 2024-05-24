#include "communicators/comm_t.hpp"
#include "communicators/CommBase.hpp"
#include "communicators/comms.hpp"
#include "utils/logging.hpp"
#include "utils/tools.hpp"
#include "utils/rapidjson_wrapper.hpp"

extern "C" {

void ygglog_error(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  std::string str = YggInterface::utils::string_format_va(fmt, ap);
  va_end(ap);
  YggLogError << str << std::endl;
}
void ygglog_debug(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  std::string str = YggInterface::utils::string_format_va(fmt, ap);
  va_end(ap);
  YggLogDebug << str << std::endl;
}
void ygglog_info(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  std::string str = YggInterface::utils::string_format_va(fmt, ap);
  va_end(ap);
  YggLogInfo << str << std::endl;
}
  
int ygg_init() {
  try {
    return YggInterface::communicator::ygg_init();
  } catch (...) {
    return -1;
  }
}

void ygg_exit() {
  try {
    YggInterface::communicator::ygg_exit();
  } catch (...) {
    _exit(1);
  }
}

int _register_function_wrapper(const char* name, c_function func,
			       bool no_prefix) {
  try {
    std::string name_s(name);
    YggInterface::communicator::FunctionWrapper::register_function_wrapper(name_s, func, no_prefix);
  } catch (...) {
    return -1;
  }
  return 0;
}
  
int register_function_wrapper(const char* name, c_function func) {
  return _register_function_wrapper(name, func, false);
}
  
void free_comm(comm_t* comm) {
    if (comm && comm->comm) {
        auto c = static_cast<YggInterface::communicator::Comm_t *>(comm->comm);
	delete c;
	c = nullptr;
        comm->comm = nullptr;
    }
}

void close_comm(comm_t* comm) {
    free_comm(comm);
}

// comm_t open_comm(char* address, const DIRECTION dir, const COMM_TYPE &t) {
//     comm_t ret;
//     std::string name = "";
//     _BEGIN_CPP {
//       ret.comm = (void*) YggInterface::communicator::new_Comm_t(dir, t, name, address);
//     } _END_CPP_CLEANUP(open_comm, ret, ret.comm = NULL);
//     return ret;
// }

comm_t _init_comm(const char* name, const enum DIRECTION dir,
		  const enum COMM_TYPE t,
		  dtype_t* datatype, const FLAG_TYPE flags,
		  const size_t ncomm) {
  comm_t ret;
  _BEGIN_CPP {
    ret.comm = (void*) YggInterface::communicator::new_Comm_t(dir, t, name, (char*)NULL, flags, ncomm);
    if (!(ret.comm)) {
      YggLogError << "init_comm(" << name << "): Error initializing comm" << std::endl;
      return ret;
    }
    if (!comm_set_datatype(ret, datatype)) {
      free_comm(&ret);
      return ret;
    }
    if (!((static_cast<YggInterface::communicator::Comm_t*>(ret.comm))->getFlags() & COMM_FLAG_VALID))
      free_comm(&ret);
  } _END_CPP_CLEANUP(init_comm, ret, ret.comm = NULL);
  return ret;
}
comm_t init_comm(const char* name, const enum DIRECTION dir,
		 const enum COMM_TYPE t,
		 dtype_t* datatype) {
  FLAG_TYPE flags = COMM_FLAG_INTERFACE;
  comm_t out = _init_comm(name, dir, t, datatype, flags, 0);
  set_comm_language(out, C_LANGUAGE);
  return out;
}
comm_t init_comm_flags(const char* name, const enum DIRECTION dir,
		       const enum COMM_TYPE t, FLAG_TYPE flags) {
  flags |= COMM_FLAG_INTERFACE;
  comm_t out = _init_comm(name, dir, t, NULL, flags, 0);
  set_comm_language(out, C_LANGUAGE);
  return out;
}

int set_comm_language(comm_t x, const enum LANGUAGE lang) {
  _BEGIN_CPP {
    if (!x.comm)
      ygglog_throw_error("set_comm_language: Comm is not initialized");
    if (!(static_cast<YggInterface::communicator::Comm_t*>(x.comm)->setLanguage(lang)))
      return 0;
  } _END_CPP(set_comm_language, 0);
  return 1;
}

int set_response_format(comm_t comm, const char *fmt) {
  _BEGIN_CPP {
    if (!comm.comm)
      ygglog_throw_error("set_response_format: Comm is not initialized");
    COMM_TYPE ctype = static_cast<YggInterface::communicator::Comm_t*>(comm.comm)->getType();
    if (ctype != SERVER_COMM && ctype != CLIENT_COMM)
      ygglog_throw_error("set_response_format: Comm is not RPC server or client");
    std::string format_str(fmt);
    if (!static_cast<YggInterface::communicator::RPCComm*>(comm.comm)->addResponseFormat(format_str))
      return 0;
  } _END_CPP(set_response_format, 0);
  return 1;
}

int set_response_datatype(comm_t x, dtype_t* datatype) {
  _BEGIN_CPP {
    if (datatype) {
      if (datatype->metadata) {
	YggInterface::utils::Metadata* metadata = static_cast<YggInterface::utils::Metadata*>(datatype->metadata);
	if (x.comm) {
	  if (!static_cast<YggInterface::communicator::RPCComm*>(x.comm)->addResponseSchema(*metadata))
	    return 0;
	}
	datatype->metadata = NULL;
	delete metadata;
      }
      if (!x.comm)
	ygglog_throw_error("set_response_datatype: Comm is not initialized");
    }
  } _END_CPP(set_response_datatype, 0);
  return 1;
}

dtype_t comm_get_datatype(comm_t x) {
  dtype_t out;
  _BEGIN_CPP {
    YggInterface::utils::Metadata* metadata = &(static_cast<YggInterface::communicator::Comm_t*>(x.comm)->getMetadata());
    dtype_t tmp;
    tmp.metadata = (void*)metadata;
    out = copy_dtype(tmp);
  } _END_CPP_CLEANUP(comm_get_datatype, out,
		     destroy_dtype(&out));
  return out;
}
int comm_set_datatype(comm_t x, dtype_t* datatype) {
  int ret = 1;
  _BEGIN_CPP {
    if (datatype && datatype->metadata) {
      YggInterface::utils::Metadata* metadata = static_cast<YggInterface::utils::Metadata*>(datatype->metadata);
      if (!static_cast<YggInterface::communicator::Comm_t*>(x.comm)->addSchema(*metadata)) {
	return 0;
      }
      datatype->metadata = NULL;
      delete metadata;
    }
  } _END_CPP_CLEANUP(comm_set_datatype, ret, ret = 0);
  return 1;
}

int is_comm_format_array_type(const comm_t x) {
  int out = 0;
  _BEGIN_CPP {
    const YggInterface::utils::Metadata* metadata = &(static_cast<const YggInterface::communicator::Comm_t*>(x.comm)->getMetadata());
    out = static_cast<int>(metadata->isFormatArray());
  } _END_CPP(is_comm_format_array_type, -1);
  return out;
}

int comm_send(comm_t comm, const char *data, const size_t len) {
  _BEGIN_CPP {
    if (!comm.comm)
      ygglog_throw_error("comm_send: Comm is not initialized");
    return static_cast<YggInterface::communicator::Comm_t*>(comm.comm)->send(data, len);
  } _END_CPP(comm_send, -1);
}
int comm_send_eof(comm_t comm) {
  _BEGIN_CPP {
    if (!comm.comm)
      ygglog_throw_error("comm_send_eof: Comm is not initialized");
    return static_cast<YggInterface::communicator::Comm_t*>(comm.comm)->send_eof();
  } _END_CPP(comm_send_eof, -1);
}
long comm_recv(comm_t comm, char *data, const size_t len) {
  _BEGIN_CPP {
    if (!comm.comm)
      ygglog_throw_error("comm_recv: Comm is not initialized");
    return static_cast<YggInterface::communicator::Comm_t*>(comm.comm)->recv(data, len, false);
  } _END_CPP(comm_recv, -1);
}
long comm_recv_realloc(comm_t comm, char **data, const size_t len) {
  _BEGIN_CPP {
    if (!comm.comm)
      ygglog_throw_error("comm_recv_realloc: Comm is not initialized");
    return static_cast<YggInterface::communicator::Comm_t*>(comm.comm)->recv(data[0], len, true);
  } _END_CPP(comm_recv_realloc, -1);
}
int ncommSend(const comm_t comm, size_t nargs, ...) {
  _BEGIN_CPP {
    if (!comm.comm)
      ygglog_throw_error("ncommSend: Comm is not initialized");
    YGGC_BEGIN_VAR_ARGS(ap, nargs, nargs, false);
    YggLogDebug << "ncommSend: nargs = " << nargs << std::endl;
    int ret = static_cast<YggInterface::communicator::Comm_t*>(comm.comm)->vSend(ap);
    YGGC_END_VAR_ARGS(ap);
    return ret;
  } _END_CPP(ncommSend, -1);
}
long ncommRecv(comm_t comm, const int allow_realloc, size_t nargs, ...) {
  _BEGIN_CPP {
    if (!comm.comm)
      ygglog_throw_error("ncommRecv: Comm is not initialized");
    YGGC_BEGIN_VAR_ARGS(ap, nargs, nargs, allow_realloc);
    YggLogDebug << "ncommRecv: nargs = " << nargs << std::endl;
    long ret = static_cast<YggInterface::communicator::Comm_t*>(comm.comm)->vRecv(ap);
    YGGC_END_VAR_ARGS(ap);
    return ret;
  } _END_CPP(ncommRecv, -1);
}
long ncommCall(comm_t comm, const int allow_realloc, size_t nargs, ...) {
  _BEGIN_CPP {
    if (!comm.comm)
      ygglog_throw_error("ncommCall: Comm is not initialized");
    YGGC_BEGIN_VAR_ARGS(ap, nargs, nargs, allow_realloc);
    YggLogDebug << "ncommCall: nargs = " << nargs << std::endl;
    long ret = static_cast<YggInterface::communicator::Comm_t*>(comm.comm)->vCall(ap);
    YGGC_END_VAR_ARGS(ap);
    return ret;
  } _END_CPP(ncommCall, -1);
}

int pcommSend(const comm_t comm, const size_t nargs,
	      void** ptrs, const int for_fortran) {
  _BEGIN_CPP {
    if (!comm.comm)
      ygglog_throw_error("pcommSend: Comm is not initialized");
    rapidjson::VarArgList ap(nargs, ptrs, false, for_fortran);
    YggLogDebug << "pcommSend: nargs = " << nargs << std::endl;
    int ret = static_cast<YggInterface::communicator::Comm_t*>(comm.comm)->vSend(ap);
    YGGC_END_VAR_ARGS(ap);
    return ret;
  } _END_CPP(pcommSend, -1);
}
long pcommRecv(comm_t comm, const int allow_realloc, const size_t nargs,
	       void** ptrs, const int for_fortran) {
  _BEGIN_CPP {
    if (!comm.comm)
      ygglog_throw_error("pcommRecv: Comm is not initialized");
    rapidjson::VarArgList ap(nargs, ptrs, allow_realloc, for_fortran);
    YggLogDebug << "pcommRecv: nargs = " << nargs << std::endl;
    long ret = static_cast<YggInterface::communicator::Comm_t*>(comm.comm)->vRecv(ap);
    YGGC_END_VAR_ARGS(ap);
    return ret;
  } _END_CPP(pcommRecv, -1);
}
long pcommCall(comm_t comm, const int allow_realloc, const size_t nargs,
	       void** ptrs, const int for_fortran) {
  _BEGIN_CPP {
    if (!comm.comm)
      ygglog_throw_error("pcommCall: Comm is not initialized");
    rapidjson::VarArgList ap(nargs, ptrs, allow_realloc, for_fortran);
    YggLogDebug << "pcommCall: nargs = " << nargs << std::endl;
    long ret = static_cast<YggInterface::communicator::Comm_t*>(comm.comm)->vCall(ap);
    YGGC_END_VAR_ARGS(ap);
    return ret;
  } _END_CPP(pcommCall, -1);
}
  
int comm_nmsg(comm_t comm) {
  _BEGIN_CPP {
    if (!comm.comm)
      ygglog_throw_error("comm_nmsg: Comm is not initialized");
    return static_cast<YggInterface::communicator::Comm_t*>(comm.comm)->nmsg();
  } _END_CPP(comm_nmsg, -1);
}

void global_scope_comm_on_c() {
  YggInterface::communicator::global_scope_comm_on();
}
void global_scope_comm_off_c() {
  YggInterface::communicator::global_scope_comm_off();
}

}
