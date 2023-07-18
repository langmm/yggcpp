#include "comm_t.hpp"
#include "CommBase.hpp"
#include "comms.hpp"
#include "utils/logging.hpp"
#include "utils/tools.hpp"

extern "C" {
  
void free_comm(comm_t* comm) {
    if (comm && comm->comm) {
        auto c = static_cast<communication::communicator::Comm_t *>(comm->comm);
	delete c;
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
//       ret.comm = (void*) communication::communicator::new_Comm_t(dir, t, name, address);
//     } _END_CPP_CLEANUP(open_comm, ret, ret.comm = NULL);
//     return ret;
// }

comm_t init_comm(const char* name, DIRECTION dir, const COMM_TYPE &t,
		 dtype_t datatype) {
  comm_t ret;
  _BEGIN_CPP {
    ret.comm = (void*) communication::communicator::new_Comm_t(dir, t, name, (char*)NULL, COMM_FLAG_INTERFACE);
    if (!(ret.comm))
      ygglog_throw_error_c("init_comm(%s): Error initializing comm", name);
    if (datatype.metadata) {
      Metadata* metadata = static_cast<Metadata*>(datatype.metadata);
      datatype.metadata = NULL;
      static_cast<communication::communicator::Comm_t*>(ret.comm)->addSchema(*metadata);
      delete metadata;
    }
    if (!((static_cast<communication::communicator::Comm_t*>(ret.comm))->getFlags() & COMM_FLAG_VALID))
      free_comm(&ret);
  } _END_CPP_CLEANUP(init_comm, ret, ret.comm = NULL);
  return ret;
}

int set_response_format(comm_t comm, const char *fmt) {
  _BEGIN_CPP {
    if (!comm.comm)
      ygglog_throw_error_c("set_response_format: Comm is not initialized");
    COMM_TYPE ctype = static_cast<communication::communicator::Comm_t*>(comm.comm)->getType();
    if (ctype != SERVER_COMM && ctype != CLIENT_COMM)
      ygglog_throw_error_c("set_response_format: Comm is not RPC server or client");
    std::string format_str(fmt);
    static_cast<communication::communicator::RPCComm*>(comm.comm)->addResponseFormat(format_str);
  } _END_CPP(set_response_format, 0);
  return 1;
}

int set_response_datatype(comm_t x, dtype_t datatype) {
  _BEGIN_CPP {
    if (datatype.metadata) {
      Metadata* metadata = static_cast<Metadata*>(datatype.metadata);
      datatype.metadata = NULL;
      if (x.comm)
	static_cast<communication::communicator::RPCComm*>(x.comm)->addResponseSchema(*metadata);
      delete metadata;
    }
    if (!x.comm)
      ygglog_throw_error_c("set_response_datatype: Comm is not initialized");
  } _END_CPP(set_response_datatype, 0);
  return 1;
}

int comm_send(comm_t comm, const char *data, const size_t len) {
  _BEGIN_CPP {
    if (!comm.comm)
      ygglog_throw_error_c("comm_send: Comm is not initialized");
    return static_cast<communication::communicator::Comm_t*>(comm.comm)->send(data, len);
  } _END_CPP(comm_send, -1);
}
int comm_send_eof(comm_t comm) {
  _BEGIN_CPP {
    if (!comm.comm)
      ygglog_throw_error_c("comm_send_eof: Comm is not initialized");
    return static_cast<communication::communicator::Comm_t*>(comm.comm)->send_eof();
  } _END_CPP(comm_send_eof, -1);
}
long comm_recv(comm_t comm, char *data, const size_t len) {
  _BEGIN_CPP {
    if (!comm.comm)
      ygglog_throw_error_c("comm_recv: Comm is not initialized");
    return static_cast<communication::communicator::Comm_t*>(comm.comm)->recv(data, len, false);
  } _END_CPP(comm_recv, -1);
}
long comm_recv_realloc(comm_t comm, char **data, const size_t len) {
  _BEGIN_CPP {
    if (!comm.comm)
      ygglog_throw_error_c("comm_recv_realloc: Comm is not initialized");
    return static_cast<communication::communicator::Comm_t*>(comm.comm)->recv(data[0], len, true);
  } _END_CPP(comm_recv_realloc, -1);
}
int ncommSend(comm_t comm, size_t nargs, ...) {
  _BEGIN_CPP {
    if (!comm.comm)
      ygglog_throw_error_c("ncommSend: Comm is not initialized");
    YGGC_BEGIN_VAR_ARGS(ap, nargs, nargs, false);
    ygglog_debug << "ncommSend: nargs = " << nargs << std::endl;
    int ret = static_cast<communication::communicator::Comm_t*>(comm.comm)->vSend(ap);
    YGGC_END_VAR_ARGS(ap);
    return ret;
  } _END_CPP(ncommSend, -1);
}
long ncommRecv(comm_t comm, const int allow_realloc, size_t nargs, ...) {
  _BEGIN_CPP {
    if (!comm.comm)
      ygglog_throw_error_c("ncommRecv: Comm is not initialized");
    YGGC_BEGIN_VAR_ARGS(ap, nargs, nargs, allow_realloc);
    ygglog_debug << "ncommRecv: nargs = " << nargs << std::endl;
    long ret = static_cast<communication::communicator::Comm_t*>(comm.comm)->vRecv(ap);
    YGGC_END_VAR_ARGS(ap);
    return ret;
  } _END_CPP(ncommRecv, -1);
}
long ncommCall(comm_t comm, const int allow_realloc, size_t nargs, ...) {
  _BEGIN_CPP {
    if (!comm.comm)
      ygglog_throw_error_c("ncommCall: Comm is not initialized");
    YGGC_BEGIN_VAR_ARGS(ap, nargs, nargs, allow_realloc);
    ygglog_debug << "ncommCall: nargs = " << nargs << std::endl;
    long ret = static_cast<communication::communicator::Comm_t*>(comm.comm)->vCall(ap);
    YGGC_END_VAR_ARGS(ap);
    return ret;
  } _END_CPP(ncommCall, -1);
}

int pcommSend(const comm_t comm, size_t nargs,
	      void** ptrs, int for_fortran) {
  _BEGIN_CPP {
    if (!comm.comm)
      ygglog_throw_error_c("pcommSend: Comm is not initialized");
    rapidjson::VarArgList ap(nargs, ptrs, false, for_fortran);
    ygglog_debug << "pcommSend: nargs = " << nargs << std::endl;
    int ret = static_cast<communication::communicator::Comm_t*>(comm.comm)->vSend(ap);
    YGGC_END_VAR_ARGS(ap);
    return ret;
  } _END_CPP(pcommSend, -1);
}
long pcommRecv(comm_t comm, const int allow_realloc, size_t nargs, void** ptrs, int for_fortran) {
  _BEGIN_CPP {
    if (!comm.comm)
      ygglog_throw_error_c("pcommRecv: Comm is not initialized");
    rapidjson::VarArgList ap(nargs, ptrs, allow_realloc, for_fortran);
    ygglog_debug << "pcommRecv: nargs = " << nargs << std::endl;
    long ret = static_cast<communication::communicator::Comm_t*>(comm.comm)->vRecv(ap);
    YGGC_END_VAR_ARGS(ap);
    return ret;
  } _END_CPP(pcommRecv, -1);
}
long pcommCall(comm_t comm, const int allow_realloc, size_t nargs, void** ptrs, int for_fortran) {
  _BEGIN_CPP {
    if (!comm.comm)
      ygglog_throw_error_c("pcommCall: Comm is not initialized");
    rapidjson::VarArgList ap(nargs, ptrs, allow_realloc, for_fortran);
    ygglog_debug << "pcommCall: nargs = " << nargs << std::endl;
    long ret = static_cast<communication::communicator::Comm_t*>(comm.comm)->vCall(ap);
    YGGC_END_VAR_ARGS(ap);
    return ret;
  } _END_CPP(pcommCall, -1);
}
  
int comm_nmsg(comm_t comm) {
  _BEGIN_CPP {
    if (!comm.comm)
      ygglog_throw_error_c("comm_nmsg: Comm is not initialized");
    return static_cast<communication::communicator::Comm_t*>(comm.comm)->comm_nmsg();
  } _END_CPP(comm_nmsg, -1);
}

void global_scope_comm_on_c() {
  communication::communicator::global_scope_comm_on();
}
void global_scope_comm_off_c() {
  communication::communicator::global_scope_comm_off();
}

}
