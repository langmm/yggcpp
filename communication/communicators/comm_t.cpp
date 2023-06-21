#include "comm_t.hpp"
#include "CommBase.hpp"
#include "comms.hpp"
#include "utils/logging.hpp"
#include "utils/tools.hpp"

extern "C" {
  
void free_comm(comm_t* comm) {
    if (comm == nullptr)
        return;
    if (comm->comm != nullptr) {
        auto c = static_cast<communication::communicator::Comm_t *>(comm->comm);
	if (!c->global())
	  delete c;
        comm->comm = nullptr;
    }
}

void close_comm(comm_t* comm) {
    free_comm(comm);
}

comm_t open_comm(char* address, const DIRECTION dir, const COMM_TYPE &t) {
    comm_t ret;
    std::string name = "";
    try {
      ret.comm = (void*) communication::communicator::new_Comm_t(dir, t, name, address);
    } catch (...) {
      ret.comm = NULL;
    }
    return ret;
}

comm_t init_comm(const char* name, DIRECTION dir, const COMM_TYPE &t,
		 dtype_t datatype) {
  comm_t ret;
  try {
    ret.comm = (void*) communication::communicator::new_Comm_t(dir, t, name, (char*)NULL, COMM_FLAG_INTERFACE);
    if (datatype.metadata) {
      Metadata* metadata = static_cast<Metadata*>(datatype.metadata);
      datatype.metadata = NULL;
      static_cast<communication::communicator::Comm_t*>(ret.comm)->addSchema(*metadata);
      delete metadata;
    }
  } catch (...) {
    ret.comm = NULL;
  }
  return ret;
}

int set_response_format(comm_t comm, const char *fmt) {
  if (!comm.comm)
    return 0;
  try {
    std::string format_str(fmt);
    static_cast<communication::communicator::RPCComm*>(comm.comm)->addResponseFormat(format_str);
  } catch (...) {
    return 0;
  }
  return 1;
}

int set_response_datatype(comm_t x, dtype_t datatype) {
  if (!x.comm) return 0;
  try {
    if (datatype.metadata) {
      Metadata* metadata = static_cast<Metadata*>(datatype.metadata);
      datatype.metadata = NULL;
      static_cast<communication::communicator::RPCComm*>(x.comm)->addResponseSchema(*metadata);
      delete metadata;
    }
  } catch (...) {
    return 0;
  }
  return 1;
}

int comm_send(comm_t comm, const char *data, const size_t len) {
  if (!comm.comm)
    return -1;
  try {
    return static_cast<communication::communicator::Comm_t*>(comm.comm)->send(data, len);
  } catch (...) {
    return -1;
  }
}
int comm_send_eof(comm_t comm) {
  if (!comm.comm)
    return -1;
  try {
    return static_cast<communication::communicator::Comm_t*>(comm.comm)->send_eof();
  } catch (...) {
    return -1;
  }
}
long comm_recv(comm_t comm, char *data, const size_t len) {
  if (!comm.comm)
    return -1;
  try {
    return static_cast<communication::communicator::Comm_t*>(comm.comm)->recv(data, len, false);
  } catch (...) {
    return -1;
  }
}
long comm_recv_realloc(comm_t comm, char **data, const size_t len) {
  if (!comm.comm)
    return -1;
  try {
    return static_cast<communication::communicator::Comm_t*>(comm.comm)->recv(data[0], len, true);
  } catch (...) {
    return -1;
  }
}
int ncommSend(comm_t comm, size_t nargs, ...) {
  if (!comm.comm)
    return -1;
  try {
    YGGC_BEGIN_VAR_ARGS(ap, nargs, nargs, false);
    ygglog_debug << "ncommSend: nargs = " << nargs << std::endl;
    int ret = static_cast<communication::communicator::Comm_t*>(comm.comm)->vSend(ap);
    YGGC_END_VAR_ARGS(ap);
    return ret;
  } catch (...) {
    return -1;
  }
}
long ncommRecv(comm_t comm, const int allow_realloc, size_t nargs, ...) {
  if (!comm.comm)
    return -1;
  try {
    YGGC_BEGIN_VAR_ARGS(ap, nargs, nargs, allow_realloc);
    ygglog_debug << "ncommRecv: nargs = " << nargs << std::endl;
    long ret = static_cast<communication::communicator::Comm_t*>(comm.comm)->vRecv(ap);
    YGGC_END_VAR_ARGS(ap);
    return ret;
  } catch (...) {
    return -1;
  }
}
long ncommCall(comm_t comm, const int allow_realloc, size_t nargs, ...) {
  if (!comm.comm)
    return -1;
  try {
    YGGC_BEGIN_VAR_ARGS(ap, nargs, nargs, allow_realloc);
    ygglog_debug << "ncommCall: nargs = " << nargs << std::endl;
    long ret = static_cast<communication::communicator::Comm_t*>(comm.comm)->vCall(ap);
    YGGC_END_VAR_ARGS(ap);
    return ret;
  } catch (...) {
    return -1;
  }
}

int comm_nmsg(comm_t comm) {
    if (!comm.comm)
        return -1;
    try {
      return static_cast<communication::communicator::Comm_t*>(comm.comm)->comm_nmsg();
    } catch (...) {
      return -1;
    }
}

/*!
  @brief Retrieve a registered global comm if it exists.
  @param[in] name Name that comm might be registered under.
  @param[in] dir Direction for comm.
  @param[in] t Communicator type.
  @returns Registered comm. Member comm willl be NULL if one does not
    exist with the specified name.
 */
  comm_t get_global_scope_comm(const char *name, const DIRECTION dir,
			       const COMM_TYPE &t) {
  comm_t out;
  out.comm = (void*)(communication::communicator::Comm_t::find_registered_comm(name, dir, t));
  return out;
};
void global_scope_comm_on() {
  communication::communicator::global_scope_comm = 1;
}
void global_scope_comm_off() {
// #ifndef _OPENMP
  communication::communicator::global_scope_comm = 0;
// #endif
}

}
