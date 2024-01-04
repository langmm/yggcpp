#include "WrapComm.hpp"

using namespace YggInterface::communicator;
using namespace YggInterface::utils;

#define IN_CONTEXT_NORET(body, err)		\
  if (checkWrapped()) {				\
    handle->body;				\
  } else {					\
    err;					\
  }
#define IN_CONTEXT(type, body, err)		\
  type out;					\
  if (checkWrapped()) {				\
    out = handle->body;				\
  } else {					\
    err;					\
  }						\
  return out
#define IN_CONTEXT_DIRECT(type, body, err)	\
  if (!checkWrapped()) {			\
    err;					\
  }						\
  return handle->body
#define WRAP_METHOD_NORET(name, argsT, args, err, mods)	\
  void WrapComm::name argsT mods {			\
    IN_CONTEXT_NORET(name args, err);			\
  }
#define WRAP_METHOD(name, argsT, args, err, type, mods)	\
  type WrapComm::name argsT mods {			\
    IN_CONTEXT(type, name args, err);			\
  }
#define WRAP_METHOD_DIRECT(name, argsT, args, err, type, mods)	\
  type WrapComm::name argsT mods {				\
    IN_CONTEXT_DIRECT(type, name args, err);			\
  }

WrapComm::WrapComm(const std::string name,
		   const utils::Address &address,
		   const DIRECTION direction,
		   int flgs, const COMM_TYPE type, size_t ncomm,
		   const COMM_TYPE wraptyp, bool delay_init) :
  CommBase(name, address, direction, type, flgs | COMM_FLAG_WRAPPER),
  wraptype(wraptyp), wrapncomm(ncomm) {
  if (wraptype == NULL_COMM)
    wraptype = type;
  if (!(delay_init || global_comm))
    init();
}
WrapComm::WrapComm(const std::string nme,
		   const DIRECTION dirn, int flgs,
		   const COMM_TYPE type, size_t ncomm,
		   const COMM_TYPE wraptype) :
  WrapComm(nme, utils::blankAddress, dirn, flgs, type,
	   ncomm, wraptype) {}
WrapComm::WrapComm(const utils::Address &addr,
		   const DIRECTION dirn, int flgs,
		   const COMM_TYPE type, size_t ncomm,
		   const COMM_TYPE wraptype) :
  WrapComm("", addr, dirn, flgs, type, ncomm, wraptype) {}
WrapComm::WrapComm(Comm_t* comm) :
  WrapComm(comm->getName(), utils::Address(comm->getAddress()),
	   comm->getDirection(), comm->getFlags(),
	   comm->getType(), 0, NULL_COMM, true) {
  handle = comm;
  fromComm();
}

// std::string WrapComm::logClass() const {
//   std::string out = CommBase::logClass();
//   out += "[WRAPPER]";
//   return out;
// }

void WrapComm::init() {
  handle = new_Comm_t(getDirection(), wraptype, getName(),
		      utils::Address(getAddress()),
		      getFlags(), wrapncomm);
  fromComm();
  CommBase::init();
}
void WrapComm::fromComm() {
  if (handle) {
    if (type == DEFAULT_COMM)
      setType(handle->getType());
    else
      handle->setType(type);
    this->name = handle->getName();
    this->direction = handle->getDirection();
    set_timeout_recv(handle->get_timeout_recv());
    updateMaxMsgSize(handle->getMaxMsgSize());
    address.address(handle->getAddress());
    updateMsgBufSize(handle->getMsgBufSize());
    getFlags() |= handle->getFlags();
  }
}

bool WrapComm::checkWrapped() const {
  return (handle != nullptr);
}

void WrapComm::_close(bool call_base) {
  if (this->handle)
    this->handle->close();
  if (call_base)
    CommBase::_close(true);
}

// public methods
WRAP_METHOD(comm_nmsg, (DIRECTION dir), (dir), out = -1, int, const)
WRAP_METHOD_DIRECT(getMetadata, (const DIRECTION dir), (dir),
		   THROW_NO_HANDLE(getMetadata),
		   YggInterface::utils::Metadata&, )
WRAP_METHOD_NORET(set_timeout_recv, (int64_t new_timeout),
		  (new_timeout),
		  THROW_NO_HANDLE(set_timeout_recv), )
WRAP_METHOD(get_timeout_recv, (), (),
	    out = CommBase::get_timeout_recv(), int64_t, )
WRAP_METHOD(wait_for_recv, (const int64_t& tout), (tout),
	    out = -1, int, )
WRAP_METHOD_NORET(close, (), (), , )
WRAP_METHOD(is_closed, (), (), out = false, bool, const)
WRAP_METHOD(is_open, (), (), out = false, bool, const)
WRAP_METHOD_DIRECT(getWorkers, (), (), THROW_NO_HANDLE(getWorkers), WorkerList&, );

int WrapComm::send_raw(const char *data, const size_t &len) {
  if ((this->flags & COMM_FLAG_FORK_CYCLE) ||
      (this->flags & COMM_FLAG_FORK_BROADCAST) ||
      (this->flags & COMM_FLAG_FORK_COMPOSITE)) {
    IN_CONTEXT(int, send_raw(data, len), out = -1);
  }
  return CommBase::send_raw(data, len);
}
long WrapComm::recv_raw(char*& data, const size_t &len,
			bool allow_realloc) {
  if ((this->flags & COMM_FLAG_FORK_CYCLE) ||
      (this->flags & COMM_FLAG_FORK_BROADCAST) ||
      (this->flags & COMM_FLAG_FORK_COMPOSITE)) {
    IN_CONTEXT(long, recv_raw(data, len, allow_realloc), out = -1);
  }
  return CommBase::recv_raw(data, len, allow_realloc);
}

// test methods
WRAP_METHOD(afterSendRecv, (Comm_t* sComm, Comm_t* rComm),
	    (sComm, rComm), out = false, bool, )
WRAP_METHOD(genMetadata, (std::string& meta), (meta),
	    out = false, bool, )

// protected methods
WRAP_METHOD(send_single, (utils::Header& header),
	    (header), out = -1, int, )
WRAP_METHOD(recv_single, (utils::Header& header),
	    (header), out = -1, long, )
WRAP_METHOD(create_header_send, (utils::Header& header),
	    (header), out = false, bool, )
WRAP_METHOD(create_worker,
	    (utils::Address& address,
	     const DIRECTION dir, int flgs),
	    (address, dir, flgs), out = nullptr, Comm_t*, )
WRAP_METHOD(create_worker_send, (utils::Header& header),
	    (header), out = nullptr, Comm_t*, )
WRAP_METHOD(create_worker_recv, (utils::Header& header),
	    (header), out = nullptr, Comm_t*, )
