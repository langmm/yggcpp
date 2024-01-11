#include "ValueComm.hpp"

using namespace YggInterface::communicator;
using namespace YggInterface::utils;

//////////////////
// ValueManager //
//////////////////

ValueManager::ValueManager(const std::string logInst) :
  LogBase(), logInst_(logInst), value(""), count(-1), index(0) {}
ValueManager::~ValueManager() {}

int ValueManager::remaining() const {
  if (value.empty()) {
    log_error() << "remaining: Value is empty" << std::endl;
    return -1;
  }
  if (count < 0 || index == count)
    return 1;
  if (index > count)
    return 0;
  return (count - index);
}

long ValueManager::recv(utils::Header& header) {
  if (value.empty()) {
    log_error() << "recv: Value is empty" << std::endl;
    return -1;
  }
  if (count >= 0 && index >= count) {
    log_debug() << "recv: Maximum count reached" << std::endl;
    header.on_recv(YGG_MSG_EOF, YGG_MSG_EOF_LEN);
    index++;
    return -2;
  }
  index++;
  return header.on_recv(value.c_str(), value.size());
}


///////////////
// ValueComm //
///////////////

ValueComm::ValueComm(const std::string name,
		     const utils::Address &address,
		     DIRECTION direction, int flgs,
		     const COMM_TYPE type) :
  CommBase(name, address, direction, type, flgs) {
  if (!global_comm)
    init();
}

ADD_CONSTRUCTORS_DEF(ValueComm)

void ValueComm::init() {
  updateMaxMsgSize(0);
  assert(!handle);
  handle = new ValueManager(logInst());
  CommBase::init();
}

void ValueComm::_close(bool call_base) {
  if (call_base)
    CommBase::_close(true);
}

int ValueComm::comm_nmsg(DIRECTION dir) const {
  if (global_comm)
    return global_comm->comm_nmsg(dir);
  if (dir == NONE)
    dir = direction;
  if (dir != direction || dir != RECV)
    return 0;
  return handle->remaining();
}

int ValueComm::send_single(utils::Header&) {
  assert(!global_comm);
  log_error() << "send_single: Cannot send to a ValueComm" << std::endl;
  return -1;
}

long ValueComm::recv_single(utils::Header& header) {
  assert(!global_comm);
  log_debug() << "recv_single:" << std::endl;
  return handle->recv(header);
}

WORKER_METHOD_DEFS(ValueComm)
