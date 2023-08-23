#include "AsyncComm.hpp"
#include "utils/logging.hpp"

using namespace communication::communicator;
using namespace communication::utils;

//////////////////
// AsyncBacklog //
//////////////////

AsyncBacklog::AsyncBacklog(Comm_t* parent) :
  comm(nullptr), comm_mutex(), opened(false), closing(false), backlog(),
  backlog_thread(&AsyncBacklog::on_thread, this, parent) {
  while (!(opened.load() || closing.load())) {
    std::this_thread::sleep_for(std::chrono::milliseconds(1));
  }
}

AsyncBacklog::~AsyncBacklog() {
  closing.store(true);
  backlog_thread.join();
}

bool AsyncBacklog::on_thread(Comm_t* parent) {
  bool out = true;
  DIRECTION direction = parent->getDirection();
  {
    const std::lock_guard<std::mutex> comm_lock(comm_mutex);
    int flgs_comm = (parent->getFlags() & ~COMM_FLAG_ASYNC) | COMM_FLAG_ASYNC_WRAPPED;
    comm = new_Comm_t(direction,
		      parent->getCommType(),
		      parent->getName(),
		      new utils::Address(parent->getAddress()),
		      flgs_comm);
    parent->updateMaxMsgSize(comm->getMaxMsgSize());
    parent->address->address(comm->getAddress());
    parent->updateMsgBufSize(comm->getMsgBufSize());
    parent->getFlags() |= (comm->getFlags() & ~flgs_comm);
    opened.store(true);
  }
  if (direction == SEND) {
    while (!closing.load()) {
      int ret = send();
      if (ret == 0) {
	std::this_thread::sleep_for(std::chrono::microseconds(YGG_SLEEP_TIME));
      } else if (ret < 0) {
	out = false;
	break;
      }
    }
  } else if (direction == RECV) {
    while (!closing.load()) {
      long ret = recv();
      if (ret == 0) {
	std::this_thread::sleep_for(std::chrono::microseconds(YGG_SLEEP_TIME));
      } else if (ret < 0) {
	out = false;
	break;
      }
    }
  }
  {
    const std::lock_guard<std::mutex> comm_lock(comm_mutex);
    delete comm;
    comm = nullptr;
  }
  return out;
}

int AsyncBacklog::send() {
  const std::lock_guard<std::mutex> comm_lock(comm_mutex);
  int out = 0;
  if (backlog.size() > 0) {
    out = comm->send_single(backlog[0]);
    if (out >= 0) {
      ygglog_debug << "AsyncBacklog::send: Sent message from backlog" << std::endl;
      backlog.erase(backlog.begin());
    }
  }
  return out;
}

long AsyncBacklog::recv() {
  const std::lock_guard<std::mutex> comm_lock(comm_mutex);
  long out = 0;
  if (comm->comm_nmsg() > 0) {
    backlog.resize(backlog.size() + 1);
    backlog[backlog.size() - 1].reset(HEAD_RESET_OWN_DATA);
    out = comm->recv_single(backlog[backlog.size() - 1]);
    if (out >= 0) {
      ygglog_debug << "AsyncBacklog::recv: Received message into backlog" << std::endl;
    }
  }
  return out;
}


///////////////
// AsyncComm //
///////////////

AsyncComm::AsyncComm(const std::string name,
		     utils::Address *address,
		     const DIRECTION direction,
		     int flgs, const COMM_TYPE type) :
  CommBase(name, address, direction, type, flgs | COMM_FLAG_ASYNC) {
  if (!global_comm) {
    handle = new AsyncBacklog(this);
  }
}
AsyncComm::AsyncComm(const std::string nme,
		     const DIRECTION dirn,
		     int flgs, const COMM_TYPE type) :
  AsyncComm(nme, nullptr, dirn, flgs, type) {}

AsyncComm::AsyncComm(utils::Address *addr,
		     const DIRECTION dirn,
		     int flgs, const COMM_TYPE type) :
  AsyncComm("", addr, dirn, flgs, type) {}

communication::utils::Metadata& AsyncComm::getMetadata(const DIRECTION dir) {
  if (global_comm)
    return global_comm->getMetadata(dir);
  const std::lock_guard<std::mutex> lock(handle->comm_mutex);
  if (handle->comm)
    return handle->comm->getMetadata(dir);
  return metadata;
}

int AsyncComm::comm_nmsg() const {
  if (global_comm)
    return global_comm->comm_nmsg();
  const std::lock_guard<std::mutex> lock(handle->comm_mutex);
  int out = static_cast<int>(handle->backlog.size());
  // if (handle->comm)
  //   out += handle->comm->comm_nmsg();
  return out;
}

int AsyncComm::send_single(Header& header) {
  const std::lock_guard<std::mutex> lock(handle->comm_mutex);
  assert((!global_comm) && handle);
  if (type == SERVER_COMM)
    return handle->comm->send_single(header);
  ygglog_debug << "AsyncComm(" << name << ")::send_single: " << header.size_curr - header.offset << " bytes" << std::endl;
  handle->backlog.resize(handle->backlog.size() + 1);
  handle->backlog[handle->backlog.size() - 1].reset(HEAD_RESET_OWN_DATA);
  handle->backlog[handle->backlog.size() - 1].CopyFrom(header);
  return 1;
}

long AsyncComm::recv_single(Header& header) {
  const std::lock_guard<std::mutex> lock(handle->comm_mutex);
  assert((!global_comm) && handle);
  if (type == CLIENT_COMM)
    return handle->comm->recv_single(header);
  long ret = -1;
  ygglog_debug << "AsyncComm(" << name << ")::recv_single " << std::endl;
  if (handle->backlog.size() == 0) {
    ygglog_error << "AsyncComm(" << name << ")::recv_single: Backlog is empty." << std::endl;
    return ret;
  }
  ret = header.on_recv(handle->backlog[0].c_str(),
		       handle->backlog[0].size());
  if (ret < 0) {
    ygglog_error << "AsyncComm(" << name << ")::recv_single: Error copying data" << std::endl;
    return ret;
  }
  handle->backlog.erase(handle->backlog.begin());
  ygglog_debug << "AsyncComm(" << name << ")::recv_single: returns " << ret << " bytes" << std::endl;
  return ret;
}

bool AsyncComm::create_header_send(Header& header) {
  const std::lock_guard<std::mutex> lock(handle->comm_mutex);
  assert(!global_comm);
  return handle->comm->create_header_send(header);
}

bool AsyncComm::create_header_recv(Header& header) {
  const std::lock_guard<std::mutex> lock(handle->comm_mutex);
  assert(!global_comm);
  return handle->comm->create_header_recv(header);
}

Comm_t* AsyncComm::create_worker(utils::Address* address,
				 const DIRECTION& dir, int flgs) {
  return new AsyncComm("", address, dir, flgs | COMM_FLAG_WORKER, type);
}
