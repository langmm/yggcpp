#include "AsyncComm.hpp"
#include "utils/logging.hpp"

using namespace communication::communicator;
using namespace communication::utils;

//////////////
// AsyncMsg //
//////////////

AsyncMsg::AsyncMsg(const char* data, const size_t &len) :
  msg(data, len), head() {}
AsyncMsg::AsyncMsg(const char* data, const size_t &len,
		   const utils::Header& header) :
  msg(data, len), head() {
  head.fromMetadata(header);
  head.flags = header.flags;
}
AsyncMsg::AsyncMsg(const AsyncMsg&& rhs) :
  msg(rhs.msg), head() {
  head.fromMetadata(rhs.head);
  head.flags = rhs.head.flags;
}
AsyncMsg& AsyncMsg::operator=(AsyncMsg&& rhs) {
  this->~AsyncMsg();
  new (this) AsyncMsg(std::move(rhs));
  return *this;
}

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
    int flgs_comm = parent->getFlags() & ~COMM_FLAG_ASYNC;
    comm = new_Comm_t(direction,
		      parent->getCommType(),
		      parent->getName(),
		      new utils::Address(parent->getAddress()),
		      flgs_comm);
    parent->updateMaxMsgSize(comm->getMaxMsgSize());
    parent->address->address(comm->getAddress());
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
    out = comm->send_single(backlog[0].msg.c_str(),
			    backlog[0].msg.size(),
			    backlog[0].head);
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
    char* data = NULL;
    size_t len = 0;
    out = comm->recv_single(data, len, true);
    if (out >= 0) {
      ygglog_debug << "AsyncBacklog::recv: Received message into backlog" << std::endl;
      backlog.emplace_back(data, static_cast<size_t>(out));
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

int AsyncComm::comm_nmsg() const {
  if (global_comm)
    return global_comm->comm_nmsg();
  const std::lock_guard<std::mutex> lock(handle->comm_mutex);
  int out = static_cast<int>(handle->backlog.size());
  // if (handle->comm)
  //   out += handle->comm->comm_nmsg();
  return out;
}

int AsyncComm::send_single(const char* data, const size_t &len,
			   const Header& header) {
  // Should never be called with global comm
  // if (global_comm)
  //   return global_comm->send_single(data, len, header);
  const std::lock_guard<std::mutex> lock(handle->comm_mutex);
  assert((!global_comm) && handle);
  ygglog_debug << "AsyncComm(" << name << ")::send_single: " << len << " bytes" << std::endl;
  handle->backlog.emplace_back(data, len, header);
  return 1;
}

long AsyncComm::recv_single(char*& data, const size_t &len,
			    bool allow_realloc) {
  // Should never be called with global comm
  // if (global_comm)
  //   return global_comm->recv_single(data, len, allow_realloc);
  const std::lock_guard<std::mutex> lock(handle->comm_mutex);
  assert((!global_comm) && handle);
  long ret = -1;
  ygglog_debug << "AsyncComm(" << name << ")::recv_single " << std::endl;
  if (handle->backlog.size() == 0) {
    ygglog_error << "AsyncComm(" << name << ")::recv_single: Backlog is empty." << std::endl;
    return ret;
  }
  std::string msg = handle->backlog[0].msg;
  ret = this->copyData(data, len, msg.c_str(), msg.size(), allow_realloc);
  if (ret < 0) {
    ygglog_error << "AsyncComm(" << name << ")::recv_single: Error copying data" << std::endl;
    return ret;
  }
  handle->backlog.erase(handle->backlog.begin());
  ygglog_debug << "AsyncComm(" << name << ")::recv_single: returns " << ret << " bytes" << std::endl;
  return ret;
}

bool AsyncComm::create_header_send(Header& header, const char* data, const size_t &len) {
  // Should never be called with global comm
  // if (global_comm)
  //   return global_comm->create_header_send(header, data, len);
  const std::lock_guard<std::mutex> lock(handle->comm_mutex);
  assert(!global_comm);
  return handle->comm->create_header_send(header, data, len);
}

bool AsyncComm::create_header_recv(Header& header, char*& data,
				   const size_t &len,
				   size_t msg_len, int allow_realloc,
				   int temp) {
  // Should never be called with global comm
  // if (global_comm)
  //   return global_comm->create_header_recv(header, data, len, msg_len,
  // 					   allow_realloc, temp);
  const std::lock_guard<std::mutex> lock(handle->comm_mutex);
  assert(!global_comm);
  return handle->comm->create_header_recv(header, data, len, msg_len,
					  allow_realloc, temp);
}

Comm_t* AsyncComm::create_worker(utils::Address* address,
				 const DIRECTION& dir, int flgs) {
  return new AsyncComm("", address, dir, flgs | COMM_FLAG_WORKER, type);
}
