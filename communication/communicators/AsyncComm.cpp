#include "AsyncComm.hpp"
#include "ClientComm.hpp"
#include "utils/logging.hpp"

using namespace communication::communicator;
using namespace communication::utils;

//////////////////
// AsyncBacklog //
//////////////////

// TODO: Preserve backlog buffers?
// TODO: Check for handle->comm before use?

#ifdef THREADSINSTALLED

AsyncBacklog::AsyncBacklog(Comm_t* parent) :
  comm(nullptr), backlog(), comm_mutex(),
  opened(false), closing(false), locked(false), complete(false),
  result(false), backlog_thread(&AsyncBacklog::on_thread, this, parent),
  logInst_(parent->logInst()) {
  while (!(opened.load() || closing.load())) {
    std::this_thread::sleep_for(std::chrono::milliseconds(1));
  }
}

#else // THREADSINSTALLED

AsyncBacklog::AsyncBacklog(Comm_t*) :
  comm(nullptr), backlog() {
  UNINSTALLED_ERROR(THREADS);
}

#endif // THREADSINSTALLED

AsyncBacklog::~AsyncBacklog() {
  log_debug() << "~AsyncBacklog: begin" << std::endl;
#ifdef THREADSINSTALLED
  closing.store(true);
  while (!complete.load()) { std::this_thread::yield(); }
  try {
    if (backlog_thread.joinable())
      backlog_thread.join();
    log_debug() << "~AsyncBacklog: joinable = " << backlog_thread.joinable() << std::endl;
  } catch (const std::system_error& e) {
    log_error() << "~AsyncBacklog: Error joining thread (" << e.code() << "): " << e.what() << std::endl;
  }
  if (!result.load()) {
    log_error() << "~AsyncBacklog: Error on thread" << std::endl;
  }
#endif // THREADSINSTALLED
  log_debug() << "~AsyncBacklog: end" << std::endl;
}

void AsyncBacklog::on_thread(Comm_t* parent) {
  bool out = true;
  try {
#ifdef THREADSINSTALLED
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
    closing.store(true);
    {
      const std::lock_guard<std::mutex> comm_lock(comm_mutex);
      delete comm;
      comm = nullptr;
    }
#else // THREADSINSTALLED
    UNUSED(parent);
#endif // THREADSINSTALLED
  } catch (...) {
    closing.store(true);
    out = false;
  }
  result.store(out);
  complete.store(true);
}

int AsyncBacklog::send() {
  int out = 0;
#ifdef THREADSINSTALLED
  const std::lock_guard<std::mutex> comm_lock(comm_mutex);
  if (backlog.size() > 0) {
    if (comm->getType() == CLIENT_COMM) {
      ClientComm* cli = dynamic_cast<ClientComm*>(comm);
      if (!cli->signon(backlog[0], comm))
	return -1;
      // Sleep outside lock
      if ((!(backlog[0].flags & (HEAD_FLAG_CLIENT_SIGNON | HEAD_FLAG_EOF)))
	  && !cli->signonComplete())
	return out;
    }
    out = comm->send_single(backlog[0]);
    if (out >= 0) {
      log_debug() << "send: Sent message from backlog" << std::endl;
      backlog.erase(backlog.begin());
    }
  }
#endif // THREADSINSTALLED
  return out;
}

long AsyncBacklog::recv() {
  long out = 0;
#ifdef THREADSINSTALLED
  const std::lock_guard<std::mutex> comm_lock(comm_mutex);
  if (comm->comm_nmsg() > 0) {
    backlog.emplace_back(true);
    out = comm->recv_single(backlog[backlog.size() - 1]);
    if (out >= 0) {
      if (backlog[backlog.size() - 1].flags & HEAD_FLAG_REPEAT) {
	backlog.resize(backlog.size() - 1);
      } else {
	log_debug() << "recv: Received message into backlog" << std::endl;
      }
    }
  }
#endif // THREADSINSTALLED
  return out;
}

////////////////////
// AsyncLockGuard //
////////////////////

AsyncLockGuard::AsyncLockGuard(AsyncBacklog* bcklog, bool dont_lock) :
  locked(false), backlog(bcklog) {
#ifdef THREADSINSTALLED
  if (!(dont_lock || backlog->locked.load())) {
    locked = true;
    backlog->comm_mutex.lock();
    backlog->locked.store(true);
  }
#else // THREADSINSTALLED
  UNUSED(dont_lock);
#endif // THREADSINSTALLED
}
AsyncLockGuard::~AsyncLockGuard() {
#ifdef THREADSINSTALLED
  if (locked) {
    backlog->locked.store(false);
    backlog->comm_mutex.unlock();
  }
#endif // THREADSINSTALLED
}


///////////////
// AsyncComm //
///////////////

AsyncComm::AsyncComm(const std::string name,
		     utils::Address *address,
		     const DIRECTION direction,
		     int flgs, const COMM_TYPE type) :
  CommBase(name, address, direction, type, flgs | COMM_FLAG_ASYNC) {
  if (type == SERVER_COMM)
    this->direction = RECV;
  else if (type == CLIENT_COMM)
    this->direction = SEND;
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

int AsyncComm::comm_nmsg(DIRECTION dir) const {
  if (global_comm)
    return global_comm->comm_nmsg(dir);
  const AsyncLockGuard lock(handle);
  if (dir == NONE)
    dir = direction;
  if (handle->is_closing()) {
    log_error() << "comm_nmsg: Thread is closing" << std::endl;
    return -1;
  }
  if ((type == CLIENT_COMM && dir == RECV) ||
      (type == SERVER_COMM && dir == SEND)) {
    return handle->comm->comm_nmsg(dir);
  }
  return static_cast<int>(handle->backlog.size());
}

communication::utils::Metadata& AsyncComm::getMetadata(const DIRECTION dir) {
  if (global_comm)
    return global_comm->getMetadata(dir);
  const AsyncLockGuard lock(handle);
  if (handle->comm)
    return handle->comm->getMetadata(dir);
  return metadata;
}

void AsyncComm::set_timeout_recv(int64_t new_timeout) {
  if (global_comm) {
    global_comm->set_timeout_recv(new_timeout);
    return;
  }
  const AsyncLockGuard lock(handle);
  if (handle->comm)
    handle->comm->set_timeout_recv(new_timeout);
  CommBase::set_timeout_recv(new_timeout);
}
int64_t AsyncComm::get_timeout_recv() {
  if (global_comm) {
    return global_comm->get_timeout_recv();
  }
  const AsyncLockGuard lock(handle);
  if (handle->comm)
    return handle->comm->get_timeout_recv();
  return CommBase::get_timeout_recv();
}

std::string AsyncComm::logClass() const {
  std::string out = CommBase::logClass();
  out += "[ASYNC]";
  return out;
}

int AsyncComm::send_single(Header& header) {
  const AsyncLockGuard lock(handle);
  assert((!global_comm) && handle);
  if (handle->is_closing()) {
    log_error() << "send_single: Thread is closing" << std::endl;
    return -1;
  }
  if (type == SERVER_COMM) {
    return handle->comm->send_single(header);
  }
  if (header.on_send() < 0)
    return -1;
  log_debug() << "send_single: " << header.size_msg << " bytes" << std::endl;
  handle->backlog.emplace_back(true);
  if (!handle->backlog[handle->backlog.size() - 1].CopyFrom(header))
    return -1;
  handle->backlog[handle->backlog.size() - 1].flags |= HEAD_FLAG_ASYNC;
  return static_cast<int>(header.size_msg);
}

long AsyncComm::recv_single(Header& header) {
  const AsyncLockGuard lock(handle);
  assert((!global_comm) && handle);
  if (handle->is_closing()) {
    log_error() << "recv_single: Thread is closing" << std::endl;
    return -1;
  }
  if (type == CLIENT_COMM) {
    return handle->comm->recv_single(header);
  }
  long ret = -1;
  log_debug() << "recv_single " << std::endl;
  if (handle->backlog.size() == 0) {
    log_error() << "recv_single: Backlog is empty." << std::endl;
    return ret;
  }
  if (!header.MoveFrom(handle->backlog[0])) {
    log_error() << "recv_single: Error copying data" << std::endl;
    return ret;
  }
  ret = static_cast<long>(header.size_data);
  handle->backlog.erase(handle->backlog.begin());
  log_debug() << "recv_single: returns " << ret << " bytes" << std::endl;
  return ret;
}

bool AsyncComm::create_header_send(Header& header) {
  const AsyncLockGuard lock(handle);
  assert(!global_comm);
  if (handle->is_closing()) {
    log_error() << "create_header_send: Thread is closing" << std::endl;
    return false;
  }
  if (type == CLIENT_COMM &&
      !(header.flags & (HEAD_FLAG_EOF | HEAD_FLAG_CLIENT_SIGNON))) {
    if (!dynamic_cast<ClientComm*>(handle->comm)->signon(header, this))
      return false;
  }
  return handle->comm->create_header_send(header);
}

Comm_t* AsyncComm::create_worker(utils::Address* address,
				 const DIRECTION& dir, int flgs) {
  return new AsyncComm("", address, dir, flgs | COMM_FLAG_WORKER, type);
}
