#include "AsyncComm.hpp"
#include "ClientComm.hpp"
#include "utils/logging.hpp"

using namespace communication::communicator;
using namespace communication::utils;

#ifdef THREADSINSTALLED
#define LOCK_BUFFER(name)					\
  log_verbose() << #name << ": Before lock" << std::endl;	\
  const std::lock_guard<std::mutex> lk(m);			\
  log_verbose() << #name << ": After lock" << std::endl
#else
#define LOCK_BUFFER(name)
#endif // THREADSINSTALLED

/////////////////
// AsyncBuffer //
/////////////////

#ifdef THREADSINSTALLED
AsyncBuffer::AsyncBuffer(const std::string logInst) :
  buffer(), closed(false), m(), cv(), logInst_(logInst) {}
void AsyncBuffer::close() {
  {
    LOCK_BUFFER(close);
    closed.store(true);
  }
  cv.notify_all();
}
bool AsyncBuffer::is_closed() const { return closed.load(); }
#else // THREADSINSTALLED
AsyncBuffer::AsyncBuffer(const std::string logInst) :
  buffer(), logInst_(logInst) {}
void AsyncBuffer::close() {}
bool AsyncBuffer::is_closed() const { return true; }
#endif // THREADSINSTALLED

size_t AsyncBuffer::size() {
  LOCK_BUFFER(size);
  if (is_closed())
    return 0;
  return buffer.size();
}
bool AsyncBuffer::insert(utils::Header& header, size_t idx,
			 bool move, bool dont_notify, bool for_send) {
  {
    LOCK_BUFFER(insert);
    if (idx >= buffer.size()) {
      idx = buffer.size();
    }
    if (is_closed()) {
      log_error() << "insert[" << idx << "]: " <<
	"Buffer is closed" <<
	" (send = " << for_send << ")" << std::endl;
      return false;
    }
    if (idx >= buffer.size()) {
      buffer.emplace_back(true);
    } else {
      buffer.emplace(buffer.begin() + idx, true);
    }
    bool res = false;
    if (move)
      res = buffer[idx].MoveFrom(header);
    else
      res = buffer[idx].CopyFrom(header);
    if (!res) {
      log_error() << "insert[" << idx << "]: " <<
	"Error adding message to backlog" <<
	" (send = " << for_send << ")" << std::endl;
      return false;
    }
    if (for_send)
      buffer[idx].flags |= HEAD_FLAG_ASYNC;
  }
#ifdef THREADSINSTALLED
  if (!dont_notify) {
      log_debug() << "insert[" << idx << "]: " <<
	"Notifying threads of message" <<
	" (send = " << for_send << ")" << std::endl;
    cv.notify_all();
  }
#endif // THREADSINSTALLED
  return true;
}
bool AsyncBuffer::append(utils::Header& header, bool move,
			 bool dont_notify, bool for_send) {
  return insert(header, buffer.size() + 10, move, dont_notify, for_send);
}
bool AsyncBuffer::prepend(utils::Header& header, bool move,
			  bool dont_notify, bool for_send) {
  return insert(header, 0, move, dont_notify, for_send);
}

bool AsyncBuffer::get(utils::Header& header, size_t idx,
		      bool move, bool erase) {
  LOCK_BUFFER(get);
  if (is_closed())
    return false;
  if (idx >= buffer.size())
    return false;
  bool out = false;
  if (move)
    out = header.MoveFrom(buffer[idx]);
  else
    out = header.CopyFrom(buffer[idx]);
  if (out && erase)
    buffer.erase(buffer.begin() + idx);
  return out;
}
bool AsyncBuffer::pop(utils::Header& header, size_t idx) {
  return get(header, idx, true, true);
}
#ifdef THREADSINSTALLED
bool AsyncBuffer::message_waiting() {
  return (is_closed() || buffer.size() > 0);
}
bool AsyncBuffer::wait() {
  std::unique_lock<std::mutex> lk(m);
  AsyncBuffer* _this = this;
  cv.wait(lk, [_this]{
    return _this->message_waiting(); });
  return true;
}
#endif // THREADSINSTALLED

//////////////////
// AsyncBacklog //
//////////////////

// TODO: Preserve backlog buffers?

#ifdef THREADSINSTALLED

AsyncBacklog::AsyncBacklog(Comm_t* parent) :
  comm(nullptr), backlog(parent->logInst()), comm_mutex(),
  locked(false), signon_sent(false),
  backlog_thread(&AsyncBacklog::on_thread, this, parent),
  status(THREAD_INACTIVE), cv_status(),
  logInst_(parent->logInst()) {
  wait_for_status(THREAD_STARTED | THREAD_COMPLETE);
}

#else // THREADSINSTALLED

AsyncBacklog::AsyncBacklog(Comm_t* parent) :
  comm(nullptr), backlog(parent->logInst()), logInst_(parent->logInst()) {
  UNINSTALLED_ERROR(THREADS);
}

#endif // THREADSINSTALLED

AsyncBacklog::~AsyncBacklog() {
  log_debug() << "~AsyncBacklog: begin" << std::endl;
#ifdef THREADSINSTALLED
  backlog.close();
  wait_for_status(THREAD_COMPLETE);
  try {
    if (backlog_thread.joinable())
      backlog_thread.join();
    log_debug() << "~AsyncBacklog: joinable = " << backlog_thread.joinable() << std::endl;
  } catch (const std::system_error& e) {
    log_error() << "~AsyncBacklog: Error joining thread (" << e.code() << "): " << e.what() << std::endl;
  }
  if (status.load() & THREAD_ERROR) {
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
      set_status(THREAD_STARTED);
    }
    if (direction == SEND) {
      while (!backlog.is_closed()) {
	cv_status.notify_all(); // Periodically notify
	int ret = send();
	if (ret == 0) {
	  backlog.wait();
	} else if (ret < 0) {
	  out = false;
	  break;
	}
      }
    } else if (direction == RECV) {
      while (!backlog.is_closed()) {
	cv_status.notify_all(); // Periodically notify
	long ret = recv();
	if (ret == 0) {
	  std::this_thread::sleep_for(std::chrono::microseconds(YGG_SLEEP_TIME));
	} else if (ret < 0) {
	  out = false;
	  break;
	}
      }
    }
    backlog.close();
    {
      const std::lock_guard<std::mutex> comm_lock(comm_mutex);
      delete comm;
      comm = nullptr;
    }
#else // THREADSINSTALLED
    UNUSED(parent);
#endif // THREADSINSTALLED
  } catch (...) {
    backlog.close();
    out = false;
  }
#ifdef THREADSINSTALLED
  if (!out)
    set_status(THREAD_ERROR);
  set_status(THREAD_COMPLETE);
#else // THREADSINSTALLED
  UNUSED(out);
#endif // THREADSINSTALLED
}

#ifdef THREADSINSTALLED
void AsyncBacklog::set_status(const int new_status) {
  status |= new_status;
  cv_status.notify_all();
}
bool AsyncBacklog::wait_for_status(const int new_status) {
  if (!(status.load() & new_status)) {
    std::unique_lock<std::mutex> lk(comm_mutex);
    AsyncBacklog* _this = this;
    cv_status.wait(lk, [_this, new_status]{
      return (_this->status.load() & new_status); });
  }
  return true;
}
#endif // THREADSINSTALLED

int AsyncBacklog::signon_status() {
  if (is_closing())
    return SIGNON_ERROR;
#ifdef THREADSINSTALLED
  const std::lock_guard<std::mutex> comm_lock(comm_mutex);
  if (comm->getType() != CLIENT_COMM)
    return SIGNON_COMPLETE;
  ClientComm* cli = dynamic_cast<ClientComm*>(comm);
  RequestList& requests = cli->getRequests();
  if (!signon_sent.load())
    return SIGNON_NOT_SENT;
  if (requests.signon_complete)
    return SIGNON_COMPLETE;
  if ((!requests.requests.empty()) &&
      requests.activeComm()->comm_nmsg(RECV) > 0)
    return SIGNON_WAITING;
  return SIGNON_NOT_WAITING;
#else // SIGNON_NOT_WAITING
  return SIGNON_ERROR;
#endif // THREADSINSTALLED
}

bool AsyncBacklog::wait_for_signon() {
#ifdef THREADSINSTALLED
  int status = SIGNON_NOT_SENT;
  int iloop = 0;
  int interval = 20;
  while (true) {
    status = signon_status();
    if (status == SIGNON_ERROR)
      return false;
    if (status == SIGNON_NOT_SENT ||
	status == SIGNON_COMPLETE)
      return true;
    {
      const std::lock_guard<std::mutex> comm_lock(comm_mutex);
      ClientComm* cli = dynamic_cast<ClientComm*>(comm);
      if (status == SIGNON_WAITING) {
	log_debug() << "wait_for_signon: Sign-on after " <<
	  iloop << " loops (" << (iloop / interval) + 1 <<
	  " messages sent)" << std::endl;
	return cli->signon();
      }
      if (!cli->send_signon(iloop, interval, comm))
	return false;
    }
    std::this_thread::sleep_for(std::chrono::microseconds(10*YGG_SLEEP_TIME));
    iloop++;
  }
#endif // THREADSINSTALLED
  return true;
}

int AsyncBacklog::send() {
  int out = 0;
#ifdef THREADSINSTALLED
  if (!wait_for_signon()) {
    log_error() << "send: Error in async client signon" << std::endl;
    return -1;
  }
  utils::Header header(true);
  if (backlog.pop(header)) {
    const std::lock_guard<std::mutex> comm_lock(comm_mutex);
    out = comm->send_single(header);
    if (out >= 0) {
      log_debug() << "send: Sent message from backlog" << std::endl;
      if (header.flags & HEAD_FLAG_CLIENT_SIGNON) {
	signon_sent.store(true);
	// Sleep for a bit on client open to prevent sending too many
	//   signon messages.
	// std::this_thread::sleep_for(std::chrono::microseconds(100*YGG_SLEEP_TIME));
      }
    } else {
      log_debug() << "send: Sending from backlog failed. " <<
	"Another attempt will be made." << std::endl;
      backlog.prepend(header, true, true, true);
    }
  }
#endif // THREADSINSTALLED
  return out;
}

long AsyncBacklog::recv() {
  long out = 0;
#ifdef THREADSINSTALLED
  bool received = false;
  utils::Header header(true);
  {
    const std::lock_guard<std::mutex> comm_lock(comm_mutex);
    if (comm->comm_nmsg() > 0) {
      out = comm->recv_single(header);
      received = true;
    }
  }
  if (received && out >= 0) {
    if (!(header.flags & HEAD_FLAG_REPEAT)) {
      log_debug() << "recv: Received message into backlog: " << out << std::endl;
      if (!backlog.append(header, true))
	out = -1;
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
  if (dir == NONE)
    dir = direction;
  if (handle->is_closing()) {
    log_error() << "comm_nmsg: Thread is closing" << std::endl;
    return -1;
  }
  if ((type == CLIENT_COMM && dir == RECV) ||
      (type == SERVER_COMM && dir == SEND)) {
    const AsyncLockGuard lock(handle);
    return handle->comm->comm_nmsg(dir);
  }
  return static_cast<int>(handle->backlog.size());
}

#ifdef THREADSINSTALLED
int AsyncComm::wait_for_recv(const int64_t& tout) {
  if (global_comm || type == CLIENT_COMM)
    return CommBase::wait_for_recv(tout);
  log_debug() << "wait_for_recv: timeout = " << tout <<
    " microseconds" << std::endl;
  if (!handle->backlog.wait_for(std::chrono::microseconds(tout)))
    return 0;
  return comm_nmsg(RECV);
}
#endif // THREADSINSTALLED

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
  assert((!global_comm) && handle);
  if (type == SERVER_COMM) {
    const AsyncLockGuard lock(handle);
    if (handle->is_closing()) {
      log_error() << "send_single: Thread is closing" << std::endl;
      return -1;
    }
    return handle->comm->send_single(header);
  }
  if (header.on_send() < 0)
    return -1;
  log_debug() << "send_single: " << header.size_msg << " bytes" << std::endl;
  if (!handle->backlog.append(header, false, false, true))
    return -1;
  return static_cast<int>(header.size_msg);
}

long AsyncComm::recv_single(Header& header) {
  if (type == CLIENT_COMM) {
    const AsyncLockGuard lock(handle);
    assert((!global_comm) && handle);
    if (handle->is_closing()) {
      log_error() << "recv_single: Thread is closing" << std::endl;
      return -1;
    }
    return handle->comm->recv_single(header);
  }
  long ret = -1;
  log_debug() << "recv_single " << std::endl;
  if (!handle->backlog.pop(header)) {
    log_error() << "recv_single: Error retrieving from backlog" << std::endl;
    return ret;
  }
  ret = static_cast<long>(header.size_data);
  log_debug() << "recv_single: returns " << ret << " bytes" << std::endl;
  return ret;
}

bool AsyncComm::create_header_send(Header& header) {
  assert(!global_comm);
  if (handle->is_closing()) {
    log_error() << "create_header_send: Thread is closing" << std::endl;
    return false;
  }
  // Early exit without lock if wrapped class dosn't have a
  //   create_header_send member
  if (type == IPC_COMM || type == MPI_COMM || type == FILE_COMM ||
      (type == DEFAULT_COMM && (COMM_BASE_TYPE == IPC_COMM ||
				COMM_BASE_TYPE == MPI_COMM ||
				COMM_BASE_TYPE == FILE_COMM)))
    return true;
  const AsyncLockGuard lock(handle);
  if (type == CLIENT_COMM &&
      !(header.flags & (HEAD_FLAG_EOF | HEAD_FLAG_CLIENT_SIGNON))) {
    if (!dynamic_cast<ClientComm*>(handle->comm)->send_signon(0, 3, this))
      return false;
    log_debug() << "AsyncComm::create_header_send: Sent signon" << std::endl;
  }
  return handle->comm->create_header_send(header);
}

Comm_t* AsyncComm::create_worker(utils::Address* address,
				 const DIRECTION& dir, int flgs) {
  return new AsyncComm("", address, dir, flgs | COMM_FLAG_WORKER, type);
}
