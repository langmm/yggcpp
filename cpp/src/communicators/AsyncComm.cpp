#include "communicators/AsyncComm.hpp"
#ifdef THREADSINSTALLED
#include <atomic>
#include <condition_variable>
#endif // THREADSINSTALLED
#include "communicators/ClientComm.hpp"
#include "communicators/ServerComm.hpp"
#include "utils/logging.hpp"

using namespace YggInterface::communicator;
using namespace YggInterface::utils;

#ifdef THREADSINSTALLED
#define LOCK_BUFFER(name)					\
  log_verbose() << #name << ": Before lock" << std::endl;	\
  const std::lock_guard<std::mutex> lk(pImplBuffer->mutex);     \
  log_verbose() << #name << ": After lock" << std::endl
#else
#define LOCK_BUFFER(name)
#endif // THREADSINSTALLED

/////////////////
// AsyncBuffer //
/////////////////

/**
 * @brief Wrapper for threaded buffer implementation.
 */
class AsyncBuffer::ImplBuffer {
public:
#ifdef THREADSINSTALLED
  /**
   * @brief Constructor.
   * @param[in] flag_init Initial value for flag.
   */
  ImplBuffer(const bool& flag_init = false) :
    flag(flag_init), mutex(), cv() {}
  std::atomic_bool flag;           /**< boolean flag */
  std::mutex mutex;                /**< mutex for locking thread */
  std::condition_variable cv;      /**< conditional variable for state */
#else // THREADSINSTALLED
  /**
   * @brief Constructor.
   * @param[in] flag_init Initial value for flag.
   */
  ImplBuffer(const bool& flag_init = false) :
    flag(flag_init) {
    UNINSTALLED_ERROR(THREADS);
  }
  bool flag;                       /**< boolean flag */
#endif // THREADSINSTALLED
  /**
   * @brief Notify all threads.
   */
  void notify() {
#ifdef THREADSINSTALLED
    cv.notify_all();
#endif // THREADSINSTALLED
  }
  /**
   * @brief Lock the mutex.
   */
  void lock() {
#ifdef THREADSINSTALLED
    mutex.lock();
#endif // THREADSINSTALLED
  }
  /**
   * @brief Unlock the mutex.
   */
  void unlock() {
#ifdef THREADSINSTALLED
    mutex.unlock();
#endif // THREADSINSTALLED
  }
  /**
   * @brief Set the flag value.
   * @param[in] new_flag New flag value.
   */
  void set_flag(const bool& new_flag) {
#ifdef THREADSINSTALLED
    flag.store(new_flag);
#else // THREADSINSTALLED
    flag = new_flag;
#endif // THREADSINSTALLED
  }
  /**
   * @brief Get the flag value.
   * @returns Flag value.
   */
  bool get_flag() const {
#ifdef THREADSINSTALLED
    return flag.load();
#else // THREADSINSTALLED
    return flag;
#endif // THREADSINSTALLED
  }
};

AsyncBuffer::AsyncBuffer(const std::string logInst) :
  buffer(), logInst_(logInst),
  pImplBuffer(std::make_unique<ImplBuffer>()) {}
AsyncBuffer::~AsyncBuffer() {}
void AsyncBuffer::close() {
  {
    LOCK_BUFFER(close);
    pImplBuffer->set_flag(true);
  }
  pImplBuffer->notify();
}
bool AsyncBuffer::is_closed() const {
  return pImplBuffer->get_flag();
}

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
  if (!dont_notify) {
      log_debug() << "insert[" << idx << "]: " <<
	"Notifying threads of message" <<
	" (send = " << for_send << ")" << std::endl;
      pImplBuffer->notify();
  }
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
		      bool move, bool erase, bool dont_notify) {
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
  if (out && erase) {
    buffer.erase(buffer.begin() + idx);
    if (!dont_notify)
      pImplBuffer->notify();
  }
  return out;
}
bool AsyncBuffer::pop(utils::Header& header, size_t idx,
		      bool dont_notify) {
  return get(header, idx, true, true, dont_notify);
}
void AsyncBuffer::notify() {
  pImplBuffer->notify();
}
bool AsyncBuffer::message_waiting(const std::string id,
				  const bool negative) {
  if (is_closed()) // exit early
    return true;
  if (buffer.empty())
    return negative;
  if (id.empty())
    return (!negative);
  std::string request_id;
  for (std::vector<utils::Header>::iterator it = buffer.begin();
       it != buffer.end(); it++) {
    if (it->GetMetaString("request_id", request_id) &&
	request_id == id)
      return (!negative);
  }
  return negative;
}
#ifdef THREADSINSTALLED
bool AsyncBuffer::wait(const std::string id, const bool negative) {
  std::unique_lock<std::mutex> lk(pImplBuffer->mutex);
  if (message_waiting(id, negative))
    return true;
  pImplBuffer->cv.wait(lk, [this, id, negative]{
    return message_waiting(id, negative); });
  return true;
}
bool AsyncBuffer::wait_for(const int64_t& twait,
                           const std::string id, const bool negative) {
  std::unique_lock<std::mutex> lk(pImplBuffer->mutex);
  if (message_waiting(id, negative))
    return true;
  return pImplBuffer->cv.wait_for(lk, std::chrono::microseconds(twait), [this, id, negative]{
    return message_waiting(id, negative); });
}
#else // THREADSINSTALLED
bool AsyncBuffer::wait(const std::string, const bool) {
  return true; // exit early
}
bool AsyncBuffer::wait_for(const int64_t&,
                           const std::string, const bool) {
  return true;
}
#endif // THREADSINSTALLED

//////////////////
// AsyncStatus //
//////////////////

/**
 * @brief Wrapper for threaded status implementation.
 */
class AsyncStatus::ImplStatus {
public:
#ifdef THREADSINSTALLED
  /**
   * @brief Constructor.
   * @param[in] flag_init Initial value for flag.
   */
  ImplStatus(const bool& flag_init = false) :
    flag(flag_init), mutex(), cv(), status(THREAD_INACTIVE), thread() {}
  std::atomic_bool flag;           /**< boolean flag */
  std::mutex mutex;                /**< mutex for locking thread */
  std::condition_variable cv;      /**< conditional variable for state */
  std::atomic_int status;          /**< bit flags describing thread status */
  std::unique_ptr<std::thread> thread; /**< thread for performing async task */
  bool _wait_status(const int new_status,
                    std::unique_lock<std::mutex>& lk) {
    if (!(status.load() & new_status)) {
      cv.wait(lk, [this, new_status]{
        return (status.load() & new_status); });
    }
    return true;
  }
#else // THREADSINSTALLED
  /**
   * @brief Constructor.
   * @param[in] flag_init Initial value for flag.
   */
  ImplStatus(const bool& flag_init = false) :
    flag(flag_init), status(THREAD_COMPLETE | THREAD_ERROR) {
    UNINSTALLED_ERROR(THREADS);
  }
  bool flag;                       /**< boolean flag */
  int status;                      /**< bit flags describing thread status */
#endif // THREADSINSTALLED
  /**
   * @brief Notify all threads.
   */
  void notify() {
#ifdef THREADSINSTALLED
    cv.notify_all();
#endif // THREADSINSTALLED
  }
  /**
   * @brief Lock the mutex.
   */
  void lock() {
#ifdef THREADSINSTALLED
    mutex.lock();
#endif // THREADSINSTALLED
  }
  /**
   * @brief Unlock the mutex.
   */
  void unlock() {
#ifdef THREADSINSTALLED
    mutex.unlock();
#endif // THREADSINSTALLED
  }
  /**
   * @brief Set the flag value.
   * @param[in] new_flag New flag value.
   */
  void set_flag(const bool& new_flag) {
#ifdef THREADSINSTALLED
    flag.store(new_flag);
#else // THREADSINSTALLED
    flag = new_flag;
#endif // THREADSINSTALLED
  }
  /**
   * @brief Get the flag value.
   * @returns Flag value.
   */
  bool get_flag() const {
#ifdef THREADSINSTALLED
    return flag.load();
#else // THREADSINSTALLED
    return flag;
#endif // THREADSINSTALLED
  }
  /**
   * @brief Get the status value.
   * @returns Status value.
   */
  int get_status() const {
#ifdef THREADSINSTALLED
    return status.load();
#else // THREADSINSTALLED
    return status;
#endif // THREADSINSTALLED
  }
  /**
   * @brief Wait for the specified status bit flags to be set.
   * @param[in] new_status Bit flags to wait for.
   * @returns true if the end status has the specified bit flags set,
   *   false otherwise.
   */
  bool wait_status(const int new_status) {
#ifdef THREADSINSTALLED
    std::unique_lock<std::mutex> lk(mutex);
    return _wait_status(new_status, lk);
#else // THREADSINSTALLED
    return (status & new_status);
#endif // THREADSINSTALLED
  }
};

AsyncStatus::AsyncStatus(const std::string& logInst) :
  logInst_(logInst),
  pImplStatus(std::make_unique<ImplStatus>()) {}
void AsyncStatus::notify() {
  pImplStatus->notify();
}
AsyncStatus::~AsyncStatus() {}
bool AsyncStatus::is_locked() const {
  return pImplStatus->get_flag();
}
void AsyncStatus::lock() {
  if (!is_locked()) {
    pImplStatus->lock();
    pImplStatus->set_flag(true);
  }
}
void AsyncStatus::unlock() {
  if (is_locked()) {
    pImplStatus->set_flag(false);
    pImplStatus->unlock();
  }
}
void AsyncStatus::set_status(const int new_status, bool dont_notify,
			     bool negative) {
  if (negative)
    pImplStatus->status &= new_status;
  else
    pImplStatus->status |= new_status;
  if (!dont_notify)
    pImplStatus->notify();
}
void AsyncStatus::set_status_lock(const int new_status, bool dont_notify,
                                  bool negative) {
  pImplStatus->lock();
  set_status(new_status, dont_notify, negative);
  pImplStatus->unlock();
}
int AsyncStatus::get_status() const {
  return pImplStatus->get_status();
}

#ifdef THREADSINSTALLED
void AsyncStatus::stop_thread() {
  if(!pImplStatus->thread.get()) {
    log_debug() << "stop_thread: No thread currently managed" << std::endl;
    return;
  }
  log_debug() << "stop_thread: begin" << std::endl;
  set_status_lock(THREAD_CLOSING);
  wait_status(THREAD_COMPLETE);
  try {
    if (pImplStatus->thread->joinable()) {
      pImplStatus->thread->join();
    }
    log_debug() << "stop_thread: joinable = " << pImplStatus->thread->joinable() << std::endl;
  } catch (const std::system_error& e) {
    log_error() << "stop_thread: Error joining thread (" << e.code() << "): " << e.what() << std::endl;
  }
  if (get_status() & THREAD_ERROR) {
    log_error() << "stop_thread: Error on thread" << std::endl;
  }
  log_debug() << "stop_thread: end" << std::endl;
}
void* AsyncStatus::get_thread() {
  return (void*)(pImplStatus->thread.get());
}
const void* AsyncStatus::get_thread() const {
  return (const void*)(pImplStatus->thread.get());
}
void AsyncStatus::set_thread(void* ptr, void* lock_ptr) {
  pImplStatus->thread.reset((std::thread*)ptr);
  if (lock_ptr == nullptr || ptr == nullptr) return;
  std::unique_lock<std::mutex>* lk = (std::unique_lock<std::mutex>*)lock_ptr;
  log_debug() << "set_thread: waiting for thread to start" << std::endl;
  pImplStatus->_wait_status(THREAD_STARTED | THREAD_COMPLETE, *lk);
  log_debug() << "set_thread: thread started" << std::endl;
}
void* AsyncStatus::get_mutex() {
  return (void*)(&(pImplStatus->mutex));
}
bool AsyncStatus::wait_status(const int new_status) {
  return pImplStatus->wait_status(new_status);
}
bool AsyncStatus::wait_for_status(const int64_t& twait,
                                  const int new_status) {
  if (pImplStatus->status.load() & new_status)
    return true;
  std::unique_lock<std::mutex> lk(pImplStatus->mutex);
  return pImplStatus->cv.wait_for(lk, std::chrono::microseconds(twait),
                                  [this, new_status]{
    return (this->pImplStatus->status.load() & new_status); });
}
#else // THREADSINSTALLED
void AsyncStatus::stop_thread() {}
void* AsyncStatus::get_thread() { return nullptr; }
const void* AsyncStatus::get_thread() const { return nullptr; }
void AsyncStatus::set_thread(void*, void*) {
  UNINSTALLED_ERROR(THREADS);
}
void* AsyncStatus::get_mutex() { return nullptr; }
bool AsyncStatus::wait_status(const int new_status) {
  return pImplStatus->wait_status(new_status);
}
bool AsyncStatus::wait_for_status(const int64_t&, const int new_status) {
  return pImplStatus->wait_status(new_status);
}
#endif // THREADSINSTALLED


//////////////////
// AsyncBacklog //
//////////////////

// TODO: Preserve backlog buffers?

AsyncBacklog::AsyncBacklog(AsyncComm* parent) :
  AsyncStatus(parent->logInst()),
  comm(nullptr), backlog(parent->logInst()) {
#ifdef THREADSINSTALLED
  std::unique_lock<std::mutex> lk(pImplStatus->mutex);
  pImplStatus->thread = std::unique_ptr<std::thread>
    (new std::thread(&AsyncBacklog::on_thread, this, parent));
  log_debug() << "start: waiting for thread to start" << std::endl;
  pImplStatus->_wait_status(THREAD_STARTED | THREAD_COMPLETE, lk);
  log_debug() << "start: thread started" << std::endl;
#else // THREADSINSTALLED
  UNINSTALLED_ERROR(THREADS);
#endif // THREADSINSTALLED
}

AsyncBacklog::~AsyncBacklog() {
  log_debug() << "~AsyncBacklog: begin" << std::endl;
  backlog.close();
  this->stop_thread();
  log_debug() << "~AsyncBacklog: end" << std::endl;
}

void AsyncBacklog::on_thread(AsyncComm* parent) {
  bool out = true;
  try {
#ifdef THREADSINSTALLED
    DIRECTION direction = parent->getDirection();
    {
      const std::lock_guard<std::mutex> comm_lock(pImplStatus->mutex);
      FLAG_TYPE flgs_comm = (parent->getFlags() & ~COMM_FLAG_ASYNC
                             & ~COMM_FLAG_GLOBAL) | COMM_FLAG_ASYNC_WRAPPED;
      COMM_TYPE comm_type = parent->getCommType();
      // Must find client/server by flags because this will be called
      // while the parent class is still being initialized?
      // if (flgs_comm & COMM_FLAG_CLIENT)
      //   comm_type = CLIENT_COMM;
      // else if (flgs_comm & COMM_FLAG_SERVER)
      //   comm_type = SERVER_COMM;
      log_debug() << "on_thread: Creating " << COMM_TYPE2str(comm_type) <<
        " comm on thread" << std::endl;
      Address addr(parent->getAddress());
      comm = new_Comm_t(direction,
                        comm_type,
			parent->getName(),
			addr, flgs_comm, 0,
			parent->request_commtype,
			parent->response_commtype,
			parent->request_flags,
			parent->response_flags,
			parent->language);
      if (comm) {
	parent->updateMaxMsgSize(comm->getMaxMsgSize());
	parent->address.address(comm->getAddress());
	parent->updateMsgBufSize(comm->getMsgBufSize());
	parent->getFlags() |= (comm->getFlags() & ~flgs_comm);
	if (comm->getCommType() == CLIENT_COMM) {
	  set_status(THREAD_IS_CLIENT, true);
	} else {
	  set_status(THREAD_SIGNON_SENT | THREAD_SIGNON_RECV, true);
	}
	set_status(THREAD_STARTED);
	log_debug() << "on_thread: Created " << COMM_TYPE2str(comm->getCommType()) << " comm on thread" << std::endl;
      } else {
	log_error() << "on_thread: Failed to create comm on thread" << std::endl;
	backlog.close();
	out = false;
	set_status(THREAD_STARTED);
      }
    }
    // wait_status(THREAD_INIT);
    if (direction == SEND) {
      while (!backlog.is_closed()) {
        notify(); // Periodically notify
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
        notify(); // Periodically notify
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
      const std::lock_guard<std::mutex> comm_lock(pImplStatus->mutex);
      if (comm) {
	delete comm;
	comm = nullptr;
      }
    }
#else // THREADSINSTALLED
    UNUSED(parent);
#endif // THREADSINSTALLED
  } catch (const std::exception &exc) {
  // } catch (...) {
    log_error() << "on_thread: C++ Error on thread: " << exc.what() << std::endl;
    backlog.close();
    out = false;
  }
#ifdef THREADSINSTALLED
  if (!out)
    set_status_lock(THREAD_ERROR);
  set_status_lock(THREAD_COMPLETE);
#else // THREADSINSTALLED
  UNUSED(out);
#endif // THREADSINSTALLED
}

int AsyncBacklog::signon_status() {
  if (is_closing())
    return SIGNON_ERROR;
#ifdef THREADSINSTALLED
  if (!(get_status() & THREAD_IS_CLIENT))
    return SIGNON_COMPLETE;
  if (!(get_status() & THREAD_SIGNON_SENT))
    return SIGNON_NOT_SENT;
  // Don't early exit to allow update to THREAD_HAS_RESPONSE
  if (get_status() & THREAD_SIGNON_RECV)
    return SIGNON_COMPLETE;
  const std::lock_guard<std::mutex> comm_lock(pImplStatus->mutex);
  if (is_closing() || !(comm)) {
    return SIGNON_ERROR;
  }
  ClientComm* cli = dynamic_cast<ClientComm*>(comm);
  RequestList& requests = cli->getRequests();
  if (requests.signon_complete) {
    // if (cli->nmsg(RECV) > 0) {
    //   if (!(status.load() & THREAD_HAS_RESPONSE))
    // 	set_status(THREAD_HAS_RESPONSE, true);
    // } else {
    //   if (status.load() & THREAD_HAS_RESPONSE)
    // 	set_status(THREAD_HAS_RESPONSE, true, true);
    // }
    set_status(THREAD_SIGNON_RECV);
    return SIGNON_COMPLETE;
  }
  if (!requests.requests.empty()) {
    int nmsg = requests.activeComm()->nmsg(RECV);
    if (nmsg > 0)
      return SIGNON_WAITING;
    else if (nmsg < 0)
      return SIGNON_ERROR;
  }
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
      const std::lock_guard<std::mutex> comm_lock(pImplStatus->mutex);
      if (is_closing() || !(comm)) {
	return false;
      }
      ClientComm* cli = dynamic_cast<ClientComm*>(comm);
      if(!cli)
        return false;
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
  if (backlog.pop(header, 0, true)) {
    const std::lock_guard<std::mutex> comm_lock(pImplStatus->mutex);
    out = comm->send_single(header);
    if (out >= 0) {
      log_debug() << "send: Sent message from backlog" << std::endl;
      if (header.flags & HEAD_FLAG_CLIENT_SIGNON) {
	set_status(THREAD_SIGNON_SENT);
	// Sleep for a bit on client open to prevent sending too many
	//   signon messages.
	// std::this_thread::sleep_for(std::chrono::microseconds(100*YGG_SLEEP_TIME));
      }
      backlog.notify();
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
  bool received = false;
  utils::Header header(true);
  {
    const std::lock_guard<std::mutex> comm_lock(pImplStatus->mutex);
    int nmsg = comm->nmsg();
    if (nmsg > 0) {
      out = comm->recv_single(header);
      received = true;
    } else if (nmsg < 0) {
      out = -1;
    }
  }
  if (received && out >= 0) {
    if (!(header.flags & HEAD_FLAG_REPEAT)) {
      log_debug() << "recv: Received message into backlog: " << out << std::endl;
      // if (status.load() & THREAD_IS_CLIENT) {
      // 	if (!addResponseSchema(dynamic_cast<ClientComm*>(comm)->getMetadata(RECV))) {
      // 	  log_error() << "recv: Error transfering response metadata back to client wrapper" << std::endl;
      // 	  return false;
      // 	}
      // }
      if (!backlog.append(header, true))
	out = -1;
    }
  }
  return out;
}

////////////////////
// AsyncLockGuard //
////////////////////

AsyncLockGuard::AsyncLockGuard(AsyncStatus* stat, bool dont_lock) :
  locked(false), status(stat) {
  if (!(dont_lock || status->is_locked())) {
    locked = true;
    status->lock();
  }
}
AsyncLockGuard::~AsyncLockGuard() {
  if (locked) {
    status->unlock();
  }
}


///////////////
// AsyncComm //
///////////////

COMM_CONSTRUCTOR_CORE_DEF_PARAM(AsyncComm, COMM_FLAG_ASYNC,
				response_metadata(),
				request_commtype(supp.request_commtype),
				response_commtype(supp.response_commtype),
				request_flags(supp.request_flags),
				response_flags(supp.response_flags))

bool AsyncComm::isInstalled() { return true; }

void AsyncComm::_open(bool call_base) {
  BEFORE_OPEN_DEF;
  if (type == SERVER_COMM)
    this->direction = RECV;
  else if (type == CLIENT_COMM)
    this->direction = SEND;
  if (!global_comm) {
    std::string err;
#ifndef YGGDRASIL_PYGIL_NO_MANAGEMENT
    if (!PyGIL_release(true))
      throw_error("AsyncComm::_open: Failed to release the Python GIL");
#endif
    try {
      if (getCommType() == FUNCTION_COMM && !getAddress().empty()) {
	ctx->create_registered_function(getAddress(), language);
      }
      handle = new AsyncBacklog(this);
    } catch (std::exception& e) {
      err = e.what();
    }
#ifndef YGGDRASIL_PYGIL_NO_MANAGEMENT
    if (!PyGIL_restore(true))
      throw_error("AsyncComm::_open: Failed to restore the Python GIL [caught error: \"" + err + "\"]");
#endif
    if (!err.empty())
      throw_error(std::string("AsyncComm::_open: ") + err);
  }
  AFTER_OPEN_DEF;
}
void AsyncComm::_close(bool call_base) {
  BEFORE_CLOSE_DEF;
  AFTER_CLOSE_DEF;
}

int AsyncComm::nmsg(DIRECTION dir) const {
  if (global_comm)
    return global_comm->nmsg(dir);
  if (dir == NONE)
    dir = direction;
  if ((!handle) || (handle->is_closing()))
    return 0;
  if ((type == CLIENT_COMM && dir == RECV) ||
      (type == SERVER_COMM && dir == SEND)) {
    const AsyncLockGuard lock(handle);
    if (handle->is_closing())
      return 0;
    return handle->comm->nmsg(dir);
  }
  return static_cast<int>(handle->backlog.size());
}

int AsyncComm::wait_for_recv(const int64_t& tout) const {
  int ret = -1;
  YGGCOMM_PYGIL_ALLOW_THREADS_BEGIN(wait_for_recv, -1)
#ifdef THREADSINSTALLED
  if (global_comm || (type == CLIENT_COMM)) {
    if (type == CLIENT_COMM && (flags & COMM_FLAG_USED_SENT)) {
      std::string req_id;
      {
	const AsyncLockGuard lock(handle);
	if (handle->is_closing() || !(handle->comm)) {
	  log_error() << "wait_for_recv: Comm is closed" << std::endl;
	  goto cleanup;
	}
	ClientComm* cli = dynamic_cast<ClientComm*>(handle->comm);
	req_id = cli->getRequests().activeRequestClient(true);
      }
      if (!req_id.empty()) {
	// Ensure that the request has actually been sent by waiting
	//   for it to exit the buffer.
	log_debug() << "wait_for_recv: timeout = " << tout <<
	  " microseconds (client response)" << std::endl;
	handle->backlog.wait_for(tout, req_id, true);
	{
	  if (handle->is_closing()) {
	    log_error() << "wait_for_recv: Comm is closed (request)" << std::endl;
	    goto cleanup;
	  }
	  const AsyncLockGuard lock(handle);
	  ret = handle->comm->wait_for_recv(tout);
	  goto cleanup;
	}
      } else {
	log_debug() << "wait_for_recv: client dosn't have any active requests" << std::endl;
      }
    }
    log_debug() << "wait_for_recv: Client using default method" << std::endl;
    ret = CommBase::wait_for_recv(tout);
    goto cleanup;
  }
  log_debug() << "wait_for_recv: timeout = " << tout <<
    " microseconds" << std::endl;
  if (!handle->backlog.wait_for(tout)) {
    ret = 0;
    goto cleanup;
  }
  ret = nmsg(RECV);
 cleanup:
#else // THREADSINSTALLED
  ret = CommBase::wait_for_recv(tout);
#endif // THREADSINSTALLED
  YGGCOMM_PYGIL_ALLOW_THREADS_END(wait_for_recv, -1)
  return ret;
}

YggInterface::utils::Metadata& AsyncComm::getMetadata(const DIRECTION dir) {
  if (global_comm)
    return global_comm->getMetadata(dir);
  if ((type == CLIENT_COMM && dir == RECV) ||
      (type == SERVER_COMM && dir == SEND)) {
    return response_metadata;
  }
  return CommBase::getMetadata();
}

void AsyncComm::set_timeout_recv(int64_t new_timeout) {
  if (global_comm) {
    global_comm->set_timeout_recv(new_timeout);
    return;
  }
  // const AsyncLockGuard lock(handle);
  // if (handle->comm)
  //   handle->comm->set_timeout_recv(new_timeout);
  CommBase::set_timeout_recv(new_timeout);
}
int64_t AsyncComm::get_timeout_recv() const {
  if (global_comm) {
    return global_comm->get_timeout_recv();
  }
  // const AsyncLockGuard lock(handle);
  // if (handle->comm)
  //   return handle->comm->get_timeout_recv();
  return CommBase::get_timeout_recv();
}

std::string AsyncComm::logClass() const {
  std::string out = CommBase::logClass();
  out += "[ASYNC]";
  return out;
}

void AsyncComm::close_thread() {
  if (handle) {
    const AsyncLockGuard lock(handle);
    handle->backlog.close();
  }
}

int AsyncComm::send_single(Header& header) {
  int ret = -1;
  assert((!global_comm) && handle);
  YGGCOMM_PYGIL_ALLOW_THREADS_BEGIN(send_single, -1)
  log_debug() << "send_single: begin" << std::endl;
  if (type == SERVER_COMM) {
    const AsyncLockGuard lock(handle);
    if (handle->is_closing()) {
      log_error() << "send_single: Thread is closing" << std::endl;
      goto cleanup;
    }
    ret = handle->comm->send_single(header);
    goto cleanup;
  }
  if (header.on_send() < 0)
    goto cleanup;
  log_debug() << "send_single: " << header.size_msg << " bytes" << std::endl;
  if (!handle->backlog.append(header, false, false, true))
    goto cleanup;
  // TODO: Wait for function call if function wrapped & Python GIL
  //   would prevent concurrency?
  ret = static_cast<int>(header.size_msg);
 cleanup:
  YGGCOMM_PYGIL_ALLOW_THREADS_END(send_single, -1)
  return ret;
}

long AsyncComm::recv_single(Header& header) {
  long ret = -1;
  YGGCOMM_PYGIL_ALLOW_THREADS_BEGIN(recv_single, -1)
  if (type == CLIENT_COMM) {
    const AsyncLockGuard lock(handle);
    assert((!global_comm) && handle);
    if (handle->is_closing()) {
      log_error() << "recv_single: Thread is closing" << std::endl;
      goto cleanup;
    }
    ret = handle->comm->recv_single(header);
    goto cleanup;
  }
  log_debug() << "recv_single " << std::endl;
  if (!handle->backlog.pop(header)) {
    log_error() << "recv_single: Error retrieving from backlog" << std::endl;
    goto cleanup;
  }
  ret = static_cast<long>(header.size_data);
  if ((ret == 0) && (header.flags & HEAD_FLAG_DOC_SET))
    ret = 1;
  log_debug() << "recv_single: returns " << ret << " bytes" << std::endl;
 cleanup:
  YGGCOMM_PYGIL_ALLOW_THREADS_END(recv_single, -1)
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
  if (handle->is_closing() || !(handle->comm)) {
    log_error() << "create_header_send: Comm is closed" << std::endl;
    return false;
  }
  if (type == CLIENT_COMM && handle->comm->getCommType() != CLIENT_COMM) {
    log_error() << "Top level type is client, but the thread's comm type is " << COMM_TYPE2str(handle->comm->getCommType()) << std::endl;
    return false;
  }
  if (type == CLIENT_COMM &&
      !(header.flags & (HEAD_FLAG_EOF | HEAD_FLAG_CLIENT_SIGNON))) {
    ClientComm* client_comm = dynamic_cast<ClientComm*>(handle->comm);
    if (!client_comm)
      return false;
    if (!dynamic_cast<ClientComm*>(handle->comm)->send_signon(0, 3, this))
      return false;
    log_debug() << "AsyncComm::create_header_send: Sent signon" << std::endl;
  }
  if (type == SERVER_COMM) {
    if (!dynamic_cast<ServerComm*>(handle->comm)->addResponseSchema(response_metadata))
      return false;
  }
  return handle->comm->create_header_send(header);
}

Comm_t* AsyncComm::create_worker(utils::Address& address,
                                 const DIRECTION dir, FLAG_TYPE flgs) {
  std::string nme;
  return new AsyncComm(nme, address, dir, flgs | COMM_FLAG_WORKER, type);
}

#define RESPONSE_SCHEMA(name, method, argsT, args)			\
  bool AsyncComm::name argsT {						\
    if (global_comm) {							\
      return (dynamic_cast<AsyncComm*>(global_comm))->name args;	\
    }									\
    if (type != CLIENT_COMM) {						\
      log_error() << "addResponseSchema: Wrapped communicator is not a client" << std::endl; \
      return false;							\
    }									\
    return response_metadata.method args;				\
  }

RESPONSE_SCHEMA(addResponseSchema, fromSchema,
		(const std::string& s, bool use_generic),
		(s, use_generic))
RESPONSE_SCHEMA(addResponseSchema, fromSchema,
		(const yggdrasil_rapidjson::Value& s, bool use_generic),
		(s, use_generic))
RESPONSE_SCHEMA(addResponseSchema, fromMetadata,
		(const utils::Metadata& metadata, bool use_generic),
		(metadata, use_generic))
RESPONSE_SCHEMA(addResponseFormat, fromFormat,
		(const std::string& format_str, bool use_generic),
		(format_str, use_generic))

#undef RESPONSE_SCHEMA
#undef LOCK_BUFFER
