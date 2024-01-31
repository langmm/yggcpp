#include "communicators/BufferComm.hpp"

using namespace YggInterface::communicator;
using namespace YggInterface::utils;

BufferComm::BufferComm(const std::string name,
		       const utils::Address &address,
		       DIRECTION direction, FLAG_TYPE flgs,
		       const COMM_TYPE type) :
  CommBase(name, address, direction, type, flgs),
  mutex(), base_handle(0) {
  ADD_CONSTRUCTOR_OPEN(BufferComm)
}

ADD_CONSTRUCTORS_DEF(BufferComm)

void BufferComm::_open(bool call_base) {
  BEFORE_OPEN_DEF;
  updateMaxMsgSize(MAX_SHARED_MEM_SIZE);
  bool created = address.address().empty();
  if (created) {
    address.address(random_string(10));
  }
  if (name.empty()) {
    this->name = "tempnewBUFF." + this->address.address();
  } else {
    this->name = name;
  }
  mutex.init(this->address.address(), created);
  {
    ProcessLockGuard<ProcessMutex> lock_guard(mutex);
#ifdef _WIN32
    base_handle = CreateFileMapping(INVALID_HANDLE_VALUE, NULL,
				    PAGE_READWRITE, 0,
				    sizeof(shmbuf_t),
				    this->address.address().c_str());
    if (base_handle == NULL)
      throw_error("_open: CreateFileMapping failed - "
		  + std::system_category().message(GetLastError()));
    handle = (shmbuf_t*)MapViewOfFile(base_handle, FILE_MAP_ALL_ACCESS,
				      0, 0, sizeof(shmbuf_t));
    if (handle == NULL) {
      CloseHandle(base_handle);
      base_handle = NULL;
      throw_error("_open: MapViewOfFile failed - "
		  + std::system_category().message(GetLastError()));
    }
#else
    key_t key = ftok(this->address.address().c_str(), 10);
    if (key == -1)
      throw_error("_open: ftok failed - " + std::string(strerror(errno)));
    base_handle = shmget(key, sizeof(shmbuf_t), 0666 | IPC_CREAT);
    if (base_handle == -1)
      throw_error("_open: shmget failed - " + std::string(strerror(errno)));
    handle = (shmbuf_t*)shmat(base_handle, NULL, 0);
    if (handle == (shmbuf_t*)(-1)) {
      handle = NULL;
      throw_error("_open: shmat failed - " + std::string(strerror(errno)));
    }
#endif
    // Initialize memory
    handle->count = 0;
    handle->total = 0;
  }
  AFTER_OPEN_DEF;
}

void BufferComm::_close(bool call_base) {
  BEFORE_CLOSE_DEF;
  if (handle && !global_comm) {
    ProcessLockGuard<ProcessMutex> lock_guard(mutex);
#ifdef _WIN32
    if (handle != NULL) {
      UnmapViewOfFile((PVOID)handle);
      handle = NULL;
    }
    if (base_handle != NULL) {
      CloseHandle(base_handle);
      base_handle = NULL;
    }
#else
    if (handle != NULL) {
      if (shmdt((void*)handle) == -1)
	log_error() << "_close: shmdt failed - " << std::string(strerror(errno)) << std::endl;
      handle = NULL;
    }
    if (base_handle >= 0) {
      if (mutex.nproc() <= 1) {
	if (shmctl(base_handle, IPC_RMID, 0) == -1)
	  log_error() << "_close: shmctl failed - " << std::string(strerror(errno)) << std::endl;
      }
      base_handle = -1;
    }
#endif
  }
  AFTER_CLOSE_DEF;
}

int BufferComm::comm_nmsg(DIRECTION dir) const {
  if (global_comm)
    return global_comm->comm_nmsg(dir);
  if (dir == NONE)
    dir = direction;
  if (dir != direction || dir != RECV)
    return 0;
  int out = -1;
  {
    ProcessLockGuard<ProcessMutex> lock_guard(const_cast<ProcessMutex*>(&mutex)[0]);
    if (handle == NULL)
      return -1;
    out = handle->count;
  }
  return out;
}

int BufferComm::send_single(utils::Header& header) {
  assert(!global_comm);
  if (header.on_send() < 0)
    return -1;
  log_debug() << "send_single: " << header.size_msg << " bytes" << std::endl;
  bool is_full = false;
  int out = -1;
  while (!is_full) {
    ProcessLockGuard<ProcessMutex> lock_guard(mutex);
    if (handle == NULL)
      return -1;
    is_full = (handle->count == (MAX_BUFFERS - 1));
    if (is_full) {
      log_debug() << "send_single: Shared memory buffers are full, sleep" << std::endl;
      THREAD_USLEEP(YGG_SLEEP_TIME);
    } else {
      out = static_cast<int>(header.size_msg);
      handle->size[handle->count] = out;
      memcpy(&(handle->buf[handle->total]), header.data_msg(), header.size_msg);
      handle->count++;
      handle->total += out;
    }
  }
  return out;
}

long BufferComm::recv_single(utils::Header& header) {
  ProcessLockGuard<ProcessMutex> lock_guard(mutex);
  assert(!global_comm);
  log_debug() << "recv_single:" << std::endl;
  if (handle == NULL || handle->count == 0)
    return -1;
  int next_size = handle->size[0];
  long ret = header.on_recv(&(handle->buf[0]), next_size);
  if (ret >= 0) {
    memmove(&(handle->buf[0]), &(handle->buf[next_size]),
	    handle->total - next_size);
    memmove(&(handle->size[0]), &(handle->size[1]),
	    (handle->count - 1) * sizeof(int));
    handle->count--;
    handle->total -= next_size;
  }
  log_debug() << "recv_single: returns " << ret << " bytes" << std::endl;
  return ret;
}

WORKER_METHOD_DEFS(BufferComm)
