#include "communicators/BufferComm.hpp"

using namespace YggInterface::communicator;
using namespace YggInterface::utils;

BufferComm::BufferComm(const std::string name,
		       const utils::Address &address,
		       DIRECTION direction, FLAG_TYPE flgs,
		       const COMM_TYPE type) :
  CommBase(name, address, direction, type, flgs), memory(NULL) {
  ADD_CONSTRUCTOR_OPEN(BufferComm)
}

ADD_CONSTRUCTORS_DEF(BufferComm)

void BufferComm::_open(bool call_base) {
  BEFORE_OPEN_DEF;
  updateMaxMsgSize(MAX_SHARED_MEM_SIZE);
  bool created = address.address().empty();
  if (created)
    address.address(this->ctx->uuid_str(10));
  Comm_t::_init_name();
  handle = new ProcessSharedMemory(this->name,
				   sizeof(shmbuf_t),
				   this->address.address(),
				   created);
  {
    ProcessLockGuard<ProcessMutex> lock_guard(handle->mutex);
    // Initialize memory
    memory = (shmbuf_t*)(handle->memory);
    memory->count = 0;
    memory->total = 0;
  }
  AFTER_OPEN_DEF;
}

void BufferComm::_close(bool call_base) {
  BEFORE_CLOSE_DEF;
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
    // ProcessLockGuard<ProcessMutex> lock_guard(const_cast<ProcessMutex*>(&(handle->mutex))[0]);
    if (memory == NULL)
      return -1;  // GCOV_EXCL_LINE
    out = memory->count;
  }
  return out;
}

int BufferComm::send_single(utils::Header& header) {
  assert(!global_comm);
  if (header.on_send() < 0)
    return -1;
  log_debug() << "send_single: " << header.size_msg << " bytes" << std::endl;
  bool is_full = true;
  int out = -1;
  while (is_full) {
    if (handle == NULL || memory == NULL)
      return -1;  // GCOV_EXCL_LINE
    ProcessLockGuard<ProcessMutex> lock_guard(handle->mutex);
    is_full = (memory->count == (MAX_BUFFERS - 1));
    if (is_full) {
      log_debug() << "send_single: Shared memory buffers are full (" <<
	memory->count << " messages (MAX_BUFFERS = " << MAX_BUFFERS <<
	"), sleep" << std::endl;
      THREAD_USLEEP(YGG_SLEEP_TIME);
    } else {
      log_debug() << "send_single: Adding message to the buffer: count = "
		  << memory->count << std::endl;
      out = static_cast<int>(header.size_msg);
      memory->size[memory->count] = out;
      memcpy(&(memory->buf[memory->total]), header.data_msg(), header.size_msg);
      memory->count++;
      memory->total += out;
      log_debug() << "send_single: Added message to the buffer: count = "
		  << memory->count << std::endl;
    }
  }
  return out;
}

long BufferComm::recv_single(utils::Header& header) {
  assert(!global_comm);
  log_debug() << "recv_single:" << std::endl;
  if (handle == NULL)
    return -1;  // GCOV_EXCL_LINE
  ProcessLockGuard<ProcessMutex> lock_guard(handle->mutex);
  if (memory == NULL || memory->count == 0)
    return -1;  // GCOV_EXCL_LINE
  int next_size = memory->size[0];
  long ret = header.on_recv(&(memory->buf[0]), next_size);
  if (ret >= 0) {
    log_debug() << "recv_single: Removing message from the buffer: " <<
      "count = " << memory->count << std::endl;
    memmove(&(memory->buf[0]), &(memory->buf[next_size]),
	    memory->total - next_size);
    memmove(&(memory->size[0]), &(memory->size[1]),
	    (memory->count - 1) * sizeof(int));
    memory->count--;
    memory->total -= next_size;
    log_debug() << "recv_single: Removed message from the buffer: " <<
      "count = " << memory->count << std::endl;
  }
  log_debug() << "recv_single: returns " << ret << " bytes" << std::endl;
  return ret;
}

WORKER_METHOD_DEFS(BufferComm)
