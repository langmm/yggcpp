#include "communicators/FileComm.hpp"


using namespace YggInterface::communicator;
using namespace YggInterface::utils;

FileComm::FileComm(const std::string name, const utils::Address &address,
		   DIRECTION direction, FLAG_TYPE flgs,
		   const COMM_TYPE type) :
  CommBase(name, address, direction, type, flgs),
  mode(std::fstream::in | std::fstream::out), mutex() {
  ADD_CONSTRUCTOR_OPEN(FileComm)
}

ADD_CONSTRUCTORS_DEF(FileComm)

void FileComm::_open(bool call_base) {
  BEFORE_OPEN_DEF;
  updateMaxMsgSize(0);
  bool created = address.address().empty();
  if (created) {
    std::string err;
#ifdef _MSC_VER
    char key[L_tmpnam] = "yggXXXXXX";
    _mktemp(key);
    if (errno != 0)
      err = std::system_category().message(GetLastError());
#else
    char key[L_tmpnam] = "yggXXXXXX";
    int fd = mkstemp(key);
    if (fd < 0)
      err = std::string(strerror(errno));
    else
      ::close(fd);
#endif
    if (!err.empty())
      throw_error("FileComm::_open: Error generating temporary file name. - " + err);
    address.address(key);
  }
  if (name.empty()) {
    this->name = "tempnewFILE." + this->address.address();
  } else {
    this->name = name;
  }
  if (direction == SEND)
    mode = std::fstream::out;
  else if (direction == RECV)
    mode = std::fstream::in;
  if (flags & FILE_FLAG_APPEND)
    mode |= std::fstream::app;
  if (flags & FILE_FLAG_BINARY)
    mode |= std::fstream::binary;
  if (created) {
    log_debug() << "FileComm::_open: Creating " << this->address.address() << std::endl;
    std::ofstream tmp;
    tmp.open(this->address.address().c_str(),
	     std::fstream::out | std::fstream::app);
    tmp.close();
    // std::FILE* tmp = fopen(this->address.address().c_str(), "a+");
    // fclose(tmp);
  }
  // if (created) {
  //   // Create fstream first so that ftok can be used to generate the
  //   // mutex on unix OSes
  //   handle = new std::fstream(this->address.address().c_str(), mode);
  //   mutex.init(this->address.address());
  // } else {
  mutex.init(this->address.address(), created);
  {
    ProcessLockGuard<ProcessMutex> lock_guard(mutex);
    handle = new std::fstream(this->address.address().c_str(), mode);
  }
  // }
  AFTER_OPEN_DEF;
}

void FileComm::_close(bool call_base) {
  BEFORE_CLOSE_DEF;
  if (handle && !global_comm) {
    ProcessLockGuard<ProcessMutex> lock_guard(mutex);
    handle->close();
  }
  // TODO: This can probably be removed and we can rely on the mutex
  // to cleanup the file
  bool delete_file = ((direction == RECV) ||
		      (!(flags & (COMM_FLAG_INTERFACE |
				  COMM_FLAG_WORKER))));
  if (ctx->for_testing_)
    delete_file = true;
  mutex.close(delete_file);
  AFTER_CLOSE_DEF;
}

void FileComm::refresh() const {
  handle->sync();
  // int pos = handle->tellg();
  // handle->close();
  // handle->open(this->address->address().c_str(), mode);
  // handle->seekg(pos, handle->beg);
  if (handle->eof() && !handle->fail())
    handle->clear();
}

int FileComm::comm_nmsg(DIRECTION dir) const {
  if (global_comm)
    return global_comm->comm_nmsg(dir);
  if (dir == NONE)
    dir = direction;
  if (dir != direction || dir != RECV)
    return 0;
  int out = -1;
  {
    refresh();
    ProcessLockGuard<ProcessMutex> lock_guard(const_cast<ProcessMutex*>(&mutex)[0]);
    if (!handle->fail()) {
      if (handle->eof() && !handle->fail())
	handle->clear();
      if (handle->peek() == EOF) {
	handle->clear();
	out = 0;
      } else if (!handle->fail()) {
	out = 1;
      }
    }
  }
  return out;
}

int FileComm::send_single(utils::Header& header) {
  ProcessLockGuard<ProcessMutex> lock_guard(mutex);
  assert(!global_comm);
  if (header.on_send() < 0)
    return -1;
  log_debug() << "send_single: " << header.size_msg << " bytes" << std::endl;
  int out = -1;
  if (handle->good()) {
    handle->write(header.data_msg(), header.size_msg);
    handle->flush();
    if (handle->good())
      out = static_cast<int>(header.size_msg);
  }
  return out;
}

long FileComm::recv_single(utils::Header& header) {
  ProcessLockGuard<ProcessMutex> lock_guard(mutex);
  assert(!global_comm);
  log_debug() << "recv_single:" << std::endl;
  refresh();
  if (handle->fail())
    return -1;
  int start = handle->tellg();
  handle->seekg(0, handle->end);
  int length = handle->tellg();
  handle->seekg(start, handle->beg);
  int ret = header.on_recv(NULL, length - start + 1);
  if (ret < 0) {
    log_error() << "recv_single: Error reallocating data" << std::endl;
    return ret;
  }
  if (flags & FILE_FLAG_READLINE)
    handle->getline(header.data_msg(), ret);
  else
    handle->get(header.data_msg(), ret);
  if (handle->eof() && !handle->fail())
    handle->clear();
  ret = handle->gcount();
  ret = header.on_recv(header.data_msg(), ret);
  log_debug() << "recv_single: returns " << ret << " bytes" << std::endl;
  return ret;
}

WORKER_METHOD_DEFS(FileComm)
