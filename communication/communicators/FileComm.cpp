#include "FileComm.hpp"


using namespace communication::communicator;
using namespace communication::utils;

FileComm::FileComm(const std::string name, Address &address,
		   DIRECTION direction, int flgs,
		   const COMM_TYPE type) :
  CommBase(name, address, direction, type, flgs),
  mode(std::fstream::in | std::fstream::out) {
  if (!global_comm)
    init();
}

ADD_CONSTRUCTORS_DEF(FileComm)

void FileComm::init() {
  updateMaxMsgSize(0);
  bool created = address.address().empty();
  if (created) {
#ifdef _MSC_VER
    char key[L_tmpnam] = "yggXXXXXX";
    _mktemp(key);
    if (errno != 0) {
#else
    char key[L_tmpnam] = "yggXXXXXX";
    int fd = mkstemp(key);
    if (fd >= 0) {
      ::close(fd);
    } else {
#endif
      log_error() << "FileComm::init: Error generating temporary file name." << std::endl;
      throw std::runtime_error("FileComm::init: Error in std::mkstemp");
    }
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
  handle = new std::fstream(this->address.address().c_str(), mode);
  CommBase::init();
}

void FileComm::_close(bool call_base) {
  if (handle && !global_comm) {
    handle->close();
#ifdef YGG_TEST
    bool delete_file = true;
#else
    bool delete_file = ((direction == RECV) ||
			(!(flags & (COMM_FLAG_INTERFACE |
				    COMM_FLAG_WORKER))));
#endif
    if (delete_file)
      std::remove(this->address.address().c_str());
  }
  if (call_base)
    CommBase::_close(true);
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
  refresh();
  int out = -1;
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
  return out;
}

int FileComm::send_single(utils::Header& header) {
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
