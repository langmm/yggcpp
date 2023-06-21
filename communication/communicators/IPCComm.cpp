#include "IPCComm.hpp"
#include "rapidjson/document.h"
#include "rapidjson/writer.h"

using namespace communication::communicator;
using namespace communication::utils;

unsigned IPCComm::_yggChannelsUsed = 0;
int IPCComm::_yggChannelNames[_yggTrackChannels];
bool IPCComm::_ipc_rand_seeded = false;

#ifdef IPCINSTALLED

IPCComm::~IPCComm() {
    ygglog_debug << "~IPCComm: Started" << std::endl;
    if (handle && !global_comm) {
        if (direction == RECV) {
            remove_comm(true);
        } else {
#ifdef YGG_TEST
            remove_comm(true);
#else // YGG_TEST
            remove_comm(false);
#endif // YGG_TEST
        }
    }
    ygglog_debug << "~IPCComm: Finished" << std::endl;
}

/*!
  @brief Check if an IPC channel is in use
  @returns int <0 if the channel is already in use
 */
int IPCComm::check_key(int key) {
    // Fail if trying to re-use the same channel twice
    unsigned i;
    int error_code = 0;
#ifdef _OPENMP
#pragma omp critical (ipc)
  {
#endif
    for (i = 0; i < _yggChannelsUsed; i++ ) {
        if (IPCComm::_yggChannelNames[i] == key) {
	    error_code = -static_cast<int>(i);
            break;
        }
    }
    // Fail if > _yggTrackChannels channels used

#ifdef _OPENMP
    }
#endif
  return error_code;
}

/*!
  @brief Add a new channel to the list of existing channels.
*/

void IPCComm::add_channel() {
#ifdef _OPENMP
#pragma omp critical (ipc)
  {
#endif
  // printf("add_channel(%s): %d, %s\n", comm->name, _yggChannelsUsed, comm->address);
        if (IPCComm::_yggChannelsUsed++ >= _yggTrackChannels) {
	    ygglog_error << "Too many channels in use, max: " << _yggTrackChannels << std::endl;
        }
    IPCComm::_yggChannelNames[IPCComm::_yggChannelsUsed] = address->key();
#ifdef _OPENMP
  }
#endif
}

/*!
  @brief Remove a channel.
  @param[in] close_comm int If 1, the queue will be closed, otherwise it will
  just be removed from the register and it is assumed that another process
  will close it.
  @returns int -1 if removal not successful.
*/

int IPCComm::remove_comm(bool close_comm) {
    int ret;
    if (close_comm) {
        msgctl(handle[0], IPC_RMID, nullptr);
    }
    ret = -1;
    unsigned i;
    int ich = address->key();
#ifdef _OPENMP
#pragma omp critical (ipc)
  {
#endif
    for (i = 0; i < IPCComm::_yggChannelsUsed; i++) {
        if (ich == IPCComm::_yggChannelNames[i]) {
            memmove(IPCComm::_yggChannelNames + i, IPCComm::_yggChannelNames + i + 1,
                    (_yggTrackChannels - (i + 1))*sizeof(int));
            IPCComm::_yggChannelsUsed--;
            ret = 0;
            break;
        }
    }
    if (ret < 0) {
        ygglog_error << "remove_comm(" << name << "): Could not locate comm in register." << std::endl;
    }
#ifdef _OPENMP
    }
#endif
    return ret;
}

/*!
  @brief Get number of messages in the comm.
  @returns int Number of messages. -1 indicates an error.
 */
int IPCComm::comm_nmsg() const {
    if (global_comm)
      return global_comm->comm_nmsg();
    struct msqid_ds buf;
    if (!handle) {
        ygglog_error << "ipc_comm_nmsg: Queue handle is NULL." << std::endl;
        return -1;
    }

    int rc = msgctl(handle[0], IPC_STAT, &buf);
    if (rc != 0) {
        /* ygglog_error << "ipc_comm_nmsg: Could not access queue."); */
        return 0;
    }
    int ret = static_cast<int>(buf.msg_qnum);
    return ret;
}

/*!
  @brief Send a message to the comm.
  Send a message smaller than YGG_MSG_MAX bytes to an output comm. If the
  message is larger, it will not be sent.
  @param[in] data character pointer to message that should be sent.
  @returns int 0 if send succesfull, -1 if send unsuccessful.
 */
int IPCComm::send_single(const char* data, const size_t &len, const Header& head) {
    if (global_comm)
      return global_comm->send_single(data, len, head);
    ygglog_debug << "IPCComm(" << name << ")::send_single: " << len << " bytes" << std::endl;
    int ret = -1;
    msgbuf_t t;
    t.mtype = 1;
    memcpy(t.data, data, len);
    while (true) {
        ret = msgsnd(handle[0], &t, len, IPC_NOWAIT);
        ygglog_debug << "IPCComm(" << name << ")::send_single: msgsnd returned " << ret << std::endl;
        if (ret == 0) {
	    ret = static_cast<int>(len);
            break;
	}
        if ((ret == -1) && (errno == EAGAIN)) {
	    ygglog_debug << "IPCComm(" << name << ")::send_single: msgsnd, sleep" << std::endl;
            usleep(YGG_SLEEP_TIME);
        } else {
            struct msqid_ds buf;
            int rtrn = msgctl(handle[0], IPC_STAT, &buf);
            if ((rtrn == 0) && ((buf.msg_qnum + len) > buf.msg_qbytes)) {
	        ygglog_debug << "IPCComm(" << name << ")::send_single: msgsnd, queue full, sleep" << std::endl;
                usleep(YGG_SLEEP_TIME);
            } else {
	        ygglog_error << "IPCComm(" << name << ")::send_single: msgsnd(" << handle[0] << ", " << &t << ", " << len
                             << ", IPC_NOWAIT) ret(" << ret << "), errno(" << errno << "): " << strerror(errno) << std::endl;
                ret = -1;
                break;
            }
        }
    }
    return ret;
}

/*!
  @brief Receive a message from an input comm.
  Receive a message smaller than YGG_MSG_MAX bytes from an input comm.
  @param[out] data char ** pointer to allocated buffer where the message
  should be saved.
  @returns int -1 if message could not be received. Length of the received
    message if message was received.
 */
long IPCComm::recv_single(char*& data, const size_t& len, bool allow_realloc) {
    if (global_comm)
      return global_comm->recv_single(data, len, allow_realloc);
    ygglog_debug << "IPCComm(" << name << ")::recv_single:" << std::endl;
    msgbuf_t t;
    t.mtype = 1;
    t.data[0] = '\0';
    long ret = -1;
    while (true) {
        ret = msgrcv(handle[0], &t, maxMsgSize, 0, IPC_NOWAIT);
        if (ret == -1 && errno == ENOMSG) {
	    ygglog_debug << "IPCComm(" << name << ")::recv_single: no input, sleep" << std::endl;
            usleep(YGG_SLEEP_TIME);
        } else {
	    ygglog_debug << "IPCComm(" << name << ")::recv_single: received input: " << ret << " bytes" << std::endl;
            break;
        }
    }
    if (ret <= 0) {
        ygglog_debug << "IPCComm(" << name << ")::recv_single: msgrecv(" << handle << ", " << &t << ", " << maxMsgSize << ", 0, IPC_NOWAIT): "
                     << strerror(errno) << std::endl;
        return -1;
    }
    ret = this->copyData(data, len, t.data, ret, allow_realloc);
    if (ret < 0) {
      ygglog_error << "IPCComm(" << name << ")::recv_single: Error copying data" << std::endl;
      return ret;
    }
    ygglog_debug << "IPCComm(" << name << ")::recv_single: returns " << ret << " bytes" << std::endl;
    return ret;
}

void IPCComm::init() {
    updateMaxMsgSize(2048);
    int key = 0;
    bool created = ((!address) || address->address().empty());
    if (created) {
#ifdef _OPENMP
#pragma omp critical (ipc)
      {
#endif
	if (!_ipc_rand_seeded) {
	  std::srand(ptr2seed(this));
	  _ipc_rand_seeded = true;
	}
#ifdef _OPENMP
      }
#endif
      while (key == 0 || check_key(key) < 0) {
        key = std::rand();
      }
      if (!address) {
        address = new utils::Address(std::to_string(key));
      } else {
        address->address(std::to_string(key));
      }
    } else {
      key = this->address->key();
    }
    if (name.empty()) {
        this->name = "tempnewIPC." + this->address->address();
    } else {
        this->name = name;
    }
    int *fid = new int;
    if (created)
      fid[0] = msgget(key, (IPC_CREAT | 0777));
    else
      fid[0] = msgget(key, 0600);
    if (fid[0] < 0) {
        ygglog_error << "IPCComm::init: msgget(" << key << ") "
		     << "created(" << created << "), "
		     << "ret(" << fid[0] << "), errno(" << errno << "): "
		     << strerror(errno) << std::endl;
        delete fid;
	throw std::runtime_error("IPCComm::init: Error in msgget");
    }
    handle = fid;
    add_channel();
    ygglog_debug << "IPCComm(" << name << ")::init: address = " << this->address->address() << ", created = " << created << std::endl;
}

IPCComm::IPCComm(const std::string &name, Address *address,
		 DIRECTION direction, int flgs) :
  CommBase(name, address, direction, IPC_COMM, flgs) {
  if (!global_comm)
    init();
}

WORKER_METHOD_DEFS(IPCComm)

#else /*IPCINSTALLED*/

/*!
  @brief Print error message about IPC library not being installed.
 */
static inline
void ipc_install_error() {
  ygglog_throw_error("Compiler flag 'IPCINSTALLED' not defined so IPC bindings are disabled.");
};

IPCComm::~IPCComm() {
  // No error as constructor should have raised one
}

int IPCComm::check_key(int) {
    ipc_install_error();
    return -1;
}

void IPCComm::add_channel() {
    ipc_install_error();
}

int IPCComm::remove_comm(bool) {
    ipc_install_error();
    return -1;
}

int IPCComm::comm_nmsg() const {
    ipc_install_error();
    return -1;
}

int IPCComm::send_single(const char *, const size_t &, const Header&) {
    ipc_install_error();
    return -1;
}

long IPCComm::recv_single(char *&, const size_t &, bool) {
    ipc_install_error();
    return -1;
}

void IPCComm::init() {
    ipc_install_error();
}

IPCComm::IPCComm(const std::string &, utils::Address *address,
		 DIRECTION direction, int flgs) :
  CommBase(address, direction, IPC_COMM, flgs) {
    ipc_install_error();
}

IPCComm::IPCComm(const std::string &name, DIRECTION direction,
		 int flgs) :
  CommBase(name, direction, IPC_COMM, flgs) {
    ipc_install_error();
}

WORKER_METHOD_DUMMY(IPCComm, ipc)

#endif /*IPCINSTALLED*/
