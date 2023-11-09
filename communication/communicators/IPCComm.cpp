#include "IPCComm.hpp"
#include "rapidjson/document.h"
#include "rapidjson/writer.h"

using namespace communication::communicator;
using namespace communication::utils;

IPCComm::IPCComm(const std::string name, Address *address,
		 DIRECTION direction, int flgs,
		 const COMM_TYPE type) :
  CommBase(name, address, direction, type, flgs) {
  if (!global_comm)
    init();
}

ADD_CONSTRUCTORS_DEF(IPCComm)

#ifdef IPCINSTALLED

int IPCComm::count_queues() {
  std::shared_ptr<FILE> pipe(popen("ipcs -q", "r"), pclose);
  int out = 0;
  char c[2];
  if (!pipe)
    YggLogThrowError("ERROR getting ipcs count");
  while (pipe.get() && !feof(pipe.get())) {
    if (fgets(c, 2, pipe.get()) != NULL) {
      if (c[0] == '\n')
	out++;
    } else {
      break;
    }
  }
  return out;
}

void IPCComm::init() {
    updateMaxMsgSize(2048);
    int key = 0;
    bool created = ((!address) || address->address().empty());
    if (created) {
      CREATE_KEY(IPCComm);
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
      log_error() << "IPCComm::init: msgget(" << key << ") "
		  << "created(" << created << "), "
		  << "ret(" << fid[0] << "), errno(" << errno << "): "
		  << strerror(errno) << std::endl;
      delete fid;
      fid = nullptr;
      throw std::runtime_error("IPCComm::init: Error in msgget");
    }
    handle = fid;
    track_key(address->key());
    log_debug() << "init: address = " << this->address->address() << ", created = " << created << std::endl;
    CommBase::init();
}

void IPCComm::_close(bool call_base) {
    if (handle && !global_comm) {
#ifdef YGG_TEST
      bool close_comm = true;
#else
      bool close_comm = ((direction == RECV) ||
			 (!(flags & (COMM_FLAG_INTERFACE |
				     COMM_FLAG_WORKER))));
#endif
      remove_comm(close_comm);
    }
    if (call_base)
      CommBase::_close(true);
}

ADD_KEY_TRACKER_DEFS(IPCComm)

/*!
  @brief Remove a channel.
  @param[in] close_comm int If 1, the queue will be closed, otherwise it will
  just be removed from the register and it is assumed that another process
  will close it.
  @returns int -1 if removal not successful.
*/

int IPCComm::remove_comm(bool close_comm) {
    if (close_comm) {
        log_debug() << "Closing queue: " << handle[0] << std::endl;
        msgctl(handle[0], IPC_RMID, nullptr);
    }
    return untrack_key(address->key());
}

int IPCComm::comm_nmsg(DIRECTION dir) const {
    if (global_comm)
      return global_comm->comm_nmsg(dir);
    if (dir == NONE)
      dir = direction;
    if (dir != direction)
      return 0;
    struct msqid_ds buf;
    assert(handle);

    int rc = msgctl(handle[0], IPC_STAT, &buf);
    if (rc != 0) {
        /* log_error() << "comm_nmsg: Could not access queue." << std::endl; */
        return 0;
    }
    int ret = static_cast<int>(buf.msg_qnum);
    return ret;
}

int IPCComm::send_single(utils::Header& header) {
    assert(!global_comm);
    if (header.on_send() < 0)
      return -1;
    log_debug() << "send_single: " << header.size_msg << " bytes" << std::endl;
    int ret = -1;
    msgbuf_t t;
    t.mtype = 1;
    memcpy(t.data, header.data_msg(), header.size_msg);
    while (true) {
        ret = msgsnd(handle[0], &t, header.size_msg, IPC_NOWAIT);
        log_debug() << "send_single: msgsnd returned " << ret << std::endl;
        if (ret == 0) {
	    ret = static_cast<int>(header.size_msg);
            break;
	}
        if ((ret == -1) && (errno == EAGAIN)) {
	    log_debug() << "send_single: msgsnd, sleep" << std::endl;
	    THREAD_USLEEP(YGG_SLEEP_TIME);
        } else { // GCOVR_EXCL_LINE
            struct msqid_ds buf;
            int rtrn = msgctl(handle[0], IPC_STAT, &buf);
            if ((rtrn == 0) && ((buf.msg_qnum + header.size_msg) > buf.msg_qbytes)) {
	        log_debug() << "send_single: msgsnd, queue full, sleep" << std::endl;
		THREAD_USLEEP(YGG_SLEEP_TIME);
            } else { // GCOVR_EXCL_LINE
	        log_error() << "send_single: msgsnd(" << handle[0] << ", " << &t << ", " << header.size_msg
                             << ", IPC_NOWAIT) ret(" << ret << "), errno(" << errno << "): " << strerror(errno) << std::endl;
                ret = -1;
                break;
            }
        }
    }
    return ret;
}

long IPCComm::recv_single(utils::Header& header) {
    // Should never be called with global comm
    // if (global_comm)
    //   return global_comm->recv_single(data, len, allow_realloc);
    assert(!global_comm);
    log_debug() << "recv_single:" << std::endl;
    msgbuf_t t;
    t.mtype = 1;
    t.data[0] = '\0';
    long ret = -1;
    while (true) {
        ret = msgrcv(handle[0], &t, maxMsgSize, 0, IPC_NOWAIT);
        if (ret == -1 && errno == ENOMSG) {
	    log_debug() << "recv_single: no input, sleep" << std::endl;
	    THREAD_USLEEP(YGG_SLEEP_TIME);
        } else { // GCOVR_EXCL_LINE
	    log_debug() << "recv_single: received input: " << ret << " bytes" << std::endl;
            break;
        }
    }
    if (ret <= 0) {
        log_debug() << "recv_single: msgrecv(" << handle << ", " << &t << ", " << maxMsgSize << ", 0, IPC_NOWAIT): "
                     << strerror(errno) << std::endl;
        return -1;
    }
    ret = header.on_recv(t.data, ret);
    if (ret < 0) {
      log_error() << "recv_single: Error copying data" << std::endl;
      return ret;
    }
    log_debug() << "recv_single: returns " << ret << " bytes" << std::endl;
    return ret;
}

WORKER_METHOD_DEFS(IPCComm)

#else  /*IPCINSTALLED*/

void IPCComm::_close(bool call_base) {
  if (call_base)
    CommBase::_close(true);
}

#endif /*IPCINSTALLED*/
