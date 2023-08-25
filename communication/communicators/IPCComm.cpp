#include "IPCComm.hpp"
#include "rapidjson/document.h"
#include "rapidjson/writer.h"

using namespace communication::communicator;
using namespace communication::utils;

unsigned IPCComm::_yggChannelsUsed = 0;
int IPCComm::_yggChannelNames[_yggTrackChannels];
bool IPCComm::_ipc_rand_seeded = false;

IPCComm::IPCComm(const std::string name, Address *address,
		 DIRECTION direction, int flgs,
		 const COMM_TYPE type) :
  CommBase(name, address, direction, type, flgs) {
  if (!global_comm)
    init();
}

ADD_CONSTRUCTORS_DEF(IPCComm)

#ifdef IPCINSTALLED

void IPCComm::init() {
    updateMaxMsgSize(2048);
    int key = 0;
    bool created = ((!address) || address->address().empty());
    if (created) {
      YGG_THREAD_SAFE_BEGIN(ipc) {
	if (!_ipc_rand_seeded) {
	  std::srand(ptr2seed(this));
	  _ipc_rand_seeded = true;
	}
      } YGG_THREAD_SAFE_END;
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
    CommBase::init();
}

void IPCComm::close() {
    ygglog_debug << "IPCComm::close: Started" << std::endl;
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
    ygglog_debug << "IPCComm::close: Finished" << std::endl;
    CommBase::close();
}

/*!
  @brief Check if an IPC channel is in use
  @returns int <0 if the channel is already in use
 */
int IPCComm::check_key(int key) {
    // Fail if trying to re-use the same channel twice
    unsigned i;
    int error_code = 0;
    YGG_THREAD_SAFE_BEGIN(ipc) {
    for (i = 0; i < _yggChannelsUsed; i++ ) {
        if (IPCComm::_yggChannelNames[i] == key) {
	    error_code = -static_cast<int>(i);
            break;
        }
    }
    // Fail if > _yggTrackChannels channels used

    } YGG_THREAD_SAFE_END;
    return error_code;
}

/*!
  @brief Add a new channel to the list of existing channels.
*/

void IPCComm::add_channel() {
  YGG_THREAD_SAFE_BEGIN(ipc) {
  // printf("add_channel(%s): %d, %s\n", comm->name, _yggChannelsUsed, comm->address);
    if (IPCComm::_yggChannelsUsed++ >= _yggTrackChannels) {
      ygglog_error << "Too many channels in use, max: " << _yggTrackChannels << std::endl;
    }
    IPCComm::_yggChannelNames[IPCComm::_yggChannelsUsed] = address->key();
  } YGG_THREAD_SAFE_END;
}

/*!
  @brief Remove a channel.
  @param[in] close_comm int If 1, the queue will be closed, otherwise it will
  just be removed from the register and it is assumed that another process
  will close it.
  @returns int -1 if removal not successful.
*/

int IPCComm::remove_comm(bool close_comm) {
    int ret = 0;
    if (close_comm) {
        ygglog_debug << "IPCComm(" << name << ")::Closing queue: " << handle[0] << std::endl;
        msgctl(handle[0], IPC_RMID, nullptr);
    }
    // ret = -1;
    unsigned i;
    int ich = address->key();
    YGG_THREAD_SAFE_BEGIN(ipc) {
      for (i = 0; i < IPCComm::_yggChannelsUsed; i++) {
        if (ich == IPCComm::_yggChannelNames[i]) {
	  memmove(IPCComm::_yggChannelNames + i, IPCComm::_yggChannelNames + i + 1,
		  (_yggTrackChannels - (i + 1))*sizeof(int));
	  IPCComm::_yggChannelsUsed--;
	  ret = 0;
	  break;
        }
      }
      // This likely occurs when both the send & receive comms are called
      //   from the same process
      // if (ret < 0) {
      //     ygglog_debug << "remove_comm(" << name << "): Could not locate comm in register." << std::endl;
      // }
    } YGG_THREAD_SAFE_END;
    return ret;
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
        /* ygglog_error << "ipc_comm_nmsg: Could not access queue."); */
        return 0;
    }
    int ret = static_cast<int>(buf.msg_qnum);
    return ret;
}

int IPCComm::send_single(utils::Header& header) {
    assert(!global_comm);
    header.on_send();
    ygglog_debug << "IPCComm(" << name << ")::send_single: " << header.size_msg << " bytes" << std::endl;
    int ret = -1;
    msgbuf_t t;
    t.mtype = 1;
    memcpy(t.data, header.data_msg(), header.size_msg);
    while (true) {
        ret = msgsnd(handle[0], &t, header.size_msg, IPC_NOWAIT);
        ygglog_debug << "IPCComm(" << name << ")::send_single: msgsnd returned " << ret << std::endl;
        if (ret == 0) {
	    ret = static_cast<int>(header.size_msg);
            break;
	}
        if ((ret == -1) && (errno == EAGAIN)) {
	    ygglog_debug << "IPCComm(" << name << ")::send_single: msgsnd, sleep" << std::endl;
	    std::this_thread::sleep_for(std::chrono::microseconds(YGG_SLEEP_TIME));
        } else { // GCOVR_EXCL_LINE
            struct msqid_ds buf;
            int rtrn = msgctl(handle[0], IPC_STAT, &buf);
            if ((rtrn == 0) && ((buf.msg_qnum + header.size_msg) > buf.msg_qbytes)) {
	        ygglog_debug << "IPCComm(" << name << ")::send_single: msgsnd, queue full, sleep" << std::endl;
		std::this_thread::sleep_for(std::chrono::microseconds(YGG_SLEEP_TIME));
            } else { // GCOVR_EXCL_LINE
	        ygglog_error << "IPCComm(" << name << ")::send_single: msgsnd(" << handle[0] << ", " << &t << ", " << header.size_msg
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
    ygglog_debug << "IPCComm(" << name << ")::recv_single:" << std::endl;
    msgbuf_t t;
    t.mtype = 1;
    t.data[0] = '\0';
    long ret = -1;
    while (true) {
        ret = msgrcv(handle[0], &t, maxMsgSize, 0, IPC_NOWAIT);
        if (ret == -1 && errno == ENOMSG) {
	    ygglog_debug << "IPCComm(" << name << ")::recv_single: no input, sleep" << std::endl;
	    std::this_thread::sleep_for(std::chrono::microseconds(YGG_SLEEP_TIME));
        } else { // GCOVR_EXCL_LINE
	    ygglog_debug << "IPCComm(" << name << ")::recv_single: received input: " << ret << " bytes" << std::endl;
            break;
        }
    }
    if (ret <= 0) {
        ygglog_debug << "IPCComm(" << name << ")::recv_single: msgrecv(" << handle << ", " << &t << ", " << maxMsgSize << ", 0, IPC_NOWAIT): "
                     << strerror(errno) << std::endl;
        return -1;
    }
    ret = header.on_recv(t.data, ret);
    if (ret < 0) {
      ygglog_error << "IPCComm(" << name << ")::recv_single: Error copying data" << std::endl;
      return ret;
    }
    ygglog_debug << "IPCComm(" << name << ")::recv_single: returns " << ret << " bytes" << std::endl;
    return ret;
}

WORKER_METHOD_DEFS(IPCComm)

#endif /*IPCINSTALLED*/
