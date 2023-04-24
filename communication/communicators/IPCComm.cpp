#include "IPCComm.hpp"
#include "rapidjson/document.h"
#include "rapidjson/writer.h"

using namespace communication::communicator;
using namespace communication::utils;
//using namespace communication::datatypes;

unsigned IPCComm::_yggChannelsUsed = 0;
int IPCComm::_yggChannelNames[_yggTrackChannels];
bool IPCComm::_ipc_rand_seeded = false;
#ifdef _YGGIPC

IPCComm::~IPCComm() {
    if (handle != nullptr) {
        if (direction == RECV) {
            remove_comm(true);
        } else {
#ifdef YGG_TEST
            remove_comm(true);
#else // YGG_TEST
            remove_comm(false);
#endif // YGG_TEST
        }
        delete handle;
        handle = nullptr;
    }
}

/*!
  @brief Check if an IPC channel can be initialized.
  @returns int -1 if the channel can't be initialized.
 */
int IPCComm::check_channels() {
    // Fail if name is empty
    if (name.empty()) {
        ygglog_error << "Cannot create channel with empty name." << std::endl;
        return -1;
    }
    // Fail if trying to re-use the same channel twice
    unsigned i;
    int error_code = 0;
#ifdef _OPENMP
#pragma omp critical (ipc)
  {
#endif
    for (i = 0; i < _yggChannelsUsed; i++ ) {
        if (IPCComm::_yggChannelNames[i] == address->key()) {
            ygglog_error << "Attempt to re-use channel: name=" << name << ", key="
                         << address << ", i=" << i << std::endl;
            error_code = -1;
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
  @brief Create a new channel.
  @returns int -1 if the address could not be created.
*/
bool IPCComm::new_address() {
    int ret;
    // TODO: small chance of reusing same number
    int key = 0;
#ifdef _OPENMP
#pragma omp critical (ipc)
  {
#endif
    if (!_ipc_rand_seeded) {
        srand(ptr2seed(this));
        _ipc_rand_seeded = true;
    }
#ifdef _OPENMP
    }
#endif
    while (key == 0) {
        key = std::rand();
    }
    if (address == nullptr) {
        address = new utils::Address(std::to_string(key));
    } else {
        address->address(std::to_string(key));
    }

    if (name.empty()) {
        name = "tempnewIPC." + std::to_string(key);
    } else {
        ret = check_channels();
        if (ret < 0)
            return false;
    }
    //std::string temp = std::to_string(key);
    //temp = "HELLO";
    int *fid = new int;
    fid[0] = msgget(key, (IPC_CREAT | 0777));
    if (fid[0] < 0) {
        ygglog_error << "new_ipc_address: msgget(" << key << ", " << IPC_CREAT << " | 0777) ret(" << fid[0]
                     << "), errno(" << errno << "): " << strerror(errno) << std::endl;
        return false;
    }
    handle = fid;
    return true;
}

/*!
  @brief Get number of messages in the comm.
  @returns int Number of messages. -1 indicates an error.
 */
int IPCComm::comm_nmsg() const {
    struct msqid_ds buf;
    if (handle == nullptr) {
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
int IPCComm::send_single(const char* data, const size_t &len) {
    ygglog_debug << "IPCComm(" << name << ")::send_single: " << len << " bytes" << std::endl;
    if (!check_size(len)) {
      ygglog_error << "IPCComm(" << name << ")::send_single: Message too large" << std::endl;
      return -1;
    }
    int ret = -1;
    msgbuf_t t;
    t.mtype = 1;
    memcpy(t.data, data, len);
    while (true) {
        ret = msgsnd(handle[0], &t, len, IPC_NOWAIT);
        ygglog_debug << "IPCComm(" << name << ")::send_single: msgsnd returned " << ret << std::endl;
        if (ret == 0)
            break;
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
	        ygglog_error << "IPCComm(" << name << ")::send_single: msgsend(" << handle[0] << ", " << &t << ", " << len
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
    ygglog_debug << "IPCComm(" << name << ")::recv_single:" << std::endl;
    msgbuf_t t;
    t.mtype = 1;
    long ret = -1;
    while (true) {
        ret = msgrcv(handle[0], &t, YGG_MSG_MAX, 0, IPC_NOWAIT);
        if (ret == -1 && errno == ENOMSG) {
	    ygglog_debug << "IPCComm(" << name << ")::recv_single: no input, sleep" << std::endl;
            usleep(YGG_SLEEP_TIME);
        } else {
            ygglog_debug << "IPCComm(" << name << ")::recv_single: received input: " << strlen(t.data) << " bytes, ret="
                         << ret << std::endl;
            break;
        }
    }
    if (ret <= 0) {
        ygglog_debug << "IPCComm(" << name << ")::recv_single: msgrecv(" << handle << ", " << &t << ", " << YGG_MSG_MAX << ", 0, IPC_NOWAIT): "
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
    maxMsgSize = 2048;
    if (address == nullptr)
        if (!new_address())
            throw std::runtime_error("No valid address generated");
    if (name.empty()) {
        this->name = "tempnewIPC." + this->address->address();
    } else {
        this->name = name;
        if (check_channels() < 0)
            throw std::runtime_error("Check channels failed");
    }
    add_channel();
    if (!handle) {
      int qkey = this->address->key();
      int *fid = new int;
      fid[0] = msgget(qkey, 0600);
      handle = fid;
    }
}

IPCComm::IPCComm(const std::string &name, Address *address, DIRECTION direction) :
        CommBase(address, direction, IPC_COMM) {
    this->name = name;
    init();
}
IPCComm::IPCComm(const std::string &name, DIRECTION direction) :
        CommBase(name, direction, IPC_COMM) {
    init();
}

/*IPCComm::IPCComm(communication::Comm_t *comm) : CommBase(comm, IPC_COMM){
    if (handle == nullptr) {
        add_channel();
        int qkey = this->address->key();
        int *fid = new int;
        fid[0] = msgget(qkey, 0600);
        handle = fid;
    }
}*/
#else /*IPCINSTALLED*/

/*!
  @brief Print error message about IPC library not being installed.
 */
static inline
void ipc_install_error() {
  ygglog_error << "Compiler flag 'IPCINSTALLED' not defined so IPC bindings are disabled." << std::endl;
};

//IPCComm::IPCComm() {
//ipc_install_error();
//}
IPCComm::~IPCComm() {
    ipc_install_error();
}

int IPCComm::check_channels() {
    ipc_install_error();
    return -1;
}

void IPCComm::add_channel() {
    ipc_install_error();
}

int IPCComm::remove_comm(bool close_comm) {
    // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
    UNUSED(close_comm);
#endif
    ipc_install_error();
    return -1;
}

int IPCComm::new_address() {
    ipc_install_error();
    return -1;
}

int IPCComm::comm_nmsg() const {
    ipc_install_error();
    return -1;
}

int IPCComm::send(const char *data, const size_t &len) {
    // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
    UNUSED(data);
#endif
    ipc_install_error();
    return -1;
}

int IPCComm::send(const dtype_t* dtype) {
    // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
    UNUSED(data);
#endif
    ipc_install_error();
    return -1;
}

long IPCComm::recv(dtype_t* dtype) {
    // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
    UNUSED(data);
#endif
    ipc_install_error();
    return -1;
}

long IPCComm::recv(char **data, const size_t &len, bool allow_realloc) {
    // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
    UNUSED(data);
#endif
    ipc_install_error();
    return -1;
}

int IPCComm::send_large(const char* data, const size_t &len) {
    // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
    UNUSED(data);
#endif
    ipc_install_error();
    return -1;
}

void IPCComm::init() {
    ipc_install_error();
}

IPCComm::IPCComm(const std::string &name, utils::Address *address, DIRECTION direction) :
        CommBase(address, direction, IPC_COMM) {
    ipc_install_error();
}

int IPCComm::send_normal(const char* data, const size_t &len) {
    // Prevent C4100 warning on windows by referencing param
#ifdef _WIN32
    UNUSED(data);
#endif
    ipc_install_error();
    return -1;
}


#endif /*IPCINSTALLED*/
