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
            remove_comm(false);
        }
        delete handle;
        handle = nullptr;
    }
}

int IPCComm::check_channels() {
    // Fail if name is empty
    if (name.empty()) {
        ygglog_error << "Cannot create channel with empty name.";
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
                         << address << ", i=" << i;
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
            ygglog_error << "Too many channels in use, max: " << _yggTrackChannels;
        }
    IPCComm::_yggChannelNames[IPCComm::_yggChannelsUsed] = address->key();
#ifdef _OPENMP
  }
#endif
}


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
        ygglog_error << "remove_comm(" << name << "): Could not locate comm in register.";
    }
#ifdef _OPENMP
    }
#endif
    return ret;
}

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
                     << "), errno(" << errno << "): " << strerror(errno);
        return false;
    }
    handle = fid;
    return true;
}

int IPCComm::comm_nmsg() const {
    struct msqid_ds buf;
    if (handle == nullptr) {
        ygglog_error << "ipc_comm_nmsg: Queue handle is NULL.";
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

int IPCComm::send(const char* data, const size_t &len) {
    ygglog_debug << "ipc_comm_send(" << name << "): " << len << " bytes";
    if (check_size(len))
        return send_normal(data, len);
    return send_large(data, len);
}

int IPCComm::send_normal(const char *data, const size_t &len) {
    int ret = -1;

    msgbuf_t t;
    t.mtype = 1;
    memcpy(t.data, data, len);
    while (true) {
        ret = msgsnd(handle[0], &t, len, IPC_NOWAIT);
        ygglog_debug << "ipc_comm_send(" << name << "): msgsnd returned " << ret;
        if (ret == 0)
            break;
        if ((ret == -1) && (errno == EAGAIN)) {
            ygglog_debug << "ipc_comm_send(" << name << "): msgsnd, sleep";
            usleep(YGG_SLEEP_TIME);
        } else {
            struct msqid_ds buf;
            int rtrn = msgctl(handle[0], IPC_STAT, &buf);
            if ((rtrn == 0) && ((buf.msg_qnum + len) > buf.msg_qbytes)) {
                ygglog_debug << "ipc_comm_send(" << name << "): msgsnd, queue full, sleep";
                usleep(YGG_SLEEP_TIME);
            } else {
                ygglog_error << "ipc_comm_send:  msgsend(" << handle[0] << ", " << &t << ", " << len
                             << ", IPC_NOWAIT) ret(" << ret << "), errno(" << errno << "): " << strerror(errno);
                ret = -1;
                break;
            }
        }
    }
    return ret;
}

int IPCComm::send_large(const char *data, const size_t &len) {
    int ret;
    size_t msgsiz = 0;
    char msg[YGG_MSG_MAX];
    sprintf(msg, "%ld", (long)(len));

    if ((ret = send_normal(msg, strlen(msg))) != 0) {
        ygglog_debug << "ipc_comm_send_nolimit(" << name << "): sending size of payload failed.";
        return ret;
    }
    size_t prev = 0;
    while (prev < len) {
        if ((len - prev) > YGG_MSG_MAX)
            msgsiz = YGG_MSG_MAX;
        else
            msgsiz = len - prev;
        ret = send_normal(data + prev, msgsiz);
        if (ret != 0) {
            ygglog_debug << "ipc_comm_send_nolimit(" << name << "): send interupted at " << prev
                         << " of " << len << " bytes.";
            break;
        }
        prev += msgsiz;
        ygglog_debug << "ipc_comm_send_nolimit(" << name << "): " << prev << " of " << len << " bytes sent";
    }

    if (ret == 0)
        ygglog_debug << "ipc_comm_send(" << name << "): returning " <<  ret;
    return ret;
}

 // TODO: Handle long messages
long IPCComm::recv(char* data, const size_t& len, bool allow_realloc) {
    ygglog_debug << "ipc_comm_recv(" << name << ")";
    msgbuf_t t;
    t.mtype = 1;
    long ret = -1;
    int len_recv = -1;
    while (true) {
        ret = msgrcv(handle[0], &t, YGG_MSG_MAX, 0, IPC_NOWAIT);
        if (ret == -1 && errno == ENOMSG) {
            ygglog_debug << "ipc_comm_recv(" << name << "): no input, sleep";
            usleep(YGG_SLEEP_TIME);
        } else {
            ygglog_debug << "ipc_comm_recv(" << name << "): received input: " << strlen(t.data) << " bytes, ret="
                         << ret;
            break;
        }
    }
    if (ret <= 0) {
        ygglog_debug << "ipc_comm_recv: msgrecv(" << handle << ", " << &t << ", " << YGG_MSG_MAX << ", 0, IPC_NOWAIT): "
                     << strerror(errno);
        return -1;
    }
    len_recv = ret + 1;
    if (len_recv > (int)len) {
        if (allow_realloc) {
            ygglog_debug << "ipc_comm_recv(" << name << "): reallocating buffer from " << len << " to "
                         << len_recv << " bytes.";
            data = (char*)realloc(data, len_recv);
            if (data == nullptr) {
                ygglog_error << "ipc_comm_recv(" << name << "): failed to realloc buffer.";
                return -1;
            }
        } else {
            ygglog_error << "ipc_comm_recv(" << name << "): buffer (" << len
                         << " bytes) is not large enough for message (" << len_recv << " bytes)";
            return -(len_recv - 1);
        }
    }
    memcpy(data, t.data, len_recv);
    data[len_recv - 1] = '\0';
    ret = len_recv - 1;
    ygglog_debug << "ipc_comm_recv(" << name << "): returns " << ret << " bytes";
    return ret;
}

void IPCComm::init() {
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
    int qkey = this->address->key();
    int *fid = new int;
    fid[0] = msgget(qkey, 0600);
    handle = fid;
}

IPCComm::IPCComm(const std::string &name, Address *address, DIRECTION direction) :
        CommBase(address, direction, IPC_COMM) {
    this->name = name;
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
    ygglog_error << "Compiler flag 'IPCINSTALLED' not defined so IPC bindings are disabled.";
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
