#include <cstdlib>
#include <iostream>
#include <sys/msg.h>
#include "mock.hpp"
#include "communicators/IPCComm.hpp"

namespace communication {
namespace mock {
int RETVAL = 0;
int SENDCOUNT = 0;


int msgsnd(int a, const void *b, size_t c, int d) {
    //std::cout << "HERE";
    //msgsnd(a, b, c, d);
    SENDCOUNT++;
    return RETVAL;
}

int msgctl(int h, int flag, msqid_ds *buf) {
    if (buf == nullptr)
        return 0;
    buf->msg_qnum = 10000;
    buf->msg_qbytes = 1000;
    RETVAL++;
    return RETVAL + 1;
}

int msgget(key_t a, int b) {
    return -1;
}

ssize_t msgrcv(int a, void* rbuf, size_t msz, long mtype, int flags) {
    std::string msg = "Hello world";
    memcpy(static_cast<communicator::msgbuf_t*>(rbuf)->data, msg.c_str(), msg.size());
    if (RETVAL < 0)
        return RETVAL;
    return msg.size();
}

void* realloc(void* ptr, size_t size) {
    return nullptr;
}
namespace zmq {
void message_tD() {
        return;
}
namespace message_t {
std::string to_string() {
    return "Hello";
}
}
namespace poller_t {
size_t wait_all(std::vector<::zmq::poller_event<> > &events, const std::chrono::milliseconds timeout) {
    return 2;
}

}
namespace detail {
namespace socket_base {

::zmq::detail::trivial_optional<size_t> send(::zmq::message_t &msg, ::zmq::send_flags flags) {
    ::zmq::detail::trivial_optional<size_t> ret(0);
    return ret;
}

::zmq::detail::trivial_optional<size_t> recv(::zmq::message_t &msg,
                                             ::zmq::recv_flags flags) {
    ::zmq::detail::trivial_optional<size_t> ret(0);
    return ret;
}

std::string get() {
    return "127.0.0.1:8888";
}

template<class OutputIt>
::zmq::detail::trivial_optional<size_t> recv_multipart(::zmq::socket_ref s, OutputIt out,
                                                       ::zmq::recv_flags flags) {
    ::zmq::detail::trivial_optional<size_t> ret(1);
    return ret;

}
}
}
}

}
}


/*int communication::mock::mock_method_return_value = 0;

void communication::mock::setValue(const int val) {
    mock_method_return_value = val;
}
#ifdef __cplusplus
extern "C" {
#endif

int __wrap_msgsnd(int id, const void* data, size_t len, int flags) {
    std::cout << std::endl << "Mock Called" << std::endl;
    return communication::mock::mock_method_return_value;
}

int __wrap_msgctl(int id, int flg,  msqid_ds *buf) {
    if (buf == nullptr)
        return 0;
    buf->msg_qnum = 5;
    return communication::mock::mock_method_return_value;
}
int __wrap_msgget(key_t key, int flags) {
    //__real_msgget(key, flags);
    return communication::mock::mock_method_return_value;
}
#ifdef __cplusplus
}
#endif*/