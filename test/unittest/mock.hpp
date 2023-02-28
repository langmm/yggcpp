#pragma once
#include <sys/msg.h>
#include "zmq.hpp"

#define LIBRARY_ADDRESS_BY_HANDLE(dlhandle) ((NULL == dlhandle) ? NULL :  (void*)*(size_t const*)(dlhandle))
#define SUBLIB "/home/friedel/crops_in_silico/yggcpp/cmake-build-release/libYggInterface.so"

#define ELFHOOK(x) elf_hook(SUBLIB, LIBRARY_ADDRESS_BY_HANDLE(handle), #x, \
    (void*)communication::mock::x);

#define ELFHOOK_PARAM(x, y) elf_hook(SUBLIB, LIBRARY_ADDRESS_BY_HANDLE(handle), #x, \
    (void*)communication::mock::y);
#define ELFREVERT(x,y) y = elf_hook(SUBLIB, LIBRARY_ADDRESS_BY_HANDLE(handle), #x, y);

#define ELFHOOK_ARGS(x, a) elf_hook(SUBLIB, LIBRARY_ADDRESS_BY_HANDLE(handle), std::string(#x) + "|" + std::string(#a), \
    (void*)communication::mock::x);

#define ELFREVERT_ARGS(x, a, y) y = elf_hook(SUBLIB, LIBRARY_ADDRESS_BY_HANDLE(handle), #x + "|" + #a, \
    y);
namespace communication {
namespace mock {
//extern int mock_method_return_value;
//void setValue(const int val);

extern int RETVAL;
extern int SENDCOUNT;

int msgsnd(int a, const void* b, size_t c, int d);

int msgctl(int h, int flag, msqid_ds *buf);

int msgget(key_t a, int b);

ssize_t msgrcv(int a, void* buf, size_t msz, long mtype, int flags);

void* realloc(void* ptr, size_t size);

namespace boost {

}
namespace zmq {
void message_tD();
namespace message_t {
std::string to_string();
}
namespace poller_t {
size_t wait_all(std::vector<::zmq::poller_event<> > &events, const std::chrono::milliseconds timeout);
}
namespace detail {
namespace socket_base {

::zmq::detail::trivial_optional<size_t> send(::zmq::message_t &msg, ::zmq::send_flags flags);

::zmq::detail::trivial_optional<size_t> recv(::zmq::message_t &msg,
                                             ::zmq::recv_flags flags = ::zmq::recv_flags::none);

//std::string get(::zmq::sockopt::array_option<int, int>, size_t init_size = 1024);
template<class OutputIt>
::zmq::detail::trivial_optional<size_t> recv_multipart(::zmq::socket_ref s, OutputIt out,
                                                       ::zmq::recv_flags flags = ::zmq::recv_flags::none);
}
}
}
}
}