#pragma once
#ifdef IPCINSTALLED
#include <sys/msg.h>
#endif // IPCINSTALLED
#ifdef ZMQCPPINSTALLED
#include "zmq.hpp"
#endif // ZMQCPPINSTALLED
#ifdef ZMQINSTALLED
#include <zmq.h>
#endif // ZMQINSTALLED
#ifdef MPIINSTALLED
#include <mpi.h>
#endif // MPIINSTALLED

#define LIBRARY_ADDRESS_BY_HANDLE(dlhandle) ((NULL == dlhandle) ? NULL :  (void*)*(size_t const*)(dlhandle))

void init_sublib_contents();

#ifdef SUBLIBFILE
extern bool sublib_read;
extern char sublib_contents[256];
#define SUBLIB sublib_contents
#else
#define SUBLIB "/home/friedel/crops_in_silico/yggcpp/cmake-build-release/libYggInterface.so"
#endif

#define ELFHOOK(x) elf_hook(SUBLIB, LIBRARY_ADDRESS_BY_HANDLE(handle), #x, \
    (void*)communication::mock::x);
#define ELFHOOK_ALT(x) elf_hook(SUBLIB, LIBRARY_ADDRESS_BY_HANDLE(handle), #x, \
    (void*)communication::mock::alt_ ## x);

#define ELFHOOK_PARAM(x, y) elf_hook(SUBLIB, LIBRARY_ADDRESS_BY_HANDLE(handle), #x, \
    (void*)communication::mock::y);
#define ELFREVERT(x,y) y = elf_hook(SUBLIB, LIBRARY_ADDRESS_BY_HANDLE(handle), #x, y);

#define ELFHOOK_ARGS(x, a) elf_hook(SUBLIB, LIBRARY_ADDRESS_BY_HANDLE(handle), std::string(#x) + "|" + std::string(#a), \
    (void*)communication::mock::x);

#define ELFREVERT_ARGS(x, a, y) y = elf_hook(SUBLIB, LIBRARY_ADDRESS_BY_HANDLE(handle), std::string(#x) + "|" + std::string(#a), \
    y);
#define ELF_BEGIN						\
  init_sublib_contents();					\
  void *handle = dlopen(SUBLIB, RTLD_LAZY);			\
  if (!handle) {						\
    std::cerr << "ERROR in dlopen: " << dlerror() << std::endl;	\
    EXPECT_TRUE(false);						\
    throw std::exception();					\
  }
#define ELF_END					\
  dlclose(handle)
  
#define ELF_BEGIN_F(func)				\
  void *original_func_ ## func = nullptr;		\
  original_func_ ## func = ELFHOOK(func);		\
  EXPECT_NE(original_func_ ## func, ((void*)0))
#define ELF_BEGIN_ALT_F(func)				\
  void *original_func_ ## func = nullptr;		\
  original_func_ ## func = ELFHOOK_ALT(func);		\
  EXPECT_NE(original_func_ ## func, ((void*)0))
#define ELF_BEGIN_F_RET(func, ret)			\
  RETVAL = ret;						\
  ELF_BEGIN_F(func)
#define ELF_END_F(func)				\
  ELFREVERT(func, original_func_ ## func)

namespace communication {
namespace mock {
  
#ifdef ELF_AVAILABLE

//extern int mock_method_return_value;
//void setValue(const int val);

extern int RETVAL;
extern int RETVAL_INC_SEND;
extern int RETVAL_INC_RECV;
extern int RETVAL_INC_POLL;
extern int SENDCOUNT;
extern int MPISTATUS;
extern bool MPICANCEL;
extern std::string RETMSG;

char *getenv(const char *name);

#ifdef IPCINSTALLED
int msgsnd(int a, const void* b, size_t c, int d);

int msgctl(int h, int flag, msqid_ds *buf);

int msgget(key_t a, int b);

ssize_t msgrcv(int a, void* buf, size_t msz, long mtype, int flags);
#endif // IPCINSTALLED
  

void* realloc(void* ptr, size_t size);

namespace boost {

}
#ifdef ZMQCPPINSTALLED
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
#endif // ZMQCPPINSTALLED

#ifdef ZMQINSTALLED
  int zmq_sendmsg (void *, zmq_msg_t *msg, int);
  int zmq_recvmsg (void *, zmq_msg_t *msg, int);
#ifdef ZMQ_HAVE_POLLER  
  int zmq_poller_wait_all (void *, zmq_poller_event_t *, int, long);
#else // ZMQ_HAVE_POLLER
  int zmq_poll (zmq_pollitem_t *, int nitems, long);
#endif // ZMQ_HAVE_POLLER
  int zmq_errno (void);
  // Only testing for errors
  int zmq_ctx_term (void *);
  void *zmq_socket (void *, int);
  int zmq_connect (void *, const char *);
  int zmq_bind (void *, const char *);
  int zmq_msg_init (zmq_msg_t *);
  int alt_zmq_msg_init_size (zmq_msg_t *, size_t);
  int zmq_setsockopt (void *, int, const void *, size_t);
  int zmq_getsockopt (void *, int, void * option_value, size_t *option_len);
#endif // ZMQINSTALLED
  
#endif // ELF_AVAILABLE
}
}
