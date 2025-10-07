#pragma once

#if defined(RESTINSTALLED) && defined(__MINGW32__)
// Ensure winsock2.h is included before windows.h included by curl
#include <winsock2.h>
#endif

#ifndef _WIN32
#include <errno.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/shm.h>
#endif

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
#ifdef RMQINSTALLED
#ifdef YGG_RMQ_NOINCLUDEDIR
#include <amqp.h>
#include <amqp_tcp_socket.h>
#else // YGG_RMQ_NOINCLUDEDIR
#include <rabbitmq-c/amqp.h>
#include <rabbitmq-c/tcp_socket.h>
#endif // YGG_RMQ_NOINCLUDEDIR
#endif // RMQINSTALLED

#include <string>

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
    (void*)YggInterface::mock::x);
#define ELFHOOK_ALT(x) elf_hook(SUBLIB, LIBRARY_ADDRESS_BY_HANDLE(handle), #x, \
    (void*)YggInterface::mock::alt_ ## x);

#define ELFHOOK_PARAM(x, y) elf_hook(SUBLIB, LIBRARY_ADDRESS_BY_HANDLE(handle), #x, \
    (void*)YggInterface::mock::y);
#define ELFREVERT(x,y) y = elf_hook(SUBLIB, LIBRARY_ADDRESS_BY_HANDLE(handle), #x, y);

#define ELFHOOK_ARGS(x, a) elf_hook(SUBLIB, LIBRARY_ADDRESS_BY_HANDLE(handle), std::string(#x) + "|" + std::string(#a), \
    (void*)YggInterface::mock::x);

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
#define ELF_BEGIN_ORIG_F(func)						\
  void *original_func_ ## func = nullptr;				\
  void *dummy_func_ ## func = nullptr;					\
  original_func_ ## func = ELFHOOK(func);				\
  EXPECT_NE(original_func_ ## func, ((void*)0));			\
  dummy_func_ ## func = ELFREVERT(func ## _orig,			\
				  original_func_ ## func);		\
  EXPECT_NE(dummy_func_ ## func, ((void*)0))
#define ELF_END_ORIG_F(func)				\
  ELFREVERT(func ## _orig, dummy_func_ ## func);	\
  ELFREVERT(func, *original_func_ ## func)

namespace YggInterface {
namespace mock {

#ifdef ELF_AVAILABLE

//extern int mock_method_return_value;
//void setValue(const int val);

extern int RETVAL;
extern int RETVAL_CREATE;
extern int RETVAL_INC_SEND;
extern int RETVAL_INC_RECV;
extern int RETVAL_INC_POLL;
extern int SENDCOUNT;
extern std::string RETMSG;
extern std::string RETMSG_META;
extern std::string RETMSG_META_DEFAULT;
#ifdef RMQINSTALLED
extern amqp_response_type_enum AMQP_REPLY_TYPE;
extern int AMQP_ERROR;
#define ELF_SET_RMQ_ERROR(reply_type, error)	\
  AMQP_REPLY_TYPE = reply_type;			\
  AMQP_ERROR = error
#endif // RMQINSTALLED

std::string _mock_message();
  
#ifdef ZMQINSTALLED
#define ELF_REPLACE_RECV_ZMQ			\
  RETVAL = -1;					\
  RETVAL_INC_POLL = 0;				\
  RETVAL_INC_RECV = 0;				\
  ELF_BEGIN_F(zmq_recvmsg)
#define ELF_RESTORE_RECV_ZMQ			\
  ELF_END_F(zmq_recvmsg)
#define ELF_REPLACE_SEND_ZMQ			\
  SENDCOUNT = 0;				\
  RETVAL = -1;					\
  RETVAL_INC_POLL = 0;				\
  RETVAL_INC_SEND = 0;				\
  ELF_BEGIN_F(zmq_sendmsg)
#define ELF_RESTORE_SEND_ZMQ			\
  ELF_END_F(zmq_sendmsg)
#define ELF_META_ZMQ(comm)			\
  {						\
    EXPECT_TRUE(comm.genMetadata(RETMSG_META));	\
  }
#define ELF_REPLACE_CREATE_ZMQ			\
  ELF_BEGIN_F(zmq_socket)
#define ELF_RESTORE_CREATE_ZMQ			\
  ELF_END_F(zmq_socket)
#ifdef ZMQ_HAVE_POLLER
#define ELF_REPLACE_NMSG_ZMQ			\
  ELF_BEGIN_F(zmq_poller_wait_all)
#define ELF_RESTORE_NMSG_ZMQ			\
  ELF_END_F(zmq_poller_wait_all)
#else
#define ELF_REPLACE_NMSG_ZMQ			\
  ELF_BEGIN_F(zmq_poll)
#define ELF_RESTORE_NMSG_ZMQ			\
  ELF_END_F(zmq_poll)
#endif
#endif

#ifdef IPCINSTALLED
#define ELF_REPLACE_RECV_IPC			\
  RETVAL = -1;					\
  RETVAL_INC_POLL = 0;				\
  RETVAL_INC_RECV = 0;				\
  ELF_BEGIN_F(msgrcv)
#define ELF_RESTORE_RECV_IPC			\
  ELF_END_F(msgrcv)
#define ELF_REPLACE_SEND_IPC			\
  SENDCOUNT = 0;				\
  RETVAL = -1;					\
  RETVAL_INC_POLL = 0;				\
  RETVAL_INC_SEND = 0;				\
  ELF_BEGIN_F(msgsnd)
#define ELF_RESTORE_SEND_IPC			\
  ELF_END_F(msgsnd)
#define ELF_REPLACE_NMSG_IPC			\
  ELF_BEGIN_F(msgctl)
#define ELF_RESTORE_NMSG_IPC			\
  ELF_END_F(msgctl)
#define ELF_META_IPC(comm)
#define ELF_REPLACE_CREATE_IPC			\
  ELF_BEGIN_F(msgget)
#define ELF_RESTORE_CREATE_IPC			\
  ELF_END_F(msgget)
#endif
  
#if defined(MPIINSTALLED) && defined(MPI_COMM_WORLD)
#define ELF_REPLACE_RECV_MPI			\
  RETVAL = -1;					\
  RETVAL_INC_POLL = 0;				\
  RETVAL_INC_RECV = 0;				\
  ELF_BEGIN_F(MPI_Recv)
#define ELF_RESTORE_RECV_MPI			\
  ELF_END_F(MPI_Recv)
#define ELF_REPLACE_SEND_MPI			\
  SENDCOUNT = 0;				\
  RETVAL = -1;					\
  RETVAL_INC_POLL = 0;				\
  RETVAL_INC_SEND = 0;				\
  ELF_BEGIN_F(MPI_Send)
#define ELF_RESTORE_SEND_MPI			\
  ELF_END_F(MPI_Send)
#define ELF_REPLACE_NMSG_MPI			\
  ELF_BEGIN_F(MPI_Probe)
#define ELF_RESTORE_NMSG_MPI			\
  ELF_END_F(MPI_Probe)
#define ELF_META_MPI(comm)
#endif

#define ELF_SET_SUCCESS				\
  RETVAL = 0;					\
  RETVAL_INC_POLL = 0;				\
  RETVAL_INC_SEND = 0;				\
  RETVAL_INC_RECV = 0
#define ELF_SET_FAILURE				\
  RETVAL = -1;					\
  RETVAL_INC_POLL = 0;				\
  RETVAL_INC_SEND = 0;				\
  RETVAL_INC_RECV = 0
  
char *alt_getenv(const char *name);

#ifdef IPCINSTALLED
int msgsnd(int a, const void* b, size_t c, int d);

int msgctl(int h, int flag, msqid_ds *buf);

int msgget(key_t a, int b);

ssize_t msgrcv(int a, void* buf, size_t msz, long mtype, int flags);
#endif // IPCINSTALLED

#ifndef _WIN32
int semget(key_t key, int nsems, int semflg);
int semctl(int semid, int semnum, int cmd, ...);
int semop(int semid, struct sembuf *sops, size_t nsops);
int shmget(key_t key, size_t size, int shmflg);
void *shmat(int shmid, const void *shmaddr, int shmflg);
int shmdt(const void *shmaddr);
int shmctl(int shmid, int cmd, struct shmid_ds *buf);
#endif // _WIN32

void* realloc(void* ptr, size_t size);
// void* malloc(size_t size);

// namespace rapidjson {
// namespace MemoryPoolAllocator {
//   void* Realloc(void* originalPtr, size_t originalSize, size_t newSize);
// }
// }

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

#if defined(MPIINSTALLED) && defined(MPI_COMM_WORLD)
  int MPI_Probe(int source, int tag, MPI_Comm comm, MPI_Status *status);
  int MPI_Send(const void *buf, int count, MPI_Datatype datatype,
	       int dest, int tag, MPI_Comm comm);
  int MPI_Recv(void *buf, int count, MPI_Datatype datatype, int source,
	       int tag, MPI_Comm comm, MPI_Status * status);
#endif

#ifdef RMQINSTALLED
  amqp_rpc_reply_t _amqp_get_rpc_reply();
  amqp_socket_t* amqp_tcp_socket_new(amqp_connection_state_t state);
  amqp_rpc_reply_t amqp_get_rpc_reply(amqp_connection_state_t state);
  amqp_queue_declare_ok_t* amqp_queue_declare(
    amqp_connection_state_t state, amqp_channel_t channel,
    amqp_bytes_t queue, amqp_boolean_t passive, amqp_boolean_t durable,
    amqp_boolean_t exclusive, amqp_boolean_t auto_delete,
    amqp_table_t arguments);
  amqp_rpc_reply_t amqp_basic_get(amqp_connection_state_t state,
				  amqp_channel_t channel,
				  amqp_bytes_t queue,
				  amqp_boolean_t no_ack);
#endif
  
#endif // ELF_AVAILABLE
}
}
