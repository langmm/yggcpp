#include <cstdlib>
#include <iostream>
#include "mock.hpp"
#include "communicators/IPCComm.hpp"

#ifdef SUBLIBFILE
bool sublib_read = false;
char sublib_contents[256] = "";
void init_sublib_contents() {
  if (!sublib_read) {
    std::string fname = SUBLIBFILE;
    FILE *fp = fopen(fname.c_str(), "r");
    if (fp == NULL) {
      std::cerr << "Failed to open SUBLIB file " << fname << std::endl;
      throw std::exception();
    }
    fgets(sublib_contents, 256, fp);
    sublib_contents[strcspn(sublib_contents, "\n")] = 0;
    fclose(fp);
    sublib_read = true;
  }
}
#else
void init_sublib_contents() {}
#endif

namespace communication {
namespace mock {

#ifdef ELF_AVAILABLE

int RETVAL = 0;
int RETVAL_CREATE = 0;
int RETVAL_INC_SEND = 0;
int RETVAL_INC_RECV = 0;
int RETVAL_INC_POLL = 0;
int SENDCOUNT = 0;
std::string RETMSG = "";
std::string RETMSG_META = "";
std::string RETMSG_META_DEFAULT = "";

std::string _mock_message() {
  if (RETMSG.empty())
    return RETMSG;
  std::string out = "YGG_MSG_HEAD{\"__meta__\": {";
  if (RETMSG_META_DEFAULT.empty()) {
    out += "\"id\": \"1\", \"model\": \"model\", \"size\": " + std::to_string(RETMSG.size());
  } else {
    out += RETMSG_META_DEFAULT;
  }
  if (!RETMSG_META.empty())
    out += ", " + RETMSG_META;
  out += "}}YGG_MSG_HEAD\"" + RETMSG + "\"";
  return out;
}
  
char *alt_getenv(const char *) {
  return NULL;
}

#define DO_ELF_SEND(err)			\
  int out = RETVAL;				\
  RETVAL += RETVAL_INC_SEND;			\
  if (out < 0)					\
    return err;					\
  SENDCOUNT++
#define DO_ELF_POLL(err)			\
  int out = RETVAL;				\
  RETVAL += RETVAL_INC_POLL;			\
  if (out < 0)					\
    return err
#define DO_ELF_RECV(err)			\
  int out = RETVAL;				\
  RETVAL += RETVAL_INC_RECV;			\
  if (out < 0)					\
    return err;					\
  std::string msgS = _mock_message()
  
#ifdef IPCINSTALLED
int msgsnd(int, const void *, size_t, int) {
  DO_ELF_SEND(out);
  return 0;
}

int msgctl(int, int, msqid_ds *buf) {
  if (buf == nullptr)
    return 0;
  buf->msg_qnum = 10000;
  buf->msg_qbytes = 1000;
  DO_ELF_POLL(out);
  return 0;
}

int msgget(key_t, int) {
  return RETVAL_CREATE;
}

ssize_t msgrcv(int, void* rbuf, size_t, long, int) {
  DO_ELF_RECV(out);
  memcpy(static_cast<communicator::msgbuf_t*>(rbuf)->data, msgS.c_str(), msgS.size());
  return (ssize_t)(msgS.size());
}
#endif // IPCINSTALLED

void* realloc(void*, size_t) {
  std::cerr << "MOCK REALLOC" << std::endl;
  return NULL;
}

// void* malloc(size_t) {
//   std::cerr << "MOCK MALLOC" << std::endl;
//   return NULL;
// }
  
// namespace rapidjson {
// namespace MemoryPoolAllocator {
//   void* Realloc(void*, size_t, size_t) {
//     std::cerr << "MOCK RAPIDJSON REALLOC" << std::endl;
//     return NULL;
//   }
// }
// }
  
#ifdef ZMQCPPINSTALLED
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
size_t wait_all(std::vector<::zmq::poller_event<> > &, const std::chrono::milliseconds) {
    return 2;
}

}
namespace detail {
namespace socket_base {

::zmq::detail::trivial_optional<size_t> send(::zmq::message_t &, ::zmq::send_flags) {
    ::zmq::detail::trivial_optional<size_t> ret(0);
    return ret;
}

::zmq::detail::trivial_optional<size_t> recv(::zmq::message_t &,
                                             ::zmq::recv_flags) {
    ::zmq::detail::trivial_optional<size_t> ret(0);
    return ret;
}

std::string get() {
    return "127.0.0.1:8888";
}

template<class OutputIt>
::zmq::detail::trivial_optional<size_t> recv_multipart(::zmq::socket_ref, OutputIt,
                                                       ::zmq::recv_flags) {
    ::zmq::detail::trivial_optional<size_t> ret(1);
    return ret;

}
}
}
}

#endif // ZMQCPPINSTALLED
  
#ifdef ZMQINSTALLED

  int zmq_sendmsg (void *, zmq_msg_t *msg, int) {
    DO_ELF_SEND(out);
    return static_cast<int>(zmq_msg_size(msg));
  }
  int zmq_recvmsg (void *, zmq_msg_t *msg, int) {
    DO_ELF_RECV(out);
    // "YGG_MSG_HEAD{\"__meta__\": {\"zmq_reply\": \"" + RETMSG_REPLY +
    // "\", \"size\": 11, \"id\": \"1\"}}YGG_MSG_HEAD" + RETMSG;
    if (zmq_msg_init_size (msg, msgS.size()) != 0)
      return -1;
    memcpy(zmq_msg_data(msg), msgS.c_str(), msgS.size());
    return 0;
  }
#ifdef ZMQ_HAVE_POLLER
  int zmq_poller_wait_all (void *, zmq_poller_event_t *, int n_events, long) {
    DO_ELF_POLL(out);
    return n_events;
  }
#else // ZMQ_HAVE_POLLER
  int zmq_poll (zmq_pollitem_t *, int nitems, long) {
    DO_ELF_POLL(out);
    return nitems;
  }
#endif // ZMQ_HAVE_POLLER
  int zmq_errno (void) {
    int out = RETVAL;
    RETVAL--;
    return out;
  }
  // Only testing for errors
  int zmq_ctx_term (void *) {
    return -1;
  }
  void *zmq_socket (void *, int) {
    assert(RETVAL_CREATE < 0);
    return NULL;
  }
  int zmq_connect (void *, const char *) {
    return -1;
  }
  int zmq_bind (void *, const char *) {
    return -1;
  }
  int zmq_msg_init (zmq_msg_t *) {
    return -1;
  }
  int alt_zmq_msg_init_size (zmq_msg_t *, size_t) {
    return -1;
  }
  int zmq_setsockopt (void *, int, const void *, size_t) {
    return -1;
  }
  int zmq_getsockopt (void *, int, void * option_value, size_t *option_len) {
    if (RETMSG.empty() || (RETMSG.size() + 2) > option_len[0])
      return -1;
    memcpy(option_value, RETMSG.c_str(), RETMSG.size());
    ((char*)option_value)[RETMSG.size()] = '\n';
    for (size_t i = RETMSG.size() + 1; i < option_len[0]; i++)
      ((char*)option_value)[i] = '\0';
    // char term = '\0';
    // memcpy(((char*)option_value) + RETMSG.size(), &term, sizeof(char));
    option_len[0] = RETMSG.size() + 2;
    return 0;
  }
  
#endif // ZMQINSTALLED

#if defined(MPIINSTALLED) && defined(MPI_COMM_WORLD)
  int MPI_Probe(int, int, MPI_Comm, MPI_Status * status) {
    // MPI_Status_set_cancelled(status, MPICANCEL);
    status->MPI_ERROR = RETVAL;
    status->MPI_SOURCE = 0;
    DO_ELF_POLL(2);
    return MPI_SUCCESS;
  }
  int MPI_Send(const void *, int, MPI_Datatype,
	       int, int, MPI_Comm) {
    DO_ELF_SEND(2);
    return MPI_SUCCESS;
  }
  int MPI_Recv(void * buf, int count, MPI_Datatype datatype, int,
	       int, MPI_Comm, MPI_Status * status) {
    // MPI_Status_set_cancelled(status, MPICANCEL);
    status->MPI_ERROR = RETVAL;
    status->MPI_SOURCE = 0;
    DO_ELF_RECV(2);
    
    int size = static_cast<int>(msgS.size());
    if (datatype == MPI_INT && count == 1) {
      memcpy(buf, &size, sizeof(int));
    } else if (datatype == MPI_CHAR && count >= size) {
      memcpy(buf, msgS.c_str(), msgS.size() * sizeof(char));
    } else {
      status->MPI_ERROR = MPI_ERR_COUNT;
      return MPI_ERR_COUNT;
    }
    return MPI_SUCCESS;
  }
#endif
  
#endif // ELF_AVAILABLE
  
}
}
