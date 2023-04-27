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
int SENDCOUNT = 0;
std::string RETMSG = "";

#ifdef IPCINSTALLED
int msgsnd(int, const void *, size_t, int) {
    //std::cout << "HERE";
    //msgsnd(a, b, c, d);
    SENDCOUNT++;
    return RETVAL;
}

int msgctl(int, int, msqid_ds *buf) {
    if (buf == nullptr)
        return 0;
    buf->msg_qnum = 10000;
    buf->msg_qbytes = 1000;
    RETVAL++;
    return RETVAL + 1;
}

int msgget(key_t, int) {
    return RETVAL;
}

ssize_t msgrcv(int, void* rbuf, size_t, long, int) {
    std::string msg = "Hello world";
    memcpy(static_cast<communicator::msgbuf_t*>(rbuf)->data, msg.c_str(), msg.size());
    if (RETVAL < 0)
        return RETVAL;
    return (ssize_t)(msg.size());
}
#endif // IPCINSTALLED

void* realloc(void*, size_t) {
    return nullptr;
}
  
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
    if (RETVAL < 0)
      return RETVAL;
    RETVAL--;
    return static_cast<int>(zmq_msg_size(msg));
  }
  int zmq_recvmsg (void *, zmq_msg_t *msg, int) {
    std::string msgS = "Hello world";
    memcpy(zmq_msg_data(msg), msgS.c_str(), msgS.size());
    if (RETVAL < 0)
        return RETVAL;
    return 0;
  }
#ifdef ZMQ_HAVE_POLLER
  int zmq_poller_wait_all (void *, zmq_poller_event_t *, int n_events, long) {
    if (RETVAL < 0)
      return RETVAL;
    return n_events;
  }
#else // ZMQ_HAVE_POLLER
  int zmq_poll (zmq_pollitem_t *, int nitems, long) {
    if (RETVAL < 0)
      return RETVAL;
    return nitems;
  }
#endif // ZMQ_HAVE_POLLER
  int zmq_errno (void) {
    return RETVAL;
  }
  // Only testing for errors
  int zmq_ctx_term (void *) {
    return -1;
  }
  void *zmq_socket (void *, int) {
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
  int zmq_msg_init_size (zmq_msg_t *, size_t) {
    return -1;
  }
  int zmq_setsockopt (void *, int, const void *, size_t) {
    return -1;
  }
  int zmq_getsockopt (void *, int, void * option_value, size_t *option_len) {
    if (RETMSG.empty() || (RETMSG.size() + 1) > option_len[0])
      return -1;
    memcpy(option_value, RETMSG.c_str(), RETMSG.size());
    char term = '\0';
    memcpy(((char*)option_value) + RETMSG.size(), &term, sizeof(char));
    return 0;
  }
  
#endif // ZMQINSTALLED

#endif // ELF_AVAILABLE
  
}
}
