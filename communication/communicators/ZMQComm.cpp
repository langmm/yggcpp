#include "ZMQComm.hpp"
#include "DefaultComm.hpp"
#include "utils/tools.hpp"
#include "utils/logging.hpp"

/* double communication::communicator::_wait_send_t = 0;  // 0.0001; */
char communication::communicator::_reply_msg[100] = "YGG_REPLY";
char communication::communicator::_purge_msg[100] = "YGG_PURGE";
int communication::communicator::_zmq_sleeptime = 10000;

using namespace communication::communicator;
using namespace communication::utils;

// const std::chrono::milliseconds timeout{1000};
// const std::chrono::milliseconds short_timeout{10};
#define timeout 1000
#define short_timeout 10

////////////////
// ZMQContext //
////////////////

void* ZMQContext::ygg_s_process_ctx = NULL;

ZMQContext::ZMQContext() : ctx(NULL) { init(); }
// ZMQContext::ZMQContext(const ZMQContext& rhs) : ctx(rhs.ctx) {}
// ZMQContext& ZMQContext::operator=(const ZMQContext& rhs) {
//   ctx = rhs.ctx;
//   return *this;
// }

#ifdef ZMQINSTALLED
void ZMQContext::init() {
  YGG_THREAD_SAFE_BEGIN(zmq) {
    if (ZMQContext::ygg_s_process_ctx == NULL) {
      if (get_thread_id() == communication::communicator::Comm_t::_ygg_main_thread_id) {
	ygglog_debug << "ZMQContext::init: Creating ZMQ context." << std::endl;
	ctx = zmq_ctx_new();
      } else {
	ygglog_throw_error("ZMQContext::init: Can only initialize the "
			   "zeromq context on the main thread. Call "
			   "ygg_init before the threaded portion of "
			   "your model.");
      }
      ygglog_debug << "ZMQContext::init: Created ZMQ context." << std::endl;
      ZMQContext::ygg_s_process_ctx = ctx;
    } else {
      ctx = ZMQContext::ygg_s_process_ctx;
    }
  } YGG_THREAD_SAFE_END;
  if (ctx == NULL) {
    ygglog_throw_error("ZMQContext::init: ZMQ context is NULL.");
  }
}

void ZMQContext::destroy() {
  YGG_THREAD_SAFE_BEGIN(zmq) {
    if (ZMQContext::ygg_s_process_ctx != NULL) {
      ygglog_debug << "ZMQContext::destroy: Destroying the ZMQ context" << std::endl;
// #ifdef _WIN32
//       if (Comm_t::_ygg_atexit == 0) {
// #endif // _WIN32
	zmq_ctx_shutdown(ZMQContext::ygg_s_process_ctx);
	if (zmq_ctx_term(ZMQContext::ygg_s_process_ctx) != 0) {
	  ygglog_error << "ZMQContext::destroy: Error terminating context with zmq_ctx_term" << std::endl;
	}
// #ifdef _WIN32
//       }
// #endif // _WIN32
      ZMQContext::ygg_s_process_ctx = NULL;
    }
  } YGG_THREAD_SAFE_END;
}
#endif // ZMQINSTALLED


///////////////
// ZMQSocket //
///////////////

int ZMQSocket::_last_port = 0;
int ZMQSocket::_last_port_set = 0;

ZMQSocket::ZMQSocket() :
  handle(NULL), endpoint(), type(0), ctx() {}
ZMQSocket::ZMQSocket(const ZMQSocket& rhs) :
  handle(NULL), endpoint(), type(rhs.type), ctx() {}
ZMQSocket::ZMQSocket(int type0, utils::Address* address,
		     int linger, int immediate, int sndtimeo) :
  handle(NULL), endpoint(), type(type0), ctx() {
  init(type0, address, linger, immediate, sndtimeo);
}

void ZMQSocket::init(int type0, std::string address,
		     int linger, int immediate, int sndtimeo) {
  if (address.empty()) {
    init(type0, NULL, linger, immediate, sndtimeo);
  } else {
    utils::Address address_;
    address_.address(address);
    init(type0, &address_, linger, immediate, sndtimeo);
  }
}

#ifdef ZMQINSTALLED
template<typename T>
int ZMQSocket::set(int member, const T& data) {
  if (zmq_setsockopt(handle, member, &data, sizeof(data)) != 0) {
    ygglog_error << "ZMQSocket::set: Error setting " << member << " to " << data << std::endl;
    return -1;
  }
  return 1;
}
void ZMQSocket::init(int type0, utils::Address* address,
		     int linger, int immediate, int sndtimeo) {
  type = type0;
  std::string except_msg;
  YGG_THREAD_SAFE_BEGIN(zmq) {
    handle = zmq_socket (ctx.ctx, type);
    if (handle == NULL) {
      except_msg = "ZMQSocket::init: Error creating new socket.";
    } else {
#define DO_SET(flag, var, def)					\
      if (except_msg.empty() && var != def &&			\
	  set(flag, var) < 0) {					\
	except_msg = "ZMQSocket::init: Error setting " #flag " to " + std::to_string(var); \
      }
      DO_SET(ZMQ_LINGER, linger, -1);
      DO_SET(ZMQ_IMMEDIATE, immediate, 0);
      DO_SET(ZMQ_SNDTIMEO, sndtimeo, -1);
#undef DO_SET
    }
  } YGG_THREAD_SAFE_END;
  if (!except_msg.empty()) {
    destroy();
    throw std::runtime_error(except_msg);
  }
  if (address && !address->address().empty()) {
    endpoint = address->address();
    if (zmq_connect(handle, endpoint.c_str()) != 0) {
      destroy();
      ygglog_throw_error("ZMQSocket::init: Error connecting to endpoint '" + endpoint + "'");
    }
    ygglog_debug << "ZMQSocket::init: Connected to endpoint '" << endpoint << "'" << std::endl;
  } else {
    const std::string protocol = "tcp";
    std::string host = "localhost";
    if (host == "localhost")
      host = "127.0.0.1";
    std::string address;
    YGG_THREAD_SAFE_BEGIN(zmqport) {
      if (_last_port_set == 0) {
	const char *model_index = getenv("YGG_MODEL_INDEX");
	if (model_index == NULL) {
	  // except_msg = "Environment variable 'YGG_MODEL_INDEX' is not defined. Connot create ZMQComm.";
	  // _last_port = -1;
	  _last_port = 49152;
	  _last_port_set = 1;
	} else { // GCOVR_EXCL_LINE
	  ygglog_debug << "ZMQSocket::init: model_index = " << model_index << std::endl;
	  _last_port = 49152 + 1000 * static_cast<int>(strtol(model_index, nullptr, 0));
	  _last_port_set = 1;
	  ygglog_debug << "ZMQSocket::init: last_port = " << _last_port << std::endl;
	}
      }
      if (except_msg.empty()) {
	// zeromq API dosn't seem to support this wildcard syntax
	// endpoint = protocol + "://" + host + ":*[" +
	//   std::to_string(_last_port + 1) + "-]";
	int port = _last_port + 1;
	int err = EADDRINUSE;
	while (err == EADDRINUSE) {
	  err = 0;
	  endpoint = protocol + "://" + host + ":" + std::to_string(port);
	  if (zmq_bind(handle, endpoint.c_str()) != 0) {
	    err = zmq_errno();
	    if (err != EADDRINUSE) {
	      except_msg = "ZMQSocket::init: Error binding to address: " + endpoint;
	    } else { // GCOVR_EXCL_LINE
	      port++;
	    }
	  }
	}
      }
      if (except_msg.empty()) {
	char endpoint_c[1000];
	size_t endpoint_len = 1000;
	if (zmq_getsockopt(handle, ZMQ_LAST_ENDPOINT, &endpoint_c, &endpoint_len) != 0) {
	  except_msg = "ZMQSocket::init: Error getting bound endpoint";
	} else {
	  endpoint.assign(endpoint_c, endpoint_len - 1); // Remove newline char
	  ygglog_debug << "ZMQSocket::init: Bound to endpoint '" << endpoint << "'" << std::endl;
	  size_t idx_port = endpoint.find_last_of(':');
	  assert(idx_port != std::string::npos);
	  if (idx_port == std::string::npos) {
	    except_msg = "ZMQSocket::init: Error getting port from endpoing";
	  } else {
	    ygglog_debug << "ZMQSocket::init: last_port = " << endpoint.substr(idx_port + 1) << ", idx_port = " << idx_port << std::endl;
	    _last_port = stoi(endpoint.substr(idx_port + 1));
	    ygglog_debug << "ZMQSocket::init: last_port = " << _last_port << std::endl;
	  }
	}
      }
    } YGG_THREAD_SAFE_END;
    if (!except_msg.empty()) {
      destroy();
      throw std::runtime_error(except_msg);
    }
  }
}

int ZMQSocket::poll(int method, int tout) {
  int out = 0;
#ifdef ZMQ_HAVE_POLLER
  void* poller = zmq_poller_new();
  zmq_poller_event_t events [1];
  if (zmq_poller_add(poller, handle, NULL, method) != 0) {
    ygglog_error << "ZMQSocket: Error adding poller" << std::endl;
    zmq_poller_destroy (&poller);
    return -1;
  }
  if (zmq_poller_wait_all(poller, events, 1, tout) == 1) {
    out = 1;
  } else {
    if (zmq_errno() == EAGAIN) {
      out = 0;
    } else {
      ygglog_error << "ZMQSocket: Error in poller" << std::endl;
      out = -1;
    }
  }
  zmq_poller_destroy (&poller);
#else // ZMQ_HAVE_POLLER
  zmq_pollitem_t items[1];
  items[0].socket = handle;
  items[0].events = method;
  out = zmq_poll(items, 1, tout);
  if (out < 0) {
    ygglog_error << "ZMQSocket: Error in poller" << std::endl;
    return -1;
  }
#endif // ZMQ_HAVE_POLLER
  return out;
}

int ZMQSocket::send(const std::string msg) {
  zmq_msg_t part;
  if (zmq_msg_init_size (&part, msg.size()) != 0)
    return -1;
  memcpy(zmq_msg_data(&part), msg.c_str(), msg.size());
  ygglog_debug << "ZMQSocket::send: Sending " << msg.size() << " bytes" << std::endl;
  if (zmq_sendmsg (handle, &part, 0) != (int)(msg.size())) {
    zmq_msg_close (&part);
    return -1;
  }
  ygglog_debug << "ZMQSocket::send: Sent " << msg.size() << " bytes" << std::endl;
  zmq_msg_close (&part);
  return msg.size();
}

int ZMQSocket::recv(std::string& msg) {
  msg = "";
  int more = 1;
  size_t more_size = sizeof(more);
  ygglog_debug << "ZMQSocket::recv: Receiving message" << std::endl;
  bool identity = (type == ZMQ_ROUTER);
  do {
    zmq_msg_t part;
    if (zmq_msg_init (&part) != 0)
      return -1;
    if (zmq_recvmsg (handle, &part, 0) == -1) {
      zmq_msg_close (&part);
      return -1;
    }
    if (!identity)
      msg += std::string((const char*)zmq_msg_data(&part), zmq_msg_size(&part));
    zmq_msg_close (&part);
    if (zmq_getsockopt (handle, ZMQ_RCVMORE, &more, &more_size) != 0)
      return -1;
    identity = false;
  } while (more);
  ygglog_debug << "ZMQSocket::recv: Received " << msg.size() << " bytes" << std::endl;
  return msg.size();
}

void ZMQSocket::destroy() {
  if (handle != NULL)
    zmq_close(handle);
  handle = NULL;
}
#endif // ZMQINSTALLED

ZMQSocket::~ZMQSocket() {
  destroy();
}

#ifdef YGG_TEST
void ZMQSocket::resetPort() {
  ZMQSocket::_last_port_set = 0;
}
#endif // YGG_TEST

//////////////
// ZMQReply //
//////////////

#ifdef YGG_TEST
bool ZMQReply::return_val = true;
void ZMQReply::set_return_val(bool new_val) {
  ZMQReply::return_val = new_val;
}
#endif // YGG_TEST

ZMQReply::ZMQReply(DIRECTION dir) :
  sockets(), n_msg(0), n_rep(0), direction(dir), last_idx(-1) {}

#ifdef ZMQINSTALLED

void ZMQReply::clear() {
  sockets.clear();
  n_msg = 0;
  n_rep = 0;
  last_idx = -1;
}

int ZMQReply::create(std::string& endpoint) {
  int out = set();
  if (out >= 0) {
    endpoint = sockets[out].endpoint;
  }
  return out;
}

int ZMQReply::find(std::string endpoint) {
  if (endpoint.empty()) {
    if (direction == SEND && sockets.size() > 0)
      return 0;
    return -1;
  }
  for (size_t i = 0; i < sockets.size(); i++)
    if (sockets[i].endpoint == endpoint)
      return (int)i;
  return -1;
}

int ZMQReply::set(std::string endpoint) {
  int out = find(endpoint);
  if (out < 0) {
    out = sockets.size();
    sockets.resize(out + 1);
    if (endpoint.empty()) {
      assert(direction == SEND);
      sockets[out].init(ZMQ_REP, "", 0, 1, _zmq_sleeptime);
    } else {
      assert(direction == RECV);
      sockets[out].init(ZMQ_REQ, endpoint, 0, 1, _zmq_sleeptime);
    }
  }
  last_idx = out;
  return out;
}
bool ZMQReply::recv(std::string msg_send) {
  if (msg_send.empty())
    msg_send.assign(_reply_msg);
#ifdef YGG_TEST
  // Exit early to prevent deadlock when running from the same thread
  return return_val;
#else // YGG_TEST
  if (!recv_stage1(msg_send))
    return false;
  return recv_stage2(msg_send);
#endif // YGG_TEST
}
bool ZMQReply::recv_stage1(std::string msg_send) {
  if (msg_send.empty())
    msg_send.assign(_reply_msg);
  if (last_idx < 0)
    return false;
  ZMQSocket* sock = &(sockets[last_idx]);
  ygglog_debug << "ZMQReply::recv_stage1: Sending handshake to confirm message was received (address = " << sock->endpoint << ")" << std::endl;
  // Send
  if (sock->send(msg_send) < 0) {
    ygglog_error << "ZMQReply::recv_stage1: Error sending confirmation." << std::endl;
    return false;
  }
  // if (msg_send == YGG_MSG_EOF) {
  //   ygglog_info << "ZMQReply::recv_stage1: EOF confirmation." << std::endl;
  //   n_msg = 0;
  //   n_rep = 0;
  //   sock->set(ZMQ_LINGER, _zmq_sleeptime);
  //   return false;
  // }
  return true;
}
bool ZMQReply::recv_stage2(std::string msg_send) {
  if (msg_send.empty())
    msg_send.assign(_reply_msg);
  if (last_idx < 0)
    return false;
  ZMQSocket* sock = &(sockets[last_idx]);
  ygglog_debug << "ZMQReply::recv_stage2: Receiving acknowledgement of handshake (address = " << sock->endpoint << ")" << std::endl;
  // Receive
  std::string msg_recv;
  sock->poll(ZMQ_POLLIN, timeout);
  if (sock->recv(msg_recv) < 0) {
    ygglog_error << "ZMQReply::recv_stage2: Error receiving reponse" << std::endl;
    return false;
  }
  n_rep++;
  ygglog_debug << "ZMQReply::recv_stage2: Handshake complete (address = "
	       << sock->endpoint << ")" << std::endl;
  last_idx = -1;
  return true;
}

bool ZMQReply::send() {
#ifdef YGG_TEST
  return return_val;
#else // YGG_TEST
  std::string msg_data;
  if (!send_stage1(msg_data))
    return false;
  return send_stage2(msg_data);
#endif // YGG_TEST
}
bool ZMQReply::send_stage1(std::string& msg_data) {
  if (sockets.size() == 0) {
    ygglog_error << "ZMQReply::send_stage1: Reply socket was not initialized." << std::endl;
    return false;
  }
  ZMQSocket* sock = &(sockets[0]);
  ygglog_debug << "ZMQReply::send_stage1: Receiving handshake to confirm message was received (address = " << sock->endpoint << ")" << std::endl;
  sock->poll(ZMQ_POLLIN, timeout);
  if (sock->recv(msg_data) < 0) {
    ygglog_error << "ZMQReply::send_stage1: Error receiving reply" << std::endl;
    return false;
  }
  // Check for EOF
  // if (msg_data == YGG_MSG_EOF) {
  //   ygglog_debug << "ZMQReply::send_stage1: EOF received" << std::endl;
  //   n_msg = 0;
  //   n_rep = 0;
  //   return -2;
  // }
  return true;
}
bool ZMQReply::send_stage2(const std::string msg_data) {
  // bool is_purge = (msg_data == _purge_msg);
  if (sockets.size() == 0) {
    ygglog_error << "ZMQReply::send_stage2: Reply socket was not initialized." << std::endl;
    return false;
  }
  ygglog_debug << "ZMQReply::send_stage2: Sending acknowledgement of handshake" << std::endl;
  ZMQSocket* sock = &(sockets[0]);
  sock->poll(ZMQ_POLLOUT, timeout);
  if (sock->send(msg_data) < 0) {
    ygglog_error << "ZMQReply::send_stage2: Error sending reply frame." << std::endl;
    return false;
  }
  // Sleep briefly to ensure receive is complete
  THREAD_USLEEP(100);
  // Check for purge or EOF
  // if (is_purge) {
  //   ygglog_debug << "ZMQReply::send_stage2: PURGE received" << std::endl;
  //   n_msg = 0;
  //   n_rep = 0;
  //   return send();
  // } else {
  n_rep++;
  // }
  ygglog_debug << "ZMQReply::send_stage2: Handshake complete (address = "
	       << sock->endpoint << ")" << std::endl;
  return true;
}

#endif // ZMQINSTALLED

// #ifdef _OPENMP
// std::vector<ygg_sock_t*> ygg_sock_t::activeSockets = {};

// void ygg_sock_t::ctx_shutdown() {
// #pragma omp critical (zmq)
//     {
//         if (ctx_valid) {
//             // force all sockets to be closed, otherwise the context close will hang
// 	    for (int i = ygg_sock_t::activeSockets.size() - 1; i >= 0; i--) {
// 	        ygg_sock_t::activeSockets[i]->close();
//             }
//             ygg_s_process_ctx.shutdown();
//             ygg_s_process_ctx.close();
//             ctx_valid = false;
//         }
//     }
// }


//#ifdef _OPENMP
//}
//#endif

// void ygg_sock_t::close() {
//     ygg_sock_t::activeSockets.erase(std::remove(ygg_sock_t::activeSockets.begin(),
//                                                 ygg_sock_t::activeSockets.end(),
//                                                 this), ygg_sock_t::activeSockets.end());
//     zmq::socket_t::close();
// }

/////////////
// ZMQComm //
/////////////

ZMQComm::ZMQComm(const std::string name, utils::Address *address,
		 const DIRECTION direction, int flgs,
		 const COMM_TYPE type) :
  CommBase(name, address, direction, type, flgs), reply(direction) {
  if (!global_comm)
    init();
}

ADD_CONSTRUCTORS_DEF(ZMQComm)

#ifdef ZMQINSTALLED

void ZMQComm::init() {
  updateMaxMsgSize(1048576);
  msgBufSize = 100;
  assert(!handle);
  int socket_type = ZMQ_PAIR;
  if (flags & COMM_ALLOW_MULTIPLE_COMMS) {
    if (direction == RECV && !(address && address->valid())) {
      socket_type = ZMQ_ROUTER;
    } else {
      socket_type = ZMQ_DEALER;
    }
  }
  handle = new ZMQSocket(socket_type, address);
  address->address(handle->endpoint);
  if (this->name.empty())
    this->name = "tempnewZMQ-" + handle->endpoint.substr(handle->endpoint.find_last_of(':') + 1);
  if (direction == SEND)
    flags |= COMM_ALWAYS_SEND_HEADER;
  CommBase::init();
}

void ZMQComm::close() {
    ygglog_debug << "ZMQComm::close: Started" << std::endl;
    if ((direction == RECV) && this->is_open() &&
	(!global_comm) && (!(flags & COMM_EOF_RECV))) {
      if (utils::YggdrasilLogger::_ygg_error_flag == 0) {
	    size_t data_len = 0;
            char *data = NULL;
            while (comm_nmsg(RECV) > 0) {
                if (long ret = recv_raw(data, data_len, true) >= 0) {
		  if (ret > (long)data_len)
		    data_len = ret;
                }
            }
	    if (data != NULL)
	      free(data);
        }
    }
    ygglog_debug << "ZMQComm::close: Finished" << std::endl;
    CommBase::close();
}

int ZMQComm::comm_nmsg(DIRECTION dir) const {
    if (global_comm)
      return global_comm->comm_nmsg(dir);
    if (dir == NONE)
      dir = direction;
    if (dir != direction)
      return 0;
    int out = 0;
    if (dir == RECV) {
        if (handle) {
	    return handle->poll(ZMQ_POLLIN, short_timeout);
        }
    } else { // GCOVR_EXCL_LINE
      ygglog_debug << "ZMQComm(" << name << ")::comm_nmsg: nmsg = " << reply.n_msg << ", nrep = "
		   << reply.n_rep << std::endl;
      out = reply.n_msg - reply.n_rep;
    }
    return out;
}

int ZMQComm::send_single(utils::Header& header) {
  assert(!global_comm);
  if (header.on_send() < 0)
    return -1;
  ygglog_debug << "ZMQComm(" << name << ")::send_single: " << header.size_msg << " bytes" << std::endl;
  std::string msg(header.data_msg(), header.size_msg);
  int ret = handle->send(msg);
  if (ret < 0) {
    ygglog_error << "ZMQComm(" << name << ")::send_single: Error in ZMQSocket::send" << std::endl;
    return -1;
  }
  // Reply
  if (!do_reply_send(header)) {
    ygglog_error << "ZMQComm(" << name << ")::send_single: Error in do_reply_send" << std::endl;
    return -1;
  }
  ygglog_debug << "ZMQComm(" << name << ")::send_single: returning " << ret << std::endl;
  return ret;
}
bool ZMQComm::do_reply_send(const utils::Header& header) {
  if (header.flags & (HEAD_FLAG_CLIENT_SIGNON | HEAD_FLAG_SERVER_SIGNON))
    return true;
  return reply.send();
}

long ZMQComm::recv_single(utils::Header& header) {
    assert((!global_comm) && handle);
    long ret = -1;
    ygglog_debug << "ZMQComm(" << name << ")::recv_single " << std::endl;

    std::string msg;
    if ((ret = handle->recv(msg)) < 0)
        return ret;

    ret = header.on_recv(msg.c_str(), msg.size());
    if (ret < 0) {
      ygglog_error << "ZMQComm(" << name << ")::recv_single: Error copying data" << std::endl;
      return ret;
    }
    ygglog_debug << "ZMQComm(" << name << ")::recv_single: received " << ret << " bytes" << std::endl;
    // Extract reply address from header
    if (!do_reply_recv(header)) {
      ygglog_error << "ZMQComm(" << name << ")::recv_single: Error in do_reply_recv" << std::endl;
      return -1;
    }
    ygglog_debug << "ZMQComm(" << name << ")::recv_single: returns " << ret << " bytes" << std::endl;
    return ret;
}

bool ZMQComm::do_reply_recv(const Header& header) {
  // Should never be called with global comm
  // if (global_comm)
  //   return dynamic_cast<ZMQComm*>(global_comm)->do_reply_recv(header);
  assert(!global_comm);
  if (header.flags & (HEAD_FLAG_CLIENT_SIGNON | HEAD_FLAG_SERVER_SIGNON))
    return true;
  std::string adr;
  if ((flags & COMM_FLAG_WORKER) && (reply.sockets.size() == 1)) {
    adr = reply.sockets[0].endpoint;
  } else {
    try {
      const char* address_c;
      if (!header.GetMetaString("zmq_reply", address_c))
	return false;
      adr.assign(address_c);
    } catch (...) {
      return false;
    }
  }
  reply.set(adr);
  return reply.recv();
}

bool ZMQComm::create_header_send(Header& header) {
  assert(!global_comm);
  bool out = Comm_t::create_header_send(header);
  if (out && !(header.flags & (HEAD_FLAG_CLIENT_SIGNON |
			       HEAD_FLAG_SERVER_SIGNON))) {
    std::string reply_address;
    reply.create(reply_address);
    ygglog_debug << "ZMQComm(" << this->name << ")::create_header_send: zmq_reply = " << reply_address << std::endl;
    if (!header.SetMetaString("zmq_reply", reply_address))
      return false;
  }
  return out;
}

WORKER_METHOD_DEFS(ZMQComm)

Comm_t* ZMQComm::create_worker_send(Header& head) {
  // Should never be called with global comm
  // if (global_comm)
  //   return global_comm->create_worker_send(head);
  assert(!global_comm);
  ZMQComm* out = dynamic_cast<ZMQComm*>(Comm_t::create_worker_send(head));
  if (out) {
    std::string reply_address;
    out->reply.create(reply_address);
    ygglog_debug << "ZMQComm(" << this->name << ")::create_worker_send: zmq_reply_worker = " << reply_address << std::endl;
    if (!head.SetMetaString("zmq_reply_worker", reply_address))
      return nullptr;
  }
  return out;
}

Comm_t* ZMQComm::create_worker_recv(Header& head) {
  // Should never be called with global comm
  // if (global_comm)
  //   return global_comm->create_worker_recv(head);
  assert(!global_comm);
  ZMQComm* out = dynamic_cast<ZMQComm*>(Comm_t::create_worker_recv(head));
  if (out) {
    try {
      const char* zmq_reply_worker;
      if (!head.GetMetaString("zmq_reply_worker", zmq_reply_worker))
	return nullptr;
      out->reply.set(std::string(zmq_reply_worker));
    } catch (...) {
      return nullptr;
    }
  }
  return out;
}

#ifdef YGG_TEST
bool ZMQComm::afterSendRecv(Comm_t* sComm, Comm_t* rComm) {
  if (sComm->global_comm) // // GCOVR_EXCL_START
    sComm = sComm->global_comm;
  if (rComm->global_comm)
    rComm = rComm->global_comm;
  if ((sComm->getType() != ZMQ_COMM && sComm->getType() != SERVER_COMM &&
       sComm->getType() != CLIENT_COMM) || 
      (rComm->getType() != ZMQ_COMM && rComm->getType() != SERVER_COMM &&
       rComm->getType() != CLIENT_COMM)) {
    ygglog_error << "ZMQComm::afterSendRecv: One or both communicators are not ZMQ communicators" << std::endl;
    return false;
  }
  ZMQComm* sComm_z = dynamic_cast<ZMQComm*>(sComm);
  ZMQComm* rComm_z = dynamic_cast<ZMQComm*>(rComm);
  if (rComm_z->getReply().sockets.size() != 1) {
    ygglog_error << "ZMQComm::afterSendRecv: Receive socket not set" << std::endl;
    return false;
  }
  if (!rComm_z->getReply().recv_stage1()) {
    ygglog_error << "ZMQComm::afterSendRecv: Error in recv_stage1" << std::endl;
    return false;
  }
  std::string msg;
  if (!sComm_z->getReply().send_stage1(msg)) {
    ygglog_error << "ZMQComm::afterSendRecv: Error in send_stage1" << std::endl;
    return false;
  }
  if (!sComm_z->getReply().send_stage2(msg)) {
    ygglog_error << "ZMQComm::afterSendRecv: Error in send_stage2" << std::endl;
    return false;
  }
  if (!rComm_z->getReply().recv_stage2()) {
    ygglog_error << "ZMQComm::afterSendRecv: Error in recv_stage2" << std::endl;
    return false;
  }
  return true; // GCOVR_EXCL_STOP
}
#endif // YGG_TEST
#endif // ZMQINSTALLED
