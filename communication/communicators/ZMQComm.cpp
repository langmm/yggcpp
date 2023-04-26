#include "ZMQComm.hpp"
#include "utils/tools.hpp"
#include "utils/logging.hpp"

/* double communication::communicator::_wait_send_t = 0;  // 0.0001; */
char communication::communicator::_reply_msg[100] = "YGG_REPLY";
char communication::communicator::_purge_msg[100] = "YGG_PURGE";
int communication::communicator::_zmq_sleeptime = 10000;

using namespace communication::communicator;
using namespace communication::utils;

#ifdef ZMQINSTALLED
#include <boost/algorithm/string.hpp>

// const std::chrono::milliseconds timeout{1000};
// const std::chrono::milliseconds short_timeout{10};
#define timeout 1000
#define short_timeout 10

////////////////
// ZMQContext //
////////////////

void* ZMQContext::ygg_s_process_ctx = NULL;

ZMQContext::ZMQContext() : ctx(NULL) {
#ifdef _OPENMP
#pragma omp critical (zmq)
  {
    if (ZMQContext::ygg_s_process_ctx == NULL) {
      if (get_thread_id() == 0) {
	ygglog_debug << "ZMQContext: Creating ZMQ context." << std::endl;
#endif // _OPENMP
	ctx = zmq_ctx_new();
#ifdef _OPENMP
      } else {
	ygglog_throw_error("ZMQContext: Can only initialize the "
			   "zeromq context on the main thread. Call "
			   "ygg_init before the threaded portion of "
			   "your model.");
      }
      ZMQContext::ygg_s_process_ctx = ctx;
    } else {
      ctx = ZMQContext::ygg_s_process_ctx;
    }
  }
#endif // _OPENMP
  if (ctx == NULL) {
    ygglog_throw_error("ZMQContext: ZMQ context is NULL.");
  }
}
ZMQContext::ZMQContext(const ZMQContext& rhs) : ctx(rhs.ctx) {}
ZMQContext& ZMQContext::operator=(const ZMQContext& rhs) {
  ctx = rhs.ctx;
  return *this;
}

void ZMQContext::destroy() {
#ifdef _OPENMP
#pragma omp critical (zmq)
  {
#endif // _OPENMP
    if (ZMQContext::ygg_s_process_ctx != NULL) {
      if (zmq_ctx_term(ZMQContext::ygg_s_process_ctx) != 0) {
	ygglog_error << "ZMQContext::destroy: Error terminating context with zmq_ctx_term" << std::endl;
      }
      ZMQContext::ygg_s_process_ctx = NULL;
    }
#ifdef _OPENMP
  }
#endif // _OPENMP
}



///////////////
// ZMQSocket //
///////////////

int ZMQSocket::_last_port = 0;
int ZMQSocket::_last_port_set = 0;

ZMQSocket::ZMQSocket() :
  handle(NULL), endpoint(), type(0), ctx() {}
ZMQSocket::ZMQSocket(int type0, utils::Address* address,
		     int linger, int immediate, int sndtimeo) :
  handle(NULL), endpoint(), type(type0), ctx() {
  init(type0, address, linger, immediate, sndtimeo);
}
ZMQSocket::ZMQSocket(int type0, std::string address,
		     int linger, int immediate, int sndtimeo) :
  handle(NULL), endpoint(), type(type0), ctx() {
  init(type0, address, linger, immediate, sndtimeo);
}
ZMQSocket::ZMQSocket(const ZMQSocket& rhs) :
  handle(NULL), endpoint(), type(rhs.type), ctx() {}

template<typename T>
int ZMQSocket::set(int member, const T& data) {
  if (zmq_setsockopt(handle, member, &data, sizeof(data)) != 0) {
    ygglog_error << "ZMQSocket::set: Error setting " << member << " to " << data << std::endl;
    return -1;
  }
  return 1;
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
void ZMQSocket::init(int type0, utils::Address* address,
		     int linger, int immediate, int sndtimeo) {
  type = type0;
  std::string except_msg;
#ifdef _OPENMP
#pragma omp critical (zmq)
  {
#endif // _OPENMP
    handle = zmq_socket (ctx.ctx, type);
    // ZMQ_SNDTIMEO
    if (handle != NULL) {
      if (linger != -1 && set(ZMQ_LINGER, linger) < 0) {
	except_msg = "ZMQSocket::init: Error setting ZMQ_LINGER to" + std::to_string(linger);
      }
      if (except_msg.empty() && immediate != 0 &&
	  set(ZMQ_IMMEDIATE, immediate) < 0) {
	except_msg = "ZMQSocket::init: Error setting ZMQ_IMMEDIATE to " + std::to_string(immediate);
      }
      if (except_msg.empty() && sndtimeo != -1 &&
	  set(ZMQ_SNDTIMEO, sndtimeo) < 0) {
	except_msg = "ZMQSocket::init: Error setting ZMQ_SNDTIMEO to " + std::to_string(sndtimeo);
      }
    }
#ifdef _OPENMP
  }
#endif // _OPENMP
  if (!except_msg.empty())
    throw std::runtime_error(except_msg);
  if (handle == NULL)
    ygglog_throw_error("ZMQSocket::init: Error creating new socket.");
  if (address && !address->address().empty()) {
    endpoint = address->address();
    if (zmq_connect(handle, endpoint.c_str()) != 0) {
      ygglog_throw_error("ZMQSocket::init: Error connecting to endpoint '" + endpoint + "'");
    }
    ygglog_debug << "ZMQSocket::init: Connected to endpoint '" << endpoint << "'" << std::endl;
  } else {
    const std::string protocol = "tcp";
    std::string host = "localhost";
    if (host == "localhost")
      host = "127.0.0.1";
    std::string address;
#ifdef _OPENMP
#pragma omp critical (zmqport)
    {
#endif
      if (_last_port_set == 0) {
	const char *model_index = getenv("YGG_MODEL_INDEX");
	if (model_index == nullptr) {
	  except_msg = "Environment variable 'YGG_MODEL_INDEX' is not defined. Connot create ZMQComm.";
	  _last_port = -1;
	} else {
	  ygglog_debug << "ZMQSocket::init: model_index = " << model_index << std::endl;
	  _last_port = 49152 + 1000 * static_cast<int>(strtol(model_index, nullptr, 0));
	  _last_port_set = 1;
	  ygglog_debug << "ZMQSocket::init: _last_port = " << _last_port << std::endl;
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
	    } else {
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
	  if (idx_port == std::string::npos) {
	    except_msg = "ZMQSocket::init: Error finding port in endpoint: " + endpoint;
	  } else {
	    _last_port = stoi(endpoint.substr(idx_port + 1));
	    ygglog_debug << "ZMQSocket::init: last_port = " << _last_port << std::endl;
	  }
	}
      }
#ifdef _OPENMP
    }
#endif
    if (!except_msg.empty())
      throw std::runtime_error(except_msg);
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
  if (zmq_sendmsg (handle, &part, 0) != (int)(msg.size())) {
    zmq_msg_close (&part);
    return -1;
  }
  zmq_msg_close (&part);
  return msg.size();
}

int ZMQSocket::recv(std::string& msg, int tout, bool for_identity) {
  // TODO: Rely on ZMQ poll timeout rather than clock?
  clock_t start = clock();
  while ((((double)(clock() - start))/CLOCKS_PER_SEC) < tout && !for_identity) {
    int nmsg = poll(ZMQ_POLLIN, 0);
    if (nmsg < 0) return -1;
    else if (nmsg > 0) break;
    else {
      ygglog_debug << "ZMQSocket::recv: No messages, sleep " << YGG_SLEEP_TIME << std::endl;
      usleep(YGG_SLEEP_TIME);
    }
  }
  if (type == ZMQ_ROUTER && !for_identity) {
    if (recv(msg, 0, true) < 0) {
      ygglog_error << "ZMQSocket::recv: Error receiving identity." << std::endl;
      return -1;
    }
  }
  msg = "";
  int more = 1;
  size_t more_size = sizeof(more);
  do {
    zmq_msg_t part;
    if (zmq_msg_init (&part) != 0)
      return -1;
    if (zmq_recvmsg (handle, &part, 0) == -1) {
      zmq_msg_close (&part);
      return -1;
    }
    msg += std::string((const char*)zmq_msg_data(&part), zmq_msg_size(&part));
    zmq_msg_close (&part);
    if (zmq_getsockopt (handle, ZMQ_RCVMORE, &more, &more_size) != 0)
      return -1;
  } while (more);
  ygglog_debug << "ZMQSocket::recv: Received " << msg.size() << " bytes" << std::endl;
  return msg.size();
}

ZMQSocket::~ZMQSocket() {
  if (handle != NULL)
    zmq_close(handle);
  handle = NULL;
}

//////////////
// ZMQReply //
//////////////

ZMQReply::ZMQReply(DIRECTION dir) :
  sockets(), n_msg(0), n_rep(0), direction(dir), last_idx(-1) {}

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
      if (direction != SEND) {
	ygglog_error << "ZMQReply::set: Only send should bind to reply socket" << std::endl;
	return -1;
      }
      sockets[out].init(ZMQ_REP, "", 0, 1, _zmq_sleeptime);
    } else {
      if (direction != RECV) {
	ygglog_error << "ZMQReply::set: Only recv should bind to reply socket" << std::endl;
	return -1;
      }
      sockets[out].init(ZMQ_REQ, endpoint, 0, 1, _zmq_sleeptime);
    }
  }
  last_idx = out;
  return out;
}
bool ZMQReply::recv(std::string msg_send) {
  if (msg_send.empty())
    msg_send.assign(_reply_msg);
  if (!recv_stage1(msg_send))
    return false;
  return recv_stage2(msg_send);
}
bool ZMQReply::recv_stage1(std::string msg_send) {
  if (msg_send.empty())
    msg_send.assign(_reply_msg);
  if (last_idx < 0)
    return false;
  ZMQSocket* sock = &(sockets[last_idx]);
  ygglog_debug << "ZMQReply::recv_stage1: address = " << sock->endpoint <<
    ", begin" << std::endl;
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
  ygglog_debug << "ZMQReply::recv_stage2: address = " << sock->endpoint <<
    ", begin" << std::endl;
  // Receive
  std::string msg_recv;
  if (sock->recv(msg_recv, timeout) < 0) {
    ygglog_error << "ZMQReply::recv_stage2: Error receiving reponse" << std::endl;
    return false;
  }
  if (msg_recv.empty()) {
    ygglog_error << "ZMQReply::recv_stage2: did not receive" << std::endl;
    return false;
  }
  n_rep++;
  ygglog_debug << "ZMQReply::recv_stage2: address=" << sock->endpoint << ", end" << std::endl;
  last_idx = -1;
  return true;
}

bool ZMQReply::send() {
  std::string msg_data;
  if (!send_stage1(msg_data))
    return false;
  return send_stage2(msg_data);
}
bool ZMQReply::send_stage1(std::string& msg_data) {
  if (sockets.size() == 0) {
    ygglog_error << "ZMQReply::send_stage1: Reply socket was not initialized." << std::endl;
    return false;
  }
  ZMQSocket* sock = &(sockets[0]);
  if (sock->recv(msg_data, timeout) < 0) {
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
  ZMQSocket* sock = &(sockets[0]);
  sock->poll(ZMQ_POLLOUT, timeout);
  if (sock->send(msg_data) < 0) {
    ygglog_error << "ZMQReply::send_stage2: Error sending reply frame." << std::endl;
    return false;
  }
  // Check for purge or EOF
  // if (is_purge) {
  //   ygglog_debug << "ZMQReply::send_stage2: PURGE received" << std::endl;
  //   n_msg = 0;
  //   n_rep = 0;
  //   return send();
  // } else {
  n_rep++;
  // }
  ygglog_debug << "ZMQReply::send_stage2: address=" << sock->endpoint
	       << ", end" << std::endl;
  return true;
}


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

void ZMQComm::init() {
  maxMsgSize = 1048576;
  msgBufSize = 100;
  if (!(flags & COMM_FLAG_VALID))
    return;
  if (init_handle()) {
    flags |= COMM_FLAG_VALID;
  } else {
    flags &= ~COMM_FLAG_VALID;
  }
}

bool ZMQComm::init_handle() {
  if (handle != nullptr) {
    delete handle;
    handle = nullptr;
  }
  if (flags & COMM_FLAG_CLIENT) {
    handle = new ZMQSocket(ZMQ_ROUTER, address);
  } else if (flags & (COMM_FLAG_SERVER | COMM_ALLOW_MULTIPLE_COMMS)) {
    handle = new ZMQSocket(ZMQ_DEALER, address);
  } else {
    handle = new ZMQSocket(ZMQ_PAIR, address);
  }
  if (handle == nullptr) {
    ygglog_error << "create_new: Could not initialize empty socket." << std::endl;
    return false;
  }
  if (address == nullptr)
    address = new utils::Address(handle->endpoint);
  else
    address->address(handle->endpoint);
  if (this->name.empty())
    this->name = "tempnewZMQ-" + handle->endpoint.substr(handle->endpoint.find_last_of(':') + 1);
  if (direction == SEND)
    flags |= COMM_ALWAYS_SEND_HEADER;
  return true;
}
/*!
  @brief Initialize a ZeroMQ communication.
  @param[in] comm comm_t * Comm structure initialized with init_comm_base.
  @returns int -1 if the comm could not be initialized.
 */
ZMQComm::ZMQComm(const std::string name, utils::Address *address,
		 const DIRECTION direction, int flgs) :
  CommBase(address, direction, ZMQ_COMM, flgs), reply(direction) {
  this->name = name;
  init();
}
ZMQComm::ZMQComm(const std::string name, const DIRECTION direction,
		 int flgs) :
  CommBase(name, direction, ZMQ_COMM, flgs), reply(direction) {
  init();
}

ZMQComm::~ZMQComm() {
    destroy();
}

void ZMQComm::destroy() {
    // Drain input
    if (direction == RECV && flags & COMM_FLAG_VALID
        && (!(flags & COMM_EOF_RECV))) {
        if (utils::_ygg_error_flag == 0) {
	    size_t data_len = 0;
            char *data = NULL;
            while (comm_nmsg() > 0) {
                if (long ret = recv(data, data_len, true) >= 0) {
		  if (ret > (long)data_len)
		    data_len = ret;
                }
            }
	    if (data != NULL)
	      free(data);
        }
    }
    if (handle != nullptr) {
        delete handle;
        ygglog_debug << "ZMQComm(" << name << ")::destroy: Destroying socket " << address->address() << std::endl;
        handle = nullptr;
    }
    ygglog_debug << "ZMQComm(" << name << ")::destroy: finished" << std::endl;

    //TODO: THERE IS MORE TO DELETE?
}

/*!
  @brief Get number of messages in the comm.
  @returns int Number of messages. -1 indicates an error.
 */
int ZMQComm::comm_nmsg() const {
    int out = 0;
    if (direction == RECV) {
        if (handle != nullptr) {
	    return handle->poll(ZMQ_POLLIN, short_timeout);
        }
    } else {
      ygglog_debug << "ZMQComm(" << name << ")::comm_nmsg: nmsg = " << reply.n_msg << ", nrep = "
		   << reply.n_rep << std::endl;
      out = reply.n_msg - reply.n_rep;
    }
    return out;
}

/*!
  @brief Send a message to the comm.
  Send a message smaller than YGG_MSG_MAX bytes to an output comm. If the
  message is larger, it will not be sent.
  @param[in] data character pointer to message that should be sent.
  @param[in] len size_t length of message to be sent.
  @returns int 0 if send succesfull, -1 if send unsuccessful.
 */
int ZMQComm::send_single(const char* data, const size_t &len, const Header& header) {
    ygglog_debug << "ZMQComm(" << name << ")::send_single: " << len << " bytes" << std::endl;
  if (!check_size(len)) {
    ygglog_error << "ZMQComm(" << name << ")::send_single: Message too large" << std::endl;
    return -1;
  }
  std::string msg(data, len);
  int ret = handle->send(msg);
  if (ret < 0) {
    ygglog_error << "ZMQComm(" << name << ")::send_single: Error in ZMQSocket::send" << std::endl;
    return -1;
  }
  // Reply
  if (!do_reply_send(header)) {
      ygglog_error << "ZMQComm(" << name << ")::send_single: Error in do_reply_send" << std::endl;
  }
  ygglog_debug << "ZMQComm(" << name << ")::send_single: returning " << ret << std::endl;
  return ret;
}
bool ZMQComm::do_reply_send(const Header& header) {
  if (header.flags & (HEAD_FLAG_CLIENT_SIGNON | HEAD_FLAG_SERVER_SIGNON))
    return true;
#ifdef YGG_TEST
  // Exit early to prevent deadlock when running from the same thread
  return true;
#else //YGG_TEST
  return reply.send();
#endif // YGG_TEST
}

/*!
  @brief Receive a message from an input comm.
  Receive a message smaller than YGG_MSG_MAX bytes from an input comm.
  @param[in] x comm_t* structure that message should be sent to.
  @param[out] data char ** pointer to allocated buffer where the message
  should be saved. This should be a malloc'd buffer if allow_realloc is 1.
  @param[in] len const size_t length of the allocated message buffer in bytes.
  @param[in] allow_realloc const int If 1, the buffer will be realloced if it
  is not large enought. Otherwise an error will be returned.
  @returns int -1 if message could not be received. Length of the received
  message if message was received.
 */
long ZMQComm::recv_single(char*& data, const size_t &len,
			  bool allow_realloc) {
    long ret = -1;
    ygglog_debug << "ZMQComm(" << name << ")::recv_single " << std::endl;
    if (handle == nullptr) {
        ygglog_error << "ZMQComm(" << name << ")::recv_single: socket handle is nullptr" << std::endl;
        return ret;
    }

    std::string msg;
    if ((ret = handle->recv(msg)) < 0)
        return ret;
    
    // Check for server signon and respond
    while (msg.compare(0, 23, "ZMQ_SERVER_SIGNING_ON::") == 0) {
      ygglog_debug << "ZMQComm(" << name << ")::recv_single: Received sign-on" << std::endl;
      std::string client_address = msg.substr(23);

      // create a DEALER socket and connect to address
      ZMQSocket* client_socket = NULL;
      try {
	client_socket = new ZMQSocket(ZMQ_DEALER, client_address,
				      _zmq_sleeptime, 1,
				      _zmq_sleeptime);
      } catch (...) {
	ygglog_error << "ZMQComm(" << name << ")::recv_single: "
		     << "Could not initalize the client side of the proxy socket to confirm signon" << std::endl;
	return -1;
      }
      if (client_socket->send(msg) < 0) {
	ygglog_error << "ZMQComm(" << name << ")::recv_single: Error sending response message." << std::endl;
	delete client_socket;
	return -1;
      }
      delete client_socket;
      
      if ((ret = handle->recv(msg)) < 0) {
	ygglog_debug << "ZMQComm(" << name << ")::recv_single: did not receive" << std::endl;
	return ret;
      }
    }

    ret = this->copyData(data, len, msg.c_str(), msg.size(), allow_realloc);
    if (ret < 0) {
      ygglog_error << "ZMQComm(" << name << ")::recv_single: Error copying data" << std::endl;
      return ret;
    }
    ygglog_debug << "ZMQComm(" << name << ")::recv_single: received " << ret << " bytes" << std::endl;
    // Extract reply address from header
    Header head;
    if (!this->create_header_recv(head, data, ret, ret, false, true)) {
      ygglog_error << "ZMQComm(" << name << ")::recv_single: Invalid header." << std::endl;
      return -1;
    }
    if (!do_reply_recv(head)) {
      ygglog_error << "ZMQComm(" << name << ")::recv_single: Error in do_reply_recv" << std::endl;
      return -1;
    }
    ygglog_debug << "ZMQComm(" << name << ")::recv_single: returns " << ret << " bytes" << std::endl;
    return ret;
}
bool ZMQComm::do_reply_recv(const Header& header) {
  if (header.flags & (HEAD_FLAG_CLIENT_SIGNON | HEAD_FLAG_SERVER_SIGNON))
    return true;
#ifdef YGG_TEST
    // Exit early to prevent deadlock when running from the same thread
    return true;
#else // YGG_TEST
    return reply.recv();
#endif // YGG_TEST
}

bool ZMQComm::create_header_send(Header& header, const char* data, const size_t &len) {
  if (!Comm_t::create_header_send(header, data, len))
    return false;
  if (!(header.flags & (HEAD_FLAG_CLIENT_SIGNON |
			HEAD_FLAG_SERVER_SIGNON))) {
    std::string reply_address;
    if (reply.create(reply_address) < 0) {
      ygglog_error << "ZMQComm(" << this->name << ")::create_header_send: Error during creation of reply socket" << std::endl;
      return false;
    }
    ygglog_debug << "ZMQComm(" << this->name << ")::create_header_send: zmq_reply = " << reply_address << std::endl;
    if (!header.SetMetaString("zmq_reply", reply_address)) {
      ygglog_debug << "ZMQComm(" << this->name << ")::create_header_send: Failed to set zmq_reply" << std::endl;
    return false;
    }
  }
  return true;
}

bool ZMQComm::create_header_recv(Header& header, char*& data,
				 const size_t &len,
				 size_t msg_len, int allow_realloc,
				 int temp) {
  bool out = Comm_t::create_header_recv(header, data, len, msg_len,
					allow_realloc, temp);
  if (temp && out &&
      (!(header.flags & (HEAD_FLAG_CLIENT_SIGNON |
			 HEAD_FLAG_SERVER_SIGNON)))) {
    std::string adr;
    if ((flags & COMM_FLAG_WORKER) && (reply.sockets.size() == 1)) {
      adr = reply.sockets[0].endpoint;
    } else {
      const char* address_c = header.GetMetaString("zmq_reply");
      adr.assign(address_c);
    }
    if (reply.set(adr) < 0)
      return false;
  }
  return true;
}

Comm_t* ZMQComm::create_worker(utils::Address* address,
			       const DIRECTION dir, int flgs) {
  return new ZMQComm("", address, dir, flgs);
}
Comm_t* ZMQComm::create_worker_recv(Header& head) {
  ZMQComm* out = dynamic_cast<ZMQComm*>(Comm_t::create_worker_recv(head));
  if (!out)
    return out;
  const char* zmq_reply_worker = head.GetMetaString("zmq_reply_worker");
  if (zmq_reply_worker == NULL) {
    ygglog_error << "ZMQComm(" << name << ")::create_worker_recv: Error getting address" << std::endl;
    delete out;
    return NULL;
  }
  if (out->reply.set(std::string(zmq_reply_worker)) < 0) {
    ygglog_error << "ZMQComm(" << name << ")::create_worker_recv: Failed to set worker reply address." << std::endl;
    delete out;
    return NULL;
  }
  return out;
}

#ifdef YGG_TEST
bool ZMQComm::afterSendRecv(Comm_t* sComm, Comm_t* rComm) {
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
  return true;
}
#endif // YGG_TEST
// Definitions in the case where ZMQ libraries not installed
#else /*ZMQINSTALLED*/

/*!
  @brief Print error message about ZMQ library not being installed.
 */
static inline
void zmq_install_error() {
  ygglog_throw_error("Compiler flag 'ZMQINSTALLED' not defined so ZMQ bindings are disabled.");
};

////////////////
// ZMQContext //
////////////////

void* ZMQContext::ygg_s_process_ctx = NULL;

ZMQContext::ZMQContext() : ctx(NULL) {
  zmq_install_error();
}
ZMQContext::ZMQContext(const ZMQContext& rhs) : ctx(rhs.ctx) {
  zmq_install_error();
}

void ZMQContext::destroy() {
  zmq_install_error();
}


///////////////
// ZMQSocket //
///////////////

int ZMQSocket::_last_port = 0;
int ZMQSocket::_last_port_set = 0;

ZMQSocket::ZMQSocket() :
  handle(NULL), endpoint(), type(0), ctx() {
  zmq_install_error();
}
ZMQSocket::ZMQSocket(int type0, utils::Address*,
		     int, int, int) :
  handle(NULL), endpoint(), type(type0), ctx() {
  zmq_install_error();
}
ZMQSocket::ZMQSocket(int type0, std::string,
		     int, int, int) :
  handle(NULL), endpoint(), type(type0), ctx() {
  zmq_install_error();
}
ZMQSocket::ZMQSocket(const ZMQSocket& rhs) :
  handle(NULL), endpoint(), type(rhs.type), ctx() {
  zmq_install_error();
}

template<typename T>
int ZMQSocket::set(int, const T&) {
  zmq_install_error();
  return -1;
}
void ZMQSocket::init(int, std::string,
		     int, int, int) {
  zmq_install_error();
}
void ZMQSocket::init(int, utils::Address*,
		     int, int, int) {
  zmq_install_error();
}

int ZMQSocket::poll(int, int) {
  zmq_install_error();
  return -1;
}

int ZMQSocket::send(const std::string) {
  zmq_install_error();
  return -1;
}

int ZMQSocket::recv(std::string&, int, bool) {
  zmq_install_error();
  return -1;
}

ZMQSocket::~ZMQSocket() {
  // No error as constructor should have raised one
}

//////////////
// ZMQReply //
//////////////

ZMQReply::ZMQReply(DIRECTION dir) :
  sockets(), n_msg(0), n_rep(0), direction(dir), last_idx(-1) {
  zmq_install_error();
}

void ZMQReply::clear() {
  zmq_install_error();
}

int ZMQReply::create(std::string&) {
  zmq_install_error();
  return -1;
}

int ZMQReply::find(std::string) {
  zmq_install_error();
  return -1;
}

int ZMQReply::set(std::string) {
  zmq_install_error();
  return -1;
}
bool ZMQReply::recv(std::string) {
  zmq_install_error();
  return false;
}
bool ZMQReply::recv_stage1(std::string) {
  zmq_install_error();
  return false;
}
bool ZMQReply::recv_stage2(std::string) {
  zmq_install_error();
  return false;
}

bool ZMQReply::send() {
  zmq_install_error();
  return false;
}
bool ZMQReply::send_stage1(std::string&) {
  zmq_install_error();
  return false;
}
bool ZMQReply::send_stage2(const std::string) {
  zmq_install_error();
  return false;
}

/////////////
// ZMQComm //
/////////////

ZMQComm::~ZMQComm() {
  // No error as constructor should have raised one
}

int ZMQComm::comm_nmsg() const {
  zmq_install_error();
  return -1;
}

int ZMQComm::send_single(const char *, const size_t &, const Header&) {
  zmq_install_error();
  return -1;
}

long ZMQComm::recv_single(char *&, const size_t &, bool) {
  zmq_install_error();
  return -1;
}

void ZMQComm::init() {
  zmq_install_error();
}

bool ZMQComm::init_handle() {
  zmq_install_error();
  return false;
}

bool ZMQComm::do_reply_recv(const Header&) {
  zmq_install_error();
  return false;
}

bool ZMQComm::do_reply_send(const Header&) {
  zmq_install_error();
  return false;
}

bool ZMQComm::create_header_send(Header&, const char*, const size_t&) {
  zmq_install_error();
  return false;
}

bool ZMQComm::create_header_recv(Header&, char*&, const size_t&,
				 size_t, int, int) {
  zmq_install_error();
  return false;
}

Comm_t* ZMQComm::create_worker(utils::Address*, const DIRECTION, int) {
  zmq_install_error();
  return NULL;
}

Comm_t* ZMQComm::create_worker_recv(Header&) {
  zmq_install_error();
  return NULL;
}

void ZMQComm::destroy() {
  zmq_install_error();
}

ZMQComm::ZMQComm(const std::string, utils::Address *address,
		 const DIRECTION direction, int flgs) :
  CommBase(address, direction, ZMQ_COMM, flgs), reply(direction) {
  zmq_install_error();
}

ZMQComm::ZMQComm(const std::string name, const DIRECTION direction,
		 int flgs) :
  CommBase(name, direction, ZMQ_COMM, flgs), reply(direction) {
  zmq_install_error();
}

#ifdef YGG_TEST
bool ZMQComm::afterSendRecv(Comm_t*, Comm_t*) {
  zmq_install_error();
  return false;
}
#endif

#endif /*ZMQINSTALLED*/
