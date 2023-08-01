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
#ifdef _OPENMP
#pragma omp critical (zmq)
  {
    if (ZMQContext::ygg_s_process_ctx == NULL) {
      if (get_thread_id() == 0) {
	ygglog_debug << "ZMQContext::init: Creating ZMQ context." << std::endl;
#endif // _OPENMP
	ctx = zmq_ctx_new();
#ifdef _OPENMP
      } else {
	ygglog_throw_error("ZMQContext::init: Can only initialize the "
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
    ygglog_throw_error("ZMQContext::init: ZMQ context is NULL.");
  }
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
#ifdef _OPENMP
#pragma omp critical (zmq)
  {
#endif // _OPENMP
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
#ifdef _OPENMP
  }
#endif // _OPENMP
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
#ifdef _OPENMP
#pragma omp critical (zmqport)
    {
#endif
      if (_last_port_set == 0) {
	const char *model_index = getenv("YGG_MODEL_INDEX");
	if (model_index == NULL) {
	  except_msg = "Environment variable 'YGG_MODEL_INDEX' is not defined. Connot create ZMQComm.";
	  _last_port = -1;
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
#ifdef _OPENMP
    }
#endif
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

int ZMQSocket::recv(std::string& msg, bool for_identity) {
  if (type == ZMQ_ROUTER && !for_identity) {
    if (recv(msg, true) < 0) {
      ygglog_error << "ZMQSocket::recv: Error receiving identity." << std::endl;
      return -1;
    }
  }
  msg = "";
  int more = 1;
  size_t more_size = sizeof(more);
  ygglog_debug << "ZMQSocket::recv: Receiving message" << std::endl;
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
  sock->poll(ZMQ_POLLIN, timeout);
  if (sock->recv(msg_recv) < 0) {
    ygglog_error << "ZMQReply::recv_stage2: Error receiving reponse" << std::endl;
    return false;
  }
  n_rep++;
  ygglog_debug << "ZMQReply::recv_stage2: address=" << sock->endpoint << ", end" << std::endl;
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
  // TODO: Handle multiple comms
  // if (flags & (COMM_FLAG_CLIENT | COMM_FLAG_SERVER_RESPONSE)) {
  //   handle = new ZMQSocket(ZMQ_ROUTER, address);
  // } else if (flags & (COMM_FLAG_SERVER | COMM_FLAG_CLIENT_RESPONSE |
  // 		      COMM_ALLOW_MULTIPLE_COMMS)) {
  //   handle = new ZMQSocket(ZMQ_DEALER, address);
  // } else {
  handle = new ZMQSocket(ZMQ_PAIR, address);
  // }
  address->address(handle->endpoint);
  if (this->name.empty())
    this->name = "tempnewZMQ-" + handle->endpoint.substr(handle->endpoint.find_last_of(':') + 1);
  if (direction == SEND)
    flags |= COMM_ALWAYS_SEND_HEADER;
}

ZMQComm::~ZMQComm() {
    ygglog_debug << "~ZMQComm: Started" << std::endl;
    if ((direction == RECV) && this->is_open() &&
	(!global_comm) && (!(flags & COMM_EOF_RECV))) {
      if (utils::YggdrasilLogger::_ygg_error_flag == 0) {
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
    ygglog_debug << "~ZMQComm: Finished" << std::endl;
}

int ZMQComm::comm_nmsg() const {
    if (global_comm)
      return global_comm->comm_nmsg();
    int out = 0;
    if (direction == RECV) {
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

int ZMQComm::send_single(const char* data, const size_t &len, const Header& header) {
  // Should never be called with global comm
  // if (global_comm)
  //   return global_comm->send_single(data, len, header);
  assert(!global_comm);
  ygglog_debug << "ZMQComm(" << name << ")::send_single: " << len << " bytes" << std::endl;
  std::string msg(data, len);
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
bool ZMQComm::do_reply_send(const Header& header) {
  if (header.flags & (HEAD_FLAG_CLIENT_SIGNON | HEAD_FLAG_SERVER_SIGNON))
    return true;
  return reply.send();
}

long ZMQComm::recv_single(char*& data, const size_t &len,
			  bool allow_realloc) {
    // Should never be called with global comm
    // if (global_comm)
    //   return global_comm->recv_single(data, len, allow_realloc);
    assert((!global_comm) && handle);
    long ret = -1;
    ygglog_debug << "ZMQComm(" << name << ")::recv_single " << std::endl;

    std::string msg;
    if ((ret = handle->recv(msg)) < 0)
        return ret;
    
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
  // Should never be called with global comm
  // if (global_comm)
  //   return dynamic_cast<ZMQComm*>(global_comm)->do_reply_recv(header);
  assert(!global_comm);
  if (header.flags & (HEAD_FLAG_CLIENT_SIGNON | HEAD_FLAG_SERVER_SIGNON))
    return true;
  return reply.recv();
}

bool ZMQComm::create_header_send(Header& header, const char* data, const size_t &len) {
  // Should never be called with global comm
  // if (global_comm)
  //   return global_comm->create_header_send(header, data, len);
  assert(!global_comm);
  bool out = Comm_t::create_header_send(header, data, len);
  if (out && !(header.flags & (HEAD_FLAG_CLIENT_SIGNON |
			       HEAD_FLAG_SERVER_SIGNON))) {
    std::string reply_address;
    reply.create(reply_address);
    ygglog_debug << "ZMQComm(" << this->name << ")::create_header_send: zmq_reply = " << reply_address << std::endl;
    header.SetMetaString("zmq_reply", reply_address);
  }
  return out;
}

bool ZMQComm::create_header_recv(Header& header, char*& data,
				 const size_t &len,
				 size_t msg_len, int allow_realloc,
				 int temp) {
  // Should never be called with global comm
  // if (global_comm)
  //   return global_comm->create_header_recv(header, data, len, msg_len,
  // 					   allow_realloc, temp);
  assert(!global_comm);
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
    reply.set(adr);
  }
  return true;
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
    head.SetMetaString("zmq_reply_worker", reply_address);
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
    const char* zmq_reply_worker = head.GetMetaString("zmq_reply_worker");
    out->reply.set(std::string(zmq_reply_worker));
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

