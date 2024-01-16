#include "RMQComm.hpp"

using namespace YggInterface::communicator;
using namespace YggInterface::utils;

// TODO: Check that server can be connected

#define RMQSTATUS_(method, msg)						\
  {									\
    if (!_check_amqp_error(method, msg)) {				\
      return -1;							\
    }									\
  }
#define RMQSTATUS_REPLY_(method, msg)					\
  {									\
    if (!_check_amqp_reply_error(method, msg)) {			\
      return -1;							\
    }									\
  }
#define ASSIGNSTR_RMQBYTES(dst, src)		\
  dst.assign(static_cast<const char*>(src))
#define STR2RMQBYTES(src)			\
  ((src.empty()) ? amqp_empty_bytes : amqp_cstring_bytes(src.c_str()))
RMQConnection::RMQConnection(const std::string logInst, DIRECTION dir,
			     const std::string& addr) :
  LogBase(), logInst_(logInst), address(addr), direction(dir),
#ifdef RMQINSTALLED
  conn(amqp_new_connection()), socket(NULL), channel(1),
#endif // RMQINSTALLED
  url(""), host(""), user(""), password(""), port(-1), vhost(""),
  exchange(""), queue_name("") {
  if (init() < 0) {
    throw_error("RMQConnection: Failed to initialize connection");
  }
}
RMQConnection::~RMQConnection() {
  if (close() < 0) {
    throw_error("RMQConnection: Failed to close connection");
  }
}
#ifdef RMQINSTALLED
int RMQConnection::init() {
  if (address.empty()) {
    std::string portstr;
    // TODO: Use amqp_default_connection_info values
#define GET_RABBITMQ_VAR(var, env, def)			\
    {							\
      char* tmp_envvar = getenv(#env);			\
      if (tmp_envvar) {					\
	var.assign(tmp_envvar);				\
      } else {						\
	var.assign(#def);				\
	if (var == "-EMPTY-") {				\
	  var.clear();					\
	}						\
      }							\
    }
    GET_RABBITMQ_VAR(user, RABBITMQ_USER, guest);
    GET_RABBITMQ_VAR(password, RABBITMQ_PASSWORD, guest);
    GET_RABBITMQ_VAR(host, RABBITMQ_HOST, localhost); // 127.0.0.1);
    GET_RABBITMQ_VAR(vhost, RABBITMQ_VHOST, /);
    GET_RABBITMQ_VAR(portstr, RABBITMQ_PORT, 5672);
    GET_RABBITMQ_VAR(exchange, RABBITMQ_EXCHANGE, -EMPTY-);
    GET_RABBITMQ_VAR(queue_name, RABBITMQ_QUEUE, -EMPTY-);
#undef GET_RABBITMQ_VAR
    port = std::stoi(portstr);
    // if (vhost.empty()) {
    //   vhost = "%2f";
    // }
    _format_address();
  } else {
    std::vector<std::string> parts = YggInterface::utils::split(
	  address, _RMQ_PARAM_SEP);
    if (parts.empty()) {
      log_error() << "init: Error parsing address: " << address <<
	std::endl;
      return -1;
    }
    url = parts[0];
    if (parts.size() == 3) {
      exchange = parts[1];
      queue_name = parts[2];
    } else if (parts.size() != 1) {
      log_error() << "init: Error parsing address: " << address <<
	std::endl;
      return -1;
    }
    amqp_connection_info url_info;
    char* url_cpy = (char*)malloc(url.size() + 1);
    memcpy(url_cpy, url.c_str(), url.size());
    url_cpy[url.size()] = '\0';
    // Hack to allow vhost of /
    if (url_cpy[url.size() - 1] == '/' && url_cpy[url.size() - 2]) {
      url_cpy[url.size() - 1] = '\0';
      url_cpy[url.size() - 2] = '\0';
    }
    RMQSTATUS_(amqp_parse_url(url_cpy, &url_info), ("init: " + url));
    // ASSIGNSTR_RMQBYTES(url, url_cpy);
    ASSIGNSTR_RMQBYTES(user, url_info.user);
    ASSIGNSTR_RMQBYTES(password, url_info.password);
    ASSIGNSTR_RMQBYTES(host, url_info.host);
    ASSIGNSTR_RMQBYTES(vhost, url_info.vhost);
    port = url_info.port;
    free(url_cpy);
    url_cpy = NULL;
    _format_address();
  }
  // TODO: Other types of sockets?
  socket = amqp_tcp_socket_new(conn);
  if (!socket) {
    log_error() << "init: Failed to create a new socket" << std::endl;
    return -1;
  }
  RMQSTATUS_(amqp_socket_open(socket, host.c_str(), port),
	     "init: Failed to open socket");
  RMQSTATUS_REPLY_(amqp_login(conn, vhost.c_str(), 0, 131072, 0,
			      AMQP_SASL_METHOD_PLAIN,
			      user.c_str(), password.c_str()),
			      // STR2RMQBYTES(user),
			      // STR2RMQBYTES(password)),
		   "init: Failed to complete login");
  amqp_channel_open(conn, channel);
  RMQSTATUS_REPLY_(amqp_get_rpc_reply(conn),
		   "init: Failed to get rpc reply");
  amqp_boolean_t passive = 0;
  if (queue_name.rfind("amq.", 0) == 0)
    passive = 1;
  amqp_queue_declare_ok_t *r = amqp_queue_declare(
    conn, channel, STR2RMQBYTES(queue_name), passive,
    0, 0, 1, amqp_empty_table);
  RMQSTATUS_REPLY_(amqp_get_rpc_reply(conn),
		   ("init: Failed to declare a queue: " + queue_name));
  if (queue_name.empty()) {
    ASSIGNSTR_RMQBYTES(queue_name, r->queue.bytes);
    _format_address();
  }
  log_debug() << "init: Declared queue \"" << queue_name <<
    "\" (passive = " << passive << ")" << std::endl;
  if (!exchange.empty()) {
    amqp_queue_bind(conn, channel,
		    STR2RMQBYTES(queue_name),
		    STR2RMQBYTES(exchange),
		    STR2RMQBYTES(queue_name),
		    amqp_empty_table);
    RMQSTATUS_REPLY_(amqp_get_rpc_reply(conn),
		     "init: Failed to bind to queue");
  }
  // if (direction == RECV) {
  //   amqp_basic_consume(conn, channel,
  // 		       STR2RMQBYTES(queue_name),
  // 		       amqp_empty_bytes, 0, 1, 0,
  // 		       amqp_empty_table);
  //   RMQSTATUS_REPLY_(amqp_get_rpc_reply(conn),
  // 		     "init: Failed to begin consuming");
  // }
  return 0;
}
int RMQConnection::close() {
  RMQSTATUS_REPLY_(amqp_channel_close(conn, channel, AMQP_REPLY_SUCCESS),
		   "close: Error closing channel");
  RMQSTATUS_REPLY_(amqp_connection_close(conn, AMQP_REPLY_SUCCESS),
		   "close: Error closing connection");
  RMQSTATUS_(amqp_destroy_connection(conn),
	     "close: Error destroying connection");
  return 0;
}
int RMQConnection::nmsg(DIRECTION) const {
  // amqp_rpc_reply_t ret = amqp_basic_get(conn, channel,
  // 					STR2RMQBYTES(queue_name), 1);
  // if (!_check_amqp_reply_error(ret, "nmsg: Failed to get nmsg"))
  //   return -1;
  // return (int)(ret.reply.id == AMQP_BASIC_GET_OK_METHOD);
  // Passive queue declare to get message count
  amqp_queue_declare_ok_t *r = amqp_queue_declare(
    conn, channel, STR2RMQBYTES(queue_name),
    1, 0, 0, 1, amqp_empty_table);
  if (!r) {
    log_error() << "nmsg: Error in amqp_queue_declare" << std::endl;
    return -1;
  }
  RMQSTATUS_REPLY_(amqp_get_rpc_reply(conn),
		   ("nmsg: Failed to declare a passive queue: " + queue_name));
  int out = static_cast<int>(r->message_count);
  return out;
}
int RMQConnection::send(utils::Header& header) {
  amqp_bytes_t message_bytes;
  amqp_basic_properties_t properties;
  properties._flags = 0;
  properties._flags |= AMQP_BASIC_DELIVERY_MODE_FLAG;
  properties.delivery_mode = AMQP_DELIVERY_NONPERSISTENT;
  message_bytes.len = header.size_msg;
  message_bytes.bytes = header.data_msg();
  RMQSTATUS_(amqp_basic_publish(conn, channel,
				STR2RMQBYTES(exchange),
				STR2RMQBYTES(queue_name),
				1, 0, &properties, message_bytes),
	     "send: Error publishing message");
  return 1;
}
long RMQConnection::recv(utils::Header& header) {
  amqp_message_t message;
  amqp_rpc_reply_t rpc_reply = amqp_basic_get(conn, channel,
					      STR2RMQBYTES(queue_name), 1);
  if (!_check_amqp_reply_error(rpc_reply, "nmsg: Failed to get nmsg"))
    return -1;
  if (rpc_reply.reply.id != AMQP_BASIC_GET_OK_METHOD) {
    log_error() << "recv: No message waiting" << std::endl;
    return -1;
  }
  // amqp_envelope_t envelope;
  // amqp_maybe_release_buffers(conn);
  // RMQSTATUS_REPLY_(amqp_consume_message(conn, &envelope, NULL, 0),
  RMQSTATUS_REPLY_(amqp_read_message(conn, channel, &message, 0),
		   "recv: Failed to read message");
  long ret = header.on_recv(static_cast<const char*>(message.body.bytes),
			    static_cast<size_t>(message.body.len));
  amqp_destroy_message(&message);
  // amqp_destroy_envelope(&envelope);
  if (ret < 0) {
    log_error() << "recv: Error copying data" << std::endl;
    return ret;
  }
  return ret;
}

void RMQConnection::_format_address() {
  url = "amqp://" + user + ":" + password + "@" + host + ":"
    + std::to_string(port) + "/" + vhost;
  address = url +
    _RMQ_PARAM_SEP + exchange +
    _RMQ_PARAM_SEP + queue_name;
  log_debug() << "_format_address: " << address << std::endl;
}
bool RMQConnection::_check_amqp_error(int x, const std::string& context) const {
  if (x < 0) {
    log_error() << context << ": " << amqp_error_string2(x) << std::endl;
    return false;
  }
  return true;
}
bool RMQConnection::_check_amqp_reply_error(amqp_rpc_reply_t x,
					    const std::string& context) const {
  switch (x.reply_type) {
  case AMQP_RESPONSE_NORMAL:
    return true;
  case AMQP_RESPONSE_NONE:
    log_error() << context << ": missing RPC reply type!" << std::endl;
    break;
  case AMQP_RESPONSE_LIBRARY_EXCEPTION:
    log_error() << context << ": " << amqp_error_string2(x.library_error) << std::endl;
    break;
  case AMQP_RESPONSE_SERVER_EXCEPTION:
    switch (x.reply.id) {
    case AMQP_CONNECTION_CLOSE_METHOD: {
      amqp_connection_close_t *m =
	(amqp_connection_close_t *)x.reply.decoded;
      log_error() << context << ": server connection error " <<
	m->reply_code << ", message: " <<
	(const char*)(m->reply_text.bytes) << std::endl;
      break;
    }
    case AMQP_CHANNEL_CLOSE_METHOD: {
      amqp_channel_close_t *m = (amqp_channel_close_t *)x.reply.decoded;
      log_error() << context << ": server channel error " <<
	m->reply_code << ", message: " <<
	(const char*)(m->reply_text.bytes) << std::endl;
      break;
    }
    default:
      log_error() << context << ": unknown server error, method id " <<
	x.reply.id << std::endl;
      break;
    }
    break;
  }
  return false;
}


#else // RMQINSTALLED
int RMQConnection::init() { return -1; }
int RMQConnection::close() { return -1; }
int RMQConnection::nmsg(DIRECTION) const { return -1; }
int RMQConnection::send(utils::Header&) { return -1; }
long RMQConnection::recv(utils::Header&) { return -1; }
#endif // RMQINSTALLED

RMQComm::RMQComm(const std::string name,
		 const utils::Address& address,
		 const DIRECTION direction, int flgs,
		 const COMM_TYPE commtype) :
  CommBase(name, address, direction, commtype, flgs) {
  ADD_CONSTRUCTOR_OPEN(RMQComm)
}

ADD_CONSTRUCTORS_DEF(RMQComm)

void RMQComm::_open(bool call_base) {
  BEFORE_OPEN_DEF;
  updateMaxMsgSize(1048576); // 2**20
  handle = new RMQConnection(logInst(), direction, address.address());
  if (!address.valid())
    address.address(handle->address);
  AFTER_OPEN_DEF;
}

void RMQComm::_close(bool call_base) {
  BEFORE_CLOSE_DEF;
  AFTER_CLOSE_DEF;
}
	
int RMQComm::comm_nmsg(DIRECTION dir) const {
  if (global_comm)
    return global_comm->comm_nmsg(dir);
  if (dir == NONE)
    dir = direction;
  if (dir != direction)
    return 0;
  return handle->nmsg(dir);
}
      
int RMQComm::send_single(utils::Header& header) {
  assert((!global_comm) && handle);
  if (header.on_send() < 0)
    return -1;
  log_debug() << "send_single: " << header.size_msg << " bytes" <<
    std::endl;
  int ret = handle->send(header);
  log_debug() << "send_single: returning " << ret << std::endl;
  return ret;
}

long RMQComm::recv_single(utils::Header& header) {
  assert((!global_comm) && handle);
  log_debug() << "recv_single " << std::endl;
  long ret = handle->recv(header);
  log_debug() << "recv_single: returns " << ret << " bytes" << std::endl;
  return ret;
}

WORKER_METHOD_DEFS(RMQComm)

#undef ASSIGNSTR_RMQBYTES
