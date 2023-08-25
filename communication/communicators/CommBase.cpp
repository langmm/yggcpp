#include "comms.hpp"

// #ifdef _OPENMP
// int communication::communicator::global_scope_comm = 1;
// #else
int communication::communicator::global_scope_comm = 0;
// #endif

using namespace communication::communicator;
using namespace communication::utils;

void _cleanup_wrapper() {
  Comm_t::_ygg_cleanup();
}

void communication::communicator::global_scope_comm_on() {
  global_scope_comm = 1;
}
void communication::communicator::global_scope_comm_off() {
  // #ifndef _OPENMP
  global_scope_comm = 0;
  // #endif
}

void Comm_t::_ygg_init() {
  YGG_THREAD_SAFE_BEGIN(init) {
    if (!Comm_t::_ygg_initialized) {
      ygglog_debug << "_ygg_init: Begin initialization" << std::endl;
#if defined(ZMQINSTALLED)
      ZMQContext* ctx = new ZMQContext();
      delete ctx;
#endif
#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
      rapidjson::initialize_python("_ygg_init");
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
      ygglog_debug << "_ygg_init: Registering cleanup" << std::endl;
      std::atexit(_cleanup_wrapper);
      Comm_t::_ygg_initialized = 1;
    }
  } YGG_THREAD_SAFE_END;
}

void Comm_t::_ygg_cleanup() {
  YGG_THREAD_SAFE_BEGIN(clean) {
    if (!Comm_t::_ygg_finalized) {
      ygglog_debug << "_ygg_cleanup: Begin cleanup of " << Comm_t::registry.size() << " communicators" << std::endl;
      for (size_t i = 0; i < Comm_t::registry.size(); i++) {
	if (Comm_t::registry[i]) {
	  if (Comm_t::registry[i]->flags & COMM_FLAG_DELETE) {
	    delete Comm_t::registry[i];
	  }
	}
      }
      YGG_THREAD_SAFE_BEGIN(comms) {
	Comm_t::registry.clear();
#if defined(ZMQINSTALLED)
	// This hangs if there are ZMQ sockets that didn't get cleaned up
	ZMQContext::destroy();
#endif
#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
	rapidjson::finalize_python("_ygg_cleanup");
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
      } YGG_THREAD_SAFE_END;
#ifndef YGG_TEST
      Comm_t::_ygg_finalized = 1;
#endif // YGG_TEST
      ygglog_debug << "_ygg_cleanup: Cleanup complete" << std::endl;
    }
  } YGG_THREAD_SAFE_END;
#ifndef YGG_TEST
#ifndef RAPIDJSON_YGGDRASIL_PYTHON
  if (YggdrasilLogger::_ygg_error_flag) {
    ygglog_debug << "_ygg_cleanup: Error code set" << std::endl;
    _exit(YggdrasilLogger::_ygg_error_flag);
  }
#endif // RAPIDJSON_YGGDRASIL_PYTHON
#endif // YGG_TEST
}

int Comm_t::_ygg_initialized = 0;
int Comm_t::_ygg_finalized = 0;

Comm_t::Comm_t(const std::string &nme, Address *addr,
	       DIRECTION dirn, const COMM_TYPE &t, int flgs) :
  type(t), name(nme), address(addr), direction(dirn), flags(flgs),
  maxMsgSize(COMM_BASE_MAX_MSG_SIZE), msgBufSize(0),
  index_in_register(-1), thread_id(-1), metadata(),
  timeout_recv(YGG_MAX_TIME), workers(), global_comm(nullptr) {

  _ygg_init();

  flags |= COMM_FLAG_VALID;
  if (direction == NONE)
    flags &= ~COMM_FLAG_VALID;
  
  thread_id = get_thread_id();
  char *allow_threading = getenv("YGG_THREADING");
  if (allow_threading)
    flags |= COMM_ALLOW_MULTIPLE_COMMS;
  char *model_name = std::getenv("YGG_MODEL_NAME");
  if (model_name) {
    std::string prefix(model_name);
    prefix += ":";
    if (name.rfind(prefix, 0) != 0) {
      prefix += name;
      name = prefix;
    }
  }

  get_global_scope_comm();
  
  Comm_t::register_comm(this);

  if (!(address && address->valid())) {
    if (address)
      delete address;
    address = addressFromEnv(name, direction);
    if ((flags & COMM_FLAG_INTERFACE) && (!address->valid())) {
      ygglog_error << "CommBase: " << name << " not registered as environment variable.\n" << std::endl;
      flags &= ~COMM_FLAG_VALID;
    }
  }
  ygglog_debug << "CommBase(" << name << "): Done" << std::endl;
}

Comm_t::~Comm_t() {
  YGG_THREAD_SAFE_BEGIN(comms) {
    if (index_in_register >= 0)
      Comm_t::registry[index_in_register] = NULL;
  } YGG_THREAD_SAFE_END;
  ygglog_debug << "~Comm_t: Started" << std::endl;
  if (address)
    delete address;
  ygglog_debug << "~Comm_t: Finished" << std::endl;
}

bool Comm_t::get_global_scope_comm() {
  COMM_TYPE global_type = getType();
  std::string global_name = name;
  DIRECTION global_direction = direction;
  bool is_server = false;
  if (global_type != SERVER_COMM && global_type != CLIENT_COMM &&
      !name.empty()) {
    char* server_var = NULL;
    if (direction == RECV) {
      server_var = std::getenv("YGG_SERVER_INPUT");
    } else if (direction == SEND) {
      server_var = std::getenv("YGG_SERVER_OUTPUT");
    }
    if (server_var && name == std::string(server_var)) {
      ygglog_debug << "CommBase: " << name <<
	" is piecemeal server (server_var = " << server_var <<
	")" << std::endl;
      is_server = true;
      global_type = SERVER_COMM;
      char* model_name = std::getenv("YGG_MODEL_NAME");
      if (global_direction == SEND) {
	global_direction = RECV;
	if (!model_name)
	  model_name = std::getenv("YGG_SERVER_INPUT");
      }
      assert(model_name);
      if (model_name)
	global_name.assign(model_name);
      global_scope_comm = 1;
    }
  }
  if (name.empty() || (!global_scope_comm) ||
      (flags & (COMM_FLAG_GLOBAL | COMM_FLAG_WORKER |
		COMM_FLAG_CLIENT_RESPONSE |
		COMM_FLAG_SERVER_RESPONSE)))
    return false;
  ygglog_debug << "CommBase: " << global_name << " (dir="
	       << global_direction << ") is a global communicator"
	       << std::endl;
  global_comm = Comm_t::find_registered_comm(global_name,
					     global_direction,
					     global_type);
  if (!global_comm) {
    ygglog_debug << "CommBase: Creating global comm \"" << global_name
		 << "\"" << std::endl;
    Address* global_address = new Address();
    if (address)
      global_address->address(address->address());
    global_comm = new_Comm_t(global_direction, global_type, global_name,
			     global_address, flags | COMM_FLAG_GLOBAL);
    ygglog_debug << "CommBase: Created global comm \"" << global_name
		 << "\"" << std::endl;
  } else {
    ygglog_debug << "CommBase: Found global comm \"" << global_name
		 << "\"" << std::endl;
  }
  if (!address)
    address = new Address();
  address->address(global_comm->address->address());
  flags = global_comm->flags & ~COMM_FLAG_GLOBAL;
  if (is_server)
    global_scope_comm = 0;
  return true;
}
  
void Comm_t::addSchema(const Metadata& s) {
  getMetadata().fromMetadata(s);
}
void Comm_t::addSchema(const rapidjson::Value& s, bool isMetadata) {
  getMetadata().fromSchema(s, isMetadata);
}
void Comm_t::addSchema(const std::string& schemaStr, bool isMetadata) {
  getMetadata().fromSchema(schemaStr, isMetadata);
}
void Comm_t::addFormat(const std::string& format_str, bool as_array) {
  getMetadata().fromFormat(format_str, as_array);
}
void Comm_t::copySchema(const Comm_t* other) {
  if (other->metadata.hasType())
    getMetadata().fromMetadata(other->metadata);
}

bool Comm_t::check_size(const size_t &len) const {
    // Make sure you aren't sending a message that is too big
    if (len > YGG_MSG_MAX) {
        ygglog_error << "comm_base_send(" << name <<
	  "): message too large for single packet (YGG_MSG_MAX=" <<
	  YGG_MSG_MAX << ", len=" << len << ")" << std::endl;
        return false;
    }
    return true;
}

Comm_t* communication::communicator::new_Comm_t(const DIRECTION dir, const COMM_TYPE type, const std::string &name, char* address, int flags) {
  Address* addr = nullptr;
  if (address)
    addr = new Address(address);
  else
    addr = new Address();
  return communication::communicator::new_Comm_t(dir, type, name, addr, flags);
}
Comm_t* communication::communicator::new_Comm_t(const DIRECTION dir, const COMM_TYPE type, const std::string &name, Address* addr, int flags) {
  flags |= COMM_FLAG_DELETE;
  if (flags & COMM_FLAG_ASYNC) {
    return new AsyncComm(name, addr, dir, flags, type);
  }
  switch(type) {
  case NULL_COMM:
    delete addr;
    break;
  case DEFAULT_COMM:
    return new COMM_BASE(name, addr, dir, flags);
  case IPC_COMM:
    return new IPCComm(name, addr, dir, flags);
  case ZMQ_COMM:
    return new ZMQComm(name, addr, dir, flags);
  case MPI_COMM:
    return new MPIComm(name, addr, dir, flags);
  case SERVER_COMM:
    return new ServerComm(name, addr, flags);
  case CLIENT_COMM:
    return new ClientComm(name, addr, flags);
  }
  return nullptr;
}
bool communication::communicator::is_commtype_installed(const COMM_TYPE type) {
  switch(type) {
  case NULL_COMM:
    break;
  case DEFAULT_COMM:
    return COMM_BASE::isInstalled();
  case IPC_COMM:
    return IPCComm::isInstalled();
  case ZMQ_COMM:
    return ZMQComm::isInstalled();
  case MPI_COMM:
    return MPIComm::isInstalled();
  case SERVER_COMM:
    return ServerComm::isInstalled();
  case CLIENT_COMM:
    return ClientComm::isInstalled();
  }
  return false;
}

Comm_t* Comm_t::create_worker_send(Header& head) {
  // Should never be called with global comm
  // if (global_comm)
  //   return global_comm->create_worker_send(head);
  assert(!global_comm);
  Comm_t* worker = workers.get(this, SEND);
  if (worker && worker->address) {
    head.SetMetaString("address", worker->address->address());
  }
  return worker;
}

Comm_t* Comm_t::create_worker_recv(Header& head) {
  // Should never be called with global comm
  // if (global_comm)
  //   return global_comm->create_worker_recv(head);
  assert(!global_comm);
  ygglog_debug << "CommBase(" << name << ")::create_worker_recv: begin" << std::endl;
  try {
    const char* address = head.GetMetaString("address");
    Address* adr = new Address(address);
    return workers.get(this, RECV, adr);
  } catch (...) {
    return nullptr;
  }
}

int Comm_t::send(const char *data, const size_t &len) {
  if (global_comm)
    return global_comm->send(data, len);
  if (direction != SEND && type != SERVER_COMM) {
    ygglog_debug << "CommBase(" << name << ")::send: Attempt to send though a communicator set up to receive" << std::endl;
    return -1;
  }
  ygglog_debug << "CommBase(" << name << ")::send: Sending " << len << " bytes to " << address->address() << std::endl;
  if (is_closed()) {
    ygglog_error << "CommBase(" << name << ")::send: Communicator closed." << std::endl;
    return -1;
  }
  Header head(data, len, this);
  if (head.flags & HEAD_FLAG_NO_HEAD) {
    ygglog_debug << "CommBase(" << name << ")::send: Sending data in single message. " << is_eof(data) << ", " << (flags & COMM_FLAGS_USED_SENT) << std::endl;
    int out = send_single(head);
    if (out >= 0)
      setFlags(head, SEND);
    return out;
  }
  if (!create_header_send(head)) {
    ygglog_error << "CommBase(" << name << ")::send: Failed to create header" << std::endl;
    return -1;
  }
  head.format();
  Comm_t* xmulti = NULL;
  if (head.flags & HEAD_FLAG_MULTIPART) {
    ygglog_debug << "CommBase(" << name << ")::send: Sending message in multiple parts" << std::endl;
    xmulti = create_worker_send(head);
    if (!xmulti) {
      ygglog_error << "CommBase(" << name << ")::send: Error creating worker" << std::endl;
      return -1;
    }
  }
  if (send_single(head) < 0) {
    ygglog_error << "CommBase(" << name << ")::send: Failed to send header." << std::endl;
    return -1;
  }
  if (!(head.flags & HEAD_FLAG_MULTIPART)) {
    ygglog_debug << "CommBase(" << name << ")::send: " << head.size_msg << " bytes completed" << std::endl;
    return head.size_msg;
  }
  while ((head.offset + head.size_msg) < head.size_curr) {
    if (xmulti->send_single(head) < 0) {
      ygglog_error << "CommBase(" << name << ")::send: send interupted at " << head.offset << " of " << head.size_curr << " bytes" << std::endl;
      return -1;
    }
    ygglog_debug << "CommBase(" << name << ")::send: " << head.offset << " of " << head.size_curr << " bytes sent to " << address->address() << std::endl;
  }
  ygglog_debug << "CommBase(" << name << ")::send: returns " << head.size_curr << std::endl;
  setFlags(head, SEND);
  return head.size_curr;
}
void Comm_t::set_timeout_recv(int new_timeout) {
  if (global_comm) {
    global_comm->set_timeout_recv(new_timeout);
    return;
  }
  timeout_recv = new_timeout;
}
int Comm_t::get_timeout_recv() {
  if (global_comm) {
    return global_comm->get_timeout_recv();
  }
  return timeout_recv;
}
int Comm_t::wait_for_recv(const int& tout) {
  if (global_comm)
    return global_comm->wait_for_recv(tout);
  clock_t start = clock();
  while (tout < 0 ||
	 (((double)(clock() - start))*1000000/CLOCKS_PER_SEC) < tout) {
    int nmsg = comm_nmsg(RECV);
    if (nmsg < 0) {
      ygglog_error << "CommBase(" << name << ")::wait_for_recv: Error in checking for messages" << std::endl;
      return -1;
    } else if (nmsg > 0) {
      return nmsg;
    }
    ygglog_debug << "CommBase(" << name << ")::wait_for_recv: No messages, sleep " << YGG_SLEEP_TIME << std::endl;
    std::this_thread::sleep_for(std::chrono::microseconds(YGG_SLEEP_TIME));
  }
  return comm_nmsg(RECV);
}
long Comm_t::recv(char*& data, const size_t &len,
		  bool allow_realloc) {
  if (global_comm)
    return global_comm->recv(data, len, allow_realloc);
  ygglog_debug << "CommBase(" << name << ")::recv: Receiving from " << address->address() << std::endl;
  if (direction != RECV && type != CLIENT_COMM) {
    ygglog_debug << "CommBase(" << name << ")::recv: Attempt to receive from communicator set up to send" << std::endl;
    return -1;
  }
  Header head(data, len, allow_realloc);
  long ret = -1;
  if (!allow_realloc) {
    char* tmp = NULL;
    size_t tmp_len = 0;
    long ret = recv(tmp, tmp_len, true);
    if (ret >= 0 || ret == -2) {
      if (ret >= 0) {
	tmp_len = static_cast<size_t>(ret);
	ret = copyData(data, len, tmp, tmp_len, false);
      }
      if (ret < 0)
	cache.emplace_back(tmp, tmp_len);
      if (tmp)
	free(tmp);
    }
    return ret;
  }
  if (!cache.empty()) {
    ret = copyData(data, len, cache.begin()->c_str(),
		   cache.begin()->size(), allow_realloc);
    if (ret >= 0)
      cache.erase(cache.begin());
    return ret;
  }
  while (true) {
    if (is_closed()) {
      ygglog_error << "CommBase(" << name << ")::recv: Communicator closed." << std::endl;
      return -1;
    }
    if (wait_for_recv(timeout_recv) <= 0) {
      ygglog_error << "CommBase(" << name << ")::recv: No messages waiting" << std::endl;
      return -1;
    }
    ret = recv_single(head);
    if (ret < 0) {
      ygglog_error << "CommBase(" << name << ")::recv: Failed to receive header" << std::endl;
      return ret;
    }
    if (!(head.flags & HEAD_FLAG_REPEAT))
      break;
    head.reset(HEAD_RESET_KEEP_BUFFER);
  }
  if (head.flags & HEAD_FLAG_EOF) {
    ygglog_debug << "CommBase(" << name << ")::recv: EOF received" << std::endl;
    setFlags(head, RECV);
    return -2;
  }
  Comm_t* xmulti = NULL;
  if (head.flags & HEAD_FLAG_MULTIPART) {
    ygglog_debug << "CommBase(" << name << ")::recv(char*& data, const size_t &len, bool allow_realloc): Message is multipart" << std::endl;
    xmulti = create_worker_recv(head);
    if (xmulti == NULL) {
      ygglog_error << "CommBase(" << name << ")::recv: Failed to create worker communicator" << std::endl;
      return -1;
    }
  }
  head.offset = head.size_curr;
  while (head.size_curr < head.size_data) {
    if (xmulti->wait_for_recv(timeout_recv) <= 0) {
      ygglog_error << "CommBase(" << name << ")::recv: No messages waiting in work comm" << std::endl;
      return -1;
    }
    ret = xmulti->recv_single(head);
    if (ret < 0) {
      ygglog_error << "CommBase(" << name << ")::recv: Receive interrupted at " << head.size_curr << " of " << head.size_data << " bytes." << std::endl;
      break;
    }
    ygglog_debug << "CommBase(" << name << ")::recv: " << head.size_curr << " of " << head.size_data << " bytes received." << std::endl;
  }
  if (xmulti)
    workers.remove_worker(xmulti);
  if (ret < 0) return ret;
  if (ret > 0) {
    head.finalize_recv();
    if (!head.hasType()) {
      ygglog_debug << "CommBase(" << name << ")::recv: No type information in message header" << std::endl;
    } else {
      update_datatype(head.schema[0], RECV);
    }
  }
  ygglog_debug << "CommBase(" << name << ")::recv: Received " << head.size_curr << " bytes from " << address->address() << std::endl;
  ret = head.size_data;
  setFlags(head, RECV);
  return ret;
}


long Comm_t::recv(const int nargs, ...) {
  size_t nargs_copy = (size_t)nargs;
  YGGCPP_BEGIN_VAR_ARGS(ap, nargs, nargs_copy, false);
  long ret = vRecv(ap);
  if (ret != -2)
    YGGCPP_END_VAR_ARGS(ap);
  return ret;
}
long Comm_t::recvRealloc(const int nargs, ...) {
  size_t nargs_copy = (size_t)nargs;
  YGGCPP_BEGIN_VAR_ARGS(ap, nargs, nargs_copy, true);
  long ret = vRecv(ap);
  if (ret != -2)
    YGGCPP_END_VAR_ARGS(ap);
  return ret;
}
int Comm_t::send(const int nargs, ...) {
    size_t nargs_copy = (size_t)nargs;
    YGGCPP_BEGIN_VAR_ARGS(ap, nargs, nargs_copy, false);
    int ret = vSend(ap);
    YGGCPP_END_VAR_ARGS(ap);
    return ret;
}

long Comm_t::call(const int nargs, ...) {
  size_t nargs_copy = (size_t)nargs;
  YGGCPP_BEGIN_VAR_ARGS(ap, nargs, nargs_copy, false);
  long ret = vCall(ap);
  YGGCPP_END_VAR_ARGS(ap);
  return ret;
}
long Comm_t::callRealloc(const int nargs, ...) {
  size_t nargs_copy = (size_t)nargs;
  YGGCPP_BEGIN_VAR_ARGS(ap, nargs, nargs_copy, true);
  long ret = vCall(ap);
  YGGCPP_END_VAR_ARGS(ap);
  return ret;
}

int Comm_t::sendVar(const std::string& data) {
  if (getMetadata(SEND).isGeneric())
    return sendVarAsGeneric(data);
  if (!checkType(data, SEND))
    return -1;
  return send(2, data.c_str(), data.size());
}
int Comm_t::sendVar(const rapidjson::Document& data) {
  getMetadata(SEND).setGeneric();
  if (!checkType(data, SEND))
    return -1;
  return send(1, &data);
}
int Comm_t::sendVar(const rapidjson::Ply& data) {
  if (!checkType(data, SEND))
    return -1;
  if (getMetadata(SEND).isGeneric())
    return sendVarAsGeneric(data);
  return send(1, &data);
}
int Comm_t::sendVar(const rapidjson::ObjWavefront& data) {
  if (!checkType(data, SEND))
    return -1;
  if (getMetadata(SEND).isGeneric())
    return sendVarAsGeneric(data);
  return send(1, &data);
}

Metadata& Comm_t::getMetadata(const DIRECTION dir) {
  if (global_comm)
    return global_comm->getMetadata(dir);
  return metadata;
}
int Comm_t::update_datatype(const rapidjson::Value& new_schema,
			    const DIRECTION dir) {
  Metadata& meta = getMetadata(dir);
  meta.fromSchema(new_schema);
  return 1;
}

int Comm_t::deserialize(const char* buf, rapidjson::VarArgList& ap) {
  Metadata& meta = getMetadata(RECV);
  if (!meta.hasType()) {
    ygglog_error << "CommBase(" << name << ")::deserialize: No datatype" << std::endl;
    return -1;
  }
  ygglog_debug << "CommBase(" << name << ")::deserialize: begin" << std::endl;
  int ret = meta.deserialize(buf, ap);
  ygglog_debug << "CommBase(" << name << ")::deserialize: returns " << ret << std::endl;
  return ret;
}

int Comm_t::serialize(char*& buf, size_t& buf_siz,
		      rapidjson::VarArgList& ap) {
  Metadata& meta = getMetadata(SEND);
  if (!meta.hasType()) {
    ygglog_error << "CommBase(" << name << ")::serialize: No datatype" << std::endl;
    return -1;
  }
  ygglog_debug << "CommBase(" << name << ")::serialize: begin" << std::endl;
  int ret = meta.serialize(&buf, &buf_siz, ap);
  ygglog_debug << "CommBase(" << name << ")::serialize: returns " << ret << std::endl;
  return ret;
}

long Comm_t::vRecv(rapidjson::VarArgList& ap) {
    ygglog_debug << "CommBase(" << name << ")::vRecv: begin" << std::endl;
    char* buf = NULL;
    size_t buf_siz = 0;
    long ret = recv(buf, buf_siz, true);
    if (ret < 0) {
        if (buf != NULL)
            free(buf);
	if (ret != -2)
	    ygglog_error << "CommBase(" << name << ")::vRecv: Error in recv" << std::endl;
        return ret;
    }
    ret = deserialize(buf, ap);
    free(buf);
    if (ret < 0) {
        ygglog_error << "CommBase(" << name << ")::vRecv: Error deserializing message" << std::endl;
        return ret;
    }
    ygglog_debug << "CommBase(" << name << ")::vRecv: returns " << ret << std::endl;
    return ret;
}
int Comm_t::vSend(rapidjson::VarArgList& ap) {
  ygglog_debug << "CommBase(" << name << ")::vSend: begin" << std::endl;
  // If type not set, but comm expecting generic, get the schema from the
  // provided generic argument
  Metadata& meta = getMetadata(SEND);
  if (meta.isGeneric() && !meta.hasType()) {
    rapidjson::Document tmp;
    Metadata tmp_meta;
    tmp_meta.fromType("any", true);
    rapidjson::VarArgList tmp_ap(ap);
    if (!tmp.GetVarArgs(*tmp_meta.schema, tmp_ap)) {
      ygglog_error << "CommBase(" << name << ")::vSend: Error getting generic argument." << std::endl;
      return -1;
    }
    rapidjson::SchemaEncoder encoder(true);
    rapidjson::Document new_schema;
    tmp.Accept(encoder);
    encoder.Accept(new_schema);
    new_schema.FinalizeFromStack();
    update_datatype(new_schema, SEND);
  }
  size_t nargs_orig = ap.get_nargs();
  char* buf = NULL;
  size_t buf_siz = 0;
  int ret = serialize(buf, buf_siz, ap);
  if (ret < 0) {
    ygglog_error << "CommBase(" << name << ")::vSend: serialization error" << std::endl;
    return ret;
  }
  ret = send(buf, ret);
  getMetadata(SEND).GetAllocator().Free(buf);
  if (ret >= 0)
    ret = (int)(nargs_orig - ap.get_nargs());
  ygglog_debug << "CommBase(" << name << ")::vSend: returns " << ret << std::endl;
  return ret;
}
long Comm_t::callVar(const rapidjson::Document& sendData,
		     rapidjson::Document& recvData) {
  if (!(flags & COMM_FLAG_CLIENT)) {
    ygglog_error << "CommBase(" << name << ")::callVar: Communicator is not a client." << std::endl;
    return -1;
  }
  if (sendVar(sendData) < 0) {
    ygglog_error << "CommBase(" << name << ")::callVar: Error in send." << std::endl;
    return -1;
  }
  return recvVar(recvData);
}
long Comm_t::vCall(rapidjson::VarArgList& ap) {
  if (!(flags & COMM_FLAG_CLIENT)) {
    ygglog_error << "CommBase(" << name << ")::vCall: Communicator is not a client." << std::endl;
    return -1;
  }
  size_t send_nargs = 0;
  rapidjson::Document tmp;
  Metadata& meta_send = getMetadata(SEND);
  if (meta_send.hasType()) {
    send_nargs = tmp.CountVarArgs(*meta_send.schema, false);
  }
  if (ap.get_nargs() < send_nargs) {
    ygglog_error << "CommBase(" << name << ")::vCall: Not enough arguments for send" << std::endl;
    return -1;
  }
  size_t recv_nargs = ap.get_nargs() - send_nargs;
  ap.set_nargs(send_nargs);
  int sret = vSend(ap);
  if (sret < 0) {
    ygglog_error << "CommBase(" << name << ")::vCall: Error in vSend"
		 << std::endl;
    return -1;
  }
  ygglog_debug << "CommBase(" << name << ")::vCall: Used " << sret
	       << " arguments in send" << std::endl;
  ap.set_nargs(recv_nargs);
  ygglog_debug << "CommBase(" << name << ")::vCall: " << ap.get_nargs()
	       << " arguments remaining for receive" << std::endl;
  long rret = vRecv(ap);
  if (rret >= 0) {
    ygglog_debug << "CommBase(" << name << ")::vCall: " << ap.get_nargs()
		 << " arguments after receive" << std::endl;
  }
  YGGCPP_END_VAR_ARGS(ap);
  return rret;
  
}


std::vector<Comm_t*> Comm_t::registry;

void Comm_t::register_comm(Comm_t* x) {
  YGG_THREAD_SAFE_BEGIN(comms) {
    x->index_in_register = Comm_t::registry.size();
    Comm_t::registry.push_back(x);
  } YGG_THREAD_SAFE_END;
}

Comm_t* Comm_t::find_registered_comm(const std::string& name,
				     const DIRECTION dir,
				     const COMM_TYPE type) {
  Comm_t* out = NULL;
  assert(!name.empty());
  YGG_THREAD_SAFE_BEGIN(comms) {
    if (global_scope_comm) {
      for (std::vector<Comm_t*>::iterator it = Comm_t::registry.begin();
	   it != Comm_t::registry.end(); it++) {
	if (*it && (*it)->global() &&
	    ((*it)->name == name) &&
	    ((*it)->direction == dir) &&
	    ((*it)->type == type)) {
	  out = *it;
	  break;
	}
      }
    }
  } YGG_THREAD_SAFE_END;
  return out;
}
