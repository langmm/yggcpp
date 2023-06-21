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

void Comm_t::_ygg_init() {
#ifdef _OPENMP
#pragma omp critical (init)
  {
#endif
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
#ifdef _OPENMP
  }
#endif
}
void Comm_t::_ygg_cleanup() {
#ifdef _OPENMP
#pragma omp critical (clean)
  {
#endif
    if (!Comm_t::_ygg_finalized) {
      ygglog_debug << "_ygg_cleanup: Begin cleanup" << std::endl;
      for (size_t i = 0; i < Comm_t::registry.size(); i++) {
	if (Comm_t::registry[i]) {
	  delete Comm_t::registry[i];
	}
      }
#ifdef _OPENMP
#pragma omp critical (comms)
      {
#endif
	Comm_t::registry.clear();
#if defined(ZMQINSTALLED)
	ZMQContext::destroy();
#endif
#ifndef YGGDRASIL_DISABLE_PYTHON_C_API
	rapidjson::finalize_python("_ygg_cleanup");
#endif // YGGDRASIL_DISABLE_PYTHON_C_API
#ifdef _OPENMP
      }
#endif
#ifndef YGG_TEST
      Comm_t::_ygg_finalized = 1;
#endif // YGG_TEST
      ygglog_debug << "_ygg_cleanup: Cleanup complete" << std::endl;
    }
#ifdef _OPENMP
  }
#endif
#ifndef YGG_TEST
  if (YggLogger::_ygg_error_flag) {
    _exit(YggLogger::_ygg_error_flag);
  }
#endif // YGG_TEST
}

int Comm_t::_ygg_initialized = 0;
int Comm_t::_ygg_finalized = 0;

Comm_t::Comm_t(const std::string &nme, utils::Address *addr,
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
  
  get_global_scope_comm();
  
  Comm_t::register_comm(this);
  
  if (!(address && address->valid())) {
    if (address)
      delete address;
    address = addressFromEnv(name, direction);
    if ((flags & COMM_FLAG_INTERFACE) &&
	(!address->valid()) &&
	(!(flags & (COMM_FLAG_CLIENT | COMM_FLAG_SERVER)))) {
      ygglog_error << "CommBase: " << name << " not registered as environment variable.\n" << std::endl;
      flags &= ~COMM_FLAG_VALID;
    }
  }
  ygglog_debug << "CommBase(" << name << "): Done" << std::endl;
}

Comm_t::~Comm_t() {
#ifdef _OPENMP
#pragma omp critical (comms)
  {
#endif
    if (index_in_register >= 0)
      Comm_t::registry[index_in_register] = NULL;
#ifdef _OPENMP
  }
#endif
    ygglog_debug << "~Comm_t: Started" << std::endl;
    if (address)
        delete address;
    ygglog_debug << "~Comm_t: Finished" << std::endl;
}

bool Comm_t::get_global_scope_comm() {
  if (name.empty() || (!global_scope_comm) ||
      (flags & (COMM_FLAG_GLOBAL | COMM_FLAG_WORKER |
		COMM_FLAG_CLIENT_RESPONSE |
		COMM_FLAG_SERVER_RESPONSE)))
    return false;
  ygglog_debug << "CommBase: " << name << " (dir=" << direction << ") is a global communicator" << std::endl;
  global_comm = Comm_t::find_registered_comm(name, direction, type);
  if (!global_comm) {
    global_comm = new_Comm_t(direction, type, name, address,
			     flags | COMM_FLAG_GLOBAL);
  }
  if (!address)
    address = new utils::Address();
  address->address(global_comm->address->address());
  flags = global_comm->flags & ~COMM_FLAG_GLOBAL;
  return true;
}
  
void Comm_t::addSchema(const Metadata& s) {
  get_metadata().fromMetadata(s);
}
void Comm_t::addSchema(const rapidjson::Value& s, bool isMetadata) {
  get_metadata().fromSchema(s, isMetadata);
}
void Comm_t::addSchema(const std::string schemaStr, bool isMetadata) {
  get_metadata().fromSchema(schemaStr, isMetadata);
}
void Comm_t::addFormat(const std::string format_str, bool as_array) {
  get_metadata().fromFormat(format_str, as_array);
}
void Comm_t::copySchema(const Comm_t* other) {
  if (other->metadata.hasType())
    get_metadata().fromMetadata(other->metadata);
}

bool Comm_t::check_size(const size_t &len) const {
    // Make sure you aren't sending a message that is too big
    if (len > YGG_MSG_MAX) {
        ygglog_error << "comm_base_send(" << name << "): message too large for single packet (YGG_MSG_MAX="
                     << YGG_MSG_MAX << ", len=" << len << ")" << std::endl;
        return false;
    }
    return true;
}

Comm_t* communication::communicator::new_Comm_t(const DIRECTION dir, const COMM_TYPE type, const std::string &name, char* address, int flags) {
  Address* addr = (address) ? nullptr : new Address(address);
  return communication::communicator::new_Comm_t(dir, type, name, addr, flags);
}
Comm_t* communication::communicator::new_Comm_t(const DIRECTION dir, const COMM_TYPE type, const std::string &name, Address* addr, int flags) {
  switch(type) {
  case NULL_COMM:
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

bool Comm_t::create_header_send(Header& header, const char* data,
				const size_t &len) {
  if (global_comm)
    return global_comm->create_header_send(header, data, len);
  header.for_send(&get_metadata(SEND), data, len);
  return true;
}

bool Comm_t::create_header_recv(Header& header, char*& data,
				const size_t &len,
				size_t msg_len, int allow_realloc,
				int temp) {
  if (global_comm)
    return global_comm->create_header_recv(header, data, len, msg_len,
					   allow_realloc, temp);
  header.for_recv(&data, len, msg_len, allow_realloc, temp);
  return true;
}

Comm_t* Comm_t::create_worker_send(Header& head) {
  if (global_comm)
    return global_comm->create_worker_send(head);
  Comm_t* worker = workers.get(this, SEND);
  if (worker && worker->address) {
    head.SetMetaString("address", worker->address->address());
  }
  return worker;
}

Comm_t* Comm_t::create_worker_recv(Header& head) {
  if (global_comm)
    return global_comm->create_worker_recv(head);
  ygglog_debug << "CommBase(" << name << ")::create_worker_recv: begin" << std::endl;
  const char* address = head.GetMetaString("address");
  utils::Address* adr = new utils::Address(address);
  return workers.get(this, RECV, adr);
}

int Comm_t::send(const char *data, const size_t &len) {
  if (global_comm)
    return global_comm->send(data, len);
  ygglog_debug << "CommBase(" << name << ")::send: Sending " << len << " bytes to " << address->address() << std::endl;
  if (is_closed()) {
    ygglog_error << "CommBase(" << name << ")::send: Communicator closed." << std::endl;
    return -1;
  }
  size_t size_max = maxMsgSize - msgBufSize;
  Header head;
  head.setMessageFlags(data, len);
  int no_type = ((head.flags & HEAD_FLAG_EOF) ||
		 (flags & COMM_FLAGS_USED));
  if ((size_max == 0 || len <= size_max) &&
      (!(flags & COMM_ALWAYS_SEND_HEADER)) && no_type) {
    ygglog_debug << "CommBase(" << name << ")::send: Sending data in single message. " << is_eof(data) << ", " << (flags & COMM_FLAGS_USED) << std::endl;
    int out = send_single(data, len, head);
    if (out >= 0)
      setFlags(head, SEND);
    return out;
  }
  if (!create_header_send(head, data, len)) {
    ygglog_error << "CommBase(" << name << ")::send: Failed to create header" << std::endl;
    return -1;
  }
  head.format(data, len, size_max);
  Comm_t* xmulti = NULL;
  if (head.flags & HEAD_FLAG_MULTIPART) {
    ygglog_debug << "CommBase(" << name << ")::send: Sending message in multiple parts" << std::endl;
    xmulti = create_worker_send(head);
    if (!xmulti) {
      ygglog_error << "CommBase(" << name << ")::send: Error creating worker" << std::endl;
      return -1;
    }
    try {
      head.format(data, len, size_max, no_type);
    } catch (std::exception& err) {
      throw err;
    }
  }
  size_t prev = 0, msgsiz = head.size_curr;
  if (head.size_curr > size_max) {
    msgsiz = size_max;
    prev += size_max;
  }
  if (send_single(head.data[0], msgsiz, head) < 0) {
    ygglog_error << "CommBase(" << name << ")::send: Failed to send header." << std::endl;
    return -1;
  }
  if (!(head.flags & HEAD_FLAG_MULTIPART)) {
    ygglog_debug << "CommBase(" << name << ")::send: " << msgsiz << " bytes completed" << std::endl;
    return msgsiz;
  }
  size_t size_max_multi = xmulti->maxMsgSize - xmulti->msgBufSize;
  while (prev < head.size_curr) {
    msgsiz = head.size_curr - prev;
    if (msgsiz > size_max_multi)
      msgsiz = size_max_multi;
    if (xmulti->send_single(head.data[0] + prev, msgsiz, head) < 0) {
      ygglog_error << "CommBase(" << name << ")::send: send interupted at " << prev << " of " << head.size_curr << " bytes" << std::endl;
      return -1;
    }
    prev += msgsiz;
    ygglog_debug << "CommBase(" << name << ")::send: " << prev << " of " << head.size_curr << " bytes sent to " << address->address() << std::endl;
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
int Comm_t::wait_for_recv(const int tout) {
  if (global_comm)
    return global_comm->wait_for_recv(tout);
  clock_t start = clock();
  while (tout < 0 ||
	 (((double)(clock() - start))*1000000/CLOCKS_PER_SEC) < tout) {
    int nmsg = comm_nmsg();
    if (nmsg < 0) {
      ygglog_error << "CommBase(" << name << ")::wait_for_recv: Error in checking for messages" << std::endl;
      return -1;
    } else if (nmsg > 0) {
      return nmsg;
    }
    ygglog_error << "CommBase(" << name << ")::wait_for_recv: No messages, sleep " << YGG_SLEEP_TIME << std::endl;
    usleep(YGG_SLEEP_TIME);
  }
  return 0;
}
long Comm_t::recv(char*& data, const size_t &len,
		  bool allow_realloc) {
  if (global_comm) {
    return global_comm->recv(data, len, allow_realloc);
  }
  ygglog_debug << "CommBase(" << name << ")::recv: Receiving from " << address->address() << std::endl;
  Header head;
  long ret = -1;
  if (!allow_realloc) {
    char* tmp = NULL;
    size_t tmp_len = 0;
    long ret = recv(tmp, tmp_len, true);
    if (ret >= 0 || ret == -2) {
      if (ret >= 0)
	ret = copyData(data, len, tmp, ret, false);
      if (tmp)
	free(tmp);
    }
    return ret;
  }
  while (true) {
    if (is_closed()) {
      ygglog_error << "CommBase(" << name << ")::recv: Communicator closed." << std::endl;
      return -1;
    }
    if (wait_for_recv(timeout_recv) < 0) {
      ygglog_error << "CommBase(" << name << ")::recv: No messages waiting" << std::endl;
      return -1;
    }
    ret = recv_single(data, len, allow_realloc);
    if (ret < 0) {
      ygglog_error << "CommBase(" << name << ")::recv: Failed to receive header" << std::endl;
      return ret;
    }
    if (!create_header_recv(head, data, len, ret, allow_realloc, 0)) {
      ygglog_error << "CommBase(" << name << ")::recv: Failed to create header" << std::endl;
      return -1;
    }
    if (!(head.flags & HEAD_FLAG_REPEAT))
      break;
    head.reset();
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
  char *pos = data + head.size_curr;
  size_t msgsiz = 0;
  while (head.size_curr < head.size_data) {
    msgsiz = head.size_data - head.size_curr + 1;
    if (xmulti->wait_for_recv(timeout_recv) < 0) {
      ygglog_error << "CommBase(" << name << ")::recv: No messages waiting in work comm" << std::endl;
      return -1;
    }
    ret = xmulti->recv_single(pos, msgsiz, false);
    if (ret < 0) {
      ygglog_error << "CommBase(" << name << ")::recv: Receive interrupted at " << head.size_curr << " of " << head.size_data << " bytes." << std::endl;
      break;
    }
    head.size_curr += ret;
    pos += ret;
    ygglog_debug << "CommBase(" << name << ")::recv: " << head.size_curr << " of " << head.size_data << " bytes received." << std::endl;
  }
  if (xmulti)
    workers.remove_worker(xmulti);
  if (ret > 0) {
    head.finalize_recv();
    if (!head.hasType()) {
      ygglog_debug << "CommBase(" << name << ")::recv: No type information in message header" << std::endl;
    } else if (!update_datatype(head.schema[0], RECV)) {
      ygglog_error << "CommBase(" << name << ")::recv: Error updating datatype." << std::endl;
      return -1;
    }
  } else {
    std::cerr << "ret iz zero? " << ret << std::endl;
  }
  if (ret >= 0) {
    ygglog_debug << "CommBase(" << name << ")::recv: Received " << head.size_curr << " bytes from " << address->address() << std::endl;
    ret = head.size_data;
    setFlags(head, RECV);
  }
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
  if (!checkType(data, SEND))
    return -1;
  return send(2, data.c_str(), data.size());
}
int Comm_t::sendVar(const rapidjson::Document& data) {
  if (!checkType(data, SEND))
    return -1;
  return send(1, &data);
}
int Comm_t::sendVar(const rapidjson::Ply& data) {
  if (!checkType(data, SEND))
    return -1;
  return send(1, &data);
}
int Comm_t::sendVar(const rapidjson::ObjWavefront& data) {
  if (!checkType(data, SEND))
    return -1;
  return send(1, &data);
}

Metadata& Comm_t::get_metadata(const DIRECTION dir) {
  if (global_comm)
    return global_comm->get_metadata(dir);
  return metadata;
}
int Comm_t::update_datatype(const rapidjson::Value& new_schema,
			    const DIRECTION dir) {
  Metadata& meta = get_metadata(dir);
  meta.fromSchema(new_schema);
  return 1;
}

int Comm_t::deserialize(const char* buf, rapidjson::VarArgList& ap) {
  Metadata& meta = get_metadata(RECV);
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
  Metadata& meta = get_metadata(SEND);
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
  Metadata& meta = get_metadata(SEND);
  if (meta.isGeneric() && !meta.hasType()) {
    rapidjson::Document tmp;
    if (!tmp.GetVarArgs(*meta.schema, ap)) {
      ygglog_error << "CommBase(" << name << ")::vSend: Error getting generic argument." << std::endl;
      return -1;
    }
    rapidjson::SchemaEncoder encoder(true);
    rapidjson::Document new_schema;
    if (!tmp.Accept(encoder)) {
      ygglog_error << "CommBase(" << name << ")::vSend: Error encoding schema."
		   << std::endl;
      return -1;
    }
    if (!encoder.Accept(new_schema)) {
      ygglog_error << "CommBase(" << name << ")::vSend: Error getting encoded schema." << std::endl;
      return -1;
    }
    new_schema.FinalizeFromStack();
    if (!update_datatype(new_schema, SEND)) {
      ygglog_error << "CommBase(" << name << ")::vSend: Error updating dtype from generic" << std::endl;
      return -1;
    }
  }
  size_t nargs_orig = ap.get_nargs();
  char* buf = NULL;
  size_t buf_siz = 0;
  int ret = serialize(buf, buf_siz, ap);
  if (ret < 0) {
    ygglog_error << "CommBase(" << name << ")::vSend: serialization error" << std::endl;
    if (buf)
      free(buf);
    return ret;
  }
  ret = send(buf, ret);
  free(buf);
  if (ret >= 0)
    ret = (int)(nargs_orig - ap.get_nargs());
  ygglog_debug << "CommBase(" << name << ")::vSend: returns " << ret << std::endl;
  return ret;
}
long Comm_t::vCall(rapidjson::VarArgList& ap) {
  if (!(flags & COMM_FLAG_CLIENT)) {
    ygglog_error << "CommBase(" << name << ")::vCall: Communicator is not a client." << std::endl;
    return -1;
  }
  size_t send_nargs = 0;
  rapidjson::Document tmp;
  Metadata& meta_send = get_metadata(SEND);
  if (!meta_send.hasType()) {
    send_nargs = 0;
  } else {
    send_nargs = tmp.CountVarArgs(*meta_send.schema, false);
  }
  rapidjson::VarArgList op(ap);
  size_t recv_nargs = ap.get_nargs() - send_nargs;
  ap.set_nargs(send_nargs);
  int sret = vSend(ap);
  if (sret < 0) {
    ygglog_error << "CommBase(" << name << ")::vCall: Error in vSend" << std::endl;
    return -1;
  }
  ygglog_debug << "CommBase(" << name << ")::vCall: Used " << sret << " arguments in send" << std::endl;
  if (!tmp.SkipVarArgs(*(meta_send.schema), op, false)) {
    ygglog_error << "CommBase(" << name << ")::vCall: Error skipping arguments" << std::endl;
    return -1;
  }
  ygglog_debug << "CommBase(" << name << ")::vCall: " << op.get_nargs() << " arguments remaining for receive" << std::endl;
  if (op.get_nargs() != recv_nargs) {
    ygglog_error << "CommBase(" << name << ")::vCall: Number of arguments after skip (" << op.get_nargs() << ") doesn't match the number expected (" << recv_nargs << std::endl;
    return -1;
  }
  long rret = vRecv(op);
  if (rret < 0) {
    ygglog_error << "CommBase(" << name << ")::vCall: Error in vRecv" << std::endl;
  }
  ygglog_debug << "CommBase(" << name << ")::vCall: " << op.get_nargs() << " arguments after receive" << std::endl;
  YGGCPP_END_VAR_ARGS(op);
  return rret;
  
}


std::vector<Comm_t*> Comm_t::registry;

void Comm_t::register_comm(Comm_t* x) {
  // TODO: init python, numpy, zmq
#ifdef _OPENMP
#pragma omp critical (comms)
  {
#endif
  x->index_in_register = Comm_t::registry.size();
  Comm_t::registry.push_back(x);
#ifdef _OPENMP
  }
#endif
}

Comm_t* Comm_t::find_registered_comm(const std::string& name,
				     const DIRECTION dir,
				     const COMM_TYPE type) {
  Comm_t* out = NULL;
  if (name.empty())
    return out;
#ifdef _OPENMP
#pragma omp critical (comms)
  {
#endif
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
#ifdef _OPENMP
  }
#endif
  return out;
}
