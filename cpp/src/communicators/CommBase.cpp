#include "communicators/comms.hpp"
#include "utils/rapidjson_wrapper.hpp"

using namespace YggInterface::communicator;
using namespace YggInterface::utils;

Comm_t::Comm_t(const std::string &nme, const Address &addr,
	       const DIRECTION dirn, const COMM_TYPE &t, int flgs) :
  ctx(global_context), type(t), name(nme), address(addr),
  direction(dirn), flags(flgs),
  maxMsgSize(COMM_BASE_MAX_MSG_SIZE), msgBufSize(0),
  index_in_register(-1), thread_id(), metadata(),
  timeout_recv(YGG_MAX_TIME), workers(), global_comm(nullptr),
  language(NO_LANGUAGE), model(), partner_model() {
  _before_open();
}

Comm_t::Comm_t(const std::string &nme,
               const DIRECTION dirn, const COMM_TYPE &t, int flgs) :
  Comm_t(nme, utils::Address(), dirn, t, flgs) {}

Comm_t::~Comm_t() {
  log_debug() << "~Comm_t: Unregistering comm (idx = " <<
    index_in_register << ")" << std::endl;
  YGG_THREAD_SAFE_BEGIN(comms) {
    if (index_in_register >= 0)
      ctx->registry_[index_in_register] = NULL;
  } YGG_THREAD_SAFE_END;
  log_debug() << "~Comm_t: Started" << std::endl;
  if (flags & COMM_FLAG_SET_OPP_ENV)
    unsetOppEnv();
  log_debug() << "~Comm_t: Finished" << std::endl;
}
void Comm_t::_before_open() {

  flags |= COMM_FLAG_VALID;
  if (direction == NONE)
    flags &= ~COMM_FLAG_VALID;

  thread_id = get_thread_id();
  char *allow_threading = getenv("YGG_THREADING");
  if (allow_threading)
    flags |= COMM_FLAG_ALLOW_MULTIPLE_COMMS;
  char *model_name = std::getenv("YGG_MODEL_NAME");
  if (model_name) {
    model.assign(model_name);
    std::string prefix(model_name);
    prefix += ":";
    if (name.rfind(prefix, 0) != 0) {
      prefix += name;
      name = prefix;
    }
  }
  setLanguage();
  
  create_global_scope_comm();
  
  ctx->register_comm(this);
  
  if (!address.valid()) {
    address = addressFromEnv(name, direction);
    if ((flags & COMM_FLAG_INTERFACE) && (!address.valid()) &&
	(!(flags & COMM_FLAG_SET_OPP_ENV))) {
      log_error() << "before_open: " << name << " not registered as environment variable.\n" << std::endl;
      flags &= ~COMM_FLAG_VALID;
    }
  }
  log_debug() << "before_open: Done" << std::endl;
}
void Comm_t::_after_open() {
  if (flags & COMM_FLAG_SET_OPP_ENV)
    setOppEnv();
}

bool Comm_t::operator==(const Comm_t& rhs) const {
  std::cerr << "Comm_t==(" <<
    (getType() == rhs.getType()) << ", " <<
    (getName() == rhs.getName()) << ", " <<
    (getFlags() == rhs.getFlags()) << ", " <<
    (getAddress() == rhs.getAddress()) << ", " <<
    (getDirection() == rhs.getDirection()) << ", " <<
    (get_timeout_recv() == rhs.get_timeout_recv()) << ", " <<
    (getLanguage() == rhs.getLanguage()) << ")" << std::endl;
  return (getType() == rhs.getType() &&
	  getName() == rhs.getName() &&
	  getFlags() == rhs.getFlags() &&
	  getAddress() == rhs.getAddress() &&
	  getDirection() == rhs.getDirection() &&
	  getMetadata() == rhs.getMetadata() &&
	  get_timeout_recv() == rhs.get_timeout_recv() &&
	  getLanguage() == rhs.getLanguage());
}

bool Comm_t::_coerce_to_dict(const rapidjson::Document& src,
			     rapidjson::Document& dst,
			     const DIRECTION dir,
			     std::vector<std::string> key_order,
			     size_t dim) const {
  switch (src.GetType()) {
  case (rapidjson::kObjectType):
    dst.CopyFrom(src, dst.GetAllocator(), true);
    break;
  case (rapidjson::kArrayType):
  case (rapidjson::kStringType):
    if (src.IsArray() || src.IsNDArray()) {
      size_t src_size = 0;
      if (src.IsArray())
	src_size = static_cast<size_t>(src.Size());
      else if (src.Is1DArray())
	src_size = static_cast<size_t>(src.GetNElements());
      else {
	const rapidjson::Value& shape = src.GetShape();
	if (dim >= static_cast<size_t>(shape.Size())) {
	  log_error() << "coerce_to_dict: dim " << dim << " exceeds the " <<
	    "number of dimensions (" << shape.Size() << ") in the " <<
	    "array" << std::endl;
	  return false;
	}
	src_size = static_cast<size_t>(shape[static_cast<rapidjson::SizeType>(dim)].GetUint64());
      }
      if (key_order.empty()) {
	if (!this->getMetadata(dir).get_field_names(key_order)) {
	  return false;
	}
	if (key_order.empty()) {
	  for (int i = 0; i < static_cast<int>(src_size); i++)
	    key_order.push_back("f" + std::to_string(i));
	}
      }
      if (key_order.size() != src_size) {
	log_error() << "coerce_to_dict: " << key_order.size() <<
	  " keys provided, but there are " << src.Size() <<
	  " elements in the message" << std::endl;
	return false;
      }
      dst.SetObject();
      for (size_t i = 0; i < src_size; i++) {
	rapidjson::Value val;
	if (src.IsArray()) {
	  val.CopyFrom(src[static_cast<SizeType>(i)],
		       dst.GetAllocator(), true);
	} else if (src.Is1DArray()) {
	  if (!src.GetElement(static_cast<SizeType>(i), val,
			       dst.GetAllocator())) {
	    log_error() << "coerce_to_dict: Could not get element " << i <<
	      " from 1D array" << std::endl; // GCOVR_EXCL_START
	    return false; // GCOVR_EXCL_STOP
	  }
	} else {
	  if (!src.GetSubArray(static_cast<SizeType>(i),
				static_cast<SizeType>(dim), val,
				dst.GetAllocator())) {
	    log_error() << "coerce_to_dict: Could not get subarray " << i <<
	      " from ND array along dimension " << dim << std::endl; // GCOVR_EXCL_START
	    return false; // GCOVR_EXCL_STOP
	  }
	}
	dst.AddMember(rapidjson::Value(key_order[i].c_str(),
				       static_cast<SizeType>(key_order[i].size()),
				       dst.GetAllocator()).Move(),
		      val, dst.GetAllocator());
      }
      break;
    }
    RAPIDJSON_DELIBERATE_FALLTHROUGH;
  default:
    rapidjson::Value val;
    val.CopyFrom(src, dst.GetAllocator(), true);
    dst.SetObject();
    dst.AddMember("f0", val, dst.GetAllocator());
  }
  return true;
}

bool Comm_t::_coerce_to_array(const rapidjson::Document& src,
			      rapidjson::Document& dst,
			      const DIRECTION dir,
			      std::vector<std::string> key_order,
			      size_t dim) const {
  switch (src.GetType()) {
  case (rapidjson::kObjectType):
    if (!this->getMetadata(dir).get_field_names(key_order)) {
      return false;
    }
    if (key_order.empty()) {
      for (typename rapidjson::Value::ConstMemberIterator it = src.MemberBegin();
	   it != src.MemberEnd(); it++)
	key_order.push_back(it->name.GetString());
    }
    dst.SetArray();
    dst.Reserve(static_cast<rapidjson::SizeType>(key_order.size()),
		dst.GetAllocator());
    for (size_t i = 0; i < key_order.size(); i++) {
      typename rapidjson::Value::ConstMemberIterator it = src.FindMember(key_order[i]);
      if (it == src.MemberEnd()) {
	log_error() << "coerce_to_array: key '" << key_order[i] << "'" <<
	  " is not present" << std::endl;
	return false;
      }
      rapidjson::Value item(it->value, dst.GetAllocator(), true);
      if (item.IsNDArray()) {
	rapidjson::Value field_name(key_order[i].c_str(),
				    static_cast<rapidjson::SizeType>(key_order[i].size()),
				    dst.GetAllocator());
	item.AddSchemaMember(rapidjson::Document::GetTitleString(), field_name);
      }
      dst.PushBack(item, dst.GetAllocator());
    }
    break;
  case (rapidjson::kArrayType):
  case (rapidjson::kStringType):
    if (src.IsArray() || src.IsNDArray()) {
      size_t src_size = 0;
      if (src.IsArray()) {
	src_size = static_cast<size_t>(src.Size());
      } else if (src.Is1DArray()) {
	if (key_order.size() <= 1) {
	  src_size = 1;
	} else {
	  src_size = static_cast<size_t>(src.GetNElements());
	}
      } else {
	const rapidjson::Value& shape = src.GetShape();
	if (dim >= static_cast<size_t>(shape.Size())) {
	  log_error() << "coerce_to_array: dim " << dim << " exceeds the " <<
	    "number of dimensions (" << shape.Size() << ") in the " <<
	    "array" << std::endl;
	  return false;
	}
	src_size = static_cast<size_t>(shape[static_cast<rapidjson::SizeType>(dim)].GetUint64());
      }
      if (key_order.empty()) {
	if (!this->getMetadata(dir).get_field_names(key_order)) {
	  return false;
	}
	if (key_order.empty()) {
	  for (int i = 0; i < static_cast<int>(src_size); i++)
	    key_order.push_back("f" + std::to_string(i));
	}
      }
      if (key_order.size() != src_size) {
	log_error() << "coerce_to_array: " << key_order.size() <<
	  " keys provided, but there are " << src.Size() <<
	  " elements in the message" << std::endl;
	return false;
      }
      dst.SetArray();
      dst.Reserve(static_cast<rapidjson::SizeType>(src_size),
		  dst.GetAllocator());
      for (size_t i = 0; i < src_size; i++) {
	rapidjson::Value val;
	if (src.IsArray()) {
	  val.CopyFrom(src[static_cast<SizeType>(i)],
		       dst.GetAllocator(), true);
	} else if (src.Is1DArray()) {
	  if (src_size == 1) {
	    val.CopyFrom(src, dst.GetAllocator(), true);
	  } else if (!src.GetElement(static_cast<SizeType>(i), val,
				     dst.GetAllocator())) {
	    log_error() << "coerce_to_array: Could not get element " << i <<
	      " from 1D array" << std::endl; // GCOVR_EXCL_START
	    return false; // GCOVR_EXCL_STOP
	  }
	} else {
	  if (!src.GetSubArray(static_cast<SizeType>(i),
				static_cast<SizeType>(dim), val,
				dst.GetAllocator())) {
	    log_error() << "coerce_to_array: Could not get subarray " << i <<
	      " from ND array along dimension " << dim << std::endl; // GCOVR_EXCL_START
	    return false; // GCOVR_EXCL_STOP
	  }
	}
	if (val.IsNDArray()) {
	  rapidjson::Value field_name(key_order[i].c_str(),
				      static_cast<SizeType>(key_order[i].size()),
				      dst.GetAllocator());
	  val.AddSchemaMember(rapidjson::Document::GetTitleString(), field_name);
	}
	dst.PushBack(val, dst.GetAllocator());
      }
      break;
    }
    RAPIDJSON_DELIBERATE_FALLTHROUGH;
  default:
    rapidjson::Value val;
    val.CopyFrom(src, dst.GetAllocator(), true);
    dst.SetArray();
    dst.PushBack(val, dst.GetAllocator());
  }
  return true;
}

std::vector<std::string> Comm_t::get_status_message(
  unsigned int nindent,
  const std::vector<std::string>& extra_lines_before,
  const std::vector<std::string>& extra_lines_after) const {
  if (global_comm)
    return global_comm->get_status_message(nindent, extra_lines_before,
					   extra_lines_after);
  std::vector<std::string> out;
  std::string prefix = "";
  for (unsigned int i = 0; i < nindent; i++)
    prefix += "    ";
  out.push_back(prefix + name);
  prefix += "    ";
  for (std::vector<std::string>::const_iterator it = extra_lines_before.begin();
       it != extra_lines_before.end(); it++)
    out.push_back(prefix + *it);
  out.push_back(prefix + "commtype  = " + COMM_TYPE_map.find(getType())->second);
  out.push_back(prefix + "address   = " + getAddress());
  out.push_back(prefix + "direction = " + DIRECTION_map.find(getDirection())->second);
  out.push_back(prefix + "language  = " + LANGUAGE_map.find(getLanguage())->second);
  out.push_back(prefix + "open      = " + std::to_string(is_open()));
  out.push_back(prefix + "recv_tout = " + std::to_string(get_timeout_recv()));
  out.push_back(prefix + "flags     = [");
  std::string flagsPrefix = prefix + "    ";
  for (std::map<const COMM_FLAG, const std::string>::const_iterator it = COMM_FLAG_map.begin();
       it != COMM_FLAG_map.end(); it++) {
    if (getFlags() & it->first)
      out.push_back(flagsPrefix + it->second);
  }
  out.push_back(prefix + "]");
  rapidjson::StringBuffer sb;
  rapidjson::PrettyWriter<rapidjson::StringBuffer> writer(sb, nullptr, 4 * (nindent + 2));
  writer.SetIndent(' ', 4);
  writer.SetYggdrasilMode(true);
  getMetadata().metadata.Accept(writer);
  out.push_back(prefix + "metadata = " + std::string(sb.GetString()));
  for (std::vector<std::string>::const_iterator it = extra_lines_after.begin();
       it != extra_lines_after.end(); it++)
    out.push_back(prefix + *it);
  return out;
}

std::string Comm_t::printStatus(
  unsigned int nindent,
  const std::vector<std::string>& extra_lines_before,
  const std::vector<std::string>& extra_lines_after) const {
  if (global_comm)
    return global_comm->printStatus(nindent, extra_lines_before,
				    extra_lines_after);
  std::vector<std::string> lines = get_status_message(
    nindent, extra_lines_before, extra_lines_after);
  std::string out;
  for (std::vector<std::string>::const_iterator it = lines.begin();
       it != lines.end(); it++) {
    out += *it + "\n";
  }
  log_info() << out;
  return out;
}

bool Comm_t::create_global_scope_comm() {
  COMM_TYPE global_type = getType();
  std::string global_name = name;
  DIRECTION global_direction = direction;
  bool is_server = false;
  int prev_global_scope_comm = get_global_scope_comm();
  if (global_type != SERVER_COMM && global_type != CLIENT_COMM &&
      !name.empty()) {
    char* server_var = NULL;
    if (direction == RECV) {
      server_var = std::getenv("YGG_SERVER_INPUT");
    } else if (direction == SEND) {
      server_var = std::getenv("YGG_SERVER_OUTPUT");
    }
    if (server_var && name == std::string(server_var)) {
      log_debug() << "create_global_scope_comm: " << name <<
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
      if (model_name)
	global_name.assign(model_name);
      global_scope_comm_on();
    }
  }
  if (name.empty() || (!get_global_scope_comm()) ||
      (flags & (COMM_FLAG_GLOBAL | COMM_FLAG_WORKER |
		COMM_FLAG_CLIENT_RESPONSE |
		COMM_FLAG_SERVER_RESPONSE)) ||
      // Allow server/client to be stored as a global_comm
      ((flags & COMM_FLAG_WRAPPER) &&
       !(global_type == SERVER_COMM || global_type == CLIENT_COMM)))
    return false;
  log_debug() << "create_global_scope_comm: " << global_name << " (dir="
	      << global_direction << ") is a global communicator ("
	      << "global_scope_comm = " << get_global_scope_comm() << ")"
	      << std::endl;
  global_comm = ctx->find_registered_comm(global_name,
					  global_direction,
					  global_type);
  if (!global_comm) {
    log_debug() << "create_global_scope_comm: Creating global comm \""
	      << global_name << "\"" << std::endl;
    Address global_address;
    if (address.valid())
      global_address.address(address.address());
    global_comm = new_Comm_t(global_direction, global_type, global_name,
			     global_address, flags | COMM_FLAG_GLOBAL);
    log_debug() << "create_global_scope_comm: Created global comm \""
	      << global_name << "\"" << std::endl;
  } else {
    log_debug() << "create_global_scope_comm: Found global comm \""
	      << global_name << "\"" << std::endl;
  }
  address.address(global_comm->address.address());
  flags = global_comm->flags & ~COMM_FLAG_GLOBAL;
  if (is_server)
    set_global_scope_comm(prev_global_scope_comm);
  return true;
}
  
bool Comm_t::addSchema(const Metadata& s, const DIRECTION dir) {
  return getMetadata(dir).fromMetadata(s);
}
bool Comm_t::addSchema(const rapidjson::Value& s, bool isMetadata,
		       const DIRECTION dir) {
  return getMetadata(dir).fromSchema(s, isMetadata);
}
bool Comm_t::addSchema(const std::string& schemaStr, bool isMetadata,
		       const DIRECTION dir) {
  return getMetadata(dir).fromSchema(schemaStr, isMetadata);
}
bool Comm_t::addFormat(const std::string& format_str, bool as_array,
		       const std::vector<std::string>& field_names,
		       const std::vector<std::string>& field_units,
		       const DIRECTION dir) {
  return getMetadata(dir).fromFormat(format_str, as_array,
				     field_names, field_units);
}
bool Comm_t::copySchema(const Comm_t* other, const DIRECTION dir) {
  if (other->getMetadata(dir).hasType()) {
    return getMetadata(dir).fromMetadata(other->getMetadata(dir));
  }
  return true;
}
bool Comm_t::check_size(const size_t &len) const {
    // Make sure you aren't sending a message that is too big
    if (len > YGG_MSG_MAX) {
        log_error() << "check_size: " <<
	  "message too large for single packet (YGG_MSG_MAX=" <<
	  YGG_MSG_MAX << ", len=" << len << ")" << std::endl;
        return false;
    }
    return true;
}

Comm_t* YggInterface::communicator::new_Comm_t(
     const DIRECTION dir, const COMM_TYPE type, const std::string &name,
     char* address, int flags, size_t ncomm,
     const COMM_TYPE request_commtype, const COMM_TYPE response_commtype,
     int request_flags, int response_flags) {
  Address addr;
  if (address)
    addr.address(address);
  return YggInterface::communicator::new_Comm_t(dir, type, name,
						 addr, flags, ncomm,
						 request_commtype,
						 response_commtype,
						 request_flags,
						 response_flags);
}

Comm_t* YggInterface::communicator::new_Comm_t(
     const DIRECTION dir, const COMM_TYPE type, const std::string &name,
     int flags, size_t ncomm,
     const COMM_TYPE request_commtype, const COMM_TYPE response_commtype,
     int request_flags, int response_flags) {
  Address addr;
  return YggInterface::communicator::new_Comm_t(dir, type, name,
						 addr, flags, ncomm,
						 request_commtype,
						 response_commtype,
						 request_flags,
						 response_flags);
}

Comm_t* YggInterface::communicator::new_Comm_t(
     const DIRECTION dir, const COMM_TYPE type, const std::string &name,
     const Address &addr, int flags, size_t ncomm,
     const COMM_TYPE request_commtype, const COMM_TYPE response_commtype,
     int request_flags, int response_flags) {
  flags |= COMM_FLAG_DELETE;
  utils::Address addr2(addr.address());
  if (!(addr2.valid() || name.empty())) {
    addr2 = Comm_t::addressFromEnv(name, dir);
  }
  if (((flags & COMM_FLAG_FORK_CYCLE) ||
       (flags & COMM_FLAG_FORK_BROADCAST) ||
       (flags & COMM_FLAG_FORK_COMPOSITE) ||
       (ncomm > 1) || (addr.address().find(",") != std::string::npos) ||
       (name.find(",") != std::string::npos)) &&
      (type != SERVER_COMM) && (type != CLIENT_COMM)) {
    return new ForkComm(name, addr, dir, flags, type, ncomm);
  }
  if (flags & COMM_FLAG_ASYNC) {
    return new AsyncComm(name, addr, dir, flags, type,
			 request_commtype, response_commtype,
			 request_flags, response_flags);
  }
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
    return new ServerComm(name, addr, flags, type, ncomm,
			  request_commtype, response_commtype,
			  request_flags, response_flags);
  case CLIENT_COMM:
    return new ClientComm(name, addr, flags, type, ncomm,
			  request_commtype, response_commtype,
			  request_flags, response_flags);
  case FILE_COMM:
    return new FileComm(name, addr, dir, flags);
  case RMQ_COMM:
    return new RMQComm(name, addr, dir, flags);
  case REST_COMM:
    return new RESTComm(name, addr, dir, flags);
  case VALUE_COMM:
    return new ValueComm(name, addr, dir, flags);
  }
  return nullptr;
}
bool YggInterface::communicator::is_commtype_installed(const COMM_TYPE type) {
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
  case FILE_COMM:
    return FileComm::isInstalled();
  case RMQ_COMM:
    return RMQComm::isInstalled();
  case REST_COMM:
    return RESTComm::isInstalled();
  case VALUE_COMM:
    return ValueComm::isInstalled();
  }
  return false;
}

Comm_t* Comm_t::create_worker_send(Header& head) {
  assert(!global_comm);
  Comm_t* worker = getWorkers().get(this, SEND);
  if (worker && worker->address.valid()) {
    if (!head.SetMetaString("address", worker->address.address()))
      return nullptr;
  }
  return worker;
}

Comm_t* Comm_t::create_worker_recv(Header& head) {
  assert(!global_comm);
  log_debug() << "create_worker_recv: begin" << std::endl;
  try {
    const char* address_str;
    if (!head.GetMetaString("address", address_str))
      return nullptr;
    Address adr(address_str);
    return getWorkers().get(this, RECV, adr);
  } catch (...) {
    return nullptr;
  }
}

//////////////////
// SEND METHODS //
//////////////////

int Comm_t::send_raw(const char *data, const size_t &len) {
  if (global_comm)
    return global_comm->send_raw(data, len);
  if (direction != SEND && type != SERVER_COMM) {
    log_debug() << "send_raw: Attempt to send though a communicator set up to receive" << std::endl;
    return -1;
  }
  log_debug() << "send_raw: Sending " << len << " bytes to " << address.address() << std::endl;
  if (is_closed()) {
    log_error() << "send_raw: Communicator closed." << std::endl;
    return -1;
  }
  Header head(data, len, this);
  if (head.flags & HEAD_FLAG_NO_HEAD) {
    log_debug() << "send_raw: Sending data in single message. " << is_eof(data) << ", " << (flags & COMM_FLAG_USED_SENT) << std::endl;
    int out = send_single(head);
    if (out >= 0)
      setFlags(head, SEND);
    return out;
  }
  if (!create_header_send(head)) {
    log_error() << "send_raw: Failed to create header" << std::endl;
    return -1;
  }
  if (head.format() < 0) {
    log_error() << "send_raw: Error formatting message with header." << std::endl;
    return -1;
  }
  log_debug() << "send_raw: Formated header" << std::endl;
  Comm_t* xmulti = NULL;
  if (head.flags & HEAD_FLAG_MULTIPART) {
    log_debug() << "send_raw: Sending message in multiple parts" << std::endl;
    xmulti = create_worker_send(head);
    if (!xmulti) {
      log_error() << "send_raw: Error creating worker" << std::endl;
      return -1;
    }
  }
  if (send_single(head) < 0) {
    log_error() << "send_raw: Failed to send header." << std::endl;
    return -1;
  }
  if (!(head.flags & HEAD_FLAG_MULTIPART)) {
    log_debug() << "send_raw: " << head.size_msg << " bytes completed" << std::endl;
    setFlags(head, SEND);
    return head.size_msg;
  }
  while ((head.offset + head.size_msg) < head.size_curr) {
    if (xmulti->send_single(head) < 0) {
      log_error() << "send_raw: send interupted at " << head.offset << " of " << head.size_curr << " bytes" << std::endl;
      return -1;
    }
    log_debug() << "send_raw: " << head.offset << " of " << head.size_curr << " bytes sent to " << address.address() << std::endl;
  }
  log_debug() << "send_raw: returns " << head.size_curr << std::endl;
  setFlags(head, SEND);
  return head.size_curr;
}
int Comm_t::send(const rapidjson::Document& data, bool not_generic) {
  log_debug() << "send: begin" << std::endl;
  YggInterface::utils::Metadata& meta = getMetadata(SEND);
  if (!(meta.hasType() || not_generic))
    meta.setGeneric();
  char* buf = NULL;
  size_t buf_siz = 0;
  int ret = meta.serialize(&buf, &buf_siz, data);
  if (ret < 0) {
    log_error() << "send: serialization error" << std::endl;
    return ret;
  }
  if (meta.checkFilter()) {
    log_debug() << "send: Skipping filtered message" << std::endl;
  } else {
    ret = send_raw(buf, ret);
  }
  meta.GetAllocator().Free(buf);
  log_debug() << "send: returns " << ret << std::endl;
  return ret;
}
int Comm_t::send(const rapidjson::Value& data, bool not_generic) {
  rapidjson::Document tmp;
  tmp.CopyFrom(data, tmp.GetAllocator(), true);
  return send(tmp, not_generic);
}
int Comm_t::send(const char *data, const size_t &len) {
  std::string data_str(data, len);
  return sendVar(data_str);
}
int Comm_t::send_dict(const rapidjson::Document& data,
		      std::vector<std::string> key_order,
		      size_t dim) {
  rapidjson::Document tmp;
  if (!_coerce_to_dict(data, tmp, SEND, key_order, dim))
    return -1;
  return send(tmp);
}
int Comm_t::send_array(const rapidjson::Document& data,
		       std::vector<std::string> key_order,
		       size_t dim) {
  rapidjson::Document tmp;
  if (!_coerce_to_array(data, tmp, SEND, key_order, dim))
    return -1;
  return send(tmp);
}

//////////////////
// RECV METHODS //
//////////////////

void Comm_t::set_timeout_recv(int64_t new_timeout) {
  if (global_comm) {
    global_comm->set_timeout_recv(new_timeout);
    return;
  }
  timeout_recv = new_timeout;
}
int64_t Comm_t::get_timeout_recv() const {
  if (global_comm) {
    return global_comm->get_timeout_recv();
  }
  return timeout_recv;
}
int Comm_t::wait_for_recv(const int64_t& tout) {
  if (global_comm)
    return global_comm->wait_for_recv(tout);
  log_debug() << "wait_for_recv: timeout = " << tout <<
    " microseconds" << std::endl;
  TIMEOUT_LOOP(tout, YGG_SLEEP_TIME) {
    int nmsg = comm_nmsg(RECV);
    if (nmsg < 0) {
      log_error() << "wait_for_recv: Error in checking for messages" << std::endl;
      return -1;
    } else if (nmsg > 0) {
      return nmsg;
    }
    log_debug() << "wait_for_recv: No messages, sleep " << YGG_SLEEP_TIME << " (timeout = " << tout << ")" << std::endl;
    AFTER_TIMEOUT_LOOP(YGG_SLEEP_TIME);
  }
  return comm_nmsg(RECV);
}
long Comm_t::recv_raw(char*& data, const size_t &len) {
  if (global_comm)
    return global_comm->recv_raw(data, len);
  log_debug() << "recv_raw: Receiving from " << address.address() << std::endl;
  if (direction != RECV && type != CLIENT_COMM) {
    log_debug() << "recv_raw: Attempt to receive from communicator set up to send" << std::endl;
    return -1;
  }
  Header head(data, len, true);
  long ret = -1;
  while (true) {
    if (is_closed()) {
      log_error() << "recv_raw: Communicator closed." << std::endl;
      return -1;
    }
    if (wait_for_recv(get_timeout_recv()) <= 0) {
      log_error() << "recv_raw: No messages waiting" << std::endl;
      return -1;
    }
    ret = recv_single(head);
    if (ret < 0) {
      log_error() << "recv_raw: Failed to receive header" << std::endl;
      return ret;
    }
    if (!(head.flags & HEAD_FLAG_REPEAT))
      break;
    head.reset(HEAD_RESET_KEEP_BUFFER);
  }
  if (head.flags & HEAD_FLAG_EOF) {
    log_debug() << "recv_raw: EOF received" << std::endl;
    setFlags(head, RECV);
    return -2;
  }
  Comm_t* xmulti = NULL;
  if (head.flags & HEAD_FLAG_MULTIPART) {
    log_debug() << "recv_raw: Message is multipart" << std::endl;
    xmulti = create_worker_recv(head);
    if (xmulti == NULL) {
      log_error() << "recv_raw: Failed to create worker communicator" << std::endl;
      return -1;
    }
  }
  head.offset = head.size_curr;
  while (head.size_curr < head.size_data) {
    if (xmulti->wait_for_recv(get_timeout_recv()) <= 0) {
      log_error() << "recv_raw: No messages waiting in work comm" << std::endl;
      return -1;
    }
    ret = xmulti->recv_single(head);
    if (ret < 0) {
      log_error() << "recv_raw: Receive interrupted at " << head.size_curr << " of " << head.size_data << " bytes." << std::endl;
      break;
    }
    log_debug() << "recv_raw: " << head.size_curr << " of " << head.size_data << " bytes received." << std::endl;
  }
  if (xmulti)
    getWorkers().remove_worker(xmulti);
  if (ret < 0) return ret;
  if (ret > 0) {
    if (!head.finalize_recv()) {
      log_error() << "recv_raw: finalize_recv failed." << std::endl;
      return -1;
    }
    if (!head.hasType()) {
      log_debug() << "recv_raw: No type information in message header" << std::endl;
    } else {
      log_debug() << "recv_raw: Updating type" << std::endl;
      YggInterface::utils::Metadata& meta = getMetadata(RECV);
      if ((!meta.hasType()) && (meta.transforms.size() == 0)) {
	if (!meta.fromSchema(head.getSchema()[0]))
	  return -1;
      }
      log_debug() << "recv_raw: Updated type" << std::endl;
    }
  }
  log_debug() << "recv_raw: Received " << head.size_curr << " bytes from " << address.address() << std::endl;
  ret = head.size_data;
  setFlags(head, RECV);
  return ret;
}
long Comm_t::recv(rapidjson::Document& data, bool not_generic) {
  log_debug() << "recv: begin" << std::endl;
  char* buf = NULL;
  size_t buf_siz = 0;
  long ret = recv_raw(buf, buf_siz);
  YggInterface::utils::Metadata& meta = getMetadata(RECV);
  if (ret < 0) {
    if (buf != NULL)
      free(buf);
    if (ret != -2)
      log_error() << "recv: Error in recv" << std::endl;
    return ret;
  }
  ret = meta.deserialize(buf, data);
  free(buf);
  if (ret < 0) {
    log_error() << "recv: Error deserializing message" << std::endl;
    return ret;
  }
  if (meta.checkFilter()) {
    log_debug() << "recv: Skipping filtered message." << std::endl;
    return recv(data, not_generic);
  }
  log_debug() << "recv: returns " << ret << std::endl;
  return ret;
}

long Comm_t::recv(char*& data, const size_t &len,
		  bool allow_realloc) {
  std::string data_str;
  long out = -1;
  if (!cache.empty()) {
    out = copyData(data, len, cache.begin()->c_str(),
		   cache.begin()->size(), allow_realloc);
    if (out >= 0)
      cache.erase(cache.begin());
    return out;
  }
  out = recv(data_str);
  if (out >= 0) {
    out = copyData(data, len, data_str.c_str(), data_str.size(),
		   allow_realloc);
    if (out < 0)
      cache.push_back(data_str);
  }
  return out;
}

long Comm_t::recv_dict(rapidjson::Document& data,
		       std::vector<std::string> key_order,
		       size_t dim) {
  rapidjson::Document tmp;
  long out = recv(tmp);
  if (out >= 0 && !_coerce_to_dict(tmp, data, RECV, key_order, dim))
    return -1;
  return out;
}

long Comm_t::recv_array(rapidjson::Document& data,
			std::vector<std::string> key_order,
			size_t dim) {
  rapidjson::Document tmp;
  long out = recv(tmp);
  if (out >= 0 && !_coerce_to_array(tmp, data, RECV, key_order, dim))
    return -1;
  return out;
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

YggInterface::utils::Metadata& Comm_t::getMetadata(const DIRECTION dir) {
  if (global_comm)
    return global_comm->getMetadata(dir);
  return metadata;
}

// int Comm_t::update_datatype(const rapidjson::Value& new_schema,
// 			    const DIRECTION dir) {
//   YggInterface::utils::Metadata& meta = getMetadata(dir);
//   if (!meta.fromSchema(new_schema))
//     return -1;
//   return 1;
// }

// int Comm_t::deserialize(const char* buf, rapidjson::VarArgList& ap) {
//   YggInterface::utils::Metadata& meta = getMetadata(RECV);
//   if (!meta.hasType()) {
//     log_error() << "deserialize: No datatype" << std::endl;
//     return -1;
//   }
//   log_debug() << "deserialize: begin" << std::endl;
//   int ret = meta.deserialize(buf, ap);
//   log_debug() << "deserialize: returns " << ret << std::endl;
//   return ret;
// }

// int Comm_t::serialize(char*& buf, size_t& buf_siz,
// 		      rapidjson::VarArgList& ap) {
//   YggInterface::utils::Metadata& meta = getMetadata(SEND);
//   if (!meta.hasType()) {
//     log_error() << "serialize: No datatype" << std::endl;
//     return -1;
//   }
//   log_debug() << "serialize: begin" << std::endl;
//   int ret = meta.serialize(&buf, &buf_siz, ap);
//   log_debug() << "serialize: returns " << ret << std::endl;
//   return ret;
// }

long Comm_t::vRecv(rapidjson::VarArgList& ap) {
    log_debug() << "vRecv: begin" << std::endl;
    rapidjson::Document data;
    size_t nargs_orig = ap.get_nargs();
    long ret = recv(data, true);
    if (ret < 0) {
	if (ret != -2)
	    log_error() << "vRecv: Error in recv" << std::endl;
        return ret;
    }
    YggInterface::utils::Metadata& meta = getMetadata(RECV);
    ret = meta.deserialize_args(data, ap);
    if (ret < 0) {
        log_error() << "vRecv: Error deserializing message" << std::endl;
        return ret;
    }
    if (ret >= 0)
      ret = (int)(nargs_orig - ap.get_nargs());
    log_debug() << "vRecv: returns " << ret << std::endl;
    return ret;
}
int Comm_t::vSend(rapidjson::VarArgList& ap) {
  log_debug() << "vSend: begin" << std::endl;
  YggInterface::utils::Metadata& meta = getMetadata(SEND);
  rapidjson::Document data;
  size_t nargs_orig = ap.get_nargs();
  if (meta.serialize_args(data, ap) < 0) {
    log_error() << "vSend: Error extracting arguments" << std::endl;
    return -1;
  }
  int ret = send(data, true);
  if (ret >= 0)
    ret = (int)(nargs_orig - ap.get_nargs());
  log_debug() << "vSend: returns " << ret << std::endl;
  return ret;
}
long Comm_t::call(const rapidjson::Document& sendData,
		  rapidjson::Document& recvData) {
  if (!(flags & COMM_FLAG_CLIENT)) {
    log_error() << "call: Communicator is not a client." << std::endl;
    return -1;
  }
  if (send(sendData) < 0) {
    log_error() << "call: Error in send." << std::endl;
    return -1;
  }
  return recv(recvData);
}
long Comm_t::vCall(rapidjson::VarArgList& ap) {
  if (!(flags & COMM_FLAG_CLIENT)) {
    log_error() << "vCall: Communicator is not a client." << std::endl;
    return -1;
  }
  size_t send_nargs = 0;
  rapidjson::Document tmp;
  YggInterface::utils::Metadata& meta_send = getMetadata(SEND);
  if (meta_send.hasType()) {
    send_nargs = tmp.CountVarArgs(*(meta_send.getSchema()), false);
  }
  if (ap.get_nargs() < send_nargs) {
    log_error() << "vCall: Not enough arguments for send" << std::endl;
    return -1;
  }
  size_t recv_nargs = ap.get_nargs() - send_nargs;
  ap.set_nargs(send_nargs);
  int sret = vSend(ap);
  if (sret < 0) {
    log_error() << "vCall: Error in vSend" << std::endl;
    return -1;
  }
  log_debug() << "vCall: Used " << sret
	    << " arguments in send" << std::endl;
  ap.set_nargs(recv_nargs);
  log_debug() << "vCall: " << ap.get_nargs()
	    << " arguments remaining for receive" << std::endl;
  long rret = vRecv(ap);
  if (rret >= 0) {
    log_debug() << "vCall: " << ap.get_nargs()
	      << " arguments after receive" << std::endl;
  }
  YGGCPP_END_VAR_ARGS(ap);
  return rret;
  
}
