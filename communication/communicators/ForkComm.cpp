#include "ForkComm.hpp"
#include <numeric>

using namespace communication::communicator;
using namespace communication::utils;

///////////////
// ForkTines //
///////////////

ForkTines::ForkTines(const std::string logInst,
		     std::vector<Comm_t*>& comm,
		     const FORK_TYPE typ) :
  logInst_(logInst), forktype(typ), comms(), iter(0) {
  comms.swap(comm);
  updateType();
}
ForkTines::ForkTines(const std::string logInst,
		     const std::vector<std::string>& names,
		     const std::vector<std::string>& addrs,
		     const DIRECTION dir, int flags,
		     const COMM_TYPE commtype, const FORK_TYPE forktyp) :
  logInst_(logInst), forktype(forktyp), comms(), iter(0) {
  size_t ncomm = (std::max)(names.size(), addrs.size());
  if (ncomm == 0) {
    ygglog_throw_error("ForkTines: Fork comm empty");
  }
  const std::string blankName = "";
  for (size_t i = 0; i < ncomm; i++) {
    const std::string* iname = &blankName;
    utils::Address iaddr("");
    if (!names.empty()) {
      if (i >= names.size()) {
	ygglog_throw_error("ForkTines: More addrs provided than names");
      }
      iname = &(names[i]);
    }
    if (!addrs.empty()) {
      if (i >= addrs.size()) {
	ygglog_throw_error("ForkTines: More names provided than addresses");
      }
      iaddr = utils::Address(addrs[i]);
    }
    Comm_t* icomm = new_Comm_t(dir, commtype, *iname, iaddr, flags);
    if (!icomm) {
      ygglog_throw_error("ForkTines: Failed to create tine comm");
    }
    if (!(icomm->getFlags() & COMM_FLAG_VALID)) {
      delete icomm;
      ygglog_throw_error("ForkTines: Invalid comm");
    }
    comms.push_back(icomm);
  }
  updateType();
}
ForkTines::~ForkTines() {
  for (size_t i = 0; i < comms.size(); i++) {
    delete comms[i];
    comms[i] = nullptr;
  }
  comms.clear();
}
void ForkTines::updateType() {
  // TODO: Set flag identifying fork tine?
  if (forktype == FORK_DEFAULT && comms.size() > 0) {
    if (comms[0]->getDirection() == SEND)
      forktype = FORK_BROADCAST;
    else
      forktype = FORK_CYCLE;
  }
  if (forktype == FORK_DEFAULT) {
    ygglog_throw_error("ForkTines: Could not determine fork type");
  }
  for (typename std::vector<Comm_t*>::iterator it = comms.begin();
       it != comms.end(); it++)
    (*it)->getFlags() |= COMM_FLAG_FORK_TINE;
}

void ForkTines::close() {
  for (typename std::vector<Comm_t*>::iterator it = comms.begin();
       it != comms.end(); it++)
    (*it)->close();
}

bool ForkTines::eof() const {
  for (typename std::vector<Comm_t*>::const_iterator it = comms.begin();
       it != comms.end(); it++) {
    if (!((*it)->getFlags() & COMM_FLAG_EOF_RECV))
      return false;
  }
  return true;
}

Comm_t* ForkTines::current_cycle() {
  return comms[iter % comms.size()];
}

int ForkTines::nmsg(DIRECTION dir) const {
  std::vector<int> nmsg;
  for (typename std::vector<Comm_t*>::const_iterator it = comms.begin();
       it != comms.end(); it++) {
    int inmsg = (*it)->comm_nmsg(dir);
    if (inmsg < 0)
      return -1;
    nmsg.push_back(inmsg);
  }
  if (forktype == FORK_COMPOSITE)
    return *std::min_element(nmsg.begin(), nmsg.end());
  else if (forktype == FORK_BROADCAST || forktype == FORK_COMPOSITE)
    return *std::max_element(nmsg.begin(), nmsg.end());
  return std::accumulate(nmsg.begin(), nmsg.end(),
			 decltype(nmsg)::value_type(0));
}

int ForkTines::send(const char *data, const size_t &len,
		    communication::utils::Metadata& meta) {
  bool is_eof = (strcmp(data, YGG_MSG_EOF) == 0);
  char* tmp_data = const_cast<char*>(data);
  size_t tmp_len = len;
  bool tmp_created = false;
  rapidjson::Document doc;
  if (!is_eof) {
    if (meta.deserialize(data, doc, true) < 0)
      return -1;
    int out = meta.serialize_updates(doc);
    if (out <= 0) // Error or filter
      return out;
    if (meta.transforms.size() > 0 && forktype != FORK_COMPOSITE) {
      tmp_created = true;
      tmp_data = nullptr;
      tmp_len = 0;
      if (meta.serialize(&tmp_data, &tmp_len, doc, true) < 0)
	return -1;
    }
  }
  if (forktype == FORK_BROADCAST || is_eof) {
    for (typename std::vector<Comm_t*>::iterator it = comms.begin();
	 it != comms.end(); it++) {
      if ((*it)->send_raw(tmp_data, tmp_len) < 0)
	return -1;
    }
  } else if (forktype == FORK_CYCLE) {
    int out = current_cycle()->send_raw(tmp_data, tmp_len);
    if (out >= 0)
      iter++;
    return out;
  } else if (forktype == FORK_COMPOSITE) {
    if (is_eof) {
      for (typename std::vector<Comm_t*>::iterator it = comms.begin();
	   it != comms.end(); it++) {
	if ((*it)->send_raw(tmp_data, tmp_len) < 0)
	  return -1;
	    }
    } else {
      if (!doc.IsArray()) {
	log_error() << "send: Cannot split message for composite: " << doc << std::endl;
	return -1;
      }
      if (static_cast<size_t>(doc.Size()) != comms.size()) {
	log_error() << "send: Message has " << doc.Size() <<
	  " elements, but there are " << comms.size() << " comms" << std::endl;
	return -1;
      }
      size_t i = 0;
      for (typename std::vector<Comm_t*>::iterator it = comms.begin();
	   it != comms.end(); it++, i++) {
	if ((*it)->send(doc[static_cast<rapidjson::SizeType>(i)]) < 0)
	  return -1;
      }
    }
  }
  if (tmp_created) {
    meta.GetAllocator().Free(tmp_data);
  }
  return 1;
}
long ForkTines::recv(char*& data, const size_t &len,
		     communication::utils::Metadata& meta) {
  long out = -1;
  rapidjson::Document doc;
  if (forktype == FORK_CYCLE) {
    while (current_cycle()->is_closed() ||
	   (current_cycle()->getFlags() & COMM_FLAG_EOF_RECV) ||
	   (current_cycle()->comm_nmsg() == 0))
      iter++;
    out = -2;
    while (out == -2 && !eof()) {
      out = current_cycle()->recv(doc);
      iter++;
    }
  } else if (forktype == FORK_COMPOSITE) {
    doc.SetArray();
    int i = 0;
    bool is_eof = false;
    for (typename std::vector<Comm_t*>::iterator it = comms.begin();
	 it != comms.end(); it++, i++) {
      rapidjson::Document idoc;
      long iout = (*it)->recv(idoc);
      if ((is_eof && iout != -2) ||
	  (iout == -2 && !is_eof && i > 0)) {
	log_error() << "Received mix of EOF & regular messages" << std::endl;
	return -1;
      }
      is_eof = (iout == -2);
      if (!is_eof)
	doc.PushBack(idoc, doc.GetAllocator());
    }
  }
  if (eof()) {
    out = -2;
  } else {
    size_t tmp_len = len;
    out = meta.deserialize_updates(doc);
    if (out == 0) {
      out = recv(data, len, meta); // message filtered
    } else if (out > 0) {
      out = meta.serialize(&data, &tmp_len, doc, true);
    }
  }
  return out;
}


//////////////
// ForkComm //
//////////////

ForkComm::ForkComm(const std::string name, const Address &address,
		   DIRECTION direction, int flgs,
		   const COMM_TYPE commtype, size_t ncmm) :
  CommBase(name, address, direction, commtype, flgs),
  forktype(FORK_DEFAULT), ncomm(ncmm) {
  if (flgs & COMM_FLAG_FORK_CYCLE)
    forktype = FORK_CYCLE;
  else if (flgs & COMM_FLAG_FORK_BROADCAST)
    forktype = FORK_BROADCAST;
  else if (flgs & COMM_FLAG_FORK_COMPOSITE)
    forktype = FORK_COMPOSITE;
  if (!global_comm)
    init();
}
ForkComm::ForkComm(const std::string name, const DIRECTION dirn,
		   int flgs, const COMM_TYPE commtype, size_t ncomm) :
  ForkComm(name, utils::blankAddress, dirn, flgs, commtype, ncomm) {}
ForkComm::ForkComm(utils::Address &addr, const DIRECTION dirn,
		   int flgs, const COMM_TYPE commtype, size_t ncomm) :
  ForkComm("", addr, dirn, flgs, commtype, ncomm) {}

ADD_DESTRUCTOR_DEF(ForkComm, CommBase, , )

void ForkComm::init() {
  std::vector<std::string> names;
  std::vector<std::string> addrs;
  if (!this->name.empty())
    names = communication::utils::split(this->name, ",");
  if (!this->address.address().empty())
    addrs = communication::utils::split(this->address.address(), ",");
  if (((!addrs.empty()) && names.size() == 1) ||
      (addrs.empty() && names.size() <= 1 && ncomm > 0)) {
    if (!addrs.empty())
      ncomm = addrs.size();
    names.clear();
    for (size_t i = 0; i < ncomm; i++)
      names.push_back(this->name + "-" + std::to_string(i));
  }
  handle = new ForkTines(this->logInst(), names, addrs, direction,
			 flags & ~COMM_FLAG_FORK_CYCLE &
			 ~COMM_FLAG_FORK_BROADCAST &
			 ~COMM_FLAG_FORK_COMPOSITE,
			 type, forktype);
  if (forktype == FORK_DEFAULT) {
    forktype = handle->forktype;
    if (forktype == FORK_CYCLE)
      flags |= COMM_FLAG_FORK_CYCLE;
    else if (forktype == FORK_BROADCAST)
      flags |= COMM_FLAG_FORK_BROADCAST;
    else if (forktype == FORK_COMPOSITE)
      flags |= COMM_FLAG_FORK_COMPOSITE;
  }
  if (!this->address.valid()) {
    std::string new_address = "";
    int i = 0;
    for (typename std::vector<Comm_t*>::iterator it = handle->comms.begin();
	 it != handle->comms.end(); it++, i++) {
      if (i > 0)
	new_address += ",";
      new_address += (*it)->getAddress();
    }
    this->address.address(new_address);
    if (flags & COMM_FLAG_SET_OPP_ENV)
      setOppEnv();
  }
}

void ForkComm::_close(bool call_base) {
  if (handle && !global_comm) {
    handle->close();
  }
  if (call_base)
    CommBase::_close(true);
}

int ForkComm::comm_nmsg(DIRECTION dir) const {
  if (global_comm)
    return global_comm->comm_nmsg(dir);
  return handle->nmsg(dir);
}

void ForkComm::set_timeout_recv(int64_t new_timeout) {
  CommBase::set_timeout_recv(new_timeout);
  for (typename std::vector<Comm_t*>::iterator it = handle->comms.begin();
       it != handle->comms.end(); it++) {
    (*it)->set_timeout_recv(new_timeout);
  }
}

int ForkComm::send_raw(const char *data, const size_t &len) {
  if (global_comm)
    return global_comm->send_raw(data, len);
  return handle->send(data, len, getMetadata(SEND));
}

long ForkComm::recv_raw(char*& data, const size_t &len,
			bool allow_realloc) {
  if (global_comm)
    return global_comm->recv_raw(data, len, allow_realloc);
  log_debug() << "ForkComm::recv_raw: Receiving from " << address.address() << std::endl;
  if (direction != RECV && type != CLIENT_COMM) {
    log_debug() << "ForkComm::recv_raw: Attempt to receive from communicator set up to send" << std::endl;
    return -1;
  }
  if (!allow_realloc) {
    char* tmp = NULL;
    size_t tmp_len = 0;
    long ret = recv_raw(tmp, tmp_len, true);
    if (ret >= 0 || ret == -2) {
      if (ret >= 0) {
	tmp_len = static_cast<size_t>(ret);
	ret = copyData(data, len, tmp, tmp_len, false);
      }
      if (tmp)
	free(tmp);
    }
    return ret;
  }
  return handle->recv(data, len, getMetadata(RECV));
}
