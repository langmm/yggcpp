#include "communicators/ForkComm.hpp"
#include <numeric>

using namespace YggInterface::communicator;
using namespace YggInterface::utils;

///////////////
// ForkTines //
///////////////

ForkTines::ForkTines(const std::string logInst,
		     const std::vector<std::string>& names,
		     const std::vector<std::string>& addrs,
		     const DIRECTION dir, FLAG_TYPE flags,
		     const COMM_TYPE commtype, const FORK_TYPE forktyp) :
  LogBase(), logInst_(logInst), forktype(forktyp), comms(), iter(0) {
  size_t ncomm = (std::max)(names.size(), addrs.size());
  if (ncomm == 0) {
    throw_error("ForkTines: Fork comm empty");
  }
  const std::string blankName = "";
  for (size_t i = 0; i < ncomm; i++) {
    const std::string* iname = &blankName;
    utils::Address iaddr("");
    if (!names.empty()) {
      if (i >= names.size()) {
	throw_error("ForkTines: More addrs provided than names");
      }
      iname = &(names[i]);
    }
    if (!addrs.empty()) {
      if (i >= addrs.size()) {
	throw_error("ForkTines: More names provided than addresses");
      }
      iaddr = utils::Address(addrs[i]);
    }
    Comm_t* icomm = new_Comm_t(dir, commtype, *iname, iaddr, flags);
    if (!(icomm && (icomm->getFlags() & COMM_FLAG_VALID))) {
      if (icomm) delete icomm;
      throw_error("ForkTines: Failed to create tine comm");
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
  if (forktype == FORK_DEFAULT) {
    if (comms[0]->getDirection() == SEND)
      forktype = FORK_BROADCAST;
    else
      forktype = FORK_CYCLE;
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
    int inmsg = (*it)->nmsg(dir);
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

int ForkTines::send(YggInterface::utils::Header& head,
		    ForkComm& parent) {
  YggInterface::utils::Metadata& meta = parent.getMetadata(SEND);
  bool is_eof = (head.flags & HEAD_FLAG_EOF);
  bool tmpdoc_created = false;
  rapidjson::Document* tmp = &(head.doc);
  int out = 1;
  if ((!is_eof) && !(head.flags & HEAD_FLAG_DOC_SET)) {
    log_error() << "send: Document not set" << std::endl;
    out = -1;
    goto cleanup;
  }
  if (is_eof) {
    for (typename std::vector<Comm_t*>::iterator it = comms.begin();
	 it != comms.end(); it++) {
      if ((*it)->send_raw(head.data[0], head.size_data) < 0) {
	log_error() << "send: Error sending raw data" << std::endl;
	out = -1;
	goto cleanup;
      }
    }
  } else if (forktype == FORK_BROADCAST) {
    for (typename std::vector<Comm_t*>::iterator it = comms.begin();
	 it != comms.end(); it++) {
      if ((*it)->send(*tmp) < 0) {
	out = -1;
	goto cleanup;
      }
    }
  } else if (forktype == FORK_CYCLE) {
    out = current_cycle()->send(*tmp);
    if (out >= 0)
      iter++;
    goto cleanup;
  } else if (forktype == FORK_COMPOSITE) {
    if (!(tmp->IsArray())) {
      tmp = new rapidjson::Document(rapidjson::kNullType);
      tmpdoc_created = true;
      if (!parent._coerce_to_array(head.doc, *tmp, SEND)) {
	log_error() << "send: Cannot split message for composite: " << head.doc << std::endl;
	out = -1;
	goto cleanup;
      }
    }
    if (static_cast<size_t>(tmp->Size()) != comms.size()) {
      log_error() << "send: Message has " << tmp->Size() <<
	" elements, but there are " << comms.size() << " comms" << std::endl;
      out = -1;
      goto cleanup;
    }
    std::vector<std::string> field_names;
    if (!meta.get_field_names(field_names)) {
      out = -1;  // GCOV_EXCL_LINE
      goto cleanup;  // GCOV_EXCL_LINE
    }
    size_t i = 0;
    for (typename std::vector<Comm_t*>::iterator it = comms.begin();
	 it != comms.end(); it++, i++) {
      // TODO: Any additional data to add from parent?
      if (!field_names.empty()) {
	(*it)->getMetadata(SEND).initSchema();
	if (!(*it)->getMetadata(SEND).SetSchemaString("title", field_names[i])) {
	  out = -1;  // GCOV_EXCL_LINE
	  goto cleanup;  // GCOV_EXCL_LINE
	}
      }
      if ((*it)->send((*tmp)[static_cast<rapidjson::SizeType>(i)]) < 0) {
	out = -1;
	goto cleanup;
      }
    }
  }
 cleanup:
  if (tmpdoc_created)
    delete tmp;
  return out;
}
long ForkTines::recv(YggInterface::utils::Header& head,
		     YggInterface::utils::Metadata& meta) {
  long out = -1;
  if (forktype == FORK_CYCLE) {
    while (current_cycle()->is_closed() ||
	   (current_cycle()->getFlags() & COMM_FLAG_EOF_RECV) ||
	   (current_cycle()->nmsg() == 0))
      iter++;
    out = -2;
    while (out == -2 && !eof()) {
      out = current_cycle()->recv(head.doc);
      iter++;
    }
  } else if (forktype == FORK_COMPOSITE) {
    head.doc.SetArray();
    int i = 0;
    bool is_eof = false, no_names = false;
    std::vector<std::string> field_names;
    std::string ival;
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
      if (!is_eof) {
	if (iout < 0) {
	  log_error() << "Error on one of the tines" << std::endl;
	  return -1;
	}
	head.doc.PushBack(idoc, head.doc.GetAllocator());
	if (!no_names) {
	  if (!(*it)->getMetadata(RECV).GetSchemaStringOptional(
	       "title", ival, ""))
	    return -1;  // GCOV_EXCL_LINE
	  if (ival.empty()) {
	    no_names = true;
	  } else {
	    field_names.push_back(ival);
	  }
	}
      }
    }
    // TODO: Any other fields need to be transfered?
    if (!field_names.empty()) {
      if (!meta.set_field_names(field_names))
	return -1;  // GCOV_EXCL_LINE
    }
    out = 1;
  }
  if (eof()) {
    out = -2;
  } else if (out >= 0) {
    head.flags |= HEAD_FLAG_DOC_SET;
    if (!head.finalize_recv()) {
      log_error() << "recv_raw: finalize_recv failed." << std::endl;
      return -1;
    }
  }
  return out;
}


//////////////
// ForkComm //
//////////////

COMM_CONSTRUCTOR_CORE_DEF_PARAM(ForkComm,
				COMM_FLAG_FORK | COMM_FLAG_DONT_SERIALIZE,
				forktype(FORK_DEFAULT),
				ncomm(supp.ncomm))

void ForkComm::_open(bool call_base) {
  if (flags & COMM_FLAG_FORK_CYCLE)
    forktype = FORK_CYCLE;
  else if (flags & COMM_FLAG_FORK_BROADCAST)
    forktype = FORK_BROADCAST;
  else if (flags & COMM_FLAG_FORK_COMPOSITE)
    forktype = FORK_COMPOSITE;
  BEFORE_OPEN_DEF;
  std::vector<std::string> names;
  std::vector<std::string> addrs;
  if (!this->name.empty())
    names = YggInterface::utils::split(this->name, ",");
  if (!this->address.address().empty())
    addrs = YggInterface::utils::split(this->address.address(), ",");
  if (((!addrs.empty()) && names.size() == 1) ||
      (addrs.empty() && names.size() <= 1 && ncomm > 0)) {
    if (!addrs.empty())
      ncomm = addrs.size();
    names.clear();
    for (size_t i = 0; i < ncomm; i++)
      names.push_back(this->name + "-" + std::to_string(i));
  }
  handle = new ForkTines(this->logInst(), names, addrs, direction,
			 flags & ~COMM_FLAG_FORK &
			 ~COMM_FLAG_DONT_SERIALIZE &
			 ~COMM_FLAG_FORK_CYCLE &
			 ~COMM_FLAG_FORK_BROADCAST &
			 ~COMM_FLAG_FORK_COMPOSITE,
			 type, forktype);
  if (forktype == FORK_DEFAULT) {
    forktype = handle->forktype;
    if (forktype == FORK_CYCLE)
      flags |= COMM_FLAG_FORK_CYCLE;
    else if (forktype == FORK_BROADCAST)
      flags |= COMM_FLAG_FORK_BROADCAST;
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
  }
  AFTER_OPEN_DEF;
}

void ForkComm::_close(bool call_base) {
  BEFORE_CLOSE_DEF;
  if (handle && !global_comm) {
    handle->close();
  }
  AFTER_CLOSE_DEF;
}

int ForkComm::nmsg(DIRECTION dir) const {
  if (global_comm)
    return global_comm->nmsg(dir);
  return handle->nmsg(dir);
}

void ForkComm::set_timeout_recv(int64_t new_timeout) {
  CommBase::set_timeout_recv(new_timeout);
  for (typename std::vector<Comm_t*>::iterator it = handle->comms.begin();
       it != handle->comms.end(); it++) {
    (*it)->set_timeout_recv(new_timeout);
  }
}

int ForkComm::send_single(YggInterface::utils::Header& head) {
  if (head.on_send() < 0)
    return -1;
  return handle->send(head, *this);
}

long ForkComm::recv_single(YggInterface::utils::Header& head) {
  log_debug() << "ForkComm::recv_single: Receiving from " << address.address() << std::endl;
  if (direction != RECV && type != CLIENT_COMM) {
    log_debug() << "ForkComm::recv_single: Attempt to receive from communicator set up to send" << std::endl;
    return -1;
  }
  return handle->recv(head, getMetadata(RECV));
}
