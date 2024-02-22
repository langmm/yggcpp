#include "communicators/RPCComm.hpp"
#include "communicators/DefaultComm.hpp"

using namespace YggInterface::communicator;
using namespace YggInterface::utils;

RPCComm::RPCComm(const std::string &name, const utils::Address& address,
		 int flgs, DIRECTION dir, DIRECTION req_dir,
		 const COMM_TYPE type,
		 const SupplementCommArgs& supp) :
  WrapComm(name, address, dir, supp.request_flags | flgs, type,
	   supp.request_commtype, supp),
  requests(req_dir,
	   supp.response_flags |
	   (flgs & (COMM_FLAG_ALLOW_MULTIPLE_COMMS |
		    COMM_FLAG_ASYNC_WRAPPED)),
	   supp.response_commtype, logInst()) {
  ADD_CONSTRUCTOR_OPEN(RPCComm);
}

COMM_DESTRUCTOR_DEF(RPCComm, WrapComm, , )

void RPCComm::_open(bool call_base) {
  BEFORE_OPEN(WrapComm);
  AFTER_OPEN(WrapComm);
}

void RPCComm::_close(bool call_base) {
  BEFORE_CLOSE(WrapComm);
  requests.destroy();
  AFTER_CLOSE(WrapComm);
}

int RPCComm::comm_nmsg(DIRECTION dir) const {
  if (global_comm)
    return global_comm->comm_nmsg(dir);
  if (dir == NONE)
    dir = direction;
  if (dir == direction)
    return WrapComm::comm_nmsg(dir);
  if (requests.requests.empty() || requests.comms.empty()) {
    log_error() << "::RPCComm:comm_nmsg: No pending requests" << std::endl;
    return -1;
  }
  return requests.comms[requests.requests[0].comm_idx]->comm_nmsg(dir);
}

int RPCComm::wait_for_recv(const int64_t& tout) {
  if (global_comm)
    return global_comm->wait_for_recv(tout);
  if (direction == RECV)
    return WrapComm::wait_for_recv(tout);
  if (requests.requests.empty() || requests.comms.empty()) {
    log_error() << "::RPCComm:wait_for_recv: No pending requests" << std::endl;
    return -1;
  }
  return requests.comms[requests.requests[0].comm_idx]->wait_for_recv(tout);
}

YggInterface::utils::Metadata& RPCComm::getMetadata(const DIRECTION dir) {
  if (global_comm)
    return global_comm->getMetadata(dir);
  if (dir == this->direction || dir == NONE)
    return WrapComm::getMetadata(dir);
  return requests.response_metadata;
}
bool RPCComm::addResponseSchema(const std::string& s, bool use_generic) {
  if (global_comm) {
    return (dynamic_cast<RPCComm*>(global_comm))->addResponseSchema(s, use_generic);
  }
  return requests.addResponseSchema(s, use_generic);
}
bool RPCComm::addResponseSchema(const rapidjson::Value& s,
				bool use_generic) {
  if (global_comm) {
    return (dynamic_cast<RPCComm*>(global_comm))->addResponseSchema(s, use_generic);
  }
  return requests.addResponseSchema(s, use_generic);
}
bool RPCComm::addResponseSchema(const YggInterface::utils::Metadata& metadata,
				bool use_generic) {
  if (global_comm) {
    return (dynamic_cast<RPCComm*>(global_comm))->addResponseSchema(metadata, use_generic);
  }
  return requests.addResponseSchema(metadata, use_generic);
}
bool RPCComm::addResponseFormat(const std::string& fmt,
				bool use_generic) {
  if (global_comm) {
    return (dynamic_cast<RPCComm*>(global_comm))->addResponseFormat(fmt, use_generic);
  }
  return requests.addResponseFormat(fmt, use_generic);
}

// Test methods
bool RPCComm::afterSendRecv(Comm_t* sComm, Comm_t* rComm) {
  if (sComm->global_comm)
    sComm = sComm->global_comm;
  if (rComm->global_comm)
    rComm = rComm->global_comm;
  if ((sComm->flags & COMM_FLAG_CLIENT) &&
      (rComm->flags & COMM_FLAG_SERVER))
    return WrapComm::afterSendRecv(sComm, rComm);
  assert((sComm->flags & COMM_FLAG_SERVER) &&
	 (rComm->flags & COMM_FLAG_CLIENT));
  Comm_t* sComm_res = dynamic_cast<RPCComm*>(sComm)->requests.lastComm();
  Comm_t* rComm_res = dynamic_cast<RPCComm*>(rComm)->requests.lastComm();
  assert(sComm_res && rComm_res);
  return sComm_res->afterSendRecv(sComm_res, rComm_res);
}
