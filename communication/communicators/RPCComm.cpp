#include "RPCComm.hpp"
#include "DefaultComm.hpp"

#ifdef COMM_BASE

using namespace communication::communicator;
using namespace communication::utils;

RPCComm::RPCComm(const std::string &name, Address *address,
		 int flgs, DIRECTION dir, DIRECTION req_dir,
		 const COMM_TYPE type) :
  COMM_BASE(name, address, dir, flgs, type),
  requests(req_dir, flgs & (COMM_ALLOW_MULTIPLE_COMMS |
			    COMM_FLAG_ASYNC_WRAPPED)) {}

void RPCComm::close() {
  requests.destroy();
  COMM_BASE::close();
}

int RPCComm::comm_nmsg(DIRECTION dir) const {
  if (global_comm)
    return global_comm->comm_nmsg(dir);
  if (dir == NONE)
    dir = direction;
  if (dir == direction)
    return COMM_BASE::comm_nmsg(dir);
  if (requests.requests.empty() || requests.comms.empty()) {
    ygglog_error << "RPCComm(" << name << ")::comm_nmsg: No pending requests" << std::endl;
    return -1;
  }
  return requests.comms[requests.requests[0].comm_idx]->comm_nmsg(dir);
}

communication::utils::Metadata& RPCComm::getMetadata(const DIRECTION dir) {
  if (global_comm)
    return global_comm->getMetadata(dir);
  if (dir == this->direction || dir == NONE)
    return this->metadata;
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
bool RPCComm::addResponseSchema(const communication::utils::Metadata& metadata,
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

#ifdef YGG_TEST
bool RPCComm::afterSendRecv(Comm_t* sComm, Comm_t* rComm) {
  if (sComm->global_comm)
    sComm = sComm->global_comm;
  if (rComm->global_comm)
    rComm = rComm->global_comm;
  if ((sComm->flags & COMM_FLAG_CLIENT) &&
      (rComm->flags & COMM_FLAG_SERVER))
    return COMM_BASE::afterSendRecv(sComm, rComm);
  assert((sComm->flags & COMM_FLAG_SERVER) &&
	 (rComm->flags & COMM_FLAG_CLIENT));
  Comm_t* sComm_res = dynamic_cast<RPCComm*>(sComm)->requests.lastComm();
  Comm_t* rComm_res = dynamic_cast<RPCComm*>(rComm)->requests.lastComm();
  assert(sComm_res && rComm_res);
  return sComm_res->afterSendRecv(sComm_res, rComm_res);
}
#endif

#endif
