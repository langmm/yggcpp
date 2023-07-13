#include "RPCComm.hpp"
#include "DefaultComm.hpp"

#ifdef COMM_BASE

using namespace communication::communicator;
using namespace communication::utils;

RPCComm::RPCComm(const std::string &name, Address *address,
		 int flgs, DIRECTION dir, DIRECTION req_dir,
		 const COMM_TYPE type) :
  COMM_BASE(name, address, dir, flgs, type), requests(req_dir) {}

Metadata& RPCComm::get_metadata(const DIRECTION dir) {
  if (global_comm)
    return global_comm->get_metadata(dir);
  if (dir == this->direction || dir == NONE)
    return this->metadata;
  return requests.response_metadata;
}
void RPCComm::addResponseSchema(const std::string& s, bool use_generic) {
  if (global_comm) {
    (dynamic_cast<RPCComm*>(global_comm))->addResponseSchema(s, use_generic);
    return;
  }
  requests.addResponseSchema(s, use_generic);
}
void RPCComm::addResponseSchema(const rapidjson::Value& s,
				bool use_generic) {
  if (global_comm) {
    (dynamic_cast<RPCComm*>(global_comm))->addResponseSchema(s, use_generic);
    return;
  }
  requests.addResponseSchema(s, use_generic);
}
void RPCComm::addResponseSchema(const Metadata& metadata,
				bool use_generic) {
  if (global_comm) {
    (dynamic_cast<RPCComm*>(global_comm))->addResponseSchema(metadata, use_generic);
    return;
  }
  requests.addResponseSchema(metadata, use_generic);
}
void RPCComm::addResponseFormat(const std::string& fmt,
				bool use_generic) {
  if (global_comm) {
    (dynamic_cast<RPCComm*>(global_comm))->addResponseFormat(fmt, use_generic);
    return;
  }
  requests.addResponseFormat(fmt, use_generic);
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
  else if (!((sComm->flags & COMM_FLAG_SERVER) &&
	     (rComm->flags & COMM_FLAG_CLIENT))) {
    ygglog_error << "RPCComm::afterSendRecv: Provided communicator types do not match a response message pattern" << std::endl;
    return false;
  }
  Comm_t* sComm_res = dynamic_cast<RPCComm*>(sComm)->requests.lastComm();
  Comm_t* rComm_res = dynamic_cast<RPCComm*>(rComm)->requests.lastComm();
  if (!(sComm_res && rComm_res))
    return false;
  return sComm_res->afterSendRecv(sComm_res, rComm_res);
}
#endif

#endif
