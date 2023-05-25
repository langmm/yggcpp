#include "RPCComm.hpp"
#include "DefaultComm.hpp"

#ifdef COMM_BASE

using namespace communication::communicator;
using namespace communication::utils;

RPCComm::RPCComm(const std::string &name, Address *address,
		 int flgs, DIRECTION dir, DIRECTION req_dir) :
  COMM_BASE(name, address, dir, flgs), requests(req_dir) {}
RPCComm::RPCComm(const std::string name, int flgs,
		 DIRECTION dir, DIRECTION req_dir) :
  COMM_BASE(name, dir, flgs), requests(req_dir) {}

Metadata& RPCComm::get_metadata(const DIRECTION dir) {
  if (dir == this->direction)
    return this->metadata;
  return requests.response_metadata;
}
void RPCComm::addResponseSchema(const std::string& s) {
  requests.addResponseSchema(s);
}
void RPCComm::addResponseSchema(const rapidjson::Value& s) {
  requests.addResponseSchema(s);
}
void RPCComm::addResponseFormat(const std::string& fmt) {
  requests.addResponseFormat(fmt);
}

#endif
