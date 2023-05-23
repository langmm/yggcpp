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

void RPCComm::addResponseFormat(std::string& fmt) {
  requests.addResponseFormat(fmt);
}

#endif
