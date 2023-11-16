#include "ServerComm.hpp"
#include "utils/tools.hpp"

#ifdef COMM_BASE

using namespace communication::communicator;
using namespace communication::utils;

ServerComm::ServerComm(const std::string nme, const utils::Address& addr,
		       int flgs, const COMM_TYPE type) :
  RPCComm(nme, addr,
	  flgs | COMM_FLAG_SERVER | COMM_ALWAYS_SEND_HEADER,
	  RECV, SEND, type) {}

ADD_CONSTRUCTORS_RPC_DEF(ServerComm)

std::string ServerComm::logClass() const {
  std::string out = "ServerComm";
  return out;
}

bool ServerComm::signon(const Header& header) {
  assert(header.flags & HEAD_FLAG_CLIENT_SIGNON);
  log_debug() << "signon: begin (" <<
    (header.flags & HEAD_FLAG_CLIENT_SIGNON) << ")" << std::endl;
  if (send_raw(YGG_SERVER_SIGNON, YGG_SERVER_SIGNON_LEN) < 0) {
    log_error() << "signon: Error in sending sign-on" << std::endl;
    return false;
  }
  log_debug() << "signon: sent" << std::endl;
  return requests.signon_complete;
}

bool ServerComm::create_header_send(utils::Header& header) {
  log_debug() << "create_header_send: begin" << std::endl;
  assert(!global_comm);
  log_debug() << "create_header_send: Checking for response comm" << std::endl;
  Comm_t* response_comm = requests.activeComm();
  if (response_comm == NULL) {
    requests.Display();
    log_error() << "create_header_send: Failed to get response comm" << std::endl;
    return false;
  }
  log_debug() << "create_header_send: Found response comm" << std::endl;
  if (!requests.transferSchemaTo(response_comm))
    return false;
  bool out = response_comm->create_header_send(header);
  if ((!out) || header.flags & HEAD_FLAG_EOF)
    return out;
  if (requests.addResponseServer(header) < 0) {
    log_error() << "create_header_send: Failed to add response" << std::endl;
    return false;
  }
  log_debug() << "create_header_send: done" << std::endl;
  return true;
}

int ServerComm::send_single(utils::Header& header) {
    assert(!global_comm);
    log_debug() << "send_single" << std::endl;
    Comm_t* response_comm = requests.activeComm();
    if (response_comm == NULL) {
        log_error() << "send_single: Failed to get response comm" << std::endl;
        return -1;
    }
    int ret = response_comm->send_single(header);
    log_debug() << "send_single: Sent " << header.size_msg << " bytes" << std::endl;
    if ((ret >= 0) && (requests.popRequestServer() < 0))
      return -1;
    return ret;
}

long ServerComm::recv_single(utils::Header& header) {
  assert(!global_comm);
  log_debug() << "recv_single" << std::endl;
  long ret = COMM_BASE::recv_single(header);
  if (ret < 0)
    return ret;
  if (header.flags & HEAD_FLAG_EOF) {
    if (!requests.partnerSignoff(header))
      header.flags |= HEAD_FLAG_REPEAT;
    return ret;
  }
  if (requests.addRequestServer(header) < 0) {
    log_error() << "recv_single: Failed to add request" << std::endl;
    return -1;
  }
  if (header.flags & HEAD_FLAG_CLIENT_SIGNON) {
    if (!signon(header))
      return -1;
    header.flags |= HEAD_FLAG_REPEAT;
  }
  return ret;
}


#endif
