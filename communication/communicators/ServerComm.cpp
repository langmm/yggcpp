#include "ServerComm.hpp"
#include "DefaultComm.hpp"
#include "utils/tools.hpp"

#ifdef COMM_BASE

using namespace communication::communicator;
using namespace communication::utils;

ServerComm::ServerComm(const std::string &name, Address *address) :
  COMM_BASE(name, address, RECV), requests(SEND) {
  // Called to create temp comm for send/recv
  if (name.empty() && address != nullptr && address->valid())
    return;
  init();
}

ServerComm::ServerComm(const std::string name) :
  COMM_BASE(name, RECV), requests(SEND) {
  init();
}

void ServerComm::init() {
  if (this->name.empty()) {
    this->name = "server_request." + this->address->address();
  }
  flags |= COMM_FLAG_SERVER;
  flags |= COMM_ALWAYS_SEND_HEADER;
}
int ServerComm::update_datatype(const rapidjson::Value& new_schema,
				const DIRECTION dir) {
  if (dir == RECV)
    return COMM_BASE::update_datatype(new_schema, dir);
  requests.addResponseSchema(new_schema);
  return 1;
}

bool ServerComm::create_header_send(Header& header, const char* data, const size_t &len) {
  bool out = COMM_BASE::create_header_send(header, data, len);
  if ((!out) || header.flags & HEAD_FLAG_EOF)
    return out;
  if (requests.addResponseServer(header, data, len) < 0) {
    ygglog_error << "create_header_send(" << name << "): Failed to add response" << std::endl;
    header.invalidate();
    return false;
  }
  // This gives the server access to the ID of the message last received
  // if (!header.SetMetaString("id", address)) {
  //   header.invalidate();
  //   return 0;
  // }
  return true;
}

bool ServerComm::create_header_recv(Header& header, char*& data,
				    const size_t &len,
				    size_t msg_len, int allow_realloc,
				    int temp) {
  bool out = COMM_BASE::create_header_recv(header, data, len,
					   msg_len, allow_realloc, temp);
  if ((!out) || header.flags & HEAD_FLAG_EOF)
    return out;
  if (requests.addRequestServer(header) < 0) {
    ygglog_error << "create_header_recv(" << name << "): Failed to add request" << std::endl;
    header.invalidate();
    return false;
  }
  return true;
}

int ServerComm::send_single(const char* data, const size_t &len){
    ygglog_debug << "server_comm_send(" << name << "): " << len << " bytes";
    Comm_t* response_comm = requests.activeComm();
    if (response_comm == NULL) {
        ygglog_error << "server_comm_send(" << name << "): Failed to get response comm" << std::endl;
        return -1;
    }
    int ret = response_comm->send_single(data, len);
    ygglog_debug << "server_comm_send(" << name << "): Sent " << len << " bytes" << std::endl;
    if (requests.popRequestServer() < 0)
        return -1;
    return ret;
}


#endif
