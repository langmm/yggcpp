#include "ClientComm.hpp"
#include "utils/tools.hpp"

#ifdef COMM_BASE

using namespace communication::communicator;
using namespace communication::utils;

unsigned ClientComm::_client_rand_seeded = 0;

ClientComm::ClientComm(const std::string &name, Address *address,
		       int flgs) :
  COMM_BASE(name, address, SEND,
	    flgs | COMM_FLAG_CLIENT | COMM_ALWAYS_SEND_HEADER),
  requests(RECV) {
  // Called to create temp comm for send/recv
  if (name.empty() && address != nullptr && address->valid())
      return;
  init();
}

ClientComm::ClientComm(const std::string name, int flgs) :
  COMM_BASE(name, SEND,
	    flgs | COMM_FLAG_CLIENT | COMM_ALWAYS_SEND_HEADER),
  requests(RECV) {
  init();
}

void ClientComm::init() {
#ifdef _OPENMP
#pragma omp critical (client)
    {
#endif
        if (!(_client_rand_seeded)) {
            srand(ptr2seed(this));
            _client_rand_seeded = 1;
        }
#ifdef _OPENMP
    }
#endif
    if (name.empty()) {
        this->name = "client_request." + this->address->address();
    }
}

// int ClientComm::wait_for_server() {
//   if (initClientResponse() < 0)
//     return -1;
//   while (!server_listening) {
    
//   }
// }

int ClientComm::update_datatype(const rapidjson::Value& new_schema,
				const DIRECTION dir) {
  if (dir == SEND)
    return COMM_BASE::update_datatype(new_schema, dir);
  requests.addResponseSchema(new_schema);
  return 1;
}

bool ClientComm::create_header_send(Header& header, const char* data, const size_t &len) {
  ygglog_debug << "ClientComm(" << name << ")::create_header_send: begin" << std::endl;
  bool out = COMM_BASE::create_header_send(header, data, len);
  if ((!out) || header.flags & HEAD_FLAG_EOF)
    return out;
  if (requests.addRequestClient(header) < 0) {
    ygglog_error << "ClientComm::create_header_send(" << name << "): Failed to add request" << std::endl;
    header.invalidate();
    return false;
  }
  ygglog_debug << "ClientComm(" << name << ")::create_header_send: done" << std::endl;
  return true;
}

bool ClientComm::create_header_recv(Header& header, char*& data, const size_t &len,
				    size_t msg_len, int allow_realloc,
				    int temp) {
  ygglog_debug << "ClientComm(" << name << ")::create_header_recv: begin" << std::endl;
  bool out = COMM_BASE::create_header_recv(header, data, len, msg_len,
					   allow_realloc, temp);
  if ((!out) || header.flags & HEAD_FLAG_EOF)
    return out;
  if (temp) {
    // Only add response the first time as the data returned may be from
    // a cached response
    if (requests.addResponseClient(header, data, len) < 0) {
      ygglog_error << "ClientComm::create_header_recv(" << name << "): Failed to add response" << std::endl;
      header.invalidate();
      return false;
    }
  }
  return true;
}

long ClientComm::recv_single(char*& rdata, const size_t &rlen, bool allow_realloc)  {
    ygglog_debug << "ClientComm::recv_single(" << name << ")" << std::endl;
    Comm_t* response_comm = requests.activeComm();
    if (response_comm == NULL) {
      ygglog_error << "ClientComm::recv_single(" << name << "): Error getting response comm" << std::endl;
      return -1;
    }
    std::string req_id = requests.requests[0].request_id;
    size_t buff_len = rlen;
    long ret = 0;
    while (!requests.isComplete(req_id)) {
        ygglog_debug << "ClientComm::recv_single(" << name << "): Waiting for response to request " << req_id << std::endl;
        ret = response_comm->recv_single(rdata, buff_len, allow_realloc);
        if (ret < 0) {
	    ygglog_error << "ClientComm::recv_single(" << name << "): response recv_single returned " << ret << std::endl;
            return ret;
        }
	if (ret > (int)buff_len) {
	  buff_len = ret;
	}
	Header header;
	if (!create_header_recv(header, rdata, buff_len, ret, allow_realloc, true)) {
	    ygglog_error << "ClientComm::recv_single(" << name << "): Invalid header." << std::endl;
            return -1;
        }
    }
    ret = requests.popRequestClient(req_id, rdata, rlen, allow_realloc);
    // Close response comm and decrement count of response comms
    ygglog_debug << "ClientComm::recv_single(" << name << "): client_pop_response returned " << ret << std::endl;;
    return ret;
}

#endif
