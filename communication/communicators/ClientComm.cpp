#include "ClientComm.hpp"
#include "utils/tools.hpp"

#ifdef COMM_BASE

using namespace communication::communicator;
using namespace communication::utils;

unsigned ClientComm::_client_rand_seeded = 0;

ClientComm::ClientComm(const std::string nme, Address *addr,
		       int flgs, const COMM_TYPE type) :
  RPCComm(nme, addr,
	  flgs | COMM_FLAG_CLIENT | COMM_ALWAYS_SEND_HEADER,
	  SEND, RECV, type) {
  // Called to create temp comm for send/recv
  if (!(global_comm || (name.empty() && address && address->valid())))
    init();
}

ADD_CONSTRUCTORS_RPC_DEF(ClientComm)

void ClientComm::set_timeout_recv(int new_timeout) {
  if (global_comm) {
    global_comm->set_timeout_recv(new_timeout);
    return;
  }
  COMM_BASE::set_timeout_recv(new_timeout);
  requests.initClientResponse();
  Comm_t* active_comm = requests.comms[0];
  active_comm->set_timeout_recv(new_timeout);
}
int ClientComm::get_timeout_recv() {
  requests.initClientResponse();
  Comm_t* active_comm = requests.comms[0];
  return active_comm->get_timeout_recv();
}
int ClientComm::wait_for_recv(const int&) {
  // Handle wait in recv_single for response comm
  return 1;
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

bool ClientComm::signon(const Header& header) {
  if (global_comm)
    return dynamic_cast<ClientComm*>(global_comm)->signon(header);
  if (header.flags & HEAD_FLAG_CLIENT_SIGNON)
    return true;
  if (requests.initClientResponse() < 0)
    return false;
  ygglog_debug << "ClientComm(" << name << ")::signon: begin" << std::endl;
  while (!requests.signon_complete) {
#ifdef YGG_TEST
    // Prevent sending extra SIGNON during testing
    if (!requests.signonSent()) {
#endif // YGG_TEST
    if (send(YGG_CLIENT_SIGNON, YGG_CLIENT_SIGNON_LEN) < 0) {
      ygglog_error << "ClientComm(" << name << ")::signon: Error in sending sign-on" << std::endl;
      return false;
    }
#ifdef YGG_TEST
    }
#endif // YGG_TEST
    if (requests.activeComm()->comm_nmsg() > 0) {
      char* data = NULL;
      int ret = recv(data, 0, true);
      if (data != NULL)
	free(data);
      if (ret < 0) {
        ygglog_error << "ClientComm(" << name << ")::signon: Error in receiving sign-on" << std::endl;
	return false;
      }
      break;
    } else {
      sleep(YGG_SLEEP_TIME);
    }
  }
  return requests.signon_complete;
}

Comm_t* ClientComm::create_worker_send(Header& head) {
  // Should never be called with global comm
  // if (global_comm)
  //   return global_comm->create_worker_send(head);
  assert(!global_comm);
  ygglog_debug << "ClientComm(" << name << ")::create_worker_send: begin" << std::endl;
  Comm_t* out = COMM_BASE::create_worker_send(head);
  // create_worker_send only called after create_header_send ensuring
  //   request_id is present
  std::string request_id(head.GetMetaString("request_id"));
  if (!workers.setRequest(out, request_id)) {
    ygglog_error << "ClientComm(" << name << ")::create_worker_send: Failed to set request on worker" << std::endl;
    return nullptr;
  }
  ygglog_debug << "ClientComm(" << name << ")::create_worker_send: done" << std::endl;
  return out;
}

Comm_t* ClientComm::create_worker_recv(Header& head) {
  // Should never be called with global comm
  // if (global_comm)
  //   return global_comm->create_worker_recv(head);
  assert(!global_comm);
  ygglog_debug << "ClientComm(" << name << ")::create_worker_recv: begin" << std::endl;
  // create_worker_recv only called after create_header_recv, ensuring
  //   request_id is present
  std::string request_id(head.GetMetaString("request_id"));
  if (!workers.setResponse(request_id)) {
    ygglog_error << "ClientComm(" << name << ")::create_worker_recv: Failed to clear request on worker (request_id = " << request_id << ")" << std::endl;
    return nullptr;
  }
  Comm_t* out = COMM_BASE::create_worker_recv(head);
  ygglog_debug << "ClientComm(" << name << ")::create_worker_recv: done" << std::endl;
  return out;
}

bool ClientComm::create_header_send(Header& header, const char* data, const size_t &len) {
  if (global_comm)
    return global_comm->create_header_send(header, data, len);
  ygglog_debug << "ClientComm(" << name << ")::create_header_send: begin" << std::endl;
  bool out = COMM_BASE::create_header_send(header, data, len);
  if (out && !(header.flags & HEAD_FLAG_EOF)) {
    out = signon(header);
    if (out)
      out = (requests.addRequestClient(header) >= 0);
  }
  if (out)
    ygglog_debug << "ClientComm(" << name << ")::create_header_send: done" << std::endl;
  return out;
}

bool ClientComm::create_header_recv(Header& header, char*& data, const size_t &len,
				    size_t msg_len, int allow_realloc,
				    int temp) {
  // Should never be called with global comm
  // if (global_comm)
  //   return global_comm->create_header_recv(header, data, len, msg_len,
  // 					   allow_realloc, temp);
  assert(!global_comm);
  ygglog_debug << "ClientComm(" << name << ")::create_header_recv: begin (temp = " << temp << ")" << std::endl;
  Comm_t* response_comm = requests.activeComm();
  // create_header_recv only called after request confirmed
  assert(response_comm);
  // if (response_comm == NULL) {
  //   ygglog_error << "ClientComm(" << name << ")::create_header_recv: Error getting response comm" << std::endl;
  //   return false;
  // }
  bool out = response_comm->create_header_recv(header, data, len, msg_len,
					       allow_realloc, temp);
  if (out && !(header.flags & HEAD_FLAG_EOF)) {
    if (temp) {
      // Only add response the first time as the data returned may be
      // from a cached response
      out = (requests.addResponseClient(header, data, len) >= 0);
      requests.transferSchemaFrom(response_comm);
    } else {
      out = (requests.popRequestClient(header) >= 0);
    }
  }
  return out;
}

long ClientComm::recv_single(char*& rdata, const size_t &rlen, bool allow_realloc)  {
    // Should never be called with global comm
    // if (global_comm)
    //   return global_comm->recv_single(rdata, rlen, allow_realloc);
    assert(!global_comm);
    ygglog_debug << "ClientComm(" << name << ")::recv_single" << std::endl;
    Comm_t* response_comm = requests.activeComm();
    if (response_comm == NULL) {
        ygglog_error << "ClientComm(" << name << ")::recv_single: Error getting response comm" << std::endl;
        return -1;
    }
    std::string req_id = requests.activeRequestClient();
    size_t buff_len = rlen;
    long ret;
    while (!requests.isComplete(req_id)) {
        ygglog_debug << "ClientComm(" << name << ")::recv_single: Waiting for response to request " << req_id << std::endl;
	if (response_comm->wait_for_recv(this->timeout_recv) <= 0) {
	  ygglog_debug << "ClientComm(" << name << ")::recv_single: No messages waiting" << std::endl;
	  return -1;
	}
        ret = response_comm->recv_single(rdata, buff_len, allow_realloc);
        if (ret < 0) {
            ygglog_error << "ClientComm(" << name << ")::recv_single: response recv_single returned " << ret << std::endl;
            return ret;
        }
        if (ret > (int)buff_len) {
            buff_len = ret;
        }
        Header header;
        if (!create_header_recv(header, rdata, buff_len, ret, allow_realloc, true)) {
            ygglog_error << "ClientComm(" << name << ")::recv_single: Invalid header." << std::endl;
            return -1;
        }
    }
    ret = requests.getRequestClient(req_id, rdata, rlen, allow_realloc);
    // Close response comm and decrement count of response comms
    ygglog_debug << "ClientComm(" << name << ")::recv_single: client_pop_response returned " << ret << std::endl;;
    return ret;
}

#endif
