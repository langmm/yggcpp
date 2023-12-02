#include "ClientComm.hpp"
#include "utils/tools.hpp"

#ifdef COMM_BASE

using namespace communication::communicator;
using namespace communication::utils;

unsigned ClientComm::_client_rand_seeded = 0;

ClientComm::ClientComm(const std::string nme, const Address& addr,
		       int flgs, const COMM_TYPE type,
		       const COMM_TYPE reqtype,
		       const COMM_TYPE restype) :
  RPCComm(nme, addr,
	  flgs | COMM_FLAG_CLIENT | COMM_ALWAYS_SEND_HEADER,
	  SEND, RECV, type, reqtype, restype) {
  // Called to create temp comm for send/recv
  if (!(global_comm || (name.empty() && address.valid())))
    init();
}

ADD_CONSTRUCTORS_RPC_DEF(ClientComm)

std::string ClientComm::logClass() const {
  std::string out = "ClientComm";
  return out;
}

int ClientComm::comm_nmsg(DIRECTION dir) const {
  int out = RPCComm::comm_nmsg(dir);
  if (dir == RECV) {
    std::string req_id = requests.activeRequestClient();
    if ((!req_id.empty()) && requests.isComplete(req_id))
      out++;
  }
  return out;
}

void ClientComm::set_timeout_recv(int64_t new_timeout) {
  if (global_comm) {
    global_comm->set_timeout_recv(new_timeout);
    return;
  }
  WrapComm::set_timeout_recv(new_timeout);
  requests.initClientResponse();
  Comm_t* active_comm = requests.comms[0];
  active_comm->set_timeout_recv(new_timeout);
}
int64_t ClientComm::get_timeout_recv() {
  requests.initClientResponse();
  Comm_t* active_comm = requests.comms[0];
  return active_comm->get_timeout_recv();
}

void ClientComm::init() {
  YGG_THREAD_SAFE_BEGIN(client) {
    if (!(_client_rand_seeded)) {
      srand(ptr2seed(this));
      _client_rand_seeded = 1;
    }
  } YGG_THREAD_SAFE_END;
  if (name.empty()) {
    this->name = "client_request." + this->address.address();
  }
}

bool ClientComm::send_signon(int nloop, int interval,
			     Comm_t* async_comm) {
  if (global_comm)
    return dynamic_cast<ClientComm*>(global_comm)->send_signon(nloop, interval, async_comm);
  if (requests.signon_complete)
    return true;
  if (requests.initClientResponse() < 0)
    return false;
  if (!async_comm) {
    if (flags & COMM_FLAG_ASYNC_WRAPPED)
      return true;
    async_comm = this;
  }
  if (((nloop + (int)(requests.signonSent())) % interval) == 0) {
    log_debug() << "send_signon: Sending signon" << std::endl;
    if (async_comm->send_raw(YGG_CLIENT_SIGNON, YGG_CLIENT_SIGNON_LEN) < 0) {
      log_error() << "send_signon: Error in sending sign-on" << std::endl;
      return false;
    }
  }
  return true;
}

bool ClientComm::signon() {
  if (global_comm)
    return dynamic_cast<ClientComm*>(global_comm)->signon();
  if (!send_signon(0)) // Will only send if no signon has been sent
    return false;
  if (requests.signon_complete)
    return true;
  Header tmp(true);
  tmp.flags |= HEAD_FLAG_SERVER_SIGNON;
  long ret = recv_single(tmp);
  if (ret < 0 || !(tmp.flags & HEAD_FLAG_SERVER_SIGNON)) {
    log_error() << "signon: Error in receiving sign-on" << std::endl;
    return false;
  } else if (ret > 0) {
    setFlags(tmp, RECV);
    log_debug() << "signon: Received response to signon" << std::endl;
  }
  return requests.signon_complete;
}

Comm_t* ClientComm::create_worker_send(Header& head) {
  // Should never be called with global comm
  // if (global_comm)
  //   return global_comm->create_worker_send(head);
  assert(!global_comm);
  log_debug() << "create_worker_send: begin" << std::endl;
  Comm_t* out = WrapComm::create_worker_send(head);
  // create_worker_send only called after create_header_send ensuring
  //   request_id is present
  std::string request_id;
  if (!head.GetMetaString("request_id", request_id))
    return nullptr;
  if (!getWorkers().setRequest(out, request_id)) {
    log_error() << "create_worker_send: Failed to set request on worker" << std::endl;
    return nullptr;
  }
  log_debug() << "create_worker_send: done" << std::endl;
  return out;
}

Comm_t* ClientComm::create_worker_recv(Header& head) {
  // Should never be called with global comm
  // if (global_comm)
  //   return global_comm->create_worker_recv(head);
  assert(!global_comm);
  log_debug() << "create_worker_recv: begin" << std::endl;
  // create_worker_recv only called after create_header_recv, ensuring
  //   request_id is present
  std::string request_id;
  if (!head.GetMetaString("request_id", request_id))
    return nullptr;
  if (!getWorkers().setResponse(request_id)) {
    log_error() << "create_worker_recv: Failed to clear request on worker (request_id = " << request_id << ")" << std::endl;
    return nullptr;
  }
  Comm_t* out = WrapComm::create_worker_recv(head);
  log_debug() << "create_worker_recv: done" << std::endl;
  return out;
}

bool ClientComm::create_header_send(Header& header) {
  if (global_comm)
    return global_comm->create_header_send(header);
  log_debug() << "create_header_send: begin" << std::endl;
  bool out = WrapComm::create_header_send(header);
  if (out && !(header.flags & HEAD_FLAG_EOF)) {
    if (!((header.flags & HEAD_FLAG_CLIENT_SIGNON) ||
	  (flags & COMM_FLAG_ASYNC_WRAPPED)))
      out = signon();
    if (out)
      out = (requests.addRequestClient(header) >= 0);
  }
  if (out)
    log_debug() << "create_header_send: done" << std::endl;
  return out;
}

long ClientComm::recv_single(utils::Header& header) {
    assert(!global_comm);
    bool in_signon = (header.flags & HEAD_FLAG_SERVER_SIGNON);
    log_debug() << "recv_single" <<
      " (signon = " << in_signon << ")" << std::endl;
    Comm_t* response_comm = requests.activeComm();
    if (response_comm == NULL) {
      log_error() << "recv_single: Error getting response comm" <<
	" (signon = " << in_signon << ")" << std::endl;
      return -1;
    }
    std::string req_id = requests.activeRequestClient();
    if (req_id.empty()) {
      log_error() << "recv_single: No active request" << std::endl;
      return -1;
    }
    long ret;
    utils::Header response_header;
    int64_t tout = get_timeout_recv();
    int nmsg = 0;
    TIMEOUT_LOOP(tout, tout / 10) {
      log_info() << "recv_single: req " << req_id << " complete = " <<
	requests.isComplete(req_id) << std::endl;
      if (requests.isComplete(req_id))
	break;
      if (in_signon && !send_signon(istep))
	return -1;
      response_header.reset(HEAD_RESET_OWN_DATA);
      log_debug() << "recv_single: Waiting for response to request " <<
	req_id << " (signon = " << in_signon << ")" << std::endl;
      nmsg = response_comm->wait_for_recv(tout / 10);
      if (nmsg > 0) {
	  ret = response_comm->recv_single(response_header);
	  if (ret < 0) {
            log_error() << "recv_single: response recv returned " <<
	      ret << " (signon = " << in_signon << ")" << std::endl;
            return ret;
	  }
	  if (!(response_header.flags & HEAD_FLAG_EOF)) {
	    if (requests.addResponseClient(response_header) < 0)
	      return -1;
	    if (!requests.transferSchemaFrom(response_comm))
	      return -1;
	  }
      } else if (nmsg < 0) {
	log_error() << "recv_single: Error during receive" <<
	  " (signon = " << in_signon << ")" << std::endl;
	break;
      } else {
	log_debug() << "recv_single: No response to oldest request (address = " << response_comm->address.address() << "), sleeping" <<
	  " (signon = " << in_signon << ")" << std::endl;
      }
    }
    // Close response comm and decrement count of response comms
    if (in_signon) {
      if (!requests.isComplete(req_id))
	return 0;
      header.flags |= ~HEAD_FLAG_SERVER_SIGNON;
    }
    ret = requests.getRequestClient(req_id, header, true);
    log_debug() << "recv_single: getRequestClient returned " << ret <<
      " (signon = " << in_signon << ")" << std::endl;
    if (header.flags & HEAD_FLAG_SERVER_SIGNON) {
      header.flags |= HEAD_FLAG_REPEAT;
    }
    return ret;
}

#endif
