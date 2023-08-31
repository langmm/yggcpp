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

void ClientComm::init() {
  YGG_THREAD_SAFE_BEGIN(client) {
    if (!(_client_rand_seeded)) {
      srand(ptr2seed(this));
      _client_rand_seeded = 1;
    }
  } YGG_THREAD_SAFE_END;
  if (name.empty()) {
    this->name = "client_request." + this->address->address();
  }
}

bool ClientComm::signon(const Header& header, Comm_t* async_comm) {
  if (global_comm)
    return dynamic_cast<ClientComm*>(global_comm)->signon(header);
  if (header.flags & (HEAD_FLAG_CLIENT_SIGNON | HEAD_FLAG_EOF))
    return true;
  if (requests.initClientResponse() < 0)
    return false;
  if (!async_comm) {
    if (flags & COMM_FLAG_ASYNC_WRAPPED)
      return true;
    async_comm = this;
  }
  ygglog_debug << "ClientComm(" << name << ")::signon: begin" << std::endl;
  Header tmp(true);
  int tout = get_timeout_recv();
  int nloop = 0;
  if (requests.signonSent())
    nloop = 1;
  clock_t start = clock();
  while ((!requests.signon_complete) &&
	 (tout < 0 ||
	  (((double)(clock() - start))*1000000/CLOCKS_PER_SEC) < tout)) {
    if ((nloop % 3) == 0) {
      ygglog_debug << "ClientComm(" << name << ")::signon: Sending signon" << std::endl;
      if (async_comm->send(YGG_CLIENT_SIGNON, YGG_CLIENT_SIGNON_LEN) < 0) {
	ygglog_error << "ClientComm(" << name << ")::signon: Error in sending sign-on" << std::endl;
	return false;
      }
    }
    nloop++;
    if ((flags & COMM_FLAG_ASYNC_WRAPPED) && (async_comm != this))
      return true;
    if (requests.activeComm()->comm_nmsg(RECV) > 0) {
      long ret = recv_single(tmp);
      if (ret < 0 || !(tmp.flags & HEAD_FLAG_SERVER_SIGNON)) {
        ygglog_error << "ClientComm(" << name << ")::signon: Error in receiving sign-on" << std::endl;
	return false;
      }
      setFlags(tmp, RECV);
      tmp.reset(HEAD_RESET_KEEP_BUFFER);
      ygglog_debug << "ClientComm(" << name << ")::signon: Received response to signon" << std::endl;
      break;
    } else {
      ygglog_debug << "ClientComm(" << name << ")::signon: No response to signon (address = " << requests.activeComm()->address->address() << "), sleeping" << std::endl;
      // Sleep outside lock on async
      if (flags & COMM_FLAG_ASYNC_WRAPPED)
	return true;
      std::this_thread::sleep_for(std::chrono::microseconds(YGG_SLEEP_TIME));
    }
  }
  if (!requests.signon_complete) {
    ygglog_error << "ClientComm(" << name << ")::signon: No response to signon" << std::endl;
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

bool ClientComm::create_header_send(Header& header) {
  if (global_comm)
    return global_comm->create_header_send(header);
  ygglog_debug << "ClientComm(" << name << ")::create_header_send: begin" << std::endl;
  bool out = COMM_BASE::create_header_send(header);
  if (out && !(header.flags & HEAD_FLAG_EOF)) {
    out = signon(header);
    if (out)
      out = (requests.addRequestClient(header) >= 0);
  }
  if (out)
    ygglog_debug << "ClientComm(" << name << ")::create_header_send: done" << std::endl;
  return out;
}

long ClientComm::recv_single(utils::Header& header) {
    assert(!global_comm);
    ygglog_debug << "ClientComm(" << name << ")::recv_single" << std::endl;
    Comm_t* response_comm = requests.activeComm();
    if (response_comm == NULL) {
        ygglog_error << "ClientComm(" << name << ")::recv_single: Error getting response comm" << std::endl;
        return -1;
    }
    std::string req_id = requests.activeRequestClient();
    long ret;
    utils::Header response_header;
    clock_t start = clock();
    int tout = get_timeout_recv();
    while ((!requests.isComplete(req_id)) &&
	   (tout < 0 ||
	    (((double)(clock() - start))*1000000/CLOCKS_PER_SEC) < tout)) {
        response_header.reset(HEAD_RESET_OWN_DATA);
        ygglog_debug << "ClientComm(" << name << ")::recv_single: Waiting for response to request " << req_id << std::endl;
	if (response_comm->comm_nmsg(RECV) > 0) {
	  ret = response_comm->recv_single(response_header);
	  if (ret < 0) {
            ygglog_error << "ClientComm(" << name << ")::recv_single: response recv_single returned " << ret << std::endl;
            return ret;
	  }
	  if (!(response_header.flags & HEAD_FLAG_EOF)) {
	    if (requests.addResponseClient(response_header) < 0)
	      return -1;
	    requests.transferSchemaFrom(response_comm);
	  }
	} else {
	  ygglog_debug << "ClientComm(" << name << ")::recv_single: No response to oldest request (address = " << response_comm->address->address() << "), sleeping" << std::endl;
	  std::this_thread::sleep_for(std::chrono::microseconds(YGG_SLEEP_TIME));
	  
	}
    }
    // Close response comm and decrement count of response comms
    ret = requests.getRequestClient(req_id, header, true);
    ygglog_debug << "ClientComm(" << name << ")::recv_single: getRequestClient returned " << ret << std::endl;
    if (header.flags & HEAD_FLAG_SERVER_SIGNON) {
      header.flags |= HEAD_FLAG_REPEAT;
    }
    return ret;
}

#endif
