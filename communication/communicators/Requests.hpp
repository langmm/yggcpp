#pragma once

#include <vector>
#include "DefaultComm.hpp"
#include "CommBase.hpp"

#ifdef COMM_BASE
namespace communication {
namespace communicator {
  class Request {
  public:
    Request() :
      response_id(), request_id(), data(), comm_idx(0), complete(false) {}
    int setData(const char* str, size_t len) {
      if (complete) {
	ygglog_error << "setData: request already complete" << std::endl;
	return -1;
      }
      data.assign(str, len);
      complete = true;
      return 0;
    }
    std::string response_id;
    std::string request_id;
    std::string data;
    size_t comm_idx;
    bool complete;
  };

  class RequestList {
  public:
    RequestList(DIRECTION dir) :
      comms(), requests(), response_dir(dir), response_metadata(),
      active_comm(-1) {}
    ~RequestList() {
      for (size_t i = 0; i < comms.size(); i++) {
	if (comms[i] != NULL) {
	  delete comms[i];
	  comms[i] = NULL;
	}
      }
    }
    int hasRequest(const std::string& request_id) const {
      for (size_t i = 0; i < requests.size(); i++) {
	if (requests[i].request_id == request_id)
	  return (int)i;
      }
      return -1;
    }
    int hasResponse(const std::string& response_id) const {
      for (size_t i = 0; i < requests.size(); i++) {
	if (requests[i].response_id == response_id)
	  return (int)i;
      }
      return -1;
    }
    int hasComm(const std::string& response_address) const {
      for (size_t i = 0; i < comms.size(); i++) {
	if (comms[i] and *(comms[i]->address) == response_address)
	  return (int)i;
      }
      return -1;
    }
    int addRequestClient(Header& header) {
      ygglog_debug << "addRequestClient: begin" << std::endl;
      if (comms.size() == 0) {
	if (addComm() < 0) {
	  ygglog_error << "addRequestClient: Error creating response comm." << std::endl;
	  return -1;
	}
      }
      std::string request_id;
      if (!header.SetMetaID("request_id", request_id)) {
	ygglog_error << "addRequestClient: Error adding request_id" << std::endl;
	return -1;
      }
      ygglog_debug << "addRequestClient: request_id = " << request_id << std::endl;
      if (hasRequest(request_id) >= 0) {
	ygglog_error << "addRequestClient: Client already has request with id '" << request_id << "'" << std::endl;
	return -1;
      }
      if (!header.SetMetaString("response_address", comms[0]->address->address())) {
	ygglog_error << "addRequestClient: Error adding response_address" << std::endl;
	return -1;
      }
      size_t idx = requests.size();
      requests.resize(requests.size() + 1);
      requests[idx].request_id = request_id;
      requests[idx].comm_idx = 0;
      ygglog_debug << "addRequestClient: response_address = " << comms[0]->address << ", request_id = " << request_id << std::endl;
      return static_cast<int>(idx);
    }
    int addRequestServer(Header& header) {
      ygglog_debug << "addRequestServer: begin" << std::endl;
      std::string request_id = header.GetMetaString("request_id");
      std::string response_address = header.GetMetaString("response_address");
      int comm_idx = hasComm(response_address);
      if (comm_idx < 0) {
	comm_idx = addComm(response_address);
	if (comm_idx < 0) {
	  ygglog_error << "addRequestServer: Error creating response comm" << std::endl;
	  return -1;
	}
      }
      std::string response_id = request_id;
      while (hasResponse(response_id) >= 0) {
	response_id += std::to_string(rand());
      }
      ygglog_debug << "addRequestServer: Response id = " << response_id << std::endl;
      size_t idx = requests.size();
      requests.resize(requests.size() + 1);
      requests[idx].request_id = request_id;
      requests[idx].response_id = response_id;
      requests[idx].comm_idx = static_cast<size_t>(comm_idx);
      return static_cast<int>(idx);
    }
    int addResponseServer(Header& header, const char* data, const size_t len) {
      if (requests.size() == 0) {
	ygglog_error << "addResponseServer: Server does not have any unprocessed requests" << std::endl;
	return -1;
      }
      if (!header.SetMetaString("request_id", requests[0].request_id)) {
	ygglog_error << "addResponseServer: Error seting request_id" << std::endl;
	return -1;
      }
      if (requests[0].setData(data, len) < 0) {
	ygglog_error << "addResponseServer: Error setting data" << std::endl;
	return -1;
      }
      active_comm = 0;
      return 0;
    }
    int addResponseClient(Header& header, const char* data, const size_t len) {
      ygglog_debug << "addResponseClient: begin" << std::endl;
      std::string request_id = header.GetMetaString("request_id");
      int idx = hasRequest(request_id);
      if (idx < 0) {
	ygglog_error << "addResponseClient: Client does not have a request with id '" << request_id << "'" << std::endl;
	return -1;
      }
      if (requests[static_cast<size_t>(idx)].setData(data, len) < 0) {
	ygglog_error << "addResponseClient: Error setting data" << std::endl;
	return -1;
      }
      return 0;
    }
    Comm_t* getComm(const std::string& request_id) {
      int idx = hasRequest(request_id);
      if (idx < 0)
	return NULL;
      return comms[static_cast<size_t>(idx)];
    }
    Comm_t* activeComm() {
      if (requests.empty() || comms.empty()) {
	ygglog_error << "activeComm: No pending requests" << std::endl;
	return NULL;
      }
      return comms[requests[0].comm_idx];
    }
    int popRequestServer() {
      if (requests.empty() || comms.empty()) {
	ygglog_error << "popRequest: No pending requests" << std::endl;
	return -1;
      }
      requests.erase(requests.begin());
      return 1;
    }
    int popRequestClient(const std::string& request_id,
			 char*& data, const size_t len, bool allow_realloc) {
      int idx = hasRequest(request_id);
      if (idx < 0) {
	ygglog_error << "RequestList::popRequestClient: No pending request with id '" << request_id << "'" << std::endl;
	return -1;
      }
      if (!requests[(size_t)idx].complete) {
	ygglog_error << "RequestList::popRequestClient: Request '" << request_id << "' does not have response" << std::endl;
	return -1;
      }
      size_t ret = requests[(size_t)idx].data.size();
      if ((requests[(size_t)idx].data.size() + 1) > len) {
	if (!allow_realloc) {
	  ygglog_error << "RequestList::popRequestClient: Response exceeds size of provided buffer" << std::endl;
	  return -1;
	}
	data = (char*)realloc(data, requests[(size_t)idx].data.size() + 1);
	if (data == NULL) {
	  ygglog_error << "RequestList::popRequestClient: Error reallocating buffer" << std::endl;
	  return -1;
	}
	memcpy(data, requests[(size_t)idx].data.c_str(), requests[(size_t)idx].data.size() + 1);
	data[requests[(size_t)idx].data.size()] = '\0';
      }
      requests.erase(requests.begin());
      return static_cast<int>(ret);
    }
    int addComm(std::string response_address = "") {
      int idx = 0;
      if (!response_address.empty()) {
	idx = hasComm(response_address);
	if (idx >= 0)
	  return idx;
      }
      utils::Address* response_adr = new utils::Address(response_address);
      Comm_t* x = new COMM_BASE("", response_adr, response_dir);
      x->addSchema(response_metadata);
      x->flags |= COMM_EOF_SENT | COMM_EOF_RECV;
      if (response_dir == RECV)
	x->flags |= COMM_FLAG_CLIENT_RESPONSE;
      else
	x->flags |= COMM_ALLOW_MULTIPLE_COMMS;
      comms.push_back(x);
      idx = (int)(comms.size() - 1);
      return idx;
    }
    bool isComplete(const std::string& request_id) const {
      int idx = hasRequest(request_id);
      if (idx < 0)
	return false;
      return requests[(size_t)idx].complete;
    }
    void addResponseSchema(const rapidjson::Value& s) {
      response_metadata.fromSchema(s);
    }
    void addResponseFormat(const std::string& format_str) {
      response_metadata.fromFormat(format_str);
    }
    std::vector<Comm_t*> comms;
    std::vector<Request> requests;
    DIRECTION response_dir;
    Metadata response_metadata;
    int active_comm;
  };
}
}
#endif // COMM_BASE
