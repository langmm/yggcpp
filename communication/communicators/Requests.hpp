#pragma once

#include <vector>
#include "DefaultComm.hpp"
#include "CommBase.hpp"

#ifdef COMM_BASE
namespace communication {
namespace communicator {
  class Partner {
  public:
    Partner() : model(), signed_off(false) {}
    Partner(const std::string adr) : model(adr), signed_off(false) {}
    std::string model;
    bool signed_off;
  };
  class Request {
  public:
    Request() :
      request_id(), data(), comm_idx(0), complete(false),
      is_signon(false) {}
    Request(const std::string& req_id,
	    const size_t cidx, bool is_son = false) :
      request_id(req_id), data(), comm_idx(cidx),
      complete(false), is_signon(is_son) {}
    int setData(const char* str, size_t len) {
      if (complete) {
	ygglog_error << "setData: request already complete" << std::endl;
	return -1;
      }
      data.assign(str, len);
      complete = true;
      return 0;
    }
    std::string request_id;
    std::string data;
    size_t comm_idx;
    bool complete;
    bool is_signon;
  };

  class RequestList {
  public:
    RequestList(DIRECTION dir) :
      comms(), requests(), partners(), response_dir(dir),
      response_metadata(), signon_complete(false),
      stashed_request() {}
    ~RequestList() {
      destroy();
    }
    void destroy() {
      for (size_t i = 0; i < comms.size(); i++) {
	if (comms[i] != NULL) {
	  delete comms[i];
	  comms[i] = NULL;
	}
      }
      comms.resize(0);
    }
    int hasRequest(const std::string& request_id) const {
      for (size_t i = 0; i < requests.size(); i++) {
	if (requests[i].request_id == request_id)
	  return (int)i;
      }
      return -1;
    }
    int hasPartner(const std::string& model) const {
      for (size_t i = 0; i < partners.size(); i++) {
	if (partners[i].model == model)
	  return (int)i;
      }
      return -1;
    }
    int hasComm(const std::string& response_address) const {
      for (size_t i = 0; i < comms.size(); i++) {
	if (comms[i] && *(comms[i]->address) == response_address)
	  return (int)i;
      }
      return -1;
    }
    int initClientResponse() {
      if (comms.size() == 0) {
	addComm();
      }
      return 0;
    }
    int stashRequest() {
      if (!requests.empty()) {
	stashed_request = requests[0].request_id;
	requests.erase(requests.begin());
      }
      return 1;
    }
    int addRequestClient(Header& header, std::string request_id="") {
      ygglog_debug << "addRequestClient: begin" << std::endl;
      initClientResponse();
      bool is_signon = (header.flags & HEAD_FLAG_CLIENT_SIGNON);
      int existing_idx = -1;
      if (is_signon) {
	for (size_t i = 0; i < requests.size(); i++) {
	  if (requests[i].is_signon) {
	    request_id = requests[i].request_id;
	    existing_idx = static_cast<int>(i);
	    break;
	  }
	}
      }
      if (request_id.empty()) {
	if (!stashed_request.empty()) {
	  request_id = stashed_request;
	  stashed_request.clear();
	  header.SetMetaString("request_id", request_id);
	} else {
	  header.SetMetaID("request_id", request_id);
	}
	ygglog_debug << "addRequestClient: request_id = "
		     << request_id << std::endl;
      } else {
	header.SetMetaString("request_id", request_id);
      }
      header.SetMetaString("response_address",
			   comms[0]->address->address());
      if (existing_idx < 0) {
	size_t idx = requests.size();
	if (hasRequest(request_id) >= 0) {
	  ygglog_error
	    << "addRequestClient: Client already has request with id '"
	    << request_id << "'" << std::endl;
	  return -1;
	}
	requests.emplace_back(request_id, 0, is_signon);
	// requests.resize(requests.size() + 1);
	// requests[idx].request_id = request_id;
	// requests[idx].comm_idx = 0;
	// requests[idx].is_signon = is_signon;
	existing_idx = static_cast<int>(idx);
      }
      ygglog_debug << "addRequestClient: done response_address = "
		   << comms[0]->address->address() << ", request_id = "
		   << request_id << std::endl;
      return existing_idx;
    }
    int addRequestServer(Header& header) {
      ygglog_debug << "addRequestServer: begin" << std::endl;
      std::string request_id(header.GetMetaString("request_id"));
      std::string response_address(header.GetMetaString("response_address"));
      std::string partner_model(header.GetMetaString("model"));
      int comm_idx = addComm(response_address);
      size_t idx = requests.size();
      if (hasPartner(partner_model) < 0)
	partners.emplace_back(partner_model);
      requests.emplace_back(request_id,
			    static_cast<size_t>(comm_idx),
			    (header.flags & HEAD_FLAG_CLIENT_SIGNON));
      ygglog_debug << "addRequestServer: done idx = " << idx
		   << ", response_address = "
		   << comms[requests[idx].comm_idx]->address->address()
		   << ", request_id = " << requests[idx].request_id
		   << std::endl;
      return static_cast<int>(idx);
    }
    int addResponseServer(Header& header, const char* data, const size_t len) {
      ygglog_debug << "addResponseServer: begin" << std::endl;
      if (requests.size() == 0) {
	ygglog_error << "addResponseServer: Server does not have any unprocessed requests" << std::endl;
	return -1;
      }
      ygglog_debug << "addResponseServer: request_id = " << requests[0].request_id << std::endl;
      header.initMeta();
      header.SetMetaString("request_id", requests[0].request_id);
      if (requests[0].setData(data, len) < 0) {
	ygglog_error << "addResponseServer: Error setting data" << std::endl;
	return -1;
      }
      if (header.flags & HEAD_FLAG_SERVER_SIGNON)
	signon_complete = true;
      ygglog_debug << "addResponseServer: done (signon_complete = " <<
	signon_complete << ")" << std::endl;
      return 0;
    }
    int addResponseClient(Header& header, const char* data, const size_t len) {
      ygglog_debug << "addResponseClient: begin" << std::endl;
      std::string request_id(header.GetMetaString("request_id"));
      std::string partner_model(header.GetMetaString("model"));
      int idx = hasRequest(request_id);
      if (idx < 0) {
	ygglog_error << "addResponseClient: Client does not have a request with id '" << request_id << "'" << std::endl;
	return -1;
      }
      if (requests[static_cast<size_t>(idx)].setData(data, len) < 0) {
	ygglog_error << "addResponseClient: Error setting data" << std::endl;
	return -1;
      }
      if (hasPartner(partner_model) < 0)
	partners.emplace_back(partner_model);
      if (header.flags & HEAD_FLAG_SERVER_SIGNON)
	signon_complete = true;
      ygglog_debug << "addResponseClient: done (signon_complete = " <<
	signon_complete << ")" << std::endl;
      return 0;
    }
    Comm_t* getComm(const std::string& request_id) {
      int idx = hasRequest(request_id);
      if (idx < 0)
	return NULL;
      return comms[static_cast<size_t>(idx)];
    }
    Comm_t* activeComm(bool require_open = false) {
      if (requests.empty() || comms.empty()) {
	ygglog_error << "activeComm: No pending requests" << std::endl;
	return NULL;
      }
      Comm_t* out = comms[requests[0].comm_idx];
      if (require_open && out->is_closed())
	return NULL;
      return out;
    }
    Comm_t* lastComm() {
      if (comms.empty()) {
	ygglog_error << "lastComm: No communicators" << std::endl;
	return NULL;
      }
      if (!requests.empty())
	return activeComm();
      return comms[comms.size() - 1];
    }
    std::string activeRequestClient() {
      if ((!signon_complete) && requests.size() > 0) {
	return requests[0].request_id;
      }
      for (size_t i = 0; i < requests.size(); i++) {
	if (!requests[i].is_signon)
	  return requests[i].request_id;
      }
      ygglog_throw_error("activeRequestClient: No active requests");
      return "";
    }
    int popRequestServer() {
      if (requests.empty() || comms.empty()) {
	ygglog_error << "RequestList::popRequestServer: No pending requests" << std::endl;
	return -1;
      }
      requests.erase(requests.begin());
      return 1;
    }
    int popRequestClient(Header& header) {
      std::string request_id(header.GetMetaString("request_id"));
      int idx = hasRequest(request_id);
      if (idx < 0) {
	ygglog_error << "RequestList::popRequestClient: No pending request with id '" << request_id << "'" << std::endl;
	return -1;
      }
      if (!requests[(size_t)idx].complete) {
	ygglog_error << "RequestList::popRequestClient: Request '" << request_id << "' does not have response" << std::endl;
	return -1;
      }
      requests.erase(requests.begin() + idx);
      return 1;
    }
    int getRequestClient(const std::string& request_id,
			 char*& data, const size_t len,
			 bool allow_realloc) {
      int idx = hasRequest(request_id);
      if (idx < 0) {
	ygglog_error <<
	  "RequestList::getRequestClient: No pending request with id '" <<
	  request_id << "'" << std::endl;
	return -1;
      }
      if (!requests[(size_t)idx].complete) {
	ygglog_error << "RequestList::getRequestClient: Request '" <<
	  request_id << "' does not have response" << std::endl;
	return -1;
      }
      return static_cast<int>(Comm_t::copyData(
        data, len, requests[(size_t)idx].data.c_str(),
	requests[(size_t)idx].data.size(), allow_realloc));
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
      comms.push_back(x);
      x->addSchema(response_metadata);
      x->flags |= COMM_EOF_SENT | COMM_EOF_RECV;
      if (response_dir == RECV)
	x->flags |= COMM_FLAG_CLIENT_RESPONSE;
      else
	x->flags |= COMM_FLAG_SERVER_RESPONSE;
      idx = (int)(comms.size() - 1);
      return idx;
    }
    bool signonSent() const {
      for (size_t i = 0; i < requests.size(); i++) {
	if (requests[i].is_signon)
	  return true;
      }
      return false;
    }
    bool isComplete(const std::string& request_id) const {
      int idx = hasRequest(request_id);
      if (idx < 0)
	return false;
      return requests[(size_t)idx].complete;
    }
    void addResponseSchema(const std::string& s, bool use_generic=false) {
      response_metadata.fromSchema(s, use_generic);
    }
    void addResponseSchema(const rapidjson::Value& s,
			   bool use_generic=false) {
      response_metadata.fromSchema(s, use_generic);
    }
    void addResponseSchema(const Metadata& metadata,
			   bool use_generic=false) {
      response_metadata.fromMetadata(metadata, use_generic);
    }
    void addResponseFormat(const std::string& format_str,
			   bool use_generic=false) {
      response_metadata.fromFormat(format_str, use_generic);
    }
    bool partnerSignoff(const Header& header) {
      if (header.flags & HEAD_FLAG_EOF) {
	std::string partner_model(header.GetMetaString("model"));
	int idx = hasPartner(partner_model);
	if (idx >= 0)
	  partners[static_cast<size_t>(idx)].signed_off = true;
	for (size_t i = 0; i < partners.size(); i++) {
	  if (!partners[i].signed_off)
	    return false;
	}
      }
      return true;
    }
    void transferSchemaTo(Comm_t* comm) {
      comm->get_metadata(response_dir).fromMetadata(response_metadata);
    }
    void transferSchemaFrom(Comm_t* comm) {
      response_metadata.fromMetadata(comm->get_metadata(response_dir));
    }
    void Display() {
      std::cout << requests.size() << " Requests:" << std::endl;
      for (size_t i = 0; i < requests.size(); i++) {
	std::cout << "  " << requests[i].request_id << ": " <<
	  requests[i].complete << std::endl;
      }
    }
    std::vector<Comm_t*> comms;
    std::vector<Request> requests;
    std::vector<Partner> partners;
    DIRECTION response_dir;
    Metadata response_metadata;
    bool signon_complete;
    std::string stashed_request;
  };
}
}
#endif // COMM_BASE
