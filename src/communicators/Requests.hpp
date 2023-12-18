#pragma once

#include <vector>
#include "DefaultComm.hpp"
#include "CommBase.hpp"

#ifdef COMM_BASE
namespace YggInterface {
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
      request_id(), data(true), comm_idx(0), complete(false),
      is_signon(false) {}
    Request(const std::string& req_id,
	    const size_t cidx, bool is_son = false) :
      request_id(req_id), data(true), comm_idx(cidx),
      complete(false), is_signon(is_son) {}
    int setData(utils::Header& header, bool copy=false) {
      if (complete && !is_signon) {
	YggLogError << "Request::setData: request already complete" << std::endl;
	return -1;
      }
      if (copy)
	data.CopyFrom(header);
      else
	data = std::move(header);
      complete = true;
      return 0;
    }
    std::string request_id;
    utils::Header data;
    size_t comm_idx;
    bool complete;
    bool is_signon;
};

class RequestList : public communication::utils::LogBase {
public:
    RequestList(DIRECTION dir, int flags = 0,
		const COMM_TYPE restyp = DEFAULT_COMM,
		std::string logName = "") :
      comms(), requests(), partners(),
      response_dir(dir), response_flags(flags),
      response_metadata(), signon_complete(false),
      stashed_request(), restype(restyp),
      logInst_(logName) {
      response_flags |= COMM_FLAG_EOF_SENT | COMM_FLAG_EOF_RECV;
      if (response_dir == RECV)
	response_flags |= COMM_FLAG_CLIENT_RESPONSE;
      else
	response_flags |= COMM_FLAG_SERVER_RESPONSE;
      if (response_flags & COMM_FLAG_ASYNC_WRAPPED) {
	response_flags &= ~COMM_FLAG_ASYNC_WRAPPED;
	response_flags |= COMM_FLAG_ASYNC;
      }
    }
    ~RequestList() {
        destroy();
    }
    std::string logClass() const override { return "RequestList"; }
    std::string logInst() const override { return logInst_; }
    // void initClientSignon() {
    //   if (requests.empty()) {
    // 	log_debug() << "initClientSignon: begin" << std::endl;
    // 	utils::Header header;
    // 	header.initMeta();
    // 	header.flags |= HEAD_FLAG_CLIENT_SIGNON;
    // 	addRequestClient(header);
    // 	log_debug() << "initClientSignon: complete" << std::endl;
    //   }
    // }
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
            if (comms[i] && comms[i]->address.address() == response_address)
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
      // if (!requests.empty()) {
      // 	stashed_request = requests[0].request_id;
      // 	requests.erase(requests.begin());
      // }
      for (std::vector<Request>::iterator it = requests.begin();
	   it != requests.end(); it++) {
	if (!it->is_signon) {
	  stashed_request = it->request_id;
	  requests.erase(it);
	  log_debug() << "stashRequest: Stashing " << stashed_request << std::endl;
	  break;
	}
      }
      return 1;
    }
    int addRequestClient(utils::Header& header, std::string request_id="") {
      log_debug() << "addRequestClient: begin" << std::endl;
      initClientResponse();
      bool is_signon = (header.flags & HEAD_FLAG_CLIENT_SIGNON);
      log_debug() << "addRequestClient: is_signon = " <<
	is_signon << std::endl;
      int existing_idx = -1;
      if (is_signon) {
	for (size_t i = 0; i < requests.size(); i++) {
	  if (requests[i].is_signon) {
	    request_id = requests[i].request_id;
	    existing_idx = static_cast<int>(i);
	    break;
	  }
	}
      // } else if (requests.empty()) {
      // 	// Ensure signon is first request
      // 	initClientSignon();
      }
      if (request_id.empty()) {
	if (!stashed_request.empty()) {
	  request_id = stashed_request;
	  stashed_request.clear();
	  if (!header.SetMetaString("request_id", request_id))
	    return -1;
	} else {
	  if (!header.SetMetaID("request_id", request_id))
	    return -1;
	}
	log_debug() << "addRequestClient: request_id = "
		     << request_id << std::endl;
      } else {
	if (!header.SetMetaString("request_id", request_id))
	  return -1;
      }
      if (!header.SetMetaString("response_address",
				comms[0]->address.address()))
	return -1;
      if (existing_idx < 0) {
	size_t idx = requests.size();
	if (hasRequest(request_id) >= 0) {
	  log_error()
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
      log_debug() << "addRequestClient: done response_address = "
		   << comms[0]->address.address() << ", request_id = "
		   << request_id << std::endl;
      return existing_idx;
    }
    int addRequestServer(utils::Header& header) {
      log_debug() << "addRequestServer: begin" << std::endl;
      std::string request_id, response_address, partner_model;
      if (!(header.GetMetaString("request_id", request_id) &&
	    header.GetMetaString("response_address", response_address) &&
	    header.GetMetaString("model", partner_model)))
	return -1;
      int comm_idx = addComm(response_address);
      size_t idx = requests.size();
      if (hasPartner(partner_model) < 0)
	partners.emplace_back(partner_model);
      requests.emplace_back(request_id,
			    static_cast<size_t>(comm_idx),
			    (header.flags & HEAD_FLAG_CLIENT_SIGNON));
      log_debug() << "addRequestServer: done idx = " << idx
		   << ", response_address = "
		   << comms[requests[idx].comm_idx]->address.address()
		   << ", request_id = " << requests[idx].request_id
		   << std::endl;
      return static_cast<int>(idx);
    }
    int addResponseServer(utils::Header& header) {
      log_debug() << "addResponseServer: begin" << std::endl;
      if (requests.size() == 0) {
	log_error() << "addResponseServer: Server does not have any unprocessed requests" << std::endl;
	return -1;
      }
      log_debug() << "addResponseServer: request_id = " << requests[0].request_id << std::endl;
      header.initMeta();
      if (!header.SetMetaString("request_id", requests[0].request_id))
	return -1;
      if (requests[0].setData(header, true) < 0) {
	log_error() << "addResponseServer: Error setting data" << std::endl;
	return -1;
      }
      if (header.flags & HEAD_FLAG_SERVER_SIGNON)
	signon_complete = true;
      log_debug() << "addResponseServer: done (signon_complete = " <<
	signon_complete << ")" << std::endl;
      return 0;
    }
    int addResponseClient(utils::Header& header) {
      log_debug() << "addResponseClient: begin" << std::endl;
      std::string request_id, partner_model;
      if (!(header.GetMetaString("model", partner_model) &&
	    header.GetMetaString("request_id", request_id)))
	return -1;
      if (header.flags & HEAD_FLAG_SERVER_SIGNON) {
	log_debug() << "addResponseClient[" << request_id <<
	  "]: signon!" << std::endl;
	signon_complete = true;
      }
      int idx = hasRequest(request_id);
      if (idx < 0) {
	log_error() << "addResponseClient[" << request_id <<
	  "]: Client does not have a request with id '" << request_id << "'" << std::endl;
	return -1;
      }
      if (requests[static_cast<size_t>(idx)].setData(header) < 0) {
	log_error() << "addResponseClient[" << request_id <<
	  "]: Error setting data" << std::endl;
	return -1;
      }
      if (hasPartner(partner_model) < 0)
	partners.emplace_back(partner_model);
      log_debug() << "addResponseClient[" << request_id <<
	"]: done (signon_complete = " <<
	signon_complete << ")" << std::endl;
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
	log_error() << "activeComm: No pending requests" << std::endl;
	return NULL;
      }
      return comms[requests[0].comm_idx];
    }
    Comm_t* lastComm() {
      if (comms.empty()) {
	log_error() << "lastComm: No communicators" << std::endl;
	return NULL;
      }
      return comms[comms.size() - 1];
    }
    std::string activeRequestClient(bool ignore_signon=false) const {
      if (!ignore_signon)
	ignore_signon = signon_complete;
      for (size_t i = 0; i < requests.size(); i++) {
	if ((requests[i].is_signon && !ignore_signon) ||
	    (ignore_signon && !requests[i].is_signon))
	  return requests[i].request_id;
      }
      return ""; // GCOVR_EXCL_LINE
    }
    int popRequestServer() {
        if (requests.empty() || comms.empty()) {
            log_error() << "popRequestServer: No pending requests" << std::endl;
            return -1;
        }
        requests.erase(requests.begin());
        return 1;
    }
    // int popRequestClient(utils::Header& header) {
    //     std::string request_id;
    //     if (!header.GetMetaString("request_id", request_id))
    //       return -1;
    //     int idx = hasRequest(request_id);
    //     if (idx < 0) {
    //         log_error() << "popRequestClient: No pending request with id '" << request_id << "'" << std::endl;
    //         return -1;
    //     }
    //     if (!requests[(size_t)idx].complete) {
    //         log_error() << "popRequestClient: Request '" << request_id << "' does not have response" << std::endl;
    //         return -1;
    //     }
    // 	if (requests[(size_t)idx].is_signon) {
    // 	    // Don't remove signon request
    // 	    return 1;
    // 	}
    // 	log_debug() << "popRequestClient: Removing request '" << request_id << "'" << std::endl;
    //     requests.erase(requests.begin() + idx);
    //     return 1;
    // }
    int getRequestClient(const std::string& request_id,
			 utils::Header& header, bool pop=false) {
      log_debug() << "getRequestClient: begin" << std::endl;
      int idx = hasRequest(request_id);
      if (idx < 0) {
	log_error() <<
	  "getRequestClient: No pending request with id '" <<
	  request_id << "'" << std::endl;
	return -1;
      }
      if (!requests[(size_t)idx].complete) {
	log_error() << "getRequestClient: Request '" <<
	  request_id << "' does not have response" << std::endl;
	return -1;
      }
      if (!header.MoveFrom(requests[(size_t)idx].data)) {
	log_error() << "getRequestClient: Error moving response header" << std::endl;
	return -1;
      }
      if (pop && !requests[(size_t)idx].is_signon) {
	requests.erase(requests.begin() + idx);
      }
      log_debug() << "getRequestClient: done" << std::endl;
      return 1;
    }
    int addComm(std::string response_address = "") {
        int idx;
        if (!response_address.empty()) {
            idx = hasComm(response_address);
            if (idx >= 0)
                return idx;
        }
        utils::Address response_adr(response_address);
	Comm_t* x = new_Comm_t(response_dir, restype, "",
			       response_adr, response_flags);
        comms.push_back(x);
        if (!x->addSchema(response_metadata))
	  return -1;
        idx = (int)(comms.size() - 1);
        return idx;
    }
    bool signonSent() const {
        for (size_t i = 0; i < requests.size(); i++) {
            if (requests[i].is_signon)
	        return true;
	        // return (comms[requests[i].comm_idx]->flags &
		//         COMM_FLAG_USED_SENT);
        }
        return false;
    }
    bool isComplete(const std::string& request_id) const {
        int idx = hasRequest(request_id);
        if (idx < 0)
            return false;
        return requests[(size_t)idx].complete;
    }
    bool addResponseSchema(const std::string& s, bool use_generic=false) {
      return response_metadata.fromSchema(s, use_generic);
    }
    bool addResponseSchema(const rapidjson::Value& s,
			   bool use_generic=false) {
      return response_metadata.fromSchema(s, use_generic);
    }
    bool addResponseSchema(const utils::Metadata& metadata,
			   bool use_generic=false) {
      return response_metadata.fromMetadata(metadata, use_generic);
    }
    bool addResponseFormat(const std::string& format_str,
			   bool use_generic=false) {
      return response_metadata.fromFormat(format_str, use_generic);
    }
    bool partnerSignoff(const utils::Header& header) {
      if (header.flags & HEAD_FLAG_EOF) {
	std::string partner_model;
	if (!header.GetMetaString("model", partner_model))
	  return false;
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
    bool transferSchemaTo(Comm_t* comm) {
      return comm->getMetadata(response_dir).fromMetadata(response_metadata);
    }
    bool transferSchemaFrom(Comm_t* comm) {
      return response_metadata.fromMetadata(comm->getMetadata(response_dir));
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
    int response_flags;
    utils::Metadata response_metadata;
    bool signon_complete;
    std::string stashed_request;
    COMM_TYPE restype;
    std::string logInst_;
};
}
}
#endif // COMM_BASE
