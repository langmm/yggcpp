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
            request_id(), response_id(), data(), comm_idx(0), complete(false),
            is_signon(false) {}
    Request(const std::string& req_id, const std::string& res_id,
            const size_t cidx, bool is_son = false) :
            request_id(req_id), response_id(res_id), data(), comm_idx(cidx),
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
    std::string response_id;
    std::string data;
    size_t comm_idx;
    bool complete;
    bool is_signon;
};

class RequestList {
public:
    RequestList(DIRECTION dir) :
            comms(), requests(), partners(), response_dir(dir),
            response_metadata(), active_comm(-1), signon_complete(false) {}
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
    int hasResponse(const std::string& response_id) const {
        for (size_t i = 0; i < requests.size(); i++) {
            if (requests[i].response_id == response_id)
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
            if (addComm() < 0) {
                ygglog_error << "initClientResponse: Error creating response comm." << std::endl;
                return -1;
            }
        }
        return 0;
    }
    int addRequestClient(utils::Header& header) {
        ygglog_debug << "addRequestClient: begin" << std::endl;
        if (initClientResponse() < 0)
            return -1;
        bool is_signon = (header.flags & HEAD_FLAG_CLIENT_SIGNON);
        int existing_idx = -1;
        std::string request_id;
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
            if (!header.SetMetaID("request_id", request_id)) {
                ygglog_error << "addRequestClient: Error adding request_id"
                             << std::endl;
                return -1;
            }
            ygglog_debug << "addRequestClient: request_id = "
                         << request_id << std::endl;
            if (hasRequest(request_id) >= 0) {
                ygglog_error
                        << "addRequestClient: Client already has request with id '"
                        << request_id << "'" << std::endl;
                return -1;
            }
        } else {
            if (!header.SetMetaString("request_id", request_id)) {
                ygglog_error << "addRequestClient: Error adding request_id"
                             << std::endl;
                return -1;
            }
        }
        if (!header.SetMetaString("response_address",
                                  comms[0]->address->address())) {
            ygglog_error <<
                         "addRequestClient: Error adding response_address" << std::endl;
            return -1;
        }
        if (existing_idx < 0) {
            size_t idx = requests.size();
            requests.emplace_back(request_id, "", 0, is_signon);
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
    int addRequestServer(utils::Header& header) {
        ygglog_debug << "addRequestServer: begin" << std::endl;
        std::string request_id(header.GetMetaString("request_id"));
        std::string response_address(header.GetMetaString("response_address"));
        std::string partner_model(header.GetMetaString("model"));
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
        size_t idx = requests.size();
        if (hasPartner(partner_model) < 0)
            partners.emplace_back(partner_model);
        requests.emplace_back(request_id, response_id,
                              static_cast<size_t>(comm_idx),
                              (header.flags & HEAD_FLAG_CLIENT_SIGNON));
        ygglog_debug << "addRequestServer: done idx = " << idx
                     << ", response_address = "
                     << comms[requests[idx].comm_idx]->address->address()
                     << ", request_id = " << requests[idx].request_id
                     << ", response_id = " << requests[idx].response_id
                     << std::endl;
        return static_cast<int>(idx);
    }
    int addResponseServer(utils::Header& header, const char* data, const size_t len) {
        ygglog_debug << "addResponseServer: begin" << std::endl;
        if (requests.size() == 0) {
            ygglog_error << "addResponseServer: Server does not have any unprocessed requests" << std::endl;
            return -1;
        }
        ygglog_debug << "addResponseServer: request_id = " << requests[0].request_id << std::endl;
        header.initMeta();
        if (!header.SetMetaString("request_id", requests[0].request_id)) {
            ygglog_error << "addResponseServer: Error seting request_id" << std::endl;
            return -1;
        }
        if (requests[0].setData(data, len) < 0) {
            ygglog_error << "addResponseServer: Error setting data" << std::endl;
            return -1;
        }
        active_comm = 0;
        if (header.flags & HEAD_FLAG_SERVER_SIGNON)
            signon_complete = true;
        ygglog_debug << "addResponseServer: done (signon_complete = " <<
                     signon_complete << ")" << std::endl;
        return 0;
    }
    int addResponseClient(utils::Header& header, const char* data, const size_t len) {
        ygglog_debug << "addResponseClient: begin" << std::endl;
        std::string request_id(header.GetMetaString("request_id"));
        std::string partner_model(header.GetMetaString("model"));
        int idx = hasRequest(request_id);
        if (idx < 0) {
#ifdef YGG_TEST
            // Allow duplicate on test where server sends signon response
            //   before client sends request
            if (header.flags & HEAD_FLAG_SERVER_SIGNON)
                return 0;
#endif // YGG_TEST
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
    Comm_t* activeComm() {
        if (requests.empty() || comms.empty()) {
            ygglog_error << "activeComm: No pending requests" << std::endl;
            return NULL;
        }
        return comms[requests[0].comm_idx];
    }
    std::string activeRequestClient() {
        if (!signon_complete) {
            return requests[0].request_id;
        }
        for (size_t i = 0; i < requests.size(); i++) {
            if (!requests[i].is_signon)
                return requests[i].request_id;
        }
        utils::ygglog_throw_error("activeRequestClient: No active requests");
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
    int popRequestClient(utils::Header& header) {
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
                         char*& data, const size_t len, bool allow_realloc) {
        int idx = hasRequest(request_id);
        if (idx < 0) {
            ygglog_error << "RequestList::getRequestClient: No pending request with id '" << request_id << "'" << std::endl;
            return -1;
        }
        if (!requests[(size_t)idx].complete) {
            ygglog_error << "RequestList::getRequestClient: Request '" << request_id << "' does not have response" << std::endl;
            return -1;
        }
        size_t ret = requests[(size_t)idx].data.size();
        if ((requests[(size_t)idx].data.size() + 1) > len) {
            if (!allow_realloc) {
                ygglog_error << "RequestList::getRequestClient: Response exceeds size of provided buffer" << std::endl;
                return -1;
            }
            char* data_t = (char*)realloc(data, requests[(size_t)idx].data.size() + 1);
            if (data_t == NULL) {
                ygglog_error << "RequestList::getRequestClient: Error reallocating buffer" << std::endl;
                return -1;
            }
            data = data_t;
            memcpy(data, requests[(size_t)idx].data.c_str(), requests[(size_t)idx].data.size() + 1);
            data[requests[(size_t)idx].data.size()] = '\0';
        }
        return static_cast<int>(ret);
    }
    int addComm(std::string response_address = "") {
        int idx;
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
    void addResponseSchema(const std::string& s) {
      response_metadata.fromSchema(s);
    }
    void addResponseSchema(const rapidjson::Value& s) {
        response_metadata.fromSchema(s);
    }
    void addResponseFormat(const std::string& format_str) {
        response_metadata.fromFormat(format_str);
    }
    bool partnerSignoff(const utils::Header& header) {
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
    std::vector<Comm_t*> comms;
    std::vector<Request> requests;
    std::vector<Partner> partners;
    DIRECTION response_dir;
    utils::Metadata response_metadata;
    int active_comm;
    bool signon_complete;
};
}
}
#endif // COMM_BASE
