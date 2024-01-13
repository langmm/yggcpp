#pragma once

#include <utility>
#include <vector>
#include "DefaultComm.hpp"
#include "CommBase.hpp"

#ifdef COMM_BASE
namespace YggInterface {
namespace communicator {

/**
 * @brief Information about the partner model at the other end of a communicator
 */
class Partner {
public:
    /**
     * @brief Constructor
     */
    Partner() : model(), signed_off(false) {}
    /**
     * @brief Constructor
     * @param[in] adr The address to use
     */
    explicit Partner(const std::string& adr) : model(adr), signed_off(false) {}
    std::string model; //!< Partner model name.
    bool signed_off;   //!< Weather the partner signed off or not.
};
/**
 * @brief Class representing a request for communication
 */
class Request {
public:
    /**
     * @brief Constructor
     */
    Request() :
      request_id(), data(true), comm_idx(0), complete(false),
      is_signon(false) {}
    /**
     * @brief Constructor
     * @param[in] req_id The request ID
     * @param[in] cidx The communicator index
     * @param[in] is_son Whether the communicator has been signed on
     */
    Request(const std::string& req_id,
	    const size_t cidx, bool is_son = false) :
      request_id(req_id), data(true), comm_idx(cidx),
      complete(false), is_signon(is_son) {}
    /**
     * @brief Set the internal data from the header
     * @param[in] header The header to get the data from
     * @param[in] copy If true then copy the header data, if false then move the header data instead
     * @return 0 on success, -1 on failure
     * @see utils::Header
     */
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
    std::string request_id;    //!< The request id
    utils::Header data;        //!< Internal header for the message
    size_t comm_idx;           //!< The communicator index to use
    bool complete;             //!< Whether the request has been completed or not
    bool is_signon;            //!< Whether the communicator has been signed on or not
};

/**
 * @brief Container for a list of requests
 */
class RequestList : public YggInterface::utils::LogBase {
public:
    /**
     * @brief Constructor
     * @param[in] dir The direction for the requests
     * @param[in] flags Response flags to use
     * @param[in] restyp Response communicator type.
     * @param[in] logName Instance descriptor for logging.
     */
    explicit RequestList(DIRECTION dir, int flags = 0,
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
    /**
     * Destructor
     */
    ~RequestList() {
        destroy();
    }
    /** \copydoc YggInterface::communicator::Comm_t::logClass */
    std::string logClass() const override { return "RequestList"; }
    /** \copydoc YggInterface::communicator::Comm_t::logInst */
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
    /**
     * @brief Destroy the communicators
     */
    void destroy() {
        for (size_t i = 0; i < comms.size(); i++) {
            if (comms[i] != nullptr) {
                delete comms[i];
                comms[i] = nullptr;
            }
        }
        comms.resize(0);
    }
    /**
     * @brief Get the index of the specified Request
     * @param[in] request_id The id of the Request to get
     * @return The index of the Request, or -1 if it is not found
     */
    int hasRequest(const std::string& request_id) const {
        for (size_t i = 0; i < requests.size(); i++) {
            if (requests[i].request_id == request_id)
                return (int)i;
        }
        return -1;
    }
    /**
     * @brief Get the index of the Partner with the given model
     * @param[in] model The model to look for.
     * @return The index of the Partner, or -1 if it is not found
     */
    int hasPartner(const std::string& model) const {
        for (size_t i = 0; i < partners.size(); i++) {
            if (partners[i].model == model)
                return (int)i;
        }
        return -1;
    }
    /**
     * @brief Get the index of the communicator with the given response address
     * @param[in] response_address The address to search for
     * @return The index of the communicator, or -1 if it is not found
     */
    int hasComm(const std::string& response_address) const {
        for (size_t i = 0; i < comms.size(); i++) {
            if (comms[i] && comms[i]->address.address() == response_address)
                return (int)i;
        }
        return -1;
    }
    /**
     * @brief Initialize the communicators
     * @return Always returns 0
     */
    int initClientResponse() {
        if (comms.empty()) {
            addComm();
        }
        return 0;
    }
    /**
     * @brief Stash the first Request that is not signed on
     * @return Always returns 1
     */
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
    /**
     * @brief Add a client to the request list
     * @param[in] header The header for the client
     * @param[in] request_id The request id, if any
     * @return The index of the client, or -1 on failure
     * @see utils::Header
     */
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
	    return -1;  // GCOV_EXCL_LINE
	} else {
	  if (!header.SetMetaID("request_id", request_id))
	    return -1;  // GCOV_EXCL_LINE
	}
	log_debug() << "addRequestClient: request_id = "
		     << request_id << std::endl;
      } else {
	if (!header.SetMetaString("request_id", request_id))
	  return -1;  // GCOV_EXCL_LINE
      }
      if (!header.SetMetaString("response_address",
				comms[0]->address.address()))
	return -1;  // GCOV_EXCL_LINE
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
    /**
     * @brief Add a server to the request list
     * @param[in] header The header for the server
     * @return The index of the server, -1 on failure
     * @see utils::Header
     */
    int addRequestServer(utils::Header& header) {
      log_debug() << "addRequestServer: begin" << std::endl;
      std::string request_id, response_address, partner_model;
      if (!(header.GetMetaString("request_id", request_id) &&
	    header.GetMetaString("response_address", response_address) &&
	    header.GetMetaString("model", partner_model)))
	return -1;  // GCOV_EXCL_LINE
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
    /**
     * @brief Add a response server to the request list
     * @param[in] header The header for the response server
     * @return 0 on success, -1 on failure
     * @see utils::Header
     */
    int addResponseServer(utils::Header& header) {
      log_debug() << "addResponseServer: begin" << std::endl;
      if (requests.size() == 0) {
	log_error() << "addResponseServer: Server does not have any unprocessed requests" << std::endl;
	return -1;
      }
      log_debug() << "addResponseServer: request_id = " << requests[0].request_id << std::endl;
      header.initMeta();
      if (!header.SetMetaString("request_id", requests[0].request_id))
	return -1;  // GCOV_EXCL_LINE
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
    /**
     * @brief Add a response client to the request list
     * @param[in] header The header for the response client
     * @return 0 on success, -1 on failure
     * @see utils::Header
     */
    int addResponseClient(utils::Header& header) {
      log_debug() << "addResponseClient: begin" << std::endl;
      std::string request_id, partner_model;
      if (!(header.GetMetaString("model", partner_model) &&
	    header.GetMetaString("request_id", request_id)))
	return -1;  // GCOV_EXCL_LINE
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
    /**
     * @brief Get the communicator associated with the request_id
     * @param[in] request_id The id of the communicator to search for
     * @return The requested communicator
     */
    Comm_t* getComm(const std::string& request_id) {
        int idx = hasRequest(request_id);
        if (idx < 0)
            return nullptr;
        return comms[static_cast<size_t>(idx)];
    }
    /**
     * @brief Get the next communicator to use
     * @return The next communicator to use, or nullptr if there are no pending requests or communicators
     */
    Comm_t* activeComm() {
      if (requests.empty() || comms.empty()) {
	log_error() << "activeComm: No pending requests" << std::endl;
	return NULL;
      }
      return comms[requests[0].comm_idx];
    }
    /**
     * @brief Get the last communicator in the list
     * @return The last communicator
     */
    Comm_t* lastComm() {
      if (comms.empty()) {
	log_error() << "lastComm: No communicators" << std::endl;
	return NULL;
      }
      return comms[comms.size() - 1];
    }
    /**
     * @brief Get the ID of the currently active request client
     * @param[in] ignore_signon If true, don't return any signon request.
     * @return The ID, or en empty string if there are none.
     */
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
    /**
     * @brief Remove the first request server, deleting it.
     * @return 1 on success, -1 on failure
     */
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
    /**
     * @brief Get the specified request client
     * @param[in] request_id The id of the client to use
     * @param[in, out] header The header to put the client data into
     * @param[in] pop If true, then delete the client if it is not signed on
     * @return 1 on success, -1 on failure
     * @see utils::Header
     */
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
	log_error() << "getRequestClient: Error moving response header" << std::endl;  // GCOV_EXCL_LINE
	return -1;  // GCOV_EXCL_LINE
      }
      if (pop && !requests[(size_t)idx].is_signon) {
	requests.erase(requests.begin() + idx);
      }
      log_debug() << "getRequestClient: done" << std::endl;
      return 1;
    }
    /**
     * @brief Add a default type communicator to the list, using the given address, if any
     * @param[in] response_address The address to use for the communicator
     * @return The index of the new communicator
     */
    int addComm(const std::string& response_address = "") {
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
	  return -1;  // GCOV_EXCL_LINE
        idx = (int)(comms.size() - 1);
        return idx;
    }
    /**
     * @brief Query if any requests are signed on
     * @return True if any are signed on
     */
    bool signonSent() const {
        for (size_t i = 0; i < requests.size(); i++) {
            if (requests[i].is_signon)
                return true;
	        // return (comms[requests[i].comm_idx]->flags &
		//         COMM_FLAG_USED_SENT);
        }
        return false;
    }
    /**
     * @brief Query whether the specified request is complete
     * @param[in] request_id The id of the request to query
     * @return True is the request is complete, false if not complete, or not found
     */
    bool isComplete(const std::string& request_id) const {
        int idx = hasRequest(request_id);
        if (idx < 0)
            return false;
        return requests[(size_t)idx].complete;
    }
    /**
     * @brief Add a schema to the response communicator(s).
     * @param[in] s JSON serialized schema.
     * @param[in] use_generic If true, set schema to expect generic
     *   JSON objects.
     * @return true if successful, false otherwise.
     */
    bool addResponseSchema(const std::string& s, bool use_generic=false) {
      return response_metadata.fromSchema(s, use_generic);
    }
    /**
     * @brief Add a schema to the response communicator(s).
     * @param[in] s JSON schema.
     * @param[in] use_generic If true, set schema to expect generic
     *   JSON objects.
     * @return true if successful, false otherwise.
     */
    bool addResponseSchema(const rapidjson::Value& s,
			   bool use_generic=false) {
      return response_metadata.fromSchema(s, use_generic);
    }
    /**
     * @brief Add a schema to the response communicator(s).
     * @param[in] metadata Metadata to copy containing JSON schema.
     * @param[in] use_generic If true, set schema to expect generic
     *   JSON objects.
     * @return true if successful, false otherwise.
     */
    bool addResponseSchema(const utils::Metadata& metadata,
			   bool use_generic=false) {
      return response_metadata.fromMetadata(metadata, use_generic);
    }
    /**
     * @brief Add a schema to the response communicator based on a
     *   C-style format string.
     * @param[in] format_str C-style format string.
     * @param[in] use_generic If true, set schema to expect generic
     *   JSON objects.
     * @return true if successful, false otherwise.
     */
    bool addResponseFormat(const std::string& format_str,
			   bool use_generic=false) {
      return response_metadata.fromFormat(format_str, use_generic);
    }
    /**
     * @brief Query whether all partners are signed off
     * @param[in] header The header to use
     * @return true if all partners are signed off, false otherwise
     * @see utils::Header
     */
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
    /**
     * @brief Transfer the internal schema to the given communicator
     * @param[in, out] comm The communicator to transfer to
     * @return true if successful, false otherwise
     */
    bool transferSchemaTo(Comm_t* comm) {
      return comm->getMetadata(response_dir).fromMetadata(response_metadata);
    }
    /**
     * @brief Transfer the schema from the given communicator
     * @param[in] comm The communicator to transfer the schema from
     * @return true if successful, false otherwise
     */
    bool transferSchemaFrom(Comm_t* comm) {
      return response_metadata.fromMetadata(comm->getMetadata(response_dir));
    }
    /**
     * @brief Print the internal data to the terminal
     */
    void Display() {
        std::cout << requests.size() << " Requests:" << std::endl;
        for (size_t i = 0; i < requests.size(); i++) {
          std::cout << "  " << requests[i].request_id << ": " <<
                    requests[i].complete << std::endl;
        }
    }
    std::vector<Comm_t*> comms;        //!< Communicators to use
    std::vector<Request> requests;     //!< Requests to process
    std::vector<Partner> partners;     //!< Any partners
    DIRECTION response_dir;            //!< The direction for this instance
    int response_flags;                //!< Response flags to use
    utils::Metadata response_metadata; //!< Metadata to use for any responses
    bool signon_complete;              //!< Whether signon is complete
    std::string stashed_request;       //!< The currently stashed request
    COMM_TYPE restype;                 //!< Response communicator type
    std::string logInst_;              //!< Instance log descriptor
};
}
}
#endif // COMM_BASE
