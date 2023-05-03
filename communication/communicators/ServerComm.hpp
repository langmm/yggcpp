#pragma once

#include <vector>
#include "DefaultComm.hpp"
#include "CommHead.hpp"
#include "CommBase.hpp"

#ifdef COMM_BASE
namespace communication {
namespace communicator {
#ifdef HAVE_OPENMP
#pragma omp threadprivate(_default_comm)
#endif

/**
 * Server communicator class, associated with ClientComms. Actual communicator type
 * is determined at compile time based on available packages. It will be either
 * an IPCComm or ZMQComm
 */
class ServerComm : public COMM_BASE {
public:
    /**
     * Constructor
     * @param name The name of the communicator
     * @param address The address to associate with the communicator, if address is nullptr
     *                then an address will be created.
     */
    explicit ServerComm(const std::string &name = "", utils::Address *address = nullptr);

    /**
     * Destructor
     */
    ~ServerComm() override;

    /**
     * Whether there is a request with the given id.
     * @param req_id The request id to search for
     * @return The index of the given id if it has a request, -1 otherwise
     */
    int has_request(const std::string &req_id) const;

    /**
     * Whether there is a response with the given id.
     * @param req_id The request id to search for
     * @return The index of the given id if it has a response, -1 otherwise
     */
    int has_response(const std::string &resp_id) const;

    /**
     * Whether There is an associated communicator with the given address
     * @param resp_address The address to look for
     * @return The index of the associated communicator, -1 if not found
     */
    int has_comm(const ::std::string &resp_address) const;

    /**
     * Whether There is an associated communicator with the given address
     * @param resp_address The address to look for
     * @return The index of the associated communicator, -1 if not found
     */
    int has_comm(const utils::Address *resp_address) const;

    /**
     * Add a communicator with the given address to the list of associated communicators
     * @param address The address to add
     * @return Always 0
     */
    int add_comm(::std::string &address);

    /**
     * Add a communicator with the given address to the list of associated communicators
     * @param address The address to add
     * @return Always 0
     */
    int add_comm(utils::Address *address);

    /**
     * Get the communicator at the given index.
     * @param idx The index of the communicator to get, if negative then return the first comm in the list
     * @return Pointer to the requested communicator, nullptr if it does not exist
     */
    Comm_t *get_comm(const int idx = -1) const;

    /**
     * Add a request to the internal queue
     * @param req_id The id of the request
     * @param response_address The communicator address to associate with the request
     * @return Always returns 0
     */
    int add_request(const std::string &req_id, utils::Address *response_address);

    /**
     * Removes the specified request from the internal list
     * @param idx The index of the request to remove.
     * @return 0 if successful, -1 otherwise
     */
    int remove_request(size_t idx);

    /**
     * The number of messages in the queue
     * @return The number of messages
     */
    int comm_nmsg() const override;

    datatypes::CommHead response_header(datatypes::CommHead &head);
    using Comm_t::send;
    using Comm_t::recv;

protected:
    /**
     * Sending function
     * @param data The message to send
     * @param len The length of data
     * @return THe status
     */
    int send(const char *data, const size_t &len) override;

    /**
     * Receiving function
     * @param data The contents of the message withh be placed here
     * @param len The initial length of data
     * @param allow_realloc Whether data can be reallocated if it is too small to hold the message.
     * @return The length of data after the message was copied.
     */
    long recv(char *data, const size_t &len, bool allow_realloc) override;

#ifndef YGG_TEST
    private:
#endif
    ::std::vector<Comm_t *> comms; //!< Array of response comms.
    ::std::vector<std::string> response_id; //!< Response ids.
    ::std::vector<std::string> request_id; //!< Request ids.
    ::std::vector<size_t> comm_idx; //!< Index of comm associated w/ a request
};

}
} // communication

#endif