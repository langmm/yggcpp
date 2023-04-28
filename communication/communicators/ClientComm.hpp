#pragma once

#include "DefaultComm.hpp"
#include "CommHead.hpp"
#ifdef COMM_BASE
namespace communication {
namespace communicator {

/**
 * Client communicator class, associated with a ServerComm. Actual communicator type
 * is determined at compile time based on available packages. It will be either
 * an IPCComm or ZMQComm
 */
class ClientComm : public COMM_BASE {
public:
    /**
     * Constructor
     * @param name The name of the communicator
     * @param address The address to associate with the communicator, if address is nullptr
     *                then an address will be created.
     */
    explicit ClientComm(const std::string &name = "", utils::Address *address = nullptr);

    /**
     * Destructor
     */
    ~ClientComm() override;

    /**
     * Whether there is a request with the given id.
     * @param req_id The request id to search for
     * @return The index of the given id if it has a request, -1 otherwise
     */
    int has_request(const std::string &req_id);

    /**
     * Whether there is a response with the given id.
     * @param req_id The request id to search for
     * @return The index of the given id if it has a response, -1 otherwise
     */
    int has_response(const std::string &req_id);

    /**
     * Add the specified request to the internal list
     * @param req_id The id to add.
     * @return Always returns 0
     */
    int add_request(const std::string &req_id);

    /**
     * Add the specified response to the internal list
     * @param req_id The id to add
     * @param rdata The contents of the response
     * @param rlen The length of rdata
     * @return 0 if successful, a negative number if there was an error
     */
    int add_response(const std::string &req_id, const char *rdata, const size_t &rlen);

    /**
     * Remove the specified request from the internal list, freeing any associated resources
     * @param req_id The id of the request to remove.
     * @return Always returns 0
     */
    int remove_request(const std::string &req_id);

    /**
     * Remove and return the next response from the internal list
     * @param req_id The id of the response to get
     * @param rdata The contents of the response will be put here
     * @param rlen The initial length of rdata
     * @param allow_realloc Whether rdata can be reallocated if it is too small to hold the message.
     * @return Negative numbers signify an error, otherwise the size of the message
     */
    int pop_response(const std::string &req_id, char *rdata, const size_t &rlen, const int allow_realloc);

    /**
     * Create a new address for this communicator
     * @return True if successful, false otherwise
     */
    bool new_address() override;

    //int init_comm();

    /**
     * The number of messages in the queue
     * @return The number of messages
     */
    int comm_nmsg() const override;

    void response_header(datatypes::CommHead &head);
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
    size_t nreq;                          //!< Current number of requests
    std::vector<std::string> request_id;  //!< List of requests
    std::vector<char *> data;             //!< response data queue
    std::vector<size_t> len;              //!< response data sizes
    static unsigned _client_rand_seeded;  //!< randome seed for gnerating addresses
};

}
} // communication

#endif