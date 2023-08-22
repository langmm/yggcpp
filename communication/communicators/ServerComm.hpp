#pragma once

#include <vector>
#include "RPCComm.hpp"

#ifdef COMM_BASE
namespace communication {
namespace communicator {

/**
 * Server communicator class, associated with ClientComms. Actual communicator type
 * is determined at compile time based on available packages. It will be either
 * an IPCComm or ZMQComm
 */
class ServerComm : public RPCComm {
public:
    /**
     * Constructor
     * @param name The name of the communicator
     * @param address The address to associate with the communicator, if address is nullptr
     *                then an address will be created.
     * @param flgs Bitwise flags describing the communicator
     */
    explicit ServerComm(const std::string name = "",
			utils::Address *address = nullptr,
			int flgs = 0, const COMM_TYPE type = SERVER_COMM);
    ADD_CONSTRUCTORS_RPC(ServerComm, SERVER_COMM)

    using RPCComm::send;
    using RPCComm::recv;
    using RPCComm::comm_nmsg;

#ifndef YGG_TEST
protected:
#endif
    virtual bool signon(const utils::Header& header);
    bool create_header_send(utils::Header& header, const char* data, const size_t &len) override;
    bool create_header_recv(utils::Header& header, char*& data, const size_t &len,
			    size_t msg_len, int allow_realloc,
			    int temp) override;
    int send_single(const char *data, const size_t &len,
		    const utils::Header& header) override;
    // long recv_single(char*& data, const size_t &len,
    // 		     bool allow_realloc) override;

};

}
} // communication

#endif
