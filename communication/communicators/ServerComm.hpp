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
class YGG_API ServerComm : public RPCComm {
public:
    /**
     * Constructor
     * @param name The name of the communicator
     * @param address The address to associate with the communicator, if address is nullptr
     *                then an address will be created.
     * @param flgs Bitwise flags describing the communicator
     * @param type Communicator type to assign (used internally).
     * @param reqtype Communicator type to use for the request communicator.
     * @param restype Communicator type to use for the response communicator.
     * @param reqflags Bitwise flags describing the request communicator.
     * @param resflags Bitwise flags describing the response communicator.
     */
    explicit ServerComm(const std::string name,
			const utils::Address& address,
			int flgs = 0, const COMM_TYPE type = SERVER_COMM,
			const COMM_TYPE reqtype = DEFAULT_COMM,
			const COMM_TYPE restype = DEFAULT_COMM,
			int reqflags = 0, int resflags = 0);
    ADD_CONSTRUCTORS_RPC(ServerComm, SERVER_COMM)

    using RPCComm::send;
    using RPCComm::recv;

    // \copydoc Comm_t::logClass
    std::string logClass() const override;
    
protected:
    virtual bool signon(const utils::Header& header);
    bool create_header_send(utils::Header& header) override;
    int send_single(utils::Header& header) override;
    long recv_single(utils::Header& header) override;

};

}
} // communication

#endif
