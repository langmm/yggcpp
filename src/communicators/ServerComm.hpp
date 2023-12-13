#pragma once

#include <vector>
#include "RPCComm.hpp"

#ifdef COMM_BASE
namespace YggInterface {
namespace communicator {

/**
 * @brief Server communicator class, associated with ClientComms.
 * Actual communicator type
 * is determined at compile time based on available packages. It will be either
 * an IPCComm or ZMQComm
 */
class ServerComm : public RPCComm {
public:
    /**
     * @brief Constructor
     * @param[in] name The name of the communicator
     * @param[in] address The address to associate with the communicator, if address is nullptr
     *                then an address will be created.
     * @param[in] flgs Bitwise flags describing the communicator
     * @see utils::Address
     */
    explicit ServerComm(const std::string& name,
			utils::Address& address,
			int flgs = 0, const COMM_TYPE type = SERVER_COMM);
    ADD_CONSTRUCTORS_RPC(ServerComm, SERVER_COMM)

    using RPCComm::send;
    using RPCComm::recv;

#ifndef YGG_TEST
protected:
#endif
    /*!
     * @brief Sign on to the server
     * @param[in] header The header to use
     * @return true if successful
     */
    virtual bool signon(const utils::Header& header);
    /*! \copydoc YggInterface::communicator::CommBase::create_header_send */
    bool create_header_send(utils::Header& header) override;
    /*! \copydoc YggInterface::communicator::CommBase::send_single */
    int send_single(utils::Header& header) override;
    /*! \copydoc YggInterface::communicator::CommBase::recv_single */
    long recv_single(utils::Header& header) override;

};

}
} // YggInterface

#endif
