#pragma once

#include <vector>
#include "communicators/RPCComm.hpp"

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
     * @param[in] type Communicator type to assign (used internally).
     * @param[in] ncomm Number of communicators in a forked request comm.
     * @param[in] reqtype Communicator type to use for the request communicator.
     * @param[in] restype Communicator type to use for the response communicator.
     * @param[in] reqflags Bitwise flags describing the request communicator.
     * @param[in] resflags Bitwise flags describing the response communicator.
     * @see utils::Address
     */
    YGG_API explicit ServerComm(const std::string& name,
				const utils::Address& address,
				FLAG_TYPE flgs = 0,
				const COMM_TYPE type = SERVER_COMM,
				size_t ncomm = 0,
				const COMM_TYPE reqtype = DEFAULT_COMM,
				const COMM_TYPE restype = DEFAULT_COMM,
				FLAG_TYPE reqflags = 0,
				FLAG_TYPE resflags = 0);
    ADD_CONSTRUCTORS_RPC(ServerComm, SERVER_COMM)

    using RPCComm::send;
    using RPCComm::recv;

    // \copydoc Comm_t::logClass
    YGG_API std::string logClass() const override;
    
protected:

    /*!
     * @brief Handle client signon
     * @param[in] header Header from client
     * @return true if successful
     */
    YGG_API virtual bool signon(const utils::Header& header);
    /*! \copydoc YggInterface::communicator::CommBase::create_header_send */
    YGG_API bool create_header_send(utils::Header& header) override;
    /*! \copydoc YggInterface::communicator::CommBase::send_single */
    YGG_API int send_single(utils::Header& header) override;
    /*! \copydoc YggInterface::communicator::CommBase::recv_single */
    YGG_API long recv_single(utils::Header& header) override;

};

}
} // YggInterface

#endif
