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
  
    COMM_CONSTRUCTOR_RPC_DEC(ServerComm, SERVER_COMM)

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
