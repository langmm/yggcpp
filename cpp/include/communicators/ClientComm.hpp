#pragma once

#include "communicators/RPCComm.hpp"
#ifdef COMM_BASE
namespace YggInterface {
namespace communicator {

/**
 * @brief Client communicator class, associated with a ServerComm.
 * Actual communicator type
 * is determined at compile time based on available packages. It will be either
 * an IPCComm or ZMQComm
 */
class ClientComm : public RPCComm {
public:
    /**
     * @brief Constructor
     * @param[in] name The name of the communicator
     * @param[in] address The address to associate with the communicator, if
     *   address is empty, then an address will be created.
     * @param[in] flgs Bitwise flags describing the communicator
     * @param[in] type Communicator type to assign (used internally).
     * @param[in] ncomm Number of communicators in a forked request comm.
     * @param[in] reqtype Communicator type to use for the request communicator.
     * @param[in] restype Communicator type to use for the response communicator.
     * @param[in] reqflags Bitwise flags describing the request communicator.
     * @param[in] resflags Bitwise flags describing the response communicator.
     * @see utils::Address
     */
    YGG_API explicit ClientComm(const std::string& name,
				const utils::Address &address,
				FLAG_TYPE flgs = 0,
				const COMM_TYPE type = CLIENT_COMM,
				size_t ncomm = 0,
				const COMM_TYPE reqtype = DEFAULT_COMM,
				const COMM_TYPE restype = DEFAULT_COMM,
				FLAG_TYPE reqflags = 0,
				FLAG_TYPE resflags = 0);
    ADD_CONSTRUCTORS_RPC(ClientComm, CLIENT_COMM)

    /** \copydoc YggInterface::communicator::Comm_t::logClass */
    YGG_API std::string logClass() const override;
    /** \copydoc YggInterface::communicator::Comm_t::comm_nmsg */
    YGG_API int comm_nmsg(DIRECTION dir=NONE) const override;
    /** \copydoc YggInterface::communicator::Comm_t::set_timeout_recv */
    YGG_API void set_timeout_recv(int64_t new_timeout) override;
    /** \copydoc YggInterface::communicator::Comm_t::get_timeout_recv */
    YGG_API int64_t get_timeout_recv() const override;

    /**
     * @brief Connect the client to the server
     * @return true if successful, false otherwise
     */
    YGG_API virtual bool signon();
    /**
     * @brief Send a signon message to the server.
     * @param[in] nloop Number of loops completed in signon.
     * @param[in] interval Interval at which signons are sent for the
     *   number of loops.
     * @param[in] async_comm Async communicator that should be used for
     *   signon messages.
     * @return true if successful, false otherwise
     */
    YGG_API bool send_signon(int nloop, int interval=3,
			     Comm_t* async_comm = nullptr);

    using RPCComm::send;
    using RPCComm::recv;

protected:
    /** \copydoc YggInterface::communicator::Comm_t::create_worker_send */
    YGG_API Comm_t* create_worker_send(utils::Header& head) override;
    /** \copydoc YggInterface::communicator::Comm_t::create_worker_recv */
    YGG_API Comm_t* create_worker_recv(utils::Header& head) override;
    /** \copydoc YggInterface::communicator::Comm_t::create_header_send */
    YGG_API bool create_header_send(utils::Header& header) override;
    /** \copydoc YggInterface::communicator::Comm_t::recv_single */
    YGG_API long recv_single(utils::Header& header) override;

private:
    static unsigned _client_rand_seeded; /**< Status of rand seed */

};

}
} // YggInterface

#endif
