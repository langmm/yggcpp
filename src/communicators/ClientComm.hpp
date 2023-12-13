#pragma once

#include "RPCComm.hpp"
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
     * @param[in] address The address to associate with the communicator, if address is nullptr
     *                then an address will be created.
     * @param[in] flgs Bitwise flags describing the communicator
     * @see utils::Address
     */
    explicit ClientComm(const std::string& name,
                        utils::Address &address,
                        int flgs = 0, const COMM_TYPE type = CLIENT_COMM);
    ADD_CONSTRUCTORS_RPC(ClientComm, CLIENT_COMM)

    /**
     * Destructor
     */
    ~ClientComm() override = default;
    /** \copydoc YggInterface::communicator::Comm_t::set_timeout_recv */
    void set_timeout_recv(int new_timeout) override;
    /** \copydoc YggInterface::communicator::Comm_t::get_timeout_recv */
    int get_timeout_recv() override;
    /**
     * @brief Connect the client to the server
     * @param[in] header The header to use
     * @param[in] async_comm The communicator to use
     * @return True if successful
     * @see utils::Header
     */
    virtual bool signon(const utils::Header& header,
                        Comm_t* async_comm=nullptr);

    using RPCComm::send;
    using RPCComm::recv;

#ifndef YGG_TEST
protected:
#endif
    void init();
    /** \copydoc YggInterface::communicator::Comm_t::create_worker_send */
    Comm_t* create_worker_send(utils::Header& head) override;
    /** \copydoc YggInterface::communicator::Comm_t::create_worker_recv */
    Comm_t* create_worker_recv(utils::Header& head) override;
    /** \copydoc YggInterface::communicator::Comm_t::create_header_send */
    bool create_header_send(utils::Header& header) override;
    /** \copydoc YggInterface::communicator::Comm_t::recv_single */
    long recv_single(utils::Header& header) override;

#ifndef YGG_TEST
private:
#endif
    static unsigned _client_rand_seeded;

};

}
} // YggInterface

#endif
