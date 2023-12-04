#pragma once

#include "RPCComm.hpp"
#ifdef COMM_BASE
namespace communication {
namespace communicator {

/**
 * Client communicator class, associated with a ServerComm. Actual communicator type
 * is determined at compile time based on available packages. It will be either
 * an IPCComm or ZMQComm
 */
class YGG_API ClientComm : public RPCComm {
public:
    /**
     * Constructor
     * @param name The name of the communicator
     * @param address The address to associate with the communicator, if
     *   address is empty, then an address will be created.
     * @param type Communicator type to assign (used internally).
     * @param reqtype Communicator type to use for the request communicator.
     * @param restype Communicator type to use for the response communicator.
     * @param reqflags Bitwise flags describing the request communicator.
     * @param resflags Bitwise flags describing the response communicator.
     */
    explicit ClientComm(const std::string name,
			const utils::Address &address,
			int flgs = 0, const COMM_TYPE type = CLIENT_COMM,
			const COMM_TYPE reqtype = DEFAULT_COMM,
			const COMM_TYPE restype = DEFAULT_COMM,
			int reqflags = 0, int resflags = 0);
    ADD_CONSTRUCTORS_RPC(ClientComm, CLIENT_COMM)

    // \copydoc Comm_t::logClass
    std::string logClass() const override;
    /*! \copydoc Comm_t::comm_nmsg */
    int comm_nmsg(DIRECTION dir=NONE) const override;
    // \copydoc Comm_t::set_timeout_recv
    void set_timeout_recv(int64_t new_timeout) override;
    // \copydoc Comm_t::get_timeout_recv
    int64_t get_timeout_recv() override;
    
    virtual bool signon();
    bool send_signon(int nloop, int interval=3,
		     Comm_t* async_comm = nullptr);

    using RPCComm::send;
    using RPCComm::recv;

#ifndef YGG_TEST
protected:
#endif
    void init();
    Comm_t* create_worker_send(utils::Header& head) override;
    Comm_t* create_worker_recv(utils::Header& head) override;
    bool create_header_send(utils::Header& header) override;
    /*! \copydoc Comm_t::recv_single */
    long recv_single(utils::Header& header) override;

#ifndef YGG_TEST
private:
#endif
    static unsigned _client_rand_seeded;

};

}
} // communication

#endif
