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
     * @param address The address to associate with the communicator, if address is nullptr
     *                then an address will be created.
     * @param flgs Bitwise flags describing the communicator
     */
    explicit ClientComm(const std::string name = "",
			utils::Address *address = nullptr,
			int flgs = 0, const COMM_TYPE type = CLIENT_COMM);
    ADD_CONSTRUCTORS_RPC(ClientComm, CLIENT_COMM)

    /**
     * Destructor
     */
    ~ClientComm() override = default;
    
    // \copydoc Comm_t::logClass
    std::string logClass() const override;
    
    void set_timeout_recv(int64_t new_timeout) override;
    int get_timeout_recv() override;
    virtual bool signon(const utils::Header& header,
			Comm_t* async_comm=nullptr);

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
