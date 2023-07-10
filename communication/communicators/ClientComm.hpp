#pragma once

#include "RPCComm.hpp"
#ifdef COMM_BASE
namespace communication {
namespace communicator {

class ClientComm : public RPCComm {
public:
    explicit ClientComm(const std::string name = "",
			utils::Address *address = nullptr,
			int flgs = 0);
    ADD_CONSTRUCTORS_RPC(ClientComm)

    void set_timeout_recv(int new_timeout) override;
    int wait_for_recv(const int tout) override;

    using RPCComm::send;
    using RPCComm::recv;
    using RPCComm::comm_nmsg;

#ifndef YGG_TEST
protected:
#endif
    void init();
    virtual bool signon(const Header& header);
    Comm_t* create_worker_send(Header& head) override;
    Comm_t* create_worker_recv(Header& head) override;
    bool create_header_send(Header& header, const char* data, const size_t &len) override;
    bool create_header_recv(Header& header, char*& data, const size_t &len,
			    size_t msg_len, int allow_realloc,
			    int temp) override;
    long recv_single(char*& data, const size_t &len,
		     bool allow_realloc) override;

#ifndef YGG_TEST
private:
#endif
    static unsigned _client_rand_seeded;
};

}
} // communication

#endif
