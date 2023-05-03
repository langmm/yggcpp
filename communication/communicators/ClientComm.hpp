#pragma once

#include "DefaultComm.hpp"
#include "Requests.hpp"
#ifdef COMM_BASE
namespace communication {
namespace communicator {

class ClientComm : public COMM_BASE {
public:
    explicit ClientComm(const std::string &name = "",
			utils::Address *address = nullptr,
			int flgs = 0);
    explicit ClientComm(const std::string name, int flgs = 0);

    void set_timeout_recv(int new_timeout) override;
    int wait_for_recv(const int tout) override;

    using Comm_t::send;
    using Comm_t::recv;
    using COMM_BASE::comm_nmsg;

#ifndef YGG_TEST
protected:
#endif
    void init() override;
    virtual bool signon(const Header& header);
    int update_datatype(const rapidjson::Value& new_schema,
			const DIRECTION dir=NONE) override;
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
    RequestList requests;
};

}
} // communication

#endif
