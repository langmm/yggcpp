#pragma once

#include "DefaultComm.hpp"
#include "Requests.hpp"
#ifdef COMM_BASE
namespace communication {
namespace communicator {

class ClientComm : public COMM_BASE {
public:
    explicit ClientComm(const std::string &name = "", utils::Address *address = nullptr);
    explicit ClientComm(const std::string name);

    // ~ClientComm() override;

    // bool new_address() override;

    //int init_comm();

    using Comm_t::send;
    using Comm_t::recv;
    using COMM_BASE::comm_nmsg;

protected:
    void init() override;
    int update_datatype(const rapidjson::Value& new_schema,
			const DIRECTION dir=NONE) override;
    bool create_header_send(Header& header, const char* data, const size_t &len) override;
    bool create_header_recv(Header& header, char*& data, const size_t &len,
			    size_t msg_len, int allow_realloc,
			    int temp) override;
    long recv_single(char*& data, const size_t &len,
		     bool allow_realloc) override;
    // void addResponseSchema(const rapidjson::Document& schema);
    // void addResponseFormat(const std::string& fmt);

#ifndef YGG_TEST
private:
#endif
    static unsigned _client_rand_seeded;
    RequestList requests;
};

}
} // communication

#endif
