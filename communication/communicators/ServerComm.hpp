#pragma once

#include <vector>
#include "DefaultComm.hpp"
#include "CommBase.hpp"
#include "Requests.hpp"

#ifdef COMM_BASE
namespace communication {
namespace communicator {

// @brief Structure for storing requests
class ServerComm : public COMM_BASE {
public:
    explicit ServerComm(const std::string &name = "",
			utils::Address *address = nullptr,
			int flgs = 0);
    explicit ServerComm(const std::string name, int flgs = 0);
    ~ServerComm() override {}

    using Comm_t::send;
    using Comm_t::recv;
    using COMM_BASE::comm_nmsg;

#ifndef YGG_TEST
protected:
#endif
    void init();
    virtual bool signon(const Header& header);
    int update_datatype(const rapidjson::Value& new_schema,
			const DIRECTION dir=NONE) override;
    bool create_header_send(Header& header, const char* data, const size_t &len) override;
    bool create_header_recv(Header& header, char*& data, const size_t &len,
			    size_t msg_len, int allow_realloc,
			    int temp) override;
    int send_single(const char *data, const size_t &len,
		    const Header& header) override;

#ifndef YGG_TEST
private:
#endif
    RequestList requests;
};

}
} // communication

#endif
