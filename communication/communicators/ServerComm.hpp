#pragma once

#include <vector>
#include "DefaultComm.hpp"
#include "CommBase.hpp"
#include "Requests.hpp"

#ifdef COMM_BASE
namespace communication {
namespace communicator {
#ifdef _OPENMP
#pragma omp threadprivate(_default_comm)
#endif

// @brief Structure for storing requests
class ServerComm : public COMM_BASE {
public:
    explicit ServerComm(const std::string &name = "", utils::Address *address = nullptr);
    explicit ServerComm(const std::string name);

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
    int send_single(const char *data, const size_t &len) override;

#ifndef YGG_TEST
    private:
#endif
    RequestList requests;
};

}
} // communication

#endif
