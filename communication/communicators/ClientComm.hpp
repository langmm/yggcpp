#pragma once

#include "DefaultComm.hpp"

namespace communication {
namespace communicator {

class ClientComm : public COMM_BASE {
public:
    explicit ClientComm(const std::string &name = "", utils::Address *address = nullptr, Direction direction = NONE,
                        datatypes::DataType *datatype = nullptr);

    ~ClientComm();

    int has_request(const std::string &req_id);

    int has_response(const std::string &req_id);

    int add_request(const std::string &req_id);

    int add_response(const std::string &req_id, const char *rdata, const size_t &rlen);

    int remove_request(const std::string &req_id);

    int pop_response(const std::string &req_id, char **rdata, const size_t &rlen, const int allow_realloc);

    int new_address();

    int init_comm();

    int comm_nmsg() const override;

    datatypes::comm_head_t response_header(datatypes::comm_head_t head);

    int send(const char *data, const size_t &len) override;

    long recv(char **data, const size_t &len, bool allow_realloc) override;

private:
    COMM_BASE *comm;
    size_t nreq;
    std::vector<std::string> request_id;
    std::vector<char *> data;
    std::vector<size_t> len;
    static unsigned _client_rand_seeded;
    COMM_BASE *base_handle;
};

}
} // communication
