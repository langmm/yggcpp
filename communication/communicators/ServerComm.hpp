#pragma once

#include <vector>
#include "DefaultComm.hpp"
#ifdef _OPENMP
#pragma omp threadprivate(_default_comm)
#endif
#include "CommBase.hpp"

namespace communication {
namespace communicator {

// @brief Structure for storing requests
class ServerComm : public COMM_BASE {
public:
    explicit ServerComm(const std::string &name = "", utils::Address *address = nullptr, DIRECTION direction = NONE);

    ~ServerComm() override;

    int has_request(const std::string &req_id) const;

    int has_response(const std::string &resp_id) const;

    int has_comm(const ::std::string &resp_address) const;

    int has_comm(const utils::Address *resp_address) const;

    int add_comm(::std::string &address);

    int add_comm(utils::Address *address);

    Comm_t *get_comm(const int idx = -1) const;

    int add_request(const std::string &req_id, utils::Address *response_address);

    int remove_request(size_t idx);

    int comm_nmsg() const override;

    datatypes::comm_head_t response_header(datatypes::comm_head_t head);

    int send(const char *data, const size_t &len) override;

    long recv(char **data, const size_t &len, bool allow_realloc) override;
    int send(const dtype_t* dtype) override;
    long recv(dtype_t* dtype) override;

private:
    ::std::vector<Comm_t *> comms; //!< Array of response comms.
    ::std::vector<std::string> response_id; //!< Response ids.
    ::std::vector<std::string> request_id; //!< Request ids.
    ::std::vector<size_t> comm_idx; //!< Index of comm associated w/ a request
    COMM_BASE *base_handle;
};

}
} // communication
