#include "ClientComm.hpp"
#include "utils/tools.hpp"

#ifdef COMM_BASE

using namespace communication::communicator;
using namespace communication::utils;
using namespace communication::datatypes;

unsigned ClientComm::_client_rand_seeded = 0;

ClientComm::ClientComm(const std::string &name, Address *address) :
        COMM_BASE(name, address, SEND) {
    request_id.clear();
    data.clear();
    len.clear();

    ygglog_debug << "init_client_comm: Creating a client comm";
#ifdef _OPENMP
#pragma omp critical (client)
    {
#endif
        if (!(_client_rand_seeded)) {
            srand(ptr2seed(this));
            _client_rand_seeded = 1;
        }
#ifdef _OPENMP
    }
#endif
    // Called to create temp comm for send/recv
    if (name.empty() && address != nullptr && address->valid())
        return;

    // Called to initialize/create client comm

    if (name.empty()) {
        this->name = "client_request." + this->address->address();
    }
    flags |= COMM_FLAG_CLIENT;
    flags |= COMM_ALWAYS_SEND_HEADER;
}

ClientComm::~ClientComm() {
    request_id.clear();
    for (auto d : data) {
        if (d != nullptr)
            free(d);
    }
    data.clear();
    len.clear();
}

int ClientComm::has_request(const std::string &req_id) {
    for (size_t i = 0; i < request_id.size(); i++) {
        if (request_id[i] == req_id)
            return (int)i;
    }
    return -1;
}

int ClientComm::has_response(const std::string &req_id) {
    int idx = has_request(req_id);
    if (idx < 0)
        return idx;
    if (data[idx] != nullptr)
        return idx;
    return -1;
}

int ClientComm::add_request(const std::string& req_id) {
    request_id.push_back(req_id);
    data.push_back(nullptr);
    len.push_back(0);
    return 0;
}

int ClientComm::add_response(const std::string& req_id, const char* rdata, const size_t &rlen) {
    int idx = has_request(req_id);
    if (idx < 0) {
        ygglog_error << "add_response: idx = " << idx;
        return idx;
    }
    if (data[idx] == nullptr)
        data[idx] = (char*)malloc(sizeof(char) * (rlen + 1));
    else
        data[idx] = (char*)realloc(data[idx], sizeof(char) * (rlen + 1));
    memcpy(data[idx], rdata, rlen);
    data[idx][rlen] = '\0';
    len[idx] = rlen;
    return 0;
}

int ClientComm::remove_request(const std::string& req_id) {
    int idx = has_request(req_id);
    if (idx < 0)
        return 0;
    if (data[idx] != nullptr)
        free(data[idx]);
    request_id.erase(request_id.begin() + idx);
    data.erase(data.begin() + idx);
    len.erase(len.begin() + idx);
    return 0;
}

int ClientComm::pop_response(const std::string& req_id, char* rdata,const size_t &rlen, const int allow_realloc) {
    int idx = has_response(req_id);
    if (idx < 0)
        return -1;
    size_t ret = len[idx];
    if ((ret + 1) > rlen) {
        if (allow_realloc) {
            ygglog_debug << "client_pop_response: reallocating buffer from " << rlen << " to "
            << ret + 1 << " bytes.";
            rdata = (char*)realloc(rdata, ret + 1);
            if (rdata == nullptr) {
                ygglog_error << "client_pop_response: failed to realloc buffer.";
                return -1;
            }
        } else {
            ygglog_error << "client_pop_response: buffer (" << rlen << " bytes) is not large enough for message ("
                         << ret + 1 << " bytes)";
            return -((int)(ret));
        }
    }
    memcpy(rdata, data[idx], ret);
    rdata[ret] = '\0';
    if (remove_request(req_id) < 0)
        return -1;
    return static_cast<int>(ret);
}

bool ClientComm::new_address() {
#ifdef _OPENMP
#pragma omp critical (client)
    {
#endif
        if (!(_client_rand_seeded)) {
            srand(ptr2seed(this));
            _client_rand_seeded = 1;
        }
#ifdef _OPENMP
    }
#endif
    type = _default_comm;
    return COMM_BASE::new_address();
}


int ClientComm::comm_nmsg() const  {
    return COMM_BASE::comm_nmsg();
}

void ClientComm::response_header(datatypes::CommHead &head) {
    // Initialize new comm
    // Add address & request ID to header
    head.response_address->address(address->address());
    head.request_id = std::to_string(rand());
    if (add_request(head.request_id) < 0) {
        ygglog_error << "client_response_header(" << name << "): Failed to add request";
        head.flags = head.flags & ~HEAD_FLAG_VALID;
        return;
    }
    if (has_request(head.request_id) < 0) {
        ygglog_error << "client_response_header(" << name << "): Failed to add request";
        head.flags = head.flags & ~HEAD_FLAG_VALID;
        return;
    }
    ygglog_debug << "client_response_header(" << name << "): response_address = " << head.response_address
                 << ", request_id = " << head.request_id;
    return;
}

int ClientComm::send(const char* rdata, const size_t &rlen) {
    int ret;
    ygglog_debug << "client_comm_send(" << name << "): " << rlen << " bytes";
    ret = COMM_BASE::send(rdata, rlen);
    if (is_eof(rdata)) {
        const_flags |= COMM_EOF_SENT;
    }
    return ret;
}

long ClientComm::recv(char* rdata, const size_t &rlen, bool allow_realloc)  {
    ygglog_debug << "client_comm_recv(" << name << ")";
    if (request_id.empty()) {
        ygglog_error << "client_comm_recv(" << name << "): no response comm registered";
        return -1;
    }
    std::string req_id = request_id[0];
    long ret = 0;
    while (has_response(req_id) < 0) {
        ret = COMM_BASE::recv(rdata, rlen, allow_realloc);
        if (ret < 0) {
            ygglog_error << "client_comm_recv(" << name << "): default_comm_recv returned " << ret;
            return ret;
        }
        auto head = datatypes::CommHead(rdata, rlen);
        if (!(head.flags & HEAD_FLAG_VALID)) {
            ygglog_error << "client_comm_recv(" << name << "): Invalid header.";
            return -1;
        }
        if (head.request_id == req_id) {
            ygglog_debug << "client_comm_recv(" << name << "): default_comm_recv returned " << ret;
            if (remove_request(req_id) < 0) {
                ygglog_error << "client_comm_recv(" << name << "): Failed to remove request " << req_id;
                return -1;
            }
            return ret;
        }
        if (add_response(head.request_id, rdata, ret) < 0) {
            ygglog_error << "client_comm_recv(" << name << "): Failed to add response " << head.request_id;
            return -1;
        }
    }
    ret = pop_response(req_id, rdata, rlen, allow_realloc);
    // Close response comm and decrement count of response comms
    ygglog_debug << "client_comm_recv(" << name << "): client_pop_response returned %d";
    return ret;
}

#endif