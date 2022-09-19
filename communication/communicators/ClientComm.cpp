#include "ClientComm.hpp"

using namespace communication::communicator;
using namespace communication::utils;
using namespace communication::datatypes;

unsigned ClientComm::_client_rand_seeded = 0;

ClientComm::ClientComm(const std::string &name, Address *address, Direction direction, DataType* datatype) : COMM_BASE(name, address, direction, datatype) {
    comm = nullptr;
    request_id.clear();
    data.clear();
    len.clear();

    ygglog_debug("init_client_comm: Creating a client comm");
#ifdef _OPENMP
#pragma omp critical (client)
    {
#endif
        if (!(_client_rand_seeded)) {
            srand(ptr2seed(comm));
            _client_rand_seeded = 1;
        }
#ifdef _OPENMP
    }
#endif
    // Called to create temp comm for send/recv
    if (name.empty() && address != nullptr && address->valid())
        return;

    // Called to initialize/create client comm
    DataType *dtype_out = nullptr;
    if (this->direction != NONE) {
        dtype_out = create_dtype_format(this->direction, 0, false);
    }
    if (this->name.empty()) {
        base_handle = new COMM_BASE("", this->address, SEND, dtype_out);
        base_handle->name = "client_request." + this->address->address();
    } else {
        base_handle = new COMM_BASE(this->name, nullptr, SEND, dtype_out);
    }
    base_handle->flags |= COMM_FLAG_CLIENT;
    base_handle->init();
    this->address->address(base_handle->address->address());

    this->direction = SEND;
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
        ygglog_error("add_response: idx = %d", idx);
        return idx;
    }
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

int ClientComm::pop_response(const std::string& req_id, char** rdata,const size_t &rlen, const int allow_realloc) {
    int idx = has_response(req_id);
    if (idx < 0)
        return -1;
    size_t ret = len[idx];
    if ((ret + 1) > rlen) {
        if (allow_realloc) {
            ygglog_debug("client_pop_response: reallocating buffer from %d to %d bytes.",
                         rlen, ret + 1);
            (*rdata) = (char*)realloc(*rdata, ret + 1);
            if (*rdata == nullptr) {
                ygglog_error("client_pop_response: failed to realloc buffer.");
                return -1;
            }
        } else {
            ygglog_error("client_pop_response: buffer (%d bytes) is not large enough for message (%d bytes)",
                         rlen, ret + 1);
            return -((int)(ret));
        }
    }
    memcpy(*rdata, data[idx], ret);
    (*rdata)[ret] = '\0';
    if (remove_request(req_id) < 0)
        return -1;
    return static_cast<int>(ret);
}

int ClientComm::new_address() {
#ifdef _OPENMP
#pragma omp critical (client)
    {
#endif
        if (!(_client_rand_seeded)) {
            srand(ptr2seed(comm));
            _client_rand_seeded = 1;
        }
#ifdef _OPENMP
    }
#endif
    comm->type = _default_comm;
    return new_default_address(comm);

}


int ClientComm::comm_nmsg() const  {
    return base_handle->comm_nmsg();
}

comm_head_t ClientComm::response_header(comm_head_t head) {
    // Initialize new comm
    if (comm == nullptr) {
        DataType * dtype_copy = copy_dtype(datatype);
        comm = new COMM_BASE("", nullptr, RECV, dtype_copy);
        comm->flags |= COMM_FLAG_CLIENT_RESPONSE;
        //int ret = comm->new_address();
        comm->const_flags |= COMM_EOF_SENT | COMM_EOF_RECV;
        ygglog_debug("client_response_header(%s): Created response comm",
                     name.c_str());
    }
    // Add address & request ID to header
    head.response_address->address(comm->address->address());
    head.request_id = std::to_string(rand());
    if (add_request(head.request_id) < 0) {
        ygglog_error("client_response_header(%s): Failed to add request",
                     name.c_str());
        head.flags = head.flags & ~HEAD_FLAG_VALID;
        return head;
    }
    if (has_request(head.request_id) < 0) {
        ygglog_error("client_response_header(%s): Failed to add request",
                     name.c_str());
        head.flags = head.flags & ~HEAD_FLAG_VALID;
        return head;
    }
    ygglog_debug("client_response_header(%s): response_address = %s, request_id = %s",
                 name.c_str(), head.response_address, head.request_id.c_str());
    return head;
}

int ClientComm::send(const char* rdata, const size_t &rlen) {
    int ret;
    ygglog_debug("client_comm_send(%s): %d bytes", name.c_str(), rlen);
    if (base_handle == nullptr) {
        ygglog_error("client_comm_send(%s): no request comm registered", name.c_str());
        return -1;
    }
    ret = base_handle->send(rdata, rlen);
    if (is_eof(rdata)) {
        base_handle->const_flags |= COMM_EOF_SENT;
    }
    return ret;
}

long ClientComm::recv(char** rdata, const size_t &rlen, bool allow_realloc)  {
    ygglog_debug("client_comm_recv(%s)", name.c_str());
    if (comm == nullptr) {
        ygglog_error("client_comm_recv(%s): no response struct set up", name.c_str());
        return -1;
    }
    if (request_id.empty()) {
        ygglog_error("client_comm_recv(%s): no response comm registered", name.c_str());
        return -1;
    }
    std::string req_id = request_id[0];
    long ret = 0;
    while (has_response(req_id) < 0) {
        ret = comm->recv(rdata, rlen, allow_realloc);
        if (ret < 0) {
            ygglog_error("client_comm_recv(%s): default_comm_recv returned %d",
                         name.c_str(), ret);
            return ret;
        }
        auto head = comm_head_t(*rdata, rlen);
        if (!(head.flags & HEAD_FLAG_VALID)) {
            ygglog_error("client_comm_recv(%s): Invalid header.", name.c_str());
            return -1;
        }
        if (head.request_id == req_id) {
            ygglog_debug("client_comm_recv(%s): default_comm_recv returned %d",
                         name.c_str(), ret);
            if (remove_request(req_id) < 0) {
                ygglog_error("client_comm_recv(%s): Failed to remove request %s",
                             name.c_str(), req_id.c_str());
                return -1;
            }
            return ret;
        }
        if (add_response(head.request_id, *rdata, ret) < 0) {
            ygglog_error("client_comm_recv(%s): Failed to add response %s",
                         name.c_str(), head.request_id.c_str());
            return -1;
        }
    }
    ret = pop_response(req_id, rdata, rlen, allow_realloc);
    // Close response comm and decrement count of response comms
    ygglog_debug("client_comm_recv(%s): client_pop_response returned %d",
                 name.c_str(), ret);
    return ret;
}

