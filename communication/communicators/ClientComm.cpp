#include "ClientComm.hpp"
#include "utils/tools.hpp"

using namespace communication::communicator;
using namespace communication::utils;
using namespace communication::datatypes;

unsigned ClientComm::_client_rand_seeded = 0;

ClientComm::ClientComm(const std::string &name, Address *address, DIRECTION direction) :
        COMM_BASE(name, address, direction) {
    comm = nullptr;
    request_id.clear();
    data.clear();
    len.clear();

    ygglog_debug << "init_client_comm: Creating a client comm";
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

    if (this->direction != NONE) {
        dtype_out = create_dtype_format(this->direction, 0, false);
    }
    if (this->name.empty()) {
        base_handle = new COMM_BASE("", this->address, SEND);
        base_handle->name = "client_request." + this->address->address();
    } else {
        base_handle = new COMM_BASE(this->name, nullptr, SEND);
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
        ygglog_error << "add_response: idx = " << idx;
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
            (*rdata) = (char*)realloc(*rdata, ret + 1);
            if (*rdata == nullptr) {
            ygglog_debug << "client_pop_response: reallocating buffer from " << rlen << " to "
            << ret + 1 << " bytes.";
                ygglog_error << "client_pop_response: failed to realloc buffer.";
                return -1;
            }
        } else {
            ygglog_error << "client_pop_response: buffer (" << rlen << " bytes) is not large enough for message ("
                         << ret + 1 << " bytes)";
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

communication::datatypes::CommHead ClientComm::response_header(datatypes::CommHead head) {
    // Initialize new comm
    if (comm == nullptr) {
        comm = new COMM_BASE("", nullptr, RECV);
        comm->flags |= COMM_FLAG_CLIENT_RESPONSE;
        //int ret = comm->new_address();
        comm->const_flags |= COMM_EOF_SENT | COMM_EOF_RECV;
        ygglog_debug << "client_response_header(" << name << "): Created response comm";
    }
    // Add address & request ID to header
    head.response_address->address(comm->address->address());
    head.request_id = std::to_string(rand());
    if (add_request(head.request_id) < 0) {
        ygglog_error << "client_response_header(" << name << "): Failed to add request";
        head.flags = head.flags & ~HEAD_FLAG_VALID;
        return head;
    }
    if (has_request(head.request_id) < 0) {
        ygglog_error << "client_response_header(" << name << "): Failed to add request";
        head.flags = head.flags & ~HEAD_FLAG_VALID;
        return head;
    }
    ygglog_debug << "client_response_header(" << name << "): response_address = " << head.response_address
                 << ", request_id = " << head.request_id;
    return head;
}

int ClientComm::send(const char* rdata, const size_t &rlen) {
    int ret;
    ygglog_debug << "client_comm_send(" << name << "): " << rlen << " bytes";
    if (base_handle == nullptr) {
        ygglog_error <<"client_comm_send(" << name << "): no request comm registered";
        return -1;
    }
    ret = base_handle->send(rdata, rlen);
    if (is_eof(rdata)) {
        base_handle->const_flags |= COMM_EOF_SENT;
    }
    return ret;
}

long ClientComm::recv(char** rdata, const size_t &rlen, bool allow_realloc)  {
    ygglog_debug << "client_comm_recv(" << name << ")";
    if (comm == nullptr) {
        ygglog_error << "client_comm_recv(" << name << "): no response struct set up";
        return -1;
    }
    if (request_id.empty()) {
        ygglog_error << "client_comm_recv(" << name << "): no response comm registered";
        return -1;
    }
    std::string req_id = request_id[0];
    long ret = 0;
    while (has_response(req_id) < 0) {
        ret = comm->recv(rdata, rlen, allow_realloc);
        if (ret < 0) {
            ygglog_error << "client_comm_recv(" << name << "): default_comm_recv returned " << ret;
            return ret;
        }
        auto head = datatypes::CommHead(*rdata, rlen);
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
        if (add_response(head.request_id, *rdata, ret) < 0) {
            ygglog_error << "client_comm_recv(" << name << "): Failed to add response " << head.request_id;
            return -1;
        }
    }
    ret = pop_response(req_id, rdata, rlen, allow_realloc);
    // Close response comm and decrement count of response comms
    ygglog_debug << "client_comm_recv(" << name << "): client_pop_response returned %d";
    return ret;
}

