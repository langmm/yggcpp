#include "ServerComm.hpp"
#include "DefaultComm.hpp"

using namespace communication::communicator;
using namespace communication::utils;
using namespace communication::datatypes;

ServerComm::ServerComm(const std::string &name, Address *address, Direction direction, DataType* datatype) : COMM_BASE(name, address, direction, datatype){
    comms.clear();
    response_id.clear();
    request_id.clear();
    comm_idx.clear();

    // Called to create temp comm for send/recv
    if (name.empty() && address != nullptr && address->valid())
        return;
    // Called to initialize/create server comm
    DataType *dtype_in = create_dtype_format(this->direction, 0, false);

    if (this->name.empty()) {
        base_handle = new COMM_BASE("", this->address, RECV, dtype_in);
        base_handle->name = "server_request." + this->address->address();
    } else {
        base_handle = new COMM_BASE(this->name, nullptr, RECV, dtype_in);
    }
    base_handle->flags |= COMM_FLAG_SERVER;
    base_handle->init();
    this->address->address(base_handle->address->address());
    // printf("init_server_comm: name = %s, type=%d, address = %s\n",
    // 	 handle->name, handle->type, handle->address);
    this->direction = RECV;
    flags |= COMM_ALWAYS_SEND_HEADER;
}

ServerComm::~ServerComm(){
    for (auto c : comms) {
        delete c;
    }
    if (base_handle != nullptr)
        delete base_handle;
    comms.clear();
    response_id.clear();
    request_id.clear();
    comm_idx.clear();
}
int ServerComm::has_request(const std::string &req_id) const {
    for (size_t i = 0; i < request_id.size(); i++) {
        if (request_id[i] == req_id)
            return (int)i;
    }
    return -1;
}

int ServerComm::has_response(const std::string &resp_id) const {
    ygglog_debug("server_has_response: nreq = %ld", request_id.size());
    for (size_t i = 0; i < request_id.size(); i++) {
        if (response_id[i] == resp_id)
            return (int)i;
    }
    return -1;
}

int ServerComm::has_comm(const ::std::string &resp_address) const {
    for (size_t i = 0; i < comms.size(); i++) {
        if (comms[i]->address->address() == resp_address)
            return (int)i;
    }
    return -1;
}

int ServerComm::has_comm(const Address* resp_address) const {
    for (size_t i = 0; i < comms.size(); i++) {
        if (comms[i]->address == resp_address)
            return (int)i;
    }
    return -1;
}

int ServerComm::add_comm(::std::string &address, const DataType* datatype){
    return add_comm(new Address(address), datatype);
}
int ServerComm::add_comm(Address* address, const DataType* datatype){
    DataType* dtype_copy = copy_dtype(datatype);
    if (dtype_copy == nullptr) {
        ygglog_error("server_add_comm(%s): Failed to create dtype_copy.",
                     address->address().c_str());
        return -1;
    }
    auto *base = new COMM_BASE("", address, SEND, dtype_copy);
    base->flags |= COMM_ALLOW_MULTIPLE_COMMS;
    base->const_flags |= COMM_EOF_SENT | COMM_EOF_RECV;
    comms.push_back(base);
    ygglog_debug("server_add_comm(%s): Added comm %ld", address->address().c_str(), comms.size());
    return 0;
}
Comm_t* ServerComm::get_comm(const int idx) const{
    if (request_id.empty())
        return nullptr;
    if (idx < 0) {
        if (comm_idx[0] >= comms.size())
            return nullptr;
        return comms[comm_idx[0]];
    } else {
        if (idx >= comms.size())
            return nullptr;
        return comms[idx];
    }
}

int ServerComm::add_request(const std::string &req_id, Address* response_address, const DataType* datatype){
    ygglog_debug("server_add_request: adding request %s for address %s",
                 req_id.c_str(), response_address->address().c_str());
    std::string resp_id = req_id;
    std::string uuid;

    while (has_response(resp_id) >= 0) {
        uuid = std::to_string(rand());
        resp_id += uuid;
    }
    ygglog_debug("server_add_request: Response id = %s", resp_id.c_str());
    // request_id
    //size_t nrq = request_id.size();
    //request_id.resize(nrq + 1);
    //size_t request_len = req_id.size();
    request_id.push_back(req_id);

    // response_id
    //response_id.resize(nrq + 1);

    response_id.push_back(resp_id);
    // comm
    int c_idx = has_comm(response_address);
    if (c_idx < 0) {
        if (add_comm(new Address(response_address), datatype) < 0) {
            ygglog_error("server_add_request: Failed to add comm");
            return -1;
        }
        c_idx = comms.size() - 1;
        ygglog_debug("server_add_request: Added comm %ld (of %ld), %s",
                     c_idx, comms.size(), response_address);
    }

    comm_idx.push_back(static_cast<size_t>(c_idx));
    ygglog_debug("server_add_request: nreq = %ld, comm_idx = %ld",
                 request_id.size(), c_idx);
    return 0;
}
int ServerComm::remove_request(size_t idx){
    if (request_id.empty() || idx >= request_id.size())
        return -1;
    ygglog_debug("server_remove_request: Removing request %ld", idx);
    int nrem = request_id.size() - (idx + 1);

    // TODO: should this be a smart pointer?
    delete comms[idx];
    request_id.erase(request_id.begin() + idx);
    response_id.erase(response_id.begin() + idx);
    comm_idx.erase(comm_idx.begin() + idx);
    comms.erase(comms.begin() + idx);

    return 0;
}

int ServerComm::comm_nmsg() const {
    return base_handle->comm_nmsg();
}
comm_head_t ServerComm::response_header(comm_head_t head){
    if (request_id.empty()) {
        ygglog_error("server_response_header(%s): There are not any registered requests.",
                     name.c_str());
        head.flags = head.flags & ~HEAD_FLAG_VALID;
        return head;
    }
    // Add request ID to header
    head.request_id = info->request_id[0];
    ygglog_debug("server_response_header(%s): request_id = %s",
                 name.c_str(), head.request_id.c_str());
    return head;

}
int ServerComm::send(const char* data, const size_t &len){
    ygglog_debug("server_comm_send(%s): %d bytes", name.c_str(), len);
    if (comms.empty()) {
        ygglog_error("server_comm_send(%s): no response comm registered", name.c_str());
        return -1;
    }
    Comm_t* response_comm = get_comm(0);
    if (response_comm == nullptr) {
        ygglog_error("server_comm_send(%s): Failed to get response comm", name.c_str());
        return -1;
    }
    int ret = response_comm->send(data, len);
    ygglog_debug("server_comm_send(%s): Sent %d bytes", name.c_str(), len);
    if (remove_request(0) < 0)
        return -1;
    return ret;
}

long ServerComm::recv(char** data, const size_t &len, bool allow_realloc){
    ygglog_debug("server_comm_recv(%s)", name.c_str());
    if (base_handle == nullptr) {
        ygglog_error("server_comm_recv(%s): no request comm registered", name.c_str());
        return -1;
    }
    long ret = base_handle->recv(data, len, allow_realloc);
    if (ret < 0) {
        return ret;
    }
    // Return EOF
    if (is_eof(*data)) {
        base_handle->const_flags |= COMM_EOF_RECV;
        return ret;
    }
    // Initialize new comm from received address
    auto head = comm_head_t(*data, len);
    if (!(head.flags & HEAD_FLAG_VALID)) {
        ygglog_error("server_comm_recv(%s): Error parsing header.", name.c_str());
        return -1;
    }
    // Return EOF
    if (is_eof((*data) + head.bodybeg)) {
        base_handle->const_flags |= COMM_EOF_RECV;
        return ret;
    }
    // On client sign off, do a second recv
    if (strcmp((*data) + head.bodybeg, YGG_CLIENT_EOF) == 0) {
        return this->recv(data, len, allow_realloc);
    }
    // If there is not a response address
    if (head.response_address != nullptr || !head.response_address->valid()) {
        ygglog_error("server_comm_recv(%s): No response address in message.", name.c_str());
        return -1;
    }
    address->address(head.id);
    if (add_request(head.request_id, head.response_address,
                    datatype) < 0) return -1;
    return ret;
}
