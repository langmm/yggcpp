#include "ServerComm.hpp"
#include "DefaultComm.hpp"
#include "utils/tools.hpp"

#ifdef COMM_BASE

using namespace communication::communicator;
using namespace communication::utils;

ServerComm::ServerComm(const std::string &name, Address *address,
                       int flgs) :
        COMM_BASE(name, address, RECV,
                  flgs | COMM_FLAG_SERVER | COMM_ALWAYS_SEND_HEADER),
        requests(SEND) {
    // Called to create temp comm for send/recv
    if (name.empty() && address && address->valid())
        return;
    init();
}

ServerComm::ServerComm(const std::string& name, int flgs) :
        COMM_BASE(name, RECV,
                  flgs | COMM_FLAG_SERVER | COMM_ALWAYS_SEND_HEADER),
        requests(SEND) {
    init();
}

void ServerComm::init() {
    if (this->name.empty()) {
        this->name = "server_request." + this->address->address();
    }
}

bool ServerComm::signon(const Header& header) {
    if (!(header.flags & HEAD_FLAG_CLIENT_SIGNON))
        return true;
    ygglog_debug << "ServerComm(" << name << ")::signon: begin" << std::endl;
    if (send(YGG_SERVER_SIGNON, YGG_SERVER_SIGNON_LEN) < 0) {
        ygglog_error << "ServerComm(" << name << ")::signon: Error in sending sign-on" << std::endl;
        return false;
    }
    return requests.signon_complete;
}

int ServerComm::update_datatype(const rapidjson::Value& new_schema,
                                const DIRECTION& dir) {
    if (dir == RECV)
        return COMM_BASE::update_datatype(new_schema, dir);
    requests.addResponseSchema(new_schema);
    return 1;
}

bool ServerComm::create_header_send(Header& header, const char* data, const size_t &len) {
    Comm_t* response_comm = requests.activeComm();
    if (response_comm == NULL) {
        ygglog_error << "ServerComm(" << name << ")::create_header_send: Failed to get response comm" << std::endl;
        return false;
    }
    if (response_comm->is_closed()) {
        ygglog_error << "ServerComm(" << name << ")::create_header_send: Response comm is closed" << std::endl;
        return false;
    }
    bool out = response_comm->create_header_send(header, data, len);
    if ((!out) || header.flags & HEAD_FLAG_EOF)
        return out;
    if (requests.addResponseServer(header, data, len) < 0) {
        ygglog_error << "ServerComm(" << name << ")::create_header_send: Failed to add response" << std::endl;
        header.invalidate();
        return false;
    }
    // This gives the server access to the ID of the message last received
    // if (!header.SetMetaString("id", address)) {
    //   header.invalidate();
    //   return 0;
    // }
    return true;
}

bool ServerComm::create_header_recv(Header& header, char*& data,
                                    const size_t &len,
                                    size_t msg_len, int allow_realloc,
                                    int temp) {
    bool out = COMM_BASE::create_header_recv(header, data, len,
                                             msg_len, allow_realloc, temp);
    if (!out)
        return out;
    if (header.flags & HEAD_FLAG_EOF) {
        if (temp || requests.partnerSignoff(header))
            return out;
        header.flags |= HEAD_FLAG_REPEAT;
        return true;
    }
    if (requests.addRequestServer(header) < 0) {
        ygglog_error << "ServerComm(" << name << ")::create_header_recv: Failed to add request" << std::endl;
        header.invalidate();
        return false;
    }
    if (header.flags & HEAD_FLAG_CLIENT_SIGNON) {
        if (!signon(header)) {
            header.invalidate();
            return false;
        }
        header.flags |= HEAD_FLAG_REPEAT;
    }
    return true;
}

int ServerComm::send_single(const char* data, const size_t &len,
                            const Header& header) {
    ygglog_debug << "ServerComm(" << name << ")::send_single: " << len << " bytes" << std::endl;
    Comm_t* response_comm = requests.activeComm();
    if (response_comm == NULL) {
        ygglog_error << "ServerComm(" << name << ")::send_single: Failed to get response comm" << std::endl;
        return -1;
    }
    int ret = response_comm->send_single(data, len, header);
    ygglog_debug << "ServerComm(" << name << ")::send_single: Sent " << len << " bytes" << std::endl;
    if (ret >= 0) {
        if (requests.popRequestServer() < 0)
            return -1;
    }
    return ret;
}


#endif
