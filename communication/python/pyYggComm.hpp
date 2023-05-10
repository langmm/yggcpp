#pragma once

#include <boost/python.hpp>
#include "communicators/CommBase.hpp"

namespace communication {
namespace python {

    struct CommWrap : communicator::Comm_t, boost::python::wrapper<communicator::Comm_t> {
        CommWrap() = delete;
        CommWrap(utils::Address *address, DIRECTION direction, const COMM_TYPE &t, int flgs = 0) :
                communicator::Comm_t(address, direction, t, flgs) {}
        CommWrap(const std::string &name, DIRECTION direction = NONE, const COMM_TYPE &t = NULL_COMM, int flgs = 0) :
                communicator::Comm_t(name, direction, t, flgs) {}

        int comm_nmsg() const {
            return this->get_override("comm_nmsg")();
        }
        void init() {
            this->get_override("init")();
        }
        void reset() {
            this->get_override("reset")();
        }
        int send(const char *data, const size_t &len) {
            return this->get_override("send")(data, len);
        }
        void close() {
            this->get_override("close")();
        }
        bool is_closed() const {
            return this->get_override("is_closed")();
        }
        communicator::Comm_t* create_worker(utils::Address* adr, const DIRECTION dirn, int flgs) {
            return this->get_override("create_worker")(adr, dirn, flgs);
        }
        int send_single(const char* data, const size_t &len, const utils::Header& header) {
            return this->get_override("send_single")(data, len, header);
        }
        long recv_single(char*& data, const size_t &len, bool allow_realloc) {
            return this->get_override("recv_single")(data, len, allow_realloc);
        }
        //long recv(char *data, const size_t &len, bool allow_realloc) {
        //    return this->get_override("recv")(data, len, allow_realloc);
        //}
    };
    void exportUtils();
    void exportCommunicators();
}
}