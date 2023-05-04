#pragma once

#include <boost/python.hpp>
#include "communicators/CommBase.hpp"

namespace communication {
namespace python {

    struct CommWrap : communicator::Comm_t, boost::python::wrapper<communicator::Comm_t> {
        CommWrap() = delete;
        CommWrap(utils::Address *address, DIRECTION direction, const COMM_TYPE &t, int flgs = 0) :
                communicator::Comm_t(address, direction, t, flgs) {}
        CommWrap(const std::string &name, DIRECTION direction = NONE, const COMM_TYPE &t = NULL_COMM) :
                communicator::Comm_t(name, direction, t) {}

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
        //long recv(char *data, const size_t &len, bool allow_realloc) {
        //    return this->get_override("recv")(data, len, allow_realloc);
        //}
    };
    void exportUtils();
    void exportCommunicators();
}
}