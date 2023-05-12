#pragma once

#include <boost/python.hpp>
#include "communicators/CommBase.hpp"

namespace bp = boost::python;
namespace communication {
namespace python {

    struct CommWrap : communicator::Comm_t, bp::wrapper<communicator::Comm_t> {
        CommWrap() = delete;
        CommWrap(utils::Address *address, DIRECTION direction, const COMM_TYPE &t, int flgs = 0) :
                communicator::Comm_t(address, direction, t, flgs) {}
        explicit CommWrap(const std::string &name, DIRECTION direction = NONE, const COMM_TYPE &t = NULL_COMM, int flgs = 0) :
                communicator::Comm_t(name, direction, t, flgs) {}

        int comm_nmsg() const override {
            return this->get_override("comm_nmsg")();
        }
        void init() override{
            this->get_override("init")();
        }
        void reset() override{
            this->get_override("reset")();
        }
        int send(const char *data, const size_t &len) override{
            return this->get_override("send")(data, len);
        }
        void close() override{
            this->get_override("close")();
        }
        bool is_closed() const override{
            return this->get_override("is_closed")();
        }
        communicator::Comm_t* create_worker(utils::Address* adr, const DIRECTION dirn, int flgs) override {
            return this->get_override("create_worker")(adr, dirn, flgs);
        }
        int send_single(const char* data, const size_t &len, const utils::Header& header) override {
            return this->get_override("send_single")(data, len, header);
        }
        long recv_single(char*& data, const size_t &len, bool allow_realloc) override {
            return this->get_override("recv_single")(data, len, allow_realloc);
        }
        long recv(char *&data, const size_t &len, bool allow_realloc) override {
            return this->get_override("recv")(data, len, allow_realloc);
        }

        //bp::object send_va(bp::tuple args);
    };
    void exportUtils();
    void exportCommunicators();
}
}