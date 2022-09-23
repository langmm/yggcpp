#include "comm_t.hpp"
#include "comms.hpp"
#include "utils/tools.hpp"
#include "utils/logging.hpp"

void free_comm(comm_t* comm) {
    if (comm == nullptr)
        return;
    if (comm->comm != nullptr) {
        auto c = static_cast<communication::communicator::Comm_t *>(comm->comm);
        delete c;
        comm->comm = nullptr;
    }
}

void close_comm(comm_t* comm) {
    free_comm(comm);
}

comm_t* open_comm(const char* address, Direction dir, const comm_type &t, dtype_t* datatype) {
    comm_t* ret = (comm_t*)malloc(sizeof(comm_t));
    if (ret == NULL) {
        communication::utils::ygglog_error("new_comm_base: Failed to malloc comm.");
        return ret;
    }
    switch (t) {
        case IPC_COMM:
            ret->comm = (void*) new communication::communicator::IPCComm("", new communication::utils::Address(address), dir, static_cast<communication::datatypes::DataType*>(datatype->obj));
            break;
        case ZMQ_COMM:
            ret->comm = (void*) new communication::communicator::ZMQComm("", new communication::utils::Address(address), dir, static_cast<communication::datatypes::DataType*>(datatype->obj));
            break;
        case SERVER_COMM:
            ret->comm = (void*) new communication::communicator::ServerComm("", new communication::utils::Address(address), dir, static_cast<communication::datatypes::DataType*>(datatype->obj));
            break;
        case CLIENT_COMM:
            ret->comm = (void*) new communication::communicator::ClientComm("", new communication::utils::Address(address), dir, static_cast<communication::datatypes::DataType*>(datatype->obj));
            break;
        case ASCII_FILE_COMM:
            ret->comm = (void*) new communication::communicator::AsciiFileComm(address, dir, static_cast<communication::datatypes::DataType*>(datatype->obj));
            break;
        case ASCII_TABLE_COMM:
            ret->comm = (void*) new communication::communicator::AsciiTableComm(address, dir, static_cast<communication::datatypes::DataType*>(datatype->obj));
            break;
        case ASCII_TABLE_ARRAY_COMM:
            ret->comm = (void*) new AsciiTableArrayComm("", new communication::utils::Address(address), dir, static_cast<communication::datatypes::DataType*>(datatype->obj));
            break;
        case MPI_COMM:
            ret->comm = (void*) new communication::communicator::MPIComm("", new communication::utils::Address(address), dir, static_cast<communication::datatypes::DataType*>(datatype->obj));
            break;
        default:
            ret->comm = NULL;
            break;
    }
    return ret;
}

comm_t* ini_comm(const char* name, Direction dir, const comm_type &t, const dtype_t* datatype) {
    comm_t* ret = (comm_t*)malloc(sizeof(comm_t));
    if (ret == NULL) {
        communication::utils::ygglog_error("new_comm_base: Failed to malloc comm.");
        return ret;
    }
    switch (t) {
        case IPC_COMM:
            ret->comm = (void*) new communication::communicator::IPCComm(name, nullptr, dir, static_cast<communication::datatypes::DataType*>(datatype->obj));
            break;
        case ZMQ_COMM:
            ret->comm = (void*) new communication::communicator::ZMQComm(name, nullptr, dir, static_cast<communication::datatypes::DataType*>(datatype->obj));
            break;
        case SERVER_COMM:
            ret->comm = (void*) new communication::communicator::ServerComm(name, nullptr, dir, static_cast<communication::datatypes::DataType*>(datatype->obj));
            break;
        case CLIENT_COMM:
            ret->comm = (void*) new communication::communicator::ClientComm(name, nullptr, dir, static_cast<communication::datatypes::DataType*>(datatype->obj));
            break;
        case ASCII_FILE_COMM:
            ret->comm = (void*) new communication::communicator::AsciiFileComm(name, dir, static_cast<communication::datatypes::DataType*>(datatype->obj));
            break;
        case ASCII_TABLE_COMM:
            ret->comm = (void*) new communication::communicator::AsciiTableComm(name, dir, static_cast<communication::datatypes::DataType*>(datatype->obj));
            break;
        case ASCII_TABLE_ARRAY_COMM:
            ret->comm = (void*) new AsciiTableArrayComm(name, nullptr, dir, static_cast<communication::datatypes::DataType*>(datatype->obj));
            break;
        case MPI_COMM:
            ret->comm = (void*) new communication::communicator::MPIComm(name, nullptr, dir, static_cast<communication::datatypes::DataType*>(datatype->obj));
            break;
        default:
            ret->comm = NULL;
            break;
    }
    return ret;
}

int comm_send(comm_t* comm, const char *data, const size_t &len) {
    if (comm == NULL)
        return -1;
    if (comm->comm == nullptr) {
        communication::utils::ygg
    }
}

int comm_recv(comm_t* comm, char **data, const size_t &len, bool allow_realloc) {
    if (comm == NULL)
        return -1;
}

int comm_nmsg(comm_t* comm) {
    if (comm == NULL)
        return -1;
}
