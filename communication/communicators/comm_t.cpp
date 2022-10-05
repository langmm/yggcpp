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

comm_t* open_comm(const char* address, DIRECTION dir, const COMM_TYPE &t) {
    auto ret = (comm_t*)malloc(sizeof(comm_t));
    if (ret == nullptr) {
        communication::utils::ygglog_error("new_comm_base: Failed to malloc comm.");
        return ret;
    }
    switch (t) {
        case IPC_COMM:
            ret->comm = (void*) new communication::communicator::IPCComm("", new communication::utils::Address(address), dir);
            break;
        case ZMQ_COMM:
            ret->comm = (void*) new communication::communicator::ZMQComm("", new communication::utils::Address(address), dir);
            break;
        case SERVER_COMM:
            ret->comm = (void*) new communication::communicator::ServerComm("", new communication::utils::Address(address), dir);
            break;
        case CLIENT_COMM:
            ret->comm = (void*) new communication::communicator::ClientComm("", new communication::utils::Address(address), dir);
            break;
        case ASCII_FILE_COMM:
            ret->comm = (void*) new communication::communicator::AsciiFileComm(address, dir);
            break;
        case ASCII_TABLE_COMM:
            ret->comm = (void*) new communication::communicator::AsciiTableComm(address, dir);
            break;
        case ASCII_TABLE_ARRAY_COMM:
            ret->comm = (void*) new AsciiTableArrayComm("", new communication::utils::Address(address), dir);
            break;
        case MPI_COMM:
            ret->comm = (void*) new communication::communicator::MPIComm("", new communication::utils::Address(address), dir);
            break;
        default:
            ret->comm = NULL;
            break;
    }
    return ret;
}

comm_t* ini_comm(const char* name, DIRECTION dir, const COMM_TYPE &t) {
    auto ret = (comm_t*)malloc(sizeof(comm_t));
    if (ret == nullptr) {
        communication::utils::ygglog_error("new_comm_base: Failed to malloc comm.");
        return ret;
    }
    switch (t) {
        case IPC_COMM:
            ret->comm = (void*) new communication::communicator::IPCComm(name, nullptr, dir);
            break;
        case ZMQ_COMM:
            ret->comm = (void*) new communication::communicator::ZMQComm(name, nullptr, dir);
            break;
        case SERVER_COMM:
            ret->comm = (void*) new communication::communicator::ServerComm(name, nullptr, dir);
            break;
        case CLIENT_COMM:
            ret->comm = (void*) new communication::communicator::ClientComm(name, nullptr, dir);
            break;
        case ASCII_FILE_COMM:
            ret->comm = (void*) new communication::communicator::AsciiFileComm(name, dir);
            break;
        case ASCII_TABLE_COMM:
            ret->comm = (void*) new communication::communicator::AsciiTableComm(name, dir);
            break;
        case ASCII_TABLE_ARRAY_COMM:
            ret->comm = (void*) new AsciiTableArrayComm(name, nullptr, dir);
            break;
        case MPI_COMM:
            ret->comm = (void*) new communication::communicator::MPIComm(name, nullptr, dir);
            break;
        default:
            ret->comm = NULL;
            break;
    }
    return ret;
}

int comm_send(comm_t* comm, const dtype_t* dtype) {
    if (comm == NULL)
        return -1;
    if (comm->comm == nullptr) {
        //communication::utils::ygglog_err("Communicator is null");
        return -1;
    }
    return static_cast<communication::communicator::Comm_t*>(comm->comm)->send(dtype);
}

int comm_recv(comm_t* comm, dtype_t* dtype) {
    if (comm == NULL)
        return -1;
}

int comm_nmsg(comm_t* comm) {
    if (comm == NULL)
        return -1;
}
