#include "comm_t.hpp"
#include "CommBase.hpp"
#include "utils/logging.hpp"
#include "utils/tools.hpp"

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

comm_t* open_comm(char* address, const DIRECTION dir, const COMM_TYPE &t) {
    auto ret = (comm_t*)malloc(sizeof(comm_t));
    if (ret == nullptr) {
        ygglog_error << "new_comm_base: Failed to malloc comm.";
        return ret;
    }
    std::string name = "";
    ret->comm = (void*) communication::communicator::new_Comm_t(dir, t, name, address);
    return ret;

    /*switch (t) {
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
        case MPI_COMM:
            ret->comm = (void*) new communication::communicator::MPIComm("", new communication::utils::Address(address), dir);
            break;
        default:
            ret->comm = NULL;
            break;
    }*/
    return ret;
}

comm_t* ini_comm(const char* name, DIRECTION dir, const COMM_TYPE &t) {
    auto ret = (comm_t*)malloc(sizeof(comm_t));
    if (ret == nullptr) {
        ygglog_error << "new_comm_base: Failed to malloc comm.";
        return ret;
    }
    ret->comm = (void*) communication::communicator::new_Comm_t(dir, t, name);
    /*switch (t) {
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
        case MPI_COMM:
            ret->comm = (void*) new communication::communicator::MPIComm(name, nullptr, dir);
            break;
        default:
            ret->comm = NULL;
            break;
    }*/
    return ret;
}


int comm_send(comm_t* comm, const char *data, const size_t len) {
  if (comm == NULL || comm->comm == nullptr)
    return -1;
  return static_cast<communication::communicator::Comm_t*>(comm->comm)->send(data, len);
}
long comm_recv(comm_t* comm, char *data, const size_t len) {
  if (comm == NULL || comm->comm == nullptr)
    return -1;
  return static_cast<communication::communicator::Comm_t*>(comm->comm)->recv(data, len, false);
}
long comm_recv_realloc(comm_t* comm, char **data, const size_t len) {
  if (comm == NULL || comm->comm == nullptr)
    return -1;
  return static_cast<communication::communicator::Comm_t*>(comm->comm)->recv(data[0], len, true);
}
int ncommSend(comm_t *comm, size_t nargs, ...) {
  if (comm == NULL || comm->comm == nullptr)
    return -1;
  YGGC_BEGIN_VAR_ARGS(ap, nargs, nargs, false);
  ygglog_debug << "ncommSend: nargs = " << nargs << std::endl;
  int ret = static_cast<communication::communicator::Comm_t*>(comm->comm)->vSend(ap);
  YGGC_END_VAR_ARGS(ap);
  return ret;
}
long ncommRecv(comm_t *comm, const int allow_realloc, size_t nargs, ...) {
  if (comm == NULL || comm->comm == nullptr)
    return -1;
  YGGC_BEGIN_VAR_ARGS(ap, nargs, nargs, allow_realloc);
  ygglog_debug << "ncommRecv: nargs = " << nargs << std::endl;
  long ret = static_cast<communication::communicator::Comm_t*>(comm->comm)->vRecv(ap);
  YGGC_END_VAR_ARGS(ap);
  return ret;
}

int comm_nmsg(comm_t* comm) {
    if (comm == NULL || comm->comm == nullptr)
        return -1;
    return static_cast<communication::communicator::Comm_t*>(comm->comm)->comm_nmsg();
}
