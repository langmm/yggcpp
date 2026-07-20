#ifdef MPIINSTALLED
#define OMPI_SKIP_MPICXX 1
#include <mpi.h>
#endif /*MPIINSTALLED*/
#include "communicators/MPIComm.hpp"

using namespace YggInterface::communicator;
using namespace YggInterface::utils;

#ifdef MPIINSTALLED
bool _check_mpi_return_code(int code, std::string& msg) {
  msg = "";
  if (code == MPI_SUCCESS)
    return true;
  else if (code == MPI_ERR_COMM)
    msg = "Invalid communicator";
  else if (code == MPI_ERR_TAG)
    msg = "Invalid tag";
  else if (code == MPI_ERR_RANK)
    msg = "Invalid rank";
  else if (code == MPI_ERR_TYPE)
    msg = "Invalid datatype";
  else if (code == MPI_ERR_COUNT)
    msg = "Invalid count";
  return false;
}
#define CHECK_MPISTATUS_CODE_(method, context, rank)            \
  {                                                             \
    int error_code = method;                                    \
    std::string error_msg;                                      \
    if (!_check_mpi_return_code(error_code, error_msg)) {       \
      log_error() << context << " (tag = " <<                   \
        handle->tag << ", rank = " << rank << "): " <<          \
        error_msg << std::endl;                                 \
      return -1;                                                \
    }                                                           \
  }
#define CHECK_MPISTATUS_STATUS_(status, context, rank)  \
  {                                                     \
    if (status.MPI_ERROR) {                             \
      log_error() << context << " (tag = " <<           \
        handle->tag << ", rank = " << rank <<           \
        "): Error in status - " << status.MPI_ERROR <<  \
        std::endl;                                      \
      return -1;                                        \
    }                                                   \
  }
#define CHECK_MPISTATUS_(method, context, rank) \
  CHECK_MPISTATUS_CODE_(method, context, rank)  \
  CHECK_MPISTATUS_STATUS_(status, context, rank)

#endif // MPIINSTALLED

class mpi_registry_t::ImplMPI {
public:
#if defined(MPIINSTALLED)  // && defined(MPI_COMM_WORLD)
  ImplMPI() : comm(MPI_COMM_WORLD) {
    
  }
  MPI_Comm comm;
#endif // MPIINSTALLED
};

mpi_registry_t::mpi_registry_t() :
  procs(), tag(0), pImplMPI(std::make_unique<ImplMPI>()) {
}

mpi_registry_t::~mpi_registry_t() = default;

#if defined(MPIINSTALLED)

int mpi_registry_t::Probe(int source, void *status) const {
  return MPI_Probe(source, tag, pImplMPI->comm, (MPI_Status*)status);
}

int mpi_registry_t::Send(const void *buf, int count, void* datatype, int dest) const {
  return MPI_Send(buf, count, (MPI_Datatype)datatype, dest, tag, pImplMPI->comm);
}

int mpi_registry_t::Recv(void *buf, int count, void* datatype, int source,
			 void *status) const {
  return MPI_Recv(buf, count, (MPI_Datatype)datatype, source, tag, pImplMPI->comm, (MPI_Status*)status);
}

#else // MPIINSTALLED

int mpi_registry_t::Probe(int, void*) const { return -1; }
int mpi_registry_t::Send(const void*, int, int, int) const { return -1; }
int mpi_registry_t::Recv(void*, int, int, int, void*) const { return -1; }

#endif // MPIINSTALLED

COMM_CONSTRUCTOR_CORE_DEF(MPIComm, 0)

void MPIComm::_open(bool call_base) {
  BEFORE_OPEN_DEF;
#if defined(MPIINSTALLED) && defined(MPI_COMM_WORLD)
  updateMaxMsgSize(2147483647);
  if (!this->address.valid()) {
    if (ctx->for_testing_)
      address.address(std::to_string(0));
    else
      throw std::runtime_error("No address specified for MPIComm constructor");
  }
  if (this->name.empty()) {
    this->name = "tempinitMPI." + address.address();
  }
  handle = new mpi_registry_t();
  handle->procs.clear();
  handle->tag = 0;
  std::vector<std::string> adrs = YggInterface::utils::split(this->address.address(), ",");
  addresses.emplace_back(this->address.address());
  if (adrs.size() > 1) {
    addresses[0].address(adrs[0]);
    for (size_t i = 1; i < adrs.size(); i++) {
      addresses.emplace_back(adrs[i]);
    }
  }
  
  size_t ibeg, iend;
  
  for (const auto &a : adrs) {
    ibeg = a.find("[");
    iend = a.find("]");
    if (ibeg != std::string::npos) {
      handle->procs.push_back(stoi(a.substr(ibeg+1, iend-ibeg-1)));
    } else {
      handle->procs.push_back(stoi(a));
    }
  }
#else // MPIINSTALLED
  UNINSTALLED_ERROR(MPI);
#endif // MPIINSTALLED
  AFTER_OPEN_DEF;
}

void MPIComm::_close(bool call_base) {
  BEFORE_CLOSE_DEF;
  addresses.clear();
  AFTER_CLOSE_DEF;
}

std::vector<YggInterface::utils::Address>& MPIComm::getAddresses() {
  return addresses;
}

#if defined(MPIINSTALLED)

int MPIComm::mpi_comm_source_id() const {
    if (direction == SEND)
        return 0;
    if (!handle) {
        log_error() << "mpi_comm_source_id(" << name << "): Queue handle is NULL." << std::endl;
        return -1;
    }
    //mpi_registry_t* reg = (mpi_registry_t*)(x->handle);
    MPI_Status status;
    int address = MPI_ANY_SOURCE;
    CHECK_MPISTATUS_(handle->Probe(address, &status),
                     "mpi_comm_source_id: Error in probe", address);
    int flag;
    MPI_Test_cancelled(&status, &flag);
    if (flag) {
      log_error() << "mpi_comm_source_id(" << name << "): Request canceled for tag = " << handle->tag << std::endl;
      return -1;
    }
    int src = status.MPI_SOURCE;
    if (src > 0) {
        for (size_t i = 0; i < handle->procs.size(); i++) {
	    if (handle->procs[i] == (size_t)src) {
                return src;
            }
        }
    }
    return 0;
}

int MPIComm::nmsg(DIRECTION dir) const {
    if (global_comm)
      return global_comm->nmsg(dir);
    if (dir == NONE)
      dir = direction;
    if (dir != direction)
      return 0;
    int src = mpi_comm_source_id();
    int nmsg = 0;
    if (src < 0) {
        log_error() << "nmsg: Error checking messages." << std::endl;
        return -1;
    } else if (src > 0) {
        nmsg = 1;
    }
    return nmsg;
}

int MPIComm::send_single(utils::Header& header) {
    assert((!global_comm) && handle);
    if (header.on_send() < 0)
      return -1;
    log_debug() << "send_single: " << header.size_msg << " bytes" << std::endl;
    int ret = (int)(header.size_msg);
    int adr = static_cast<int>(handle->procs[handle->tag % handle->procs.size()]);
    CHECK_MPISTATUS_CODE_(handle->Send(&ret, 1, MPI_INT, adr),
                          "send_single: Error sending message size", adr);
    CHECK_MPISTATUS_CODE_(handle->Send(header.data_msg(), ret, MPI_CHAR, adr),
                          "send_single: Error receiving message", adr);
    log_debug() << "send_single: returning " <<  ret << std::endl;
    handle->tag++;
    return ret;
}

long MPIComm::recv_single(utils::Header& header) {
    assert(!global_comm);
    log_debug() << "recv_single" << std::endl;
    MPI_Status status;
    int adr = mpi_comm_source_id();
    CHECK_MPISTATUS_(handle->Probe(adr, &status),
                     "recv_single: Error in probe", adr);
    int ret = 0;
    CHECK_MPISTATUS_(handle->Recv(&ret, 1, MPI_INT, adr, &status),
                     "recv_single: Error receiving message size", adr);
    ret = static_cast<int>(header.on_recv(nullptr, ret));
    if (ret < 0) {
      log_error() << "recv_single: Error reallocating data" << std::endl;
      return ret;
    }
    CHECK_MPISTATUS_(handle->Recv(header.data_msg(), ret,
                                  MPI_CHAR, adr, &status),
                     "recv_single: Error receiving message", adr);
    header.data_msg()[ret] = '\0';
    ret = header.on_recv(header.data_msg(), ret);
    log_debug() << "recv_single: returns " << ret << " bytes" << std::endl;
    handle->tag++;
    return ret;
}

WORKER_METHOD_DEFS(MPIComm)

#undef CHECK_MPISTATUS_
#undef CHECK_MPISTATUS_STATUS_
#undef CHECK_MPISTATUS_CODE_

#else // MPIINSTALLED

int MPIComm::mpi_comm_source_id() const { return -1; }
int MPIComm::nmsg(DIRECTION) const { return -1; }
int MPIComm::send_single(utils::Header&) { return -1; }
long MPIComm::recv_single(utils::Header&) { return -1; }

#endif // MPIINSTALLED
