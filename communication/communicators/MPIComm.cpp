#include "MPIComm.hpp"
#include <boost/algorithm/string.hpp>
#ifdef MPIINSTALLED
#include <mpi.h>
#endif /*MPIINSTALLED*/

using namespace communication::communicator;
using namespace communication::utils;

#if defined(MPIINSTALLED) && defined(MPI_COMM_WORLD)

mpi_registry_t::~mpi_registry_t() {}

mpi_registry_t &mpi_registry_t::Clone() const {
    MPI_Comm ncomm;
    MPI_Comm_dup( this->comm, &ncomm);
    auto *clone = new mpi_registry_t(ncomm);
    return *clone;
}

void mpi_registry_t::CheckReturn(int code, std::string method, int rank) const {
  if (code == MPI_SUCCESS)
    return;
  else if (code == MPI_ERR_COMM)
    ygglog_error << method << "(" << tag << "): Invalid communicator" << std::endl;
  else if (code == MPI_ERR_TAG)
    ygglog_error << method << "(" << tag << "): Invalid tag" << std::endl;
  else if (code == MPI_ERR_RANK)
    ygglog_error << method << "(" << tag << "): Invalid rank '" <<
      rank << "'" << std::endl;
  else if (code == MPI_ERR_TYPE)
    ygglog_error << method << "(" << tag << "): Invalid datatype" << std::endl;
  else if (code == MPI_ERR_COUNT)
    ygglog_error << method << "(" << tag << "): Invalid count" << std::endl;
}

int mpi_registry_t::Probe(int source, MPI_Status *status) const {
  int out = MPI_Probe(source, tag, comm, status);
  CheckReturn(out, "Probe", source);
  if (status->MPI_ERROR) {
    ygglog_error << "Probe(" << tag << "): Error in status: " << status->MPI_ERROR << std::endl;
  }
  return out;
}

int mpi_registry_t::Send(const void *buf, int count, MPI_Datatype datatype, int dest) const {
  int out = MPI_Send(buf, count, datatype, dest, tag, comm);
  CheckReturn(out, "Send", dest);
  return out;
}

int mpi_registry_t::Recv(void *buf, int count, MPI_Datatype datatype, int source,
			 MPI_Status *status) const {
  int out = MPI_Recv(buf, count, datatype, source, tag, comm, status);
  CheckReturn(out, "Recv", source);
  return out;
}

MPIComm::MPIComm(const std::string &name, utils::Address *address, const DIRECTION direction) :
        CommBase(address, direction, MPI_COMM) {
    this->name = name;
    init();
}
MPIComm::MPIComm(const std::string &name, const DIRECTION direction) :
        CommBase(name, direction, MPI_COMM) {
    init();
}

void MPIComm::init() {
    //if (!(comm->flags & COMM_FLAG_VALID))
    //    return -1;
    maxMsgSize = 2147483647;
    if (this->address == nullptr)
        throw std::runtime_error("No address specified for MPIComm constructor");
    if (this->name.empty()) {
        this->name = "tempinitMPI." + address->address();
    }
    handle = new mpi_registry_t(MPI_COMM_WORLD);
    if (handle == nullptr) {
        ygglog_error << "init_mpi_comm: Could not alloc MPI registry." << std::endl;
        return;
    }
    handle->procs.clear();
    handle->tag = 0;
    std::vector<std::string> adrs;
    boost::split(adrs, this->address->address(), boost::is_any_of(","));
    addresses.push_back(this->address);
    if (adrs.size() > 1) {
        addresses[0]->address(adrs[0]);
        for (size_t i = 1; i < adrs.size(); i++) {
            addresses.push_back(new communication::utils::Address(adrs[i]));
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
}

MPIComm::~MPIComm() {
  for (size_t i = 1; i < addresses.size(); i++)
    delete addresses[i];
  addresses.clear();
}

int MPIComm::mpi_comm_source_id() const {
    if (direction == SEND)
        return 0;
    if (handle == nullptr) {
        ygglog_error << "mpi_comm_source_id(" << name << "): Queue handle is NULL." << std::endl;
        return -1;
    }
    //mpi_registry_t* reg = (mpi_registry_t*)(x->handle);
    MPI_Status status;
    int address = MPI_ANY_SOURCE;
    if (handle->Probe(address, &status) != MPI_SUCCESS) {
      ygglog_error << "mpi_comm_source_id(" << name << "): Error in probe" << std::endl;
      return -1;
    }
    if (status.MPI_ERROR) {
        ygglog_error << "mpi_comm_source_id(" << name << "): Error in status for tag = " << handle->tag
                     << ": " << status.MPI_ERROR << std::endl;
        return -1;
    }
    int flag;
    MPI_Test_cancelled(&status, &flag);
    if (flag) {
        ygglog_error << "mpi_comm_source_id(" << name << "): Request canceled for tag = " << handle->tag << std::endl;
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

int MPIComm::comm_nmsg() const {
    int src = mpi_comm_source_id();
    int nmsg = 0;
    if (src < 0) {
        ygglog_error << "MPIComm(" << name << ")::comm_nmsg: Error checking messages." << std::endl;
        return -1;
    } else if (src > 0) {
        nmsg = 1;
    }
    return nmsg;
}

int MPIComm::send_single(const char *data, const size_t &len) {
    ygglog_debug << "MPIComm(" << name << ")::send_single: " << len << " bytes" << std::endl;
    if (!check_size(len)) {
      ygglog_error << "MPIComm(" << name << ")::send_single: Message too large" << std::endl;
      return -1;
    }
    if (handle == nullptr) {
        ygglog_error << "MPIComm(" << name << ")::send_single: Queue handle is NULL." << std::endl;
        return -1;
    }
    int ret = (int)(len);
    int adr = static_cast<int>(handle->procs[handle->tag % handle->procs.size()]);
    handle->Send(&ret, 1, MPI_INT, adr);
    //if (MPI_Send(&ret, 1, MPI_INT, adr, handle->tag, handle)) {
    //    ygglog_error("mpi_comm_send(%s): Error sending message size for tag = %d.",
    //                 name.c_str(), handle->tag);
    //    return -1;
    //}
    handle->Send(data, ret, MPI_CHAR, adr);
    //if (MPI_Send(data, ret, MPI_CHAR, address, handle->tag, handle->comm)) {
    //    ygglog_error("mpi_comm_send(%s): Error sending message for tag = %d.",
    //                 name.c_str(), handle->tag);
    //    return -1;
    //}
    ygglog_debug << "MPIComm(" << name << ")::send_single: returning " <<  ret << std::endl;
    handle->tag++;
    return ret;
}

long MPIComm::recv_single(char*& data, const size_t &len, bool allow_realloc) {
    ygglog_debug << "MPIComm(" << name << ")::recv_single" << std::endl;
    MPI_Status status;
    int adr = mpi_comm_source_id();
    handle->Probe(adr, &status);
    if (status.MPI_ERROR) {
        ygglog_error << "MPIComm(" << name << ")::recv_single: Error in probe for tag = " << handle->tag << std::endl;
        return -1;
    }
    int ret = 0;
    handle->Recv(&ret, 1, MPI_INT, adr, &status);
    if (status.MPI_ERROR) {
        ygglog_error << "MPIComm(" << name << ")::recv_single: Error receiving message size for tag = " << handle->tag << std::endl;
        return -1;
    }
    ret = this->copyData(data, len, NULL, ret, allow_realloc);
    if (ret < 0) {
      ygglog_error << "MPIComm(" << name << ")::recv_single: Error reallocating data" << std::endl;
      return ret;
    }
    handle->Recv(data, ret, MPI_CHAR, adr, &status);
    if (status.MPI_ERROR) {
        // TODO: Check status?
        ygglog_error << "MPIComm(" << name << ")::recv_single: Error receiving message for tag = " << handle->tag << std::endl;
        return -1;
    }
    data[ret] = '\0';
    ygglog_debug << "MPIComm(" << name << ")::recv_single: returns " << ret << " bytes" << std::endl;
    handle->tag++;
    return ret;
}

// Definitions in the case where MPI libraries not installed
#else /*MPIINSTALLED*/

/*!
  @brief Print error message about MPI library not being installed.
 */
static inline
void mpi_install_error() {
  ygglog_throw_error("Compiler flag 'MPIINSTALLED' not defined so MPI bindings are disabled.");
}


MPIComm::MPIComm(const std::string &, utils::Address *address, const DIRECTION direction) :
  CommBase(address, direction, MPI_COMM), addresses() {
  mpi_install_error();
}
MPIComm::MPIComm(const std::string &name, const DIRECTION direction) :
  CommBase(name, direction, MPI_COMM), addresses() {
  mpi_install_error();
}

void MPIComm::init() {
  mpi_install_error();
}

MPIComm::~MPIComm() {
  // No error as constructor should have raised one
}

int MPIComm::mpi_comm_source_id() const {
  mpi_install_error();
  return -1;
}

int MPIComm::comm_nmsg() const {
  mpi_install_error();
  return -1;
}

int MPIComm::send_single(const char *, const size_t &) {
  mpi_install_error();
  return -1;
}

long MPIComm::recv_single(char*&, const size_t &, bool) {
  mpi_install_error();
  return -1;
}

#endif
