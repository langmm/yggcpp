#include "MPIComm.hpp"
#ifdef MPIINSTALLED
#include <mpi.h>
#endif /*MPIINSTALLED*/

using namespace communication::communicator;
using namespace communication::utils;

#if defined(MPIINSTALLED) && defined(MPI_COMM_WORLD)

mpi_registry_t::~mpi_registry_t() {}

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

#endif

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

MPIComm::MPIComm(const std::string name, utils::Address *address,
		 const DIRECTION direction, int flgs,
		 const COMM_TYPE type) :
  CommBase(name, address, direction, type, flgs) {
  if (!global_comm)
    init();
}

ADD_CONSTRUCTORS_DEF(MPIComm)

#if defined(MPIINSTALLED) && defined(MPI_COMM_WORLD)

void MPIComm::init() {
    updateMaxMsgSize(2147483647);
    assert(!handle);
    if (!(this->address && this->address->valid())) {
#ifdef YGG_TEST
        if (!address) {
	  address = new utils::Address(std::to_string(0));
	} else { // GCOVR_EXCL_LINE
	  address->address(std::to_string(0));
	}
#else // YGG_TEST
        throw std::runtime_error("No address specified for MPIComm constructor");
#endif // YGG_TEST
    }
    if (this->name.empty()) {
        this->name = "tempinitMPI." + address->address();
    }
    handle = new mpi_registry_t(MPI_COMM_WORLD);
    handle->procs.clear();
    handle->tag = 0;
    std::vector<std::string> adrs = communication::utils::split(this->address->address(), ",");
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
    CommBase::init();
}

void MPIComm::close() {
  for (size_t i = 1; i < addresses.size(); i++)
    delete addresses[i];
  addresses.clear();
  CommBase::close();
}

int MPIComm::mpi_comm_source_id() const {
#if defined(MPIINSTALLED) && defined(MPI_COMM_WORLD)  
    if (direction == SEND)
        return 0;
    if (!handle) {
        ygglog_error << "mpi_comm_source_id(" << name << "): Queue handle is NULL." << std::endl;
        return -1;
    }
    //mpi_registry_t* reg = (mpi_registry_t*)(x->handle);
    MPI_Status status;
    int address = MPI_ANY_SOURCE;
    if ((handle->Probe(address, &status) != MPI_SUCCESS) ||
	status.MPI_ERROR) {
      ygglog_error << "mpi_comm_source_id(" << name << "): Error in probe" << std::endl;
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
#else
    return -1;
#endif
}

int MPIComm::comm_nmsg(DIRECTION dir) const {
    if (global_comm)
      return global_comm->comm_nmsg(dir);
    if (dir == NONE)
      dir = direction;
    if (dir != direction)
      return 0;
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

int MPIComm::send_single(utils::Header& header) {
    assert((!global_comm) && handle);
    if (header.on_send() < 0)
      return -1;
    ygglog_debug << "MPIComm(" << name << ")::send_single: " << header.size_msg << " bytes" << std::endl;
    int ret = (int)(header.size_msg);
    int adr = static_cast<int>(handle->procs[handle->tag % handle->procs.size()]);
    if (handle->Send(&ret, 1, MPI_INT, adr) != MPI_SUCCESS) {
      ygglog_error << "MPIComm(" << name << ")::send_single: Error sending message size for tag = " << handle->tag << std::endl;
      return -1;
    }
    if (handle->Send(header.data_msg(), ret, MPI_CHAR, adr) != MPI_SUCCESS) {
      ygglog_error << "MPIComm(" << name << ")::send_single: Error receiving message for tag = " << handle->tag << std::endl;
      return -1;
    }
    ygglog_debug << "MPIComm(" << name << ")::send_single: returning " <<  ret << std::endl;
    handle->tag++;
    return ret;
}

long MPIComm::recv_single(utils::Header& header) {
    assert(!global_comm);
    ygglog_debug << "MPIComm(" << name << ")::recv_single" << std::endl;
    MPI_Status status;
    int adr = mpi_comm_source_id();
    if (handle->Probe(adr, &status) != MPI_SUCCESS || status.MPI_ERROR) {
        ygglog_error << "MPIComm(" << name << ")::recv_single: Error in probe for tag = " << handle->tag << std::endl;
        return -1;
    }
    int ret = 0;
    if (handle->Recv(&ret, 1, MPI_INT, adr, &status) != MPI_SUCCESS ||
	status.MPI_ERROR) {
        ygglog_error << "MPIComm(" << name << ")::recv_single: Error receiving message size for tag = " << handle->tag << std::endl;
        return -1;
    }
    ret = header.on_recv(NULL, ret);
    if (ret < 0) {
      ygglog_error << "MPIComm(" << name << ")::recv_single: Error reallocating data" << std::endl;
      return ret;
    }
    if (handle->Recv(header.data_msg(), ret,
		     MPI_CHAR, adr, &status) != MPI_SUCCESS ||
	status.MPI_ERROR) {
        ygglog_error << "MPIComm(" << name << ")::recv_single: Error receiving message for tag = " << handle->tag << std::endl;
        return -1;
    }
    header.data_msg()[ret] = '\0';
    ret = header.on_recv(header.data_msg(), ret);
    ygglog_debug << "MPIComm(" << name << ")::recv_single: returns " << ret << " bytes" << std::endl;
    handle->tag++;
    return ret;
}

WORKER_METHOD_DEFS(MPIComm)

#endif
