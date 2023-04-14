#pragma once

#ifdef MPIINSTALLED
#include <mpi.h>
#endif /*MPIINSTALLED*/
#include "CommBase.hpp"
#include <vector>

namespace communication {
namespace communicator {

#if defined(MPIINSTALLED) && defined(MPI_COMM_WORLD)


class mpi_registry_t {
public:
    explicit mpi_registry_t(MPI_Comm comm0) : comm(comm0), tag(0) {}

    MPI_Comm comm;
    std::vector<size_t> procs; //!< IDs for partner processes.
    int tag; //!< Tag for next message.
    mpi_registry_t &Clone() const;
    virtual int Probe(int source, MPI_Status *status) const;
    virtual int Send(const void *buf, int count, MPI_Datatype datatype, int dest) const;
    virtual int Recv(void *buf, int count, MPI_Datatype datatype, int source,
		     MPI_Status *status) const;
private:
  void CheckReturn(int code, std::string method, int rank=0) const ;
};


class MPIComm : public CommBase<mpi_registry_t, int> {
#else
    class MPIComm : public CommBase<void,void> {};
#endif
public:
    MPIComm(const std::string &name, utils::Address *address, const DIRECTION direction);

    //explicit MPIComm(const Comm_t* comm);
    ~MPIComm() override;

    int comm_nmsg() const override;

    int mpi_comm_source_id() const;
    using Comm_t::send;
    using Comm_t::recv;

protected:
    int send(const char *data, const size_t &len) override;

    long recv(char *data, const size_t &len, bool allow_realloc) override;
#ifndef YGG_TEST
private:
#endif
    std::vector<utils::Address *> addresses;
};

}
} // communication

