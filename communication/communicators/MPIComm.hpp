#pragma once

#ifdef MPIINSTALLED
#include <mpi.h>
#endif /*MPIINSTALLED*/
#include <CommBase.hpp>
#include <vector>

namespace communication {
namespace communicator {

#if defined(MPIINSTALLED) && defined(MPI_COMM_WORLD)


class mpi_registry_t : public MPI::Comm {
public:
    explicit mpi_registry_t(MPI_Comm comm) : Comm(comm), nproc(0), tag(0) {}

    int nproc; //!< Number of processes in procs.
    std::vector<size_t> procs; //!< IDs for partner processes.
    int tag; //!< Tag for next message.
    mpi_registry_t &Clone() const override;
};


class MPIComm : public CommBase<mpi_registry_t, int> {
#else
    class MPIComm : public CommBase<void,void> {};
#endif
public:
    MPIComm(const std::string &name, std::string &address, Direction direction,
            datatypes::DataType *datatype);

    //explicit MPIComm(const Comm_t* comm);
    ~MPIComm();

    int send(const char *data, const size_t &len) override;

    long recv(char **data, const size_t &len, bool allow_realloc) override;

    int comm_nmsg() override;

    int mpi_comm_source_id();

private:
    std::vector<utils::Address *> addresses;
};

}
} // communication

