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
    explicit mpi_registry_t(MPI_Comm comm0) :
      comm(comm0), procs(), tag(0) {}
    mpi_registry_t(const mpi_registry_t& rhs);
    virtual ~mpi_registry_t();
      
    mpi_registry_t& operator=(const mpi_registry_t& rhs);

    MPI_Comm comm;
    std::vector<size_t> procs; //!< IDs for partner processes.
    int tag; //!< Tag for next message.
    virtual int Probe(int source, MPI_Status *status) const;
    virtual int Send(const void *buf, int count, MPI_Datatype datatype, int dest) const;
    virtual int Recv(void *buf, int count, MPI_Datatype datatype, int source,
		     MPI_Status *status) const;
private:
  void CheckReturn(int code, std::string method, int rank=0) const ;
};

#else
class mpi_registry_t {
public:
  mpi_registry_t() {}
};
#endif
  
class MPIComm : public CommBase<mpi_registry_t> {
public:
    MPIComm(const std::string name = "",
	    utils::Address *address = new utils::Address(),
	    const DIRECTION direction = NONE,
	    int flgs = 0, const COMM_TYPE type = MPI_COMM);
    ADD_CONSTRUCTORS(MPIComm, MPI_COMM)

#if defined(MPIINSTALLED) && defined(MPI_COMM_WORLD)
    ~MPIComm() override;

    int comm_nmsg() const override;

    int mpi_comm_source_id() const;
    using Comm_t::send;
    using Comm_t::recv;

protected:
    void init();
    int send_single(const char *data, const size_t &len,
		    const Header& header) override;

    long recv_single(char*& data, const size_t &len, bool allow_realloc) override;
    WORKER_METHOD_DECS(MPIComm);
#else // MPIINSTALLED
    void init() { UNINSTALLED_ERROR(MPI); }
#endif // MPIINSTALLED
  
#ifdef YGG_TEST
public:
    std::vector<utils::Address *>& getAddresses() { return addresses; }
#endif
  
private:
    std::vector<utils::Address *> addresses;
};

}
} // communication

