#pragma once

#ifdef MPIINSTALLED
#include <mpi.h>
#endif /*MPIINSTALLED*/
#include "CommBase.hpp"
#include <vector>

namespace communication {
namespace communicator {

/**
 * Wrapper for a MPI communicator.
 */
class mpi_registry_t {
public:
#if defined(MPIINSTALLED) && defined(MPI_COMM_WORLD)
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
#else
    mpi_registry_t() {}
#endif
protected:
  void CheckReturn(int code, std::string method, int rank=0) const ;
};

class MPIComm : public CommBase<mpi_registry_t> {
public:
    /**
     * Constructor
     * @param name The name of the communicator
     * @param address The address to associate with the communicator, if address is nullptr
     *                then an address will be created.
     * @param direction Enumerated direction for the communicator
     * @param flags Bitwise flags describing the communicator
     */
    MPIComm(const std::string name = "",
	    utils::Address *address = new utils::Address(),
	    const DIRECTION direction = NONE,
	    int flgs = 0, const COMM_TYPE type = MPI_COMM);
    ADD_CONSTRUCTORS(MPIComm, MPI_COMM)

#if defined(MPIINSTALLED) && defined(MPI_COMM_WORLD)
    /**
     * Destructor
     */
    ~MPIComm() override;

    /**
     * The number of messages in the queue
     * @return The number of messages
     */
    int comm_nmsg() const override;

    /**
     * Get the communicator source id
     * @return
     */
    int mpi_comm_source_id() const;
    using Comm_t::send;
    using Comm_t::recv;

protected:
    void init();
    int send_single(const char *data, const size_t &len,
		    const utils::Header& header) override;

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

