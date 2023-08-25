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
private:
    mpi_registry_t(const mpi_registry_t&) = delete;
    mpi_registry_t& operator=(const mpi_registry_t&) = delete;
public:
    virtual ~mpi_registry_t();
    virtual int Probe(int source, MPI_Status *status) const;
    virtual int Send(const void *buf, int count, MPI_Datatype datatype, int dest) const;
    virtual int Recv(void *buf, int count, MPI_Datatype datatype, int source,
		     MPI_Status *status) const;
    MPI_Comm comm;
    std::vector<size_t> procs; //!< IDs for partner processes.
    int tag; //!< Tag for next message.
#else
    mpi_registry_t() : tag(0) {}
    int tag; //!< Tag for next message.
#endif
protected:
  void CheckReturn(int code, std::string method, int rank=0) const ;
};

#if !defined(MPIINSTALLED) && !defined(MPI_COMM_WORLD)
enum MPI_STATUS_FLAG {
  MPI_SUCCESS,
  MPI_ERR_BUFFER,
  MPI_ERR_COUNT,
  MPI_ERR_TYPE,
  MPI_ERR_TAG,
  MPI_ERR_COMM,
  MPI_ERR_RANK
};
#endif

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
    ADD_CONSTRUCTORS(MPI)

#if defined(MPIINSTALLED) && defined(MPI_COMM_WORLD)
    /*! \copydoc Comm_t::close */
    void close() override;
    /*! \copydoc Comm_t::comm_nmsg */
    int comm_nmsg(DIRECTION dir=NONE) const override;

    /**
     * Get the communicator source id
     * @return
     */
    int mpi_comm_source_id() const;
    using Comm_t::send;
    using Comm_t::recv;

protected:
    void init();
    int send_single(utils::Header& header) override;
    long recv_single(utils::Header& header) override;
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

