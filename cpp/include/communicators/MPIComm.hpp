#pragma once

#ifdef MPIINSTALLED
#define OMPI_SKIP_MPICXX 1
#include <mpi.h>
#endif /*MPIINSTALLED*/
#include "communicators/CommBase.hpp"
#include <vector>

namespace YggInterface {
namespace communicator {

/**
 * @brief Wrapper for a MPI communicator.
 */
class mpi_registry_t {
public:
#if defined(MPIINSTALLED) && defined(MPI_COMM_WORLD)
    explicit mpi_registry_t(MPI_Comm comm0) :
      comm(comm0), procs(), tag(0) {}
        mpi_registry_t(const mpi_registry_t&) = delete;
        mpi_registry_t& operator=(const mpi_registry_t&) = delete;
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
  /**
   * @brief Check the return code from an MPI operation, logging the
   *   appropriate error if there is one
   * @param[in] code MPI return code
   * @param[in] method MPI method returning the provided code
   * @param[in] rank Rank of the MPI process returning the provided code
   */
  void CheckReturn(int code, const std::string& method, int rank=0) const ;
};

#ifndef DOXYGEN_SHOULD_SKIP_THIS
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
#endif // DOXYGEN_SHOULD_SKIP_THIS

/**
 * @brief MPI based communicator
 */
class MPIComm : public CommBase<mpi_registry_t> {
public:
    COMM_CONSTRUCTOR_CORE_DEC(MPIComm, MPI_COMM, MPI_INSTALLED_FLAG)

#if defined(MPIINSTALLED) && defined(MPI_COMM_WORLD)
    /*! \copydoc Comm_t::nmsg */
    YGG_API int nmsg(DIRECTION dir=NONE) const override;

    /**
     * Get the communicator source id
     * @return
     */
    YGG_API int mpi_comm_source_id() const;

    std::vector<utils::Address>& getAddresses() { return addresses; }
    
protected:
    /** \copydoc YggInterface::communicator::Comm_t::send_single */
    YGG_API int send_single(utils::Header& header) override;
    /** \copydoc YggInterface::communicator::Comm_t::recv_single */
    YGG_API long recv_single(utils::Header& header) override;
    WORKER_METHOD_DECS(MPIComm);
#endif // MPIINSTALLED
  
private:
    std::vector<utils::Address> addresses;   //!< Internal listing of addresses
};

}
} // YggInterface

