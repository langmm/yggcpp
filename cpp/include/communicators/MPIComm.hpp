#pragma once

#include "communicators/CommBase.hpp"
#include <vector>

namespace YggInterface {
namespace communicator {

/**
 * @brief Wrapper for a MPI communicator.
 */
class mpi_registry_t {
public:
    mpi_registry_t();
    mpi_registry_t(const mpi_registry_t&) = delete;
    mpi_registry_t& operator=(const mpi_registry_t&) = delete;
    virtual ~mpi_registry_t();
    virtual int Probe(int source, void *status) const;
    virtual int Send(const void *data, int size, int dest) const;
    virtual int Send(const int data, int dest) const;
    virtual int Recv(void *data, int size, int source,
		     void *status) const;
    virtual int Recv(int& data, int source, void *status) const;
    std::vector<size_t> procs; /**< IDs for partner processes. */
    int tag; /**< Tag for next message. */
private:
  class ImplMPI; /**< Forward declaration of MPI implementation */
  std::unique_ptr<ImplMPI> pImplMPI;  /**< Pointer to MPI implementation */
};

/**
 * @brief MPI based communicator
 */
class MPIComm : public CommBase<mpi_registry_t> {
public:
    COMM_CONSTRUCTOR_CORE_DEC(MPIComm, MPI_COMM, MPI_INSTALLED_FLAG)

    /** \copydoc Comm_t::nmsg */
    YGG_API int nmsg(DIRECTION dir=NONE) const override;

    /**
     * @brief Get the communicator source id
     * @return Rank of process that sent the incoming message.
     */
    YGG_API int mpi_comm_source_id() const;

    /**
     * @brief Get the set of MPI ranks accessed from this comm.
     * @returns Set of MPI ranks this comm exchanges data with.
     */
    std::vector<utils::Address>& getAddresses();
    
protected:
    /** \copydoc YggInterface::communicator::Comm_t::send_single */
    YGG_API int send_single(utils::Header& header) override;
    /** \copydoc YggInterface::communicator::Comm_t::recv_single */
    YGG_API long recv_single(utils::Header& header) override;
    WORKER_METHOD_DECS(MPIComm);
  
private:
    std::vector<utils::Address> addresses; /**< Internal listing of addresses */
};

}
} // YggInterface

