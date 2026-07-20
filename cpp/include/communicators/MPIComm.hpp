#pragma once

#include "communicators/CommBase.hpp"
#include <vector>

namespace YggInterface {
namespace communicator {

/**
 * @brief Wrapper for a MPI communicator.
 */
class mpi_registry_t {
private:
    mpi_registry_t(const mpi_registry_t&) = delete;
    mpi_registry_t& operator=(const mpi_registry_t&) = delete;
public:
    /** @brief Constructor */
    mpi_registry_t();
    /** @brief Destructor */
    virtual ~mpi_registry_t();
    /**
     * @brief Check if there is a message in flight.
     * @param[in] source Rank of communicator to check for message from.
     * @param[in,out] status Pointer to status structure to populate.
     * @returns MPI status code indicating if probe was successful.
     */
    virtual int Probe(int source, void *status) const;
    /**
     * @brief Send a message.
     * @param[in] data Pointer to buffer containing bytes to send.
     * @param[in] size Number of bytes in data.
     * @param[in] dest Rank of communicator that the message should be
     *   sent to.
     * @return MPI status code indicating if send was successful.
     */
    virtual int Send(const void *data, int size, int dest) const;
    /**
     * @brief Send a message size.
     * @param[in] data Size of data that should be sent in preparation
     *   for sending the data itself.
     * @param[in] dest Rank of communicator that the message should be
     *   sent to.
     * @return MPI status code indicating if send was successful.
     */
    virtual int Send(const int data, int dest) const;
    /**
     * @brief Receive a message.
     * @param[in,out] data Preallocated buffer that message bytes should
     *   be copied to.
     * @param[in] size Size of preallocated buffer that data points to.
     * @param[in] source Rank of communicator that a message should be
     *   received from.
     * @param[in,out] status Pointer to MPI status structure that should
     *   be populated with information about the message.
     * @returns MPI status code indicating if receive was successful.
     */
    virtual int Recv(void *data, int size, int source,
		     void *status) const;
    /**
     * @brief Receive a message size.
     * @param[out] data Reference where the size of the incoming data
     *   should be stored in preparation for receiving the data itself.
      * @param[in] source Rank of communicator that a message should be
     *   received from.
     * @param[in,out] status Pointer to MPI status structure that should
     *   be populated with information about the message.
     * @returns MPI status code indicating if receive was successful.
     */
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
    /** \copydoc YggInterface::communicator::CommBase::create_worker */
    WORKER_METHOD_DECS(MPIComm);
  
private:
    std::vector<utils::Address> addresses; /**< Internal listing of addresses */
};

}
} // YggInterface

