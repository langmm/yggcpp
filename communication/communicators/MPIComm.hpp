#pragma once

#ifdef MPIINSTALLED
#include <mpi.h>
#endif /*MPIINSTALLED*/
#include "CommBase.hpp"
#include <vector>

namespace communication {
namespace communicator {

#if defined(MPIINSTALLED) && defined(MPI_COMM_WORLD)

/**
 * Wrapper for a MPI communicator.
 */
class mpi_registry_t : public MPI::Comm {
public:
    explicit mpi_registry_t(MPI_Comm comm) : Comm(comm), nproc(0), tag(0) {}

    int nproc; //!< Number of processes in procs.
    std::vector<size_t> procs; //!< IDs for partner processes.
    int tag; //!< Tag for next message.
    mpi_registry_t &Clone() const override;
};

/**
 * Class for MPI communicators
 */
class MPIComm : public CommBase<mpi_registry_t, int> {
#else
    class MPIComm : public CommBase<void,void> {};
#endif
public:
    /**
     * Constructor
     * @param name The name of the communicator
     * @param address The address to associate with the communicator, if address is nullptr
     *                then an address will be created.
     * @param direction Enumerated direction for the communicator
     */
    MPIComm(const std::string &name, utils::Address *address, const DIRECTION direction);

    //explicit MPIComm(const Comm_t* comm);
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
    /**
     * Sending function
     * @param data The message to send
     * @param len The length of data
     * @return THe status
     */
    int send(const char *data, const size_t &len) override;

    /**
     * Receiving function
     * @param data The contents of the message withh be placed here
     * @param len The initial length of data
     * @param allow_realloc Whether data can be reallocated if it is too small to hold the message.
     * @return The length of data after the message was copied.
     */
    long recv(char *data, const size_t &len, bool allow_realloc) override;
#ifndef YGG_TEST
private:
#endif
    std::vector<utils::Address *> addresses;  //!< associated addresses
};

}
} // communication

