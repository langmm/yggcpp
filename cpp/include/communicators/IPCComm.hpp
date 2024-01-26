#pragma once

#include "utils/tools.hpp"

#ifdef IPCINSTALLED
#include <fcntl.h>           /* For O_* constants */
#include <sys/stat.h>        /* For mode constants */
#include <sys/msg.h>
#include <sys/types.h>
#include <sys/sem.h>
#include <sys/shm.h>
#endif // IPCINSTALLED

#include "communicators/CommBase.hpp"

namespace YggInterface {
namespace communicator {

#ifndef DOXYGEN_SHOULD_SKIP_THIS
/*!
  @brief Message buffer structure for Sys V IPC queues.
*/
typedef struct msgbuf_t {
    long mtype;      //!< Message buffer type
    char data[2048]; //!< Buffer for the message
} msgbuf_t;
#endif // DOXYGEN_SHOULD_SKIP_THIS

/**
 * @brief Sys V IPC queue based communicator
 */
class YGG_API IPCComm : public CommBase<int> {
public:
    /**
     * Constructor
     * @param[in] name The name for the communicator, if not given one will be generated
     * @param[in] address The address for the communicator, if not given one will be generated
     * @param[in] direction Enuerated direction for this instance
     * @param[in] flgs Bitwise flags describing the communicator
     * @param[in] type The communicator type
     * @see utils::Address
     */
    IPCComm(const std::string& name,
            const utils::Address& address,
            const DIRECTION direction = NONE,
            FLAG_TYPE flgs = 0, const COMM_TYPE type = IPC_COMM);
    ADD_CONSTRUCTORS(IPC)

#ifdef IPCINSTALLED

    /*!
      @brief Get the number of IPC queues that are currently open.
      @return Number of IPC queues.
    */
    static int count_queues();

    /*!
     * @brief Remove a channel.
     * @param[in] close_comm int If 1, the queue will be closed, otherwise it will
     *  just be removed from the register and it is assumed that another process
     *  will close it.
     * @returns int -1 if removal not successful.
     */
    int remove_comm(bool close_comm);

    /** \copydoc YggInterface::communicator::Comm_t::comm_nmsg */
    int comm_nmsg(DIRECTION dir=NONE) const override;
    using Comm_t::send;
    using Comm_t::recv;

protected:
    /** \copydoc YggInterface::communicator::Comm_t::send_single() */
    int send_single(utils::Header& header) override;

    /** \copydoc YggInterface::communicator::Comm_t::recv_single() */
    long recv_single(utils::Header& header) override;
  
    WORKER_METHOD_DECS(IPCComm);
#endif // IPCINSTALLED

private:
    friend class ClientComm;   //!< @see ClientComm
    friend class ServerComm;   //!< @see ServerComm
    ADD_KEY_TRACKER_DECS;
};

}
}
