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

#include "CommBase.hpp"

/*! @brief Maximum number of channels. */
#define _yggTrackChannels 256

namespace YggInterface {
namespace communicator {
/*!
  @brief Message buffer structure.
*/
typedef struct msgbuf_t {
    long mtype;      //!< Message buffer type
    char data[2048]; //!< Buffer for the message
} msgbuf_t;

/*!
 * @brief IPC based communicator
 */
class IPCComm : public CommBase<int> {
public:
    /**
     * Constructor for an IPC based communicator
     * @param[in] name The name for the communicator, if not given one will be generated
     * @param[in] address The address for the communicator, if not given one will be generated
     * @param[in] direction Enuerated direction for this instance
     * @param[in] flgs Bitwise flags describing the communicator
     * @param[in] type The communicator type
     * @see utils::Address
     */
    IPCComm(const std::string& name,
            utils::Address& address,
            const DIRECTION direction = NONE,
            int flgs = 0, const COMM_TYPE type = IPC_COMM);
    ADD_CONSTRUCTORS(IPC)

#ifdef IPCINSTALLED

    /*!
      @brief Check if an IPC channel is in use
      @param[in] key The channel to check
      @returns int <0 if the channel is already in use
     */
    static int check_key(int key);

    /*! \copydoc Comm_t::close */
    void close() override;

    /**
     *  @brief Add a new channel to the list of existing channels.
     */
    void add_channel();

    /*!
     * @brief Remove a channel.
     * @param[in] close_comm int If 1, the queue will be closed, otherwise it will
     *  just be removed from the register and it is assumed that another process
     *  will close it.
     * @returns int -1 if removal not successful.
     */
    int remove_comm(bool close_comm);

    /*! \copydoc Comm_t::comm_nmsg */
    int comm_nmsg(DIRECTION dir=NONE) const override;
    using Comm_t::send;
    using Comm_t::recv;

#ifndef YGG_TEST
protected:
#endif
    /**
     * Initialize the communciator
     */
    void init();
    /*! \copydoc Comm_t::send_single() */
    int send_single(utils::Header& header) override;

    /*! \copydoc Comm_t::recv_single() */
    long recv_single(utils::Header& header) override;
  
    WORKER_METHOD_DECS(IPCComm);
#else // IPCINSTALLED
    void init() { UNINSTALLED_ERROR(IPC); }
#endif // IPCINSTALLED

private:
    friend class ClientComm;   //!< @see ClientComm
    friend class ServerComm;   //!< @see ServerComm
    /*! @brief Names of channels in use. */
    static int _yggChannelNames[_yggTrackChannels];
    /*! @brief Number of channels in use. */
    static unsigned _yggChannelsUsed;
    static bool _ipc_rand_seeded;

};

}
}
