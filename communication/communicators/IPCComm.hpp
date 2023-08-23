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

namespace communication {
namespace communicator {
/*!
  @brief Message buffer structure.
*/
typedef struct msgbuf_t {
    long mtype; //!< Message buffer type
    char data[2048]; //!< Buffer for the message
} msgbuf_t;
  
class IPCComm : public CommBase<int> {
public:
    /**
     * Constructor for an IPC based communicator
     * @param name The name for the communicator, if not given one will be generated
     * @param address The address for the communicator, if not given one will be generated
     * @param direction Enuerated direction for this instance
     * @param flags Bitwise flags describing the communicator
     */
    explicit IPCComm(const std::string name = "",
		     utils::Address *address = new utils::Address(),
                     const DIRECTION direction = NONE,
		     int flgs = 0, const COMM_TYPE type = IPC_COMM);
    ADD_CONSTRUCTORS(IPC)

#ifdef IPCINSTALLED

    /**
     * Destructor
     */
    ~IPCComm() override;

    int check_key(int key);

    /**
     * Add a new channel to the list of channels
     * @return
     */
    void add_channel();

    /**
     * Remove the given ipc queue
     * @param close_comm If 1, close the queue, otherwise remove the given comm from the register
     * @return -1 on error
     */
    int remove_comm(bool close_comm);

    /**
     * The number of messages in the queue
     * @return The number of messages
     */
    int comm_nmsg() const override;
    using Comm_t::send;
    using Comm_t::recv;

#ifndef YGG_TEST
protected:
#endif
    void init();
    /*! \copydoc Comm_t::send_single */
    int send_single(utils::Header& header) override;

    /*! \copydoc Comm_t::recv_single */
    long recv_single(utils::Header& header) override;
  
    WORKER_METHOD_DECS(IPCComm);
#else // IPCINSTALLED
    void init() { UNINSTALLED_ERROR(IPC); }
#endif // IPCINSTALLED

private:
    friend class ClientComm;
    friend class ServerComm;
    /*! @brief Names of channels in use. */
    static int _yggChannelNames[_yggTrackChannels];
    /*! @brief Number of channels in use. */
    static unsigned _yggChannelsUsed;
    static bool _ipc_rand_seeded;

};

}
}
